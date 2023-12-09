% gameClient.erl - A Distributed Adventure Game Client

-module(gameClient).
-author('Jason Gasparini').
-define(else, true).  % -- This is to make the if statements (somewhat) readable.
-define(id, "-- game client: ").


%--------
% Public
%--------

-export([start/0, start/1]).

start() ->
   io:fwrite("You must supply a game sever node.~n", []).

start(ServerNode) ->
   % -- Spawn this game client process.
   io:fwrite("~sStarting Distributed Adventure Game Client (pid ~w) on node ~w.~n",[?id, self(), node()]),
   GameClientPid = spawn(fun clientLoop/0),
   io:fwrite("~sSpawned game client with pid ~w",[?id, GameClientPid]),
   % We want to publish this process in Erlang's local process registry.
   % Before we do that, we need to un-register it if it's already been registered.
   SomePlace = whereis(gameClient),
   if (SomePlace /= undefined) ->  % "not undefined" = "is defined"  (This is horrible, I know.)
      unregister(gameClient);
   ?else ->
      % The proccess was NOT already published/registered, so we don't really want to do anything here.
      true    % This is dumb, but I couldn't think of a better "no-op".
   end,
   % Publish this process in Erlang's local process registry.
   register(gameClient, GameClientPid),
   io:fwrite(", registered as ~w.~n",[gameClient]),
   % Initialize server monitoring.
   gameClient ! {monitor, ServerNode},
   % -- Begin the play loop
   playLoop(ServerNode).


%---------------------------------
% Private, but accepting messages.
%---------------------------------
clientLoop() ->
   receive
      {monitor, ServerNode} ->
         io:fwrite("~sMonitoring game server on node ~w.~n",[?id, ServerNode]),
         monitor_node(ServerNode, true),
         clientLoop();

      {nodedown, Node} ->
         % This client monitors the server node.
         % The server node has gone down. Notify the admin console...
         io:fwrite("~sServer node ~w has left our cluster and is no longer reachable. Shutting down.~n",[?id, Node]),
         % ...  and shut down.
         % TODO: exit the playLoop too.
         exit(normal);

      {FromNode, _Any}  ->
         io:fwrite("~sReceived message [~p] from node ~w.~n",[?id, _Any, FromNode]),
         clientLoop()
   end.


%---------
% Private
%---------

playLoop(ServerNode) ->
   % -- Get a line of input from the user.
   Line = io:get_line(io_lib:format("~s[play] Enter action or help -] ", [?id])),  % Line is returned as a string.
   {ResultAtom, ResultText} = processCommand(Line, ServerNode),
   %
   % -- Update the display.
   io:fwrite("~s~s~n", [?id, ResultText]),
   %
   % -- Quit or Recurse/Loop.
   if (ResultAtom == quit) ->
      io:fwrite("~sThank you for playing.~n", [?id]);
   ?else ->
     playLoop(ServerNode)  % This is tail recursion, so it's really a jump to the top of playLoop.
   end. % if


processCommand(Line , ServerNode) ->
   % Do some elementary parsing of the line in two parts:
   % 1. Remove the trailing newline charater.
   Command = lists:sublist(Line, length(Line)-1),  % (Because Line is a character list ending with a linefeed.)
   % 2. Break the line into two parts: before the space and after the space (if there's even a space)
   Verb = lists:takewhile( fun(Element) -> Element /= 32 end, Command),
   Noun = lists:dropwhile( fun(Element) -> Element /= 32 end, Command),
   %
   case Verb of
      "help"   -> {help,   helpText()};
      "quit"   -> {quit,   "Quitting."};
      "q"      -> {quit,   "Quitting."};
      "nodes"  -> {nodes,  listNodes()};
      "server" -> {server, server(ServerNode)};
      "go"     -> {go,     go(Noun, ServerNode)};
      % -- Otherwise...
      _Else  -> {unknownCommand, "Silly human."}
   end.

helpText() ->
   io_lib:format("Commands: [help], [quit], [nodes], [server], [go <location>]", []).

listNodes() ->
   io_lib:format("This node: ~w~n", [node()]) ++   % No ?id here because it will be supplied when printed above.
   io_lib:format("~sOther nodes in our cluster: ~w", [?id, nodes()]).

server(ServerNode) ->
   KnownNode = lists:member(ServerNode, nodes()),
   if KnownNode ->
      io_lib:format("Talking to game server on node ~w, which is known to be in our cluster.", [ServerNode]);
   ?else ->
      io_lib:format("Talking to game server on node ~w, which is NOT known to be in our cluster, and that may be a problem.", [ServerNode])
   end. % if

go([_Space | Destination], ServerNode) ->
   DestAtom = list_to_atom(Destination),
   io:fwrite("~s[debug] Going to location [~w].~n", [?id, DestAtom]),
   {gameServer, ServerNode} ! {node(), goToLocation, DestAtom},
   ok;
go([], _ServerNode) ->
   io_lib:format("Where do you want to go?", []).
