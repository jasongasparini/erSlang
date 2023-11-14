%
% tttServer.erl
%
-module(tttServer).
-author('Jason Gasparini').

-define(else, true).
-define(id, "-- server: ").


%
% Public
%
-export([start/0]).

start() ->
   io:fwrite("~sTTT server started on node ~w (pid ~w) ", [?id, node(), self()]),
   ServerPid = spawn(fun serverLoop/0),
   % We want to publish this process in Erlang's process registry.
   % Before we do that, we need to un-register it if it's already been registered.
   SomePlace = whereis(tttServer),
   if (SomePlace /= undefined) ->  % "not undefined" = "is defined"  (This is horrible, I know.)
      unregister(tttServer);
   ?else ->
      % The proccess was NOT already published/registered, so we don't really want to do anything here.
      true    % This is dumb, but I couldn't think of a better "no-op".
   end,
   % Publish this process in Erlang's process registry.
   register(tttServer, ServerPid),
   io:fwrite("with pid ~w registered as ~w.~n", [ServerPid, tttServer]).


%
% Private, but accepting messages sent to serverLoop because of the way it was spawned.
%
serverLoop() -> receive
                   {FromNode, start_game} ->
                      io:fwrite("~sReceived [start_game] request from node ~w.~n",[?id, FromNode]),
                      io:fwrite("~sSending [player_turn] response to node ~w.~n",[?id, FromNode]),
                      InitialBoard = [0,0,0, 0,0,0, 0,0,0],
                      {tttClient, FromNode} ! {node(), player_turn, InitialBoard},
                      serverLoop();

                   {FromNode, process_player_turn, Board, PlayerPos} ->
                      io:fwrite("~sReceived [process_player_turn] request from node ~w with board ~w and player move ~w.~n",[?id, FromNode, Board, PlayerPos]),
                      NewBoard = processPlayerMove(PlayerPos, Board),
                      % Do more stuff here.
                      serverLoop();

                   {FromNode, computer_turn, Board} ->
                      io:fwrite("~sReceived [computer_turn] request from node ~w with board ~p.~n",[?id, FromNode, Board]),
                      % Do more stuff here.
                      serverLoop();

                   {FromNode, _Any} ->
                      io:fwrite("~sReceived unknown request [~p] from node ~w.~n",[?id, _Any, FromNode]),
                      serverLoop()
                end.


%
% Private (not even accepting messages)
%
processPlayerMove(Position, Board) ->
   Target = lists:nth(Position, Board),
   if(Target == 0) ->
      io:fwrite("~sPlacing an X into position ~w.~n", [?id, Position]),
      UpdatedBoard = replaceInList(1, Position, Board),
      UpdatedBoard;
   ?else ->
      io:fwrite("~sCannot place an X into position ~w.~n", [?id, Position]),
      Board
   end. % if

replaceInList(Value, Position, List) ->
   {Part1, Part2} = lists:split(Position-1, List),     % Break the list in two just before the specified Position.
   [_ | Tail] = Part2,                                 % Separate Part2 into Head and Tail, discarding the Head.
   Part1 ++ [Value] ++ Tail.                           % CONS together the result: Part1 ++ the new Value ++ the Tail from Part2.