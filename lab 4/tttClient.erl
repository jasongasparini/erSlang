%
% tttClient.erl
%
-module(tttClient).
-author('Jason Gasparini').

-define(else, true).
-define(id, "-- client: ").


%
% Public
%
-export([start/0, play/0, play/1]).

start() ->
   io:fwrite("~sTTT client started on node ~w (pid ~w) ", [?id, node(), self()]),
   ClientPid = spawn(fun clientLoop/0),
   % We want to publish this process in Erlang's process registry.
   % Before we do that, we need to un-register it if it's already been registered.
   SomePlace = whereis(tttClient),
   if (SomePlace /= undefined) ->  % "not undefined" = "is defined"  (This is horrible, I know.)
      unregister(tttClient);
   ?else ->
      % The proccess was NOT already published/registered, so we don't really want to do anything here.
      true    % This is dumb, but I couldn't think of a better "no-op".
   end,
   % Publish this process in Erlang's process registry.
   register(tttClient, ClientPid),
   io:fwrite("with pid ~w registered as ~w.~n", [ClientPid, tttClient]).


play() ->
   io:fwrite("You must supply a node.~n", []).

play(ServerNode) ->
   io:fwrite("~sSending [start_game] request to node ~w.~n",[?id, ServerNode]),
   {tttServer, ServerNode} ! {node(), start_game},
   clientLoop().


%
% Private, but accepting messages sent to clientLoop because of the way it was spawned.
%
clientLoop() -> receive
                   {FromNode, player_turn, Board} ->
                      io:fwrite("~sReceived [player_turn] request from node ~w with board ~p.~n",[?id, FromNode, Board]),
                      drawBoard(Board),
                      io:fwrite("~s", [?id]),
                      {ok, PlayerMove} = io:fread("Where do you want to move [1-9]? ", "~d"),   % PlayerMove gets read as a list.
                      io:fwrite("~sSending [process_player_turn] response to node ~w with board ~w and player move ~w.~n",[?id, FromNode, Board, hd(PlayerMove)]),
                      {tttServer, FromNode} ! {node(), process_player_turn, Board, hd(PlayerMove)},
                      clientLoop();

                      % These are the ending scenarios based off the win conditions sent from the server
                   {FromNode, {game_result, player_wins}} ->
                     io:fwrite("~sReceived [game_result] request from node ~w.~n",[?id, FromNode]),
                     % drawBoard(Board),
                     io:format("Winner...The server learns...~n"),
                     
                     erlang:halt();

                   {FromNode, {game_result, tie}} ->
                     io:fwrite("~sReceived [game_result] request from node ~w.~n",[?id, FromNode]),
                     % drawBoard(Board),
                     io:format("You tied. How boring...It seems like this has happened before...~n"),
                     
                     erlang:halt();
                     
                   {FromNode, {game_result, computer_wins}} ->
                     io:fwrite("~sReceived [game_result] request from node ~w.~n",[?id, FromNode]),
                     % drawBoard(Board),
                     io:format("Get rekt. You may never win. Give up for eternity.~n"),
                     
                     erlang:halt();                     

                   {FromNode, _Any}  ->
                      io:fwrite("~sReceived unknown request [~p] from node ~w.~n",[?id, _Any, FromNode]),
                      clientLoop()
                end.


%
% Private; no messages either.
%

drawBoard(Board) -> io:fwrite(" ~s | ~s | ~s ~n", [getDisplay(Board,1), getDisplay(Board,2), getDisplay(Board,3)] ),
                    io:fwrite("---+---+---~n", []),
                    io:fwrite(" ~s | ~s | ~s ~n", [getDisplay(Board,4), getDisplay(Board,5), getDisplay(Board,6)] ),
                    io:fwrite("---+---+---~n", []),
                    io:fwrite(" ~s | ~s | ~s ~n", [getDisplay(Board,7), getDisplay(Board,8), getDisplay(Board,9)] ).

getDisplay(Board,Position) -> case lists:nth(Position, Board) of
                                -1 -> ["O"];
                                 0 -> [" "];
                                 1 -> ["X"];
                                 _  -> " "
                              end.