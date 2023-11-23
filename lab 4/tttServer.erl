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
                      NewBoard = makeMove(InitialBoard),
                      {tttClient, FromNode} ! {node(), player_turn, NewBoard},
                      serverLoop();

                  {FromNode, process_player_turn, Board, PlayerPos} ->
                        io:fwrite("~sReceived [process_player_turn] request from node ~w with board ~w and player move ~w.~n",
                                 [?id, FromNode, Board, PlayerPos]),

                        % New board with the player's move
                        NewBoard = processPlayerMove(PlayerPos, Board),

                        case winner(NewBoard) of
                           true ->
                              io:fwrite("~sSending [game_over] response to node ~w. Player wins!~n", [?id, FromNode]),
                              drawBoard(NewBoard),
                              {tttClient, FromNode} ! {node(), {game_result, player_wins}},
                              serverLoop();
                           false ->
                              case lists:all(fun(X) -> X =/= 0 end, NewBoard) of   % Tie clause
                                    true ->
                                       io:fwrite("~sSending [game_over] response to node ~w. It's a tie!~n", [?id, FromNode]),
                                       drawBoard(NewBoard),
                                       {tttClient, FromNode} ! {node(), {game_result, tie}},
                                       serverLoop();
                                    
                                    false ->
                                       NewNewBoard = makeMove(NewBoard),
                                       % Check for winner
                                       case winner(NewNewBoard) of
                                          true ->
                                                io:fwrite("~sSending [game_over] response to node ~w. Computer wins!~n", [?id, FromNode]),
                                                drawBoard(NewNewBoard),
                                                {tttClient, FromNode} ! {node(), {game_result, computer_wins}},
                                                serverLoop();

                                          false -> % Return the board to client for next move                                             
                                                {tttClient, FromNode} ! {node(), player_turn, NewNewBoard},
                                                serverLoop()
                                       end
                              end
                        end;

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

makeMove(Board) ->
   ComputerMove = calculate(Board),
   io:fwrite("Server is Placing a 0 into position ~w.~n", [ComputerMove]),
    UpdatedBoard = replaceInList(-1, ComputerMove, Board),
    UpdatedBoard.

calculate(Board) -> openPos(0, Board).

openPos(Target, [Head | Tail]) when (Target == Head) -> 1;
openPos(Target, [Head | Tail]) when (Target /= Head) -> 1 + openPos(Target, Tail);      
openPos(Target, [])                                  -> 1.

winner(Board) ->
   Patterns = [[1, 2, 3], [4, 5, 6], [7, 8, 9], [1, 4, 7], [2, 5, 8], [3, 6, 9], [1, 5, 9], [3, 5, 7]],
   lists:any(fun(Condition) -> checkForPattern(Board, Condition) end, Patterns).

checkForPattern(Board, [A, B, C]) ->
    ElementA = lists:nth(A, Board),
    ElementB = lists:nth(B, Board),
    ElementC = lists:nth(C, Board),
    (ElementA =:= ElementB) andalso (ElementB =:= ElementC) andalso (ElementA =/= 0).

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