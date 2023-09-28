%
% Guess
%

-module(guess).
-author('Alan').

-define(else, true).

%
% -- Public -- 
%

-export([start/0]).

start() -> 
   Target = 42,
   try io:fread("Guess an integer: ", "~d") of 
      {ok, Input} -> io:fwrite("You guessed ~w, which is ", Input),
					 if (Input == [Target]) ->
						io:fwrite("correct. Nice going!~n");
					 ?else ->
						io:fwrite("wrong. Try again but guess "),
						
                        if ( Input > [Target]) ->
                            io:fwrite("lower. ~n");
                        ?else ->
                            io:fwrite("higher. ~n")
                        end,
                        start()
					 end;
      {error,{fread,integer}} -> io:fwrite("Bad input. Try again.~n"),
	                             start()					 
    catch
       _:_ -> oops
    end.

%
% -- Private -- 
%   

