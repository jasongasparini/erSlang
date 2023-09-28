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
   {ok, Input} = io:fread("Guess an integer: ", "~d"),
   io:fwrite("You guessed ~w, which is ", Input),
   if (Input == [Target]) ->
      io:fwrite("correct. Nice going!~n");
   ?else ->
      io:fwrite("wrong. Try again.~n"),
      start()
   end.


%
% -- Private -- 
%   