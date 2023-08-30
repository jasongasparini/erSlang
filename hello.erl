%
% My first program.
% By Alan.
%

-module(hello).
-export([start/1]).

%
% -- Public -- 
%
start(X) ->
   io:format("Hello world!~n"),
   tart().

%
% -- Private -- 
%
tart() ->
   io:format("Tart to Tart~n").
   