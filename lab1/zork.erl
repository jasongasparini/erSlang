%
% My first program.
% By Jason.
%

-module(zork).
-export([start/0]).

%
% -- Public -- 
%
start() ->
   io:format("Zork!~n"),
   welcome().

%
% -- Private -- 
%
welcome() ->
   io:format("West of the House~n").
   