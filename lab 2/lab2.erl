% 
% Lab 2
%

-module(lab2).
-author('Jason').

%
% -- Public --
%

-export([start/0]).

start() -> 
    X = read_integer("Enter a value for X: "),
    Y = read_integer("Enter a value for Y: "),
    Const = X,
    Flist = listLoop(X, Y, Const),
    io:format("~p~n", [Flist]).



%
% -- Private -- 
%

read_integer(Prompt) ->
    case io:fread(Prompt, "~d") of
        {ok, [Value]} when Value >= 0 ->
            Value;
        _ ->
            io:format("Invalid input. Please enter an integer.~n"),
            read_integer(Prompt)
    end.


listLoop(0, Y, Const) -> [];
listLoop(X, Y, Const) -> [elementLoop(Const, Y, X) | listLoop(X-1, Y, Const)].


elementLoop(Const, 1, X) ->[X];
elementLoop(Const, Y, X) -> Val = X + (Const*(Y-1)),
    [Val | elementLoop(Const, Y-1, X)].