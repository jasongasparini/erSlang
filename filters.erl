-module(filters).

-export([f1/2, f2/2]).


f1(_, []) -> [];
f1(BF, List) ->
        case BF(hd(List)) of
            true -> [hd(List) | f1(BF, tl(List))];
            false -> f1(BF, tl(List))
        end.


-define(else, true).

f2(BF, List) ->
    if(List == []) ->
        [];
    ?else ->
        Include = BF(hd(List)),
        if (Include) ->
            [hd(List) | f2(BF, tl(List))];
        ?else ->
            f2(BF, tl(List))
        end
    end.