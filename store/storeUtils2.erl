%
% Apple Store Utilities.
% By Alan.
%

-module(storeUtils).
-export([total/1, test/0, myMap/2]).

%
% -- Public -- 
%
total([ {What, HowMany} | Tail ]) -> (intVal(store:cost(What))*HowMany) + total(Tail);
total([  Head           | Tail ]) -> (intVal(store:cost(Head))) + total(Tail);
total([])                         -> 0.

test() -> 3199 = total([iMac, iPad]), 
          1798 = total([iPhone, iPad]),
          'The tests have passed.'.


myMap(_, []) -> [];
myMap(Function, List) -> [Function(hd(list)) | myMap(Function, tl(list))].


%
% -- Private -- 
%   
intVal(Val) when is_integer(Val) -> Val;
intVal(_)                        -> 0.