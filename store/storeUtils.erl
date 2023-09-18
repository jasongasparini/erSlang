%
% Store utilities
% By Jason.
%

-module(storeUtils).
-export([total/1]).

%
% -- Public -- 
%
total([Head|Tail]) -> intVal(store:cost(Head)) + total(Tail);
total([]) -> 0.


%
% -- Private -- 
%

intVal(Val) when is_integer(Val) -> Val;
intVal(_) -> 0.