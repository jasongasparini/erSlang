%
% Store utilities
% By Jason.
%

-module(storeUtils).
-export([total/1]).

%
% -- Public -- 
%
total([Head|Tail]) -> store:cost(Head) + total(Tail);
total([]) -> 0.
