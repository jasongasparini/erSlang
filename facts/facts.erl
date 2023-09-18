%
% Facts.
% By Jason.
%

-module(facts).
-export([fac1/1]).
-export([fac2/1]).


%
% -- Public -- 
%
fac1(0) -> 1;
fac1(N) -> N * fac1(N-1).


fac2(N) when (N =< 0) -> 1;
fac2(N) when (N > 0) -> N * fac2(N-1).

%
%
%


