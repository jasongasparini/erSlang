%
% My second program.
% By Jason.
%

-module(store).
-export([cost/1]).

%
% -- Public -- 
%
cost(iPhone) -> 999;
cost(iPad) -> 499;
cost(macBook) -> 1900;
cost(iMac) -> 2400;
cost(pencil) -> 99;
cost(visionPro) -> 36000;
cost(insight) -> priceless;
cost(_) -> notSold.


%
% -- Private -- 
%

   