% ag_server9.erl - (Silver) Adventure Game Server

-module(ag_server9).
-author('Alan G. Labouseur').
-define(else, true).  % -- This is to make the if statements (somewhat) readable.

% TODO: Add to inventory by visiting locales.
%       Increase score by visiting locales.
%       Show ratio of score to turns at the end of the game.
%       Move more of the gameplay logic into the server.


%--------
% Public
%--------

-export([start/0]).

start() ->
   % -- Spawn the server process.
   io:fwrite("Starting AG server.~n",[]),
   ServerPid = spawn(fun serverLoop/0),
   % -- Display the initial location description by moving north from -1.
   {_NewLocale, Description, Score} = processCommand(-1, "north", ServerPid, [], 0),
   io:fwrite("~n~s~n~n", [Description]),

   % -- Kick off the game loop with the ServerPID, location = 0, turn count = 1, score, and radioactivePocketLint in inventory.
   gameLoop(ServerPid, 0, 1, Score, [radioactivePocketLint]).


%---------
% Private
%---------

gameLoop(ServerPid, CurrentLocale, TurnCount, Score, InventoryList) ->
   % -- Show the map and get input from the player.
   io:fwrite("~s", [showMap(CurrentLocale)]),
   io:fwrite("~nScore=~w  Turn ~w ] ", [Score, TurnCount]),
   {ok, Input} = io:fread("Enter a command (or help) -] ", "~s"),  % Input gets returned as a list from io:fread.
   [Command | _] = Input,   % (Because Input is a list.)
   %
   % -- Process the player's input/command into a NewLocale and Description.
   {NewLocale, Description, NewScore} = processCommand(CurrentLocale, Command, ServerPid, InventoryList, Score),
   %
   % -- Update the display.
   io:fwrite("~n~s~n~n", [Description]),
   %
   % -- Quit or Recurse/Loop.
   if (NewLocale < 0) ->
     io:fwrite("Goodbye.~n",[]);
   ?else ->
     gameLoop(ServerPid, NewLocale, TurnCount+1, NewScore, InventoryList)  % This is tail recursion,
   end. % if                                                               % so it's really a jump to the top of gameLoop.


processCommand(CurrentLocale, Command, ServerPid, Inventory, Score) ->
   case Command of
      % -- Compass directions - Get the new location from the server.
      "north" -> move(ServerPid, {CurrentLocale, north, Score});
      "n"     -> move(ServerPid, {CurrentLocale, north, Score});
      "south" -> move(ServerPid, {CurrentLocale, south, Score});
      "s"     -> move(ServerPid, {CurrentLocale, south, Score});
      "east"  -> move(ServerPid, {CurrentLocale, east, Score});
      "e"     -> move(ServerPid, {CurrentLocale, east, Score});
      "west"  -> move(ServerPid, {CurrentLocale, west, Score});
      "w"     -> move(ServerPid, {CurrentLocale, west, Score});
      % -- Other commands - Handle non-movement commands.
      "quit"      -> {-1, "Thank you for playing.", Score};
      "q"         -> {-1, "Thank you for playing.", Score};
      "look"      -> {CurrentLocale, locationDesc(CurrentLocale), Score};  % TODO: Does not account for items found at locales.
      "l"         -> {CurrentLocale, locationDesc(CurrentLocale), Score};
      "help"      -> {CurrentLocale, helpText(), Score};
      "sing"      -> {CurrentLocale, itsPitchDark(), Score};
      "map"       -> {CurrentLocale, showMap(CurrentLocale), Score};
      "show map"  -> {CurrentLocale, showMap(CurrentLocale), Score};
      "inventory" -> {CurrentLocale, showInventory(Inventory), Score};
      "i"         -> {CurrentLocale, showInventory(Inventory), Score};
      % -- Otherwise...
      _Else   -> {CurrentLocale, "I do not understand.", Score}  % Starting _Else with "_" prevents the "unused" warning.
   end.


helpText() -> io_lib:format("You can enter compass directions: [n] or [north], [s] or [south], [e] or [east], ", []) ++
              io_lib:format("[w] or [west], as well as [quit], [look], [help], [map], [inventory], and other commands.", []).


% Send the move message (a tuple) to the server.
move(ServerPid, MoveTuple) ->
   ServerPid ! {self(), MoveTuple},
   receive
      {ServerPid, Response} -> Response  % This waits for a response from ToPid.
   end.


% This is the process spawned at the start.
serverLoop() ->
   receive
      {FromPid, {CurrentLocale, Direction, Score}} ->
         NewLocaleNumber = mapper(CurrentLocale, Direction),
         if NewLocaleNumber > -1 ->
            % Valid move.
            NewLocaleDesciption = locationDesc(NewLocaleNumber),
            NewLocaleItems      = locationItems(NewLocaleNumber),
            FromPid ! {self(), {NewLocaleNumber, formatText(NewLocaleDesciption, NewLocaleItems), Score+1}},
            serverLoop();
         ?else ->
            % Invalid move.
            FromPid ! {self(), {CurrentLocale, "You cannot go that way.", Score-1}},
            serverLoop()
         end;

      {FromPid, _, Score} ->
         FromPid ! {self(), "Internal error: You are lost. Nice going, Indiana.", Score-10},
         serverLoop()
   end.

% Format the text output so that an empty inventory list is NOT printed.
formatText(NewLocaleDesciption, []) -> io_lib:format("~s", [NewLocaleDesciption]);
formatText(NewLocaleDesciption, NewLocaleItems) -> io_lib:format("~s You see ~w scattered around.", [NewLocaleDesciption, NewLocaleItems]).

% Mapper. Double-check with showMap().
mapper(-1, north) -> 0;
mapper( 0, north) -> 1;
mapper( 0, south) -> 2;
mapper( 0, west)  -> 5;
mapper( 1, south) -> 0;
mapper( 1, west)  -> 5;
mapper( 2, north) -> 0;
mapper( 2, east)  -> 4;
mapper( 3, south) -> 4;
mapper( 4, north) -> 3;
mapper( 4, west)  -> 2;
mapper( 5, north) -> 1;
mapper( 5, east)  -> 0;
mapper( _, _)     -> -1.


% Show map. Double-check with mapper().
showMap(CurrentLocale) ->
   io_lib:format("................... ~n",    []) ++
   io_lib:format(".. +---- ~s ........ ~n",   [dispLocale(CurrentLocale, 1)]) ++
   io_lib:format(".. | ... | ........ ~n",    []) ++
   io_lib:format(".. | ... | ........ ~n",    []) ++
   io_lib:format(".. ~s --- ~s ... ~s .. ~n", [dispLocale(CurrentLocale, 5), dispLocale(CurrentLocale, 0), dispLocale(CurrentLocale, 3)]) ++
   io_lib:format("........ | ... | .. ~n",    []) ++
   io_lib:format("........ | ... | .. ~n",    []) ++
   io_lib:format("........ ~s --- ~s .. ~n",  [dispLocale(CurrentLocale, 2), dispLocale(CurrentLocale, 4)]) ++
   io_lib:format("................... ~n",    []).


dispLocale(CurrentLocale, MapLoc) ->
   if CurrentLocale == MapLoc ->
      "@";
   ?else ->
      integer_to_list(MapLoc)  % Remember, strings are lists of ASCII/Unicode values in Erlang.
   end.


% Location Descriptions
% These location descriptions DO NOT end with ~n newlines. The newline is taken care of in the display code.
locationDesc(0)   -> io_lib:format("0. Inn of the Last Home~nThis tavern is famous for its wonderful food and ale.", []);
locationDesc(1)   -> io_lib:format("1. Yellow Brick Road~nGold, emerald, rubies...", []);
locationDesc(2)   -> io_lib:format("2. Depths of the Earth~nYou find yourself in a vast subterranean network of interconnected caverns and tunnels.", []);
locationDesc(3)   -> io_lib:format("3. Vault of the Drow~nYou have entered a hemispherical cyst in the crust of the earth, a huge domed vault miles long and nearly as wide.", []);
locationDesc(4)   -> io_lib:format("4. Mouth of the Yawning Cave~nYou step into the inky darkness, a chorus of glowing eyes following your every move.", []);
locationDesc(5)   -> io_lib:format("5. West Lake~nSomehow you have stumbled onto a freshwater lake in Hangzhou, China. There are temples, pagodas, and gardens all around, and some pretty good tea too.", []);
locationDesc(Loc) -> io_lib:format("Oops! Unknown locale: ~w.", [Loc]).


% Location Items
locationItems(0)    -> [potionOfHealing, eyeOfVecna];
locationItems(1)    -> [rodOfMassDeath, gemOfSeeing];
locationItems(2)    -> [babaYagasHut];
locationItems(3)    -> [];
locationItems(4)    -> [bagOfHolding, helmOfSight, necklaceOfAdaptation];
locationItems(5)    -> [ringOfInvisibility];
locationItems(_Loc) -> [].  % TODO: throw exception due to the invalid location value.


% Other Commands

showInventory([])            -> io_lib:format("You are not carrying anything of use.", []);
showInventory(InventoryList) -> io_lib:format("You are carrying ~w.", [InventoryList]).


itsPitchDark() -> io_lib:format("You are likely to be eaten by a grue. ~n",         []) ++
                  io_lib:format("If this predicament seems particularly cruel, ~n", []) ++
                  io_lib:format("consider whose fault it could be: ~n",             []) ++
                  io_lib:format("not a torch or a match in your inventory. ~n",     []) ++
                  io_lib:format("                              - MC Frontalot~n",   []) ++
                  io_lib:format(" https://www.youtube.com/watch?v=4nigRT2KmCE",     []).