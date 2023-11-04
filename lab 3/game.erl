% game.erl  An OSRS inspired text adventrure game

-module(game).
-author('Jason Gasparini').
-define(else, true).  % -- This is to make the if statements (somewhat) readable.


-type direction() :: north | south | east | west | drink.

%--------
% Public
%--------

-export([start/0]).

start() ->
   % -- Spawn the server process.
   io:fwrite("Starting AG server.~n",[]),
   ServerPid = spawn(fun serverLoop/0),
   % -- Display the initial location description by moving north from -1.
   {_NewLocale, Description, _NewInventory} = processCommand(-1, "north", ServerPid, []),
   io:fwrite("~n~s~n~n", [Description]),

   % -- Kick off the game loop with the ServerPID, location = 0, and turn count = 1.
   gameLoop(ServerPid, 0, 1, []).


%---------
% Private
%---------

gameLoop(ServerPid, CurrentLocale, TurnCount, InventoryList) ->
   % -- Show the map and get input from the player.
   io:fwrite("~s", [showMap(CurrentLocale)]),
   io:fwrite("~nTurn ~w ] ", [TurnCount]),
   {ok, Input} = io:fread("Where will you go? (North, East, South, West) ", "~s"),
   [Command | _] = Input,

   % -- Process the player's input/command into a NewLocale and Description.
   {NewLocale, Description, NewInventory} = processCommand(CurrentLocale, Command, ServerPid, InventoryList),
   %
   % -- Update the display.
   io:fwrite("~n~s~n~n", [Description]),
   %
   % -- Quit or Recurse/Loop.
   if (NewLocale < 0) ->
     io:fwrite("Goodbye.~n",[]);
   ?else ->
     gameLoop(ServerPid, NewLocale, TurnCount+1, NewInventory)
   end. 


processCommand(CurrentLocale, Command, ServerPid, Inventory) ->
   case Command of
      % -- Compass directions - Get the new location from the server.
      "north" -> move(ServerPid, {CurrentLocale, north}, Inventory);
      "n"     -> move(ServerPid, {CurrentLocale, north}, Inventory);
      "south" -> move(ServerPid, {CurrentLocale, south}, Inventory);
      "s"     -> move(ServerPid, {CurrentLocale, south}, Inventory);
      "east"  -> move(ServerPid, {CurrentLocale, east}, Inventory);
      "e"     -> move(ServerPid, {CurrentLocale, east}, Inventory);
      "west"  -> move(ServerPid, {CurrentLocale, west}, Inventory);
      "w"     -> move(ServerPid, {CurrentLocale, west}, Inventory);
      % -- Other commands - Handle non-movement commands.
      "quit"      -> {-1, "Thank you for playing.", Inventory};
      "q"         -> {-1, "Thank you for playing.", Inventory};
      "look"      -> {CurrentLocale, locationDesc(CurrentLocale), Inventory};
      "l"         -> {CurrentLocale, locationDesc(CurrentLocale), Inventory};
      "help"      -> {CurrentLocale, helpText(), Inventory};
      "map"       -> {CurrentLocale, showMap(CurrentLocale), Inventory};
      "show map"  -> {CurrentLocale, showMap(CurrentLocale), Inventory};
      "inventory" -> {CurrentLocale, showInventory(Inventory), Inventory};
      "i"         -> {CurrentLocale, showInventory(Inventory), Inventory};
      "pickup"    -> {CurrentLocale, "You pick up a few items...", pickUp(CurrentLocale, Inventory)};
      "drink"     -> drank(ServerPid, {CurrentLocale, drink}, Inventory);
      "reset"     -> {0, "Game has been reset", []};
      "place"     -> placed(CurrentLocale, Inventory);
      % -- Otherwise...
      _Else   -> {CurrentLocale, "I do not understand.", Inventory}  % Starting _Else with "_" prevents the "unused" warning.
   end.


helpText() -> io_lib:format("You can enter compass directions: [n] or [north], [s] or [south], [e] or [east], ", []) ++
              io_lib:format("[w] or [west], as well as [quit], [look], [help], [map], [inventory], [pickup], [drink], [reset] and other commands.", []).


% Send the move message (a tuple) to the server.
-spec move(pid(), {integer(), direction()}, []) -> integer(). %  This is not enforced at runtime. It's for Dializer and Typer.
move(ServerPid, MoveTuple, Inventory) ->
    case MoveTuple of
        {6, north} ->
            case lists:member(dagger, Inventory) of
                true ->
                    io:fwrite("As you listen to your instincts and flee the manor, you are attacked by a ZOMBIE! Good thing you picked up the dagger.~n", []),
                    ServerPid ! {self(), MoveTuple, Inventory},
                    receive
                        {ServerPid, Response} -> Response
                    end;
                false ->
                    io:fwrite("As you turn away from the manor, you are viciously attacked by a ZOMBIE. If only you had picked up the dagger you saw in Lumbridge...~n", []),
                    exit(self(), "Game Over.")
            end;
        _ ->
            ServerPid ! {self(), MoveTuple, Inventory},
            receive
                {ServerPid, Response} -> Response  % This waits for a response from ToPid.
            end
    end.

% This is the process spawned at the start.
serverLoop() ->
   receive
      {FromPid, {CurrentLocale, Direction}, Inventory} ->
         NewLocaleNumber = mapper(CurrentLocale, Direction),
         if NewLocaleNumber > -1 ->
            % Valid move.
            NewLocaleDesciption = locationDesc(NewLocaleNumber),
            NewLocaleItems      = locationItems(NewLocaleNumber),
            FromPid ! {self(), {NewLocaleNumber, io_lib:format("~s You see ~w scattered around.", [NewLocaleDesciption, NewLocaleItems]), Inventory}},
            serverLoop();
         ?else ->
            % Invalid move.
            FromPid ! {self(), {CurrentLocale, "You smash your head on an invisible wall. You cannot go that way.", Inventory}},
            serverLoop()
         end;

      {FromPid, _, Inventory} ->
         FromPid ! {self(), "Internal error: How the f*** did you manage this...", Inventory},
         serverLoop()
   end.


% Mapper. Double-chcek with showMap().
mapper(_, drink) -> 10;
mapper(-1, north) -> 0;
mapper( 0, north) -> 1;
mapper( 0, east) -> 3;
mapper( 1, south) -> 0; 
mapper( 1, west)  -> 5;
mapper( 2, south)  -> 3;
mapper( 3, south) -> 4;
mapper( 3, north) -> 2;
mapper( 3, west) -> 0;
mapper( 4, north) -> 3;
mapper( 5, east)  -> 1;
mapper( 5, south)  -> 6;
mapper( 6, north)  -> 5;
mapper( 10, _)    -> 0;
mapper( _, _)     -> -1.


% Show map. Double-check with mapper().
showMap(CurrentLocale) ->
   io_lib:format("...................................... ~s ............ ~n",    [dispLocale(CurrentLocale, 10)]) ++
   io_lib:format("....  --- ~s ..... ~s -- ? ............................. ~n",   [dispLocale(CurrentLocale, 1), dispLocale(CurrentLocale, 2)]) ++
   io_lib:format("... / ... | ..... | .................................. ~n",    []) ++
   io_lib:format(". ~s ..... | ..... | .................................. ~n",    [dispLocale(CurrentLocale, 5)]) ++
   io_lib:format(". | ..... | ..... | .................................. ~n",    []) ++
   io_lib:format(". | ..... ~s ----- ~s .................................. ~n", [dispLocale(CurrentLocale, 0), dispLocale(CurrentLocale, 3)]) ++
   io_lib:format(". ~s ............. | .................................. ~n",    [dispLocale(CurrentLocale, 6)]) ++
   io_lib:format("................. | .................................. ~n",    []) ++
   io_lib:format("................. ~s .................................. ~n",  [dispLocale(CurrentLocale, 4)]) ++
   io_lib:format("...................................................... ~n",    []).


dispLocale(CurrentLocale, MapLoc) ->
   if CurrentLocale == MapLoc ->
      "@";
   ?else ->
      integer_to_list(MapLoc)  % Remember, strings are lists of ASCII/Unicode values in Erlang.
   end.


% Location Descriptions
% These location descriptions DO NOT end with ~n newlines. The newline is taken care of in the display code.
locationDesc(0)   -> io_lib:format("0. Lumbridge~nThe village of newbies and veterans alike. It has a cozy atmosphere and many call it home.", []);
locationDesc(1)   -> io_lib:format("1. Varrock~nThe city of Varrock. A merchants paradise where scam artists and folks of all types gather.", []);
locationDesc(2)   -> io_lib:format("2. Death's Tomb~nAfter stumbling down a hidden tunnel located on the North side of the arena, you are greeted by a grand archway closed off by two stone doors.~nThe doors have characters carved into them of which you have never seen and cannot decipher.~n~nTo the right of the doors is a hole in the wall, almost in the shape of a mask of some sort. Maybe something fits here?", []);
locationDesc(3)   -> io_lib:format("3. Emir's Arena~nAh, Emir's Arena, many stories have been told of great warriors making their name at this place.~nBut that was long ago, sand and dirt has molded what was and there is nothing left but the echos of the past.", []);
locationDesc(4)   -> io_lib:format("4. Al Kharid~nAfter a long trip south, you come across the bustling centerpiece of the Kharidian Desert.", []);
locationDesc(5)   -> io_lib:format("5. Falador~nYou journey west to the City of the White Knights. The building are marble and pristine and men and women of status fill the streets. You feel like you don't belong here.", []);
locationDesc(6)   -> io_lib:format("6. Draynor Manor~nFollowing the long since abandoned path leading from the city, you arrive at the haunted manor. You think to yourself, 'It would be wise to turn back now'", []);
locationDesc(10)   -> io_lib:format("?. Ape Atoll~n...You wake up stranded on an island filled with evil monkeys. Maybe you shouldn't have drank that ale. You see a boat in the distance which could bring you home...", []);
locationDesc(Loc) -> io_lib:format("Oops! Unknown locale: ~w.", [Loc]).


% Location Items
locationItems(0)    -> [dagger, ale, steak];
locationItems(1)    -> [goldCoin];
locationItems(2)    -> [];
locationItems(3)    -> [rustyHelmet];
locationItems(4)    -> [sand];
locationItems(5)    -> [];
locationItems(6)    -> [key];
locationItems(10)   -> [mysteriousMask];
locationItems(_Loc) -> [].  % TODO: throw exception due to the invalid location value.


% Other Commands

showInventory([])            -> io_lib:format("You are not carrying anything of use.", []);
showInventory(InventoryList) -> io_lib:format("You are carrying ~w.", [InventoryList]).

pickUp(CurrentLocale, Inventory) -> Inventory ++ locationItems(CurrentLocale).

drank(ServerPid, {CurrentLocale, Direction}, Inventory) ->
   case lists:member(ale, Inventory) of
      true ->
         move(ServerPid, {CurrentLocale, Direction}, Inventory);
      false ->
         {CurrentLocale, "You have no ale to drink. Maybe you should go find some...", Inventory}
   end.

placed(CurrentLocale, Inventory) -> 
   case CurrentLocale == 2 of
      true ->
         case lists:member(mysteriousMask, Inventory) of
            true -> 
               {-1, "You place the mask you found from the island into the hole. Something clicks and the doors swing open to reveal a room filled with treasure. YOU WIN!!!", Inventory};
            false ->
               {CurrentLocale, "You don't have anything that might fit in the hole. It looks like some type of monkey-faced mask would fit here...", Inventory}
         end;
      false ->
         {CurrentLocale, "In a German Accent you think to yourself, 'I lift things up and put them down'. It seems there is a better location to do this...", Inventory}
      end.