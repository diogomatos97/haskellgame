
import Data.List
import Data.Char

type Location = String
--declaring the variable "Location" as a String
type Direction = String
--declaring the variable "Direction" as a String
type Thing = String
--declaring the variable "Thing" as a String
type Response = String
--declaring the variable "Response" as a String

--map locations
type PathMap = [((Location, Direction), Location)]
--declaring the attributes for the variable Pathmap
--it consists of ((current location, direction to move to), new location)
paths :: PathMap
--path takes the variable PathMap
--takes ((current location, direction to move to), new location)
paths = [
    (("spider", "d"), "spiders den"),
--((current location, direction to move to), new location)
    (("spiders den", "u"), "spider"),
    (("spiders den", "e"), "hall"),
    (("hall", "w"), "spiders den"),
    (("hall", "s"), "start"),
    (("hall", "e"), "dragon room"),
    
    (("start", "n"), "hall"),
    (("dragon room", "w"), "hall"),
    (("hall", "n"), "arrow room"),
    (("arrow room", "s"), "hall"),
    (("arrow room", "w"), "weapons room"),
    (("weapons room", "e"), "arrow room")
    ]
--item locations
type LocationMap = [(Thing, Location)]
--locationMap is a conjunction of thing and location
locations :: LocationMap
--locations takes the variable LocationMap
locations =  [
    ("fist", "dragon room"),
    --(thing/item, thing/item location)
    ("key", "spider"),
    ("shield", "start"),
    ("sword", "weapons room"),
    ("myself", "start"),
    -- characters begins at starting point
    ("spider", "alive"),
    ("dragon", "alive")
    --dragon and spider current state
    ]
--generate world
type World = (PathMap, LocationMap, Response)
world :: IO (PathMap, LocationMap, Response)
--wold takes input
world = return (paths, locations, "")
--retuns

--game start
main :: IO (String)
--takes string input
main = do
--when user types main, the program dows the following
    putStrLn "\nWelcome to the Iron fist Game!\n"
    putStrLn instructions
    --outputs game instruction
    putStrLn ""
    putStrLn first_room
    --outputs quest for the user
    play_game $ return (paths, locations, "")
    --read user input until 
    return "Goodbye!"
    --game ends
    
instructions =
    "Enter commands using one or two words.\n" ++
    "Available commands are:\n" ++
    "main               -- to restart the game.\n" ++
    "n  s  e  w  u  d   -- to go in that direction.\n" ++
    "take object        -- to pick up the named object.\n" ++
    "drop object        -- to put down the named object.\n" ++
    "kill               -- to attack an enemy.\n" ++
    "look               -- to look closely for items.(There might be an item aroud you)\n" ++
    "i                  -- to see your inventory (what you are holding).\n" ++
    "quit               -- to end the game and quit."

first_room =
--description for starting point
    "You are at the start of your journey." ++
    "To the north is the hall.\n" ++
    "Your assignment, is to defeat the dragon and win the iron fist"
    
play_game :: IO (World) -> IO (World)
--takes World and returns world (takes input returns output)
play_game world = do
    (paths, locations, response) <- world
    putStrLn response
    putStrLn ""
    if game_over locations
        then return ([], [], "")
        else do
            putStr "command> "
            command <- getLine
            --get command/user input
            if command == "quit"
            --if user inputs quit, returns response as "quiting"
                then return (paths, locations, "Quitting.")
                else  play_game $ return (do_command command paths locations)
                --else keep playing game

game_over :: LocationMap -> Bool
--the game over condition takes in the LocationMap and returns a boolean
game_over locations =
--for the game to be over:
    let my_location = get "myself" locations
    --gets the user location
        fist_location = get "fist" locations
        --locates the fist
    in my_location == "dead" || (my_location == "start" && fist_location == "holding")
    --game is over if player is dead or,
    --game is over if player is at start holding the fist
    
can_move :: Location -> Direction -> PathMap -> LocationMap -> Bool
--conditions so that the player can move
--it itakes the location, direction, pathmap and locationMap
--returns a boolean(true or false)
can_move "hall" "n" _ locations= get "shield" locations == "holding"
--can move to the arrow room if the player is holding a shield
can_move "hall" "e" _ locations = get "key" locations == "holding"
--can move to the dragon room, if player is holding a key
can_move from direction paths _ =
    elem (from, direction) keys 
    where (keys, _) = unzip paths

cannot_move_because :: Location -> Direction -> Response
--conditions where the player cant move
--takes location and direction and returns a response
cannot_move_because "hall" "n" = "Go forward without a shield? Are you crazy?"
--cannot move to arrows room because player is not holding a shield
cannot_move_because "hall" "e" = "The door appears to be locked."
--cannot go to dragon room because player is not holding the key
cannot_move_because _ _ = "You can't go that way."
--no room available on that direction

move :: Location -> Direction -> PathMap -> Location
--function to move 
--takes current location, direction to move, PAthmap
--returns a new location
move from direction paths = get (from, direction) paths

do_command :: String -> PathMap -> LocationMap -> World
--funtion to move the player 
--takes the input string from the user, pathmap and location map
--returns a "new world"
do_command "n" paths locations = go "n" paths locations
--move n locations, north
do_command "e" paths locations = go "e" paths locations
--move east
do_command "s" paths locations = go "s" paths locations
--move south
do_command "u" paths locations = go "u" paths locations
--go up 
do_command "d" paths locations = down_from_spider "d" paths locations
--whenever user moves down, it calls the function down_from_spider
do_command "w" paths locations = fist_taken "w" paths locations
--whenever user goes west, it tries to validate the fist_taken function

do_command "look" paths locations = look paths locations
--looks for items
do_command "kill" paths locations = kill paths locations
--kills "things" in the map
do_command "i" paths locations = (paths, locations, inventory locations)
--returns player inventory
do_command "quit" paths locations = (paths, locations, "quit")
--command to quit the game
do_command "dump" paths locations =
    (paths, locations, "paths = " ++ show paths ++ "\nlocations = " ++ show locations)
    --returns the possible movememnt to finish the game
    --returns map
do_command cmd paths locations = do_command_2 cmd paths locations

do_command_2 :: String -> PathMap -> LocationMap -> World
--takes a string, pathmap, location and returns world
do_command_2 cmd paths locations
    | isPrefixOf "take " cmd =
    --if user input starts with take
          game_take (tail $ snd $ span isLetter cmd) paths locations
          --calls game_take function
    | isPrefixOf "drop " cmd =
    --if user input starts with drop
          game_drop (tail $ snd $ span isLetter cmd) paths locations
    | otherwise = (paths, locations, "I don't understand: " ++ cmd)
     --calls game_take function
     --if the item user wants to drop is not available 
     --display error response

game_take :: Thing -> PathMap -> LocationMap -> World          
game_take thing paths locations =
    let here = get "myself" locations
    --get player location
        there = get thing locations
        --get item location
    in if here == there
    --if location is the same
       then (paths, (put thing "holding" locations), "OK, taken.")
       --take the item
       else if there == "holding"
       --if the user is already holding it
            then (paths, locations, "You are already holding it.")
            else (paths, locations, "I don't see it here.")
            --else if no item is available
        
game_drop :: Thing -> PathMap -> LocationMap -> World          
game_drop thing paths locations = --(paths, locations, "filler")
    let here = get "myself" locations
    --get player location
        there = get thing locations
        --get item location
    in if there == "holding"
    --if item is being hold
        then (paths, (put thing here locations), "OK, dropped.")
        --creates new location for the item, and user drops it
        else (paths, locations, "You aren't holding it.")

go :: String -> PathMap -> LocationMap -> World
go direction paths locations = do
--function to generate new status for player on every move
    let my_location = get "myself" locations
    --get location
    if can_move my_location direction paths locations
    --if player moves
        then do
            let new_location = move my_location direction paths
            --set new location
            let new_locations = put "myself" new_location locations
            --set user in that location
            let response = describe new_location new_locations
            --return room description for new location
            (paths, new_locations, response)
        else (paths, locations, cannot_move_because my_location direction)
        --else the player can't move and displays error message
        
down_from_spider :: String -> PathMap -> LocationMap -> World
down_from_spider direction paths locations =
--gets player and spider locations
    if get "myself" locations == "spider" &&
    --if spider is alive
       get "spider" locations == "alive" &&
       --player takes the key
       get "key" locations == "holding"
       --put player dead and returns description
           then (paths, put "myself" "dead" locations, description "spiders den3")
           else go direction paths locations 

fist_taken :: String -> PathMap -> LocationMap -> World
fist_taken direction paths locations =
--gets player and dragon locations
     if get "myself" locations == "dragon room" &&
     --if dragon is alive
        get "dragon" locations == "alive" &&
        --player takes the fist
        get "fist" locations == "holding"
        --put player dead and returns description
            then (paths, put "myself" "dead" locations, description "spiders den3")
            else go direction paths locations
            
--when user inputs "look"

look :: PathMap -> LocationMap -> World
look paths locations =

--if no item is there
    if things == []
        then (paths, locations, describe my_location locations)
        --if there is an item
        else (paths, locations, things)
    where my_location = get "myself" locations
          things = items_here locations
--"kill"
kill :: PathMap -> LocationMap -> World
kill paths locations =
    case get "myself" locations of
    --get playher location

        "cave" -> (paths, locations,
        --if player tries to kill spider bellow it
                   "The spider's leg is about as tough as a telephone pole.")
        "spider" ->
        --if player tries to kill the spider when holding a sword
            if get "sword" locations == "holding"
                then (paths,
                      put "spider" "dead" locations,
                      "You hack repeatedly at the spider's back.  Slimy ichor\n" ++
                     "gushes out of the spider''s back, and gets all over you.\n" ++
                     "I think you have killed it, despite the continued twitching.")
                     --if player is not holding a sword
                else (paths,
                      locations,
                      "Beating on the spider's back with your fists has no\n" ++
                      "effect.  This is probably just as well.")
        "dragon" ->
        
        --if player is holding a sword
           if get "sword" locations == "holding"
              then (paths,
                    put "dragon" "dead" locations,
                    --change dragon status to dead
                    "You killed the dragon")
              else (paths,
              --if no sword is available
                    locations,
                    "There is no effect!")
        _ -> (paths, locations, "I see nothing inimical here.")
        
inventory :: LocationMap -> Response
inventory locations =
    let my_stuff = [thing | (thing, "holding") <- locations]
    --checks list of items hold
    in if my_stuff == []
    --if list is empty
        then "You aren't holding anything."
        else intercalate ", " my_stuff
        --if not, then display items

items_here :: LocationMap -> Response
--when user types look
--this function is called
items_here locations =
    let here = get "myself" locations
    --gets player location
        things = ["There is a " ++ thing ++ " here." |
                  (thing, place) <- locations, place == here, thing /= "myself"]
                  --if item is there
    in intercalate "\n" things

-- "get" finds the value of a key in a (key, value) list
get :: Eq a => a -> [(a, String)] -> String
get value list = case lookup value list of
                     Just result -> result
                     Nothing -> "Not found."

put :: Eq t => t -> t1 -> [(t, t1)] -> [(t, t1)]
put key value list =
    let without = filter (\(x, y) -> x /= key) list
    in (key, value) : without

describe :: Location -> LocationMap -> String
--describe function takes location and locationMap
--returns a string with the new location description
describe new_location locations =
    let here = get "myself" locations
    --gets player location
    --gets things locations
        spider_status = get "spider" locations
        dragon_status = get "dragon" locations
        fist_location = get "fist" locations
    in describe_helper here spider_status dragon_status  locations 
    --calls describe_helper

describe_helper :: Location -> String -> String -> LocationMap -> String
--takes location, 2 strings and the locationMap
--returns a string
--requires player location, spider status, dragon status
describe_helper "start" "dead" "dead" locations = description "start2"
--if user is at start, spider is dead, dragon is dead
--display start 2 description
describe_helper "spiders den" "alive" "alive" locations = description "spiders den3"
describe_helper "dragon room" "alive" "alive" locations = description "spiders den3"
describe_helper "spiders den" "dead" _ locations = description "spiders den2"
describe_helper "spider" "dead" _ locations = description "spider2"
--if location is spider, spider is dead, display despription
describe_helper here _ _ locations = description here
--if player moves to invalid place
description :: Location -> String
description "start" =
    "You are at the start of your journey." ++
    "To the north is the hall.\n" ++
    "Your assignment, is to defeat the dragon and win the iron fist"

description "start2" = "Congratulations!!  You have recovered the fist and won the game."

description "weapons room" =
    "You are in the weapon's room. There is hundreads of weapons\n" ++
    "in the wall. Sadly, it's all locked..."

description "arrow room" =
    "You are in the arrows room. An arrow is shot at ypu!\n" ++
    "You block it with your shield. To your left is the weapons room."
    
description "dragon room" =
    "This is a dragon looking at you. The fist is behind him!"

description "hall" =
    "You are in the hall.  The exit is to\n" ++
    "the south; there are two doors, one to west and one to\n" ++
    "the east. There is another door labeled trap in from of you."

description "spiders den" =
    "There is a giant spider here!\n"++
    "kill it to retrieve the key"
    
description "spiders den2" =
    "Yecch!  There is a giant spider here, twitching."

description "spiders den3" =
     "The spider sees you with the key and attacks!!!\n" ++
     "    ...it is over in seconds...."

description "spider" =
    "You are on top of a giant spider."

description "spider2" =
    "Oh, gross!  You are on top of a giant dead spider!"

description someplace = someplace ++ ", and you can't see anything."