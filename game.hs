import Data.List
import Data.Char

type Location = String
type Direction = String
type Thing = String
type Response = String
--map locations
type PathMap = [((Location, Direction), Location)]
paths :: PathMap
paths = [
    (("spider", "d"), "spiders den"),
    (("spiders den", "u"), "spider"),
    (("spiders den", "e"), "hall"),
    (("hall", "w"), "spiders den"),
    (("hall", "s"), "start"),
    (("hall", "e"), "dragon room"),
    --(("meadow", "n"), "cave entrance"),
    (("start", "n"), "hall"),
    (("dragon room", "w"), "hall"),
    (("hall", "n"), "arrow room"),
    (("arrow room", "s"), "hall"),
    (("arrow room", "w"), "weapons room"),
    (("weapons room", "e"), "arrow room")
    ]
--item locations
type LocationMap = [(Thing, Location)]
locations :: LocationMap
locations =  [
    ("fist", "dragon room"),
    ("key", "spider"),
    ("shield", "start"),
    ("sword", "weapons room"),
    ("myself", "start"),
    -- This is a hack, so I don't have to add more lists to the "World" state
    ("spider", "alive")
    --need to add dragon validation
    ]
--generate world
type World = (PathMap, LocationMap, Response)
world :: IO (PathMap, LocationMap, Response)
world = return (paths, locations, "")
--game start
main :: IO (String)
main = do
    putStrLn "\nWelcome to the Iron fist Game!\n"
    putStrLn instructions
    putStrLn ""
    putStrLn first_room
    play_game $ return (paths, locations, "")
    return "Goodbye!"
    
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
    "You are in a meadow.  To the north is the dark mouth\n" ++
    "of a cave; to the south is a small building.  Your\n" ++
    "assignment, should you decide to accept it, is to\n" ++
    "recover the famed Bar-Abzad ruby and return it to\n" ++
    "this meadow."
    
play_game :: IO (World) -> IO (World)
play_game world = do
    (paths, locations, response) <- world
    putStrLn response
    putStrLn ""
    if game_over locations
        then return ([], [], "")
        else do
            putStr "command> "
            command <- getLine
            if command == "quit"
                then return (paths, locations, "Quitting.")
                else  play_game $ return (do_command command paths locations)

game_over :: LocationMap -> Bool
game_over locations =
    let my_location = get "myself" locations
        fist_location = get "fist" locations
    in my_location == "dead" || (my_location == "start" && fist_location == "holding")
    
can_move :: Location -> Direction -> PathMap -> LocationMap -> Bool
can_move "hall" "n" _ locations= get "shield" locations == "holding"
can_move "hall" "e" _ locations = get "key" locations == "holding"
can_move from direction paths _ =
    elem (from, direction) keys 
    where (keys, _) = unzip paths

cannot_move_because :: Location -> Direction -> Response
cannot_move_because "hall" "n" = "Go forward without a shiel? Are you crazy?"
cannot_move_because "hall" "e" = "The door appears to be locked."
cannot_move_because _ _ = "You can't go that way."

move :: Location -> Direction -> PathMap -> Location
move from direction paths = get (from, direction) paths

do_command :: String -> PathMap -> LocationMap -> World
do_command "n" paths locations = go "n" paths locations
do_command "e" paths locations = go "e" paths locations
do_command "s" paths locations = go "s" paths locations
do_command "w" paths locations = go "w" paths locations
do_command "u" paths locations = go "u" paths locations
do_command "d" paths locations = down_from_spider "d" paths locations
do_command "look" paths locations = look paths locations
do_command "kill" paths locations = kill paths locations
do_command "i" paths locations = (paths, locations, inventory locations)
do_command "quit" paths locations = (paths, locations, "quit")
do_command "dump" paths locations =
    (paths, locations, "paths = " ++ show paths ++ "\nlocations = " ++ show locations)
do_command cmd paths locations = do_command_2 cmd paths locations

do_command_2 :: String -> PathMap -> LocationMap -> World
do_command_2 cmd paths locations
    | isPrefixOf "take " cmd =
          game_take (tail $ snd $ span isLetter cmd) paths locations
    | isPrefixOf "drop " cmd =
          game_drop (tail $ snd $ span isLetter cmd) paths locations
    | otherwise = (paths, locations, "I don't understand: " ++ cmd)

game_take :: Thing -> PathMap -> LocationMap -> World          
game_take thing paths locations =
    let here = get "myself" locations
        there = get thing locations
    in if here == there
       then (paths, (put thing "holding" locations), "OK, taken.")
       else if there == "holding"
            then (paths, locations, "You are already holding it.")
            else (paths, locations, "I don't see it here.")
        
game_drop :: Thing -> PathMap -> LocationMap -> World          
game_drop thing paths locations = --(paths, locations, "filler")
    let here = get "myself" locations
        there = get thing locations
    in if there == "holding"
        then (paths, (put thing here locations), "OK, dropped.")
        else (paths, locations, "You aren't holding it.")

go :: String -> PathMap -> LocationMap -> World
go direction paths locations = do
    let my_location = get "myself" locations
    if can_move my_location direction paths locations
        then do
            let new_location = move my_location direction paths
            let new_locations = put "myself" new_location locations
            let response = describe new_location new_locations
            (paths, new_locations, response)
        else (paths, locations, cannot_move_because my_location direction)

down_from_spider :: String -> PathMap -> LocationMap -> World
down_from_spider direction paths locations =
    if get "myself" locations == "spider" &&
       get "spider" locations == "alive" &&
       get "key" locations == "holding"
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
    --dragon status put dead
        "dragon room" -> (paths,locations,
                   --put "myself" "dead" locations,                   
                   "You killed the dragon")
        "cave" -> (paths, locations,
                   "The spider's leg is about as tough as a telephone pole.")
        "spider" ->
            if get "sword" locations == "holding"
                then (paths,
                      put "spider" "dead" locations,
                      "You hack repeatedly at the spider's back.  Slimy ichor\n" ++
                     "gushes out of the spider''s back, and gets all over you.\n" ++
                     "I think you have killed it, despite the continued twitching.")
                else (paths,
                      locations,
                      "Beating on the spider's back with your fists has no\n" ++
                      "effect.  This is probably just as well.")
        _ -> (paths, locations, "I see nothing inimical here.")
        
inventory :: LocationMap -> Response
inventory locations =
    let my_stuff = [thing | (thing, "holding") <- locations]
    in if my_stuff == []
        then "You aren't holding anything."
        else intercalate ", " my_stuff

items_here :: LocationMap -> Response
items_here locations =
    let here = get "myself" locations
        things = ["There is a " ++ thing ++ " here." |
                  (thing, place) <- locations, place == here, thing /= "myself"]
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
describe new_location locations =
    let here = get "myself" locations
    --meed to add dragon status
        spider_status = get "spider" locations
        fist_location = get "fist" locations
    in describe_helper here spider_status fist_location  locations 

describe_helper :: Location -> String -> String -> LocationMap -> String
describe_helper "start" "dead" "holding" locations = description "start2"
describe_helper "spiders den" "alive" "holding" locations = description "spiders den3"
describe_helper "spiders den" "dead" _ locations = description "spiders den2"
describe_helper "spider" "dead" _ locations = description "spider2"
describe_helper here _ _ locations = description here

description :: Location -> String
description "sart" =
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