import Html exposing (Html)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Random
import Keyboard
import Signal
import Time

type alias Position  = (Int, Int)

gameWidth : Int
gameWidth = 64
gameHeight : Int
gameHeight = 32
blockWidth : Int             
blockWidth = 10
blockHeight : Int             
blockHeight = 10
wormStyle : String              
wormStyle = "fill: green;"              

type Game = Game { worm : List Position
                 , length : Int
                 , direction : Position
                 , food : Position
                 , random: Random.Seed }

type GameUpdate = GameUpdate Game | GameOver

          
type Model = Playing Game | NotPlaying

random : Random.Seed -> Int -> Int -> (Int, Random.Seed)
random seed min max = Random.generate (Random.int min max) seed

initialPosition : Position
initialPosition = (32, 16)
                  
initialGame : Game
initialGame = Game { worm = []
                   , length = 5
                   , direction = (1, 0)
                   , food = (10, 10)
                   , random = Random.initialSeed 666 }
type alias Arrows a = {a | x : Int, y : Int }
type Action = TurnAction {x : Int, y : Int} | Tick | StartGame

{-| Functions that are the rules of the game: they take in the current game state
and return a new updated game state or end the game. -}

-- Grow worm to the current direction and remove tail if at target length
grow : Game -> GameUpdate
grow (Game g) =
  let
    (dx,dy) = g.direction
  in
    case g.worm of
      [] ->
        GameUpdate (Game { g | worm = [initialPosition] })
             
      ((headx, heady) :: _) ->
        GameUpdate (Game {g | worm = (headx + dx, heady + dy) :: List.take g.length g.worm })

-- Eat food if current head is at the food position                   
eatFood : Game -> GameUpdate
eatFood (Game g) =
  case g.worm of
    [] -> GameUpdate (Game g)
    (head :: _) ->
      if head == g.food then
        let
          (x, s) = random g.random 0 (gameWidth - 1)
          (y, s') = random s 0 (gameHeight - 1)
        in
          GameUpdate (Game { g | food = (x, y), random = s', length = g.length + 5})
      else
        GameUpdate (Game g)

-- Check if worm has crashed itself                   
crash : Game -> GameUpdate
crash (Game g) =
  case g.worm of
    [] -> GameUpdate (Game g)
    (head :: tail) ->
      if List.any (\pos -> pos == head) tail then
        GameOver
      else
        GameUpdate (Game g)

-- Check if the worm head is outside the game area
outOfBounds : Game -> GameUpdate
outOfBounds (Game g) =
  case g.worm of
    [] -> GameUpdate (Game g)
    ((x,y) :: _) ->
      if x < 0 || x >= gameWidth || y < 0 || y >= gameHeight then
        GameOver
      else
        GameUpdate (Game g)

-- Update worm direction by arrow keys                   
turn : Game -> { a | x : Int, y : Int } -> Game             
turn (Game g) {x, y} =
  let
    (oldx, oldy) = g.direction
  in 
    if x /= 0 then
      Game { g | direction = (x, 0) }
    else if y /= 0 then
      Game { g | direction = (0, -y) }
    else
      Game g

-- Update game state by the game rules (for each tick)           
updateGame : Game -> List (Game -> GameUpdate) -> Model
updateGame game rules =
  case rules of
    [] -> Playing game
    (rule :: rules) ->
      let
        update = rule game
      in
        case update of
          GameUpdate g -> updateGame g rules
          GameOver -> NotPlaying

-- Take in an action and update the game model
update : Action -> Model -> Model
update action model =
  case model of
    Playing game -> 
      case action of
        Tick ->
          updateGame game [grow, eatFood, crash, outOfBounds]

        TurnAction arrows ->
          Playing (turn game arrows)
                  
        _ -> Playing game
             
    NotPlaying ->
      case action of
        StartGame -> Playing initialGame
        _ -> model
          


{-| View part. Render the game in SVG. -}

viewWorm : Position -> Svg
viewWorm (wx, wy) =
  rect [ x (toString (wx * blockWidth))
       , y (toString (wy * blockHeight))
       , width (toString blockWidth)
       , height (toString blockHeight)
       , Svg.Attributes.style wormStyle ] []

viewGame : Game -> List Svg
viewGame (Game g) =
  [ rect [ x "0", y "0"
         , width (toString (gameWidth * blockWidth))
         , height (toString (gameHeight * blockHeight))
         , fill "none"
         , stroke "black" ] []
  , let (fx, fy) = g.food in
    circle [ cx (toString (((toFloat fx) + 0.5) * toFloat blockWidth))
           , cy (toString (((toFloat fy) + 0.5) * toFloat blockHeight))
           , r (toString (toFloat blockWidth / 2))
           , fill "blue" ] []
  , Svg.g [] (List.map viewWorm g.worm)
  ]

view : Model -> Html
view model =
  svg [ version "1.1", width (toString (gameWidth * blockWidth)), height (toString (gameHeight * blockHeight)) ]
      (case model of
         NotPlaying -> [ text' [ x "150", y "100" ] [ text "Press space to play" ] ]
         Playing g -> viewGame g )


{-| Main. Combine three signals (clock timer, arrow keys and space), fold them through 
the game update function and map the view over it. -}

main : Signal Html
main =
  let
    turns = (Signal.map TurnAction Keyboard.arrows)
    ticks = (Signal.map (\ _ -> Tick) (Time.fps 30))
    spaces = (Signal.map (\_ -> StartGame ) Keyboard.space)
  in
    Signal.map view (Signal.foldp update (Playing initialGame) (Signal.mergeMany [turns, ticks, spaces]))
