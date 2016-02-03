import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Keyboard
import Text
import Time exposing (..)
import Window
import Random
import Random exposing (..)
import Date


-- Inputs

type alias Input =
  { space : Bool
  , paddle : Int
  , delta : Time
  }

delta : Signal Time
delta =
  Signal.map inSeconds (fps 35)

input : Signal Input
input =
  Signal.sampleOn delta <|
    Signal.map3 Input
      Keyboard.space
      (Signal.map .x Keyboard.arrows)
      delta

(gameWidth,gameHeight) = (600, 400)
(halfWidth,halfHeight) = (300, 200)

-- Model

type alias Object a =
  { a | 
      x : Float
    , y : Float
    , vx : Float
    , vy : Float
    , ax : Float
    , ay : Float
  }

type alias Star =
  Object { layer : Int }

type alias StarField =
  List Star

type alias Player = Object {}

type State = Play | Pause

type alias Game =
  { state : State
  , starField : StarField
  , seed : Seed
  , player : Player
  }

star : Float -> Float -> Int -> Star
star x y layer =
  { x = x
  , y = y
  , vx = 0
  , vy = (toFloat layer) * -15
  , ax = 0
  , ay = 0
  , layer = layer
  }

player = 
  { x = 0
  , y = -halfHeight + 20
  , vx = 0
  , vy = 0
  , ax = 0
  , ay = 0
  }

randX = Random.float -halfWidth halfWidth
randY = Random.float -halfHeight halfHeight
randZ = Random.int 1 3
andMap = Random.map2 (<|)
succeed x = Random.map (always x) Random.bool

starGenerator : Generator Star
starGenerator = 
  star 
    `map`    randX
    `andMap` randY
    `andMap` randZ

starField : Int -> Generator StarField
starField population =
  (Random.list population (starGenerator))

defaultGame : Game
defaultGame =
  let 
    (starField', seed) = generate (starField 100) (initialSeed 1000)
  in
    { state = Pause
    , starField = starField'
    , seed = seed
    , player = player
    }

-- Update

stepObject : Time -> Object a -> Object a
stepObject t ({x, y, vx, vy, ax, ay} as obj) =
  { obj |
    x = x + vx * t
  , y = y + vy * t
  , vx = (vx + ax * t)
  , vy = (vy + ay * t)
  }

sign : Float -> Float
sign n =
  if n == 0 then
    0
  else if n < 0 then
    -1 
  else 
    1

dragObject : Time -> Object a -> Object a
dragObject t obj =
  let
    vx = (obj.vx + ((sign obj.vx) * -600 * t))
    vy = (obj.vy + ((sign obj.vy) * -600 * t))
  in
    { obj |
      vx = if (abs vx) < 0.3 then 0 else vx
    , vy = if (abs vy) < 0.3 then 0 else vy
    }

stepStar : Time -> Star -> Generator Star
stepStar t s =
  if s.y < -halfHeight then
    Random.map2 ((flip star) (s.y + (2 * halfHeight))) randX randZ
  else
    succeed (stepObject t s)

stepStarField : Time -> StarField -> Generator StarField
stepStarField t starField =
  case starField of
    star'::starField' ->
      (::) `map`    (stepStar t star')
           `andMap` (stepStarField t starField')
    [] -> succeed []

stepPlayer : Time -> Int -> Player -> Player
stepPlayer t direction player =
  dragObject t (stepObject t { player | ax = toFloat direction * 1200 })

stepGame : Input -> Game -> Game
stepGame input game =
  let
    {space, paddle, delta} = input
    {state, starField, seed, player} = game

    (starField', seed') = generate (stepStarField delta starField) seed
  in
    { game |
      starField = starField'
    , seed = seed'
    , player = stepPlayer delta paddle player
    }

gameState : Signal Game
gameState =
  Signal.foldp stepGame defaultGame input

-- View

starFieldBlack = rgb 30 30 30
starFieldWhite = rgb 200 200 200
shipLineStyle =
  let
    solidBlue = solid blue
  in
    { solidBlue |
        width = 3
    }

displayObj : Object a -> Shape -> Form
displayObj obj shape =
  move (obj.x, obj.y) (filled starFieldWhite shape)

displayStarField : StarField -> List Form
displayStarField starField =
  List.map (\n -> displayObj n (oval (toFloat n.layer) (toFloat n.layer))) starField

displayShip : Player -> Form
displayShip player =
  move (player.x, player.y) (traced shipLineStyle (path ([(-6.0, 0.0), (0.0, 15.0), (6.0, 0.0)])))

display : (Int, Int) -> Game -> Element
display (w, h) {state, starField, player} =
  container w h middle <|
  collage gameWidth gameHeight
    ([ filled starFieldBlack (rect gameWidth gameHeight) ]
      ++ displayStarField starField ++
     [ displayShip player ]
    )

main =
  Signal.map2 display Window.dimensions gameState