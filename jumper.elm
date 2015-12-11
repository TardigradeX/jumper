import Color exposing (..)
import Debug
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Keyboard
import Time exposing (..)
import Window


-- MODEL

type alias Model =
    { x : Float
    , y : Float
    , vx : Float
    , vy : Float
    , dir : Direction
    }

type alias World = {xSize : Int, ySize : Int }

type Direction = Left | Right

type alias Keys = { x:Int, y:Int }


ball : Model
ball =
    { x = 0
    , y = 0
    , vx = 0
    , vy = 0
    , dir = Right
    }

gameWorld : World
gameWorld =
    { x = 0
    , y = 0
    }


-- UPDATE

update : (Float, Keys) -> Model -> Model
update (dt, keys) ball =
    ball
        |> gravity dt
        |> jump keys
        |> walk keys
        |> physics dt


jump : Keys -> Model -> Model
jump keys ball =
    if keys.y > 0 && ball.vy == 0
      then { ball | vy = 20.0 }
      else ball


gravity : Float -> Model -> Model
gravity dt ball =
    { ball |
        vy =
        if ball.y > 0 then
          ball.vy - dt/2
        else if (ball.y == 0 && ball.vy < -8) then
          -ball.vy * 0.8
        else
          0
    }


physics : Float -> Model -> Model
physics dt ball =
    { ball |
        x = ball.x + dt * ball.vx,
        y =  max 0 (ball.y + dt * ball.vy)
    }


walk : Keys -> Model -> Model
walk keys ball =
    { ball |
        vx = toFloat keys.x * 4,
        dir =
          if  keys.x < 0 then
            Left
          else if keys.x > 0 then
            Right
          else
            ball.dir
    }


-- VIEW

view : (Int, Int) -> Model -> Element
view (w',h') ball =
  let (w,h) = (toFloat w', toFloat h')

      dir =
        case ball.dir of
          Left -> "left"
          Right -> "right"

      groundY = 50 - h/2

      position = (ball.x, ball.y + groundY)
  in
      collage w' h'
          [ rect w h
              |> filled (rgb 0 0 0)
          , rect w 50
              |> filled (rgb 145 156 100)
              |> move (0, 20-h/2)
          , oval 10 10
              |> filled (rgb 200 200 200)
              |> move position
          ]


-- SIGNALS

main : Signal Element
main =
  Signal.map2 view Window.dimensions (Signal.foldp update ball input)


input : Signal (Float, Keys)
input =
  let delta = Signal.map (\t -> t/10) (fps 30)
      deltaArrows = Signal.map2 (,) delta  Keyboard.arrows
  in
      Signal.sampleOn delta deltaArrows
