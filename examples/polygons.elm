import Math.Vector2 as V2
import Math.Vector3 exposing (..)
import Math.Matrix4 exposing (..)
import WebGL exposing (..)
import Html exposing (Html)
import Html.App as Html
import Html.Attributes as Attr
import Html.Events as Evt
import Html exposing (div, text, code, article, input, section, aside, label)
import String
import AnimationFrame


main : Program Never
main =
  Html.beginnerProgram
    { model = initialModel
    , view = view
    , update = update
    }


initialModel =
  { sides = 5
  , radius = 0.5
  , color = vec3 1.0 0.6 0.0
  , mesh = ngon (V2.vec2 0 0) 5 0.5 (vec3 1.0 0.6 0.0)
  }

type alias Vertex = { position : Vec3, color : Vec3 }

type alias Model =
  { sides : Int
  , radius : Float
  , color : Vec3
  , mesh : Drawable Vertex
  }

type Msg
  = Sides Int
  | Radius Float
  --| Color Vec3


update : Msg -> Model -> Model
update msg model =
  case msg of
    Sides sides ->
      let 
        mesh = ngon (V2.vec2 0 0) sides model.radius model.color
      in
        { model | sides = sides, mesh = mesh }
    Radius radius ->
      let 
        mesh = ngon (V2.vec2 0 0) model.sides radius model.color
      in
        { model | radius = radius, mesh = mesh }

parseInt s =
  case (String.toInt s) of
    Ok n -> n
    Err e -> 0

parseFloat s =
  case (String.toFloat s) of
    Ok n -> n
    Err e -> 0.0


view : Model -> Html Msg
view model =
  article []
  [ aside [ Attr.style [ ("position", "absolute"), ("width", "400px"), ("right", "0") ] ]
    [ div []
      [ label [] [ text "Sides" ]
      , input
        [ Attr.type' "number"
        , Attr.min "3"
        , Attr.max "20"
        , Attr.step "1"
        , Attr.value (toString model.sides)
        , Evt.onInput (\v -> Sides (parseInt v))
        ] []
      ]
    , div []
      [ label [] [ text "Radius" ]
      , input
        [ Attr.type' "number"
        , Attr.min "0.01"
        , Attr.max "1.0"
        , Attr.step "0.01"
        , Attr.value (toString model.radius)
        , Evt.onInput (\v -> Radius (parseFloat v))
        ] []
      ]
    , div []
      [ code [] [ text (toString model.mesh) ] ]
    ]
  , section []
    [ WebGL.toHtml
      [ Attr.width 400, Attr.height 400 ]
      [ WebGL.render vertexShader fragmentShader model.mesh { } ]
    ]
  ]


nextNGonPoint : Float -> Float -> Int -> List V2.Vec2 -> List V2.Vec2
nextNGonPoint scale alpha current acc =
  let
    -- the point which is radius from position in direction angle
    x = scale * cos (alpha * (toFloat current))
    y = scale * sin (alpha * (toFloat current))
    vertex = V2.vec2 x y
    next = current - 1
  in
    if current == 0 then
      vertex::acc
    else
      nextNGonPoint scale alpha next (vertex::acc)

ngon : V2.Vec2 -> Int -> Float -> Vec3 -> Drawable Vertex
ngon position sides radius color =
  let
    alpha : Float
    alpha = (pi * 2.0 / (toFloat sides)) --+ (pi/4.0)
    vertices = nextNGonPoint radius alpha (sides - 1) []
    vec2ToVertex = (\v -> 
      { position = vec3 (V2.getX v) (V2.getY v) 0, color = color }
    )
  in
    List.map vec2ToVertex vertices
      |> TriangleFan

    
-- Shaders

vertexShader :
  Shader
  { attr | position: Vec3, color: Vec3 }
  { }
  { vcolor:Vec3 }
vertexShader = [glsl|

attribute vec3 position;
attribute vec3 color;
varying vec3 vcolor;

void main () {
    gl_Position = vec4(position, 1.0);
    gl_PointSize = 4.0;
    vcolor = color;
}

|]


fragmentShader : Shader {} u { vcolor: Vec3 }
fragmentShader = [glsl|

precision mediump float;
varying vec3 vcolor;

void main () {
    gl_FragColor = vec4(vcolor, 1.0);
}

|]
