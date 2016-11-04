import Math.Vector2 as V2
import Math.Vector3 exposing (..)
import Math.Matrix4 exposing (..)
import WebGL exposing (..)
import Html exposing (Html)
import Html.App as Html
import Html.Attributes exposing (width, height)
import Html exposing (div, text, p)
import AnimationFrame

type alias Vertex = { position : Vec3, color : Vec3 }


line : V2.Vec2 -> V2.Vec2 -> Vec3 -> Drawable Vertex
line from to color =
  Lines
    [( Vertex (vec3 (V2.getX from) (V2.getY from) 0) color
    , Vertex (vec3 (V2.getX to) (V2.getY to) 0) color
    )]


square : V2.Vec2 -> Float -> Vec3 -> Drawable Vertex
square position size color =
  rectangle position size size color


rectangle : V2.Vec2 -> Float -> Float -> Vec3 -> Drawable Vertex
rectangle position width height color =
  let
    x = V2.getX position
    y = V2.getY position
    halfWidth = width / 2
    halfHeight = height / 2
  in
    TriangleFan 
      [ Vertex (vec3 (x + halfWidth) (y + halfHeight) 0) color
      , Vertex (vec3 (x - halfWidth) (y + halfHeight) 0) color
      , Vertex (vec3 (x - halfWidth) (y - halfHeight) 0) color
      , Vertex (vec3 (x + halfWidth) (y - halfHeight) 0) color
      ]


mesh1 = square (V2.vec2 0.0 0.4) 0.2 (vec3 0.2 1.0 0.3)
mesh2 = square (V2.vec2 0.2 0) 0.2 (vec3 0.2 1.0 0.3)
mesh3 = square (V2.vec2 -0.2 0) 0.2 (vec3 0.2 1.0 0.3)
mesh4 = line (V2.vec2 0.0 0.4) (V2.vec2 0.2 0) (vec3 1.0 0.1 0.0)
mesh5 = line (V2.vec2 0.0 0.4) (V2.vec2 -0.2 0) (vec3 1.0 0.1 0.0)
 
main : Program Never
main =
  Html.program
    { init = (0, Cmd.none)
    , view = view
    , subscriptions = (\model -> AnimationFrame.diffs Basics.identity)
    , update = (\elapsed currentTime -> (elapsed + currentTime, Cmd.none))
    }


view : Float -> Html msg
view t =
  WebGL.toHtml
    [ width 400, height 400 ]
    [ WebGL.render vertexShader fragmentShader mesh4 { }
    , WebGL.render vertexShader fragmentShader mesh5 { }
    , WebGL.render vertexShader fragmentShader mesh1 { }
    , WebGL.render vertexShader fragmentShader mesh2 { }
    , WebGL.render vertexShader fragmentShader mesh3 { }
    ]


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
