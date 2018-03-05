module Data.Picture where
  
import Prelude

import Data.Foldable (foldl)
import Global as Global
import Math as Math
import Data.Maybe

data Point = Point { x :: Number, y :: Number }

showPoint :: Point -> String
showPoint (Point {x, y}) =
    "(" <> show x <> ", " <> show y <> ")"

data Shape
    = Circle Point Number
    | Rectangle Point Number Number
    | Line Point Point
    | Text Point String

showShape :: Shape -> String
showShape (Circle c r) =
    "Circle [center: " <> showPoint c <> ", radius: " <> show r <> "]"
showShape (Rectangle p w h) =
    "Rectangle [center: " <> showPoint p <> ", width: " <> show w <> ", height: " <> show h <> "]"
showShape (Line p1 p2) =
    "Line [start: " <> showPoint p1 <> ", end: " <> showPoint p2 <> "]"
showShape (Text p t) =
    "Text [location: " <> showPoint p <> ", text: " <> show t <> "]"

type Picture = Array Shape

data Bounds = Bounds
    { top :: Number
    , left :: Number
    , bottom :: Number
    , right :: Number
    }

showBounds :: Bounds -> String
showBounds (Bounds b) =
    "Bounds [top: " <> show b.top <>
    ", left: " <> show b.left <>
    ", bottom: " <> show b.bottom <>
    ", right: " <> show b.right <>
    "]"

shapeBounds :: Shape -> Bounds
shapeBounds (Circle (Point {x, y}) r) = Bounds
    { top: y + r
    , left: x - r
    , bottom: y - r
    , right: x + r
    }
shapeBounds (Rectangle (Point {x, y}) w h) = Bounds
    { top: y + h / 2.0
    , left: x - w / 2.0
    , bottom: y - h / 2.0
    , right: x + w / 2.0
    }
shapeBounds (Line (Point {x: x1, y: y1}) (Point {x: x2, y: y2})) = Bounds
    { top: Math.max y1 y2
    , left: Math.min x1 x2
    , bottom: Math.min y1 y2
    , right: Math.max x1 x2
    }
shapeBounds (Text (Point {x, y}) _) = Bounds
    { top: y
    , left: x
    , bottom: y
    , right: x
    }

union :: Bounds -> Bounds -> Bounds
union (Bounds b1) (Bounds b2) = Bounds
    { top: Math.max b1.top b2.top
    , left: Math.min b1.left b2.left
    , bottom: Math.min b1.bottom b2.bottom
    , right: Math.max b1.right b2.right
    }

intersect :: Bounds -> Bounds -> Bounds
intersect (Bounds b1) (Bounds b2) = Bounds
    { top: Math.min b1.top b2.top
    , left: Math.max b1.left b2.left
    , bottom: Math.max b1.bottom b2.bottom
    , right: Math.min b1.right b2.right
    }

emptyBounds :: Bounds
emptyBounds = Bounds
    { top: -Global.infinity
    , left: Global.infinity
    , bottom: Global.infinity
    , right: -Global.infinity
    }

infiniteBounds :: Bounds
infiniteBounds = Bounds
    { top: Global.infinity
    , left: -Global.infinity
    , bottom: -Global.infinity
    , right: Global.infinity
    }

bounds :: Picture -> Bounds
bounds = foldl combine emptyBounds
    where
        combine :: Bounds -> Shape -> Bounds
        combine b s = union b (shapeBounds s)

origin :: Point
origin = Point {x: 0.0, y: 0.0}

transformShape :: Shape -> Shape
transformShape (Circle _ r) =
    Circle origin (r * 2.0)
transformShape (Rectangle _ w h) =
    Rectangle origin (w * 2.0) (h * 2.0)
transformShape s = s

extractText :: Shape -> Maybe String
extractText (Text _ t) = Just (show t)
extractText _ = Nothing

area :: Shape -> Number
area (Circle _ r) = Math.pi * r * r
area (Rectangle _ w h) = w * h
area _ = 0.0