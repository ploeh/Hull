module Ploeh.Geometry

type Direction = Left = -1 | Straight = 0 | Right = 1

let inline turn (x1, y1) (x2, y2) (x3, y3) = Direction.Left