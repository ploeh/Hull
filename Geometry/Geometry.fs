module Ploeh.Geometry

type Direction = Left = -1 | Straight = 0 | Right = 1

let inline turn (x1, y1) (x2, y2) (x3, y3) =
    let prod = (x2 - x1) * (y3 - y1) - (y2 - y1) * (x3 - x1)
    if prod > LanguagePrimitives.GenericZero then Direction.Left
    elif prod < LanguagePrimitives.GenericZero then Direction.Right
    else Direction.Straight

let inline hull points = [(3., 1.); (3., 7.); (1., 6.); (1., 2.)]