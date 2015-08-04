module Ploeh.Geometry.Tests

open Xunit
open Swensen.Unquote

[<Theory>]
[<InlineData(0, 0, 1, 0, 1, 1, Direction.Left)>]
let ``turn returns correct result``
    (x1 : float)
    (y1 : float)
    (x2 : float)
    (y2 : float)
    (x3 : float)
    (y3 : float)
    (expected : Direction) =
    
    let actual : Direction = turn (x1, y1) (x2, y2) (x3, y3)
    expected =! actual

