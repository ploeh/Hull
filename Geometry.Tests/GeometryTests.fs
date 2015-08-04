module Ploeh.Geometry.Tests

open Xunit
open Swensen.Unquote

[<Theory>]
[<InlineData( 0,  0,  1,  0,  1,  1, Direction.Left)>]
[<InlineData( 0,  0, -1, -1,  1, -1, Direction.Left)>]
[<InlineData( 1,  1,  2,  2,  3,  2, Direction.Right)>]
[<InlineData(-2, -3,  2,  2, -3, -9, Direction.Right)>]
[<InlineData( 1,  1,  2,  2,  3,  3, Direction.Straight)>]
[<InlineData(-2,  0,  0,  1,  2,  2, Direction.Straight)>]
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

// No [<ClassData>] attribute in xUnit.net 2.0 :(
type HullDataAttribute() =
    inherit Xunit.Sdk.DataAttribute ()
    override this.GetData testMethod =
        let inline tupleFloat (x, y) = (float x, float y)
        // The following input data comes from randomly generated points.
        // The expected values come from a prototype of the hull function where
        // the output was subsequently visually inspected by drawing the input
        // points and the calculated hull on a coordinate system to verify that
        // the hull prototype function calculated the correct values.
        seq {
            yield
                [|
                    // Points (input):
                    [(3, 1); (3, 7); (2, 5); (2, 4); (1, 6); (2, 3); (1, 2)] |> List.map tupleFloat
                    // Expected:
                    [(3, 1); (3, 7); (1, 6); (1, 2)] |> List.map tupleFloat
                |]
            yield
                [|
                    [(1, -4); (2, 5); (1, 3); (1, -3); (1, -2); (0, 4)] |> List.map tupleFloat
                    [(1, -4); (2, 5); (0, 4)] |> List.map tupleFloat
                |]
            yield
                [|
                    [(1, 1); (0, 3); (-2, 1); (-4, 3); (5, 2); (3, 2); (5, 5); (2, 5); (1, 3); (1, -3); (1, -2); (7, -4); (-1, 1); (-3, 0); (-5, -2); (1, -4); (0, 1); (0, 4); (3, -3); (6, 1)] |> List.map tupleFloat
                    [(1, -4); (7, -4); (6, 1); (5, 5); (2, 5); (-4, 3); (-5, -2)] |> List.map tupleFloat
                |]
            yield
                [|
                    [(-7, -7); (4, -7); (2, 3); (4, 4); (3, 1); (2, -1); (-3, -5); (4, -2); (-1, -7); (-6, 9); (4, 4); (-8, -2); (9, 4); (3, 0); (7, 0); (-7, 3); (0, 9); (4, -7); (-7, -6); (-1, 7); (6, 5); (7, -3); (-8, -8); (-6, -2); (3, 5); (-5, 7); (8, 1); (3, -2); (-9, -4); (-7, 8)] |> List.map tupleFloat
                    [(-8, -8); (4, -7); (7, -3); (9, 4); (0, 9); (-6, 9); (-7, 8); (-9, -4)] |> List.map tupleFloat
                |]
            yield
                [|
                    [(3, -3); (-9, -3); (0, 7); (3, 8); (3, -9); (1, 3); (-9, 5); (-4, 9); (-2, -10); (8, -2); (-4, 2); (-7, -9); (-5, -10); (0, 2); (9, -7); (6, -4); (4, 7); (-9, -7); (2, 1); (-3, -5); (-5, -1); (9, 6); (-3, 1); (6, -6); (-5, -4); (-6, 5); (0, 9); (-2, -9); (-6, -10); (-8, -1); (-4, -9); (8, -1); (-5, -5); (9, -6); (4, -8); (-3, 7); (2, 3); (-8, 6); (3, -4); (3, 4); (-6, -5); (-4, 3); (9, -10); (5, 4); (-1, 9); (9, 1); (-1, 7); (8, -7); (1, -1); (0, -9); (2, 1); (0, -8); (8, -3); (-8, 7); (7, 1); (-2, 8); (-4, -2); (-5, -10); (4, -6); (0, -5); (-1, -6); (5, 4); (-7, 6); (-3, 4); (4, 8); (-6, -7); (5, 2); (-9, 2); (5, -6); (4, 2); (7, 8); (7, 7)] |> List.map tupleFloat
                    [(-6, -10); (-5, -10); (-2, -10); (9, -10); (9, -7); (9, -6); (9, 1); (9, 6); (7, 8); (0, 9); (-1, 9); (-4, 9); (-8, 7); (-9, 5); (-9, 2); (-9, -3); (-9, -7)] |> List.map tupleFloat
                |]
        }

[<Theory; HullData>]
let ``hull returns correct result``
    (points : (float * float) list)
    (expected : (float * float) list) = 

    let actual = hull points
    expected =! actual
