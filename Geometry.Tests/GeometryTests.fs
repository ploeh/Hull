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
        }

[<Theory; HullData>]
let ``hull returns correct result``
    (points : (float * float) list)
    (expected : (float * float) list) = 

    let actual = hull points
    expected =! actual
