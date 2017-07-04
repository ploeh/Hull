module Ploeh.Geometry

type Direction = Left = -1 | Straight = 0 | Right = 1

type Hull<'a> = Hull of ('a * 'a) list

let inline turn (x1, y1) (x2, y2) (x3, y3) =
    let prod = (x2 - x1) * (y3 - y1) - (y2 - y1) * (x3 - x1)
    if prod > LanguagePrimitives.GenericZero then Direction.Left
    elif prod < LanguagePrimitives.GenericZero then Direction.Right
    else Direction.Straight

let inline hull points =
    let compareLexigraphic (x1, y1) (x2, y2) = compare (y1, x1) (y2, x2)

    let comparePolar p0 p1 p2 = turn p0 p1 p2 |> int

    let p0 = points |> List.sortWith compareLexigraphic |> List.head
    let cmp p1 p2 =    
        match comparePolar p0 p1 p2 with
        | 0 -> compareLexigraphic p1 p2
        | x -> x

    let tryDiscard points =
        let rec tryDiscardImp acc = function
            | [p1; p2; p3] when turn p1 p2 p3 = Direction.Right ->
                p3 :: p1 :: acc
            | [p1; p2; p3] -> p3 :: p2 :: p1 :: acc
            | p :: ps -> tryDiscardImp (p :: acc) ps
            | [] -> acc
        let newPoints = tryDiscardImp [] points |> List.rev
        if newPoints.Length <> points.Length
        then Some newPoints
        else None

    let rec discardFrom candidates =
        match tryDiscard candidates with
        | Some newCandidates -> discardFrom newCandidates
        | None -> candidates

    let rec hullPoints candidates = function
        | [] -> candidates
        | p :: tail -> hullPoints (discardFrom (candidates @ [p])) tail

    points
    |> List.sortWith cmp
    |> Seq.distinct
    |> Seq.toList
    |> hullPoints []
    |> Hull