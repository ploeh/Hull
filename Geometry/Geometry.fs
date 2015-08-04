module Ploeh.Geometry

type Direction = Left = -1 | Straight = 0 | Right = 1

let inline turn (x1, y1) (x2, y2) (x3, y3) =
    let prod = (x2 - x1) * (y3 - y1) - (y2 - y1) * (x3 - x1)
    if prod > LanguagePrimitives.GenericZero then Direction.Left
    elif prod < LanguagePrimitives.GenericZero then Direction.Right
    else Direction.Straight

let inline hull points =
    let inline compareLexigraphic (x1, y1) (x2, y2) = compare (y1, x1) (y2, x2)

    let inline comparePolar p0 p1 p2 = turn p0 p1 p2 |> int

    let p0 = points |> List.sortWith compareLexigraphic |> List.head
    let cmp p1 p2 =        
        let polarCmp = comparePolar p0 p1 p2
        match polarCmp with
        | 0 -> compareLexigraphic p1 p2
        | _ -> polarCmp

    let inline check points =
        let rec checkImp = function
            | [p1; p2; p3] when turn p1 p2 p3 = Direction.Right -> [p1; p3]
            | [p1; p2; p3] -> [p1; p2; p3]
            | p :: ps -> p :: checkImp ps
            | [] -> []
        let newPoints = checkImp points
        newPoints.Length <> points.Length, newPoints

    let inline hullPoints points =
        let mutable ps = []
        for p in points do
            ps <- ps @ [p]
            let mutable shouldCheck = true
            while shouldCheck do
                let wasDiscarded, newPoints = check ps
                shouldCheck <- wasDiscarded
                if wasDiscarded then ps <- newPoints
        ps

    points |> List.sortWith cmp |> Seq.distinct |> hullPoints