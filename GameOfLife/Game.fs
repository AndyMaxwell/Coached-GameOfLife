namespace GameOfLife

open FsUnit
open NUnit.Framework
module Game =
    
    let neighboursOf (x, y) =
        [(x-1,y-1); (x-1,y); (x-1,y+1);
           (x,y-1);            (x,y+1);
         (x+1,y-1); (x+1,y); (x+1,y+1)]
    
    let includeNeighbours pattern =
        pattern
        |> Seq.map neighboursOf
        |> Seq.concat
        |> Seq.append pattern
        |> Set.ofSeq
        
    let applyRules pattern allCells=
        let aBiogenesis cell =
            let checkAlive c = pattern |> Set.contains c
            let isAlive = checkAlive cell
            let livingNeighbourCount = 
                cell
                |> neighboursOf
                |> Seq.filter checkAlive
                |> Seq.length

            match livingNeighbourCount with
            | 2 when isAlive -> Some cell
            | 3 -> Some cell
            | _ -> None
       
        allCells
        |> Seq.map aBiogenesis
        |> Seq.choose id

    let evolve pattern = 
        pattern
        |> includeNeighbours
        |> applyRules pattern

(*
    [<Test>]
    let ``blinker should have a period of 2``() =
        let blinker = [(-1,-1); (-1,0); (-1,1)]
        blinker
        |> Set.ofList 
        |> evolve
        |> evolve
        |> Set.toList
        |> should equal blinker

*)
        


    
        
        
