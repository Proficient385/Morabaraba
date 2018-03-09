open System
open System.IO

type Cell =
| Y
| M
| Blank

type GameBoard =
| Board of (Cell*Cell*Cell)*(Cell*Cell*Cell)*(Cell*Cell*Cell)*(Cell*Cell*Cell)*(Cell*Cell*Cell)*(Cell*Cell*Cell)*(Cell*Cell*Cell)*(Cell*Cell*Cell)

// defining the functions that will operate on this data

let isBlank game position =
    match position, game with
    | "A1", Board ((Blank,_,_),_,_,_,_,_,_,_) -> true
    | "A4", Board ((_,Blank,_),_,_,_,_,_,_,_) -> true
    | "A7", Board ((_,_,Blank),_,_,_,_,_,_,_) -> true         // ~A
    | "B2", Board (_,(Blank,_,_),_,_,_,_,_,_) -> true
    | "B4", Board (_,(_,Blank,_),_,_,_,_,_,_) -> true
    | "B6", Board (_,(_,_,Blank),_,_,_,_,_,_) -> true         // ~B
    | "C3", Board (_,_,(Blank,_,_),_,_,_,_,_) -> true
    | "C4", Board (_,_,(_,Blank,_),_,_,_,_,_) -> true
    | "C5", Board (_,_,(_,_,Blank),_,_,_,_,_) -> true
    | "D1", Board (_,_,_,(Blank,_,_),_,_,_,_) -> true
    | "D2", Board (_,_,_,(_,Blank,_),_,_,_,_) -> true
    | "D3", Board (_,_,_,(_,_,Blank),_,_,_,_) -> true
    | "D5", Board (_,_,_,_,(Blank,_,_),_,_,_) -> true
    | "D6", Board (_,_,_,_,(_,Blank,_),_,_,_) -> true
    | "D7", Board (_,_,_,_,(_,_,Blank),_,_,_) -> true
    | "E3", Board (_,_,_,_,_,(Blank,_,_),_,_) -> true
    | "E4", Board (_,_,_,_,_,(_,Blank,_),_,_) -> true
    | "E5", Board (_,_,_,_,_,(_,_,Blank),_,_) -> true
    | "F2", Board (_,_,_,_,_,_,(Blank,_,_),_) -> true
    | "F4", Board (_,_,_,_,_,_,(_,Blank,_),_) -> true
    | "F6", Board (_,_,_,_,_,_,(_,_,Blank),_) -> true
    | "G1", Board (_,_,_,_,_,_,_,(Blank,_,_)) -> true
    | "G4", Board (_,_,_,_,_,_,_,(_,Blank,_)) -> true
    | "G7", Board (_,_,_,_,_,_,_,(_,_,Blank)) -> true
    | _ -> false

let blankBoard =
    let blankRow = Blank, Blank, Blank
    Board (blankRow, blankRow, blankRow,blankRow,blankRow,blankRow,blankRow,blankRow)

let printBoard (Board (r1, r2, r3,r4,r5,r6,r7,r8)) =
    System.Console.Clear ()
    let cell offset value n =
        match value with
        | Y -> " Y  "
        | M -> " M  "
        |Blank -> "   "
        //| //Blank -> string (offset+n)
    let printRow (x,y,z) offset =
        let cell = cell offset
        printfn "\t\t\tA\t |%s| \t\t\t\t|%s|\t\t\t\t|%s|" (cell x 1) (cell y 2) (cell z 3)
    let printRow2 (x,y,z) offset =
        let cell = cell offset
        printfn "\t\t\tB\t |\t |%s| \t\t\t|%s|\t\t\t|%s| \t    |" (cell x 1) (cell y 2) (cell z 3)
    let printRow3 (x,y,z) offset =
        let cell = cell offset
        printfn "\t\t\tC\t |\t |\t |%s| \t\t|%s|\t\t|%s|\t    | \t    |" (cell x 1) (cell y 2) (cell z 3)
    let printRow4 (x,y,z) offset =
        let cell = cell offset
        printfn "\t\t\tD\t |%s| \t |%s|\t |%s|" (cell x 1) (cell y 2) (cell z 3)
    let printRow5 (x,y,z) offset =
        let cell = cell offset
        printfn "\t\t\t\t\t\t\t\t\t\t |%s| \t|%s|\t|%s|" (cell x 1) (cell y 2) (cell z 3)
    let printRow8 (x,y,z) offset =
        let cell = cell offset
        printfn "\t\t\tG\t |%s| \t\t\t\t|%s|\t\t\t\t|%s|" (cell x 1) (cell y 2) (cell z 3)
    let printRow7 (x,y,z) offset =
        let cell = cell offset
        printfn "\t\t\tF\t |\t |%s| \t\t\t|%s|\t\t\t|%s| \t    |" (cell x 1) (cell y 2) (cell z 3)
    let printRow6 (x,y,z) offset =
        let cell = cell offset
        printfn "\t\t\tE\t |\t |\t |%s| \t\t|%s|\t\t|%s|\t    | \t    |" (cell x 1) (cell y 2) (cell z 3)
    let printSeparator () = printfn "\t\t\t\t |------------------------------------------------------------------|"
    //let printSeparator () = printfn "\t\t\t\t --\t\t\t\t+---\t\t\t\t+--"
    //let printSeparator2 () = printfn "\t\t\t\t\t --\t\t\t+---\t\t\t+--"
    let printSeparator2 () = printfn "\t\t\t\t |\t |--------------------------------------------------|\t\t|"
    //let printSeparator3 () = printfn "\t\t\t\t\t|\t --\t\t+---\t\t+--"
    let printSeparator3 () = printfn "\t\t\t\t |\t |\t ------------------------------------ \t    |"
    let printSeparator4 () = printfn "\t\t\t\t |--------------------\t\t\t\t\t\t\t|"
    let printSeparator5 () = printfn "\t\t\t\t\t\t\t\t\t\t --------------------"
    printfn "\n\n\n\n\n\n\n\t\t\t\t   1 \t   2 \t   3 \t\t  4 \t   \t  5 \t  6 \t  7 \n\n "
    printRow r1 0
    printSeparator ()
    printfn "\t\t\t\t |\t\t\t\t\t\t\t\t\t|\n"
    printRow2 r2 3
    printSeparator2 ()
    printfn "\t\t\t\t |\t\t\t\t\t\t\t\t\t|\n"
    printRow3 r3 6
    printSeparator3 ()
    printfn "\t\t\t\t |\t\t\t\t\t\t\t\t\t|\n"
    printfn "\t\t\t\t |\t\t\t\t\t\t\t\t\t|\n"
    printRow4 r4 9
    printSeparator4 ()
    printRow5 r5 12
    printSeparator5 ()
    printfn "\t\t\t\t |\t\t\t\t\t\t\t\t\t|\n"
    printfn "\t\t\t\t |\t\t\t\t\t\t\t\t\t|\n"
    printRow6 r6 15
    printSeparator3 ()
    printfn "\t\t\t\t |\t\t\t\t\t\t\t\t\t|\n"
    printRow7 r7 18
    printSeparator2 ()
    printfn "\t\t\t\t |\t\t\t\t\t\t\t\t\t|\n"
    printRow8 r8 21
    printSeparator ()
  

let makeMove symbol (Board (r1,r2,r3,r4,r5,r6,r7,r8)) position =
    let newBoard =
        let changeCol col (a,b,c) =
            match col with
            | 0 -> symbol,b,c
            | 1 -> a,symbol,c
            | 2 -> a,b,symbol
            | _ -> failwith "NOOOOOOOOooooooooooooooo it's all gone to ......... the dogs"
        let data =
            match position with
            | "A1" -> changeCol 0 r1,r2,r3,r4,r5,r6,r7,r8
            | "A4" -> changeCol 1 r1,r2,r3,r4,r5,r6,r7,r8
            | "A7" -> changeCol 2 r1,r2,r3,r4,r5,r6,r7,r8
            | "B2" -> r1,changeCol 0 r2,r3,r4,r5,r6,r7,r8
            | "B4" -> r1,changeCol 1 r2,r3,r4,r5,r6,r7,r8
            | "B6" -> r1,changeCol 2 r2,r3,r4,r5,r6,r7,r8
            | "C3" -> r1,r2,changeCol 0 r3,r4,r5,r6,r7,r8
            | "C4" -> r1,r2,changeCol 1 r3,r4,r5,r6,r7,r8
            | "C5" -> r1,r2,changeCol 2 r3,r4,r5,r6,r7,r8
            | "D1" -> r1,r2,r3,changeCol 0 r4,r5,r6,r7,r8
            | "D2" -> r1,r2,r3,changeCol 1 r4,r5,r6,r7,r8
            | "D3" -> r1,r2,r3,changeCol 2 r4,r5,r6,r7,r8
            | "D5" -> r1,r2,r3,r4,changeCol 0 r5,r6,r7,r8
            | "D6" -> r1,r2,r3,r4,changeCol 1 r5,r6,r7,r8
            | "D7" -> r1,r2,r3,r4,changeCol 2 r5,r6,r7,r8
            | "E3" -> r1,r2,r3,r4,r5,changeCol 0 r6,r7,r8
            | "E4" -> r1,r2,r3,r4,r5,changeCol 1 r6,r7,r8
            | "E5" -> r1,r2,r3,r4,r5,changeCol 2 r6,r7,r8
            | "F2" -> r1,r2,r3,r4,r5,r6,changeCol 0 r7,r8
            | "F4" -> r1,r2,r3,r4,r5,r6,changeCol 1 r7,r8
            | "F6" -> r1,r2,r3,r4,r5,r6,changeCol 2 r7,r8
            | "G1" -> r1,r2,r3,r4,r5,r6,r7,changeCol 0 r8
            | "G4" -> r1,r2,r3,r4,r5,r6,r7,changeCol 1 r8
            | "G7" -> r1,r2,r3,r4,r5,r6,r7,changeCol 2 r8
            | _ -> failwith "i hate myself"
        Board data
    let k = newBoard
    k

 (*
let rec run player game =
    // need to find the blank cells that can be used...
    printBoard game
    printfn "%A's turn.  Type the number of the cell that you want to play into." player
    let n = System.Console.ReadLine()
    match n with
    | "A1" | "A4" | "A7" | "B2" | "B4" | "B6" | "C3" | "C4" | "C5"| "D1" | "D2" | "D3" | "D5" | "D6" | "D7" | "E3" | "E4" | "E5" | "F2" | "F4" | "F6" | "G1" | "G4" | "G7"  ->
        match isBlank game n with
        | true -> makeMove player game n
        | _ -> run player game
    | _ -> run player game

let rec runGame currentPlayer game =
    let playAgain () =
        printfn "Play again? [y/N] "
        match System.Console.ReadLine() with
        | "Y" | "y" -> runGame Y blankBoard
        | _ -> ()
    playAgain ()
*)
//let (a1,a4,a7): Cell = (Blank, Blank, Blank) 
//a1 a4 a7 b2 b4 b6 c3 c4 c5 d1 d2 d3 d5 d6 d7 e3 e4 e5 f2 f4 f6 g1 g4 g7

//let board = 
          // let 
           
let printBoard2 (Board (r1, r2, r3,r4,r5,r6,r7,r8))= 
                   
                  //System.Console.Clear ()
                  let tc value = 
                            match value with
                            |Y -> 'Y'
                            |M -> 'M'
                            |_ -> ' '
                  let a1, a4, a7 = r1
                  let b2, b4, b6 = r2
                  let c3, c4, c5 = r3
                  let d1, d2, d3 = r4
                  let d5, d6, d7 = r5
                  let e3, e4, e5 = r6
                  let f2, f4, f6 = r6
                  let g1, g4, g7 = r8


                  let printBdA = printfn " %A ______________%A______________%A" (tc a1) (tc a4) (tc a7)
                                 printfn " |  \               |              / |"
                                 printfn " |   \              |             /  | "
                                 printfn " |    \             |            /   |"
                  let printBdB = printfn " |    %A__________%A_________%A   |" (tc b2) (tc b4) (tc b6)
                                 printfn " |     |\           |          /|    |"
                                 printfn " |     | \          |         / |    |"
                                 printfn " |     |  \         |        /  |    |"
                  let printBdC = printfn " |     |   %A_____%A____%A   |    |" (tc c3) (tc c4) (tc c5)
                                 printfn " |     |    |              |    |    |"
                                 printfn " |     |    |              |    |    |"
                                 printfn " |     |    |              |    |    |"
                  let printBdD =
                                 printfn " %A__%A__%A            %A__%A__%A" (tc d1) (tc d2) (tc d3) (tc d5) (tc d6) (tc d7)
                                 printfn " |     |    |              |    |    |"
                                 
                  let printBdE = printfn " |     |    |              |    |    |"
                                 printfn " |     |    |              |    |    |"
                                 printfn " |     |   %A_____%A____%A   |    |" (tc e3) (tc e4) (tc e5)
                         
                  let printBdF = printfn " |     |   /        |        \  |    |"
                                 printfn " |     |  /         |         \ |    |"
                                 printfn " |     | /          |          \|    |"
                                 printfn " |    %A__________%A_________%A   |" (tc f2) (tc f4) (tc f6)

                  let printBdG  = printfn " |    /             |            \   |"
                                  printfn " |   /              |             \  | "
                                  printfn " |  /               |              \ |"
                                  printfn "  %A _____________%A_____________%A" (tc g1) (tc g4) (tc g7)    
                  printBdA
                  printBdB
                  printBdC
                  printBdD
                  printBdE
                  printBdF
                  printBdG

[<EntryPoint>]
let main argv =
    //runGame Y blankBoard
    printBoard blankBoard
    printf "\n\n\n"
    printBoard2  blankBoard
    

    Console.Read()

    //runGame X blankBoard
    0 // ret
