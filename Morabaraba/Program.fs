open System
open System.IO

type Cell =
| Y
| M
| Blank

type Mill = 
| One 
| Zero 

type GameBoard =
| Board of (Cell*Cell*Cell)*(Cell*Cell*Cell)*(Cell*Cell*Cell)*(Cell*Cell*Cell)*(Cell*Cell*Cell)*(Cell*Cell*Cell)*(Cell*Cell*Cell)*(Cell*Cell*Cell)

type Result =
| Ongoing of GameBoard
| Winner of Cell * GameBoard
| Draw

let swapPlayer x =
    match x with
    | Y -> M
    | M -> Y
    | Blank -> failwith "RUN FOR YOUR LIVES THE END IS NIGH"

(*let listOfNulled = []

let nulledComb game =
    let (Board (r1,r2,r3,r4,r5,r6,r7,r8)) = game
    let xs = [r1;r2;r3;r4;r5;r6;r7;r8]
    let rec nullTuple list =
     match list with
     |[] -> (Y,Y,Y)
     |_::rest ->
        match list.Head with
        | (Y,Y,Y) -> list.Head
        | _ -> nullTuple rest
     
    (nullTuple xs)::listOfNulled
    *)
let eliminate (Board (r1,r2,r3,r4,r5,r6,r7,r8)) position =
    let newBoard =
        let changeCol col (a,b,c) =
            match col with
            | 0 -> Blank,b,c
            | 1 -> a,Blank,c
            | 2 -> a,b,Blank
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
    Ongoing newBoard

let m1 = [0]
let mill_Y_Check game =
    let (Board (r1,r2,r3,r4,r5,r6,r7,r8)) = game
    
    match r1,r2,r3,r4,r5,r6,r7,r8 with
    | (Y,Y,Y),_,_,_,_,_,_,_ -> match m1 with
                               | 0::(1::m1) ->  printf ("Y has mill, pick M's position to eliminate ")
                               | 1::(0::m1) ->  printf ("")
    | _,(Y,Y,Y),_,_,_,_,_,_     // r2 Y mill ~B
    | _,_,(Y,Y,Y),_,_,_,_,_     // r3 Y mill ~C
    | _,_,_,(Y,Y,Y),_,_,_,_     // r4 Y mill ~D1 ~Middle
    | _,_,_,_,(Y,Y,Y),_,_,_     // r5 Y mill ~D2 ~Middle
    | _,_,_,_,_,(Y,Y,Y),_,_     // r6 Y mill ~E
    | _,_,_,_,_,_,(Y,Y,Y),_     // r7 Y mill ~F
    | _,_,_,_,_,_,_,(Y,Y,Y) ->  // r8 Y mill ~G
         printf ("Y has mill, pick M's position to eliminate: ")
         //let n = Console.ReadLine()
         //eliminate game n
    | _-> printf ("") 

let mill_Y_Check2 game =
    let (Board (r1,r2,r3,r4,r5,r6,r7,r8)) = game
    match r1,r2,r3,r4,r5,r6,r7,r8 with
    | (Y,_,_),(Y,_,_),(Y,_,_),_,_,_,_,_    // A1-B2-C3 Y mill
    | (_,Y,_),(_,Y,_),(_,Y,_),_,_,_,_,_      // A4-B4-C4 Y mill ~Middle
    | (_,_,Y),(_,_,Y),(_,_,Y),_,_,_,_,_      // A7-B6-C5 Y mill
    | (Y,_,_),_,_,(Y,_,_),_,_,_,(Y,_,_)      // A1-D1-G1 Y mill ~leftmost vertical
    | _,(Y,_,_),_,(_,Y,_),_,_,(Y,_,_),_      // B2-D2-F2 Y mill
    | _,_,(Y,_,_),(_,_,Y),_,(Y,_,_),_,_      // C3-D3-E3 Y mill
    | _,_,_,_,_,(Y,_,_),(Y,_,_),(Y,_,_) ->   // G1-F2-E3 Y mill
         printfn("Hey Y has mill")
    | _-> printf("") 

let mill_Y_Check3 game =
    let (Board (r1,r2,r3,r4,r5,r6,r7,r8)) = game
    match r1,r2,r3,r4,r5,r6,r7,r8 with
    | _,_,_,_,_,(_,Y,_),(_,Y,_),(_,Y,_)     // G4-F4-E4 Y mill ~Middle
    | _,_,_,_,_,(_,_,Y),(_,_,Y),(_,_,Y)     // G7-F6-E5 Y mill
    | (_,_,Y),_,_,_,(_,_,Y),_,_,(_,_,Y)     // A7-D7-G7 Y mill ~rightmost vertical
    | _,(_,_,Y),_,_,(_,Y,_),_,(_,_,Y),_     // B6-D6-F6 Y mill
    | _,_,(_,_,Y),_,(Y,_,_),(_,_,Y),_,_ ->  // G4-F4-E4 Y mill  
        printfn("Hey Y has milled now ! ") 
    | _-> printf("") 

let mill_M_Check game =
    let (Board (r1,r2,r3,r4,r5,r6,r7,r8)) = game
    match r1,r2,r3,r4,r5,r6,r7,r8 with
    | (M,M,M),_,_,_,_,_,_,_     // r1 M mill ~A                         
    | _,(M,M,M),_,_,_,_,_,_     // r2 M mill ~B
    | _,_,(M,M,M),_,_,_,_,_     // r3 M mill ~C
    | _,_,_,(M,M,M),_,_,_,_     // r4 M mill ~D1 ~Middle
    | _,_,_,_,(M,M,M),_,_,_     // r5 M mill ~D2 ~Middle
    | _,_,_,_,_,(M,M,M),_,_     // r6 M mill ~E
    | _,_,_,_,_,_,(M,M,M),_     // r7 M mill ~F
    | _,_,_,_,_,_,_,(M,M,M) ->  // r8 Y mill ~G 
         printfn("Hey M has mill")    // r8 M mill ~G *)
    | _ -> printf("") 
 
let mill_M_Check2 game =
    let (Board (r1,r2,r3,r4,r5,r6,r7,r8)) = game
    match r1,r2,r3,r4,r5,r6,r7,r8 with
    | (M,_,_),(M,_,_),(M,_,_),_,_,_,_,_      // A1-B2-C3 M mill
    | (_,M,_),(_,M,_),(_,M,_),_,_,_,_,_      // A4-B4-C4 M mill ~Middle
    | (_,_,M),(_,_,M),(_,_,M),_,_,_,_,_      // A7-B6-C5 M mill
    | (M,_,_),_,_,(M,_,_),_,_,_,(M,_,_)      // A1-D1-G1 M mill ~leftmost vertical
    | _,(M,_,_),_,(_,M,_),_,_,(M,_,_),_      // B2-D2-F2 M mill
    | _,_,(M,_,_),(_,_,M),_,(M,_,_),_,_      // C3-D3-E3 M mill
    | _,_,_,_,_,(M,_,_),(M,_,_),(M,_,_) ->   // G1-F2-E3 M mill
         printfn("HeM M has mill")
    | _-> printf("") 

let mill_M_Check3 game =
    let (Board (r1,r2,r3,r4,r5,r6,r7,r8)) = game
    match r1,r2,r3,r4,r5,r6,r7,r8 with
    | _,_,_,_,_,(_,M,_),(_,M,_),(_,M,_)     // G4-F4-E4 M mill ~Middle
    | _,_,_,_,_,(_,_,M),(_,_,M),(_,_,M)     // G7-F6-E5 M mill
    | (_,_,M),_,_,_,(_,_,M),_,_,(_,_,M)     // A7-D7-G7 M mill ~rightmost vertical
    | _,(_,_,M),_,_,(_,M,_),_,(_,_,M),_     // B6-D6-F6 M mill
    | _,_,(_,_,M),_,(M,_,_),(_,_,M),_,_ ->  // G4-F4-E4 M mill  
        printfn("HeM M has milled now ! ") 
    | _-> printf("") 

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

let printBoard (Board (r1, r2, r3,r4,r5,r6,r7,r8))= 
                   
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
                  let f2, f4, f6 = r7
                  let g1, g4, g7 = r8

                  let top = printf "\n\n\n\t\t\t\t  1    2    3       4      5    6    7 \n\n\n"
                  let printBdA = printfn "\t\t\tA\t %A ______________%A______________%A" (tc a1) (tc a4) (tc a7)
                                 printfn "\t\t\t\t |  \               |              / |"
                                 printfn "\t\t\t\t |   \              |             /  | "
                                 printfn "\t\t\t\t |    \             |            /   |"
                  let printBdB = printfn "\t\t\tB\t |    %A__________%A_________%A   |" (tc b2) (tc b4) (tc b6)
                                 printfn "\t\t\t\t |     |\           |          /|    |"
                                 printfn "\t\t\t\t |     | \          |         / |    |"
                                 printfn "\t\t\t\t |     |  \         |        /  |    |"
                  let printBdC = printfn "\t\t\tC\t |     |   %A_____%A____%A   |    |" (tc c3) (tc c4) (tc c5)
                                 printfn "\t\t\t\t |     |    |              |    |    |"
                                 printfn "\t\t\t\t |     |    |              |    |    |"
                                 printfn "\t\t\t\t |     |    |              |    |    |"
                  let printBdD =
                                 printfn "\t\t\tD\t %A__%A__%A            %A__%A__%A" (tc d1) (tc d2) (tc d3) (tc d5) (tc d6) (tc d7)
                                 printfn "\t\t\t\t |     |    |              |    |    |"
                                 
                  let printBdE = printfn "\t\t\t\t |     |    |              |    |    |"
                                 printfn "\t\t\t\t |     |    |              |    |    |"
                                 printfn "\t\t\tE\t |     |   %A_____%A____%A   |    |" (tc e3) (tc e4) (tc e5)
                         
                  let printBdF = printfn "\t\t\t\t |     |   /        |        \  |    |"
                                 printfn "\t\t\t\t |     |  /         |         \ |    |"
                                 printfn "\t\t\t\t |     | /          |          \|    |"
                                 printfn "\t\t\tF\t |    %A__________%A_________%A   |" (tc f2) (tc f4) (tc f6)

                  let printBdG  = printfn "\t\t\t\t |    /             |            \   |"
                                  printfn "\t\t\t\t |   /              |             \  | "
                                  printfn "\t\t\t\t |  /               |              \ |"
                                  printfn "\t\t\tG\t  %A _____________%A_____________%A" (tc g1) (tc g4) (tc g7)    
                  printBdA
                  printBdB
                  printBdC
                  printBdD
                  printBdE
                  printBdF
                  printBdG

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
    mill_M_Check newBoard
    mill_M_Check2 newBoard
    mill_M_Check3 newBoard
    mill_Y_Check newBoard
    mill_Y_Check2 newBoard
    mill_Y_Check3 newBoard
    Ongoing newBoard
    
    

 
let rec run player game =
    // need to find the blank cells that can be used...
    printBoard game
    printfn "%A's turn.  Type the number of the cell that you want to play into." player
    let n = System.Console.ReadLine()
    match n with
    | "A1" | "A4" | "A7" 
    | "B2" | "B4" | "B6" 
    | "C3" | "C4" | "C5"
    | "D1" | "D2" | "D3" 
    | "D5" | "D6" | "D7" 
    | "E3" | "E4" | "E5" 
    | "F2" | "F4" | "F6" 
    | "G1" | "G4" | "G7"  ->
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
    match run currentPlayer game with
    | Ongoing newBoard -> runGame (swapPlayer currentPlayer) newBoard
    | Winner (player, board) ->
        printBoard board
        printfn "Winner is %A" player
        playAgain ()
    | Draw ->
        printfn "Draaaaw"
        playAgain ()
//let (a1,a4,a7): Cell = (Blank, Blank, Blank) 
//a1 a4 a7 b2 b4 b6 c3 c4 c5 d1 d2 d3 d5 d6 d7 e3 e4 e5 f2 f4 f6 g1 g4 g7

     


[<EntryPoint>]
let main argv =
    //runGame Y blankBoard
   // printBoard blankBoard
    
    runGame Y blankBoard
    
    
    Console.Read()

    //runGame X blankBoard
    0 // ret

(*
                       ----------------------------------------------- PROGRESS ------------------------------------------------------
                       1. CAN DISPLAY NICE BOARD                                                                                 _DONE
                       2. CAN PLACE PIECES (COWS) ON THE BOARD ACCORDING TO NAMED CELLS (A-G -> 1-7 )                            _DONE
                       3. CAN CHECK FOR NULL COWS - 3 PAIRS                                                               _IN PROGRESS
                           ISSUES : KEEPING TRACK OF PREVIOUS MILLS
*)