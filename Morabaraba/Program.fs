open System
open System.IO

type Cell =
| Y
| M
| Blank

type GameBoard =
| Board of (Cell*Cell*Cell)*(Cell*Cell*Cell)*(Cell*Cell*Cell)*(Cell*Cell*Cell)*(Cell*Cell*Cell)*(Cell*Cell*Cell)*(Cell*Cell*Cell)*(Cell*Cell*Cell)

type Result =
| Mill of GameBoard
| Ongoing of GameBoard

let swapPlayer x =
    match x with
    | Y -> M
    | M -> Y
    | Blank -> failwith "RUN FOR YOUR LIVES THE END IS NIGH"


let playerAtPos (Board (r1,r2,r3,r4,r5,r6,r7,r8)) position =
    let (a1,a2,a3) = r1
    let (b1,b2,b3) = r2
    let (c1,c2,c3) = r3
    let (d1,d2,d3) = r4
    let (d4,d5,d6) = r5
    let (e1,e2,e3) = r6
    let (f1,f2,f3) = r7
    let (g1,g2,g3) = r8

    match position with
    | "A1" -> a1
    | "A4" -> a2
    | "A7" -> a3
    | "B2" -> b1
    | "B4" -> b2
    | "B6" -> b3
    | "C3" -> c1
    | "C4" -> c2
    | "C5" -> c3
    | "D1" -> d1
    | "D2" -> d2
    | "D3" -> d3
    | "D5" -> d4
    | "D6" -> d5
    | "D7" -> d6
    | "E3" -> e1
    | "E4" -> e2
    | "E5" -> e3
    | "F2" -> f1
    | "F4" -> f2
    | "F6" -> f3
    | "G1" -> g1
    | "G4" -> g2
    | "G7" -> g3
    | _ -> Blank


let updateMill_status (state:byref<_>)=
    match state with
    | 0 -> state <- 1
    | _ -> state <- 2

let mutable mY1 = 0
let mutable mY2 = 0
let mutable mY3 = 0
let mutable mY4 = 0
let mutable mY5 = 0
let mutable mY6 = 0
let mutable mY7 = 0
let mutable mY8 = 0

let mill_Y_Check game =
    let (Board (r1,r2,r3,r4,r5,r6,r7,r8)) = game
    
    match r1,r2,r3,r4,r5,r6,r7,r8 with
    | (Y,Y,Y),_,_,_,_,_,_,_ -> updateMill_status &mY1
    | _ -> ()
    match r1,r2,r3,r4,r5,r6,r7,r8 with
    | _,(Y,Y,Y),_,_,_,_,_,_ -> updateMill_status &mY2    // r2 Y mill ~B
    | _-> ()
    match r1,r2,r3,r4,r5,r6,r7,r8 with
    | _,_,(Y,Y,Y),_,_,_,_,_ -> updateMill_status &mY3     // r3 Y mill ~C
    | _-> ()
    match r1,r2,r3,r4,r5,r6,r7,r8 with
    | _,_,_,(Y,Y,Y),_,_,_,_ -> updateMill_status &mY4     // r4 Y mill ~D1 ~Middle
    | _-> ()
    match r1,r2,r3,r4,r5,r6,r7,r8 with
    | _,_,_,_,(Y,Y,Y),_,_,_ -> updateMill_status &mY5     // r5 Y mill ~D2 ~Middle
    | _-> ()
    match r1,r2,r3,r4,r5,r6,r7,r8 with
    | _,_,_,_,_,(Y,Y,Y),_,_ -> updateMill_status &mY6    // r6 Y mill ~E
    | _-> ()
    match r1,r2,r3,r4,r5,r6,r7,r8 with
    | _,_,_,_,_,_,(Y,Y,Y),_ -> updateMill_status &mY7    // r7 Y mill ~F
    | _-> ()
    match r1,r2,r3,r4,r5,r6,r7,r8 with
    | _,_,_,_,_,_,_,(Y,Y,Y) -> updateMill_status &mY8 // r8 Y mill ~G
    | _-> ()

   //let ans =
    match mY1=1 || mY2=1 || mY3=1 || mY4=1 || mY5=1 || mY6=1 || mY7=1 || mY8=1 with
    | true -> true
    | _ -> false

let mutable mY_1_1 = 0
let mutable mY_1_2 = 0
let mutable mY_1_3 = 0
let mutable mY_1_4 = 0
let mutable mY_1_5 = 0
let mutable mY_1_6 = 0
let mutable mY_1_7 = 0

let mill_Y_Check2 game =
    let (Board (r1,r2,r3,r4,r5,r6,r7,r8)) = game
    match r1,r2,r3,r4,r5,r6,r7,r8 with
    | (Y,_,_),(Y,_,_),(Y,_,_),_,_,_,_,_ -> updateMill_status &mY_1_1
    | _ -> ()               
    match r1,r2,r3,r4,r5,r6,r7,r8 with
    | (_,Y,_),(_,Y,_),(_,Y,_),_,_,_,_,_ -> updateMill_status &mY_1_2     // A4-B4-C4 Y mill ~Middle
    | _-> ()
    match r1,r2,r3,r4,r5,r6,r7,r8 with
    | (_,_,Y),(_,_,Y),(_,_,Y),_,_,_,_,_ -> updateMill_status &mY_1_3  // A7-B6-C5 Y mill
    | _-> ()
    match r1,r2,r3,r4,r5,r6,r7,r8 with
    | (Y,_,_),_,_,(Y,_,_),_,_,_,(Y,_,_) -> updateMill_status &mY_1_4     // A1-D1-G1 Y mill ~leftmost vertical
    | _-> ()
    match r1,r2,r3,r4,r5,r6,r7,r8 with
    | _,(Y,_,_),_,(_,Y,_),_,_,(Y,_,_),_ -> updateMill_status &mY_1_5     // B2-D2-F2 Y mill
    | _-> ()
    match r1,r2,r3,r4,r5,r6,r7,r8 with
    | _,_,(Y,_,_),(_,_,Y),_,(Y,_,_),_,_ -> updateMill_status &mY_1_6     // C3-D3-E3 Y mill
    | _-> ()
    match r1,r2,r3,r4,r5,r6,r7,r8 with
    | _,_,_,_,_,(Y,_,_),(Y,_,_),(Y,_,_) -> updateMill_status &mY_1_7   // G1-F2-E3 Y mill
    | _-> ()
    
    match mY_1_1=1 || mY_1_2=1 || mY_1_3=1 || mY_1_4=1 || mY_1_5=1 || mY_1_6=1 || mY_1_7=1 with
    | true -> true
    | _ -> false

let mutable mY_2_1 = 0
let mutable mY_2_2 = 0
let mutable mY_2_3 = 0
let mutable mY_2_4 = 0
let mutable mY_2_5 = 0

let mill_Y_Check3 game =
    let (Board (r1,r2,r3,r4,r5,r6,r7,r8)) = game
    match r1,r2,r3,r4,r5,r6,r7,r8 with
    | _,_,_,_,_,(_,Y,_),(_,Y,_),(_,Y,_) -> updateMill_status &mY_2_1     // G4-F4-E4 Y mill ~Middle
    | _-> ()
    match r1,r2,r3,r4,r5,r6,r7,r8 with
    | _,_,_,_,_,(_,_,Y),(_,_,Y),(_,_,Y) -> updateMill_status &mY_2_2    // G7-F6-E5 Y mill
    | _-> ()
    match r1,r2,r3,r4,r5,r6,r7,r8 with
    | (_,_,Y),_,_,_,(_,_,Y),_,_,(_,_,Y) -> updateMill_status &mY_2_3    // A7-D7-G7 Y mill ~rightmost vertical
    | _-> ()
    match r1,r2,r3,r4,r5,r6,r7,r8 with
    | _,(_,_,Y),_,_,(_,Y,_),_,(_,_,Y),_ -> updateMill_status &mY_2_4    // B6-D6-F6 Y mill
    | _-> ()
    match r1,r2,r3,r4,r5,r6,r7,r8 with
    | _,_,(_,_,Y),_,(Y,_,_),(_,_,Y),_,_ -> updateMill_status &mY_2_5  // C5-D5-E5 Y mill  
    | _-> () 

    match mY_2_1=1 || mY_2_2=1 || mY_2_3=1 || mY_2_4=1 || mY_2_5=1 with
    | true -> true
    | _ -> false

let mutable mM1 = 0
let mutable mM2 = 0
let mutable mM3 = 0
let mutable mM4 = 0
let mutable mM5 = 0
let mutable mM6 = 0
let mutable mM7 = 0
let mutable mM8 = 0
  
let mill_M_Check game =
    let (Board (r1,r2,r3,r4,r5,r6,r7,r8)) = game
    
    match r1,r2,r3,r4,r5,r6,r7,r8 with
    | (M,M,M),_,_,_,_,_,_,_ -> updateMill_status &mM1
    | _ -> ()
    match r1,r2,r3,r4,r5,r6,r7,r8 with
    | _,(M,M,M),_,_,_,_,_,_ -> updateMill_status &mM2    // r2 M mill ~B
    | _-> ()
    match r1,r2,r3,r4,r5,r6,r7,r8 with
    | _,_,(M,M,M),_,_,_,_,_ -> updateMill_status &mM3     // r3 M mill ~C
    | _-> ()
    match r1,r2,r3,r4,r5,r6,r7,r8 with
    | _,_,_,(M,M,M),_,_,_,_ -> updateMill_status &mM4     // r4 M mill ~D1 ~Middle
    | _-> ()
    match r1,r2,r3,r4,r5,r6,r7,r8 with
    | _,_,_,_,(M,M,M),_,_,_ -> updateMill_status &mM5     // r5 M mill ~D2 ~Middle
    | _-> ()
    match r1,r2,r3,r4,r5,r6,r7,r8 with
    | _,_,_,_,_,(M,M,M),_,_ -> updateMill_status &mM6    // r6 M mill ~E
    | _-> ()
    match r1,r2,r3,r4,r5,r6,r7,r8 with
    | _,_,_,_,_,_,(M,M,M),_ -> updateMill_status &mM7    // r7 M mill ~F
    | _-> ()
    match r1,r2,r3,r4,r5,r6,r7,r8 with
    | _,_,_,_,_,_,_,(M,M,M) -> updateMill_status &mM8 // r8 M mill ~G
    | _-> ()

   //let ans =
    match mM1=1 || mM2=1 || mM3=1 || mM4=1 || mM5=1 || mM6=1 || mM7=1 || mM8=1 with
    | true -> true
    | _ -> false
 
let mutable mM_1_1 = 0
let mutable mM_1_2 = 0
let mutable mM_1_3 = 0
let mutable mM_1_4 = 0
let mutable mM_1_5 = 0
let mutable mM_1_6 = 0
let mutable mM_1_7 = 0

let mill_M_Check2 game =
    let (Board (r1,r2,r3,r4,r5,r6,r7,r8)) = game
    match r1,r2,r3,r4,r5,r6,r7,r8 with
    | (M,_,_),(M,_,_),(M,_,_),_,_,_,_,_ -> updateMill_status &mM_1_1
    | _ -> ()               
    match r1,r2,r3,r4,r5,r6,r7,r8 with
    | (_,M,_),(_,M,_),(_,M,_),_,_,_,_,_ -> updateMill_status &mM_1_2     // A4-B4-C4 M mill ~Middle
    | _-> ()
    match r1,r2,r3,r4,r5,r6,r7,r8 with
    | (_,_,M),(_,_,M),(_,_,M),_,_,_,_,_ -> updateMill_status &mM_1_3  // A7-B6-C5 M mill
    | _-> ()
    match r1,r2,r3,r4,r5,r6,r7,r8 with
    | (M,_,_),_,_,(M,_,_),_,_,_,(M,_,_) -> updateMill_status &mM_1_4     // A1-D1-G1 M mill ~leftmost vertical
    | _-> ()
    match r1,r2,r3,r4,r5,r6,r7,r8 with
    | _,(M,_,_),_,(_,M,_),_,_,(M,_,_),_ -> updateMill_status &mM_1_5     // B2-D2-F2 M mill
    | _-> ()
    match r1,r2,r3,r4,r5,r6,r7,r8 with
    | _,_,(M,_,_),(_,_,M),_,(M,_,_),_,_ -> updateMill_status &mM_1_6     // C3-D3-E3 M mill
    | _-> ()
    match r1,r2,r3,r4,r5,r6,r7,r8 with
    | _,_,_,_,_,(M,_,_),(M,_,_),(M,_,_) -> updateMill_status &mM_1_7   // G1-F2-E3 M mill
    | _-> ()
    
    match mM_1_1=1 || mM_1_2=1 || mM_1_3=1 || mM_1_4=1 || mM_1_5=1 || mM_1_6=1 || mM_1_7=1 with
    | true -> true
    | _ -> false

let mutable mM_2_1 = 0
let mutable mM_2_2 = 0
let mutable mM_2_3 = 0
let mutable mM_2_4 = 0
let mutable mM_2_5 = 0

let mill_M_Check3 game =
    let (Board (r1,r2,r3,r4,r5,r6,r7,r8)) = game
    match r1,r2,r3,r4,r5,r6,r7,r8 with
    | _,_,_,_,_,(_,M,_),(_,M,_),(_,M,_) -> updateMill_status &mM_2_1     // G4-F4-E4 M mill ~Middle
    | _-> ()
    match r1,r2,r3,r4,r5,r6,r7,r8 with
    | _,_,_,_,_,(_,_,M),(_,_,M),(_,_,M) -> updateMill_status &mM_2_2    // G7-F6-E5 M mill
    | _-> ()
    match r1,r2,r3,r4,r5,r6,r7,r8 with
    | (_,_,M),_,_,_,(_,_,M),_,_,(_,_,M) -> updateMill_status &mM_2_3    // A7-D7-G7 M mill ~rightmost vertical
    | _-> ()
    match r1,r2,r3,r4,r5,r6,r7,r8 with
    | _,(_,_,M),_,_,(_,M,_),_,(_,_,M),_ -> updateMill_status &mM_2_4    // B6-D6-F6 M mill
    | _-> ()
    match r1,r2,r3,r4,r5,r6,r7,r8 with
    | _,_,(_,_,M),_,(M,_,_),(_,_,M),_,_ -> updateMill_status &mM_2_5  // G4-F4-E4 M mill  
    | _-> () 

    match mM_2_1=1 || mM_2_2=1 || mM_2_3=1 || mM_2_4=1 || mM_2_5=1 with
    | true -> true
    | _ -> false

let updateMill_status3 (state1:byref<_>) (state2:byref<_>) (state3:byref<_>) = 
    match state1 = 2 || state2 = 2 || state3 = 2 || state1 = 1 || state2 = 1 || state3 = 1 with
     | true -> state1 <- 0
               state2 <- 0
               state3 <- 0
     | _ -> ()

let updateMill_status2 (state1:byref<_>) (state2:byref<_>) = 
    match state1 = 2 || state2 = 2 || state1 = 1 || state2 = 1 with
     | true -> state1 <- 0
               state2 <- 0
     | _ -> ()

let Broken_Mill position =
    match position with
    | "A1" -> updateMill_status3 &mY1 &mY_1_1 &mY_1_4
    | "A4" -> updateMill_status2 &mY1 &mY_1_2
    | "A7" -> updateMill_status3 &mY1 &mY_1_3 &mY_2_3
    | "B2" -> updateMill_status3 &mY2 &mY_1_5 &mY_1_1
    | "B4" -> updateMill_status2 &mY2 &mY_1_2
    | "B6" -> updateMill_status3 &mY2 &mY_2_4 &mY_1_3
    | "C3" -> updateMill_status3 &mY3 &mY_1_1 &mY_1_6
    | "C4" -> updateMill_status2 &mY3 &mY_1_2
    | "C5" -> updateMill_status3 &mY3 &mY_1_3 &mY_2_5
    | "D1" -> updateMill_status2 &mY4 &mY_1_4
    | "D2" -> updateMill_status2 &mY4 &mY_1_5
    | "D3" -> updateMill_status2 &mY4 &mY_1_6
    | "D5" -> updateMill_status2 &mY5 &mY_2_5
    | "D6" -> updateMill_status2 &mY5 &mY_2_4
    | "D7" -> updateMill_status2 &mY5 &mY_2_3
    | "E3" -> updateMill_status3 &mY6 &mY_1_6 &mY_1_7
    | "E4" -> updateMill_status2 &mY6 &mY_2_1 
    | "E5" -> updateMill_status3 &mY6 &mY_2_5 &mY_2_2
    | "F2" -> updateMill_status3 &mY7 &mY_1_7 &mY_1_5
    | "F4" -> updateMill_status2 &mY7 &mY_2_1
    | "F6" -> updateMill_status3 &mY7 &mY_2_2 &mY_2_4
    | "G1" -> updateMill_status3 &mY8 &mY_1_7 &mY_1_4
    | "G4" -> updateMill_status2 &mY8 &mY_2_1 
    | "G7" -> updateMill_status3 &mY8 &mY_2_2 &mY_2_3
    | _ -> ()
    
    match position with
    | "A1" -> updateMill_status3 &mM1 &mM_1_1 &mM_1_4
    | "A4" -> updateMill_status2 &mM1 &mM_1_2
    | "A7" -> updateMill_status3 &mM1 &mM_1_3 &mM_2_3
    | "B2" -> updateMill_status3 &mM2 &mM_1_5 &mM_1_1
    | "B4" -> updateMill_status2 &mM2 &mM_1_2
    | "B6" -> updateMill_status3 &mM2 &mM_2_4 &mM_1_3
    | "C3" -> updateMill_status3 &mM3 &mM_1_1 &mM_1_6
    | "C4" -> updateMill_status2 &mM3 &mM_1_2
    | "C5" -> updateMill_status3 &mM3 &mM_1_3 &mM_2_5
    | "D1" -> updateMill_status2 &mM4 &mM_1_4
    | "D2" -> updateMill_status2 &mM4 &mM_1_5
    | "D3" -> updateMill_status2 &mM4 &mM_1_6
    | "D5" -> updateMill_status2 &mM5 &mM_2_5
    | "D6" -> updateMill_status2 &mM5 &mM_2_4
    | "D7" -> updateMill_status2 &mM5 &mM_2_3
    | "E3" -> updateMill_status3 &mM6 &mM_1_6 &mM_1_7
    | "E4" -> updateMill_status2 &mM6 &mM_2_1 
    | "E5" -> updateMill_status3 &mM6 &mM_2_5 &mM_2_2
    | "F2" -> updateMill_status3 &mM7 &mM_1_7 &mM_1_5
    | "F4" -> updateMill_status2 &mM7 &mM_2_1
    | "F6" -> updateMill_status3 &mM7 &mM_2_2 &mM_2_4
    | "G1" -> updateMill_status3 &mM8 &mM_1_7 &mM_1_4
    | "G4" -> updateMill_status2 &mM8 &mM_2_1 
    | "G7" -> updateMill_status3 &mM8 &mM_2_2 &mM_2_3
    | _ -> ()

let mutable Y_cows = 12
let mutable M_cows = 12

let rec eliminate (Board (r1,r2,r3,r4,r5,r6,r7,r8)) player =
    let game = Board (r1,r2,r3,r4,r5,r6,r7,r8)
    printf (" %A has mill! select a position to eliminate %A  :  ") player (swapPlayer player)
    let position = Console.ReadLine()
    
   // let otherP = swapPlayer player
    match (playerAtPos game position) with
    | Y -> match player with
           | M ->
            M_cows <- M_cows - 1
            Broken_Mill position
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
            newBoard
            | _ -> eliminate game player
    |M -> match player with
           | Y ->
            Y_cows <- Y_cows - 1
            Broken_Mill position
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
            newBoard
            | _ -> eliminate game player
    | _ -> eliminate game player


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

let gameCheck game =

    match mill_Y_Check game || mill_Y_Check2 game || mill_Y_Check3 game || mill_M_Check game || mill_M_Check2 game || mill_M_Check3 game with
    | true -> Mill game
    | _ -> Ongoing game

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
                                 printfn "\t\t\t\t |     |\           |          /|    | \t\t Y \t\t\t\t M"
                                 printfn "\t\t\t\t |     | \          |         / |    |"
                                 printfn "\t\t\t\t |     |  \         |        /  |    | \t\t Cows: %d \t\t Cows: %d" Y_cows M_cows
                  let printBdC = printfn "\t\t\tC\t |     |   %A_____%A____%A   |    |" (tc c3) (tc c4) (tc c5)
                                 printfn "\t\t\t\t |     |    |              |    |    | \t\t kills: %d \t\t kills: %d " (12-M_cows) (12-Y_cows)
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
    gameCheck newBoard
    
// This function moves a cow from position A
let moveCow (Board (r1, r2, r3,r4,r5,r6,r7,r8)) play moveFrom moveTo =
    let currentBoard  = (Board (r1, r2, r3,r4,r5,r6,r7,r8))
    let a1,a4,a7 = r1
    let b2,b4,b6 = r2
    let c3,c4,c5 = r3
    let d1,d2,d3 = r4
    let d5,d6,d7 = r5
    let e3,e4,e5 = r6
    let f2,f4,f6 = r7
    let g1,g4,g7 = r8

    

    let ammend (Board (r1, r2, r3,r4,r5,r6,r7,r8)) pos player =  
    
        let a1,a4,a7 = r1
        let b2,b4,b6 = r2
        let c3,c4,c5 = r3
        let d1,d2,d3 = r4
        let d5,d6,d7 = r5
        let e3,e4,e5 = r6
        let f2,f4,f6 = r7
        
        let amn =
            match pos with 
            | "A1" -> (player,a4,a7),r2,r3,r4,r5,r6,r7,r8
            | "A4" -> (a1,player,a7),r2,r3,r4,r5,r6,r7,r8
            | "A7" -> (a1,a4,player),r2,r3,r4,r5,r6,r7,r8
            | "B2" -> r1,(player,b4,b6),r3,r4,r5,r6,r7,r8
            | "B4" -> r1,(b2,player,b6),r3,r4,r5,r6,r7,r8
            | "B6" -> r1,(b2,b4,player),r3,r4,r5,r6,r7,r8
            | "C3" -> r1,r2,(player,c4,c5),r4,r5,r6,r7,r8
            | "C4" -> r1,r2,(c3,player,c5),r4,r5,r6,r7,r8
            | "C5" -> r1,r2,(c3,c4,player),r4,r5,r6,r7,r8
            | "D1" -> r1,r2,r3,(player,d2,d3),r5,r6,r7,r8
            | "D2" -> r1,r2,r3,(d1,player,c5),r5,r6,r7,r8
            | "D3" -> r1,r2,r3,(d1,d2,player),r5,r6,r7,r8
            | "D5" -> r1,r2,r3,r4,(player,d6,d7),r6,r7,r8
            | "D6" -> r1,r2,r3,r4,(d5,player,d7),r6,r7,r8
            | "D7" -> r1,r2,r3,r4,(d5,d6,player),r6,r7,r8
            | "E3" -> r1,r2,r3,r4,r5,(player,e4,e5),r7,r8
            | "E4" -> r1,r2,r3,r4,r5,(e3,player,e5),r7,r8
            | "E5" -> r1,r2,r3,r4,r5,(e3,e4,player),r7,r8
            | "F2" -> r1,r2,r3,r4,r5,r6,(player,f4,f6),r8
            | "F4" -> r1,r2,r3,r4,r5,r6,(f2,player,f6),r8
            | "F6" -> r1,r2,r3,r4,r5,r6,(f2,f4,player),r8
            | "G1" -> r1,r2,r3,r4,r5,r6,r7,(player,g4,g7)
            | "G4" -> r1,r2,r3,r4,r5,r6,r7,(g1,player,g7)
            | "G7" -> r1,r2,r3,r4,r5,r6,r7,(g1,g4,player)
            | _ -> failwith "i hate myself"  
        Board amn
     

    //Check if the position to move to is empty
    let checkMove = 
        match moveTo with
        | "A1" -> a1 = Blank        
        | "A4" -> a4 = Blank
        | "A7" -> a7 = Blank
        | "B2" -> b2 = Blank
        | "B4" -> b4 = Blank
        | "B6" -> b6 = Blank
        | "C3" -> c3 = Blank
        | "C4" -> c4 = Blank
        | "C5" -> c5 = Blank
        | "D1" -> d1 = Blank
        | "D2" -> d2 = Blank 
        | "D3" -> d3 = Blank 
        | "D5" -> d5 = Blank
        | "D6" -> d6 = Blank
        | "D7" -> d7 = Blank
        | "E3" -> e3 = Blank
        | "E4" -> e4 = Blank
        | "E5" -> e5 = Blank
        | "F2" -> f2 = Blank
        | "F4" -> f4 = Blank
        | "F6" -> f6 = Blank
        | "G1" -> g1 = Blank 
        | "G4" -> g4 = Blank
        | "G7" -> g7 = Blank
        | _ -> false

    let checkNeighbours = 

        let aOne = ["A4";"B2";"D1"]
        let aFour = ["A1";"A7";"B4"]
        let aSeven = ["A4";"B6";"D7"]
        let bTwo = ["A1";"C3";"B4";"D2"]
        let bFour = ["B2";"A4";"B6"]
        let bSix = ["A7";"B4";"D7"]
        let cThree = ["B2";"C4";"D3"]
        let cFour = ["C3";"B4";"C5"]
        let cFive = ["C4";"B6";"D5"]
        let dOne = ["A1";"D2";"G1"]
        let dTwo = ["D1";"B2";"D3"]
        let dThree =["C3";"D2";"E3"] 
        let dFive = ["C5";"D6";"E5"]
        let dSix = ["D5";"B6";"D7";"F6"]
        let dSeven = ["A7";"D6";"G7"]
        let eThree = ["D7";"E4";"F2"]
        let eFour = ["F4";"E3";"E5"]
        let eFive = ["D5";"E4";"F6"]
        let fTwo = ["G1";"E3";"F4";"D2"]
        let fFour = ["E4";"F6";"G4";"F2"] 
        let fSix = ["G7";"D6";"F4";"E5"]
        let gOne = ["D1";"F2";"G4"]
        let gFour = ["F4";"G1";"G7"]
        let gSeven = ["F4";"G1";"G7"]

        let tst = fun p -> p = moveTo
        match moveFrom with
        | "A1" -> List.exists tst aOne
        | "A4" -> List.exists tst aFour
        | "A7" -> List.exists tst aSeven
        | "B2" -> List.exists tst bTwo
        | "B4" -> List.exists tst bFour
        | "B6" -> List.exists tst bSix
        | "C3" -> List.exists tst cThree
        | "C4" -> List.exists tst cFour
        | "C5" -> List.exists tst cFive
        | "D1" -> List.exists tst dOne
        | "D2" -> List.exists tst dTwo
        | "D3" -> List.exists tst dThree
        | "D5" -> List.exists tst dFive
        | "D6" -> List.exists tst dSix
        | "D7" -> List.exists tst dSeven
        | "E3" -> List.exists tst eThree
        | "E4" -> List.exists tst eFour
        | "E5" -> List.exists tst eFive
        | "F2" -> List.exists tst fTwo
        | "F4" -> List.exists tst fFour
        | "F6" -> List.exists tst fSix
        | "G1" -> List.exists tst gOne
        | "G4" -> List.exists tst gFour
        | "G7" -> List.exists tst gSeven
        | _ -> failwith "How did we get here" 
    
    let RemoveCow  =  ammend currentBoard moveFrom Blank
    
    match checkMove && checkNeighbours with
    | true ->gameCheck (ammend RemoveCow moveTo play)
    | _->  gameCheck currentBoard
       
       //*)
let flyCow (Board (r1, r2, r3,r4,r5,r6,r7,r8)) play moveFrom moveTo =
    let currentBoard  = (Board (r1, r2, r3,r4,r5,r6,r7,r8))
    let a1,a4,a7 = r1
    let b2,b4,b6 = r2
    let c3,c4,c5 = r3
    let d1,d2,d3 = r4
    let d5,d6,d7 = r5
    let e3,e4,e5 = r6
    let f2,f4,f6 = r7
    let g1,g4,g7 = r8

    

    let ammend (Board (r1, r2, r3,r4,r5,r6,r7,r8)) pos player =  
    
        let a1,a4,a7 = r1
        let b2,b4,b6 = r2
        let c3,c4,c5 = r3
        let d1,d2,d3 = r4
        let d5,d6,d7 = r5
        let e3,e4,e5 = r6
        let f2,f4,f6 = r7
        
        let amn =
            match pos with 
            | "A1" -> (player,a4,a7),r2,r3,r4,r5,r6,r7,r8
            | "A4" -> (a1,player,a7),r2,r3,r4,r5,r6,r7,r8
            | "A7" -> (a1,a4,player),r2,r3,r4,r5,r6,r7,r8
            | "B2" -> r1,(player,b4,b6),r3,r4,r5,r6,r7,r8
            | "B4" -> r1,(b2,player,b6),r3,r4,r5,r6,r7,r8
            | "B6" -> r1,(b2,b4,player),r3,r4,r5,r6,r7,r8
            | "C3" -> r1,r2,(player,c4,c5),r4,r5,r6,r7,r8
            | "C4" -> r1,r2,(c3,player,c5),r4,r5,r6,r7,r8
            | "C5" -> r1,r2,(c3,c4,player),r4,r5,r6,r7,r8
            | "D1" -> r1,r2,r3,(player,d2,d3),r5,r6,r7,r8
            | "D2" -> r1,r2,r3,(d1,player,c5),r5,r6,r7,r8
            | "D3" -> r1,r2,r3,(d1,d2,player),r5,r6,r7,r8
            | "D5" -> r1,r2,r3,r4,(player,d6,d7),r6,r7,r8
            | "D6" -> r1,r2,r3,r4,(d5,player,d7),r6,r7,r8
            | "D7" -> r1,r2,r3,r4,(d5,d6,player),r6,r7,r8
            | "E3" -> r1,r2,r3,r4,r5,(player,e4,e5),r7,r8
            | "E4" -> r1,r2,r3,r4,r5,(e3,player,e5),r7,r8
            | "E5" -> r1,r2,r3,r4,r5,(e3,e4,player),r7,r8
            | "F2" -> r1,r2,r3,r4,r5,r6,(player,f4,f6),r8
            | "F4" -> r1,r2,r3,r4,r5,r6,(f2,player,f6),r8
            | "F6" -> r1,r2,r3,r4,r5,r6,(f2,f4,player),r8
            | "G1" -> r1,r2,r3,r4,r5,r6,r7,(player,g4,g7)
            | "G4" -> r1,r2,r3,r4,r5,r6,r7,(g1,player,g7)
            | "G7" -> r1,r2,r3,r4,r5,r6,r7,(g1,g4,player)
            | _ -> failwith "i hate myself"  
        Board amn
     

    //Check if the position to move to is empty
    let checkMove = 
        match moveTo with
        | "A1" -> a1 = Blank        
        | "A4" -> a4 = Blank
        | "A7" -> a7 = Blank
        | "B2" -> b2 = Blank
        | "B4" -> b4 = Blank
        | "B6" -> b6 = Blank
        | "C3" -> c3 = Blank
        | "C4" -> c4 = Blank
        | "C5" -> c5 = Blank
        | "D1" -> d1 = Blank
        | "D2" -> d2 = Blank 
        | "D3" -> d3 = Blank 
        | "D5" -> d5 = Blank
        | "D6" -> d6 = Blank
        | "D7" -> d7 = Blank
        | "E3" -> e3 = Blank
        | "E4" -> e4 = Blank
        | "E5" -> e5 = Blank
        | "F2" -> f2 = Blank
        | "F4" -> f4 = Blank
        | "F6" -> f6 = Blank
        | "G1" -> g1 = Blank 
        | "G4" -> g4 = Blank
        | "G7" -> g7 = Blank
        | _ -> false
        
    let RemoveCow  =  ammend currentBoard moveFrom Blank
    
    match checkMove with
    | true ->gameCheck (ammend RemoveCow moveTo play)
    | _->  gameCheck currentBoard
   

 
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

let rec run1 player game =
    // need to find the blank cells that can be used...
    printBoard game
    printf "%A Enter position to move FROM : " player
    let moveFrom = Console.ReadLine()

    printf "%A Enter position to move TO : " player
    let moveTo = Console.ReadLine()
    
    let checkNeighbours = 

            let aOne = ["A4";"B2";"D1"]
            let aFour = ["A1";"A7";"B4"]
            let aSeven = ["A4";"B6";"D7"]
            let bTwo = ["A1";"C3";"B4";"D2"]
            let bFour = ["B2";"A4";"B6"]
            let bSix = ["A7";"B4";"D7"]
            let cThree = ["B2";"C4";"D3"]
            let cFour = ["C3";"B4";"C5"]
            let cFive = ["C4";"B6";"D5"]
            let dOne = ["A1";"D2";"G1"]
            let dTwo = ["D1";"B2";"D3"]
            let dThree =["C3";"D2";"E3"] 
            let dFive = ["C5";"D6";"E5"]
            let dSix = ["D5";"B6";"D7";"F6"]
            let dSeven = ["A7";"D6";"G7"]
            let eThree = ["D7";"E4";"F2"]
            let eFour = ["F4";"E3";"E5"]
            let eFive = ["D5";"E4";"F6"]
            let fTwo = ["G1";"E3";"F4";"D2"]
            let fFour = ["E4";"F6";"G4";"F2"] 
            let fSix = ["G7";"D6";"F4";"E5"]
            let gOne = ["D1";"F2";"G4"]
            let gFour = ["F4";"G1";"G7"]
            let gSeven = ["F4";"G1";"G7"]

            let tst = fun p -> p = moveTo
            match moveFrom with
            | "A1" -> List.exists tst aOne
            | "A4" -> List.exists tst aFour
            | "A7" -> List.exists tst aSeven
            | "B2" -> List.exists tst bTwo
            | "B4" -> List.exists tst bFour
            | "B6" -> List.exists tst bSix
            | "C3" -> List.exists tst cThree
            | "C4" -> List.exists tst cFour
            | "C5" -> List.exists tst cFive
            | "D1" -> List.exists tst dOne
            | "D2" -> List.exists tst dTwo
            | "D3" -> List.exists tst dThree
            | "D5" -> List.exists tst dFive
            | "D6" -> List.exists tst dSix
            | "D7" -> List.exists tst dSeven
            | "E3" -> List.exists tst eThree
            | "E4" -> List.exists tst eFour
            | "E5" -> List.exists tst eFive
            | "F2" -> List.exists tst fTwo
            | "F4" -> List.exists tst fFour
            | "F6" -> List.exists tst fSix
            | "G1" -> List.exists tst gOne
            | "G4" -> List.exists tst gFour
            | "G7" -> List.exists tst gSeven
            | _ -> false 
    match checkNeighbours with
    | true ->
        match moveFrom with
        | "A1" | "A4" | "A7" 
        | "B2" | "B4" | "B6" 
        | "C3" | "C4" | "C5"
        | "D1" | "D2" | "D3" 
        | "D5" | "D6" | "D7" 
        | "E3" | "E4" | "E5" 
        | "F2" | "F4" | "F6" 
        | "G1" | "G4" | "G7"  ->
            match moveTo with
            | "A1" | "A4" | "A7" 
            | "B2" | "B4" | "B6" 
            | "C3" | "C4" | "C5"
            | "D1" | "D2" | "D3" 
            | "D5" | "D6" | "D7" 
            | "E3" | "E4" | "E5" 
            | "F2" | "F4" | "F6" 
            | "G1" | "G4" | "G7"  ->
                match isBlank game moveTo with
                | true -> Broken_Mill moveFrom
                          moveCow game player moveFrom moveTo
                | _ -> run1 player game
            | _ -> run1 player game
        | _ -> run1 player game
    | _ ->run1 player game   

let rec run2 player game =
    // need to find the blank cells that can be used...
    printBoard game
    printf "%A Enter position to move FROM : " player
    let moveFrom = Console.ReadLine()

    printf "%A Enter position to move TO : " player
    let moveTo = Console.ReadLine()
    match moveFrom with
     | "A1" | "A4" | "A7" 
     | "B2" | "B4" | "B6" 
     | "C3" | "C4" | "C5"
     | "D1" | "D2" | "D3" 
     | "D5" | "D6" | "D7" 
     | "E3" | "E4" | "E5" 
     | "F2" | "F4" | "F6" 
     | "G1" | "G4" | "G7"  ->
        match moveTo with
        | "A1" | "A4" | "A7" 
        | "B2" | "B4" | "B6" 
        | "C3" | "C4" | "C5"
        | "D1" | "D2" | "D3" 
        | "D5" | "D6" | "D7" 
        | "E3" | "E4" | "E5" 
        | "F2" | "F4" | "F6" 
        | "G1" | "G4" | "G7"  ->
           match isBlank game moveTo with
           | true -> Broken_Mill moveFrom 
                     flyCow game player moveFrom moveTo
           | _ -> run2 player game
        | _ -> run2 player game
     | _ -> run2 player game
       

let rec runGame currentPlayer game i =
    let playAgain () =
        printfn "Play again? [y/N] "
        match System.Console.ReadLine() with
        | "Y" | "y" -> runGame Y blankBoard i
        | _ -> ()
    match i with
    | 0 ->  match currentPlayer with
            | Y -> match Y_cows with
                   | 2 -> printf "\n\nGame Over!!!! %A is the winner" currentPlayer
                   | 3 -> match run2 currentPlayer game with
                            | Mill newBoard-> printBoard newBoard
                                              let newB = eliminate newBoard currentPlayer
                                              runGame (swapPlayer currentPlayer) newB 0
                            | Ongoing newBoard -> runGame (swapPlayer currentPlayer) newBoard 0
                            | _ -> printfn "OVER"
                   | _ ->
                        match run1 currentPlayer game with
                        | Mill newBoard-> printBoard newBoard
                                          let newB = eliminate newBoard currentPlayer
                                          runGame (swapPlayer currentPlayer) newB 0
                        | Ongoing newBoard -> runGame (swapPlayer currentPlayer) newBoard 0
            | M -> match M_cows with
                   | 2 -> printf "\n\nGame Over!!!! %A is the winner" currentPlayer
                   | 3 -> match run2 currentPlayer game with
                            | Mill newBoard-> printBoard newBoard
                                              let newB = eliminate newBoard currentPlayer
                                              runGame (swapPlayer currentPlayer) newB 0
                            | Ongoing newBoard -> runGame (swapPlayer currentPlayer) newBoard 0
                   | _ ->
                        match run1 currentPlayer game with
                        | Mill newBoard-> printBoard newBoard
                                          let newB = eliminate newBoard currentPlayer
                                          runGame (swapPlayer currentPlayer) newB 0
                        | Ongoing newBoard -> runGame (swapPlayer currentPlayer) newBoard 0
            | _ -> ()
    | _-> match run currentPlayer game with
            | Mill newBoard-> printBoard newBoard
                              let newB = eliminate newBoard currentPlayer
                              runGame (swapPlayer currentPlayer) newB (i-1)
            | Ongoing newBoard -> runGame (swapPlayer currentPlayer) newBoard (i-1)
 
[<EntryPoint>]
let main argv =
    Console.ForegroundColor<-ConsoleColor.Cyan  
    runGame Y blankBoard 24

    0 
(*
                       ----------------------------------------------- PROGRESS ------------------------------------------------------
                       1. MILL BREAK FROM MOVE AND FLY, WAIT 1 TURN BEFORE MATCHING A SAME MILL AGAIN                             _DONE
                           
*)