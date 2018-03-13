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

let currentStatus (yMlist:'int list) i = 
     match yMlist.[i] with
      | 0 -> 1
      | _ -> 0

let mill_Y_Check game (yMlist:'int list) =
    let (Board (r1,r2,r3,r4,r5,r6,r7,r8)) = game
    
    let y = match r1,r2,r3,r4,r5,r6,r7,r8 with
             | (Y,Y,Y),_,_,_,_,_,_,_ -> currentStatus yMlist 0
             | _ -> 0
    let y1 = match r1,r2,r3,r4,r5,r6,r7,r8 with
             | _,(Y,Y,Y),_,_,_,_,_,_ -> currentStatus yMlist 1    // r2 Y mill ~B
             | _-> 0
    let y2 = match r1,r2,r3,r4,r5,r6,r7,r8 with
             | _,_,(Y,Y,Y),_,_,_,_,_ -> currentStatus yMlist 2     // r3 Y mill ~C
             | _-> 0
    let y3 = match r1,r2,r3,r4,r5,r6,r7,r8 with
             | _,_,_,(Y,Y,Y),_,_,_,_ -> currentStatus yMlist 3     // r4 Y mill ~D1 ~Middle
             | _-> 0
    let y4 = match r1,r2,r3,r4,r5,r6,r7,r8 with
             | _,_,_,_,(Y,Y,Y),_,_,_ -> currentStatus yMlist 4     // r5 Y mill ~D2 ~Middle
             | _-> 0
    let y5 = match r1,r2,r3,r4,r5,r6,r7,r8 with
             | _,_,_,_,_,(Y,Y,Y),_,_ -> currentStatus yMlist 5    // r6 Y mill ~E
             | _-> 0
    let y6 = match r1,r2,r3,r4,r5,r6,r7,r8 with
             | _,_,_,_,_,_,(Y,Y,Y),_ -> currentStatus yMlist 6    // r7 Y mill ~F
             | _-> 0
    let y7 = match r1,r2,r3,r4,r5,r6,r7,r8 with
             | _,_,_,_,_,_,_,(Y,Y,Y) -> currentStatus yMlist 7 // r8 Y mill ~G
             | _-> 0

    match y = 1 || y1 = 1 || y2 = 1 || y3 = 1 || y4 = 1 || y5 = 1 || y6 = 1 || y7 = 1 with
    | true -> true
    | _ -> false

let updateList (xs : int list) index : int list =
            let rec innerF xs out i =
             match xs with
             | [] -> List.rev out
             | _::rest -> 
               match i=index with
                | true -> innerF rest (1::out) (i+1)               
                | _ -> innerF rest (xs.Head::out) (i+1)
            innerF xs [] 0 

let updateList2 (xs : int list) index : int list =
            let rec innerF xs out i =
             match xs with
             | [] -> List.rev out
             | _::rest -> 
               match i=index with
                | true -> innerF rest (0::out) (i+1)               
                | _ -> innerF rest (xs.Head::out) (i+1)
            innerF xs [] 0 

let updateStatusOrNot (yMlist:'int list) index =
    match yMlist.[index] with
     | 0 -> updateList yMlist index
     | _ -> yMlist

let mill_Y_Update game (yMlist:'int list) =
    let (Board (r1,r2,r3,r4,r5,r6,r7,r8)) = game
    
    let a1 = match r1,r2,r3,r4,r5,r6,r7,r8 with
             | (Y,Y,Y),_,_,_,_,_,_,_ -> updateStatusOrNot yMlist 0 
             | _ -> yMlist
    let a2 = match r1,r2,r3,r4,r5,r6,r7,r8 with
             | _,(Y,Y,Y),_,_,_,_,_,_ -> updateStatusOrNot a1 1     // r2 Y mill ~B
             | _-> a1
    let a3 = match r1,r2,r3,r4,r5,r6,r7,r8 with
             | _,_,(Y,Y,Y),_,_,_,_,_ -> updateStatusOrNot a2 2      // r3 Y mill ~C
             | _-> a2
    let a4 = match r1,r2,r3,r4,r5,r6,r7,r8 with
             | _,_,_,(Y,Y,Y),_,_,_,_ -> updateStatusOrNot a3 3      // r4 Y mill ~D1 ~Middle
             | _-> a3
    let a5 = match r1,r2,r3,r4,r5,r6,r7,r8 with
             | _,_,_,_,(Y,Y,Y),_,_,_ -> updateStatusOrNot a4 4      // r5 Y mill ~D2 ~Middle
             | _-> a4
    let a6 = match r1,r2,r3,r4,r5,r6,r7,r8 with
             | _,_,_,_,_,(Y,Y,Y),_,_ -> updateStatusOrNot a5 5     // r6 Y mill ~E
             | _-> a5
    let a7 = match r1,r2,r3,r4,r5,r6,r7,r8 with
             | _,_,_,_,_,_,(Y,Y,Y),_ -> updateStatusOrNot a6 6     // r7 Y mill ~F
             | _-> a6
    let a8 = match r1,r2,r3,r4,r5,r6,r7,r8 with
             | _,_,_,_,_,_,_,(Y,Y,Y) -> updateStatusOrNot a7 7  // r8 Y mill ~G
             | _-> a7
    let a9 = match r1,r2,r3,r4,r5,r6,r7,r8 with
             | (Y,_,_),(Y,_,_),(Y,_,_),_,_,_,_,_ -> updateStatusOrNot a8 8
             | _ -> a8
    let a10= match r1,r2,r3,r4,r5,r6,r7,r8 with
             | (_,Y,_),(_,Y,_),(_,Y,_),_,_,_,_,_ -> updateStatusOrNot a9 9     // A4-B4-C4 Y mill ~Middle
             | _-> a9
    let a11= match r1,r2,r3,r4,r5,r6,r7,r8 with
             | (_,_,Y),(_,_,Y),(_,_,Y),_,_,_,_,_ -> updateStatusOrNot a10 10  // A7-B6-C5 Y mill
             | _-> a10
    let a12= match r1,r2,r3,r4,r5,r6,r7,r8 with
             | (Y,_,_),_,_,(Y,_,_),_,_,_,(Y,_,_) -> updateStatusOrNot a11 11     // A1-D1-G1 Y mill ~leftmost vertical
             | _-> a11
    let a13= match r1,r2,r3,r4,r5,r6,r7,r8 with
             | _,(Y,_,_),_,(_,Y,_),_,_,(Y,_,_),_ -> updateStatusOrNot a12 12     // B2-D2-F2 Y mill
             | _-> a12
    let a14= match r1,r2,r3,r4,r5,r6,r7,r8 with
             | _,_,(Y,_,_),(_,_,Y),_,(Y,_,_),_,_ -> updateStatusOrNot a13 13     // C3-D3-E3 Y mill
             | _-> a13
    let a15= match r1,r2,r3,r4,r5,r6,r7,r8 with
             | _,_,_,_,_,(Y,_,_),(Y,_,_),(Y,_,_) -> updateStatusOrNot a14 14   // G1-F2-E3 Y mill
             | _-> a14
    let a16= match r1,r2,r3,r4,r5,r6,r7,r8 with
             | _,_,_,_,_,(_,Y,_),(_,Y,_),(_,Y,_) -> updateStatusOrNot a15 15     // G4-F4-E4 Y mill ~Middle
             | _-> a15
    let a17= match r1,r2,r3,r4,r5,r6,r7,r8 with
             | _,_,_,_,_,(_,_,Y),(_,_,Y),(_,_,Y) -> updateStatusOrNot a16 16    // G7-F6-E5 Y mill
             | _-> a16
    let a18= match r1,r2,r3,r4,r5,r6,r7,r8 with
             | (_,_,Y),_,_,_,(_,_,Y),_,_,(_,_,Y) -> updateStatusOrNot a17 17    // A7-D7-G7 Y mill ~rightmost vertical
             | _-> a17
    let a19= match r1,r2,r3,r4,r5,r6,r7,r8 with
             | _,(_,_,Y),_,_,(_,Y,_),_,(_,_,Y),_ -> updateStatusOrNot a18 18    // B6-D6-F6 Y mill
             | _-> a18
    let a20= match r1,r2,r3,r4,r5,r6,r7,r8 with
             | _,_,(_,_,Y),_,(Y,_,_),(_,_,Y),_,_ -> updateStatusOrNot a19 19  // C5-D5-E5 Y mill  
             | _-> a19 
    a20

let mill_Y_Check2 game (yMlist:'int list) =
    let (Board (r1,r2,r3,r4,r5,r6,r7,r8)) = game
    let y = match r1,r2,r3,r4,r5,r6,r7,r8 with
            | (Y,_,_),(Y,_,_),(Y,_,_),_,_,_,_,_ -> currentStatus yMlist 8
            | _ -> 0              
    let y1= match r1,r2,r3,r4,r5,r6,r7,r8 with
            | (_,Y,_),(_,Y,_),(_,Y,_),_,_,_,_,_ -> currentStatus yMlist 9     // A4-B4-C4 Y mill ~Middle
            | _-> 0
    let y2= match r1,r2,r3,r4,r5,r6,r7,r8 with
            | (_,_,Y),(_,_,Y),(_,_,Y),_,_,_,_,_ -> currentStatus yMlist 10  // A7-B6-C5 Y mill
            | _-> 0
    let y3= match r1,r2,r3,r4,r5,r6,r7,r8 with
            | (Y,_,_),_,_,(Y,_,_),_,_,_,(Y,_,_) -> currentStatus yMlist 11     // A1-D1-G1 Y mill ~leftmost vertical
            | _-> 0
    let y4= match r1,r2,r3,r4,r5,r6,r7,r8 with
            | _,(Y,_,_),_,(_,Y,_),_,_,(Y,_,_),_ -> currentStatus yMlist 12     // B2-D2-F2 Y mill
            | _-> 0
    let y5= match r1,r2,r3,r4,r5,r6,r7,r8 with
            | _,_,(Y,_,_),(_,_,Y),_,(Y,_,_),_,_ -> currentStatus yMlist 13     // C3-D3-E3 Y mill
            | _-> 0
    let y6= match r1,r2,r3,r4,r5,r6,r7,r8 with
            | _,_,_,_,_,(Y,_,_),(Y,_,_),(Y,_,_) -> currentStatus yMlist 14   // G1-F2-E3 Y mill
            | _-> 0
    
    match y = 1 || y1 = 1 || y2 = 1 || y3 = 1 || y4 = 1 || y5 = 1 || y6 = 1 with
    | true -> true
    | _ -> false

let mill_Y_Check3 game (yMlist:'int list) =
    let (Board (r1,r2,r3,r4,r5,r6,r7,r8)) = game
    let y1= match r1,r2,r3,r4,r5,r6,r7,r8 with
            | _,_,_,_,_,(_,Y,_),(_,Y,_),(_,Y,_) -> currentStatus yMlist 15     // G4-F4-E4 Y mill ~Middle
            | _-> 0
    let y2= match r1,r2,r3,r4,r5,r6,r7,r8 with
            | _,_,_,_,_,(_,_,Y),(_,_,Y),(_,_,Y) -> currentStatus yMlist 16    // G7-F6-E5 Y mill
            | _-> 0
    let y3= match r1,r2,r3,r4,r5,r6,r7,r8 with
            | (_,_,Y),_,_,_,(_,_,Y),_,_,(_,_,Y) -> currentStatus yMlist 17    // A7-D7-G7 Y mill ~rightmost vertical
            | _-> 0
    let y4= match r1,r2,r3,r4,r5,r6,r7,r8 with
            | _,(_,_,Y),_,_,(_,Y,_),_,(_,_,Y),_ -> currentStatus yMlist 18    // B6-D6-F6 Y mill
            | _-> 0
    let y5= match r1,r2,r3,r4,r5,r6,r7,r8 with
            | _,_,(_,_,Y),_,(Y,_,_),(_,_,Y),_,_ -> currentStatus yMlist 19  // C5-D5-E5 Y mill  
            | _-> 0 

    match y1 = 1 || y2 = 1 || y3 = 1 || y4 = 1 || y5 = 1 with
    | true -> true
    | _ -> false

let mill_M_Update game (m_Mlist:'int list) =
    let (Board (r1,r2,r3,r4,r5,r6,r7,r8)) = game
    
    let a1 = match r1,r2,r3,r4,r5,r6,r7,r8 with
             | (M,M,M),_,_,_,_,_,_,_ -> updateStatusOrNot m_Mlist 0 
             | _ -> m_Mlist
    let a2 = match r1,r2,r3,r4,r5,r6,r7,r8 with
             | _,(M,M,M),_,_,_,_,_,_ -> updateStatusOrNot a1 1     // r2 M mill ~B
             | _-> a1
    let a3 = match r1,r2,r3,r4,r5,r6,r7,r8 with
             | _,_,(M,M,M),_,_,_,_,_ -> updateStatusOrNot a2 2      // r3 M mill ~C
             | _-> a2
    let a4 = match r1,r2,r3,r4,r5,r6,r7,r8 with
             | _,_,_,(M,M,M),_,_,_,_ -> updateStatusOrNot a3 3      // r4 M mill ~D1 ~Middle
             | _-> a3
    let a5 = match r1,r2,r3,r4,r5,r6,r7,r8 with
             | _,_,_,_,(M,M,M),_,_,_ -> updateStatusOrNot a4 4      // r5 M mill ~D2 ~Middle
             | _-> a4
    let a6 = match r1,r2,r3,r4,r5,r6,r7,r8 with
             | _,_,_,_,_,(M,M,M),_,_ -> updateStatusOrNot a5 5     // r6 M mill ~E
             | _-> a5
    let a7 = match r1,r2,r3,r4,r5,r6,r7,r8 with
             | _,_,_,_,_,_,(M,M,M),_ -> updateStatusOrNot a6 6     // r7 M mill ~F
             | _-> a6
    let a8 = match r1,r2,r3,r4,r5,r6,r7,r8 with
             | _,_,_,_,_,_,_,(M,M,M) -> updateStatusOrNot a7 7  // r8 M mill ~G
             | _-> a7
    let a9 = match r1,r2,r3,r4,r5,r6,r7,r8 with
             | (M,_,_),(M,_,_),(M,_,_),_,_,_,_,_ -> updateStatusOrNot a8 8
             | _ -> a8
    let a10= match r1,r2,r3,r4,r5,r6,r7,r8 with
             | (_,M,_),(_,M,_),(_,M,_),_,_,_,_,_ -> updateStatusOrNot a9 9     // A4-B4-C4 M mill ~Middle
             | _-> a9
    let a11= match r1,r2,r3,r4,r5,r6,r7,r8 with
             | (_,_,M),(_,_,M),(_,_,M),_,_,_,_,_ -> updateStatusOrNot a10 10  // A7-B6-C5 M mill
             | _-> a10
    let a12= match r1,r2,r3,r4,r5,r6,r7,r8 with
             | (M,_,_),_,_,(M,_,_),_,_,_,(M,_,_) -> updateStatusOrNot a11 11     // A1-D1-G1 M mill ~leftmost vertical
             | _-> a11
    let a13= match r1,r2,r3,r4,r5,r6,r7,r8 with
             | _,(M,_,_),_,(_,M,_),_,_,(M,_,_),_ -> updateStatusOrNot a12 12     // B2-D2-F2 M mill
             | _-> a12
    let a14= match r1,r2,r3,r4,r5,r6,r7,r8 with
             | _,_,(M,_,_),(_,_,M),_,(M,_,_),_,_ -> updateStatusOrNot a13 13     // C3-D3-E3 M mill
             | _-> a13
    let a15= match r1,r2,r3,r4,r5,r6,r7,r8 with
             | _,_,_,_,_,(M,_,_),(M,_,_),(M,_,_) -> updateStatusOrNot a14 14   // G1-F2-E3 M mill
             | _-> a14
    let a16= match r1,r2,r3,r4,r5,r6,r7,r8 with
             | _,_,_,_,_,(_,M,_),(_,M,_),(_,M,_) -> updateStatusOrNot a15 15     // G4-F4-E4 M mill ~Middle
             | _-> a15
    let a17= match r1,r2,r3,r4,r5,r6,r7,r8 with
             | _,_,_,_,_,(_,_,M),(_,_,M),(_,_,M) -> updateStatusOrNot a16 16    // G7-F6-E5 M mill
             | _-> a16
    let a18= match r1,r2,r3,r4,r5,r6,r7,r8 with
             | (_,_,M),_,_,_,(_,_,M),_,_,(_,_,M) -> updateStatusOrNot a17 17    // A7-D7-G7 M mill ~rightmost vertical
             | _-> a17
    let a19= match r1,r2,r3,r4,r5,r6,r7,r8 with
             | _,(_,_,M),_,_,(_,M,_),_,(_,_,M),_ -> updateStatusOrNot a18 18    // B6-D6-F6 M mill
             | _-> a18
    let a20= match r1,r2,r3,r4,r5,r6,r7,r8 with
             | _,_,(_,_,M),_,(M,_,_),(_,_,M),_,_ -> updateStatusOrNot a19 19  // C5-D5-E5 M mill  
             | _-> a19 
    a20

let mill_M_Check game (m_Mlist:'int list) =
    let (Board (r1,r2,r3,r4,r5,r6,r7,r8)) = game
    
    let M = match r1,r2,r3,r4,r5,r6,r7,r8 with
             | (M,M,M),_,_,_,_,_,_,_ -> currentStatus m_Mlist 0
             | _ -> 0
    let M1 = match r1,r2,r3,r4,r5,r6,r7,r8 with
             | _,(M,M,M),_,_,_,_,_,_ -> currentStatus m_Mlist 1    // r2 M mill ~B
             | _-> 0
    let M2 = match r1,r2,r3,r4,r5,r6,r7,r8 with
             | _,_,(M,M,M),_,_,_,_,_ -> currentStatus m_Mlist 2     // r3 M mill ~C
             | _-> 0
    let M3 = match r1,r2,r3,r4,r5,r6,r7,r8 with
             | _,_,_,(M,M,M),_,_,_,_ -> currentStatus m_Mlist 3     // r4 M mill ~D1 ~Middle
             | _-> 0
    let M4 = match r1,r2,r3,r4,r5,r6,r7,r8 with
             | _,_,_,_,(M,M,M),_,_,_ -> currentStatus m_Mlist 4     // r5 M mill ~D2 ~Middle
             | _-> 0
    let M5 = match r1,r2,r3,r4,r5,r6,r7,r8 with
             | _,_,_,_,_,(M,M,M),_,_ -> currentStatus m_Mlist 5    // r6 M mill ~E
             | _-> 0
    let M6 = match r1,r2,r3,r4,r5,r6,r7,r8 with
             | _,_,_,_,_,_,(M,M,M),_ -> currentStatus m_Mlist 6    // r7 M mill ~F
             | _-> 0
    let M7 = match r1,r2,r3,r4,r5,r6,r7,r8 with
             | _,_,_,_,_,_,_,(M,M,M) -> currentStatus m_Mlist 7 // r8 M mill ~G
             | _-> 0

   //let ans =
    match M = 1 || M1 = 1 || M2 = 1 || M3 = 1 || M4 = 1 || M5 = 1 || M6 = 1 || M7 = 1 with
    | true -> true
    | _ -> false

let mill_M_Check2 game (m_Mlist:'int list) =
    let (Board (r1,r2,r3,r4,r5,r6,r7,r8)) = game
    let M = match r1,r2,r3,r4,r5,r6,r7,r8 with
            | (M,_,_),(M,_,_),(M,_,_),_,_,_,_,_ -> currentStatus m_Mlist 8
            | _ -> 0              
    let M1= match r1,r2,r3,r4,r5,r6,r7,r8 with
            | (_,M,_),(_,M,_),(_,M,_),_,_,_,_,_ -> currentStatus m_Mlist 9     // A4-B4-C4 M mill ~Middle
            | _-> 0
    let M2= match r1,r2,r3,r4,r5,r6,r7,r8 with
            | (_,_,M),(_,_,M),(_,_,M),_,_,_,_,_ -> currentStatus m_Mlist 10  // A7-B6-C5 M mill
            | _-> 0
    let M3= match r1,r2,r3,r4,r5,r6,r7,r8 with
            | (M,_,_),_,_,(M,_,_),_,_,_,(M,_,_) -> currentStatus m_Mlist 11     // A1-D1-G1 M mill ~leftmost vertical
            | _-> 0
    let M4= match r1,r2,r3,r4,r5,r6,r7,r8 with
            | _,(M,_,_),_,(_,M,_),_,_,(M,_,_),_ -> currentStatus m_Mlist 12     // B2-D2-F2 M mill
            | _-> 0
    let M5= match r1,r2,r3,r4,r5,r6,r7,r8 with
            | _,_,(M,_,_),(_,_,M),_,(M,_,_),_,_ -> currentStatus m_Mlist 13     // C3-D3-E3 M mill
            | _-> 0
    let M6= match r1,r2,r3,r4,r5,r6,r7,r8 with
            | _,_,_,_,_,(M,_,_),(M,_,_),(M,_,_) -> currentStatus m_Mlist 14   // G1-F2-E3 M mill
            | _-> 0
    
    match M = 1 || M1 = 1 || M2 = 1 || M3 = 1 || M4 = 1 || M5 = 1 || M6 = 1 with
    | true -> true
    | _ -> false

let mill_M_Check3 game (m_Mlist:'int list) =
    let (Board (r1,r2,r3,r4,r5,r6,r7,r8)) = game
    let M1= match r1,r2,r3,r4,r5,r6,r7,r8 with
            | _,_,_,_,_,(_,M,_),(_,M,_),(_,M,_) -> currentStatus m_Mlist 15     // G4-F4-E4 M mill ~Middle
            | _-> 0
    let M2= match r1,r2,r3,r4,r5,r6,r7,r8 with
            | _,_,_,_,_,(_,_,M),(_,_,M),(_,_,M) -> currentStatus m_Mlist 16    // G7-F6-E5 M mill
            | _-> 0
    let M3= match r1,r2,r3,r4,r5,r6,r7,r8 with
            | (_,_,M),_,_,_,(_,_,M),_,_,(_,_,M) -> currentStatus m_Mlist 17    // A7-D7-G7 M mill ~rightmost vertical
            | _-> 0
    let M4= match r1,r2,r3,r4,r5,r6,r7,r8 with
            | _,(_,_,M),_,_,(_,M,_),_,(_,_,M),_ -> currentStatus m_Mlist 18    // B6-D6-F6 M mill
            | _-> 0
    let M5= match r1,r2,r3,r4,r5,r6,r7,r8 with
            | _,_,(_,_,M),_,(M,_,_),(_,_,M),_,_ -> currentStatus m_Mlist 19  // C5-D5-E5 M mill  
            | _-> 0 

    match M1 = 1 || M2 = 1 || M3 = 1 || M4 = 1 || M5 = 1 with
    | true -> true
    | _ -> false

let updateMill_status3 (list:'int list) idx1 idx2 idx3 = 
    match list.[idx1] = 1 || list.[idx2] = 1 || list.[idx3] = 1 with
     | true -> let a = updateList2 list idx1
               let b = updateList2 a idx2
               let c = updateList2 b idx3
               c
     | _ -> list

let updateMill_status2 (list:'int list) idx1 idx2 = 
    match list.[idx1] = 1 || list.[idx2] = 1 with
     | true -> let a = updateList2 list idx1
               let b = updateList2 a idx2
               b
     | _ -> list

let Broken_yMill position y_Mlist =
    let y = match position with
            | "A1" -> updateMill_status3 y_Mlist 0 8 11
            | "A4" -> updateMill_status2 y_Mlist 9 0
            | "A7" -> updateMill_status3 y_Mlist 0 10 17
            | "B2" -> updateMill_status3 y_Mlist 1 8 12
            | "B4" -> updateMill_status2 y_Mlist 1 9
            | "B6" -> updateMill_status3 y_Mlist 1 10 18
            | "C3" -> updateMill_status3 y_Mlist 2 8 13
            | "C4" -> updateMill_status2 y_Mlist 2 9 
            | "C5" -> updateMill_status3 y_Mlist 2 10 19
            | "D1" -> updateMill_status2 y_Mlist 3 11
            | "D2" -> updateMill_status2 y_Mlist 3 12
            | "D3" -> updateMill_status2 y_Mlist 3 13
            | "D5" -> updateMill_status2 y_Mlist 4 19
            | "D6" -> updateMill_status2 y_Mlist 4 18
            | "D7" -> updateMill_status2 y_Mlist 4 17
            | "E3" -> updateMill_status3 y_Mlist 5 14 13
            | "E4" -> updateMill_status2 y_Mlist 5 15 
            | "E5" -> updateMill_status3 y_Mlist 5 19 16
            | "F2" -> updateMill_status3 y_Mlist 6 12 14
            | "F4" -> updateMill_status2 y_Mlist 6 15
            | "F6" -> updateMill_status3 y_Mlist 6 16 18
            | "G1" -> updateMill_status3 y_Mlist 7 14 11
            | "G4" -> updateMill_status2 y_Mlist 7 15
            | "G7" -> updateMill_status3 y_Mlist 7 17 16
            | _ -> y_Mlist
    y
    
let Broken_mMill position m_Mlist =
    let m = match position with
            | "A1" -> updateMill_status3 m_Mlist 0 8 11
            | "A4" -> updateMill_status2 m_Mlist 9 0
            | "A7" -> updateMill_status3 m_Mlist 0 10 17
            | "B2" -> updateMill_status3 m_Mlist 1 8 12
            | "B4" -> updateMill_status2 m_Mlist 1 9
            | "B6" -> updateMill_status3 m_Mlist 1 10 18
            | "C3" -> updateMill_status3 m_Mlist 2 8 13
            | "C4" -> updateMill_status2 m_Mlist 2 9 
            | "C5" -> updateMill_status3 m_Mlist 2 10 19
            | "D1" -> updateMill_status2 m_Mlist 3 11
            | "D2" -> updateMill_status2 m_Mlist 3 12
            | "D3" -> updateMill_status2 m_Mlist 3 13
            | "D5" -> updateMill_status2 m_Mlist 4 19
            | "D6" -> updateMill_status2 m_Mlist 4 18
            | "D7" -> updateMill_status2 m_Mlist 4 17
            | "E3" -> updateMill_status3 m_Mlist 5 14 13
            | "E4" -> updateMill_status2 m_Mlist 5 15 
            | "E5" -> updateMill_status3 m_Mlist 5 19 16
            | "F2" -> updateMill_status3 m_Mlist 6 12 14
            | "F4" -> updateMill_status2 m_Mlist 6 15
            | "F6" -> updateMill_status3 m_Mlist 6 16 18
            | "G1" -> updateMill_status3 m_Mlist 7 14 11
            | "G4" -> updateMill_status2 m_Mlist 7 15
            | "G7" -> updateMill_status3 m_Mlist 7 17 16
            | _ -> m_Mlist
    m

let kill (list: int list) player =
    match player with 
    | Y -> list.Head::(list.[1]-1)::[]
    | M -> (list.Head-1)::(list.[1])::[]
    | _ -> list

let rec eliminate (Board (r1,r2,r3,r4,r5,r6,r7,r8)) player =
    let game = Board (r1,r2,r3,r4,r5,r6,r7,r8)
    printf (" %A has mill! select a position to eliminate %A  :  ") player (swapPlayer player)
    let position = Console.ReadLine()
    
   // let otherP = swapPlayer player
    match (playerAtPos game position) with
    | Y -> match player with
           | M ->
            //Broken_Mill position
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
            //Broken_Mill position
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

let gameCheck game yMlist mMlist =

    match mill_Y_Check game yMlist || mill_Y_Check2 game yMlist || mill_Y_Check3 game yMlist || mill_M_Check game mMlist || mill_M_Check2 game mMlist || mill_M_Check3 game mMlist with
    | true -> Mill game
    | _ -> Ongoing game

let printBoard (Board (r1, r2, r3,r4,r5,r6,r7,r8)) (cows:int list)= 
                   
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
                                 printfn "\t\t\t\t |     |  \         |        /  |    | \t\t Cows: %d \t\t Cows: %d" cows.[0] cows.[1]
                  let printBdC = printfn "\t\t\tC\t |     |   %A_____%A____%A   |    |" (tc c3) (tc c4) (tc c5)
                                 printfn "\t\t\t\t |     |    |              |    |    | \t\t kills: %d \t\t kills: %d " (12-cows.[0]) (12-cows.[1])
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

let makeMove symbol (Board (r1,r2,r3,r4,r5,r6,r7,r8)) position yMlist mMlist =
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
    gameCheck newBoard yMlist mMlist
    
let moveCow (Board (r1, r2, r3,r4,r5,r6,r7,r8)) play moveFrom moveTo yMlist mMlist=
    
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
    | true ->gameCheck (ammend RemoveCow moveTo play) yMlist mMlist
    | _->  gameCheck currentBoard yMlist mMlist

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
 
let rec run player game yMlist mMlist (cows:int list)=
    // need to find the blank cells that can be used...
    printBoard game cows
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
        | true -> makeMove player game n yMlist mMlist
        | _ -> run player game yMlist mMlist cows
    | _ -> run player game yMlist mMlist cows

let rec run1 player game yMlist mMlist  (cows:int list) =
    // need to find the blank cells that can be used...
    printBoard game cows
    printf "%A Enter position to move FROM : " player
    let moveFrom = Console.ReadLine()

    printf "%A Enter position to move TO : " player
    let moveTo = Console.ReadLine()
    
    match playerAtPos game moveFrom with
    | Y ->
         match player with
         | Y -> let checkNeighbours = 

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
                            | true -> //Broken_Mill moveFrom
                                      (moveCow game player moveFrom moveTo yMlist mMlist,moveFrom)
                            | _ -> run1 player game yMlist mMlist cows
                        | _ -> run1 player game yMlist mMlist cows
                    | _ -> run1 player game yMlist mMlist cows
                | _ ->run1 player game yMlist mMlist cows   
            | _ ->run1 player game yMlist mMlist cows
         | _ -> run1 player game yMlist mMlist cows
    | M ->
         match player with
         | M -> let checkNeighbours = 

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
                            | true -> //Broken_Mill moveFrom
                                      (moveCow game player moveFrom moveTo yMlist mMlist,moveFrom)
                            | _ -> run1 player game yMlist mMlist cows
                        | _ -> run1 player game yMlist mMlist cows
                    | _ -> run1 player game yMlist mMlist cows
                | _ ->run1 player game yMlist mMlist cows    
         | _ ->run1 player game yMlist mMlist cows
    | _ -> run1 player game yMlist mMlist cows
      
let rec run2 player game (cows:int list) =
    // need to find the blank cells that can be used...
    printBoard game cows
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
           | true -> //Broken_Mill moveFrom 
                     flyCow game player moveFrom moveTo
           | _ -> run2 player game cows
        | _ -> run2 player game cows
     | _ -> run2 player game cows      

let rec runGame currentPlayer game i yMlist m_Mill_List (cows:int list) =
    let playAgain () =
        printfn "Play again? [y/N] "
        match System.Console.ReadLine() with
        | "Y" | "y" -> runGame Y blankBoard i yMlist m_Mill_List cows
        | _ -> ()
    match i with
    | 0 ->  match currentPlayer with
            | Y -> match cows.[0] with
                   | 2 -> printf "\n\nGame Over!!!! %A is the winner" currentPlayer
                   | 3 -> match run2 currentPlayer game yMlist m_Mill_List cows with
                            | Mill newBoard-> printBoard newBoard cows
                                              let newcows = kill cows currentPlayer
                                              let newB = eliminate newBoard currentPlayer
                                              runGame (swapPlayer currentPlayer) newB 0 yMlist m_Mill_List newcows
                            | Ongoing newBoard -> runGame (swapPlayer currentPlayer) newBoard 0 yMlist m_Mill_List cows 
                            | _ -> printfn "OVER"
                   | _ -> match run1 currentPlayer game yMlist m_Mill_List cows with
                           | Mill newBoard,_-> printBoard newBoard cows
                                               let newYMlist = mill_Y_Update newBoard yMlist
                                               let new_M_Mlist = mill_M_Update newBoard m_Mill_List                         
                                               let newcows = kill cows currentPlayer
                                               let newB = eliminate newBoard currentPlayer 
                                               runGame (swapPlayer currentPlayer) newB 0 newYMlist new_M_Mlist newcows
                           | Ongoing newBoard, pos -> let newYMlist = Broken_yMill pos yMlist 
                                                      let new_M_Mlist = Broken_yMill pos m_Mill_List                             
                                                      runGame (swapPlayer currentPlayer) newBoard 0 newYMlist new_M_Mlist cows
                                                    
 
                   
            | M -> match cows.[1] with
                   | 2 -> printf "\n\nGame Over!!!! %A is the winner" currentPlayer
                   | 3 -> match run2 currentPlayer game yMlist m_Mill_List cows with
                            | Mill newBoard-> printBoard newBoard cows
                                              let newB = eliminate newBoard currentPlayer
                                              let newcows = kill cows currentPlayer
                                              runGame (swapPlayer currentPlayer) newB 0 yMlist m_Mill_List newcows
                            | Ongoing newBoard -> runGame (swapPlayer currentPlayer) newBoard 0 yMlist m_Mill_List cows
                   | _ ->
                        match run1 currentPlayer game yMlist m_Mill_List cows with
                         | Mill newBoard,_-> printBoard newBoard cows
                                             let newYMlist = mill_Y_Update newBoard yMlist
                                             let new_M_Mlist = mill_M_Update newBoard m_Mill_List                         
                                             let newB = eliminate newBoard currentPlayer
                                             let newcows = kill cows currentPlayer
                                             runGame (swapPlayer currentPlayer) newB 0 newYMlist new_M_Mlist newcows
                         | Ongoing newBoard, pos -> let newYMlist = Broken_yMill pos yMlist 
                                                    let new_M_Mlist = Broken_yMill pos m_Mill_List                             
                                                    runGame (swapPlayer currentPlayer) newBoard 0 newYMlist new_M_Mlist cows
            | _ -> ()
    | _-> match run currentPlayer game yMlist m_Mill_List cows with
            | Mill newBoard-> printBoard newBoard cows
                              let newYMlist = mill_Y_Update newBoard yMlist
                              let new_M_Mlist = mill_M_Update newBoard m_Mill_List
                              let newcows = kill cows currentPlayer
                              let newB = eliminate newBoard currentPlayer
                              runGame (swapPlayer currentPlayer) newB (i-1) newYMlist new_M_Mlist newcows
            | Ongoing newBoard -> runGame (swapPlayer currentPlayer) newBoard (i-1) yMlist m_Mill_List cows
 
[<EntryPoint>]
let main argv =
    Console.ForegroundColor<-ConsoleColor.Cyan 
    let Y_millList = [0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0]; 
    let M_millList = [0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0];
    let cows = [12;12]
    runGame Y blankBoard 24 Y_millList M_millList cows

    0 
(*
                       ----------------------------------------------- PROGRESS ------------------------------------------------------
                       1. MILL BREAK FROM MOVE AND FLY, WAIT 1 TURN BEFORE MATCHING A SAME MILL AGAIN                             _DONE
                           
*)