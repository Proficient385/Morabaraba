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

let currentStatus (yMlist:'int list) i = 
     match yMlist.[i] with
      | 0 -> 1
      | _ -> 0

let updateList (xs : int list) index : int list =
    let rec innerF xs out i =
     match xs with
     | [] -> List.rev out
     | F::rest -> 
       match i=index with
        | true -> match F with
                  | 1 -> innerF rest (2::out) (i+1)
                  | 0 -> innerF rest (1::out) (i+1) 
                  | _ -> innerF rest (xs.Head::out) (i+1)
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



/// <summary>
/// < Checking Mills formed by Y and M player respectively, function returns true if any new mill was formed
/// </summary>
/// <param name="game"> The game's current board to search mills from</param>
/// <param name="yMlist"> The list that contains states, which is a way of keeping track of previous mills, list is used to determine whether a mill was previous encountered </param>

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

/// <summary>
/// < following 2 functions are list returning function, that when given a list, calls other function to update the states should it be required
/// </summary>
/// <param name="game"> Game board</param>
/// <param name="yMlist"> List with states to be updated</param>

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
    
/// <summary>
/// < The following 2 functions returns a list and updating it as a move/fly had been made from a mill formation
/// </summary>
/// <param name="position"> Game board</param>
/// <param name="y_Mlist"> List with states to be updated</param>

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
    let m = 
        match position with
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

// Function to reduce opponent cows
let kill (list: int list) player =
    match player with 
    | Y -> list.Head::(list.[1]-1)::[]
    | M -> (list.Head-1)::(list.[1])::[]
    | _ -> list

// function used to check whether a mill is present
let check_positions1 idx1 idx2 idx3 (list:int list) =
    match list.[idx1] = 1 || list.[idx2] = 1 || list.[idx3] = 1 || list.[idx1] = 2 || list.[idx2] = 2 || list.[idx3] = 2 with
    | true -> 1
    | _ -> 0

 // function used to check whether a mill is present
let check_positions2 idx1 idx2 (list:int list) =
    match list.[idx1] = 1 || list.[idx2] = 1 || list.[idx1] = 2 || list.[idx2] = 2 with
    | true -> 1
    | _ -> 0

/// <summary>
/// <Function to check whether the cow to be killed was not alread in a mill
/// </summary>
/// <param name="pos"> Position of victim cow</param>
/// <param name="list">Victim's list to see if was in a mill</param>

let cowIN_mill pos (list:int list) =
    let status = 
        match pos with
        | "A1" -> check_positions1 0 8 11 list
        | "A4" -> check_positions2 0 9 list
        | "A7" -> check_positions1 0 10 17 list
        | "B2" -> check_positions1 1 8 12 list
        | "B4" -> check_positions2 1 9 list
        | "B6" -> check_positions1 1 10 18 list
        | "C3" -> check_positions1 2 8 13 list
        | "C4" -> check_positions2 2 9 list
        | "C5" -> check_positions1 2 10 19 list
        | "D1" -> check_positions2 3 11 list
        | "D2" -> check_positions2 3 12 list
        | "D3" -> check_positions2 3 13 list
        | "D5" -> check_positions2 4 19 list
        | "D6" -> check_positions2 4 18 list
        | "D7" -> check_positions2 4 17 list
        | "E3" -> check_positions1 5 13 14 list
        | "E4" -> check_positions2 5 15 list
        | "E5" -> check_positions1 5 16 19 list
        | "F2" -> check_positions1 6 12 14 list
        | "F4" -> check_positions2 6 15 list
        | "F6" -> check_positions1 6 16 18 list
        | "G1" -> check_positions1 7 11 14 list
        | "G4" -> check_positions2 7 15 list
        | "G7" -> check_positions1 7 16 17 list
        | _ -> 0
    
    match status with
    | 1 -> true
    | _ -> false

// Accept correct input for placing, moving and flying
let input_validation moveFrom moveTo =
   
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
        | "G1" | "G4" | "G7"  -> true
        | _ -> false
    | _ -> false

let input_vallidation2 (n:string) =
    match n with
    | "A1" | "A4" | "A7" 
    | "B2" | "B4" | "B6" 
    | "C3" | "C4" | "C5"
    | "D1" | "D2" | "D3" 
    | "D5" | "D6" | "D7" 
    | "E3" | "E4" | "E5" 
    | "F2" | "F4" | "F6" 
    | "G1" | "G4" | "G7" -> true
    | _ -> false

// Draw a new board with updated cell
let updateBoard (Board (r1,r2,r3,r4,r5,r6,r7,r8)) symbol position =
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
    newBoard

// When a mill is identified, it's time to kill, this function applies other functions to ensure that an elimination should take place
let rec eliminate (Board (r1,r2,r3,r4,r5,r6,r7,r8)) player ylist mlist =
    let game = Board (r1,r2,r3,r4,r5,r6,r7,r8)

    Console.ForegroundColor<-ConsoleColor.Yellow
    printf (" %A has mill! select a position to eliminate %A  :  ") player (swapPlayer player)
    Console.ForegroundColor<-ConsoleColor.Cyan
    
    let position = Console.ReadLine().ToUpper()
    
    match (playerAtPos game position, player) with
    | Y,M -> match cowIN_mill position ylist with             // Y to be killed by M
             | true -> Console.ForegroundColor<-ConsoleColor.Red 
                       printf "  The cow you selected is already in a mill, can't be killed\n"
                       Console.ForegroundColor<-ConsoleColor.Cyan
                       eliminate game player ylist mlist
             | _ -> updateBoard game Blank position

    | M,Y -> match cowIN_mill position mlist with
             | true -> Console.ForegroundColor<-ConsoleColor.Red
                       printf "  The cow you selected is already in a mill, can't be killed\n"
                       Console.ForegroundColor<-ConsoleColor.Cyan
                       eliminate game player mlist mlist
             | _ -> updateBoard game Blank position        
  
    | _ ->  Console.ForegroundColor<-ConsoleColor.Red
            printf "Can't kill yourself, a blank cell or imvalid position\n"
            Console.ForegroundColor<-ConsoleColor.Cyan
            eliminate game player ylist mlist 

/// <summary>
/// < Game Check is where relevant messages are broadcasted to the entire game, messages are MILL or ONGOING
      /// gameCheck is only applied to placing as few a mill is not conditional
      /// gameCheck2 is applied to moving and flying because mill is recognized if and only if the previous move did not result in breaking that mill which is supposely being formed
/// </summary>
/// <param name="game"></param>
/// <param name="yMlist"></param>
/// <param name="mMlist"></param>

let gameCheck game yMlist mMlist =

    match mill_Y_Check game yMlist || mill_Y_Check2 game yMlist || mill_Y_Check3 game yMlist || mill_M_Check game mMlist || mill_M_Check2 game mMlist || mill_M_Check3 game mMlist with
    | true -> Mill game
    | _ -> Ongoing game

let gameCheck2 game yMlist mMlist moveFrom moveTo (y_prevMove:string list) (m_prevMove: string list) player=
    match player with
    | Y -> 
           match y_prevMove.[0] = moveTo && y_prevMove.[1] = moveFrom with
            | true -> Ongoing game
            | _ ->  match mill_Y_Check game yMlist|| mill_Y_Check2 game yMlist || mill_Y_Check3 game yMlist  with
                    | true -> Mill game
                    | _ -> Ongoing game

    | M ->    
           match m_prevMove.[0] = moveTo && m_prevMove.[1] = moveFrom with
            | true -> Ongoing game
            | _ ->    match mill_M_Check game mMlist || mill_M_Check2 game mMlist || mill_M_Check3 game mMlist with
                      | true -> Mill game
                      | _ -> Ongoing game

    | _ -> Ongoing game

// Checks if a cell is empty
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

// create a new blank board
let blankBoard =
    let blankRow = Blank, Blank, Blank
    Board (blankRow, blankRow, blankRow,blankRow,blankRow,blankRow,blankRow,blankRow)

/// <summary>
/// <The following customized myprintfs functions are specially made for some color printing, to give Y and M player different colors 
/// </summary>
/// <param name="x"> Player {M,Y} </param>

let myprintf x =
    match x with
    | 'Y' -> Console.ForegroundColor<-ConsoleColor.Green
             printf " 'Y'"
             Console.ForegroundColor<-ConsoleColor.Cyan
             printf"______________"
    | 'M' -> Console.ForegroundColor<-ConsoleColor.Red
             printf " 'X'"
             Console.ForegroundColor<-ConsoleColor.Cyan
             printf"______________"
    | _ -> printf" ' '______________"

let myprintf2 x =
    match x with
    | 'Y' -> Console.ForegroundColor<-ConsoleColor.Green
             printfn "'Y'"
             Console.ForegroundColor<-ConsoleColor.Cyan
             
    | 'M' -> Console.ForegroundColor<-ConsoleColor.Red
             printfn "'X'"
             Console.ForegroundColor<-ConsoleColor.Cyan
            
    | _ -> printfn "' '"
   
let myprintf3 x =
    match x with
    | 'Y' -> Console.ForegroundColor<-ConsoleColor.Green
             printf "    'Y'"
             Console.ForegroundColor<-ConsoleColor.Cyan
             printf "__________"
    | 'M' -> Console.ForegroundColor<-ConsoleColor.Red
             printf "    'M'"
             Console.ForegroundColor<-ConsoleColor.Cyan
             printf"__________"
    | _ -> printf "    ' '__________"

let myprintf4 x =
    match x with
    | 'Y' -> Console.ForegroundColor<-ConsoleColor.Green
             printf "'Y'"
             Console.ForegroundColor<-ConsoleColor.Cyan
             printf "_________"
    | 'M' -> Console.ForegroundColor<-ConsoleColor.Red
             printf "'M'"
             Console.ForegroundColor<-ConsoleColor.Cyan
             printf"_________"
    | _ -> printf "' '_________"

let myprintf5 x =
    match x with
    | 'Y' -> Console.ForegroundColor<-ConsoleColor.Green
             printf "'Y'"
             Console.ForegroundColor<-ConsoleColor.Cyan
    | 'M' -> Console.ForegroundColor<-ConsoleColor.Red
             printf "'M'"
             Console.ForegroundColor<-ConsoleColor.Cyan
    | _ -> printf "' '"

let myprintf6 x =
    match x with
    | 'Y' -> Console.ForegroundColor<-ConsoleColor.Green
             printf "'Y'"
             Console.ForegroundColor<-ConsoleColor.Cyan
             printf "_____"
    | 'M' -> Console.ForegroundColor<-ConsoleColor.Red
             printf "'M'"
             Console.ForegroundColor<-ConsoleColor.Cyan
             printf"_____"
    | _ -> printf "' '_____"

let myprintf7 x =
    match x with
    | 'Y' -> Console.ForegroundColor<-ConsoleColor.Green
             printf "'Y'"
             Console.ForegroundColor<-ConsoleColor.Cyan
             printf "_____"
    | 'M' -> Console.ForegroundColor<-ConsoleColor.Red
             printf "'M'"
             Console.ForegroundColor<-ConsoleColor.Cyan
             printf"_____"
    | _ -> printf "' '_____"

let myprintf8 x =
    match x with
    | 'Y' -> Console.ForegroundColor<-ConsoleColor.Green
             printf "'Y'"
             Console.ForegroundColor<-ConsoleColor.Cyan
             printf "__"
    | 'M' -> Console.ForegroundColor<-ConsoleColor.Red
             printf "'M'"
             Console.ForegroundColor<-ConsoleColor.Cyan
             printf"__"
    | _ -> printf "' '__"

// Printing the whole board
let printBoard (Board (r1, r2, r3,r4,r5,r6,r7,r8)) (cows:int list)= 
                   
      System.Console.Clear ()
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
      let printBdA = printf "\t\t\tA\t" 
                     myprintf (tc a1)
                     myprintf (tc a4)
                     myprintf2 (tc a7)
                     printfn "\t\t\t\t |  \               |              / |"
                     printfn "\t\t\t\t |   \              |             /  | "
                     printfn "\t\t\t\t |    \             |            /   |"
      let printBdB = printf "\t\t\tB\t |"    
                     myprintf3 (tc b2)
                     myprintf4 (tc b4)
                     myprintf5 (tc b6)
                     printfn "   |" 
                     printfn "\t\t\t\t |     |\           |          /|    | \t\t Y \t\t\t\t M"
                     printfn "\t\t\t\t |     | \          |         / |    |"
                     printfn "\t\t\t\t |     |  \         |        /  |    | \t\t Cows: %d \t\t Cows: %d" cows.[0] cows.[1]
      let printBdC = printf "\t\t\tC\t |     |   "
                     myprintf6 (tc c3)
                     myprintf7 (tc c4)
                     myprintf5 (tc c5)   
                     printfn"  |    |"   
                     printfn "\t\t\t\t |     |    |              |    |    | \t\t kills: %d \t\t kills: %d " (12-cows.[1]) (12-cows.[0])
                     printfn "\t\t\t\t |     |    |              |    |    |"
                     printfn "\t\t\t\t |     |    |              |    |    |"
      let printBdD =
                     printf "\t\t\tD\t "
                     myprintf8 (tc d1)
                     myprintf8 (tc d2)
                     myprintf5 (tc d3)
                     printf "            "
                     myprintf8 (tc d5)
                     myprintf8 (tc d6)
                     myprintf5 (tc d7)
                     printfn ""
                     printfn "\t\t\t\t |     |    |              |    |    |"
                     
      let printBdE = printfn "\t\t\t\t |     |    |              |    |    |"
                     printfn "\t\t\t\t |     |    |              |    |    |"
                     printf "\t\t\tE\t |     |   "
                     myprintf6 (tc e3)
                     myprintf7 (tc e4)
                     myprintf5 (tc e5)
                     printfn"  |    |"   
             
      let printBdF = printfn "\t\t\t\t |     |   /        |        \  |    |"
                     printfn "\t\t\t\t |     |  /         |         \ |    |"
                     printfn "\t\t\t\t |     | /          |          \|    |"
                     printf "\t\t\tF\t |"   
                     myprintf3 (tc f2)
                     myprintf4 (tc f4)
                     myprintf5 (tc f6)  
                     printfn"   |"
      let printBdG  = printfn "\t\t\t\t |    /             |            \   |"
                      printfn "\t\t\t\t |   /              |             \  | "
                      printfn "\t\t\t\t |  /               |              \ |"
                      printf "\t\t\tG\t"
                      myprintf (tc g1)
                      myprintf (tc g4)
                      myprintf2 (tc g7)  
                      printfn ""
      printBdA
      printBdB
      printBdC
      printBdD
      printBdE
      printBdF
      printBdG

// place player
let makeMove symbol (Board (r1,r2,r3,r4,r5,r6,r7,r8)) position yMlist mMlist =
    let newBoard = updateBoard (Board (r1,r2,r3,r4,r5,r6,r7,r8)) symbol position
    gameCheck newBoard yMlist mMlist

// Check player's available cell to moveTo
let checkNeighbors moveFrom moveTo =
 
    let aOne = ["A4";"B2";"D1"]
    let aFour = ["A1";"A7";"B4"]
    let aSeven = ["A4";"B6";"D7"]
    let bTwo = ["A1";"C3";"B4";"D2"]
    let bFour = ["B2";"A4";"D6";"C4"]
    let bSix = ["A7";"B4";"D7";"C5"]
    let cThree = ["B2";"C4";"D3"]
    let cFour = ["C3";"B4";"C5"]
    let cFive = ["C4";"B6";"D5"]
    let dOne = ["A1";"D2";"G1"]
    let dTwo = ["D1";"B2";"D3";"F2"]
    let dThree =["C3";"D2";"E3"] 
    let dFive = ["C5";"D6";"E5"]
    let dSix = ["D5";"B6";"D7";"F6"]
    let dSeven = ["A7";"D6";"G7"]
    let eThree = ["D3";"E4";"F2"]
    let eFour = ["F4";"E3";"E5"]
    let eFive = ["D5";"E4";"F6"]
    let fTwo = ["G1";"E3";"F4";"D2"]
    let fFour = ["E4";"F6";"G4";"F2"] 
    let fSix = ["G7";"D6";"F4";"E5"]
    let gOne = ["D1";"F2";"G4"]
    let gFour = ["F4";"G1";"G7"]
    let gSeven = ["F6";"D7";"G4"]
    
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
 
// Move the cow 
let moveCow (Board (r1, r2, r3,r4,r5,r6,r7,r8)) play moveFrom moveTo yMlist mMlist y_prevMove m_prevMove=
    
    let currentBoard  = (Board (r1, r2, r3,r4,r5,r6,r7,r8))
    let RemoveCow  =  updateBoard currentBoard Blank moveFrom
    
    let newB = updateBoard RemoveCow play moveTo
    gameCheck2 newB yMlist mMlist moveFrom moveTo y_prevMove m_prevMove play
    
let flyCow (Board (r1, r2, r3,r4,r5,r6,r7,r8)) play moveFrom moveTo yMlist mMlist y_prevMove m_prevMove =
    let currentBoard  = (Board (r1, r2, r3,r4,r5,r6,r7,r8))
        
    let RemoveCow  =  updateBoard currentBoard Blank moveFrom
    let newB = updateBoard RemoveCow play moveTo
    gameCheck2 newB yMlist mMlist moveFrom moveTo y_prevMove m_prevMove play

/// <summary>
/// < Reseting values
/// </summary>
/// <param name="yMlist"></param>

let mill_reset (yMlist:'int list) =
   let x = List.length yMlist
   List.init x (fun _ -> 0)

let cowsLife (cows:'int list) =
   let x = List.length cows
   List.init x (fun _ -> 12)

let prev_moves (moves:'int list) = 
    let x = List.length moves
    List.init x (fun _ -> "")

// Recursive function to place to the board 
let rec run player game yMlist mMlist (cows:int list) v =
    // need to find the blank cells that can be used...
    printBoard game cows
    match v with 
    | 1 ->  Console.ForegroundColor<-ConsoleColor.Red
            printf " Invalid entry, either occupied or not in the board, try again \n"
            Console.ForegroundColor<-ConsoleColor.Cyan
    | _ -> ()

    printfn "%A's turn.  Type the number of the cell that you want to play into." player
    let n = System.Console.ReadLine().ToUpper()
    //let n = inp.ToUpper()
   
    match isBlank game n && input_vallidation2 n with
     | true -> makeMove player game n yMlist mMlist
     | _ -> run player game yMlist mMlist cows 1
 
// Recursive function to move cow in the board 
let rec run1 player game yMlist mMlist (cows:int list) y_prevMove m_prevMove v =
    // need to find the blank cells that can be used...
    printBoard game cows

    match v with 
    | 1 ->  Console.ForegroundColor<-ConsoleColor.Red
            printf " Invalid entry, you can't move enemy cow or you can't fly as yet, try again \n"
            Console.ForegroundColor<-ConsoleColor.Cyan
    | _ -> ()

    printf "%A Enter position to move FROM : " player
    let moveFrom = Console.ReadLine().ToUpper()

    printf "%A Enter position to move TO : " player
    let moveTo = Console.ReadLine().ToUpper()
    
    match (playerAtPos game moveFrom,player) with
    | Y,Y -> match (input_validation moveFrom moveTo) && (checkNeighbors moveFrom moveTo) && (isBlank game moveTo) with
             | true -> (moveCow game player moveFrom moveTo yMlist mMlist y_prevMove m_prevMove,moveFrom,moveTo)
             | _ -> run1 player game yMlist mMlist cows y_prevMove m_prevMove 1
    
    | M,M -> match (input_validation moveFrom moveTo) && (checkNeighbors moveFrom moveTo) && (isBlank game moveTo) with
             | true -> (moveCow game player moveFrom moveTo yMlist mMlist y_prevMove m_prevMove,moveFrom,moveTo)
             | _ -> run1 player game yMlist mMlist cows y_prevMove m_prevMove 1
    | _ -> run1 player game yMlist mMlist cows y_prevMove m_prevMove 1     
  
// Recursive  function to fly the cow 
let rec run2 player game yMlist mMlist (cows:int list) y_prevMove m_prevMove v =
    // need to find the blank cells that can be used...
    printBoard game cows

    match v with 
    | 1 ->  Console.ForegroundColor<-ConsoleColor.Red
            printf " Invalid entry, you can't fly enemy cow, try again \n"
            Console.ForegroundColor<-ConsoleColor.Cyan
    | _ -> ()

    printf "%A Enter position to move FROM : " player
    let moveFrom = Console.ReadLine().ToUpper()

    printf "%A Enter position to move TO : " player
    let moveTo = Console.ReadLine().ToUpper()
    
    match (playerAtPos game moveFrom,player) with
    | Y,Y -> match (input_validation moveFrom moveTo) && (isBlank game moveTo) with
             | true -> (flyCow game player moveFrom moveTo yMlist mMlist y_prevMove m_prevMove, moveFrom,moveTo)
             | _ -> run2 player game yMlist mMlist cows y_prevMove m_prevMove 1 
             
    | M,M -> match (input_validation moveFrom moveTo) && (isBlank game moveTo) with
             | true -> (flyCow game player moveFrom moveTo yMlist mMlist y_prevMove m_prevMove,moveFrom,moveTo)
             | _ -> run2 player game yMlist mMlist cows y_prevMove m_prevMove 1
    | _ -> run2 player game yMlist mMlist cows y_prevMove m_prevMove 1

// The main recursive function to run the whole game
let rec runGame currentPlayer game i yMlist m_Mill_List (cows:int list) y_prevMove m_prevMove =
    let playAgain yMlist m_Mill_List cows y_prevMove m_prevMove=
        Console.ForegroundColor<-ConsoleColor.Cyan
        printfn "Play again? [y/N] "
        match System.Console.ReadLine() with
        | "Y" | "y" -> runGame Y blankBoard i yMlist m_Mill_List cows y_prevMove m_prevMove
        | _ -> ()
    match i with
    | 0 ->  match currentPlayer,cows.[0] with
            | Y,2 -> Console.ForegroundColor<-ConsoleColor.DarkYellow
                     printf "\n\nGame Over!!!! %A is the winner\n\n" M
                     let newYlist = mill_reset yMlist
                     let newMlist = mill_reset m_Mill_List
                     let newcows = cowsLife cows
                     let newPrevMovesY = prev_moves y_prevMove
                     let newPrevMovesM = prev_moves m_prevMove
                     playAgain newYlist newMlist newcows newPrevMovesY newPrevMovesM
            | Y,3 ->  match run2 currentPlayer game yMlist m_Mill_List cows y_prevMove m_prevMove 0 with
                      | Mill newBoard,posF,posT-> printBoard newBoard cows
                                                  let newcows = kill cows currentPlayer
                                                  let newB = eliminate newBoard currentPlayer yMlist m_Mill_List
                                                  let new_y_prevMove = posF::posT::[]
                                                  runGame (swapPlayer currentPlayer) newB 0 yMlist m_Mill_List newcows new_y_prevMove m_prevMove
                      | Ongoing newBoard,posF,posT -> let newYMlist = Broken_yMill posF yMlist 
                                                      let new_M_Mlist = Broken_yMill posF m_Mill_List
                                                      let new_y_prevMove = posF::posT::[]
                                                      runGame (swapPlayer currentPlayer) newBoard 0 newYMlist new_M_Mlist cows new_y_prevMove m_prevMove
                            
            | Y,_ -> match run1 currentPlayer game yMlist m_Mill_List cows y_prevMove m_prevMove 0 with
                     | Mill newBoard,posF,posT->   printBoard newBoard cows
                                                   let newYMlist = mill_Y_Update newBoard yMlist
                                                   let new_M_Mlist = mill_M_Update newBoard m_Mill_List                         
                                                   let newcows = kill cows currentPlayer
                                                   let newB = eliminate newBoard currentPlayer yMlist m_Mill_List
                                                   let new_y_prevMove = [posF;posT]
                                                   runGame (swapPlayer currentPlayer) newB 0 newYMlist new_M_Mlist newcows new_y_prevMove m_prevMove
                     | Ongoing newBoard,posF,posT -> let newYMlist = Broken_yMill posF yMlist 
                                                     let new_M_Mlist = Broken_yMill posF m_Mill_List
                                                     let new_y_prevMove = [posF;posT]
                                                     runGame (swapPlayer currentPlayer) newBoard 0 newYMlist new_M_Mlist cows new_y_prevMove m_prevMove
             | _ -> ()      
            
            match currentPlayer,cows.[1] with
            | M,2 -> Console.ForegroundColor<-ConsoleColor.DarkYellow
                     printf "\n\nGame Over!!!! %A is the winner" Y
                     let newYlist = mill_reset yMlist
                     let newMlist = mill_reset m_Mill_List
                     let newcows = cowsLife cows
                     let newPrevMovesY = prev_moves y_prevMove
                     let newPrevMovesM = prev_moves m_prevMove
                     playAgain newYlist newMlist newcows newPrevMovesY newPrevMovesM
            | M,3 -> match run2 currentPlayer game yMlist m_Mill_List cows y_prevMove m_prevMove 0 with
                      | Mill newBoard,posF,posT-> printBoard newBoard cows
                                                  let newB = eliminate newBoard currentPlayer yMlist m_Mill_List
                                                  let newcows = kill cows currentPlayer
                                                  let new_m_prevMove = posF::posT::[]
                                                  runGame (swapPlayer currentPlayer) newB 0 yMlist m_Mill_List newcows y_prevMove new_m_prevMove
                      | Ongoing newBoard,posF,posT -> let newYMlist = Broken_yMill posF yMlist 
                                                      let new_M_Mlist = Broken_yMill posF m_Mill_List                             
                                                      let new_m_prevMove = posF::posT::[]
                                                      runGame (swapPlayer currentPlayer) newBoard 0 newYMlist new_M_Mlist cows y_prevMove new_m_prevMove
            | M,_ ->
                  match run1 currentPlayer game yMlist m_Mill_List cows y_prevMove m_prevMove 0 with
                   | Mill newBoard,posF,posT->   printBoard newBoard cows
                                                 let newYMlist = mill_Y_Update newBoard yMlist
                                                 let new_M_Mlist = mill_M_Update newBoard m_Mill_List                         
                                                 let newB = eliminate newBoard currentPlayer newYMlist new_M_Mlist
                                                 let newcows = kill cows currentPlayer
                                                 let new_m_prevMove = posF::posT::[]
                                                 runGame (swapPlayer currentPlayer) newB 0 newYMlist new_M_Mlist newcows y_prevMove new_m_prevMove
                   | Ongoing newBoard,posF,posT -> let newYMlist = Broken_yMill posF yMlist 
                                                   let new_M_Mlist = Broken_yMill posF m_Mill_List
                                                   let new_m_prevMove = posF::posT::[]
                                                   runGame (swapPlayer currentPlayer) newBoard 0 newYMlist new_M_Mlist cows y_prevMove new_m_prevMove
            | _ -> ()
    | _-> match run currentPlayer game yMlist m_Mill_List cows 0 with
            | Mill newBoard-> printBoard newBoard cows
                              let newYMlist = mill_Y_Update newBoard yMlist
                              let new_M_Mlist = mill_M_Update newBoard m_Mill_List
                              let newcows = kill cows currentPlayer
                              let newB = eliminate newBoard currentPlayer newYMlist new_M_Mlist
                              runGame (swapPlayer currentPlayer) newB (i-1) newYMlist new_M_Mlist newcows y_prevMove m_prevMove
            | Ongoing newBoard -> let newYMlist = mill_Y_Update newBoard yMlist
                                  let new_M_Mlist = mill_M_Update newBoard m_Mill_List
                                  runGame (swapPlayer currentPlayer) newBoard (i-1) newYMlist new_M_Mlist cows y_prevMove m_prevMove
 
[<EntryPoint>]
let main argv =
    Console.ForegroundColor<-ConsoleColor.Cyan 
    let y_millList = [0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0]; 
    let m_millList = [0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0];
    let cows = [12;12]
    let y_prevMove = ["";""]
    let m_prevMove = ["";""]
    runGame Y blankBoard 24 y_millList m_millList cows y_prevMove m_prevMove

    0 
(*
                       ----------------------------------------------- PROGRESS ------------------------------------------------------
                       1. DONE
                           
*)