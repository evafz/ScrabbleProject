module internal Eval
    open StateMonad

    let hello = [('H', 4); ('E', 1); ('L', 1); ('L', 1); ('O', 1)]
    let state = mkState [("x", 5); ("y", 42)] hello ["_pos_"; "_result_"]
    let emptyState = mkState [] [] []

    let add a b = a >>= 
                            (fun x -> b >>= 
                                        (fun y -> ret (x+y)))      

    let div a b = a >>= 
                        (fun x -> b >>= 
                                    (fun y -> if y=0 
                                                    then fail DivisionByZero 
                                                    else ret (x/y)))  

    let modul a b = a >>= 
                        (fun x -> b >>= 
                                    (fun y -> if y=0 
                                                    then fail DivisionByZero 
                                                    else ret (x%y)))    


    type aExp =
        | N of int
        | V of string
        | WL
        | PV of aExp
        | Add of aExp * aExp
        | Sub of aExp * aExp
        | Mul of aExp * aExp
        | Div of aExp * aExp
        | Mod of aExp * aExp
        | CharToInt of cExp

    and cExp =
       | C  of char
       | CV of aExp
       | ToUpper of cExp
       | ToLower of cExp
       | IntToChar of aExp

    type bExp =             
       | TT
       | FF
       | AEq of aExp * aExp
       | ALt of aExp * aExp
       | Not of bExp
       | Conj of bExp * bExp
       | IsVowel of cExp
       | IsConsonant of cExp
       | IsLetter of cExp
       | IsDigit of cExp

    let (.+.) a b = Add (a, b)
    let (.-.) a b = Sub (a, b)
    let (.*.) a b = Mul (a, b)
    let (./.) a b = Div (a, b)
    let (.%.) a b = Mod (a, b)

    let (~~) b = Not b
    let (.&&.) b1 b2 = Conj (b1, b2)
    let (.||.) b1 b2 = ~~(~~b1 .&&. ~~b2)
    let (.->.) b1 b2 = (~~b1) .||. b2
       
    let (.=.) a b = AEq (a, b)
    let (.<.) a b = ALt (a, b)
    let (.<>.) a b = ~~(a .=. b)
    let (.<=.) a b = a .<. b .||. ~~(a .<>. b)
    let (.>=.) a b = ~~(a .<. b)
    let (.>.) a b = ~~(a .=. b) .&&. (a .>=. b)

    let binop f a b =
                a >>= fun x ->
                b >>= fun y ->
                ret (f x y)

    let monop f a = a >>= fun x -> ret (f x)


    let rec arithEval a : SM<int> = 
        match a with
            | N x -> ret x
            | V x -> lookup x
            | WL -> wordLength
            | PV x -> arithEval x
            | Add (x, y) -> binop ( + ) (arithEval x) (arithEval y)
            | Sub (x, y) -> binop ( - ) (arithEval x) (arithEval y)
            | Mul (x, y) -> binop ( * ) (arithEval x) (arithEval y)
            | Div (x, y) -> div (arithEval x) (arithEval y)
            | Mod (x, y) -> modul (arithEval x) (arithEval y)
            | CharToInt x -> monop int (charEval x)
    

    and charEval c : SM<char> = 
        match c with
            | C x -> ret x
            | CV x -> monop char (arithEval x)
            | ToUpper x -> monop System.Char.ToUpper (charEval x)
            | ToLower x -> monop System.Char.ToLower (charEval x)
            | IntToChar x -> monop char (arithEval x)  

    let vowels = ['a'; 'e'; 'i'; 'o'; 'u'; 'y'; 'A'; 'E'; 'I'; 'O'; 'U'; 'Y']
    let isVowel a = match a with
                        | a when List.contains a vowels -> true
                        | _ -> false

    //this just checks that it is not a vowel but could still be non-letter values
    let isConsonant x = not (isVowel x)
    let rec boolEval b : SM<bool> = 
        match b with
                | TT -> ret true
                | FF -> ret false

                | AEq (x, y) -> binop ( = ) (arithEval x) (arithEval y)
                | ALt (x, y) -> binop ( < ) (arithEval x) (arithEval y)

                | Not x -> monop not (boolEval x)
                | Conj (x, y) -> binop ( && ) (boolEval x) (boolEval y)

                | IsVowel x -> monop isVowel (charEval x)
                | IsConsonant x -> monop isConsonant (charEval x)
                | IsLetter x -> monop System.Char.IsLetter (charEval x)
                | IsDigit x-> monop System.Char.IsDigit (charEval x)     (* check for digit *)



    type stm =
    | Declare of string
    | Ass of string * aExp
    | Skip
    | Seq of stm * stm
    | ITE of bExp * stm * stm
    | While of bExp * stm

    //let rec stmntEval stmnt : SM<unit> = 
      //  match stmnt with
        //    |Declare s

(* Part 3 (Optional) *)
    type StateBuilder() =

        member this.Bind(f, x)    = f >>= x
        member this.Return(x)     = ret x
        member this.ReturnFrom(x) = x
        member this.Delay(f)      = f ()
        member this.Combine(a, b) = a >>= (fun _ -> b)
        
    let prog = new StateBuilder()

    let arithEval2 a = failwith "Not implemented"
    let charEval2 c = failwith "Not implemented"
    let rec boolEval2 b = failwith "Not implemented"

    let stmntEval2 stm = failwith "Not implemented"

(* Part 4 *) 

    type word = (char * int) list
    type squareFun = word -> int -> int -> Result<int, Error>

    let stmntToSquareFun stm = failwith "Not implemented"

    type coord = int * int

    type boardFun = coord -> Result<squareFun option, Error> 

    let stmntToBoardFun stm m = failwith "Not implemented"

    type board = {
        center        : coord
        defaultSquare : squareFun
        squares       : boardFun
    }

    let mkBoard c defaultSq boardStmnt ids = failwith "Not implemented"
