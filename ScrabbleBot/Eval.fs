// Insert your updated Eval.fs file here from Assignment 7. All modules must be internal.
module internal Eval
    open StateMonad

    let add a b = failwith "Not implemented"
    let div a b = failwith "Not implemented"

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

    let arithEval a : SM<int> = failwith "Not implemented"

    let charEval c : SM<char> = failwith "Not implemented"

    let boolEval b : SM<bool> = failwith "Not implemented"

    type stm =
    | Declare of string
    | Ass of string * aExp
    | Skip
    | Seq of stm * stm
    | ITE of bExp * stm * stm
    | While of bExp * stm

    let rec stmntEval stmnt : SM<unit> = failwith "Not implemented"

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
