module internal Eval
    open StateMonad

    let hello = [('H', 4); ('E', 1); ('L', 1); ('L', 1); ('O', 1)]
    let state = mkState [("x", 5); ("y", 42)] hello ["_pos_"; "_result_"]
    let emptyState = mkState [] [] []

    let monop f a =
        a >>= fun x ->
        ret (f x)

    let binop f a b =
        a >>= fun x -> 
        b >>= fun y -> 
        ret (f x y)

    let add a b = binop ( + ) a b

    let div a b =
        a >>= fun x -> 
        b >>= fun y -> 
            match y <> 0 with
            | true -> ret (x / y)
            | false -> fail DivisionByZero

    let modulo a b =
        a >>= fun x -> 
        b >>= fun y -> 
            match y <> 0 with
            | true -> ret (x % y)
            | false -> fail DivisionByZero    

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

    let rec arithEval a : SM<int> = 
        match a with
        | N n -> ret n
        | V v -> lookup v
        | WL -> wordLength
        | PV x -> (arithEval x) >>= pointValue
        | Add (x, y) -> add (arithEval x) (arithEval y)
        | Sub (x, y) -> binop ( - ) (arithEval x) (arithEval y)
        | Mul (x, y) -> binop ( * ) (arithEval x) (arithEval y)
        | Div (x, y) -> div (arithEval x) (arithEval y)
        | Mod (x, y) -> modulo (arithEval x) (arithEval y)
        | CharToInt x -> monop int (charEval x)
    and charEval c : SM<char> =
        match c with
        | C c -> ret c
        | CV x -> (arithEval x) >>= characterValue
        | ToUpper x -> monop System.Char.ToUpper (charEval x) 
        | ToLower x -> monop System.Char.ToLower (charEval x)
        | IntToChar x -> monop char (arithEval x)
    
    let isVowel (c:char) = "AEIOU" |> String.exists (( = ) (System.Char.ToUpper c))
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
        | IsDigit x -> monop System.Char.IsDigit (charEval x)

    type stm =
    | Declare of string
    | Ass of string * aExp
    | Skip
    | Seq of stm * stm
    | ITE of bExp * stm * stm
    | While of bExp * stm
