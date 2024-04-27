// ScrabbleUtil contains the types coord, boardProg, and SquareProg. Remove these from your file before proceeding.
// Also note that the modulse Ass7 and ImpParser have been merged to one module called Parser.

// Insert your Parser.fs file here from Assignment 7. All modules must be internal.
module internal Parser
    open StateMonad
    open ScrabbleUtil
    open ScrabbleLib
    open Eval
    open FParsecLight.TextParser
    
    let pIntToChar  = pstring "intToChar"
    let pPointValue = pstring "pointValue"

    let pCharToInt  = pstring "charToInt"
    let pToUpper    = pstring "toUpper"
    let pToLower    = pstring "toLower"
    let pCharValue  = pstring "charValue"

    let pTrue       = pstring "true"
    let pFalse      = pstring "false"
    let pIsDigit    = pstring "isDigit"
    let pIsLetter   = pstring "isLetter"
    let pIsVowel   = pstring "isVowel"

    let pif       = pstring "if"
    let pthen     = pstring "then"
    let pelse     = pstring "else"
    let pwhile    = pstring "while"
    let pdo       = pstring "do"
    let pdeclare  = pstring "declare"

    let whitespaceChar = satisfy System.Char.IsWhiteSpace <?> "whitespace"
    let pletter        = satisfy System.Char.IsLetter <?> "letter"
    let palphanumeric  = satisfy System.Char.IsLetterOrDigit <?> "alphanumeric"

    let spaces         = many whitespaceChar <?> "space"
    let spaces1        = many1 whitespaceChar <?> "space1"

    let (.>*>.) p1 p2 = p1 .>> spaces .>>. p2
    let (.>*>) p1 p2  = p1 .>> spaces .>> p2
    let (>*>.) p1 p2  = p1 .>> spaces >>. p2

    let parenthesise p = pchar '(' >*>. p .>*> pchar ')'

    let charListToString (clist: char list) =
       System.String.Concat(Array.ofList(clist))


    
    let pid = (pchar '_' <|> pletter ) .>>. many palphanumeric |>> fun (head, tail) -> charListToString(head::tail)
    
    let unop op a = op >*>. a
    let binop op p1 p2 = p1 .>*> op .>*>. p2

    let TermParse, tref = createParserForwardedToRef<aExp>()
    let ProdParse, pref = createParserForwardedToRef<aExp>()
    let AtomParse, aref = createParserForwardedToRef<aExp>()

    let CEParse, cref = createParserForwardedToRef<cExp>()

    let SubParse = binop (pchar '-') ProdParse TermParse |>> Sub <?> "Sub"
    let AddParse = binop (pchar '+') ProdParse TermParse |>> Add <?> "Add"
    do tref := choice [AddParse; SubParse; ProdParse]


    let DivParse = binop (pchar '/') AtomParse ProdParse |>> Div <?> "Div"
    let ModParse = binop (pchar '%') AtomParse ProdParse |>> Mod <?> "Mod"
    let MulParse = binop (pchar '*') AtomParse ProdParse |>> Mul <?> "Mul"
    do pref := choice [MulParse; DivParse; ModParse; AtomParse]

    let VParse = pid |>> V <?> "String"
    let CharToIntParse = unop pCharToInt CEParse |>> CharToInt <?> "CharToInt"
    let PVParse = unop pPointValue AtomParse |>> PV <?> "PV"
    let NParse   = pint32 |>> N <?> "Int"

    let NegParse = unop (pchar '-') NParse |>> fun (number) -> Mul (N (-1), number) 
    let ParParse = parenthesise TermParse
    do aref := choice [ParParse; NegParse; PVParse; CharToIntParse; VParse; NParse]

    let AexpParse = TermParse 


    let CParse = pchar ''' >>. anyChar .>> pchar ''' |>> C <?> "Char"
    let CharValueParse = unop pCharValue TermParse |>> CV <?> "CharValue"
    let IntToCharParse = unop pIntToChar TermParse |>> IntToChar <?> "IntToChar"
    let ToLowerParse = unop pToLower CEParse |>> ToLower <?> "ToLower"
    let ToUpperParse = unop pToUpper CEParse |>> ToUpper <?> "ToUpper"

    let CParParse = parenthesise CEParse
    do cref := choice [CParParse; CParse; CharValueParse; IntToCharParse; ToLowerParse; ToUpperParse]
    let CexpParse = CEParse

    let BexpParse = pstring "not implemented"

    let stmParse = pstring "not implemented"

    (* The rest of your parser goes here *)
    type word   = (char * int) list
    //word + acc + pos gives total point value for word
    type squareFun = word -> int -> int -> Result<int, Error>
    type square = Map<int, squareFun>
    
    type boardFun2 = coord -> Result<square option, Error>
        
    type board = {
        center        : coord
        defaultSquare : square
        squares       : boardFun2
    }
    
    // Default (unusable) board in case you are not implementing a parser for the DSL.
    let mkBoard : boardProg -> board = fun _ -> {center = (0,0); defaultSquare = Map.empty; squares = fun _ -> Success (Some Map.empty)}

    let mkBoardFun (boardProg : boardProg) : coord -> bool = ScrabbleLib.simpleBoardLangParser.parseSimpleBoardProg boardProg
