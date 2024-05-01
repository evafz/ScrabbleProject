module internal Parser
    open ScrabbleLib
    
    let mkBoardFun boardProg = simpleBoardLangParser.parseSimpleBoardProg boardProg
