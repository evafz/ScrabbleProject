module internal Parser
    open ScrabbleUtil

    val mkBoardFun : boardProg -> (coord -> bool)
