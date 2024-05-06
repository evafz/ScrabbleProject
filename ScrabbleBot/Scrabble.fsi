namespace FunctionalBot

module Scrabble =
    open ScrabbleUtil
    open System.IO

    val startGame :
        boardProg ->
        (bool -> Dictionary.Dict) ->
        uint32 ->
        uint32 ->
        uint32 ->
        (uint32 * uint32) list ->
        Map<uint32, tile> ->
        uint32 option ->
        Stream ->
        (unit -> unit)
