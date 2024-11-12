(*
Heavily inspired by Markdig, see
https://github.com/xoofx/markdig/blob/master/src/Markdig/Helpers/CharHelper.cs
*)

module CFlat.Helpers.CharHelper

let inline isNewLineOrLineFeed (c: char) =
    c = '\n' || c = '\r'

let inline isSpace (c: char) =
    c = ' '

let inline isZero (c: char) =
    c = '\u0000'

let inline isTab (c: char) =
    c = '\t'
    
let inline isSpaceOrTab (c: char) =
    (isSpace c) || (isTab c)
    
    
let inline isAlphaUpper (c: char) =
    let dec = int (c - 'A')
    dec >= 0 && dec <= int ('Z' - 'A')

let inline isAlpha (c: char) =
    let dec = int (c - 'a')
    dec >= 0 && dec <= int ('z' - 'a')
    
let inline isDigit (c: char) =
    let dec = int (c - '0')
    dec >= 0 && dec <= int ('9' - '0')
    
let inline isAlphaNumeric (c: char) =
    isAlpha c || isAlphaUpper c || isDigit c

let rec isAsciiPunctuation (c: char) =
    // 2.1 Characters and lines 
    // An ASCII punctuation character is
    // !, ", #, $, %, &, ', (, ), *, +, ,, -, ., / (U+0021–2F),
    // :, ;, <, =, >, ?, @ (U+003A–0040),
    // [, \, ], ^, _, ` (U+005B–0060),
    // {, |, }, or ~ (U+007B–007E).
    let dec = int c
    dec <= 127 &&
        (isInInclusiveRange dec 33 47 ||
         isInInclusiveRange dec 58 64 ||
         isInInclusiveRange dec 91 96 ||
         isInInclusiveRange dec 123 126)
    
and isInInclusiveRange c min max =
    c >= min && c <= max