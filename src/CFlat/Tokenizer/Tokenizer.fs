namespace CFlat.Tokenizer

open CFlat.Helpers.CharHelper
open CFlat.ExceptionBase
open CFlat.Tokenizer

[<AutoOpen>]
module private TokenizerHelpers =
    let tryGetCharIfMatch (charsToMatch: char array) (chars: char array) (index: int) =
        if index >= 0 && index < chars.Length then
            match chars[index] with
            | c when Array.contains c charsToMatch ->
                Some c
            | _ ->
                None
        else
            None
            
    let prefixCharToString (charOption: char option) (str: string) =
        match charOption with
        | Some char -> char.ToString() + str
        | None -> str
    
    let readUntil (chars: char array) (charPredicate: char -> bool) start =
        let mutable current = start
        while current < chars.Length && charPredicate chars[current] do
            current <- current + 1
        current
    
    let readAlphaNumeric (chars: char array) start =
        readUntil chars isAlphaNumeric start
        
    let readAlpha (chars: char array) start =
        readUntil chars (fun c -> isAlphaUpper c || isAlpha c) start
        
    let readNumber (chars: char array) start =
        readUntil chars (fun c -> isDigit c || c = '.') start
        
    let readAsciiPunctuation (chars: char array) start =
        readUntil chars isAsciiPunctuation start
        
        
    let tryGetTokenType identifier =
        match identifier with
        | "<" -> Some (TokenType.Symbol Lt)
        | ">" -> Some (TokenType.Symbol Gt)
        | "=" -> Some (TokenType.Symbol Eq)
        | "/" -> Some (TokenType.Symbol Slash)
        | "\\" -> Some (TokenType.Symbol Backslash)
        | "*" -> Some (TokenType.Symbol Asterisk)
        | "!" -> Some (TokenType.Symbol ExclamationMark)
        | "+" -> Some (TokenType.Symbol Plus)
        | "-" -> Some (TokenType.Symbol Minus)
        | "_" -> Some (TokenType.Symbol Underscore)
        | "|" -> Some (TokenType.Symbol VerticalBar)
        | "&" -> Some (TokenType.Symbol Ampersand)
        | "." -> Some (TokenType.Symbol Period)
        | "," -> Some (TokenType.Symbol Comma)
        | ":" -> Some (TokenType.Symbol Colon)
        | ";" -> Some (TokenType.Symbol Semicolon)
        | "(" -> Some (TokenType.Symbol Lparen)
        | ")" -> Some (TokenType.Symbol Rparen)
        | "{" -> Some (TokenType.Symbol Lbrace)
        | "}" -> Some (TokenType.Symbol Rbrace)
        | "[" -> Some (TokenType.Symbol Lbracket)
        | "]" -> Some (TokenType.Symbol Rbracket)
        | "'" -> Some (TokenType.Symbol SingleQuot)
        | "\"" -> Some (TokenType.Symbol DoubleQuot)
        
        | "using" -> Some (TokenType.Keyword Using)
        | "namespace" -> Some (TokenType.Keyword Namespace)
        | "struct" -> Some (TokenType.Keyword Struct)
        | "union" -> Some (TokenType.Keyword Union)
        | "class" -> Some (TokenType.Keyword Class)
        
        | "signed" -> Some (TokenType.Keyword Signed)
        | "unsigned" -> Some (TokenType.Keyword Unsigned)
        | "int" -> Some (TokenType.Keyword Int)
        | "short" -> Some (TokenType.Keyword Short)
        | "long" -> Some (TokenType.Keyword Long)
        | "byte" -> Some (TokenType.Keyword Byte)
        | "char" -> Some (TokenType.Keyword Char)
        | "float" -> Some (TokenType.Keyword Float)
        | "double" -> Some (TokenType.Keyword Double)
        | "string" -> Some (TokenType.Keyword String)
        | "unit" -> Some (TokenType.Keyword Unit)
        
        | "if" -> Some (TokenType.Keyword If)
        | "else" -> Some (TokenType.Keyword Else)
        | "return" -> Some (TokenType.Keyword Return)
        | "match" -> Some (TokenType.Keyword Match)
        | "with" -> Some (TokenType.Keyword With)
        
        | "public" -> Some (TokenType.Keyword Public)
        | "private" -> Some (TokenType.Keyword Private)
        | "static" -> Some (TokenType.Keyword Static)
        
        | "var" -> Some (TokenType.Keyword Var)
        | "const" -> Some (TokenType.Keyword Const)
        | "mut" -> Some (TokenType.Keyword Mut)
        
        | "new" -> Some (TokenType.Keyword New)
        
        | _ -> None
    

type Tokenizer(lines: string array) =
    
    let mutable currentLine = -1
    let mutable tokensList: Token List = []
    
    member this.GetTokens() =
        tokensList
        |> List.toArray
        |> Array.rev
    
    member this.CanRead() =
        currentLine <- currentLine + 1
        currentLine < lines.Length
        
    member this.ReadLine() =
        let line = lines[currentLine]
        let chars = line.ToCharArray()
        
        let mutable col = 0
        while col < line.Length do
            match chars[col] with
            | c when isAlpha c || isAlphaUpper c ->
                let start = col
                col <- readAlphaNumeric chars start
                let slice = chars[start .. col - 1]
                let asString = System.String(slice)
                
                let asToken = {
                    Literal = asString
                    TokenType = match tryGetTokenType asString with | Some tokenType -> tokenType | None -> Identifier
                    Line = currentLine
                    StartCol = start 
                }
                tokensList <-  asToken :: tokensList
                
            | c when isDigit c ->
                // Handle prefix cases
                let prefixOption = tryGetCharIfMatch [| '-' |] chars (col - 1)
                if prefixOption.IsSome then
                    tokensList <- tokensList.Tail
                
                // Read token
                let start = col
                col <- readNumber chars start
                let slice = chars[start .. col - 1]
                let asString = System.String(slice)
                
                let startAdjusted = if prefixOption.IsSome then start - 1 else start
                
                let asToken = {
                    Literal = prefixCharToString prefixOption asString
                    TokenType = TokenType.Literal Literal.Number
                    Line = currentLine
                    StartCol = startAdjusted 
                }
                tokensList <-  asToken :: tokensList
                
            | c when c = '\"' ->  // case for string literal token types
                // Handle prefix cases
                let prefixOption = tryGetCharIfMatch [| '$' |] chars (col - 1)
                
                let tokenType =
                    match prefixOption with
                    | Some char when char = '$' -> Literal.InterpolatedString
                    | _ -> Literal.String
                    
                if prefixOption.IsSome then
                    tokensList <- tokensList.Tail
                
                // Read token
                let start = col + 1
                col <- readUntil chars (fun c -> c <> '\"') start
                col <- col + 1  // skip the closing '"' char
                let slice = chars[start - 1 .. col - 1]
                let asString = System.String(slice)
                
                let startAdjustedForPrefix =
                    if prefixOption.IsSome
                    then start - 1
                    else start
                
                let asToken = {
                    Literal = prefixCharToString prefixOption asString
                    TokenType = TokenType.Literal tokenType
                    Line = currentLine
                    StartCol = startAdjustedForPrefix - 1
                }
                tokensList <-  asToken :: tokensList
                
            | c when isAsciiPunctuation c ->
                let asString = c.ToString()
                let asToken = {
                    Literal = asString
                    TokenType = match tryGetTokenType asString with | Some tokenType -> tokenType | None -> Identifier
                    Line = currentLine
                    StartCol = col
                }
                
                tokensList <-  asToken :: tokensList
                col <- col + 1
                
            | c when isSpaceOrTab c ->
                col <- col + 1
                
            | _ ->
                col <- col + 1
            
        if currentLine = lines.Length - 1 then
            let eofToken = { Literal = "\0"; TokenType = Eof; Line = currentLine; StartCol = line.Length }
            tokensList <-  eofToken :: tokensList
