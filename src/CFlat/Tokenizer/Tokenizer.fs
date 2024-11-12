namespace CFlat.Tokenizer

open CFlat.Helpers.CharHelper
open CFlat.ExceptionBase
open CFlat.Tokenizer

[<AutoOpen>]
module private TokenizerHelpers =
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
        readUntil chars isDigit start
        
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
        | "var" -> Some (TokenType.Keyword Var)
        | "const" -> Some (TokenType.Keyword Const)
        | "mut" -> Some (TokenType.Keyword Mut)
        
        | _ -> None
    

type Tokenizer(lines: string array) =
    
    let mutable currentLine = -1
    let mutable tokensList: Token List = []
    let mutable tokenizerExceptions: IExceptionBase List = []
    
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
            | c when isAlphaNumeric c ->
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
                
            | c when c = '\"' ->  // case for string literal token types
                let start = col + 1
                col <- readUntil chars (fun c -> c <> '\"') start
                col <- col + 1  // skip the closing '"' char
                let slice = chars[start - 1 .. col - 2]
                let asString = System.String(slice)
                
                let asToken = {
                    Literal = asString
                    TokenType = TokenType.Literal Literal.String
                    Line = currentLine
                    StartCol = start 
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
            let eofToken = { Literal = "\0"; TokenType = Eof; Line = currentLine; StartCol = line.Length + 1 }
            tokensList <-  eofToken :: tokensList
