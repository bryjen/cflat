module CFlat.Lexer.Lexer

open CFlat.Lexer.Lexemes
open CFlat.Tokenizer

[<AutoOpen>]
module private LexerHelpers =
    let peek (tokens: Token array) (startIndex: int) (count: int) =
        match (startIndex + count) with
        | i when i >= 0 && i < tokens.Length -> Some tokens[i]
        | _ -> None

type Lexer(tokens: Token array) =
    let mutable currentTokenIndex = -1
    let mutable parenDepth = 0
    let mutable lexemeList: Lexeme List = []
    
    
    let peek count = LexerHelpers.peek tokens currentTokenIndex count
    
    
    member this.GetTokens() =
        lexemeList
        |> List.toArray
        |> Array.rev

    member this.CanRead() =
        currentTokenIndex <- currentTokenIndex + 1
        currentTokenIndex < tokens.Length
        
    member this.Lex() =
        while currentTokenIndex < tokens.Length do
            let currentToken = tokens[currentTokenIndex]
            match currentToken.TokenType with
            | Eof ->
                let asLexeme = {
                    Literal = currentToken.Literal
                    LexemeType = LexemeType.Eof
                    Line = currentToken.Line
                    StartCol =  currentToken.StartCol
                }
                lexemeList <- asLexeme :: lexemeList
                currentTokenIndex <- currentTokenIndex + 1
            | Identifier ->
                failwith "TODO"
            | Symbol symbol ->
                failwith "TODO"
            | Literal literal ->
                failwith "TODO"
            | Keyword keyword ->
                failwith "TODO"
                
    member private this.HandleIdentifier() =
        let currentToken = tokens[currentTokenIndex]
        let previousTokenOption = peek -1
        let nextTokenOption = peek 1
        
        match (previousTokenOption, nextTokenOption) with
        | None, None ->
            ()  // do nothin
        | None, Some nextToken ->
            // Start of file is identifier, invalid
            // TODO Throw exception
            failwith "todo"
        | Some previousToken, None ->
            // End of file is identifier, invalid
            // TODO Throw exception
            failwith "todo"
        | Some previousToken, Some nextToken ->
            
            match previousToken.TokenType with
            | Keyword keyword when keyword = Keyword.Namespace || keyword = Keyword.Using ->
                failwith "todo"
            | Symbol symbol when symbol = Symbol.Period ->
                failwith "todo"
            | Symbol symbol when symbol = Symbol.Period ->
                failwith "todo"
            
            failwith "todo"
        
        failwith "todo"
        
    member private this.HandleSymbol() =
        failwith "todo"
        
    member private this.HandleLiteral() =
        failwith "todo"
        
    member private this.HandleKeyword() =
        failwith "todo"
