module CFlat.Tests.Lexer.LexerTests

open CFlat.Tokenizer
open NUnit.Framework
open CFlat.Lexer.Lexemes
open CFlat.Lexer.Lexer

[<SetUp>]
let Setup () =
    ()
    
    
[<Test>]
[<Description("Assumes that 'Tokenizer' works. ")>]
let ``Lex 'HelloWorld.cf'`` () =
    
    let expectedLexemes = [|
        { Literal = "using"; LexemeType = LexemeType.Keyword Using; Line = 0; StartCol = 0 }
        { Literal = "System"; LexemeType = LexemeType.Identifier Identifier.Namespace; Line = 0; StartCol = 6 }
        { Literal = ";"; LexemeType = LexemeType.Symbol Semicolon; Line = 0; StartCol = 12 }
        
        { Literal = "namespace"; LexemeType = LexemeType.Keyword Keyword.Namespace; Line = 2; StartCol = 0 }
        { Literal = "Program"; LexemeType = LexemeType.Identifier Identifier.Namespace; Line = 2; StartCol = 10 }
        { Literal = ";"; LexemeType = LexemeType.Symbol Semicolon; Line = 2; StartCol = 17 }
        
        { Literal = "public"; LexemeType = LexemeType.Keyword (AccessModifier Public); Line = 4; StartCol = 0 }
        { Literal = "int"; LexemeType = LexemeType.Keyword (PrimitiveType Int); Line = 4; StartCol = 7 }
        { Literal = "Main"; LexemeType = LexemeType.Identifier Method; Line = 4; StartCol = 11 }
        { Literal = "("; LexemeType = LexemeType.Symbol Lparen; Line = 4; StartCol = 15 }
        { Literal = "string"; LexemeType = LexemeType.Keyword (PrimitiveType String); Line = 4; StartCol = 16 }
        { Literal = "["; LexemeType = LexemeType.Symbol Lbracket; Line = 4; StartCol = 22 }
        { Literal = "]"; LexemeType = LexemeType.Symbol Rbracket; Line = 4; StartCol = 23 }
        { Literal = "args"; LexemeType = LexemeType.Identifier Parameter; Line = 4; StartCol = 25 }
        { Literal = ")"; LexemeType = LexemeType.Symbol Rparen; Line = 4; StartCol = 29 }
        { Literal = "{"; LexemeType = LexemeType.Symbol Lbrace; Line = 4; StartCol = 31 }
        
        { Literal = "Console"; LexemeType = LexemeType.Identifier Type; Line = 5; StartCol = 4 }
        { Literal = "."; LexemeType = LexemeType.Symbol Period; Line = 5; StartCol = 11 }
        { Literal = "WriteLine"; LexemeType = LexemeType.Identifier Method; Line = 5; StartCol = 12 }
        { Literal = "("; LexemeType = LexemeType.Symbol Lparen; Line = 5; StartCol = 21 }
        { Literal = "\"Hello, World!\""; LexemeType = LexemeType.Literal Literal.String; Line = 5; StartCol = 22 }
        { Literal = ")"; LexemeType = LexemeType.Symbol Rparen; Line = 5; StartCol = 37 }
        { Literal = ";"; LexemeType = LexemeType.Symbol Semicolon; Line = 5; StartCol = 38 }
        
        { Literal = "return"; LexemeType = LexemeType.Keyword (ControlFlow Return); Line = 6; StartCol = 4 }
        { Literal = "0"; LexemeType = LexemeType.Literal Number; Line = 6; StartCol = 11 }
        { Literal = ";"; LexemeType = LexemeType.Symbol Semicolon; Line = 6; StartCol = 12 }
        
        { Literal = "}"; LexemeType = LexemeType.Symbol Rbrace; Line = 7; StartCol = 0 }
        
        { Literal = "\0"; LexemeType = LexemeType.Eof; Line = 7; StartCol = 1 }
    |]
    failwith "TODO"