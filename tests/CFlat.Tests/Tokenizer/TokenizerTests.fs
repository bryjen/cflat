module CFlat.Tests.Tokenizer.TokenizerTests

open System
open System.IO
open System.Reflection
open Microsoft.FSharp.Core
open NUnit.Framework
open CFlat.Tokenizer

let recordToStr (record: 'T) : string =
    let recordType = record.GetType()
    let properties = recordType.GetProperties(BindingFlags.Public ||| BindingFlags.Instance)
    properties |> Array.map (fun prop ->
        let name = prop.Name
        let value = prop.GetValue(record)
        $"%s{name}: %A{value}"
    )
    |> String.concat "\n"

let assertRecordsEqual (expected: 'T) (actual: 'T) =
    if expected <> actual then
        let differences =
            typeof<'T>.GetProperties()
            |> Seq.choose (fun prop ->
                let expectedValue = prop.GetValue(expected)
                let actualValue = prop.GetValue(actual)
                if expectedValue <> actualValue then
                    Some $"Property '%s{prop.Name}' differs: Expected = %A{expectedValue}, Actual = %A{actualValue}"
                else None
            )
            |> Seq.toList
        if differences.Length > 0 then
            let message = "Records do not match:\n"
                          + (String.concat "\n" differences)
                          + $"\n\nExpected:\n{recordToStr expected}"
                          + $"\n\nActual:\n{recordToStr actual}"
            Assert.Fail(message)
    else
        ()

[<SetUp>]
let Setup () =
    ()

[<Test>]
let ``Tokenize 'HelloWorld.cf'`` () =
    let text = File.ReadAllText("samples/HelloWorld.cf")
    let tokenizer = Tokenizer(text.Split('\n'))
    
    let expectedTokens = [|
        { Literal = "using"; TokenType = TokenType.Keyword Using; Line = 0; StartCol = 0 }
        { Literal = "System"; TokenType = TokenType.Identifier; Line = 0; StartCol = 6 }
        { Literal = ";"; TokenType = TokenType.Symbol Semicolon; Line = 0; StartCol = 12 }
        
        { Literal = "namespace"; TokenType = TokenType.Keyword Namespace; Line = 2; StartCol = 0 }
        { Literal = "Program"; TokenType = TokenType.Identifier; Line = 2; StartCol = 10 }
        { Literal = ";"; TokenType = TokenType.Symbol Semicolon; Line = 2; StartCol = 17 }
        
        { Literal = "public"; TokenType = TokenType.Keyword Public; Line = 4; StartCol = 0 }
        { Literal = "int"; TokenType = TokenType.Keyword Int; Line = 4; StartCol = 7 }
        { Literal = "Main"; TokenType = TokenType.Identifier; Line = 4; StartCol = 11 }
        { Literal = "("; TokenType = TokenType.Symbol Lparen; Line = 4; StartCol = 15 }
        { Literal = "string"; TokenType = TokenType.Keyword Keyword.String; Line = 4; StartCol = 16 }
        { Literal = "["; TokenType = TokenType.Symbol Lbracket; Line = 4; StartCol = 22 }
        { Literal = "]"; TokenType = TokenType.Symbol Rbracket; Line = 4; StartCol = 23 }
        { Literal = "args"; TokenType = TokenType.Identifier; Line = 4; StartCol = 25 }
        { Literal = ")"; TokenType = TokenType.Symbol Rparen; Line = 4; StartCol = 29 }
        { Literal = "{"; TokenType = TokenType.Symbol Lbrace; Line = 4; StartCol = 31 }
        
        { Literal = "Console"; TokenType = TokenType.Identifier; Line = 5; StartCol = 4 }
        { Literal = "."; TokenType = TokenType.Symbol Period; Line = 5; StartCol = 11 }
        { Literal = "WriteLine"; TokenType = TokenType.Identifier; Line = 5; StartCol = 12 }
        { Literal = "("; TokenType = TokenType.Symbol Lparen; Line = 5; StartCol = 21 }
        { Literal = "\"Hello, World!\""; TokenType = TokenType.Literal Literal.String; Line = 5; StartCol = 22 }
        { Literal = ")"; TokenType = TokenType.Symbol Rparen; Line = 5; StartCol = 37 }
        { Literal = ";"; TokenType = TokenType.Symbol Semicolon; Line = 5; StartCol = 38 }
        
        { Literal = "return"; TokenType = TokenType.Keyword Return; Line = 6; StartCol = 4 }
        { Literal = "0"; TokenType = TokenType.Literal Literal.Int; Line = 6; StartCol = 11 }
        { Literal = ";"; TokenType = TokenType.Symbol Semicolon; Line = 6; StartCol = 12 }
        
        { Literal = "}"; TokenType = TokenType.Symbol Rbrace; Line = 7; StartCol = 0 }
        
        { Literal = "\0"; TokenType = TokenType.Eof; Line = 7; StartCol = 1 }
    |]
    
    while tokenizer.CanRead() do
        tokenizer.ReadLine()
        
    let tokens = tokenizer.GetTokens()
    if expectedTokens.Length <> tokens.Length then
        Assert.Fail($"Expected {expectedTokens.Length}, got {tokens.Length}")
        
    for i = 0 to tokens.Length - 1 do
        let actualToken = tokens[i]
        let expectedToken = expectedTokens[i]
        
        assertRecordsEqual expectedToken actualToken
        
    Assert.Pass()
