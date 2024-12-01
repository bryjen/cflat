module CFlat.Tests.Tokenizer.TokenizerTests

open System
open System.IO
open System.Reflection
open System.Runtime.InteropServices.JavaScript
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
        { Literal = "0"; TokenType = TokenType.Literal Literal.Number; Line = 6; StartCol = 11 }
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



[<Test>]
let ``Tokenize 'SimpleVector.cf'`` () =
    let text = File.ReadAllText("samples/SimpleVector.cf")
    let tokenizer = Tokenizer(text.Split('\n'))
    
    let expectedTokens = [|
        { Literal = "using"; TokenType = TokenType.Keyword Using; Line = 0; StartCol = 0 }
        { Literal = "System"; TokenType = TokenType.Identifier; Line = 0; StartCol = 6 }
        { Literal = "."; TokenType = TokenType.Symbol Period; Line = 0; StartCol = 12 }
        { Literal = "Text"; TokenType = TokenType.Identifier; Line = 0; StartCol = 13 }
        { Literal = "."; TokenType = TokenType.Symbol Period; Line = 0; StartCol = 17 }
        { Literal = "Json"; TokenType = TokenType.Identifier; Line = 0; StartCol = 18 }
        { Literal = ";"; TokenType = TokenType.Symbol Semicolon; Line = 0; StartCol = 22 }
        
        { Literal = "namespace"; TokenType = TokenType.Keyword Namespace; Line = 2; StartCol = 0 }
        { Literal = "CFlat"; TokenType = TokenType.Identifier; Line = 2; StartCol = 10 }
        { Literal = ";"; TokenType = TokenType.Symbol Semicolon; Line = 2; StartCol = 15 }
        
        { Literal = "public"; TokenType = TokenType.Keyword Public; Line = 4; StartCol = 0 }
        { Literal = "class"; TokenType = TokenType.Keyword Class; Line = 4; StartCol = 7 }
        { Literal = "Vector"; TokenType = TokenType.Identifier; Line = 4; StartCol = 13 }
        { Literal = "{"; TokenType = TokenType.Symbol Lbrace; Line = 4; StartCol = 20 }
        
        { Literal = "public"; TokenType = TokenType.Keyword Public; Line = 5; StartCol = 4 }
        { Literal = "float"; TokenType = TokenType.Keyword Float; Line = 5; StartCol = 11 }
        { Literal = "X"; TokenType = TokenType.Identifier; Line = 5; StartCol = 17 }
        { Literal = ";"; TokenType = TokenType.Symbol Semicolon; Line = 5; StartCol = 18 }
        
        { Literal = "public"; TokenType = TokenType.Keyword Public; Line = 6; StartCol = 4 }
        { Literal = "float"; TokenType = TokenType.Keyword Float; Line = 6; StartCol = 11 }
        { Literal = "Y"; TokenType = TokenType.Identifier; Line = 6; StartCol = 17 }
        { Literal = ";"; TokenType = TokenType.Symbol Semicolon; Line = 6; StartCol = 18 }
        
        { Literal = "public"; TokenType = TokenType.Keyword Public; Line = 7; StartCol = 4 }
        { Literal = "float"; TokenType = TokenType.Keyword Float; Line = 7; StartCol = 11 }
        { Literal = "Z"; TokenType = TokenType.Identifier; Line = 7; StartCol = 17 }
        { Literal = ";"; TokenType = TokenType.Symbol Semicolon; Line = 7; StartCol = 18 }
        
        { Literal = "}"; TokenType = TokenType.Symbol Rbrace; Line = 8; StartCol = 0 }
        
        { Literal = "public"; TokenType = TokenType.Keyword Public; Line = 10; StartCol = 0 }
        { Literal = "static"; TokenType = TokenType.Keyword Static; Line = 10; StartCol = 7 }
        { Literal = "int"; TokenType = TokenType.Keyword Int; Line = 10; StartCol = 14 }
        { Literal = "Main"; TokenType = TokenType.Identifier; Line = 10; StartCol = 18 }
        { Literal = "("; TokenType = TokenType.Symbol Lparen; Line = 10; StartCol = 22 }
        { Literal = "string"; TokenType = TokenType.Keyword String; Line = 10; StartCol = 23 }
        { Literal = "["; TokenType = TokenType.Symbol Lbracket; Line = 10; StartCol = 29 }
        { Literal = "]"; TokenType = TokenType.Symbol Rbracket; Line = 10; StartCol = 30 }
        { Literal = "args"; TokenType = TokenType.Identifier; Line = 10; StartCol = 32 }
        { Literal = ")"; TokenType = TokenType.Symbol Rparen; Line = 10; StartCol = 36 }
        { Literal = "{"; TokenType = TokenType.Symbol Lbrace; Line = 10; StartCol = 38 }
        
        { Literal = "var"; TokenType = TokenType.Keyword Var; Line = 11; StartCol = 4 }
        { Literal = "vector"; TokenType = TokenType.Identifier; Line = 11; StartCol = 8 }
        { Literal = "="; TokenType = TokenType.Symbol Eq; Line = 11; StartCol = 15 }
        { Literal = "new"; TokenType = TokenType.Keyword New; Line = 11; StartCol = 17 }
        { Literal = "Vector"; TokenType = TokenType.Identifier; Line = 11; StartCol = 21 }
        { Literal = "{"; TokenType = TokenType.Symbol Lbrace; Line = 11; StartCol = 28 }
        
        { Literal = "X"; TokenType = TokenType.Identifier; Line = 12; StartCol = 8 }
        { Literal = "="; TokenType = TokenType.Symbol Eq; Line = 12; StartCol = 10 }
        { Literal = "11.1"; TokenType = TokenType.Literal Number; Line = 12; StartCol = 12 }
        { Literal = ","; TokenType = TokenType.Symbol Comma; Line = 12; StartCol = 16 }
        
        { Literal = "Y"; TokenType = TokenType.Identifier; Line = 13; StartCol = 8 }
        { Literal = "="; TokenType = TokenType.Symbol Eq; Line = 13; StartCol = 10 }
        { Literal = "0"; TokenType = TokenType.Literal Number; Line = 13; StartCol = 12 }
        { Literal = ","; TokenType = TokenType.Symbol Comma; Line = 13; StartCol = 13 }
        
        { Literal = "Z"; TokenType = TokenType.Identifier; Line = 14; StartCol = 8 }
        { Literal = "="; TokenType = TokenType.Symbol Eq; Line = 14; StartCol = 10 }
        { Literal = "-12.6"; TokenType = TokenType.Literal Number; Line = 14; StartCol = 12 }
        { Literal = ","; TokenType = TokenType.Symbol Comma; Line = 14; StartCol = 17 }
        
        { Literal = "}"; TokenType = TokenType.Symbol Rbrace; Line = 15; StartCol = 4 }
        { Literal = ";"; TokenType = TokenType.Symbol Semicolon; Line = 15; StartCol = 5 }
        
        { Literal = "Console"; TokenType = TokenType.Identifier; Line = 16; StartCol = 4 }
        { Literal = "."; TokenType = TokenType.Symbol Period; Line = 16; StartCol = 11 }
        { Literal = "WriteLine"; TokenType = TokenType.Identifier; Line = 16; StartCol = 12 }
        { Literal = "("; TokenType = TokenType.Symbol Lparen; Line = 16; StartCol = 21 }
        { Literal = "$\"{vector.X}\""; TokenType = TokenType.Literal InterpolatedString; Line = 16; StartCol = 22 }
        { Literal = ")"; TokenType = TokenType.Symbol Rparen; Line = 16; StartCol = 35 }
        { Literal = ";"; TokenType = TokenType.Symbol Semicolon; Line = 16; StartCol = 36 }
        
        { Literal = "return"; TokenType = TokenType.Keyword Return; Line = 17; StartCol = 4 }
        { Literal = "0"; TokenType = TokenType.Literal Number; Line = 17; StartCol = 11 }
        { Literal = ";"; TokenType = TokenType.Symbol Semicolon; Line = 17; StartCol = 12 }
        
        { Literal = "}"; TokenType = TokenType.Symbol Rbrace; Line = 18; StartCol = 0 }
        
        { Literal = "\0"; TokenType = TokenType.Eof; Line = 18; StartCol = 1 }
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
