module CFlat.Tokenizer.TokenizerExceptions

open CFlat.ExceptionBase
open CFlat.Tokenizer

type UnrecognizedTokenException(token: Token) =
    
    interface IExceptionBase with
        member this.FormatPretty() =
            "todo"