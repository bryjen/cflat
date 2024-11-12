module CFlat.ExceptionBase

type IExceptionBase =
    abstract member FormatPretty: unit -> string
