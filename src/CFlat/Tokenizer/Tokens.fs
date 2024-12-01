namespace CFlat.Tokenizer

type TokenType =
    | Eof
    | Identifier
    | Symbol of Symbol 
    | Literal of Literal
    | Keyword of Keyword
    
and Literal =
    | String
    | InterpolatedString 
    | Number 
    
and Symbol =
    | Lt
    | Gt
    | Eq
    | Slash
    | Backslash 
    | Asterisk
    | ExclamationMark
    | Plus
    | Minus
    | Underscore 
    | VerticalBar 
    | Ampersand
    
    | Period
    | Comma
    | Colon
    | Semicolon
    | Lparen
    | Rparen
    | Lbrace
    | Rbrace
    | Lbracket 
    | Rbracket
    
    | SingleQuot 
    | DoubleQuot 
    
and Keyword =
    | Using
    | Namespace
    | Class
    | Struct
    | Union
    
    | Signed
    | Unsigned
    | Int
    | Short
    | Long
    | Byte
    | Char
    | Float
    | Double
    | String
    | Unit
    
    | If
    | Else
    | Return
    | Match
    | With
    
    | Public
    | Private
    | Static
    
    | Var
    | Const
    | Mut
    
    | New
    
    
type Token =
    { Literal: string
      TokenType: TokenType
      Line: int
      StartCol: int }