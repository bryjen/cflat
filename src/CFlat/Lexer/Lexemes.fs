module CFlat.Lexer.Lexemes

type LexemeType =
    | Eof
    | Symbol of Symbol
    | Literal of CFlat.Tokenizer.Literal
    | Operator of Operator
    | Identifier of Identifier
    | Keyword of Keyword

and Symbol =
    | Backslash 
    | ExclamationMark
    | Underscore 
    
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

and Operator =
    | Lt
    | Gt
    | Eq
    | Slash
    | Asterisk
    | Plus
    | Minus
    | LogicalAnd
    | LogicalOr
    
    
and Identifier =
    | Namespace         // using System.Text.Json
    | Type              // *Console*.WriteLine()
    | Method            // Console.*WriteLine*()  OR  public int *Main*
    | Variable          // var something
    | Parameter         // public static int Main(string[] *args*)
    | Property          // someJsonSerializerOptions.*WriteIndented*
    | MemberVariable    // public bool *WriteIndented*
    
    
and Keyword =
    | Using
    | TypeDec of TypeDec
    | Namespace
    | ControlFlow of ControlFlow
    | VariableDec of VariableDec
    | AccessModifier of AccessModifier
    | PrimitiveType of PrimitiveType
    
and TypeDec =
    | Var
    | Const
    | Mut
    
and ControlFlow = 
    | If
    | Else
    | Return
    | Match
    | With
    
and VariableDec = 
    | Var
    | Const
    | Mut
    
and AccessModifier =  
    | Public
    | Private
    
and PrimitiveType =
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
    
type Lexeme =
    { Literal: string
      LexemeType: LexemeType
      Line: int
      StartCol: int }