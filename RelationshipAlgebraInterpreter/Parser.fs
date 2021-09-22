module Parser

open FParsec
open RelationShip
open Ast

type Parser<'Result> = Parser<'Result, unit>

let betweenSpaces parser = between spaces spaces parser

let biOperator str (f: 'b * 'b -> 'b) : Parser<'b -> 'b -> 'b> =
    pstring str >>. preturn (fun x y -> f (x, y))

let anyOfString (stringList: string list) : Parser<string> =
    stringList
    |> List.map pstring
    |> choice

let anyOfStringPhrase (stringPhraseList: string list list) : Parser<string> =
    stringPhraseList
    |> List.map (fun stringPhrase ->
        let (parser, str) =
            stringPhrase
            |> List.fold (fun (parser, phrase) str -> (parser >>. spaces >>. pstring str, phrase + " " + str)) (pzero, "") 
        parser >>. preturn str
        )
    |> choice

let pString: Parser<AstLiteral> =

    let escapedCharSnippet =

        let escape =
            parse {
                let! c = anyOf "\"\\/bfnrt"
                return
                    match c with
                    | 'b' -> "\b"
                    | 'f' -> "\u000C"
                    | 'n' -> "\n"
                    | 'r' -> "\r"
                    | 't' -> "\t"
                    | c   -> string c
            }

        let unicodeEscape =
            /// converts a hex char ([0-9a-fA-F]) to its integer number (0-15)
            let hexToint c =
                (int c &&& 15) + (int c >>> 6) * 9

            parse {
                let! _ = pchar 'u'
                let! (((h3, h2), h1), h0) = hex .>>. hex .>>. hex .>>. hex
                return
                    (hexToint h3) * 4096 + (hexToint h2) * 256 + (hexToint h1) * 16 + hexToint h0
                    |> char
                    |> string
            }

        pchar '\\' >>. (escape <|> unicodeEscape)

    let normalCharSnippet =
        manySatisfy (fun c -> c <> '"' && c <> '\\')

    parse {
        let! str = between (pchar '\"') (pchar '\"') (stringsSepBy normalCharSnippet escapedCharSnippet)
        return AstLiteral.String str
    }

let pBool: Parser<AstLiteral> =
    anyOfString ["true"; "false"]
    |>> fun str -> AstLiteral.Bool (str = "true")

let pNumber: Parser<AstLiteral> =
    pfloat
    |>> AstLiteral.Number

let pNull: Parser<AstLiteral> =
    pstring "null"
    |>> fun _ -> AstLiteral.Null

let pLiteral: Parser<AstLiteral> =
    choice [pBool; pNumber; pNull; pString; pNull]

let pIdentity: Parser<string> =

    let isIdentifierFirstChar c =
        isLetter c || c = '_' || c = '#'

    let isIdentifierChar c =
        isLetter c || isDigit c || c = '_' || c = '#'

    many1Satisfy2 isIdentifierFirstChar isIdentifierChar
    <?> "identifier"

let pVar: Parser<string * int> =
    let pVarOffset =
        between (pchar '[') (pchar ']') pint32
        <|>
        preturn 0

    parse {
        let! identity = pIdentity
        let! offset = pVarOffset
        return (identity, offset)
    }

let pCondition, pConditionRef = createParserForwardedToRef<AstCondition, _>()

do pConditionRef :=

    let pConditionLiteral: Parser<AstCondition> =
        pLiteral |>> AstCondition.Literal
    
    let pConditionVar: Parser<AstCondition> =
        pVar |>> AstCondition.Var

    let pConditionT: Parser<AstCondition> =
            pConditionLiteral
        <|> between (pchar '(') (pchar ')') (betweenSpaces pCondition)
        <|> ((pstring "not" >>. spaces >>. pCondition) |>> AstCondition.Not)
        <|> pConditionVar
    
    let pConditionOrderBiOperator: Parser<AstCondition -> AstCondition -> AstCondition> =
        [
            "=", AstCondition.Equal
            "<>", AstCondition.NotEqual
            "<", AstCondition.LessThan
            "<=", AstCondition.LessEqualThan
            ">", AstCondition.GreaterThan
            ">=", AstCondition.GreaterEqualThan
            ]
        |> List.map (fun (x, y) -> biOperator x y)
        |> choice
    
    let pConditionOrder: Parser<AstCondition> =
        chainl1 (pConditionT .>> spaces) (pConditionOrderBiOperator .>> spaces)
    
    let pConditionAnd: Parser<AstCondition> =
        chainl1 pConditionOrder (betweenSpaces (biOperator "and" AstCondition.And))
    
    let pConditionOr: Parser<AstCondition> =
        chainl1 pConditionAnd (betweenSpaces (biOperator "or" AstCondition.Or))

    pConditionOr

let pRAIType: Parser<RAIType> =
    choice [
        pstring "bool"   |>> fun _ -> TypeBool
        pstring "number" |>> fun _ -> TypeNumber
        pstring "string" |>> fun _ -> TypeString
    ]

let pAttribute: Parser<string * RAIType> =
    parse {
        let! identity = pIdentity
        let! _ = spaces
        let! raiType = pRAIType
        return identity, raiType
    }

let pAttributeList: Parser<(string * RAIType) list> =
    sepBy1 (pAttribute .>> spaces) (pchar ',' .>> spaces)

let pTuple: Parser<AstLiteral list> =
    parse {
        let! _ = pchar '('
        let! _ = spaces
        let! tuple = sepBy1 (pLiteral .>> spaces) (pchar ',' .>> spaces)
        let! _ = pchar ')'
        return tuple
    }

let pTupleList: Parser<AstLiteral list list> =
    sepBy1 pTuple (pchar ',' .>> spaces)

let pVarList: Parser<(string * int32) list> =
    sepBy1 pVar (pchar ',' .>> spaces)

let pExpr, pExprRef = createParserForwardedToRef<AstExpr, _>()

do pExprRef :=

    let pRelationship: Parser<AstExpr> =
        parse {
            let! _ = pstring "relationship"
            let! _ = spaces
            let! _ = pchar '('
            let! _ = spaces
            let! attributes = pAttributeList
            let! _ = spaces
            let! _ = pchar ')'
            return Relationship attributes
        }

    let pInsert: Parser<AstExpr> =
        parse {
            let! _ = pstring "insert"
            let! _ = spaces
            let! tupleList = pTupleList
            let! _ = spaces
            let! expr = pExpr
            return Insert (tupleList, expr)
        }

    let pSelect: Parser<AstExpr> =
        parse {
            let! _ = pstring "select"
            let! _ = spaces
            let! condition = pCondition
            let! _ = spaces
            let! _ = pstring "from"
            let! _ = spaces
            let! expr = pExpr
            return Select (condition, expr)
        }

    let pProject: Parser<AstExpr> =
        parse {
            let! _ = pstring "project"
            let! _ = spaces
            let! _ = pchar '('
            let! _ = spaces
            let! varList = pVarList
            let! _ = spaces
            let! _ = pchar ')'
            let! _ = spaces
            let! expr = pExpr
            return Project (varList, expr)
        }

    let pAdd: Parser<AstExpr> =
        
        let pAttributeListCanEmpty =
            parse {
                let! _ = pchar '('
                let! _ = spaces
                let! attributes = sepBy (pAttribute .>> spaces) (pchar ',' .>> spaces)
                let! _ = spaces
                let! _ = pchar ')'
                return attributes
            }

        parse {
            let! _ = pstring "add"
            let! _ = spaces
            let! leftAttributeList = pAttributeListCanEmpty
            let! _ = spaces
            let! rightAttributeList = pAttributeListCanEmpty
            let! _ = spaces
            let! expr = pExpr
            return Add (leftAttributeList, rightAttributeList, expr)
        }

    let pBracket: Parser<AstExpr> =
        between (pchar '(') (pchar ')') (betweenSpaces pExpr)

    let pOperator: Parser<AstExpr -> AstExpr -> AstExpr> =
        [
            "union", Union
            "except", Except
            "intersect", Intersect
            "cross join", CrossJoin
            "left outer join", LeftOuterJoin
            "right outer join", RightOuterJoin
            "natural join", NatrualJoin
            ]
        |> List.map (fun (x, y) -> biOperator x y)
        |> choice     

    let pExprT: Parser<AstExpr> =
        choice [
            pRelationship
            pInsert
            pSelect
            pProject
            pAdd
            pBracket
            pIdentity |>> Identity
        ]

    chainl1 (pExprT .>> spaces) (between spaces spaces1 pOperator)

let pStatment: Parser<AstStatement> =

    let pDelete =
        parse {
            let! _ = pstring "delete"
            let! _ = spaces
            let! identity = pIdentity
            return Delete identity
        }
    
    let pAssignment =
        parse {
            let! _ = pstring "set"
            let! _ = spaces
            let! identity = pIdentity
            let! _ = spaces
            let! _ = pstring "to"
            let! _ = spaces
            let! expr = pExpr
            return Assignment (identity, expr)
        }

    choice [
        pDelete
        pAssignment
        pExpr |>> Expr
    ]

let pProgram: Parser<AstStatement list> =
    many (betweenSpaces pStatment .>> ((betweenSpaces (pchar ';')) <|> preturn ' ')) .>> eof