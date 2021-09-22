module Ast

open RelationShip

/// 字面量
type AstLiteral =
    | Bool of bool
    | Number of float
    | String of string
    | Null

/// 实际值
type AstCondition =
    | Literal of AstLiteral
    | Var of identity: string * offset: int
    | Not of AstCondition
    | And of left: AstCondition * right: AstCondition
    | Or of left: AstCondition * right: AstCondition
    | Equal of left: AstCondition * right: AstCondition
    | NotEqual of left: AstCondition * right: AstCondition
    | LessThan of left: AstCondition * right: AstCondition
    | GreaterThan of left: AstCondition * right: AstCondition
    | LessEqualThan of left: AstCondition * right: AstCondition
    | GreaterEqualThan of left: AstCondition * right: AstCondition

/// 关系表达式
type AstExpr =
    | Identity of string
    | Relationship of (string * RAIType) list
    | Insert of AstLiteral list list * AstExpr
    | Select of AstCondition * AstExpr
    | Project of (string * int) list * AstExpr
    | Add of leftAttributes: (string * RAIType) list * rightAttributes: (string * RAIType) list * AstExpr
    | Union of left: AstExpr * right: AstExpr
    | Except of left: AstExpr * right: AstExpr
    | Intersect of left: AstExpr * right: AstExpr
    | CrossJoin of left: AstExpr * right: AstExpr
    | LeftOuterJoin of left: AstExpr * right: AstExpr
    | RightOuterJoin of left: AstExpr * right: AstExpr
    | NatrualJoin of left: AstExpr * right: AstExpr

/// 语句
type AstStatement =
    | Delete of string
    | Assignment of string * AstExpr
    | Expr of AstExpr