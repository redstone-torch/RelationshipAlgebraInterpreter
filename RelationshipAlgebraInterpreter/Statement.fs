module Statement

open Util
open RelationShip
open RelationalAlgebra

type RuntimeContext = Map<string, Relationship>

type RAIStatement =
    | Assignment of id: string * RelationalAlgebraExpr
    | Delete of id: string
    | Expr of RelationalAlgebraExpr

let invokeStatement (statement: RAIStatement) (context: RuntimeContext) : Result<RuntimeContext * string, string> =
    result {
        match statement with
        | Assignment (id, expr) ->
            let! r = invokeAlgebra expr
            return (context |> Map.add id r), $"assignment `{id}` successfully"
            
        | Delete id ->
            if context |> Map.containsKey id then
                return (context |> Map.remove id), $"delete `{id}` successfully"
            else
                return! Error $"cannot find `{id}`"

        | Expr expr ->
            let! r = invokeAlgebra expr
            return context, relationshipToString r
    }
    