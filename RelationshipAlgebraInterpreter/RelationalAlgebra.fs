module RelationalAlgebra

open System
open Util
open RelationShip

/// 条件表达式 用于关系代数中的Selection操作
type ConditionExpr =
    | Value of obj
    | IndexVaribale of int
    | IsNull of ConditionExpr
    | Neg of ConditionExpr
    | And of a: ConditionExpr * b: ConditionExpr
    | Or of a: ConditionExpr * b: ConditionExpr
    | Equal of a: ConditionExpr * b: ConditionExpr
    | LessThan of a: ConditionExpr * b: ConditionExpr
    | GreaterThan of a: ConditionExpr * b: ConditionExpr

type RelationalAlgebraExpr =
    | Relation of Relationship
    | Distinct of e: RelationalAlgebraExpr
    | Union of a: RelationalAlgebraExpr * b: RelationalAlgebraExpr
    | Except of a: RelationalAlgebraExpr * b: RelationalAlgebraExpr
    | Insert of tupleList: Tuple list * e: RelationalAlgebraExpr
    | Addtion of leftAttributes: RAIAttribute list * rightAttributes: RAIAttribute list * e: RelationalAlgebraExpr
    | ProjectIndex of indexList: int list * e: RelationalAlgebraExpr
    | CrossJoin of a: RelationalAlgebraExpr * b: RelationalAlgebraExpr
    | Selection of predicate: ConditionExpr * e: RelationalAlgebraExpr


/// 在属性中找到名字为name的偏移offset的属性的索引，可能不存在
///
/// ("a", 0) 表示 在attributes中找到名为"a"的第一个属性 找得到返回Ok索引 找不到返回Error错误信息
/// ("a", 2) 表示 在attributes中找到名为"a"的第三个属性 找得到返回Ok索引 找不到返回Error错误信息
let tryFindAttributeIndex (attributes: RAIAttribute list) (name, offset) =

    let index =
        attributes
        |> List.indexed
        |> List.filter (fun (_, attribute) -> attribute.Name = name)
        |> List.map (fun (index, _) -> index)
        |> List.tryItem offset

    match index with
    | Some index -> Ok index
    | None -> Error $"unable to find attribute ({name}, {offset})"

let rec invokeCondition (relation: Relationship) (condition: ConditionExpr) (tuple: Tuple) : Result<obj, string> =

    let getObjectType (obj: obj) =
        if obj = null then "null" else obj.GetType().Name

    result {
        match condition with
        | Value v -> return v

        | IndexVaribale index ->
            return!
                tuple
                |> List.tryItem index
                |> optionToResult $"找不到第{index}个字段"

        | IsNull e ->
            let! r = invokeCondition relation e tuple
            return isNull r :> obj

        | Neg e ->
            let! r = invokeCondition relation e tuple
            match r with
            | :? bool as b -> return not b :> obj
            | _ as o -> return! Error $"Neg expr should be Bool, but found {getObjectType o}"

        | And (a, b) ->
            let! a = invokeCondition relation a tuple
            let! b = invokeCondition relation b tuple
            match a, b with
            | (:? bool as b1), (:? bool as b2) -> return (b1 && b2) :> obj
            | _ -> return! Error $"And expr should be both Bool, but found {getObjectType a}, {getObjectType b}"

        | Or (a, b) ->
            let! a = invokeCondition relation a tuple
            let! b = invokeCondition relation b tuple
            match a, b with
            | (:? bool as b1), (:? bool as b2) -> return (b1 || b2) :> obj
            | _ -> return! Error $"And expr should be both Bool, but found {getObjectType a}, {getObjectType b}"

        | Equal (a, b) ->
            let! a = invokeCondition relation a tuple
            let! b = invokeCondition relation b tuple
            if a = null || b = null then
                return false :> obj
            else
                return a = b :> obj

        | LessThan (a, b) ->
            let! a = invokeCondition relation a tuple
            let! b = invokeCondition relation b tuple
            match a, b with
            | (:? double as d1), (:? double as d2) -> return (d1 < d2) :> obj
            | _ -> return! Error $"Less expr should be both Number, but found {getObjectType a}, {getObjectType b}"

        | GreaterThan (a, b) ->
            let! a = invokeCondition relation a tuple
            let! b = invokeCondition relation b tuple
            match a, b with
            | (:? double as d1), (:? double as d2) -> return (d1 > d2) :> obj
            | _ -> return! Error $"Greater expr should be both Number, but found {getObjectType a}, {getObjectType b}"
    }

let rec getAttributesFromAlgebra (expr: RelationalAlgebraExpr) : Result<RAIAttribute list, string> =
    result {
        match expr with
        | Relation r -> return r.Attributes
        | Distinct e
        | Union (e, _)
        | Except (e, _)
        | Insert (_, e)
        | Selection (_, e) ->
            return! getAttributesFromAlgebra e
        | Addtion (l, r, e) ->
            let! attributes = getAttributesFromAlgebra e
            return l ++ attributes ++ r
        | CrossJoin (a, b) ->
            let! attributes1 = getAttributesFromAlgebra a
            let! attributes2 = getAttributesFromAlgebra b
            return attributes1 ++ attributes2

        | ProjectIndex (indexList, e) ->
            let! attributes =  getAttributesFromAlgebra e

            return
                indexList
                |> List.map (fun index -> attributes.[index])
    }

let invokeAlgebra (expr: RelationalAlgebraExpr) : Result<Relationship, string> =

    let rec invokeAlgebra_inner (expr: RelationalAlgebraExpr) : Result<Relationship, string> =
    
        let checkAttributeEqual (a: Relationship) (b: Relationship) =
            if a.Attributes = b.Attributes then
                Result.Ok ()
            else
                Result.Error $"Property {a.Attributes} is not equal to property {b.Attributes}"
    
        result {
            match expr with
            | Relation r -> return r
    
            | Distinct e ->
                let! r = invokeAlgebra_inner e
                return { Attributes = r.Attributes; Tuples = List.distinct r.Tuples }
    
            | Union (a, b) ->
                let! a = invokeAlgebra_inner a
                let! b = invokeAlgebra_inner b
                do! checkAttributeEqual a b
                return { Attributes = a.Attributes; Tuples = a.Tuples ++ b.Tuples }
    
            | Except (a, b) ->
                let! a = invokeAlgebra_inner a
                let! b = invokeAlgebra_inner b
                do! checkAttributeEqual a b
                return { Attributes = a.Attributes; Tuples = List.except b.Tuples a.Tuples }
    
            | Insert (tupleList, e) ->
                let! r = invokeAlgebra_inner e
                return { Attributes = r.Attributes; Tuples = r.Tuples ++ tupleList }
    
            | Selection (predicate, e) ->
                let! r = invokeAlgebra_inner e
    
                let conditionResultToBool (res: obj) : bool =
                    match res with
                    | :? bool as b -> b
                    | _ -> false
    
                let! tuples =
                    r.Tuples
                    |> listTryFilter (invokeCondition r predicate >> Result.map conditionResultToBool)

                return {
                    Attributes = r.Attributes
                    Tuples = tuples
                }
    
            | Addtion (leftAttributes, rightAttributes, e) ->
                let! r = invokeAlgebra_inner e
    
                let leftTuple = List.init leftAttributes.Length (fun _ -> null)
                let rightTuple = List.init rightAttributes.Length (fun _ -> null)
    
                return {
                    Attributes = leftAttributes ++ r.Attributes ++ rightAttributes
                    Tuples =
                        r.Tuples
                        |> List.map (fun tuple -> leftTuple ++ tuple ++ rightTuple)
                }

            | ProjectIndex (indexList, e) ->
                let! r = invokeAlgebra_inner e
    
                return {
                    Attributes =
                        indexList
                        |> List.map (fun index -> r.Attributes.[index])
                    Tuples =
                        r.Tuples
                        |> List.map (fun tuple -> [ for index in indexList do tuple.[index] ])
                }
    
            | CrossJoin (a, b) ->
                let! a = invokeAlgebra_inner a
                let! b = invokeAlgebra_inner b
                
                return {
                    Attributes = a.Attributes ++ b.Attributes
                    Tuples =
                        a.Tuples
                        |> List.collect (fun t1 -> List.map (fun t2 -> t1 ++ t2) b.Tuples)
                }
        }

    /// 输入的元组不全为null才返回true
    let notAllNull =
        List.exists ((<>) null)

    /// 去除重复行和null行
    result {
        let! r = invokeAlgebra_inner expr
        return {
            Attributes= r.Attributes
            Tuples = List.distinct (List.filter notAllNull r.Tuples)
        }
    }