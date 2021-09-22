module Generator

open Util
open RelationShip
open RelationalAlgebra
open Statement
open Ast


let private tryGet key (context: RuntimeContext) =
    context
    |> Map.tryFind key
    |> optionToResult $"table `{key}` not found"

let literalToObj (literal: AstLiteral) =
    match literal with
    | Null -> null
    | String v -> v :> obj
    | Bool v -> v :> obj
    | Number v -> v :> obj

let rec generateCondition (context: RuntimeContext) (algebra: RelationalAlgebraExpr) (ast: AstCondition) =
    result {
        match ast with
        | Literal literal -> return ConditionExpr.Value (literalToObj literal)
        | Var (identity, offset) ->
            let! attributes = getAttributesFromAlgebra algebra
            let! index = tryFindAttributeIndex attributes (identity, offset)
            return ConditionExpr.IndexVaribale index
        | Not astExpr ->
            let! condition = generateCondition context algebra astExpr
            return ConditionExpr.Neg condition
        | And (l, r) ->
            let! l = generateCondition context algebra l
            let! r = generateCondition context algebra r
            return ConditionExpr.And (l, r)
        | Or (l, r) ->
            let! l = generateCondition context algebra l
            let! r = generateCondition context algebra r
            return ConditionExpr.Or (l, r)
        | Equal (l, r) ->
            let! l = generateCondition context algebra l
            match r with
            | Literal (Null) ->
                return ConditionExpr.IsNull l
            | _ ->
                let! r = generateCondition context algebra r
                return ConditionExpr.Equal (l, r)
            
        | NotEqual (l, r) ->
            let! l = generateCondition context algebra l
            match r with
            | Literal (Null) ->
                return ConditionExpr.Neg (ConditionExpr.IsNull l)
            | _ ->
                let! r = generateCondition context algebra r
                return ConditionExpr.Neg (ConditionExpr.Equal (l, r))
            
        | LessThan (l, r) ->
            let! l = generateCondition context algebra l
            let! r = generateCondition context algebra r
            return ConditionExpr.LessThan (l, r)
        | GreaterThan (l, r) ->
            let! l = generateCondition context algebra l
            let! r = generateCondition context algebra r
            return ConditionExpr.GreaterThan (l, r)
        | LessEqualThan (l, r) ->
            let! l = generateCondition context algebra l
            let! r = generateCondition context algebra r
            return ConditionExpr.Or (ConditionExpr.Equal (l, r), ConditionExpr.LessThan (l, r))
        | GreaterEqualThan (l, r) ->
            let! l = generateCondition context algebra l
            let! r = generateCondition context algebra r
            return ConditionExpr.Or (ConditionExpr.Equal (l, r), ConditionExpr.GreaterThan (l, r))
    }

/// 找到两个属性集的相等属性组索引
let findEqualSetOfPropertiesIndex (list1: RAIAttribute list) (list2: RAIAttribute list) =

    let rec findIndex (indexedList1: (int * RAIAttribute) list) (indexedList2: (int * RAIAttribute) list) (newList: (int * int) list) =
        match indexedList1 with
        | (index1, attr1) :: xs ->
            match List.tryFind (fun (_, attr) -> attr = attr1) indexedList2 with
            | Some (index2, _) ->
                let cuttedList2 = List.filter (fun (index, _) -> index <> index2) indexedList2
                findIndex xs cuttedList2 (newList ++ [index1, index2])
            | None ->
                findIndex xs indexedList2 newList
        | [] -> newList

    
    findIndex (List.indexed list1) (List.indexed list2) []

let checkTuples (tuples: AstLiteral list list) (attributes: RAIAttribute list) =
    let checkTuple (tuple: AstLiteral list) (attributes: RAIAttribute list) =
        if tuple.Length <> attributes.Length then
            Error "The length of the inserted tuple should be equal to the property"
        elif
            tuple
            |> Seq.indexed
            |> Seq.exists(fun (index, literal) ->
                let attribute = attributes.[index]
                match literal with
                | Null -> false
                | String _ -> attribute.Type <> TypeString
                | Number _ -> attribute.Type <> TypeNumber
                | Bool _ -> attribute.Type <> TypeBool
            )
        then Error $"insert tuple {tuple} doesn't match the attributes {attributes}"
        else Ok ()

    listFoldResult (fun _ e -> checkTuple e attributes) () tuples

let rec generateAlgebra (context: RuntimeContext) (ast: AstExpr) =

    let toRAIAttribute (a: string * RAIType) =
        let name, tinyType = a
        { Name = name; Type = tinyType }

    result {
        match ast with

        | Identity tableId ->
            let! relationship = (tryGet tableId context)
            return RelationalAlgebraExpr.Relation relationship

        | Relationship attributes ->
            let attributes = List.map toRAIAttribute attributes
            return RelationalAlgebraExpr.Relation { Attributes = attributes; Tuples = [] }

        | Insert (tupleList, astExpr) ->
            let! algebra = generateAlgebra context astExpr
            let! attributes = getAttributesFromAlgebra algebra
            do! checkTuples tupleList attributes
            return RelationalAlgebraExpr.Insert (List.map (List.map literalToObj) tupleList, algebra)

        | Select (astCondition, astExpr) ->
            let! algebra = generateAlgebra context astExpr
            let! condition = generateCondition context algebra astCondition
            return RelationalAlgebraExpr.Selection (condition, algebra)

        | Project (identityList, astExpr) ->
            let! algebra = generateAlgebra context astExpr
            let! attributes = getAttributesFromAlgebra algebra
            let! indexList =
                identityList
                |> listTryMap (tryFindAttributeIndex attributes)
            return RelationalAlgebraExpr.ProjectIndex (indexList, algebra)

        | Add (leftAttributes, rightAttributes, astExpr) ->
            let! algebra = generateAlgebra context astExpr
            return RelationalAlgebraExpr.Addtion (List.map toRAIAttribute leftAttributes, List.map toRAIAttribute rightAttributes, algebra)
        
        | Union (l, r) ->
            let! l = generateAlgebra context l
            let! r = generateAlgebra context r
            return RelationalAlgebraExpr.Union (l, r)

        | Except (l, r) ->
            let! l = generateAlgebra context l
            let! r = generateAlgebra context r
            return RelationalAlgebraExpr.Except (l, r)

        | Intersect (l, r) ->
            let! l = generateAlgebra context l
            let! r = generateAlgebra context r
            return RelationalAlgebraExpr.Except (l, RelationalAlgebraExpr.Except (l, r))

        | CrossJoin (l, r) ->
            let! l = generateAlgebra context l
            let! r = generateAlgebra context r
            return RelationalAlgebraExpr.CrossJoin (l, r)

        | LeftOuterJoin (l, r) ->
            let! l = generateAlgebra context l
            let! r = generateAlgebra context r
            let! attr1 = getAttributesFromAlgebra l
            let! attr2 = getAttributesFromAlgebra r
            let attr1Length = attr1.Length
            let attr2Length = attr2.Length
            let indexPairList = findEqualSetOfPropertiesIndex attr1 attr2
            let equalConditionExpr =
                indexPairList
                |> List.map (fun (index1, index2) -> ConditionExpr.Equal (IndexVaribale index1, IndexVaribale (index2 + attr1Length)))
                |> List.fold (fun state e -> ConditionExpr.And (state, e)) (ConditionExpr.Value true)

            let indexList = 
                let indexList2 =
                    indexPairList
                    |> List.map (fun (_, i) -> i + attr1Length)
                
                [0 .. attr1.Length + attr2.Length - 1]
                |> List.except indexList2

            return
                RelationalAlgebraExpr.ProjectIndex(
                    indexList,
                    RelationalAlgebraExpr.Union(
                        RelationalAlgebraExpr.Selection(
                            equalConditionExpr,
                            RelationalAlgebraExpr.CrossJoin(l, r)
                        ),
                        RelationalAlgebraExpr.CrossJoin(
                            RelationalAlgebraExpr.Except(
                                l,
                                RelationalAlgebraExpr.ProjectIndex(
                                    [0 .. attr1.Length - 1],
                                    RelationalAlgebraExpr.Selection(
                                        equalConditionExpr,
                                        RelationalAlgebraExpr.CrossJoin(l, r)
                                    )
                                )
                            ),
                            RelationalAlgebraExpr.Relation {
                                Attributes = attr2
                                Tuples = [List.init attr2Length (fun _ -> null)]
                            }
                        )
                    )
                )

        | RightOuterJoin (l, r) ->
            let! l = generateAlgebra context l
            let! r = generateAlgebra context r
            let! attr1 = getAttributesFromAlgebra l
            let! attr2 = getAttributesFromAlgebra r
            let attr1Length = attr1.Length
            let indexPairList = findEqualSetOfPropertiesIndex attr1 attr2
            let equalConditionExpr =
                indexPairList
                |> List.map (fun (index1, index2) -> ConditionExpr.Equal (IndexVaribale index1, IndexVaribale (index2 + attr1Length)))
                |> List.fold (fun state e -> ConditionExpr.And (state, e)) (ConditionExpr.Value true)

            let indexList = 
                let indexList1 =
                    indexPairList
                    |> List.map (fun (i, _) -> i)
                
                [0 .. attr1.Length + attr2.Length - 1]
                |> List.except indexList1

            return 
                RelationalAlgebraExpr.ProjectIndex(
                    indexList,
                    RelationalAlgebraExpr.Union(
                        RelationalAlgebraExpr.Selection(
                            equalConditionExpr,
                            RelationalAlgebraExpr.CrossJoin(l, r)
                        ),
                        RelationalAlgebraExpr.CrossJoin(
                            RelationalAlgebraExpr.Relation {
                                Attributes = attr1
                                Tuples = [List.init attr1Length (fun _ -> null)]
                            },
                            RelationalAlgebraExpr.Except(
                                r,
                                RelationalAlgebraExpr.ProjectIndex(
                                    [attr1.Length .. attr1.Length + attr2.Length - 1],
                                    RelationalAlgebraExpr.Selection(
                                        equalConditionExpr,
                                        RelationalAlgebraExpr.CrossJoin(l, r)
                                    )
                                )
                            )
                        )
                    )
                )

        | NatrualJoin (l, r) ->
            let! l = generateAlgebra context l
            let! r = generateAlgebra context r
            let! attr1 = getAttributesFromAlgebra l
            let! attr2 = getAttributesFromAlgebra r
            let attr1Length = attr1.Length
            let indexPairList = findEqualSetOfPropertiesIndex attr1 attr2
            let equalConditionExpr =
                indexPairList
                |> List.map (fun (index1, index2) -> ConditionExpr.Equal (IndexVaribale index1, IndexVaribale (index2 + attr1Length)))
                |> List.fold (fun state e -> ConditionExpr.And (state, e)) (ConditionExpr.Value true)

            let indexList = 
                let indexList2 =
                    indexPairList
                    |> List.map (fun (_, i) -> i + attr1Length)
                
                [0 .. attr1.Length + attr2.Length - 1]
                |> List.except indexList2

            return
                RelationalAlgebraExpr.ProjectIndex(
                    indexList,
                    RelationalAlgebraExpr.Selection(
                        equalConditionExpr,
                        RelationalAlgebraExpr.CrossJoin(l, r)
                    )
                )

    }
    
let rec generateStatement (context: RuntimeContext) (ast: AstStatement) =
    result {
        match ast with
        | AstStatement.Assignment (identity, astExpr) ->
            let! algebra = generateAlgebra context astExpr
            return RAIStatement.Assignment (identity, algebra)

        | AstStatement.Delete (identity) ->
            return RAIStatement.Delete identity

        | AstStatement.Expr (astExpr) ->
            let! algebra = generateAlgebra context astExpr
            return RAIStatement.Expr algebra
    }