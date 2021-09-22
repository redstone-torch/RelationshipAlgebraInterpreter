module Util

type ResultBuilder() =
    member inline _.Bind(m, f) =
        match m with
        | Result.Ok r -> f r
        | Result.Error e -> Result.Error e
    member inline _.Return(x) = Result.Ok x
    member inline _.ReturnFrom(x) = x
    member inline _.Zero() = Result.Ok ()

/// 用于处理Reuslt的计算表达式
let result = ResultBuilder()

type Either<'a, 'b> =
    | Left of v: 'a
    | Right of v: 'b

let inline (++) (list1: ^t list) (list2: ^t list) = List.append list1 list2

/// 借鉴 haskell foldM
/// 
/// 如果在fold的过程中出现了Error 则终止过程并返回Error 否则按照正常fold的过程执行
let listFoldResult (f: 'state -> 'a -> Result<'state, 'error>) (state: 'state) (list: 'a list) : Result<'state, 'error> =
    let rec loop f state list =
        match list with
        | [] -> Ok state
        | e :: xs ->
            match f state e with
            | Ok r -> loop f r xs
            | Error e -> Error e
    loop f state list

/// 借鉴 haskell foldM
/// 
/// 如果在foldBack的过程中出现了Error 则终止过程并返回Error 否则按照正常foldBack的过程执行
let listFoldBackResult (f: 'state -> 'a -> Result<'state, 'error>) (state: 'state) (list: 'a list) : Result<'state, 'error> =
    let rec loop f state list =
        match list with
        | [] -> Ok state
        | e :: xs ->
            match loop f state xs with
            | Ok r -> f r e
            | Error e -> Error e
    loop f state list

/// 映射所有的选项到Result
///
/// 如果全部Ok 则返回转换后的列表 一旦存在Error则失败 并且返回Error的值
let listTryMap (mapping: 'a -> Result<'b, 'error>) (list: 'a list) : Result<'b list, 'error> =
    listFoldBackResult (fun l a ->
        match mapping a with
        | Ok b -> Ok (b :: l)
        | Error e -> Error e
    ) [] list

/// 过滤
///
/// 一旦存在Error则失败 并且返回Error的值
let listTryFilter (predicate: 'a -> Result<bool, 'error>) (list: 'a list) : Result<'a list, 'error> =
    listFoldBackResult (fun l a ->
        match predicate a with
        | Ok b -> if b then Ok (a :: l) else Ok l
        | Error e -> Error e
    ) [] list

let optionToResult (error: 'error) (option: 'a option) =
    match option with
    | Some a -> Ok a
    | None -> Error error

let charListToString (chars: char list) =
    chars
    |> List.fold (fun (builder: System.Text.StringBuilder) c -> builder.Append(c)) (new System.Text.StringBuilder())
    |> string


