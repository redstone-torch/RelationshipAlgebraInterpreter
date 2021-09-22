// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System
open System.Text
open Util
open Parser
open Statement
open System.IO

type ParserResult<'a, 'b> = FParsec.CharParsers.ParserResult<'a, 'b>

let printStr = printfn "%s"

[<EntryPoint>]
let main argv =

    let runStatement context statement =
        match Generator.generateStatement context statement with
        | Ok s ->
            match invokeStatement s context with
            | Ok (context, msg) -> printStr msg; context
            | Error msg -> printStr msg; context
        | Error msg -> printStr msg; context

    let runOnString context str =
        match FParsec.CharParsers.run pStatment str with
        | ParserResult.Success (statement, _, _) -> runStatement context statement
        | ParserResult.Failure (msg, _, _) -> printStr msg; context

    let runOnStream stream context =
        match FParsec.CharParsers.runParserOnStream pProgram () "noname" stream Encoding.UTF8 with
        | ParserResult.Success (statementList, _, _) ->
            List.fold runStatement context statementList
        | ParserResult.Failure (msg, _, _) -> printStr msg; context

    let context = Map.empty

    match argv with
    | [| "c" |] ->
        use stream = Console.OpenStandardInput()
        runOnStream stream context |> ignore

    | [| "f"; path |] ->
        use stream = File.OpenRead path
        runOnStream stream context |> ignore

    | [| "i" |] ->
        let inputs = (seq { while true do yield Console.ReadLine() })
        Seq.fold runOnString context inputs |> ignore

    | _ -> printfn "c: console input \nf path: file input \ni: interactive"

    0