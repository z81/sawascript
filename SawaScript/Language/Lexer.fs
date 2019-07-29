module Language.Lexer

open System.Text.RegularExpressions
open Nessos.Streams

type TokenParams = {
    name: string;
    pattern: string;
}

type Token = {
    name: string;
    pattern: Regex;
}

type TokenPosition = {
    offset: int;
    length: int;
    endPostion: int;
    token: Token;
    value: string;
}

let createToken (tokenParams: TokenParams) = {
    name = tokenParams.name
    pattern = Regex(@"\G" + tokenParams.pattern)
}

let tokenize tokens code =
     let mutable offset = 0
     Stream.initInfinite (fun i -> i)
        |> Stream.map (fun _ ->
            tokens
                |> Stream.ofList
                |> Stream.map (fun token -> (token, token.pattern.Match(code, offset)))
                |> Stream.filter (fun (_, result) -> result.Success)
                |> Stream.take 1
                |> Stream.tryHead
        )
        |> Stream.takeWhile Option.isSome
        |> Stream.map Option.get
        |> Stream.map (fun (token, result) ->
            offset <- result.Index + result.Length
            {
                offset = result.Index
                length = result.Length
                endPostion = result.Index + result.Length
                token = token
                value = code.Substring(result.Index, result.Length)
            }
        )
        |> Stream.toArray