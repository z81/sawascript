open Nessos.Streams

open System
open System.IO
open Language.Lexer
open Language.Parser
open SawaScript.Ast
open SawaScript.Tokens
open Nessos.Streams


let VariableDeclaration = Rule "VariableDeclaration" [
   Or [ Consume VAR; Consume VAL ]
   Consume WHITESPACE
   Consume VARNAME |> Level 1
   Consume WHITESPACE |> Optional
   Consume EQUAL
   Consume WHITESPACE |> Optional
   Consume NUMBERLITERAL |> Level 1
]


let grammatic = Or [
    VariableDeclaration
]

  
    
let unknownToken (code: string, offset: int32) =
    let len = if code.Length - offset > 10 then 10 else code.Length - offset

    printf "Unknown token: %s\n---------------^" (code.Substring(offset, len))



[<EntryPoint>]
let main argv =
    let code = "val a= 1"
    
    let tokenPos = tokenize tokens code
    
    match tokenPos |> Array.tryLast with
    | Some token when token.endPostion <> String.length code ->
        unknownToken(code, token.endPostion)
        1
    | None -> 0
    | _ ->
        Array.iter (fun t -> printf "[%s '%s']" t.token.name t.value) tokenPos
        let cst = parse(tokenPos, grammatic, 0)
        let c = ast cst
        
        0
