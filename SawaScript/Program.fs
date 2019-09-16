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
   Consume VARNAME |> Pick
   Consume WHITESPACE |> Optional
   Consume EQUAL
   Consume WHITESPACE |> Optional
   Consume NUMBERLITERAL |> Pick
   Consume EOL |> Optional
]


let grammatic = Rules [
    VariableDeclaration
]

  
    
let unknownToken (code: string, offset: int32) =
    let len = if code.Length - offset > 10 then 10 else code.Length - offset

    printf "Unknown token: %s\n---------------^" (code.Substring(offset, len))



[<EntryPoint>]
let main argv =
    let code = "val a= 1val b= 2"
    
    let tokenPos = tokenize tokens code
    
    match tokenPos |> Array.tryLast with
    | Some token when token.endPostion <> String.length code ->
        unknownToken(code, token.endPostion)
        1
    | None -> 0
    | _ ->
        Array.iter (fun t -> printf "[%s '%s']" t.token.name t.value) tokenPos
        let mutable offset = 0
        let cst =
           Stream.initInfinite (fun i -> i)
           |> Stream.map (fun _ ->
              printf "Parse root expression start at %d\n" offset
              let v =  parseExpr(tokenPos, grammatic, offset)
              printf "Expression items %d\n" (Array.length v)
              v
           )
           |> Stream.map (fun expr ->
                if Array.length expr > 0 then
                     let head = (Array.head expr)
                     let tokenOffset = (Array.findIndex (fun t -> t.offset = head.endPosition) tokenPos)
                     offset <- tokenOffset
                else
                    offset <- offset
                    
                expr
           )
           |> Stream.takeWhile (fun _ -> offset < tokenPos.Length )
           |> Stream.toArray
//        let c = ast cst
        
        
        0
