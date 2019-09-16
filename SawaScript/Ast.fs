module SawaScript.Ast
open Language.Parser

type Identifier = {
    name: string
    start: int
    endPostion: int
}

type NumberLiteral = {
    raw: string
    value: int
    start: int
    endPostion: int
}

    
type VariableDeclaration = {
    start: int
    endPostion: int
    id: Identifier
    value: NumberLiteral
}


type 'a NodeChildren =
    | NodeValue of VariableDeclaration
    | NodeList of 'a list


type Node = {
    children: Node NodeChildren
}
    

let parseVarDeclr (p: ParserResult) =
    match p.children with
    | ParseResultList t->
        let id = t |> Array.head 
        let value  = t |> Array.last
        
        {
            start = p.start
            endPostion = p.endPosition
            id = {
                 start = id.start
                 endPostion = id.endPosition
                 name = id.value
            }
            value = {
                start = value.start
                endPostion = value.endPosition
                raw = value.value
                value = value.value |> int
            }
        }

let ast (parseResult: ParserResult[][]) =
    parseResult |> Array.map (fun v ->
        Array.map (fun z ->
            match z.rule.name with
            | "VariableDeclaration" ->
            {
                children = NodeValue(parseVarDeclr z)
            }    
        )
    ) 
