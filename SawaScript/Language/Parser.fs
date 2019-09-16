module Language.Parser
        open Language.Lexer
        open Nessos.Streams
        
        type 'r RuleChildren =
            | ConsumeRule of Token
            | OrRule of 'r list
            | AndRule of 'r list
            | Rule of 'r list
            | RulesList of 'r list
        
        type Rule = {
            name: string
            level: int
            optional: bool
            children: Rule RuleChildren
        }
        
        
        type 'a ParseResultChildren =
            | ParseResultValue of 'a
            | ParseResultList of 'a[]
        
        type ParserResult = {
            rule: Rule
            children: ParserResult ParseResultChildren
            start: int
            endPosition: int
            value: string
        }
        
        
        
        let Optional rule = { rule with optional = true }
        let Level lvl rule = { rule with level = lvl }
        let Pick rule = Level 1 rule
        
        let Consume (token: Token) = {
            name = token.name
            level = 0
            optional = false
            children = ConsumeRule token
        }
        
        let And  (children: Rule list) = {
            name = "AND"
            level = 0
            optional = false
            children = AndRule children
        }
        
        
        let Or (children: Rule list) = {
            name = "OR"
            level = 0
            optional = false
            children = OrRule children
        }
        
        let Rules (children: Rule list) = {
            name = "RULES"
            level = 0
            optional = false
            children = RulesList children
        }
        
        let Rule (name: string) (children: Rule list) = {
            name = name
            level = 0
            optional = false
            children = Rule children
        }
        
        
        let parseAnd (toffset: int) (gram: Rule list) parse (tokensPositions: TokenPosition[]) =
            let mutable off = toffset
            let r =
                gram
                |> Stream.ofList
                |> Stream.map (fun g ->
                    let vv = parse(tokensPositions, g, off)
                    printf "Parse and offset: %d res: %d\n" toffset (Array.length vv)
                    off <- off + Array.length vv
                    (vv, g)
                )
                |> Stream.filter (fun (v, g) -> g.optional = false)
                |> Stream.map (fun (v, g) -> v)
                |> Stream.takeWhile (fun _ -> off < tokensPositions.Length)
                |> Stream.toArray
                
            let a = r |> Array.forall (fun v -> Array.length v <> 0)
            if a then
                printf "Parse and success len: %d\n" (Array.length r)
                r |> Array.collect (fun (v) -> v)
            else [||]
        
        
        let rec parseExpr (tokensPositions: TokenPosition[], grammar: Rule, toffset: int) =
            match grammar.children with
            | ConsumeRule t ->
                 let tp = Array.get tokensPositions toffset
                 
                 if t.name.Equals(tp.token.name) then
                     printf "Consume %s is %s %d %s\n" t.name tp.token.name toffset tp.value    
                     [| {
                        rule = grammar
                        children = ParseResultList [||]
                        start = tp.offset
                        endPosition = tp.endPostion
                        value = tp.value
                     }|]
                 else
                     printf "Consume %s not %s %d %s\n" t.name tp.token.name toffset tp.value 
                     [||]
            | OrRule gram ->
                printf "Or %d\n" toffset
                let t =
                    gram
                    |> Stream.ofList
                    |> Stream.map (fun g ->
                        let v = parseExpr(tokensPositions, g, toffset)
                        
                        v
                    )
                    |> Stream.filter (fun t -> Array.length t <> 0)
                    |> Stream.tryHead
                    
                match t with
                | Some v -> v
                | None -> [||]
                
            | AndRule gram  ->
                parseAnd toffset gram parseExpr tokensPositions
                
            | Rule gram ->
                let result = parseAnd toffset gram parseExpr tokensPositions
                let children  = result |> Array.filter (fun v -> v.rule.level <> 0)
                printf "Rule name: %s t: %d off: %d" grammar.name toffset
                
                if  Array.length result <> 0 then
                    [|{
                        rule = grammar
                        children = ParseResultList children
                        start = (Array.head result).start
                        endPosition = (Array.last result).endPosition
                        value = result |> Array.map (fun c -> c.value) |> String.concat ""
                    }|]
                else
                    [||]
            | RulesList gram ->
                let mutable offset = toffset
                let vv =
                    Stream.initInfinite (fun i -> i)
                    |> Stream.map (fun _ ->
                        Stream.ofList gram
                        |> Stream.map(fun g ->
                          let v = parseExpr(tokensPositions, g, offset)
                          v
                        )
                        |> Stream.filter (fun g -> g.Length > 0)
                        |> Stream.take 1
                        |> Stream.toArray
                    )
                    |> Stream.map (fun v ->
                        if v.Length > 0 then
                            offset <- offset + (v |> Array.head |> Array.sumBy (fun s -> s.endPosition))
                        v
                    )
                    |> Stream.takeWhile (fun v -> offset < tokensPositions.Length && v.Length > 0)
                    |> Stream.map (fun v -> Array.head v)
                    |> Stream.toArray
                    |> Array.toList
                
                match vv with
                    | head :: tail -> head
                    | [] -> [||]
                    