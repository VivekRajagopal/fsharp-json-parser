open Parser

[<Literal>]
let DefaultJsonToParse = """{ "key": null }"""

[<EntryPoint>]
let main argv =
  let jsonToParse =
    argv
    |> List.ofArray
    |> List.tryHead
    |> Option.defaultValue DefaultJsonToParse

  let parseResult = 
    jsonToParse
    |> Parser.run JsonParsing.JSON.parser
  
  match parseResult with
  | Ok jvalue -> 
    printf "%A\n" jvalue
  | Error message -> 
    printf "%s\n" message

  0