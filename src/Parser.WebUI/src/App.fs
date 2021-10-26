module App

open Browser.Dom
open Parser

let mutable parsedJson: Result<JValue * string, string> = Error "Uninitialised"

let unparsedJsonInput = document.getElementById("raw-text-input") :?> Browser.Types.HTMLTextAreaElement
let parsedJsonOutput = document.getElementById("parsed-json") :?> Browser.Types.HTMLTextAreaElement

let rec renderValue indentationLevel value =
  let indent level =
    let twoSpaces = "  "
    String.replicate level twoSpaces

  let reduceLines (lines: string list) =
    match lines.Length with
    | 0 -> ""
    | _ -> lines |> List.reduce (fun a b -> $"""{a},
{indent indentationLevel}{b}""")

  match value with
  | JNull -> "null"
  | JBool bool -> string bool
  | JNumber num -> string num
  | JString str -> $"\"{str}\""
  | JArray array ->
    let lines = 
      array
      |> List.map (renderValue (indentationLevel + 1))
      |> reduceLines
     
    $"""[
{indent indentationLevel}{lines}
{indent (indentationLevel - 1)}]"""

  | JObject map -> 
    let lines = 
      map
      |> Map.toList
      |> List.map (fun (key, value) -> $"\"{key}\": {renderValue (indentationLevel + 1) value}")
      |> reduceLines
       
    $"""{{
{indent indentationLevel}{lines}
{indent (indentationLevel - 1)}}}"""

let parseRawText () = 
    parsedJson <- Parser.run JsonParsing.JSON.parser unparsedJsonInput.value
    match parsedJson with
    | Ok (jvalue, _remaining) -> 
        parsedJsonOutput.innerText <- renderValue 1 jvalue
    | Error error ->
        parsedJsonOutput.innerText <- sprintf "ERROR: %A" error

unparsedJsonInput.oninput <- fun _ev -> parseRawText()

parseRawText()