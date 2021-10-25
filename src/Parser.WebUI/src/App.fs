module App

open Browser.Dom
open Parser

let mutable parsedJson: Result<JValue * string, string> = Error "Uninitialised"

let unparsedJsonInput = document.getElementById("raw-text-input") :?> Browser.Types.HTMLTextAreaElement
let parsedJsonOutput = document.getElementById("parsed-json") :?> Browser.Types.HTMLTextAreaElement

let parseRawText () = 
    parsedJson <- Parser.run JsonParsing.JSON.parser unparsedJsonInput.value
    match parsedJson with
    | Ok (jvalue, _remaining) -> 
        parsedJsonOutput.innerText <- sprintf $"""%A{jvalue}"""
    | Error error ->
        parsedJsonOutput.innerText <- sprintf "ERROR: %A" error

unparsedJsonInput.oninput <- fun _ev -> parseRawText()

parseRawText()