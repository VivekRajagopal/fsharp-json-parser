module App

open Browser.Dom
open Parser

// Mutable variable to count the number of times we clicked the button
let mutable parsedJson: Result<JValue * string, string> = Error "Uninitialised"

// Get a reference to our button and cast the Element to an HTMLButtonElement
let unparsedJsonInput = document.getElementById("raw-text-input") :?> Browser.Types.HTMLTextAreaElement
let parsedJsonOutput = document.getElementById("parsed-json") :?> Browser.Types.HTMLTextAreaElement

// Register our listener
unparsedJsonInput.oninput <- fun _ev ->
    parsedJson <- Parser.run JsonParsing.JSON.parser unparsedJsonInput.value
    match parsedJson with
    | Ok (jvalue, remaining) -> 
        parsedJsonOutput.innerText <- sprintf "%A" jvalue
    | Error error ->
        parsedJsonOutput.innerText <- sprintf "ERROR: %A" error
