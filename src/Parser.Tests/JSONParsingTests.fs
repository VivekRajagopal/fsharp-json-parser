module JSONParsingTests

open NUnit.Framework
open Parser

let parseJSON = Parser.run JsonParsing.JSON.parser >> Result.map fst

[<Test>]
let ParsesEmptyJSON () =
  match parseJSON "[]" with
  | Ok (JArray list) -> Assert.IsEmpty(list)
  | Ok notJArray -> Assert.Fail($"Expected JArray. Got %A{notJArray}")
  | Error err -> Assert.Fail(err)

[<Test>]
let ParsesJSONOfNumbers () =
  match parseJSON "[1, 2.2, 3.65]" with
  | Ok list -> Assert.AreEqual([1.; 2.2; 3.65] |> List.map JNumber |> JArray, list)
  | Error err -> failwith err

[<Test>]
let ParsesJSONOfJSON () =
  match parseJSON "[[], [3]]" with
  | Ok list -> Assert.AreEqual(JArray [JArray []; JArray [JNumber 3.]], list)
  | Error err -> failwith err
  
[<Test>]
let ParsesJSONWithInvalidTrailingComma () =
  match parseJSON "[true, ]" with
  | Ok _list -> failwith "Expected Parse Error"
  | Error err -> Assert.AreEqual("Unexpected '['", err)

[<Test>]
let ParsesJSONWithInvalidMissingClosingBracket () =
  match parseJSON "[true, false" with
  | Ok _list -> failwith "Expected Parse Error"
  | Error err -> Assert.AreEqual("Unexpected '['", err)
