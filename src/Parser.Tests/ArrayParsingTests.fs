module ArrayParsingTests

open NUnit.Framework
open Parser

let parseArray = Parser.run JsonParsing.JArray.parser >> Result.map fst

[<Test>]
let ParsesEmptyArray () =
  match parseArray "[]" with
  | Ok list -> Assert.IsEmpty(list)
  | Error err -> failwith err

[<Test>]
let ParsesArrayOfNumbers () =
  match parseArray "[1, 2.2, 3.65]" with
  | Ok list -> Assert.AreEqual([1.; 2.2; 3.65] |> List.map JNumber, list)
  | Error err -> failwith err

[<Test>]
let ParsesArrayOfArray () =
  match parseArray "[[], [3]]" with
  | Ok list -> Assert.AreEqual([JArray []; JArray [JNumber 3.]], list)
  | Error err -> failwith err
  
[<Test>]
let ParsesArrayWithInvalidTrailingComma () =
  match parseArray "[true, ]" with
  | Ok _list -> failwith "Expected Parse Error"
  | Error err -> Assert.AreEqual("Unexpected ','", err)

[<Test>]
let ParsesArrayWithInvalidMissingClosingBracket () =
  match parseArray "[true, false" with
  | Ok _list -> failwith "Expected Parse Error"
  | Error err -> Assert.AreEqual("input is null or empty", err)
