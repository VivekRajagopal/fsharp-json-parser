module BoolParsingTests

open NUnit.Framework
open Parser

let parseBool = Parser.run JsonParsing.pbool >> Result.map fst

type ParsesBoolCorrectlyResult =
  | True = 0
  | False = 1
  | Error = 2

[<TestCase("true", true, ExpectedResult = "True")>]
[<TestCase("false", true, ExpectedResult = "False")>]
[<TestCase("troi", false, ExpectedResult = "Error")>]
let ParsesBoolCorrectly (rawJson, isExpectedValid) =
  match parseBool rawJson, isExpectedValid with
  | Ok value, true -> if value then "True" else "False"
  | Ok _value, false -> failwith "isExpectedValid should be true for Parse Ok"
  | Error _err, true -> failwith "isExpectedValid should be false for Parse Error"
  | Error _err, false -> "Error"
