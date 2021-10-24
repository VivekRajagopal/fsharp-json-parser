module NullParsingTests

open NUnit.Framework
open Parser

let parseNull = Parser.run JsonParsing.pnull >> Result.map fst

[<TestCase("null", true)>]
[<TestCase("n0ll", false)>]
let ParsesNullCorrectly (rawJson, isExpectedValid) =
  match parseNull rawJson, isExpectedValid with
  | Ok (), true -> Assert.IsTrue(isExpectedValid)
  | Ok (), false -> failwith "isExpectedValid should be true for Parse Ok"
  | Error _err, false -> Assert.IsFalse(isExpectedValid)
  | Error _err, true -> failwith "isExpectedValid should be false for Parse Error"
    

