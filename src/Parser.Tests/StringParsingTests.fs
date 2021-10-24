module StringParsingTests

open NUnit.Framework
open Parser

let parseString = Parser.run JsonParsing.JString.parser >> Result.map fst

[<Literal>]
let ErrorExpectedResult = "ParsesStringsCorrectly ERROR"

[<TestCase("\"some string\"", ExpectedResult = "some string")>]
[<TestCase("some unquoted string", ExpectedResult = ErrorExpectedResult)>]
[<TestCase("\"Unicode: \\u01E0\"", ExpectedResult = "Unicode: Ǡ")>]
let ParsesStringsCorrectly (rawJson) =
  match parseString rawJson with
  | Ok stringValue-> stringValue
  | Error _err -> ErrorExpectedResult
