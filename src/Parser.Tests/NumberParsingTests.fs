module NumberParsingTests

open NUnit.Framework
open Parser

let parseNull = Parser.run JsonParsing.pint >> Result.map fst
let parseFloat = Parser.run JsonParsing.pfloat >> Result.map fst

[<TestCase("1", true, 1)>]
[<TestCase("0", true, 0)>]
[<TestCase("01", true, 0)>]
[<TestCase("-1", true, -1)>]
[<TestCase("-0", true, 0)>]
let ParsesIntegersCorrectly (rawJson, isExpectedValid, expectedInt) =
  match parseNull rawJson, isExpectedValid with
  | Ok intValue, true -> Assert.AreEqual(expectedInt, intValue)
  | Error _err, false -> Assert.IsFalse(isExpectedValid)
  | Error err, true -> 
    failwith $"Unexpected parseInt result. Expected valid int %b{isExpectedValid}. ParsingError: {err}"    
  | Ok intValue, false -> 
    failwith $"Unexpected parseInt result. Expected valid int %b{isExpectedValid}. ParsedValue: {intValue}"

[<TestCase("56.25", true, 56.25)>]
[<TestCase("-56.25", true, -56.25)>]
[<TestCase("-50.00", true, -50.00)>]
[<TestCase("3.14159", true, 3.14159)>]
[<TestCase("3.005", true, 3.005)>]
[<TestCase("7984634.2", true, 7984634.2)>]
[<TestCase("0.2", true, 0.2)>]
[<TestCase("0.0", true, 0.0)>]
[<TestCase("00.2", false, -1)>]
[<TestCase("s56.25", false, -1)>]
[<TestCase(" 56.25", false, -1)>]
let ParsesFloatsCorrectly (rawJson, isExpectedValid, expectedFloat) =
  match parseFloat rawJson, isExpectedValid with
  | Ok floatValue, true -> Assert.AreEqual(expectedFloat, floatValue)
  | Error _err, false -> Assert.IsFalse(isExpectedValid)
  | Error err, true -> 
    failwith $"Unexpected parseFloat result. Expected valid float %b{isExpectedValid}. ParsingError: {err}"    
  | Ok floatValue, false -> 
    failwith $"Unexpected parseFloat result. Expected valid float %b{isExpectedValid}. ParsedValue: {floatValue}"
    

