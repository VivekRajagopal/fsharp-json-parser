namespace Parser

open System

type Parser<'T> =
  Parser of (string -> Result<'T * string, string>)

module Parser =
  let satisfy predicate =
    let parser input =
      if String.IsNullOrEmpty(input) then
        Error "input is null or empty"
      else
        let a = input.[0]

        if predicate a then
          let rem = input.[1..]
          Ok (a, rem)

        else
          Error $"Unexpected '%c{a}'"

    Parser parser

  let mapP f (Parser p) =
    let parser input =
      input
      |> p
      |> Result.map (fun (v, r) -> f v, r)

    Parser parser

  let run (Parser parser) input =
    input |> parser

  let andThen (Parser parser1) (Parser parser2) =
    let parser input =
      input
      |> parser1
      |> Result.bind (
        fun (v1, r1) -> 
          parser2 r1 
          |> Result.map (fun (v2,r2) -> (v1, v2, r2))
      )
      |> Result.bind (fun (v1, v2, r2) -> Ok ((v1, v2), r2))

    Parser parser

  let (>>>) = andThen  
  
  let orElse (Parser parser1) (Parser parser2) =
    let parser input =
      let result1 = input |> parser1
      let result2 = input |> parser2

      match result1, result2 with
      | Ok (v1, r1), Ok _ -> Ok (v1, r1)
      | _, Ok (v2, r2) -> Ok (v2, r2)
      | Ok (v1, r1), _ -> Ok (v1, r1)
      | Error err1, Error _ -> Error err1

    Parser parser

  let (<|>) = orElse

  let choice parsers =
    List.reduce (<|>) parsers

  let (.>>) p1 p2 =
    p1 >>> p2
    |> mapP (fun (a, _b) -> a)
  
  let (>>.) p1 p2 =
    p1 >>> p2
    |> mapP (fun (_a, b) -> b)

  let returnP x =
    (fun input -> Ok (x, input))
    |> Parser      

  let consP p1 p2 =
    p1 >>> p2
    |> mapP (fun (v1, v2) -> v1::v2)

  let rec requireAll (parsers: Parser<'a> list) =
    match parsers with
    | [] -> returnP []
    | headParser::tail ->
      tail
      |> requireAll
      |> consP headParser

  let rec zeroOrMany (Parser parser) input =
    match parser input with
    | Error _ -> [], input
    | Ok (firstMatch, remainingAfterFirst) ->
      let matches, remaining = zeroOrMany (Parser parser) remainingAfterFirst
      firstMatch::matches, remaining

  let many parser =
    let parser' input =
      Ok (zeroOrMany parser input)
    Parser parser'

  let atleastOne parser =
    let parser' input =
      match zeroOrMany parser input with
      | [], _ -> Error "atleastOne matched zero"
      | atleastOne, rem -> Ok (atleastOne, rem)

    Parser parser'

  let opt parser =
    (parser |> mapP Some) <|> (returnP None)

  let setResult r p =
    p
    |> mapP (fun _v -> r)

  let sepBy1 parser seperator =
    parser >>> many (seperator >>. parser)
    |> mapP (fun (head, tail) -> head::tail)

  let sepBy parser separator =
    sepBy1 parser separator <|> returnP []

[<RequireQualifiedAccess>]
module JsonParsing =
  open Parser
  
  let pchar charToMatch =
    satisfy (fun c -> c = charToMatch)

  let digitChar =
    satisfy Char.IsDigit

  let nonZeroDigitChar =
    satisfy (fun c -> Char.IsDigit(c) && c <> '0')

  let whitespaceChar =
    satisfy Char.IsWhiteSpace

  let anyChars chars =
    chars
    |> List.map pchar
    |> choice

  let pliteral (literal: string) =
    literal.ToCharArray()
    |> List.ofArray
    |> List.map pchar
    |> requireAll
    |> mapP (List.toArray >> String)

  let pint =
    let wholeZero = (pchar '0' |> setResult 0)
    let nonZeroWithFollowingDigits =
      nonZeroDigitChar >>> many digitChar
      |> mapP (
        fun (firstChar, remainingChars) -> int $"{firstChar}{remainingChars |> List.toArray |> String}"
      )
    let firstZeroOrManyNonZero = wholeZero <|> nonZeroWithFollowingDigits

    opt (pchar '-') >>> firstZeroOrManyNonZero
    |> mapP (
      function
      | Some _ch, num -> -1 * num
      | None, num -> num
    )

  let pfloat =
    pint >>> (pchar '.') >>> atleastOne digitChar
    |> mapP (fun ((whole, _point), fraction) -> $"%i{whole}.%s{fraction |> List.toArray |> String}" |> float)

  let pNumber = pfloat <|> (mapP float pint)

  let whitespace = many whitespaceChar

  let between p1 p2 p3 =
    p1 >>. p2 .>> p3

  let pbool = (pliteral "true" |> setResult true) <|> (pliteral "false" |> setResult false)

  let pnull = pliteral "null" |> setResult ()

  [<RequireQualifiedAccess>]
  module JString =
    let jUnescapedChar = satisfy (fun ch -> ch <> '\\' && ch <> '\"')
    let jEscapedChar = 
      [
        ("\\\"",'\"')      // quote
        ("\\\\",'\\')      // reverse solidus
        ("\\/",'/')        // solidus
        ("\\b",'\b')       // backspace
        ("\\f",'\f')       // formfeed
        ("\\n",'\n')       // newline
        ("\\r",'\r')       // cr
        ("\\t",'\t')       // tab
      ]
      |> List.map (fun (literal, char) -> pliteral literal |> mapP (fun _ -> char))
      |> choice

    let jUnicodeChar =
      let backslash = pchar '\\'
      let uChar = pchar 'u'
      let hexDigit = anyChars (['0'..'9'] @ ['A'..'F'] @ ['a'..'f'])

      let fourHexDigits = hexDigit >>> hexDigit >>> hexDigit >>> hexDigit

      backslash >>. uChar >>. fourHexDigits
      |> mapP (fun (((h1, h2), h3), h4) -> 
        Int32.Parse($"{h1}{h2}{h3}{h4}", Globalization.NumberStyles.HexNumber) 
        |> char
      )

    let jstring = 
      jUnescapedChar <|> jEscapedChar <|> jUnicodeChar 
      |> many
      |> mapP (List.toArray >> String)

    let parser =
      let quote = pchar '\"'
      
      quote >>. jstring .>> quote

  let createForwardRefParser<'T> () =
    let dummyParser: Parser<'T> = Parser (fun _ -> failwith "Null forward parser")

    let parserRef = ref dummyParser

    let actualParser input =
      input 
      |> Parser.run !parserRef

    Parser actualParser, parserRef

  let jValue, jValueRef = createForwardRefParser<JValue>()

  [<RequireQualifiedAccess>]
  module JArray =
    let opening = pchar '[' .>> whitespace
    let closing = pchar ']' .>> whitespace
    let comma = pchar ',' .>> whitespace
    let values = sepBy (jValue .>> whitespace) comma

    let parser = opening >>. values .>> closing

  [<RequireQualifiedAccess>]
  module JObject =
    let keyValuePair =
      JString.parser .>> whitespace >>> pchar ':' .>> whitespace >>> jValue
      |> mapP (fun ((key, _sep), value) -> key, value)

    let opening = pchar '{' .>> whitespace
    let closing = pchar '}' .>> whitespace
    let comma = pchar ',' .>> whitespace
    let values = sepBy (keyValuePair .>> whitespace) comma

    let parser = 
      opening >>. values .>> closing
      |> mapP Map.ofList

  [<RequireQualifiedAccess>]
  module JValue =
    jValueRef :=
      (setResult JNull pnull)
      <|> (mapP JBool pbool)
      <|> (mapP JNumber pNumber)
      <|> (mapP JString JString.parser)
      <|> (mapP JArray JArray.parser)
      <|> (mapP JObject JObject.parser)

    let parser = jValue

  [<RequireQualifiedAccess>]
  module JSON =
    let parser =
      (opt whitespace) >>. jValue .>> (opt whitespace)