# fsharp-json-parser

[![cicd_webui](https://github.com/VivekRajagopal/fsharp-json-parser/actions/workflows/cicd_webui.yml/badge.svg?branch=main)](https://github.com/VivekRajagopal/fsharp-json-parser/actions/workflows/cicd_webui.yml)

Basic JSON Parser written in FSharp. Two basic client interfaces exist; a CLI and a WebUI. The WebUI is built with Fable, a FSharp to JS transpiler, which allows direct importing of FSharp projects into the WebUI App source.

## Requirements

- dotnet 6+
- npm + node

## Development

1. Clone repo
2. `cd src`
3. `dotnet restore`
4. `dotnet build`

### CLI

1. `dotnet run --project Parser.CLI -- <raw-json-text>`
    - e.g. `dotnet run --project Parser.CLI -- '{"hello": "world"}'`

### WebUI

1. `cd Parser.WebUI`
2. `npm install`
3. `npm start`

## Testing

Testing uses NUnit. In `src` folder, run `dotnet test`

## CICD

Github Actions is used to deploy the WebUI build to Netlify. Due to custom build processing involving `dotnet`, Netlify cannot directly be used to build and deploy the site.
