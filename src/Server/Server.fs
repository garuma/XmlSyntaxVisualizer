open System
open System.IO
open System.Threading.Tasks

open Microsoft.AspNetCore
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Hosting
open Microsoft.Extensions.DependencyInjection
open Microsoft.AspNetCore.Http

open Giraffe
open Giraffe.Serialization.Json
open FSharp.Control.Tasks
open Newtonsoft.Json

open Shared
open Microsoft.Language.Xml

let clientPath = Path.Combine("..","Client") |> Path.GetFullPath
let port = 8085us

let shouldClassify = function
  | XmlClassificationTypes.XmlName
  | XmlClassificationTypes.XmlAttributeName
  | XmlClassificationTypes.XmlAttributeQuotes
  | XmlClassificationTypes.XmlAttributeValue -> true
  | _ -> false

let rec wrapString (str: String) (startIndex: int) (spans: (int * int * XmlClassificationTypes) list) =
  if List.isEmpty spans then
    [ str.Substring(startIndex), Plain ]
  else
    let (start, count, classification) = List.head spans
    if shouldClassify classification then
      let prelude = if startIndex = start then String.Empty else str.Substring(startIndex, start - startIndex)
      let value = str.Substring(start, count)
      let wrappedValue = value, Span (classification.ToString())
      let result = wrappedValue :: wrapString str (start + count) (List.tail spans)
      if String.IsNullOrEmpty(prelude) then result else (prelude, Plain) :: result
    else
      wrapString str startIndex (List.tail spans)

let createClassifiedHtml (node: SyntaxNode) =
  let mutable classifications = List.empty
  ClassifierVisitor.VisitRecursive(
    node,
    0,
    node.End,
    fun start count node classification -> classifications <- (start, count, classification) :: classifications
  ) |> ignore
  classifications <- List.rev classifications
  let fullText = node.ToFullString()
  let html = wrapString fullText 0 classifications
  html

let rec exportNodeTree (node: SyntaxNode) =
  let name = if node.IsList then "SyntaxList" else node.GetType().Name
  let text = if node.IsToken then Some (node :?> SyntaxToken).Text else None
  { Type = name
    TypeClass = if node.IsList then "list" else if node.IsToken then "token" else "syntax"
    Text = text
    Collapsed = false
    Span = { Start = node.FullSpan.Start; End = node.FullSpan.End }
    Children = List.ofSeq (Seq.map exportNodeTree node.ChildNodes) }

let parseXmlTree xml =
  let rootNode = Parser.ParseText(xml)
  let classifiedXml = createClassifiedHtml rootNode
  let nodeTree = exportNodeTree rootNode
  { ParsedTree = Some nodeTree
    ClassifiedXml = classifiedXml }

let parseXml =
  fun (next : HttpFunc) (ctx : HttpContext) ->
    task {
      let! payload = ctx.BindModelAsync<XmlRequest>()
      let! result = Task.Run(fun () -> parseXmlTree payload.XmlToParse)
      return! json result next ctx
    }

let webApp : HttpHandler =
  choose [
    GET  >=> route "/" >=> redirectTo true "/index.html"
    POST >=> route "/api/parseXml" >=> parseXml
  ]

let configureApp  (app : IApplicationBuilder) =
  app.UseStaticFiles()
     .UseGiraffe webApp

let configureServices (services : IServiceCollection) =
    services.AddGiraffe() |> ignore
    // Configure JsonSerializer to use Fable.JsonConverter
    let fableJsonSettings = JsonSerializerSettings()
    fableJsonSettings.Converters.Add(Fable.JsonConverter())

    services.AddSingleton<IJsonSerializer>(
        NewtonsoftJsonSerializer(fableJsonSettings)) |> ignore

WebHost
  .CreateDefaultBuilder()
  .UseKestrel(fun options -> options.Limits.MaxRequestBodySize <- new System.Nullable<int64> (15_000L))
  .UseWebRoot(clientPath)
  .UseContentRoot(clientPath)
  .Configure(Action<IApplicationBuilder> configureApp)
  .ConfigureServices(configureServices)
  .UseUrls("http://0.0.0.0:" + port.ToString() + "/")
  .Build()
  .Run()