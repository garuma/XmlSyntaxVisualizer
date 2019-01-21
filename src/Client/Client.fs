module Client

open System

open Elmish
open Elmish.React

open Fable.Core.JsInterop
open Fable.Helpers.React
open Fable.Helpers.React.Props
open Fable.PowerPack.Fetch
open Fable.PowerPack

open Shared

open Fulma
open Thoth.Json

type Model =
  { Xml: string
    ParsedTree: XmlNode option
    ClassifiedXml: (string * TextTag) list
    HighlightedSpan: Span option
  }

type Msg =
| ParseXml of string
| ChangeCurrentSpan of Span
| ToggleNode of XmlNode
| ParsedXml of Result<Result<XmlResult, string>, exn>

let rec toggleXmlNode (node: XmlNode) (root: XmlNode) =
  if root = node then
    { root with Collapsed = not root.Collapsed }
  else
    { root with Children = List.map (toggleXmlNode node) root.Children }

let init () = 
  let model = 
    { Xml = Shared.InitialXml
      ParsedTree = None
      ClassifiedXml = []
      HighlightedSpan = None }
  model, (Cmd.ofMsg (ParseXml Shared.InitialXml))

let xmlResultDecoder = Decode.Auto.generateDecoder<XmlResult>()

let update msg (model : Model) =
  let model' =
    match msg with
    | ParsedXml (Ok (Ok { ParsedTree = newNode; ClassifiedXml = classifiedXml })) ->
      { model with ParsedTree = newNode
                   ClassifiedXml = classifiedXml }
    | ParseXml xml ->
      { model with Xml = xml }
    | ChangeCurrentSpan span ->
      { model with HighlightedSpan = Some span }
    | ToggleNode xmlNode ->
      { model with ParsedTree = Option.bind (fun root -> Some (toggleXmlNode xmlNode root)) model.ParsedTree }
    | _ -> model
  let postXml xml _ =
    postRecord "/api/parseXml" { XmlToParse = xml } []
    |> Promise.bind (fun fetched -> fetched.text())
    |> Promise.map (Decode.fromString xmlResultDecoder)
  let cmd' =
    match msg with
    | ParseXml xml ->
      Cmd.ofPromise
        (postXml xml)
        []
        (Ok >> ParsedXml)
        (Error >> ParsedXml)
    | _ -> Cmd.none
  model', cmd'

let strOrEmpty =
  function
  | Some str -> str
  | None -> String.Empty

let createTreeViewLabel xmlNode =
  [ span [Class ("node-type " + xmlNode.TypeClass)] [str xmlNode.Type] 
    span [Class "node-text"] [str (strOrEmpty xmlNode.Text)]
    span [Class "node-span"] [str (sprintf "[%i..%i)" xmlNode.Span.Start xmlNode.Span.End)]
  ]

let rec createTreeViewNode xmlNode changeSpanCallback toggleNodeCallback =
  let childNodes =
    if xmlNode.Children.Length > 0 then
      [ul
        [Class "menu-list"; Style [Display (if xmlNode.Collapsed then "none" else "block")]]
        [ for child in xmlNode.Children do yield (createTreeViewNode child changeSpanCallback toggleNodeCallback) ]]
    else
      []
  li []
    ([ a [ OnMouseEnter (fun _ -> changeSpanCallback xmlNode)
           OnClick (fun _ -> toggleNodeCallback xmlNode)]  (createTreeViewLabel xmlNode)] @ childNodes)

let createTreeView model changeSpanCallback toggleNodeCallback =
  match model.ParsedTree with
  | Some rootNode -> [ul [Class "menu-list"] [ createTreeViewNode rootNode changeSpanCallback toggleNodeCallback ]]
  | None -> [str "XML Tree"]

let createTextArea model changeXmlCallback =
  [textarea
    [ OnChange (fun (e: Fable.Import.React.FormEvent) -> changeXmlCallback (unbox e.target?value))
      DefaultValue model.Xml
    ]
    []
  ]

let addHighlight (classifiedXml: (string * TextTag) list) (span: Span) =
  let rec grabUntil count (classifiedList: (string * TextTag) list) =
    if List.isEmpty classifiedList then [], []
    else
      let (text, tag) = List.head classifiedList
      if text.Length > count then
        [ text.Substring(0, count), tag ], ((text.Substring(count), tag) :: List.tail classifiedList)
      else
        let (additional, tail) = grabUntil (count - text.Length) (List.tail classifiedList)
        (text, tag) :: additional, tail
  (* Split the tagged list of string into a 3-tuple where the middle element contains the list of
     tagged text that falls in the given span, if necessary that method can cut in the middle of
     an existing tagged text and project it to 3 items with the same tag and the text splitted *)
  let rec splitBetween (span: Span) (classifiedList: (string * TextTag) list) (previous: (string * TextTag) list) =
    if List.isEmpty classifiedList then
      (List.rev previous), None, []
    else
      let head = List.head classifiedList
      let (text, tag) = head
      let length = text.Length
      if span.Start > length then splitBetween ({ Start = span.Start - length; End = span.End - length }) (List.tail classifiedList) (head :: previous)
      else
        let start = span.Start
        let realCount = span.End - start;
        let currentCount = Math.Min(length - start, realCount)

        (* Take the text we need from the current node *)
        let prevText = text.Substring(0, start)
        let middleText = text.Substring(start, currentCount)

        let previous' = List.rev ((prevText, tag) :: previous)

        if realCount = currentCount then
          (* We got all the text we needed so just return now with what's left of this node *)
          let remainingText = text.Substring(span.End)
          previous', Some [(middleText, tag)], (remainingText, tag) :: (List.tail classifiedList)
        else
          (* We still need to steal text from the following tagged text nodes which will likely split a node further down *)
          let middle, tail = grabUntil (realCount - currentCount) (List.tail classifiedList)
          previous', Some ((middleText, tag) :: middle), tail
  let (before, middle, tail) = splitBetween span classifiedXml []
  (*printfn "%A :: %A :: %A" before middle tail*)
  match middle with
    | None -> classifiedXml
    | Some inner -> before @ ((String.Empty, Mark inner) :: tail)


let rec reduceClassifiedList classifiedXml =
  let mapper =
    function
    | (text, Span name) -> span [Class name] [str text]
    | (text, Plain) -> str text
    | (_, Mark inner) -> mark [] (reduceClassifiedList inner)
  List.map mapper classifiedXml

let createClassificationWindow model =
  let content =
    match model.HighlightedSpan with
    | Some span -> addHighlight model.ClassifiedXml span
    | _ -> model.ClassifiedXml
  [Content.content [] [pre [] (reduceClassifiedList content) ]]

let view model dispatch =
  let changeXmlCallback xml = dispatch (ParseXml xml)
  let changeSpanCallback node = dispatch (ChangeCurrentSpan node.Span)
  let toggleNodeCallback node = dispatch (ToggleNode node)
  Tile.ancestor [Tile.Props [Style [ Height "100%"; Width "100%" ] ] ]
    [ Tile.parent [Tile.Size Tile.Is7]
        [ Tile.child [Tile.CustomClass "editor"] (createTextArea model changeXmlCallback)]
      Tile.parent [Tile.IsVertical]
        [ Tile.child [Tile.CustomClass "xml-tree"] (createTreeView model changeSpanCallback toggleNodeCallback)
          Tile.child [Tile.CustomClass "classification"] (createClassificationWindow model)
        ]
    ]
  
#if DEBUG
open Elmish.Debug
open Elmish.HMR
#endif

Program.mkProgram init update view
#if DEBUG
|> Program.withConsoleTrace
|> Program.withHMR
#endif
|> Program.withReact "elmish-app"
#if DEBUG
|> Program.withDebugger
#endif
|> Program.run
