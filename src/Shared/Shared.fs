namespace Shared

type Span =
  { Start: int
    End: int }

type TextTag =
  | Plain
  | Span of string
  | Mark of (string * TextTag) list

type XmlNode =
  { Type: string
    TypeClass: string
    Text: string option
    Span: Span
    Children: XmlNode list }

type XmlResult =
  { ParsedTree: XmlNode option
    ClassifiedXml: (string * TextTag) list }

[<CLIMutable>]
type XmlRequest =
  { XmlToParse: string }

module Shared =
  [<Literal>]
  let InitialXml = @"<?xml version=""1.0"" encoding=""UTF-8"" standalone=""yes""?>
  <X>
      <n:T></n:T>
      <X/>
      <A.B></A.B>
      <A B=""value""></A>
      <A>&#x03C0;</A>
      <A>a &lt;</A>
      <A><![CDATA[bar]]></A>
      <!-- comment -->
  </X>"