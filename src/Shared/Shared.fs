namespace Shared

open System

type Span =
  { Start: int
    End: int }

type TextTag =
  | Plain
  | Span of string
  | Mark of (string * TextTag) list

[<CustomEquality;NoComparison>]
type XmlNode =
  { Type: string
    TypeClass: string
    Text: string option
    Span: Span
    Collapsed: bool
    Children: XmlNode list }
  override x.Equals(yObj) =
    match yObj with
    | :? XmlNode as y -> (String.Equals(y.Type, x.Type, StringComparison.Ordinal) && y.Span.Start = x.Span.Start && y.Span.End = x.Span.End)
    | _ -> false

  override x.GetHashCode() = hash x.Span

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