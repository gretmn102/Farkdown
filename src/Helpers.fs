module Farkdown.Helpers
open Farkdown.SyntaxTree

let bold x = LineElement.Bold x
let italic x = LineElement.Italic x
let text s = LineElement.Text s
let img src title alt =
    LineElement.Image(src, title, alt)
let url href title description =
    LineElement.Url(href, title, description)

let h1 line statements = Statement.Header(1, line, statements)
let h2 line statements = Statement.Header(2, line, statements)

let p statements = Statement.Paragraph statements

let ul xs = Statement.List(false, xs)
let ol xs = Statement.List(true, xs)
let li line statements = line, statements : ListItem

let statementComment s = Statement.Comment s
let lineComment s = LineElement.Comment s
let inline comment<'T when 'T : (static member MakeComment: string -> 'T)> (s: string) : 'T =
    (^T : (static member MakeComment : string -> 'T) s)
