module Tests.Program
open Fuchu
open FsharpMyExtension
open FsharpMyExtension.Either

open Farkdown.Markdown

let parse content =
    let nodes contents =
        contents
        |> HtmlAgilityPackExt.HtmlNode.ofString
        |> fun x -> x.ChildNodes

    nodes content
    |> HtmlToMarkdown.start2
    |> Either.getOrDef' (failwithf "%A")

[<Tests>]
let listTest =
    let parsed =
        [HtmlList
           (false,
            [[Paragraph [[Text "First"]];
              HtmlList
                (false,
                 [[JustInlines [[Text "First.Item First"]]];
                  [JustInlines
                     [[Text "First.Item "; WithFontStyle (FontStyle.Italic, [Text "Second"])];
                      []; [Text "Second Text"]]]]); Paragraph [[Text "First Text"]];
              HtmlList (false, [[JustInlines [[Text "First.Item Third"]]]])];
             [Paragraph [[Text "Second"]; [Text "Second Text"]]]])]
    let markdown =
        [
            "* First"
            "  * First.Item First"
            "  * First.Item *Second*"
            "    Second Text"
            ""
            "  First Text"
            "  * First.Item Third"
            "* Second"
            "  Second Text"
        ] |> String.concat "\n"
    testList "list test" [
        testCase "parse test" <| fun () ->
            let input =
                [
                    "<ul>"
                    "<li><p>First</p>"
                    "<ul>"
                    "<li>First.Item First</li>"
                    "<li>First.Item <em>Second</em><br>"
                    "Second Text</li>"
                    "</ul>"
                    "<p>First Text</p>"
                    "<ul>"
                    "<li>First.Item Third</li>"
                    "</ul>"
                    "</li>"
                    "<li><p>Second<br>"
                    "Second Text</p>"
                    "</li>"
                    "</ul>"
                ] |> String.concat "\n"
            let act =
                parse input
                |> List.map FlowContent.format

            Assert.Equal("", parsed, act)
        testCase "parse test" <| fun () ->
            let act = Show.print parsed

            Assert.Equal("", markdown, act)
    ]

[<EntryPoint>]
let main arg =
    defaultMainThisAssembly arg
