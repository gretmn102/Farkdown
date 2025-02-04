module SyntaxTree.Tests
open Fuchu
open FsharpMyExtension.FParsecExt

open Farkdown.SyntaxTree
open Farkdown.Helpers

[<Tests>]
let ``LineElement.Parser.pimage`` =
    testList "LineElement.Parser.pimage" [
        testCase "![]()" <| fun () ->
            Assert.Equal(
                "",
                runResult LineElement.Parser.pimage "![]()",
                Ok {| Alt = ""; Src = ""; Title = None; |}
            )
        testCase "![](https://example.com/image.png)" <| fun () ->
            Assert.Equal(
                "",
                runResult LineElement.Parser.pimage "![](https://example.com/image.png)",
                Ok {|
                    Alt = ""
                    Src = "https://example.com/image.png"
                    Title = None
                |}
            )
        testCase "![](https://example.com/image.png \"title\")" <| fun () ->
            Assert.Equal(
                "",
                runResult LineElement.Parser.pimage "![](https://example.com/image.png \"title\")",
                Ok {|
                    Alt = ""
                    Src = "https://example.com/image.png"
                    Title = Some "title"
                |}
            )
        testCase "![image alt](https://example.com/image.png \"title\")" <| fun () ->
            Assert.Equal(
                "",
                runResult LineElement.Parser.pimage "![image alt](https://example.com/image.png \"title\")",
                Ok {|
                    Alt = "image alt"
                    Src = "https://example.com/image.png"
                    Title = Some "title"
                |}
            )
    ]

[<Tests>]
let documentTests =
    testList "documentTests" [
        testCase "base" <| fun () ->
            let document : Document =
                [
                    h1 [ text "Tales of "; comment "inline commentary"; italic (bold ( text "formatting" )) ] [
                        p [
                            [ text "Paragraph one." ]
                            [ text "A sentence with "; italic (text "a soft"); text " line break." ]
                        ]

                        p [[ text "Paragraph two." ]]

                        comment "Commentary between statements."

                        ul [
                            li [ text "Unordered item one" ] [
                                p [[ text "Item text"]]
                            ]

                            li [ text "Item "; italic (text "two") ] [
                                ol [
                                    li [ text "Ordered item one" ] [
                                        comment "Commentary between statements."
                                    ]
                                ]
                            ]
                        ]

                        p [[ text "Paragraph three." ]]

                        h2 [ text "Chapter 1" ] [
                            p [[ text "Contents of chapter one." ]]

                            p [
                                [ url "https://example.com" "best site" [ text "A link that leads to the ";  italic (text "best"); text " site" ] ]
                                [ img "https://example.com/image.png" "title" "alt" ]
                            ]
                        ]
                    ]
                ]

            let exp =
                [
                    "# Tales of <!-- inline commentary -->***formatting***"
                    "Paragraph one."
                    "A sentence with *a soft* line break."
                    ""
                    "Paragraph two."
                    ""
                    "<!-- Commentary between statements. -->"
                    ""
                    "* Unordered item one"
                    "    Item text"
                    "* Item *two*"
                    "    1. Ordered item one"
                    "        <!-- Commentary between statements. -->"
                    ""
                    "Paragraph three."
                    ""
                    "## Chapter 1"
                    "Contents of chapter one."
                    ""
                    "[A link that leads to the *best* site](https://example.com \"best site\")"
                    "![alt](https://example.com/image.png \"title\")"
                ] |> String.concat "\n"

            let act =
                Document.serialize 4 document

            Assert.Equal("", exp, act)
    ]
