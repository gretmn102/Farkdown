module Tests.Experimental
open Fuchu
open Farkdown.Experimental.SyntaxTree
open Farkdown.Experimental.Helpers

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
