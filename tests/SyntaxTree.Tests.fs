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
let ``LineElement.Parser.ptext`` =
    testList "LineElement.Parser.ptext" [
        testCase "empty" <| fun () ->
            Assert.Equal(
                "",
                runResult LineElement.Parser.ptext "",
                Error (String.concat System.Environment.NewLine [
                    "Error in Ln: 1 Col: 1"
                    "Note: The error occurred at the end of the input stream."
                    "Expecting: text"
                    ""
                ])
            )
        testCase "!" <| fun () ->
            Assert.Equal(
                "",
                runResult LineElement.Parser.ptext "!",
                Ok "!"
            )
        testCase "![" <| fun () ->
            Assert.Equal(
                "",
                runResult LineElement.Parser.ptext "![",
                Error (String.concat System.Environment.NewLine [
                    "Error in Ln: 1 Col: 1"
                    "!["
                    "^"
                    "Expecting: text"
                    ""
                ])
            )
        testCase "!abc" <| fun () ->
            Assert.Equal(
                "",
                runResult LineElement.Parser.ptext "!abc",
                Ok "!abc"
            )
        testCase "abc!" <| fun () ->
            Assert.Equal(
                "",
                runResult LineElement.Parser.ptext "abc!",
                Ok "abc!"
            )
        testCase "abc![" <| fun () ->
            Assert.Equal(
                "",
                runResult LineElement.Parser.ptext "abc![",
                Ok "abc"
            )
    ]

[<Tests>]
let ``Line.Parser.parse`` =
    testList "Line.Parser.parse" [
        testCase "empty" <| fun () ->
            Assert.Equal(
                "",
                runResult Line.Parser.parse "",
                Error (String.concat System.Environment.NewLine [
                    "Error in Ln: 1 Col: 1"
                    "Note: The error occurred at the end of the input stream."
                    "Expecting: not * <spaces> or # in line, text or '!'"
                    ""
                ])
            )
        testCase "first line" <| fun () ->
            Assert.Equal(
                "",
                runResult Line.Parser.parse "first line",
                Ok [
                    LineElement.Text "first line"
                ]
            )
        testCase "first line\\nsecond line" <| fun () ->
            Assert.Equal(
                "",
                runResult Line.Parser.parse "first line\nsecond line",
                Ok [
                    LineElement.Text "first line"
                ]
            )
        testCase "* list item" <| fun () ->
            Assert.Equal(
                "",
                Error (String.concat System.Environment.NewLine [
                    "Error in Ln: 1 Col: 1"
                    "* list item"
                    "^"
                    "Expecting: not * <spaces> or # in line"
                    ""
                ]),
                runResult Line.Parser.parse "* list item"
            )
        testCase "*italic*" <| fun () ->
            Assert.Equal(
                "",
                Ok [
                    LineElement.Text "*italic*"
                ],
                runResult Line.Parser.parse "*italic*"
            )
        testCase "#Header" <| fun () ->
            Assert.Equal(
                "",
                Error (String.concat System.Environment.NewLine [
                    "Error in Ln: 1 Col: 1"
                    "#Header"
                    "^"
                    "Expecting: not * <spaces> or # in line"
                    ""
                ]),
                runResult Line.Parser.parse "#Header"
            )
        testCase "# Header" <| fun () ->
            Assert.Equal(
                "",
                Error (String.concat System.Environment.NewLine [
                    "Error in Ln: 1 Col: 1"
                    "# Header"
                    "^"
                    "Expecting: not * <spaces> or # in line"
                    ""
                ]),
                runResult Line.Parser.parse "# Header"
            )
    ]


[<Tests>]
let ``Statement.Parser.pheader`` =
    let parser = Statement.Parser.pheader (FParsec.Primitives.preturn [])
    testList "Statement.Parser.pheader" [
        testCase "empty" <| fun () ->
            Assert.Equal(
                "",
                runResult parser "",
                Error (String.concat System.Environment.NewLine [
                    "Error in Ln: 1 Col: 1"
                    "Note: The error occurred at the end of the input stream."
                    "Expecting: one or more #"
                    ""
                ])
            )
        testCase "lorem ipsum" <| fun () ->
            Assert.Equal(
                "",
                runResult parser "lorem ipsum",
                Error (String.concat System.Environment.NewLine [
                    "Error in Ln: 1 Col: 1"
                    "lorem ipsum"
                    "^"
                    "Expecting: one or more #"
                    ""
                ])
            )
        testCase "#Header" <| fun () ->
            Assert.Equal(
                "",
                runResult parser "#Header",
                Ok {|
                    Level = 1
                    Line = [
                        LineElement.Text "Header"
                    ]
                    Body = []
                |}
            )
        testCase "# Header" <| fun () ->
            Assert.Equal(
                "",
                runResult parser "# Header",
                Ok {|
                    Level = 1
                    Line = [
                        LineElement.Text "Header"
                    ]
                    Body = []
                |}
            )
        testCase "## Header" <| fun () ->
            Assert.Equal(
                "",
                runResult parser "## Header",
                Ok {|
                    Level = 2
                    Line = [
                        LineElement.Text "Header"
                    ]
                    Body = []
                |}
            )
    ]

[<Tests>]
let ``Statement.Parser.pparagraph`` =
    let parser = Statement.Parser.pparagraph
    testList "Statement.Parser.pparagraph" [
        testCase "empty" <| fun () ->
            Assert.Equal(
                "",
                Error (String.concat System.Environment.NewLine [
                    "Error in Ln: 1 Col: 1"
                    "Note: The error occurred at the end of the input stream."
                    "Expecting: not * <spaces> or # in line, text or '!'"
                    ""
                ]),
                runResult parser ""
            )
        testCase "Hello" <| fun () ->
            Assert.Equal(
                "",
                runResult parser "Hello",
                Ok [[LineElement.Text "Hello"]]
            )
        testCase "First line\\nSecond line" <| fun () ->
            Assert.Equal(
                "",
                runResult parser "First line\nSecond line",
                Ok [
                    [LineElement.Text "First line"]
                    [LineElement.Text "Second line"]
                ]
            )
        testCase "First line\\n" <| fun () ->
            Assert.Equal(
                "",
                runResult parser "First line\n",
                Ok [
                    [LineElement.Text "First line"]
                ]
            )
        testCase "\\n" <| fun () ->
            Assert.Equal(
                "",
                Error (String.concat System.Environment.NewLine [
                    "Error in Ln: 1 Col: 1"
                    "Note: The error occurred on an empty line."
                    "Expecting: not * <spaces> or # in line, text or '!'"
                    ""
                ]),
                runResult parser "\n"
            )
        testCase "\\nsecond line" <| fun () ->
            Assert.Equal(
                "",
                Error (String.concat System.Environment.NewLine [
                    "Error in Ln: 1 Col: 1"
                    "Note: The error occurred on an empty line."
                    "Expecting: not * <spaces> or # in line, text or '!'"
                    ""
                ]),
                runResult parser "\nsecond line"
            )
    ]

[<Tests>]
let ``Statement.Parser.punorderedList`` =
    let parser = Statement.Parser.punorderedList
    testList "Statement.Parser.punorderedList" [
        testCase "just text" <| fun () ->
            Assert.Equal(
                "",
                Error (String.concat System.Environment.NewLine [
                    "Error in Ln: 1 Col: 1"
                    "just text"
                    "^"
                    "Expecting: '*'"
                    ""
                ]),
                runResult parser "just text"
            )
        testCase "* First item\\n* Second item" <| fun () ->
            Assert.Equal(
                "",
                Ok [
                    [LineElement.Text "First item"], []
                    [LineElement.Text "Second item"], []
                ],
                runResult parser "* First item\n* Second item"
            )
        testCase "* First item\\n\\n* Second item" <| fun () ->
            Assert.Equal(
                "",
                Ok [
                    [LineElement.Text "First item"], []
                    [LineElement.Text "Second item"], []
                ],
                runResult parser "* First item\n\n* Second item"
            )
        testCase "* First item\\nSecond line" <| fun () ->
            Assert.Equal(
                "",
                Ok [
                    [LineElement.Text "First item"], []
                ],
                runResult parser "* First item\nSecond line"
            )
    ]

[<Tests>]
let ``ListItem.Parser.parse`` =
    let parser = ListItem.Parser.parse
    testList "ListItem.Parser.parse" [
        testCase "*item" <| fun () ->
            Assert.Equal(
                "",
                Error (String.concat System.Environment.NewLine [
                    "Error in Ln: 1 Col: 1"
                    "*item"
                    "^"
                    ""
                    "The parser backtracked after:"
                    "  Error in Ln: 1 Col: 2"
                    "  *item"
                    "   ^"
                    "  Expecting: whitespace"
                    ""
                ]),
                runResult parser "*item"
            )
        testCase "* item" <| fun () ->
            Assert.Equal(
                "",
                Ok (
                    [LineElement.Text "item"], []
                ),
                runResult parser "* item"
            )
    ]

[<Tests>]
let ``Statement.Parser.parse`` =
    let parser = Statement.Parser.parse (FParsec.Primitives.preturn [])
    testList "Statement.Parser.parse" [
        testCase "# header" <| fun () ->
            Assert.Equal(
                "",
                Ok (
                    Statement.Header(1, [ LineElement.Text "header" ], [])
                ),
                runResult parser "# header"
            )
        testCase "# header\\ntext" <| fun () ->
            Assert.Equal(
                "",
                Ok (
                    Statement.Header(1, [ LineElement.Text "header" ], [])
                ),
                runResult
                    (
                        FParsec.Primitives.(.>>)
                            parser
                            (FParsec.CharParsers.pstring "text")
                    )
                    "# header\ntext"
            )
        testCase "# header\\n\\ntext" <| fun () ->
            Assert.Equal(
                "",
                Ok (
                    Statement.Header(1, [ LineElement.Text "header" ], [])
                ),
                runResult
                    (
                        FParsec.Primitives.(.>>)
                            parser
                            (FParsec.CharParsers.pstring "text")
                    )
                    "# header\n\ntext"
            )
        testCase "* list item" <| fun () ->
            Assert.Equal(
                "",
                Ok (
                    Statement.List(false, [
                        [LineElement.Text "list item"], []
                    ])
                ),
                runResult parser "* list item"
            )
        testCase "just text" <| fun () ->
            Assert.Equal(
                "",
                Ok (
                    Statement.Paragraph [
                        [ LineElement.Text "just text" ]
                    ]
                ),
                runResult parser "just text"
            )
        testCase "1.1\\n1.2\\n\\n2.1" <| fun () ->
            Assert.Equal(
                "",
                Ok [
                    Statement.Paragraph [
                        [ LineElement.Text "1.1" ]
                        [ LineElement.Text "1.2" ]
                    ]
                    Statement.Paragraph [
                        [ LineElement.Text "2.1" ]
                    ]
                ],
                runResult (FParsec.Primitives.many parser) "1.1\n1.2\n\n2.1"
            )
    ]

let document: Document =
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

let rawDocument =
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


[<Tests>]
let ``Document.Parser.parse`` =
    let parser = Document.Parser.parse
    testList "Document.Parser.parse" [
        testCase "base" <| fun () ->
            Assert.Equal(
                "",
                Ok [],
                runResult parser rawDocument
            )
    ]

[<Tests>]
let documentTests =
    testList "documentTests" [
        testCase "base" <| fun () ->
            let act =
                Document.serialize 4 document

            Assert.Equal("", rawDocument, act)
    ]
