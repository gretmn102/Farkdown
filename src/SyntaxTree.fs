namespace rec Farkdown.SyntaxTree
open FsharpMyExtension

module CommonParser =
    open FParsec

    type Parser<'a> = Parser<'a, unit>

[<RequireQualifiedAccess>]
type LineElement =
    | Bold of LineElement
    | Italic of LineElement
    | Strikeout of LineElement
    | Underline of LineElement
    | Text of string
    | Comment of string
    /// ```markdown
    /// [description](https://example.com "Title of image")
    /// ```
    | Url of href: string * title: string * description: LineElement list
    /// ```markdown
    /// ![Alt-text](https://example.com/image.png "Title of image")
    /// `````
    | Image of src: string * title: string * alt: string

    static member MakeComment s =
        Comment s

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module LineElement =
    module Show =
        open FsharpMyExtension.ShowList

        let showComment comment =
            showString "<!-- " << showString comment << showString " -->"

        let rec show (showLine: Line -> ShowS) (line: LineElement) : ShowS =
            let f quot lineElement : ShowS =
                quot << show showLine lineElement << quot

            match line with
            | LineElement.Text s ->
                showString s

            | LineElement.Bold(lineElement) ->
                f (showString "**") lineElement

            | LineElement.Italic(lineElement) ->
                f (showString "*") lineElement

            | LineElement.Strikeout(lineElement) ->
                f (showString "~~") lineElement

            | LineElement.Underline(lineElement) ->
                f (showString "__") lineElement

            | LineElement.Url(href, title, description) ->
                let title =
                    if title = "" then
                        empty
                    else
                        showChar ' ' << showAutoParen "\"" (showString title)

                let url =
                    showAutoParen "(" (showString href << title)

                showAutoParen "[" (showLine description) << url

            | LineElement.Image(src, title, alt) ->
                let title =
                    if title = "" then
                        empty
                    else
                        showChar ' ' << showAutoParen "\"" (showString title)

                showChar '!'
                << showChar '[' << showString alt << showChar ']'
                << showAutoParen "(" (showString src << title)

            | LineElement.Comment comment ->
                showComment comment

    module Parser =
        open FParsec

        open CommonParser

        let pimage: Parser<_> =
            pchar '!'
            >>. pipe2
                (between (pchar '[') (pchar ']') (
                    manySatisfy ((<>) ']')
                ))
                (between (pchar '(') (pchar ')') (
                    let ptitle =
                        between (pchar '"') (pchar '"') (manySatisfy ((<>) '"'))
                    manySatisfy (fun c ->
                        not (c = ')' || System.Char.IsWhiteSpace c)
                    )
                    .>> spaces .>>. opt ptitle
                ))
                (fun alt (src, title) ->
                    {|
                        Alt = alt
                        Src = src
                        Title = title
                    |}
                )

type Line = LineElement list

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module Line =
    module Show =
        open FsharpMyExtension.ShowList

        let rec show (line: Line) : ShowS =
            line
            |> List.map (LineElement.Show.show show)
            |> joinsEmpty empty

[<RequireQualifiedAccess>]
type ListItem = Line * Statement list

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module ListItem =
    module Show =
        open FsharpMyExtension.ShowList

        let show (marker: ShowS) (indent: ShowS) (showStatements: _ -> ShowS list) (listItem: ListItem) : ShowS list =
            let line, statements = listItem

            let item =
                marker << showSpace << Line.Show.show line
            let body =
                showStatements statements |> List.map ((<<) indent)
            item::body

[<RequireQualifiedAccess>]
type Statement =
    | Header of level: int * Line * Statement list
    | Paragraph of Line list
    | Comment of string
    | List of isOrdered: bool * items: ListItem list

    static member MakeComment s =
        Comment s

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module Statement =
    module Show =
        open FsharpMyExtension.ShowList

        let show indent showStatements (x: Statement) : ShowS list =
            match x with
            | Statement.Paragraph lines ->
                lines
                |> List.map Line.Show.show

            | Statement.List(isOrdered, items) ->
                let xs =
                    let showList marker item =
                        ListItem.Show.show marker indent showStatements item

                    if isOrdered then
                        items
                        |> List.mapi (fun i item ->
                            showList (shows (i + 1) << showChar '.') item
                        )
                    else
                        let bullet = showChar '*'
                        items
                        |> List.map (fun xs ->
                            showList bullet xs
                        )

                xs |> List.concat

            | Statement.Header(level, line, statements) ->
                let header =
                    replicate level '#' << showChar ' ' << Line.Show.show line
                let body =
                    showStatements statements
                header::body

            | Statement.Comment comment ->
                LineElement.Show.showComment comment
                |> List.singleton

type Document = Statement list

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module Document =
    module Show =
        open FsharpMyExtension.ShowList

        let rec show indent (document: Document) : ShowS list =
            document
            |> List.map (fun xs ->
                Statement.Show.show indent (show indent) xs
            )
            |> List.sepBy [empty]
            |> List.concat

        let createSpacesIndentSize size =
            replicate size ' '

    let serialize (spacesIndentSize: int) (document: Document) =
        let indent = Show.createSpacesIndentSize spacesIndentSize
        Show.show indent document
        |> ShowList.joinEmpty "\n"
        |> ShowList.show
