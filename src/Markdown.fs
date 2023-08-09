module Farkdown.Markdown
open FsharpMyExtension
open FsharpMyExtension.Either

type FontStyle =
    | Italic
    | Bold
    | Strikeout
    | Underline
type Inline =
    | WithFontStyle of FontStyle * Inline list
    | Text of string
    /// `[Текст ссылки](адрес://ссылки.здесь "Заголовок ссылки")`
    | Url of description:Inline list * href:string * alt:string
    /// `![Alt-текст](адрес://ссылки.здесь "Заголовок изображения")`
    | Img of alt:string * src:string * title:string
    /// Сюда входят стили, которые не удалось поместить во всё остальное.
    | Span of style:string * body:Inline list
/// По-идеи, сами не имеют знаков переносов.
/// Между ними — `<br>`, он же "мягкий" перенос.
type Inlines = Inline list
type FlowContent =
    | Custom of name:string * attributes:(string * string) list * body:FlowContent list
    | Paragraph of Inlines list
    | JustInlines of Inlines list
    | HtmlList of ordered:bool * list<FlowContent list>
    | Header of int * Inlines

let formatInlines (xs:Inlines) =
    let rec f (xs:Inlines) =
        let replace (str:string) =
            str.Replace('\n', ' ')
        xs
        |> List.map (function
            | Text x ->
                Text(replace x)
            | WithFontStyle(fontStyle, body) ->
                WithFontStyle(fontStyle, f body)
            | Url(description, href, alt) ->
                Url(f description, href, alt)
            | Img(alt, src, title) as x -> x
            | Span(style, body) ->
                Span(style, f body)
        )
    let first =
        let rec f = function
            | x::xs ->
                match x with
                | Text x ->
                    Text(x.TrimStart())
                | WithFontStyle(fontStyle, body) ->
                    WithFontStyle(fontStyle, f body)
                | Url(description, href, alt) ->
                    Url(f description, href, alt)
                | Img(alt, src, title) as x -> x
                | Span(style, body) ->
                    Span(style, f body)
                |> fun x -> x::xs
            | [] -> [] // TODO: надо взять соседний узел
        f
    let last =
        let mapLast fn =
            let rec f acc = function
                | [x] -> fn x::acc |> List.rev
                | x::xs -> f (x::acc) xs
                | [] -> List.rev acc // TODO: надо взять предыдущий узел
            f []
        let rec f xs =
            xs
            |> mapLast (fun x ->
                match x with
                | Text x ->
                    Text(x.TrimEnd())
                | WithFontStyle(fontStyle, body) ->
                    WithFontStyle(fontStyle, f body)
                | Url(description, href, alt) ->
                    Url(f description, href, alt)
                | Img(alt, src, title) as x -> x
                | Span(style, body) ->
                    Span(style, f body)
            )
        f

    xs
    |> f
    |> first
    |> last

let formatLine =
    let rec f = function
        | FlowContent.JustInlines xss ->
            List.map formatInlines xss
            |> FlowContent.JustInlines
        | FlowContent.Paragraph xss ->
            List.map formatInlines xss
            |> FlowContent.Paragraph
        | FlowContent.Custom(name, attributes, body) ->
            FlowContent.Custom(name, attributes, List.map f body)
        | FlowContent.HtmlList(ordered, x) ->
            FlowContent.HtmlList(ordered, List.map (List.map f) x)
        | FlowContent.Header(h, xss) ->
            FlowContent.Header(h, formatInlines xss)
    f

module HtmlToMarkdown =
    open FsharpMyExtension
    open FsharpMyExtension.HtmlAgilityPackExt
    open Parser.Primitives
    open Parser.ParHtmlNode2
    type Parser<'a> = Pars<HtmlAgilityPack.HtmlNode, 'a, unit>

    let rec pinlines : Parser<Inline list> =
        let (p:Parser<Inline list list>), pref = createParserForwardedToRef()
        let p' =
            p .>> eof "pinlines" |>> List.concat
        let pstrong =
            takeN "strong" <|> takeN "b"
            >>@
                p' |>> fun body ->
                    Inline.WithFontStyle(Bold, body)
        let pem =
            takeN "em" <|> takeN "i"
            >>= fun body ->
                sub p' (body, ()) |>> fun body ->
                    Inline.WithFontStyle(Italic, body)
        let punderline =
            takeN "u"
            >>= fun body ->
                sub p' (body, ()) |>> fun body ->
                    Inline.WithFontStyle(Underline, body)
        let pa =
            takeN "a"
            >>= fun node ->
                sub p' (node, ()) |>> fun body ->
                    let f x =
                        HtmlNode.tryGetAttVal x node
                        |> Option.defaultValue ""
                    Url(body, f "href", f "title")
        let pimg =
            takeN "img"
            |>> fun node ->
                let f x =
                    HtmlNode.tryGetAttVal x node
                    |> Option.defaultValue ""
                Img(f "alt", f "src", f "title")
        let ptext =
            satisfym (fun (node:HtmlAgilityPack.HtmlNode) ->
                if node.NodeType = HtmlAgilityPack.HtmlNodeType.Text then
                    node.InnerText |> Some
                else None)
                "text"
            |>> Text
        let pcomment =
            satisfym (fun (node:HtmlAgilityPack.HtmlNode) ->
                if node.NodeType = HtmlAgilityPack.HtmlNodeType.Comment then
                    node.InnerText |> Some
                else None
            ) "<--comment-->"
        /// > A <span> element used to color a part of a text
        ///
        /// Конечно, в стиле можно задать модификации шрифта (например, так: <span style="color:darkolivegreen;font-weight:bold">), потому их тоже надо бы учесть...
        let pspan =
            takeN "span"
            >>= fun node ->
                sub p' (node, ()) |>> fun body ->
                    match HtmlNode.tryGetAttVal "style" node with
                    | Some v ->
                        [Inline.Span(v, body)]
                    | None ->
                        body
        let xs : Parser<_> =
            choice [
                pstrong |>> List.singleton
                pem |>> List.singleton
                punderline |>> List.singleton
                pa |>> List.singleton
                pimg |>> List.singleton
                ptext |>> List.singleton
                pspan
                pcomment >>% []
            ]
        pref := many1 xs

        p
        |>> List.concat
        |>> formatInlines // TODO: Вот это всё безобразие нужно на стадии парсинга сделать

    let p2 : Parser<_> =
        let takeN name = takeN name .>> ws
        let (p2:Parser<_>), p2ref = createParserForwardedToRef()
        let pheader =
            // let n = 6
            // List.init n ((+) 1 >> sprintf "takeN \"h%d\"") |> String.concat " <|> "
            // List.init n ((+) 1 >> sprintf "<h%d>") |> String.concat " or "
            takeN "h1" <|> takeN "h2" <|> takeN "h3" <|> takeN "h4" <|> takeN "h5" <|> takeN "h6"
            >>= fun node ->
                let n = int (string node.Name.[1])
                sub pinlines (node, ())
                |>> fun xs -> Header(n, xs)

        let pbr = takeN "br"
        let pinliness1 =
            // TODO: раскомментировать, когда форматирование до ума доведу:
            // many1 pbr >>. many (pinlines .>> many pbr)
            // <|> many1 (pinlines .>> many pbr)
            many1 (pinlines <|> (pbr >>% []))
        let pinliness =
            many pbr
            >>. many (pinlines .>> many pbr)
        let pparagraph =
            takeN "aside" <|> takeN "p"
            >>= fun node -> sub (pinliness .>> eof "pinliness") (node, ()) |>> Paragraph

        let plist =
            let plistItem =
                takeN "li"
                >>= fun xs ->
                    sub (many p2 |>> List.concat .>> eof "eof") (xs, ())
            takeN "ul" <|> takeN "ol"
            >>= fun node ->
                let typ = node.Name
                let isOrdered =
                    match typ with
                    | "ol" -> true
                    | _ -> false
                let p' =
                    many plistItem
                    |>> fun ys -> HtmlList(isOrdered, ys)
                sub (p' .>> eof "eof") (node, ())

        let custom =
            satisfy (fun (node:HtmlAgilityPack.HtmlNode) ->
                node.NodeType = HtmlAgilityPack.HtmlNodeType.Element
            ) "node.NodeType = HtmlAgilityPack.HtmlNodeType.Element"
            >>= fun node ->
                sub (many p2 .>> eof "eof") (node, ())
                |>> fun xs ->
                    let atts =
                        node.Attributes
                        |> Seq.map (fun x -> x.Name, x.Value )
                        |> List.ofSeq
                    Custom(node.Name, atts, List.concat xs)

        p2ref :=
            choice [
                pinliness1 |>> (JustInlines >> List.singleton)
                plist |>> List.singleton
                pparagraph |>> List.singleton
                pheader |>> List.singleton
                custom |>> List.singleton
            ]
        p2
    let start2 xs =
        run (ws >>. many p2 |>> List.concat .>> eof "p2 or eof") xs

module Show =
    open FsharpMyExtension.ShowList
    let rec showInline = function
        | Inline.WithFontStyle(typ, xs) ->
            let quot =
                match typ with
                | Underline -> "__"
                | Italic -> "*"
                | Bold -> "**"
                | Strikeout -> "~~"
            let xs = List.collect showInline xs
            showString quot << joinEmpty "" xs << showString quot
            |> List.singleton
        | Inline.Text x -> showString x |> List.singleton
        | Inline.Url(description, href, alt) ->
            let x =
                if alt = "" then
                    empty
                else
                    showChar ' ' << showString alt
            let xs = List.collect showInline description
            showChar '[' << joinEmpty "" xs << showChar ']'
            << showAutoParen "(" (showString href << x)
            |> List.singleton
        | Inline.Img(alt, src, title) ->
            let x =
                if alt = "" then
                    empty
                else
                    showChar ' ' << showString title
            showChar '!'
            << showChar '[' << showString alt << showChar ']'
            << showAutoParen "(" (showString src << x)
            |> List.singleton
        | Inline.Span(style, body) ->
            showString "<span style='" << showString style << showString "'>"
            << joinEmpty "" (List.collect showInline body)
            << showString "</span>"
            |> List.singleton

    let showInlines xs =
        xs
        |> List.map (showInline >> joinEmpty "")
        |> joinEmpty "" : ShowS
    let rec show = function
        | FlowContent.JustInlines xss ->
            List.map showInlines xss
            : ShowS list
        | FlowContent.Paragraph xss ->
            List.map showInlines xss @ [empty]
            : ShowS list
        | FlowContent.HtmlList(ordered, items) ->
            let tab = showString "  "
            let xs =
                let f x =
                    match x with
                    | FlowContent.HtmlList(_) ->
                        show x
                        |> List.map (fun x -> tab << x)
                    | _ ->
                        show x
                        |> List.map (fun x -> showChar ' ' << x)
                if ordered then
                    items
                    |> List.mapi (fun i xs ->
                        match xs with
                        | x::xs ->
                            match f x with
                            | x::ys ->
                                let first = shows (i + 1) << x
                                let rest = List.collect f xs
                                first::(ys@rest)
                            | xs ->

                                failwithf "error in ordered:\n%A" xs
                        | [] -> []
                    )
                else
                    items
                    |> List.map (fun xs ->
                        match xs with
                        | x::xs ->
                            match f x with
                            | x::ys ->
                                let first = showChar '*' << x
                                let rest = List.collect f xs
                                first::(ys@rest)
                            | xs ->

                                failwithf "error in non-ordered:\n%A" xs
                        | [] -> []
                    )
            xs |> List.concat

        | FlowContent.Header(n, inlines) ->
            let inlines =
                inlines
                |> List.map (showInline >> joinEmpty "")
                |> joinEmpty ""
            replicate n '#' << showChar ' ' << inlines
            |> List.singleton
        | FlowContent.Custom(name, attributes, body) ->
            let atts =
                if List.isEmpty attributes then empty
                else
                    let atts =
                        attributes
                        |> List.map (fun (name, value) ->
                            showString name << showChar '='
                            << showAutoParen "'" (showString value) )
                        |> join " "
                    showChar ' ' << atts
            showChar '<' << showString name << atts << showChar '>'
            << joinEmpty "" (List.collect show body)
            << showChar '<' << showChar '/' << showString name << showChar '>'
            |> List.singleton
    let print =
        List.collect show
        >> FsharpMyExtension.ShowList.joinsEmpty FsharpMyExtension.ShowList.nl
        >> FsharpMyExtension.ShowList.show
