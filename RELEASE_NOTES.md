## 0.5.0
* feat: add a `Image` type that uses `LineElement.Image` (#28)
* feat: add a `Link` type that uses `LineElement.Link` (#27)
* feat: add `Link.Parser.plink` (#24)
* refactor!: rename `Url` in `LineElement` to `Link` (#22)
* feat: add `Document.parse` (#19)
* feat: add `Header` type, that uses `Statement` (#18)
* chore: remove `HtmlAgilityPack`, `Fuchu` from dependencies
* chore: set dependency versions

## 0.4.0
* feat!: set the experimental types as the main
* feat: write simple parser

## 0.3.0
* feat!: remove `HtmlToMarkdown`
* feat!: add to `Experimental.SyntaxTree.Statement` `RequireQualifiedAccess`
* feat: add to `Experimental.SyntaxTree.Statement` module suffix

## 0.2.0
* add netstandard2.0 target framework
* add Fable
* breaking: make `FontStyle` require qualified access
* breaking: move `formatInlines` to `Inlines.format`
* breaking: move `formatLine` to `FlowContent.format`
* breaking: make `FlowContent` require qualified access
* breaking: move `Show.showInline` to `Inline.Show.show`
* breaking: make `Inline` require qualified access
* breaking: move `Show.showInlines` to `Inlines.Show.show`
* breaking: move `Show.show` to `FlowContent.Show.show`
* add `Document` type
* breaking: move `Show.print` to `Document.serialize`

## 0.1.0
* init
