# V

## The goals

* Have a fun project where I "can" use its results on a daily basis and, hopefully one day, might prefer it to other alternatives.
* Build a text editor similar to vi (modal editting) where:
    * It relies on the keyboard as main input method and terminal emulators as UI.
    * It comes with a basic set of software development features out of the box [like Helix](https://helix-editor.com/)
    * It focuses on discoverability and ergonomy but keeping the familiar vi MO.

## First design principles

* Functionality should be easy to add through some sort of functional composition.
* One should be able to go back and forward in "time" given a set of inputs/signals.
    * I will try to use a State, Reducer and View model. Akin to the Elm architecture, using [brick](https://hackage.haskell.org/package/brick) as a starting point.
* I don't want to reimplement language parsers nor functionality provided by LSP servers, formatters, linters, etc...
    * LSP and language parsing (most likely Tree-sitter) support will be built-in. [This is an interesting read](https://github.com/microsoft/vscode/issues/50140)
* It should not require a Haskell development environment to be used for other languages.
    * A plugin system is not an initial goal, but we should keep this in mind for installing parsers, and any sort of extension.
* Solid types first, performance second.
    * Hopefuly we can easily switch some implementations around as well as leverage Haskell concurrency and paralelism libraries to optimize after the fact.

## First design questions

* What type would be good to represent buffers? Having some initial sketches with a very naive underlying data-struct is one of the first things to better understand the problem space.
* What [data structure](https://iq.opengenus.org/data-structures-used-in-text-editor/) we can use to represent buffers?
    * We have a few [ropes libraries](https://hackage.haskell.org/packages/search?terms=ropes)
    * What are the trade-offs between ropes, vectors and linked-lists?
    * Helix, [yi](https://github.com/yi-editor/yi) and [xi](https://xi-editor.io/) use ropes.
    * Interesting read about the [choice of data structure for VSCode](https://code.visualstudio.com/blogs/2018/03/23/text-buffer-reimplementation)
    * Another interesting read on the topic of [neovim's data structure](https://github.com/neovim/neovim/discussions/25647)
    * A [detailed description of Emacs buffer representation](https://www.gnu.org/software/emacs/manual/html_node/elisp/Buffer-Internals.html)
* Is Tree-sitter the way to go for parsing? Are there alternatives with less visibility that might be more suited for small projects?
* How extensible is [brick](https://hackage.haskell.org/package/brick) and how easy is to implement:
    * Multi-window view (both tiled and floating)
    * Configurable colorful syntax highlighting and text markings (virtual lines are a nice to have)

## Notes for implementation

* [Cross platform library to monitor file changes](https://hackage.haskell.org/package/fsnotif)
* [Discussion on highlighting text in Brick](https://github.com/jtdaugherty/brick/issues/400)
