# .emacs.d

This is my attempt of keeping a consistent editor configuration across systems.

## Sources

Here are a couple of good sources

- [emacsrocks (magnars)](https://github.com/magnars/.emacs.d/)
- [emacs4developers](https://github.com/pierre-lecocq/emacs.d)
- [Parens of the Dead](https://www.parens-of-the-dead.com/)

## Init cascade

Init files (if existing) are loaded in this order:

- `~/.emacs.d/init.el`
- `~/.emacs.d/custom.el` - contains default customizations
- `~/.emacs.d/custom-local.el` - is set as `custom-file`
- `~/.emacs.d/init-local.el` - setting for the local system
- `./.emacs-project.el` - settings for the project
- `./.emacs-local.el` - local setting for the project (add to .gitignore)


## Commands

All commands that I'm actually using.

### Keybound

#### Basic

Invoked without a prefix.

Pattern:
- `C-KEY` does a thing
- `M-KEY` does that thing "bigger"
- `s-KEY` does it even "bigger"
- `S-` does it in the other direction

#### Editing

- adding
    - `CHAR` insert char at point
    - `RET` add new line at point
    - `C-RET` add new line below current line
    - `S-C-RET` add new line above current line
- removing
    - `C-d` delete previous char
    - `S-C-d` delete next char
    - `M-d` kill previous word
    - `S-M-d` kill next word
    - `C-M-d` kill previous S-expression
    - `S-C-M-d` kill next S-expression
    - `C-k` kill until end of line
    - `S-C-k` kill until beginning of line
- kill ring
    - `M-w` copy region
    - `C-w` kill region
    - `S-C-w` kill region and extend ring
    - `S-M-w` copy region and extend ring
    - `C-y` yank last in ring
    - `M-y` yank next in ring
    - show kill ring
- undoing
    - undo
    - redo
    - show undo tree
- moving
    - `TAB` indent current line

#### Selection

#### Buffer Navigation

### Extendend

Invoked with prefix `C-x`

- `g` show Magit status window

### Other

Invoked with `M-x`


## Cheat sheet

- basics
    - exit `C-x C-c`
    - open/create file `C-x C-f`
    - save file `C-x C-s`
    - close buffer `C-x k`
- windows
    - split right `C-x 3`
    - split down `C-x 2`
    - next window `C-x o`
    - close current window `C-x 0`
    - close other windows `C-x 1`
    - move buffer `C-s-ARROW`
    - back/forward windows configs `C-x ARROW`
- file management
    - jump to project root `C-x C-j`
    - create folder `+`
    - delete file `D`
- text manipulation
    - indent region
        - interactively `C-x TAB`
        - by 2 space in and out
    - duplicate lines `C-M-UP/DOWN`
    - move line `M-UP/DOWN`
    - kill line `C-k C-k`
    - search and replace `M-%`
        - with regex `C-M-%`
        - capture groups `\(.+\)` > `\1`
    - comment lines `C-x /`
- cursor
    - multi cursor
        - per line in region `C-c c`
        - next like this or line `C-c n/p`
        - next symbol `C-c f/b`
        - all symbols `C-c a`
    - expand region `C-c g`
- navigate in buffer
    - collapse everything `C-c h H`
    - expand next level `C-c h l`
    - expand block `C-c h s`
    - search for string `C-s` `C-r`
    - jump to char `C-c SPC`
- navigate in project
    - search file `C-x p f`
        - including line numbers
    - full-text search `C-x p g`
- terminal
    - open shell `M-x term|shell|eshell`
    - goto error line
- execution
    - eval at point `C-x C-e`
    - eval `M-:`
    - repeat command `C-x z`
- git `C-x g`
- refactoring
- snippets
    - class
    - method
    - constructur
    - this/self
    - test

## To explore

- projectile
- yasnippet
- dump-jump (cross plattform?)

## Still to figure out

- map modifiers on Mac to [C], [s], [M]
- speed up init time with use-package
- setup snippets with yasnippet
- projects (check out projectile)
    - open project
    - load project init file
    - have multiple projects open
- shortcut to restore tests in side window
- register a window configuration
    - for the session?
    - for the project?
- indent multiple lines
- jump to error line
- refactoring
    - javascript
    - python
- quickly find methods of type
    - auto-complete for known type
- format buffer
    - javascript
    - python (using local config as in pre-commit hook)
    - JSON `M-x js-json-mode RET M-x json-pretty-print-buffer`
- automatically add import
- remove unsused imports
- go to definition
- find usages
- jump back and forwards marks
- change window by number
- dir local instead of init file cascade?
