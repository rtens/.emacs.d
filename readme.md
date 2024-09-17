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

### Basic

Invoked without a prefix.

#### Moving

Movement is based on units and actions on the unit. Each unit is associated with a modifier, and each action with a key.

The units and their modifiers are
- `C` row/column/character
- `s` word
- `M` line/buffer
- `C-s` symbol
- `C-M` s-expression

And the actions and their keys are
- `j` left/prev
- `l` right/next
- `i` up/out
- `k` down/in
- `u` beginning
- `o` end
- `d` delete *TODO*
- `e` duplicate *TODO*

#### Jumping

- `C-a` jump to character
- `C-b` goto line
- `C-s` search forward
- `C-r` search backwards

#### Folding

- collapse everything `C-c h H` *TODO*
- expand next level `C-c h l` *TODO*
- expand block `C-c h s` *TODO*
- expand everything `C-c h S` *TODO*

#### Marking

- `S-` mark with next move *TODO*
- `C-SPC` set mark/deactivate mark
- `C-g` quit/deactivate mark
- `C-c` add cursor with next move
- move back/forward in mark ring *TODO*
- expand region *TODO*

#### Killing

- `C-d` delete region / with next move *TODO*
- `C-w` kill region / with next move *TODO*
- `S-C-w` extend kill region / with next move *TODO*
- `M-w` copy region
- `S-M-w` extend copy region / with next move *TODO*
- `C-y` yank last kill
- `M-y` yank next kill
- `s-y` show kill ring *TODO*

#### History

- `C-q` undo
- `S-C-q` redo
- `s-q` show undo tree *TODO*

#### Moving Text

- `C-v i/k` move line/region up/down *TODO*
- `C-v j/l` move line/region left/right *TODO*
- `C-RET` insert line below current
- `S-C-RET` insert line above current

#### Scrolling

- `C-v i/k` scroll one line up/down *TODO*
- `C-v j/l` scroll one column left/right *TODO*
- `S-C-v i/k` scroll page up/down *TODO*
- `S-C-v j/l` scroll page left/right *TODO*
- `C-f` move point to top/center/bottom *TODO*

#### Buffers

- move buffer `C-s-ARROW` *TODO*
- back/forward windows configs `C-x ARROW` *TODO*
- toggle buffer *TODO*

### Extendend

Invoked with prefix `C-x`


#### Editing

- search and replace `M-%` *TODO*
- search and replace with regex `C-M-%` (capture `\(.+\)` > `\1`) *TODO*
- comment lines `C-x /` *TODO*

#### Other

- `r q` exit *TODO*
- `g` show Magit status
- open/create file `C-x C-f`
- save file `C-x C-s`
- save some files `C-x s`
- close buffer `C-x k`
- change buffer `C-x b`
- show buffer list `C-x C-b`
- imenu `C-x i`
- format buffer `C-x f` *TODO*
- cleanup buffer `C-x c` *TODO*

- windows
    - close current window `C-x 0`
    - close other windows `C-x 1`
    - split down `C-x 2`
    - split right `C-x 3`
    - switch buffer in other window `C-x 4 b`
    - switch window `C-x o` *TODO*
- file management
    - jump to project root `C-x C-j`
    - create folder `+`
    - delete file `D`
- navigate in project
    - search file `C-x p f`
    - open file at line *TODO*
    - full-text search `C-x p g`
- terminal
    - open shell `M-x term|shell|eshell`
    - goto error line
- execution
    - eval at point `C-x C-e`
    - eval `C-x M-e` *TODO*
    - repeat command `C-x z`

### Unbound

Invoked with `M-x`

- eval-buffer
- refactoring *TODO*
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

- how to scroll without moving the point
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
- go to definition of symbol
- find usages of symbol
- jump back and forwards marks
- change window by number
- dir local instead of init file cascade?
