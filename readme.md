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

## Cheat sheet

- basics
    - exit `C-x C-c`
    - open/create file `C-x C-f`
    - save file `C-x C-s`
    - close buffer `C-x k`
- file management
    - create folder `C-x d RET +`
    - delete file `C-x d RET D`
    - delete multiple files `C-x d RET m D`
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
- navigate in buffer
    - collapse everything
    - expand under point
    - search for string `C-s`
    - jump to char
- navigate in project
    - search file
        - including line numbers
    - full-text search
- open shell `M-x term|shell|eshell`
- git `C-x g`
- refactoring
- snippets
    - class
    - method
    - constructur
    - this/self
    - test
- execute lisp `C-x C-e`

## To explore

- [projectile](https://github.com/bbatsov/projectile)
- [jumping around](https://emacsrocks.com/e10.html)
    - ace-jump-mode
    - ido-imenu-push-mark
- expand-region
- yasnippet
- iy-go-to-char

## Open questions

- register a window configuration
    - for the session?
    - for the project?
- navigation in buffer
    - collapse everything
    - move to word / char quickly
    - expand everything under point
- full text search in project
- jump to file in project (typing just about any part of its path)
- indent multiple lines
- browse with file tree
- jump to error line
- refactoring
    - javascript
    - python
- quickly find methods of type
    - auto-complete for known type
- jump between projects
    - restore windows
    - load project init file
    - have multiple projects open
- format buffer
    - javascript
    - python (using local config as in pre-commit hook)
    - JSON (pretty)
- automatically add import
- remove unsused imports
- comment multiple lines
- go to definition
- find usages
- jump back and forwards marks
- move buffer to other/new window
- change window by number
- dir local instead of init file cascade
