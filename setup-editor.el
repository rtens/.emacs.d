;; Make pairs close automatically
(electric-pair-mode 1)
(setq electric-pair-pairs (append electric-pair-pairs '((?` . ?`))))

;; Set locale to UTF8
(set-language-environment 'utf-8)
(set-terminal-coding-system 'utf-8)
(setq locale-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; Use unix-style line endings (LF)
(set-default buffer-file-coding-system 'utf-8-unix)

;; Cleanup whitespaces on save
(add-hook 'before-save-hook 'whitespace-cleanup)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Insert final newlines automatically on save
(setq require-final-newline t)

;; Overwrite selected region
(pending-delete-mode t)

;; Keybindings for movements

;; Units
(defvar char "C-")
(defvar word "s-")
(defvar symbol "C-s-")
(defvar line "M-")
(defvar sexp "C-M-")

;; Actions
(defvar left "l")
(defvar right "ä")
(defvar up "p")
(defvar down "ö")
(defvar begin "o")
(defvar end "ü")

;; Move by row
(my-key "row-up" (concat char up)
	'(previous-line))
(my-key "row-down" (concat char down)
	'(next-line))
(my-key "column-left" (concat char left)
	'(left-char))
(my-key "column-right" (concat char right)
	'(right-char))
(my-key "column-begin" (concat char begin)
	'(beginning-of-visual-line))
(my-key "column-end" (concat char end)
	'(end-of-visual-line))

;; Move by word
(my-key "word-left" (concat word left)
	'(right-char)
	'(backward-to-word 1)
	'(left-word))
(my-key "word-right" (concat word right)
	'(forward-to-word 1))
(my-key "word-begin" (concat word begin)
	'(right-char)
	'(left-word))
(my-key "word-end" (concat word end)
	'(left-char)
	'(right-word))

;; Move by symbol
(my-key "symbol-left" (concat symbol left)
	'(right-char)
	'(forward-symbol -2))
(my-key "symbol-right" (concat symbol right)
	'(forward-symbol 2)
	'(forward-symbol -1))
(my-key "symbol-begin" (concat symbol begin)
	'(right-char)
	'(forward-symbol -1))
(my-key "symbol-end" (concat symbol end)
	'(left-char)
	'(forward-symbol 1))

;; Move by line
(my-key "line-left" (concat line left)
	'(forward-line -1))
(my-key "line-right" (concat line right)
	'(forward-line 1))
(my-key "line-up" (concat line up)
	'(scroll-down))
(my-key "line-down" (concat line down)
	'(scroll-up))
(my-key "line-begin" (concat line begin)
	'(beginning-of-buffer))
(my-key "line-end" (concat line end)
	'(end-of-buffer))

;; Move by s-expression
(my-key "sexp-left" (concat sexp left)
	'(backward-sexp))
(my-key "sexp-right" (concat sexp right)
	'(forward-sexp 2)
	'(backward-sexp))
(my-key "sexp-up" (concat sexp up)
	'(backward-up-list))
(my-key "sexp-down" (concat sexp down)
	'(down-list))
(my-key "sex-begin" (concat sexp begin)
	'(backward-up-list)
	'(down-list))
(my-key "sexp-end" (concat sexp end)
	'(backward-up-list)
	'(forward-sexp)
	'(left-char))

;; Undo and redo
(my-key-one "C-q" 'undo)
(my-key-one "S-C-q" 'undo-redo)

;; Insert new line
(my-key "insert-line-below" "C-<return>"
	'(end-of-line)
	'(newline)
	'(indent-for-tab-command))
(my-key "insert-line-above" "S-C-<return>"
	'(beginning-of-line)
	'(newline)
	'(forward-line -1)
	'(indent-for-tab-command))

;; Search and replace
(my-key-one "C-M-s" 'query-replace)
(my-key-one "C-M-r" 'query-replace-regexp)

;; Comment-line
(my-key-one "C-/" 'comment-line)

;; Recenter point
(my-key-one "C-f" 'recenter-top-bottom)
