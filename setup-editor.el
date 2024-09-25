;; Use tabs
(setq-default indent-tabs-mode t)
(setq-default tab-width 2)

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

;; Set mark
(my-key-one "C-a" 'set-mark-command)

;; Keybindings for movements

;; Units
(defvar char "")
(defvar word "C-n ")
(defvar symbol "M-n ")
(defvar sexp "M-m ")
(defvar line "M-i ")

;; Actionsmy-keymy-key
(defvar point "")
(defvar add-cursor "M-v ")
(defvar kill "C-d ")
(defvar append-kill "M-d ")
(defvar copy "C-w ")
(defvar append-copy "M-w ")
(defvar move "C-e ")
(defvar duplicate "M-e ")

;; Direction
(defvar left "C-j")
(defvar right "C-l")
(defvar up "C-o")
(defvar down "C-k")
(defvar begin "C-u")
(defvar end "C-p")

;; Big jumps
(defvar left-many "M-j")
(defvar right-many "M-l")
(defvar up-many "M-o")
(defvar down-many "M-k")
(defvar begin-many "M-u")
(defvar end-many "M-p")

;; Move by char
(my-key-dir '(char up)
						'(previous-line))
(my-key-dir '(char down)
						'(next-line))
(my-key-dir '(char left)
						'(left-char))
(my-key-dir '(char right)
						'(right-char))
(my-key-dir '(char begin)
						'(beginning-of-visual-line))
(my-key-dir '(char end)
						'(end-of-visual-line))

;; Move by many
(my-key-dir '(char up-many)
						'(previous-line 10))
(my-key-dir '(char down-many)
						'(next-line 10))
(my-key-dir '(char left-many)
						'(if (< (- (point) 10) (pos-bol))
								 (beginning-of-visual-line)
							 (left-char 10)))
(my-key-dir '(char right-many)
						'(if (> (+ (point) 10) (pos-eol))
								 (end-of-visual-line)
							 (right-char 10)))
(my-key-dir '(char begin-many)
						'(scroll-down))
(my-key-dir '(char end-many)
						'(scroll-up))

;; Move by word
(my-key-dir '(word left)
						'(right-char)
						'(backward-to-word 1)
						'(left-word))
(my-key-dir '(word right)
						'(forward-to-word 1))
(my-key-dir '(word begin)
						'(right-char)
						'(left-word))
(my-key-dir '(word end)
						'(left-char)
						'(right-word))
(my-key-dir '(word up)
						'(my-char-up))
(my-key-dir '(word down)
						'(my-char-down))

;; Move by line
(my-key-dir '(line left)
						'(beginning-of-line)
						'(left-char)
						'(beginning-of-line))
(my-key-dir '(line right)
						'(end-of-line)
						'(right-char))
(my-key-dir '(line up)
						'(scroll-down))
(my-key-dir '(line down)
						'(scroll-up))
(my-key-dir '(line begin)
						'(beginning-of-buffer))
(my-key-dir '(line end)
						'(end-of-buffer))

;; Move by symbol
(my-key-dir '(symbol left)
						'(right-char)
						'(forward-symbol -2))
(my-key-dir '(symbol right)
						'(forward-symbol 2)
						'(forward-symbol -1))
(my-key-dir '(symbol begin)
						'(right-char)
						'(forward-symbol -1))
(my-key-dir '(symbol end)
						'(left-char)
						'(forward-symbol 1))
(my-key-dir '(symbol up)
						'(my-char-up))
(my-key-dir '(symbol down)
						'(my-char-down))

;; Move by s-expression
(my-key-dir '(sexp left)
						'(backward-sexp))
(my-key-dir '(sexp right)
						'(forward-sexp 2)
						'(backward-sexp))
(my-key-dir '(sexp up)
						'(backward-up-list))
(my-key-dir '(sexp down)
						'(down-list))
(my-key-dir '(sexp begin)
						'(backward-up-list)
						'(down-list))
(my-key-dir '(sexp end)
						'(backward-up-list)
						'(forward-sexp)
						'(left-char))

;; Kill
(my-key "kill-region-or-line" "C-d C-d"
						'(if (use-region-p)
								 (kill-region nil nil t)
							 (kill-whole-line)))
(defun my-kill (first &rest kill)
	(eval first)
	(set-mark-command nil)
	(dolist (k kill)
		(eval k))
	(kill-region nil nil t))

;; ... Char
(my-key-dir '(kill char right)
						'(delete-char 1))
(my-key-dir '(kill char left)
						'(delete-char -1))
(my-key-dir '(kill char down)
						'(my-kill-line-down))

;; ... Word
(my-key-dir '(kill word right)
						'(my-kill
							'(my-word-begin)
							'(my-word-right)))
(my-key-dir '(kill word left)
						'(my-kill
							'(my-word-end)
							'(my-word-left)
							'(my-word-left)
							'(my-word-end)))
(my-key-dir '(kill word down)
						'(my-kill
							'(my-word-begin)
							'(my-word-end)))

;; ... Symbol
(my-key-dir '(kill symbol right)
						'(my-kill
							'(my-symbol-begin)
							'(my-symbol-right)))
(my-key-dir '(kill symbol left)
						'(my-kill
						'(my-symbol-end)
						'(my-symbol-left)
						'(my-symbol-end)))
(my-key-dir '(kill symbol down)
						'(my-kill
							'(my-symbol-begin)
							'(my-symbol-end)))

;; ... Line
(my-key-dir '(kill line down)
						'(kill-whole-line))

;; Move
;; ... Line
(my-key-dir '(move char up)
						'(my-kill-line-down)
						'(my-line-left)
						'(yank)
						'(my-line-left))
(my-key-dir '(move char down)
						'(my-kill-line-down)
						'(my-line-right)
						'(yank)
						'(my-line-left))

;; Duplicate
;; ... Line
(my-key-dir '(duplicate char up)
						'(my-kill-line-down)
						'(yank)
						'(my-line-left)
						'(yank)
						'(my-line-left))
(my-key-dir '(duplicate char down)
						'(my-kill-line-down)
						'(yank)
						'(yank)
						'(my-line-left))

;; Copy
(my-key-one "C-w" 'kill-ring-save)

;; Undo and redo
(my-key-one "C-z" 'undo)
(my-key-one "M-z" 'undo-redo)

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
(my-key-one "C-h" 'my-insert-line-below)
(my-key-one "M-h" 'my-insert-line-above)

;; Search and replaec
(my-key-one "M-s" 'isearch-backward)
(my-key-one "C-r" 'query-replace)
(my-key-one "M-r" 'query-replace-regexp)

;; Recenter point
(my-key-one "M-f" 'recenter-top-bottom)

;; Comment-line
(my-key-one "C-x c" 'comment-line)

;; Expand region
(use-package expand-region
	:ensure t
	:defer 3
	:bind ("C-v" . 'er/expand-region))

;; ;; Multi-cursor
;; (use-package multiple-cursors
;;	:ensure t
;;	:defer 3
;;	:config
;;	(let ((prefix add-cursor))
;;		(my-key-one (concat prefix add-cursor)
;;		'mc/edit-lines)
;;		(my-key-one (concat prefix char right)
;;		'mc/mark-next-like-this)
;;		(my-key-one (concat prefix char left)
;;		'mc/mark-previous-like-this)
;;		(my-key-one (concat prefix word right)
;;								'mc/mark-next-like-this-word)
;;		(my-key-one (concat prefix word left)
;;								'mc/mark-previous-like-this-word)
;;		(my-key-one (concat prefix symbol right)
;;								'mc/mark-next-like-this-symbol)
;;		(my-key-one (concat prefix symbol left)
;;								'mc/mark-previous-like-this-symbol)
;;		(my-key-one (concat prefix symbol end)
;;								'mc/mark-all-symbols-like-this)
;;		))
