;; Jump to chars
(use-package avy
	:ensure t
	:defer 3
	:config
	(my-key-one "C-s" 'avy-goto-char-timer)
	(setq avy-timeout-seconds 0.25))

;; Goto line with help
(my-key-one "M-g" 'goto-line-with-feedback)
(defun goto-line-with-feedback ()
	"Show line numbers temporarily, while prompting for the line number input"
	(interactive)
	(unwind-protect
			(progn
	(display-line-numbers-mode 1)
	(goto-line (read-number "Goto line: ")))
		(display-line-numbers-mode -1)))

;; Folding
(add-hook 'prog-mode-hook #'hs-minor-mode)
(let ((prefix "C-f "))
	(my-key "hide-all" (concat prefix char begin)
					'(hs-hide-all))
	(my-key "show-all" (concat prefix char end)
					'(hs-show-all))
	(my-key "hide-block" (concat prefix char up)
					'(hs-hide-block))
	(my-key "show-block" (concat prefix char down)
					'(hs-show-block))
	(my-key "hide-level" (concat prefix char left)
					'(hs-hide-block))
	(my-key "show-next-level" (concat prefix char right)
					'(hs-show-block)
					'(hs-hide-level 0)))
