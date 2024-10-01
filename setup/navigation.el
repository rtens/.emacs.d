;; Jump to chars
(use-package avy
	:ensure t
	:defer 3
	:config
	(my-key-one "C-s" 'avy-goto-char-timer)
	(setq avy-timeout-seconds 0.25))

;; Jump to next char
(unless (package-installed-p 'iy-go-to-char)
	(package-vc-install "https://github.com/doitian/iy-go-to-char"))
(defun my-go-to-char (n char)
	"Goes up to char and then continues with directions"
	(interactive "p\ncGo up to char: ")
	(iy-go-up-to-char n char)
	(my-go-to-char-continue))
(defun my-go-to-char-continue ()
	(set-transient-map
	 (let ((xkmap (make-sparse-keymap)))
		 (define-key xkmap (kbd right) (lambda ()
																		 (interactive)
																		 (iy-go-up-to-char-continue 1)
																		 (my-go-to-char-continue)))
		 (define-key xkmap (kbd left) (lambda ()
																		(interactive)
																		(iy-go-to-char-continue-backward 1)
																		(my-go-to-char-continue)))
		 xkmap)))
(global-set-key (kbd "M-a") 'my-go-to-char)

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
