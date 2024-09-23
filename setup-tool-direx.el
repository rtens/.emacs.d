;; Load the package
(use-package direx
	:ensure t
	:config (global-set-key (kbd "C-c d") 'direx:jump-to-directory))
