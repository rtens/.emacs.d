(use-package js2-mode
	:ensure t
	:mode (("\\.js$" . js2-mode))
	:hook (js2-mode
				 . (lambda ()
						 (setq-default tab-width 2)
						 (setq js-indent-level 2
									 js2-basic-offset 2)
						 (js2-imenu-extras-mode))))

(use-package js2-refactor
	:ensure t
	:hook (js2-mode . js2-refactor-mode)
	:config (js2r-add-keybindings-with-prefix "C-c r"))
