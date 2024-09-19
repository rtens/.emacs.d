;; Load the package
(use-package direx
  :ensure t
  :config (global-set-key (kbd "C-x C-j") 'direx:jump-to-directory))
