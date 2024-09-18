;; Keep it clean
(tool-bar-mode -1)
(menu-bar-mode -1)

;; Dive right in
(setq inhibit-splash-screen t)

;; Break only on words
(global-visual-line-mode t)

;; Center buffer
(use-package centered-window
  :ensure t
  :config (centered-window-mode t))

;; Make it dark
(use-package darcula-theme
  :ensure t
  :config (load-theme 'darcula))
