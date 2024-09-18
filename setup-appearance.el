;; Keep it clean
(tool-bar-mode -1)
(menu-bar-mode -1)

;; Dive right in
(setq inhibit-splash-screen t)

;; Center buffer
(use-package centered-window
  :ensure t
  :config (centered-window-mode t))

;; Fix for emacs saying window is too small for splitting
(defun my/cwm-turn-off (fnc &optional window size side pixelwise)
  "Deactivate the centered-window-mode before splitting."
  (if centered-window-mode
      (progn
        (centered-window-mode -1)
        (apply fnc window size side pixelwise)
        (centered-window-mode t))
    (apply fnc window size side pixelwise)))
(advice-add 'split-window :around #'my/cwm-turn-off)

;; Break only on words
(global-visual-line-mode t)

;; Make it dark and pretty
(use-package darcula-theme
  :ensure t
  :config
  (set-frame-font "DejaVu Sans Mono-10")
  (load-theme 'darcula t))
