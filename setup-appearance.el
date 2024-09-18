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

;; Fix for not splitting right with wide margins
(defun my-split-window-sensibly (&optional window)
    (centered-window-mode 0)
    (unwind-protect
	(split-window-sensibly window)
      (centered-window-mode 1)))
(setq split-window-preferred-function #'my-split-window-sensibly)

(defun my-split-window-right ()
  (interactive)
  (centered-window-mode 0)
  (split-window-right)
  (centered-window-mode 1))
(global-set-key (kbd "C-x 3") 'my-split-window-right)
