;; Open magit status fullscreen
(defadvice magit-status (around magit-fullscreen activate)
  (window-configuration-to-register :magit-fullscreen)
  ad-do-it
  (delete-other-windows))
(defun magit-quit-session ()
  "Restores the previous window configuration and kills the magit buffer"
  (interactive)
  (kill-buffer)
  (jump-to-register :magit-fullscreen))


;; Use package
(use-package magit
  :ensure t
  :config (define-key magit-status-mode-map (kbd "q") 'magit-quit-session))
