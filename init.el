;; ========== basics ===============
(tool-bar-mode -1)
(menu-bar-mode -1)

(setq inhibit-splash-screen t)
(setq user-full-name "Nikolas Martens")
(setq user-mail-address "Nikolas.M@rtens.org")
(setq root-directory default-directory)
(setq require-final-newline t)

(fset 'yes-or-no-p 'y-or-n-p)
(winner-mode 1)
(electric-pair-mode 1)
(global-subword-mode 1)
(ido-mode t)
(savehist-mode t)
(pending-delete-mode t)

;; Keep buffers up-to-date
(global-auto-revert-mode 1)
;; Also auto refresh dired, but be quiet about it
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)

(setq-default indent-tabs-mode nil
        tab-width 2
        fill-column 80)

(add-hook 'before-save-hook 'whitespace-cleanup)
(add-hook 'before-save-hook (lambda() (delete-trailing-whitespace)))

;; Set locale to UTF8
(set-language-environment 'utf-8)
(set-terminal-coding-system 'utf-8)
(setq locale-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; Font size
(set-frame-font (if (>= (display-pixel-width) 2500) "Menlo 20" "Menlo 12"))

;; Own key bindings
(global-set-key (kbd "C-c C-v") 'imenu)
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; Toggle two most recent buffers
(fset 'quick-switch-buffer [?\C-x ?b return])
(global-set-key (kbd "s-b") 'quick-switch-buffer)

;; Don't beep. Just blink the modeline on errors.
(setq ring-bell-function (lambda ()
         (invert-face 'mode-line)
         (run-with-timer 0.05 nil 'invert-face 'mode-line)))

;; Rename buffer in file
(defun rename-current-buffer-file ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let ((name (buffer-name))
  (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
  (error "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " filename)))
  (if (get-buffer new-name)
      (error "A buffer named '%s' already exists!" new-name)
    (rename-file filename new-name 1)
    (rename-buffer new-name)
    (set-visited-file-name new-name)
    (set-buffer-modified-p nil)
    (message "File '%s' successfully renamed to '%s'"
       name (file-name-nondirectory new-name)))))))
(global-set-key (kbd "C-x C-r") 'rename-current-buffer-file)

;; Open line below or above
(defun open-line-below ()
  (interactive)
  (end-of-line)
  (newline)
  (indent-for-tab-command))
(defun open-line-above ()
  (interactive)
  (beginning-of-line)
  (newline)
  (forward-line -1)
  (indent-for-tab-command))
(global-set-key (kbd "<C-return>") 'open-line-below)
(global-set-key (kbd "<C-S-return>") 'open-line-above)

;; Goto line with help
(global-set-key [remap goto-line] 'goto-line-with-feedback)
(defun goto-line-with-feedback ()
  "Show line numbers temporarily, while prompting for the line number input"
  (interactive)
  (unwind-protect
      (progn
  (display-line-numbers-mode 1)
  (goto-line (read-number "Goto line: ")))
    (display-line-numbers-mode -1)))

;; ========== javascrpt ============
(setq js-indent-level 2)
;; (add-hook 'js-mode-hook #'display-line-numbers-mode)

;; ======== desktop mode ===========
(desktop-save-mode 1)
(setq desktop-path '("."))

;; ======== auto saves =============
(let ((my-auto-save-dir (locate-user-emacs-file "auto-save")))
  (setq auto-save-file-name-transforms
  `((".*" ,(expand-file-name "\\2" my-auto-save-dir) t)))
  (unless (file-exists-p my-auto-save-dir)
    (make-directory my-auto-save-dir)))
(setq auto-save-default t
      auto-save-timeout 10
      auto-save-interval 200)

;; ======== backup files ==========
(let ((my-backup-dir (locate-user-emacs-file "backups")))
  (setq backup-directory-alist `(("." . ,my-backup-dir)))
  (unless (file-exists-p my-backup-dir)
    (make-directory my-backup-dir)))
(setq backup-by-copying t
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)

;; ===== customization files ======
(setq custom-file "~/.emacs.d/custom-local.el")

;; ====== init files cascade ======
(dolist (init-file (list "~/.emacs.d/custom.el"
       "~/.emacs.d/custom-local.el"
       "~/.emacs.d/init-local.el"
       "./.emacs-project.el"
       "./.emacs-local.el"))
  (if (file-exists-p init-file)
      (load-file init-file)))

;; ========== packages ============
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(use-package move-dup :ensure t)
(global-move-dup-mode)

(use-package flycheck
  :ensure t
  :config
  (add-hook 'after-init-hook #'global-flycheck-mode))
(flycheck-add-mode 'javascript-eslint 'js-mode)

;; use local eslint from node_modules before global
;; http://emacs.stackexchange.com/questions/21205/flycheck-with-file-relative-eslint-executable
(defun my/use-eslint-from-node-modules ()
  (let* ((root (locate-dominating-file
    (or (buffer-file-name) default-directory)
    "node_modules"))
   (eslint (and root
          (expand-file-name "node_modules/eslint/bin/eslint.js"
          root))))
    (when (and eslint (file-executable-p eslint))
      (setq-local flycheck-javascript-eslint-executable eslint))))
(add-hook 'flycheck-mode-hook #'my/use-eslint-from-node-modules)

(use-package auto-complete :ensure t)
(global-auto-complete-mode t)

(use-package highlight-symbol
  :ensure t
  :config (add-hook 'js-mode-hook #'highlight-symbol-mode))

(use-package multiple-cursors :ensure t)
(global-set-key (kbd "C-c c") 'mc/edit-lines)
(global-set-key (kbd "C-c n") 'mc/mark-next-like-this)
(global-set-key (kbd "C-c p") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c f") 'mc/mark-next-like-this-symbol)
(global-set-key (kbd "C-c b") 'mc/mark-previous-like-this-symbol)
(global-set-key (kbd "C-c a") 'mc/mark-all-symbols-like-this)

(use-package magit :ensure t)

;; full screen magit-status
(defadvice magit-status (around magit-fullscreen activate)
  (window-configuration-to-register :magit-fullscreen)
  ad-do-it
  (delete-other-windows))
(defun magit-quit-session ()
  "Restores the previous window configuration and kills the magit buffer"
  (interactive)
  (kill-buffer)
  (jump-to-register :magit-fullscreen))
(define-key magit-status-mode-map (kbd "q") 'magit-quit-session)

(use-package buffer-move :ensure t)

(use-package uniquify :ensure t)
(setq uniquify-buffer-name-style 'forward)

;; Save point position between sessions
(use-package saveplace :ensure t)
(setq-default save-place t)
(setq save-place-file (expand-file-name ".places" user-emacs-directory))

(provide 'init)
;;; init.el ends here
