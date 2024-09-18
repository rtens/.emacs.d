;; Make pairs close automatically
(electric-pair-mode 1)
(setq electric-pair-pairs (append electric-pair-pairs '((?` . ?`))))

;; Set locale to UTF8
(set-language-environment 'utf-8)
(set-terminal-coding-system 'utf-8)
(setq locale-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; Use unix-style line endings (LF)
(set-default buffer-file-coding-system 'utf-8-unix)

;; Cleanup whitespaces on save
(add-hook 'before-save-hook 'whitespace-cleanup)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Insert final newlines automatically on save
(setq require-final-newline t)

;; Overwrite selected region
(pending-delete-mode t)
