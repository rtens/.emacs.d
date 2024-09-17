(when (eq system-type 'darwin)
  (setq
   ns-control-modifier 'control
   ns-option-modifier 'super
   ns-command-modifier 'meta
   ;;  ns-function-modifier 'hyper
   ))

;; Minor mode tutorial: http://nullprogram.com/blog/2013/02/06/
;; Main use is to have my key bindings have the highest priority
;; https://github.com/kaushalmodi/.emacs.d/blob/master/elisp/modi-mode.el

(defvar my-mode-map (make-sparse-keymap)
  "Keymap for `my-mode'.")

;;;###autoload
(define-minor-mode my-mode
  "A minor mode so that my key settings override annoying major modes."
  ;; If init-value is not set to t, this mode does not get enabled in
  ;; `fundamental-mode' buffers even after doing \"(global-my-mode 1)\".
  ;; More info: http://emacs.stackexchange.com/q/16693/115
  :init-value t
  :lighter " my-mode"
  :keymap my-mode-map)

;;;###autoload
(define-globalized-minor-mode global-my-mode my-mode my-mode)

;; https://github.com/jwiegley/use-package/blob/master/bind-key.el
;; The keymaps in `emulation-mode-map-alists' take precedence over
;; `minor-mode-map-alist'
(add-to-list 'emulation-mode-map-alists `((my-mode . ,my-mode-map)))

;; Turn off the minor mode in the minibuffer
(defun turn-off-my-mode ()
  "Turn off my-mode."
  (interactive)
  (my-mode -1))
(add-hook 'minibuffer-setup-hook #'turn-off-my-mode)
(add-hook 'magit-mode-hook #'turn-off-my-mode)

(defun my/my-mode-key (key commands)
  (define-key my-mode-map (kbd key) (append '(lambda () (interactive)) commands)))

;; ### MOVE

;; by row
(my/my-mode-key "C-i" '((previous-line)))
(my/my-mode-key "C-k" '((next-line)))

;; by column
(my/my-mode-key "C-j" '((left-char)))
(my/my-mode-key "C-l" '((right-char)))
(my/my-mode-key "C-u" '((beginning-of-visual-line)))
(my/my-mode-key "C-o" '((end-of-visual-line)))

;; by word
(my/my-mode-key "s-j" '((right-char) (backward-to-word 1) (left-word)))
(my/my-mode-key "s-l" '((forward-to-word 1)))
(my/my-mode-key "s-u" '((right-char) (left-word)))
(my/my-mode-key "s-o" '((left-char) (right-word)))

;; by symbol
(my/my-mode-key "C-s-j" '((right-char) (forward-symbol -2)))
(my/my-mode-key "C-s-l" '((forward-symbol 2) (forward-symbol -1)))
(my/my-mode-key "C-s-u" '((right-char) (forward-symbol -1)))
(my/my-mode-key "C-s-o" '((left-char) (forward-symbol 1)))

;; by line
(my/my-mode-key "M-j" '((forward-line -1)))
(my/my-mode-key "M-l" '((forward-line 1)))
(my/my-mode-key "M-i" '((scroll-down)))
(my/my-mode-key "M-k" '((scroll-up)))
(my/my-mode-key "M-u" '((beginning-of-buffer)))
(my/my-mode-key "M-o" '((end-of-buffer)))

;; by s-expression
(my/my-mode-key "C-M-j" '((backward-sexp)))
(my/my-mode-key "C-M-l" '((forward-sexp 2) (backward-sexp)))
(my/my-mode-key "C-M-i" '((backward-up-list)))
(my/my-mode-key "C-M-k" '((down-list)))
(my/my-mode-key "C-M-u" '((backward-up-list) (down-list)))
(my/my-mode-key "C-M-o" '((backward-up-list) (forward-sexp) (left-char)))


(provide 'my-mode)
;;; my-mode.el ends here
