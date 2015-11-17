;;; emacs-settings --- Configure Built-in Emacs Settings
;;; Commentary:

;;; Code:

; indent on newline
(define-key global-map (kbd "RET") 'newline-and-indent)

;; save history to .emacs.d
(setq-default savehist-additional-variables
	      '(search-ring regexp-search-ring)
	      savehist-file (concat user-emacs-directory "savehist"))
(savehist-mode t)

;; keep backups somewhere else
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; highight current line
(visual-line-mode 1)

;; hide toolbar
(tool-bar-mode -1)

;; show line numbers
(global-linum-mode)
(setq-default linum-format "%4d \u2502 ")

;; use ido everywhere
(setq-default ido-enable-flex-matching t)
(setq-default ido-everywhere t)
(ido-mode 1)

(provide 'emacs-settings)
;;; emacs-settings ends here
