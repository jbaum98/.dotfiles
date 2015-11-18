;;; emacs-settings --- Configure Built-in Emacs Settings
;;; Commentary:

;;; Code:

(push "/usr/local/bin" exec-path)

; indent on newline
(define-key global-map (kbd "RET") 'newline-and-indent)

(setq-default tab-width 2)
(setq-default indent-tabs-mode nil)
(setq inhibit-startup-message t)
(fset 'yes-or-no-p 'y-or-n-p)
(delete-selection-mode t)
(if (display-graphic-p)
  (progn
    (tool-bar-mode -1)
    (scroll-bar-mode -1)
    (set-fringe-style -1)
    (tooltip-mode -1)))
(blink-cursor-mode t)
(show-paren-mode t)
(column-number-mode t)

;; save history to .emacs.d
(setq-default savehist-additional-variables
              '(search-ring regexp-search-ring)
              savehist-file (concat user-emacs-directory "savehist"))
(savehist-mode t)
(setq make-backup-files nil)
(setq auto-save-default nil)

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
