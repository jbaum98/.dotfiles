;;; init --- Jake Waksbaum's Emacs Init File
;;; Commentary:

;;; Code:
(add-to-list 'load-path (concat user-emacs-directory "config"))

(require 'use-package-init)
(require 'emacs-settings)

(use-package monokai-theme
  :init
  (load-theme 'monokai t)
  )

(require 'evil-config)
(require 'projectile-helm-config)
(require 'completion)
(require 'magit-config)
(require 'ruby)

(use-package flycheck
  :config
  (set-face-attribute 'flycheck-error nil :slant 'italic :foreground "red" :underline t)
  (setq-default flycheck-disabled-checkers '(ruby-rubylint))
  (global-flycheck-mode)
  )

(use-package haskell-mode
  :mode ("\\.hs\\'" . haskell-mode)
  :config
  (setq haskell-font-lock-symbols t)
  )

(provide 'init)
;;; init.el ends here
