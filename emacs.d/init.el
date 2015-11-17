;;; init --- Jake Waksbaum's Emacs Init File
;;; Commentary:

;;; Code:
(add-to-list 'load-path (concat user-emacs-directory "config"))

(add-to-list 'load-path "/home/jake/benchmark-init-el")
(require 'benchmark-init-loaddefs)
(benchmark-init/activate)

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

(use-package rspec-mode
  :mode ("\\.rb\\'" . ruby-mode)
  :config
  (custom-set-variables
   '(rspec-use-bundler-when-possible nil)
   '(rspec-use-rake-when-possible nil)
   '(rspec-use-spring-when-possible t)
   )
  (evil-leader/set-key-for-mode 'rspec-verifiable-mode
    "tv" 'rspec-verify
    "ta" 'rspec-verify-all
    "tt" 'rspec-toggle-spec-and-target
    "te" 'rspec-toggle-spec-and-target-find-example
    "tr" 'rspec-rerun
    "tm" 'rspec-verify-matching
    "tc" 'rspec-verify-continue
    "ts" 'rspec-verify-method
    "tf" 'rspec-run-last-failed
    )
  (evil-leader/set-key-for-mode 'rspec-mode-keymap
    "ts" 'rspec-verify-single
    "td" 'rspec-toggle-example-pendingness
    )
  )

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
(benchmark-init/deactivate)

(provide 'init)
;;; init.el ends here
