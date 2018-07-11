;;; config-packages --- Summary
;;; Commentary:
;;; Code:

(eval-when-compile
  (require 'use-package))

(use-package company
  :pin melpa
  :ensure
  :bind ("<C-tab>" . company-complete)
  :diminish company-mode
  :commands (company-mode global-company-mode)
  :defer 1
  :config
  (global-company-mode))

(use-package counsel
  :pin melpa-stable
  :ensure
  :commands (counsel-descbinds)
  :bind (([remap execute-extended-command] . counsel-M-x)
         ("C-x C-f" . counsel-find-file)
         ("C-c g" . counsel-git)
         ("C-c j" . counsel-git-grep)
         ("C-c k" . counsel-ag)
         ("C-x l" . counsel-locate)
         ("M-y" . counsel-yank-pop)))

(use-package flycheck
  :pin melpa-stable
  :ensure
  :defer 2
  :commands (global-flycheck-mode)
  :diminish flycheck-mode
  :config
  (global-flycheck-mode)
  (custom-set-variables
   '(flycheck-emacs-lisp-load-path 'inherit)))

(use-package ivy
  :pin melpa-stable
  :ensure
  :defer 1
  :bind (("C-c C-r" . ivy-resume)
         ("C-x C-b" . ivy-switch-buffer)
         :map ivy-minibuffer-map
         ("C-j" . ivy-call))
  :diminish ivy-mode
  :commands ivy-mode
  :config
  (ivy-mode 1))

(require 'config-magit)

(use-package projectile
  :commands projectile-mode
  :ensure
  :bind-keymap ("C-c p" . projectile-command-map)
  :defer 5
  :config
  (projectile-mode))

(provide 'config-packages)
;;; config-packages.el ends here
