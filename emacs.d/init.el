(require 'package) 
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))

(setq package-enable-at-startup nil)
(package-initialize)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)

(define-key global-map (kbd "RET") 'newline-and-indent)
(setq savehist-additional-variables    ;; also save...
      '(search-ring regexp-search-ring)    ;; ... my search entries
      savehist-file "~/.emacs.d/savehist") ;; keep my home clean
(savehist-mode t)                      ;; do customization before activate
(visual-line-mode 1)
(tool-bar-mode -1)
(defun font-existsp (font)
  (if (null (x-list-fonts font))
      nil t))
(if window-system
    (progn
      (set-face-attribute 'default nil :height 160)
      (if (font-existsp "Consolas")
	  (set-face-attribute 'default nil :font "Consolas"))
  ))

(use-package monokai-theme
  :init
  (load-theme 'monokai t)
  )

(use-package helm
  :config (helm-mode 1)
  )

(use-package projectile
  :init (use-package flx-ido)
  :config
  (projectile-global-mode)
  (setq projectile-enable-caching t)
  )

(use-package helm-projectile
  :config
  (helm-projectile-on)
  )

(use-package company
  :config
  (global-company-mode)
  )

(use-package key-chord
  :config
  (key-chord-mode 1)
  )

(use-package evil-leader
  :config
  (global-evil-leader-mode)
  (evil-leader/set-leader "<SPC>")
  (evil-leader/set-key
    "p" 'helm-projectile-find-file
    "d" 'helm-projectile-find-directory
    "b" 'switch-to-buffer
    "k" 'kill-buffer)
  )

(use-package evil
  :config
  (evil-mode t)
  (key-chord-define evil-normal-state-map "jk" 'evil-force-normal-state)
  (key-chord-define evil-visual-state-map "jk" 'evil-change-to-previous-state)
  (key-chord-define evil-insert-state-map "jk" 'evil-normal-state)
  (key-chord-define evil-replace-state-map "jk" 'evil-normal-state)
  (evil-define-key 'normal evil-normal-state-map (kbd ";") 'evil-ex)
  (evil-define-key 'normal evil-normal-state-map (kbd "C-h") 'evil-window-left)
  (evil-define-key 'normal evil-normal-state-map (kbd "C-j") 'evil-window-down)
  (evil-define-key 'normal evil-normal-state-map (kbd "C-k") 'evil-window-up)
  (evil-define-key 'normal evil-normal-state-map (kbd "C-l") 'evil-window-right)
  (evil-define-key 'normal emacs-lisp-mode-map "K" 'elisp-slime-nav-describe-elisp-thing-at-point)
  (define-key evil-normal-state-map (kbd ";") 'evil-ex)
  )

(use-package evil-nerd-commenter
  :config
  (evil-leader/set-key
    "cc" 'evilnc-comment-or-uncomment-lines
    "cl" 'evilnc-quick-comment-or-uncomment-to-the-line
    "cp" 'evilnc-comment-or-uncomment-paragraphs
    "cr" 'comment-or-uncomment-region
    "cv" 'evilnc-toggle-invert-comment-line-by-line
    )
  )

(use-package flycheck
  :config
  (set-face-attribute 'flycheck-error nil :slant 'italic :foreground "red" :underline t)
  (global-flycheck-mode)
  )

(provide 'init)
;;; init.el ends here
