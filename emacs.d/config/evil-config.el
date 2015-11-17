;;; evil-config --- Evil Mode Configurationa and Other Keybindings

;;; Commentary:

;;; Code:

(autoload 'use-package "Loads packages on demand" nil nil)
(autoload 'evil-define-key "Define key for evil mode map" nil nil)
(autoload 'evil-leader/set-leader "Set Evil leader key" nil nil)

(use-package evil
  :config
  (evil-mode t)
  (setq-default evil-auto-indent t)
  (define-key evil-normal-state-map (kbd ";") 'evil-ex)
  (define-key evil-normal-state-map (kbd "C-h") 'evil-window-left)
  (define-key evil-normal-state-map (kbd "C-j") 'evil-window-down)
  (define-key evil-normal-state-map (kbd "C-k") 'evil-window-up)
  (define-key evil-normal-state-map (kbd "C-l") 'evil-window-right)
  )

(use-package evil-escape
  :config
  (evil-escape-mode)
  (setq-default evil-escape-key-sequence "jk")
  )

(use-package evil-leader
  :config
  (global-evil-leader-mode)
  (evil-leader/set-leader "<SPC>")
  (evil-leader/set-key
   "k" 'kill-buffer
   )
  )

(use-package key-chord
  :config
  (key-chord-mode 1)
  )

(use-package evil-nerd-commenter
  :commands (evilnc-comment-or-uncomment-lines
	     evilnc-comment-or-uncomment-paragraphs
	     evilnc-quick-comment-or-uncomment-to-the-line
	     comment-or-uncomment-region
	     evilnc-toggle-invert-comment-line-by-line)
  :config
  (evil-leader/set-key
   "cc" 'evilnc-comment-or-uncomment-lines
   "cl" 'evilnc-quick-comment-or-uncomment-to-the-line
   "cp" 'evilnc-comment-or-uncomment-paragraphs
   "cr" 'comment-or-uncomment-region
   "cv" 'evilnc-toggle-invert-comment-line-by-line
   )
  )

(provide 'evil-config)
;;; evil-config ends here
