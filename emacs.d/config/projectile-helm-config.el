;;; projectile-helm-config --- Projectile and Helm Configuration

;;; Commentary:

;;; Code:

(autoload 'use-package "Loads packages on demand" nil nil)

(use-package projectile
  :commands (projectile-find-file projectile-find-directory helm-projectile-find-file helm-projectile-find-directory)
  :config
  (projectile-global-mode)
  (setq-default projectile-enable-caching nil)
  (require 'helm-projectile)
  (setq-default projectile-completion-system 'helm)
  (helm-projectile-on)
  )

(use-package helm
  :commands (helm-projectile-find-file helm-projectile-find-directory
	     helm-find-files helm-buffers-list helm-M-x helm-apropos)
  :init
  (evil-leader/set-key
    "p" 'helm-projectile-find-file
    "d" 'helm-projectile-find-directory
    "f" 'helm-find-files
    "b" 'helm-buffers-list
    "x" 'helm-M-x
    "h" 'helm-apropos
    )
  )

(provide 'projectile-helm-config)
;;; projectile-helm-config ends here
