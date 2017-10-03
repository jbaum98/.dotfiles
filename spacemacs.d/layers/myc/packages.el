(setq myc-packages '(flycheck))

(defun myc/post-init-flycheck ()
  (add-hook 'c-mode-hook 'myc/flycheck-splint-setup)
  )
