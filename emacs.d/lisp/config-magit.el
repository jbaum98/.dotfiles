;;; config-magit --- Summary
;;; Commentary:
;;; Code:

(eval-when-compile
  (require 'use-package))

(use-package magit
  :ensure
  :defer
  :if (executable-find "git")
  :bind (("C-x g" . magit-status)
         ("C-x G" . magit-dispatch-popup))
  :init
  (setq magit-completing-read-function 'ivy-completing-read))

(provide 'config-magit)
;;; config-magit.el ends here
