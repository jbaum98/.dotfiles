;;; magit-config --- magit configuration

;;; Commentary:

;;; Code:

(use-package magit
  :commands magit-status
  :init
  (evil-leader/set-key
    "m" 'magit-status
    )
  :config
  (use-package evil-magit)
  )

(provide 'magit-config)
;;; magit-config ends here
