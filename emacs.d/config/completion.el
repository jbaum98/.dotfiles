;;; completion --- Code Completion

;;; Commentary:

;;; Code:

(autoload 'use-package "Loads packages on demand" nil nil)

(use-package company
  :commands company-complete
  :init
  (define-key evil-insert-state-map (kbd "TAB") 'company-complete)
  :config
  (company-mode)
  (define-key evil-insert-state-map (kbd "C-n") 'company-select-next)
  (define-key evil-insert-state-map (kbd "C-p") 'company-select-previous)
  (setq company-backends '((company-capf company-dabbrev-code company-files)))
  )

(provide 'completion)
;;; completion ends here
