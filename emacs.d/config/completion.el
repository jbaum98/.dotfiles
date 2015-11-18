;;; completion --- Code Completion

;;; Commentary:

;;; Code:

(use-package company
  :commands company-complete company-select-next company-select-previous
  :init
  (define-key evil-insert-state-map (kbd "TAB") 'company-complete)
  :config
  (global-company-mode)
  (setq-default
   company-selection-wrap-around t
   company-auto-complete t
   company-transformers '(company-sort-by-occurrence)
   company-frontends '(company-preview-frontend company-pseudo-tooltip-unless-just-one-frontend)
   register-preview-delay nil
   )
  (delete 'company-capf company-backends)
  (define-key company-active-map (kbd "C-j") #'company-select-next)
  (define-key company-active-map (kbd "C-k") #'company-select-previous)
  (dolist (key (list (kbd "TAB") (kbd "<tab>") [tab]))
    (define-key company-active-map key #'company-select-next))
  (dolist (key (list (kbd "RET") (kbd "<return>") [return]))
    (define-key company-active-map key nil))
  )

(provide 'completion)
;;; completion ends here
