;;; jw-lang-coq --- Coq

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))
(require 'jw-core-keybindings)

(use-package proof-site
  :no-require
  :commands coq-mode
  :load-path "~/.nix-profile/share/emacs/site-lisp/ProofGeneral/generic"
  :mode ("\\.v\\'" . coq-mode))

(use-package company-coq
  :ensure
  :no-require
  :requires proof-site
  :commands 'company-coq-initialize
  :hook (coq-mode . company-coq-mode)
  :config
  (custom-set-faces
   '(proof-eager-annotation-face ((t (:background "medium blue"))))
   '(proof-error-face ((t (:background "dark red"))))
   '(proof-warning-face ((t (:background "indianred3")))))
  (jw/define-leader-keys-for-major-mode 'coq-mode
    "n" 'proof-assert-next-command-interactive
    "]" 'proof-assert-next-command-interactive
    "u" 'proof-undo-last-successful-command
    "[" 'proof-undo-last-successful-command
    "h" 'company-coq-doc
    "ll" 'proof-layout-windows
    "lp" 'proof-prf
    "x" 'proof-shell-exit
    "s" 'proof-find-theorems
    "?" 'coq-Check
    "p" 'coq-Print
    ";" 'pg-insert-last-output-as-comment
    "o" 'company-coq-occur
    "." 'proof-goto-point)
  :custom
  (proof-three-window-mode-policy 'hybrid))

(provide 'jw-lang-coq)
;;; jw-lang-coq.el ends here
