;;; ruby --- Ruby and RoR Configuration

;;; Commentary:

;;; Code:

(use-package inf-ruby
  :mode ("\\.rb\\'" . ruby-mode)
  )
  

(use-package rspec-mode
  :mode ("\\.rb\\'" . ruby-mode)
  :config
  (custom-set-variables
   '(rspec-use-bundler-when-possible nil)
   '(rspec-use-rake-when-possible nil)
   '(rspec-use-spring-when-possible t)
   )
  (evil-leader/set-key-for-mode 'rspec-verifiable-mode
    "tv" 'rspec-verify
    "ta" 'rspec-verify-all
    "tt" 'rspec-toggle-spec-and-target
    "te" 'rspec-toggle-spec-and-target-find-example
    "tr" 'rspec-rerun
    "tm" 'rspec-verify-matching
    "tc" 'rspec-verify-continue
    "ts" 'rspec-verify-method
    "tf" 'rspec-run-last-failed
    )
  (evil-leader/set-key-for-mode 'rspec-mode-keymap
    "ts" 'rspec-verify-single
    "td" 'rspec-toggle-example-pendingness
    )
  )


(provide 'ruby)
;;; ruby ends here
