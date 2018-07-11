;;; init --- Summary
;;; Commentary:
;;; Code:

(setq package-enable-at-startup nil)

  ;; Use a hook so the message doesn't get clobbered by other messages.
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs ready in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                               (time-subtract after-init-time before-init-time)))
                     gcs-done)))


(eval-and-compile
  (add-to-list 'load-path
               (eval-when-compile (expand-file-name "lisp"))))

(require 'config-packages)

(provide 'init)
;;; init.el ends here
