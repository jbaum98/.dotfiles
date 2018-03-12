(require 'flycheck)

(provide 'javac-flycheck)

  (defun fix-buf-offsets (errors)
    (seq-do (lambda (err)
              (let ((region (flycheck-error-region err)))
                (when region
                  (let ((start (car region))
                        (end   (cdr region)))
                    (setf (flycheck-error-region err)
                          (cond
                           ((not end)
                            (cons (+ start 1) (+ start 2)))
                           ((= start end)
                            (cons (+ start 1) (+ end 2)))
                           (t
                            (cons (+ start 1) (+ end 1)))))))))
            errors)
    errors)

  (flycheck-define-checker java/javac-mod
    "A Java syntax and style checker using javac."
    :command ("java" "-jar" "/Users/jake/.emacs.d/private/Compiler.jar" source-original)
    :error-patterns
    ((error line-start "E:" (file-name) ":" region-start ":" region-end ":" (message))
     (error line-start "E:" (file-name) ":" region-start ":" (message))
     (warning line-start "W:" (file-name) ":" region-start ":" region-end ":" (message))
     (warning line-start "W:" (file-name) ":" region-start ":" (message)))
    :error-filter fix-buf-offsets
    :modes java-mode
    :predicate flycheck-buffer-saved-p
    )

  (add-to-list 'flycheck-checkers 'java/javac-mod)
