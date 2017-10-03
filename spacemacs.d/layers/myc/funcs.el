;;;###autoload
(defun myc/courselab-setup ()
  (if buffer-file-name
      (let ((hostname (file-remote-p buffer-file-name 'host)))
        (if (and hostname (equal hostname "courselab.cs.princeton.edu"))
            (setq-local
             compile-command
             (let* ((file-name (file-name-nondirectory (file-remote-p buffer-file-name 'localname)))
                    (out-name (file-name-sans-extension file-name)))
               (if (or (file-exists-p "makefile")
                       (file-exists-p "Makefile"))
                   (concat "make -k " (shell-quote-argument out-name))
                 (concat "gcc217 "
                         (shell-quote-argument file-name)
                         " -o "
                         (shell-quote-argument out-name)))))))))

;;;###autoload
(defun myc/flycheck-splint-setup ()
  (defcustom flycheck-splint-arguments '("-showfunc" "-hints" "+quiet")
    "Argument to pass to splint.")

  (flycheck-define-checker splint
    "A checker using splint.
See `http://www.splint.org/'."
    :command ("splint"
              (eval flycheck-splint-arguments)
              source)
    :error-patterns
    ((warning line-start (file-name) ":" line ":" column ":"
              (message (minimal-match (one-or-more anything)))
              line-end)
     (warning line-start (file-name) "(" line "," column "):"
              (message (minimal-match (one-or-more anything)))
              line-end)
     (warning line-start (file-name) ":" line ":"
              (message (minimal-match (one-or-more anything)))
              line-end)
     (warning line-start (file-name) "(" line "):"
              (message (minimal-match (one-or-more anything)))
              line-end))
    :modes c-mode)

  (add-to-list 'flycheck-checkers 'splint 'append)
  (mapc
   (lambda (checker) (flycheck-add-next-checker checker '(error . splint)))
   '(c/c++-clang c/c++-gcc))
  )
