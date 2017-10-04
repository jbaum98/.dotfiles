(spacemacs/set-leader-keys-for-major-mode 'c-mode
  "c"
  (lambda ()
    (interactive)
    (compile compile-command))
  "s"
  (lambda ()
    (interactive)
    (let ((old-compile-command compile-command))
      (compile (combine-and-quote-strings (list "splint"
                                                (file-remote-p buffer-file-name 'localname))))
      (setq-local compile-command old-compile-command)))
  "r"
  (lambda ()
    (interactive)
    (let ((old-compile-command compile-command))
      (compile (combine-and-quote-strings (list "critTer"
                                                (file-name-nondirectory (file-remote-p buffer-file-name 'localname)))))
      (setq-local compile-command old-compile-command)))
  "t"
  (lambda ()
    (interactive)
    (let ((old-compile-command compile-command))
      (compile (combine-and-quote-strings (list "make" "test"
                                                (file-name-nondirectory (file-remote-p buffer-file-name 'localname)))))
      (setq-local compile-command old-compile-command)))
  )
