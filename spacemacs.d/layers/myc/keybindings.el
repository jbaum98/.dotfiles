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
      (setq-local compile-command old-compile-command))))
