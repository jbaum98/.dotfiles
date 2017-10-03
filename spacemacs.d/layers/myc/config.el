(defun courselab-setup ()
  (if buffer-file-name
      (let ((hostname (file-remote-p buffer-file-name 'host)))
        (if (and hostname
                 (equal hostname "courselab.cs.princeton.edu"))
            (setq-local compile-command
                        (let* ((file-name (file-name-nondirectory (file-remote-p buffer-file-name 'localname)))
                               (out-name (file-name-sans-extension file-name)))
                          (if (or (file-exists-p "makefile")
                                  (file-exists-p "Makefile"))
                              (concat "make -k "
                                      (shell-quote-argument out-name))
                            (concat "gcc217 "
                                    (shell-quote-argument file-name)
                                    " -o "
                                    (shell-quote-argument out-name)))))))))

(add-hook 'c-mode-hook 'courselab-setup 't)
