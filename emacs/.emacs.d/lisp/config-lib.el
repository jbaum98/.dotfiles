(defun jakemaks/tangle-section-canceled ()
  "Check if the previous section header was CANC."
  (save-excursion
    (when (re-search-backward "^\\*+\\s-+\\(.*?\\)?\\s-*$" nil t)
      (string-prefix-p "CANC" (match-string 1)))))

(defun jakemaks/tangle-config-org (orgfile elfile)
  "Tange source blocks from ORGFILE to ELFILE.

Writes all source blocks that are not marked as =:tangle no=,
have a source-code of =emacs-lisp=, and doesn't have the
todo-marker CANC."
  (let* (;; list where we cobble together body parts
         (body-list ())
         ;; disable special file handlers when loading .org files
         (file-name-handler-alist nil)
         ;; monster-regexp to extract pieces out of an .org file
         (org-babel-src-block-regexp (concat
                                      ;; (1) indentation                 (2) lang
                                      "^\\([ \t]*\\)#\\+begin_src[ \t]+\\([^ \f\t\n\r\v]+\\)[ \t]*"
                                      ;; (3) switches
                                      "\\([^\":\n]*\"[^\"\n*]*\"[^\":\n]*\\|[^\":\n]*\\)"
                                      ;; (4) header arguments
                                      "\\([^\n]*\\)\n"
                                      ;; (5) body
                                      "\\([^\000]*?\n\\)??[ \t]*#\\+end_src")))
    (with-temp-buffer
      (insert-file-contents orgfile)
      (goto-char (point-min))
      (while (re-search-forward org-babel-src-block-regexp nil t)
        (let ((lang (match-string 2))
              (args (match-string 4))
              (body (match-string 5))
              (canc (jakemaks/tangle-section-canceled)))
          (when (and (string= lang "emacs-lisp")
                     (not (string-match-p ":tangle\\s-+no" args))
                     (not canc))
            (add-to-list 'body-list body)))))
    (with-temp-file elfile
      (apply 'insert (reverse body-list)))))

(defun jakemaks/ensure-config ()
  "Tangle configuration if necessary."
  (let* ((buf-name "emacs-config-compile")
         (config-org "~/.emacs.d/lisp/config.org")
         (config-el (concat (file-name-sans-extension config-org) ".el"))
         (config-elc (concat (file-name-sans-extension config-org) ".elc")))
    (when (file-newer-than-file-p config-org config-elc)
      (when (file-exists-p config-elc) (delete-file config-elc))
      (when (file-newer-than-file-p config-org config-el)
        (jakemaks/tangle-config-org config-org config-el)))
    (require 'config)
    (unless (file-exists-p config-elc)
      (start-process "emacs-config-compile" "emacs-config-compile" "emacs" "-Q" "--batch" "-l" config-el "-f" "batch-byte-compile" config-el)
      (display-buffer-at-bottom (get-buffer "emacs-config-compile") nil))))

(provide 'config-lib)
