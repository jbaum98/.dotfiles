(setq-default
 org-src-fontify-natively t
 org-src-tab-acts-natively t
 org-latex-listings 'minted
 org-latex-to-pdf-process "latexmk -xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"
 )


(add-hook 'org-babel-after-execute-hook 'org-display-inline-images)
(add-hook 'org-babel-after-execute-hook 'spacemacs/toggle-typographic-substitutions-on)

;;;###autoload
(defun jk-org-kwds ()
  "parse the buffer and return a cons list of (property . value)
from lines like:
#+PROPERTY: value"
  (org-element-map (org-element-parse-buffer 'element) 'keyword
    (lambda (keyword) (cons (org-element-property :key keyword)
                            (org-element-property :value keyword)))))

;;;###autoload
(defun jk-org-kwd (KEYWORD)
  "get the value of a KEYWORD in the form of #+KEYWORD: value"
  (cdr (assoc KEYWORD (jk-org-kwds))))

;;;###autoload
(defun org-export-filter-timestamp-remove-brackets (timestamp backend info)
  "removes relevant brackets from a timestamp"
  (cond
   ((org-export-derived-backend-p backend 'latex)
    (replace-regexp-in-string "[<>]\\|[][]" "" timestamp))
   ((org-export-derived-backend-p backend 'html)
    (replace-regexp-in-string "&[lg]t;\\|[][]" "" timestamp))))

(eval-after-load 'ox '(add-to-list
                       'org-export-filter-timestamp-functions
                       'org-export-filter-timestamp-remove-brackets))

(defun flatten (LIST)
  "flattens LIST"
  (cond
   ((atom LIST) (list LIST))
   ((null (cdr LIST)) (flatten (car LIST)))
   (t (append (flatten (car LIST)) (flatten (cdr LIST)))))) 


;;;###autoload
(defun orgtbl-to-booktabs (table params)
  "Convert the orgtbl-mode TABLE to LaTeX booktabs."
  (orgtbl-to-generic
   table
   (org-combine-plists
    '(:lstart "" :lend " \\\\" :sep " & "
              :hline "\\midrule" :efmt "%se%s" :fmt "\\num{%s}"
              )
    params))) 
