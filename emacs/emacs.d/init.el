;;; init.el -- Get the ball rolling  -*- lexical-binding: t; -*-

;;; Commentary:
;;

;;; Code:

;; Let 'em know who we are.
(setf user-full-name "Jake Waksbaum"
      user-mail-address "jake.waksbaum@gmail.com")

;; Let us know how fast our startup us
;; Use a hook so the message doesn't get clobbered by other messages.
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs ready in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))

;; Increase the ~gc-cons-threshold~ to avoid garbage collections
;; during startup.
(defvar file-name-handler-alist-backup
  file-name-handler-alist)

(setf gc-cons-threshold most-positive-fixnum
      file-name-handler-alist nil)

(add-hook 'after-init-hook
          (lambda ()
            (garbage-collect)
            (setf gc-cons-threshold (car (get 'gc-cons-threshold 'standard-value))
                  file-name-handler-alist (append
                                           file-name-handler-alist-backup
                                           file-name-handler-alist))))

;; Setup use-package the way we like.

(eval-when-compile
  (defvar use-package-enable-imenu-support))

(setf use-package-enable-imenu-support 't)

(require 'bind-key)
(eval-when-compile
  (require 'use-package)
  (use-package use-package
    :ensure))

(setf use-package-expand-minimally 't)

;; Bring in the rest of our config
;; This uses byte-compiling magic to ensure that this path is
;; computed relative to the location at compile-time.
(eval-and-compile
  (defvar jw-lisp-directory
    (if load-file-name
        (expand-file-name "lisp" (file-name-directory load-file-name))
      (expand-file-name "lisp")))

  (add-to-list 'load-path (expand-file-name "core" jw-lisp-directory))
  (add-to-list 'load-path (expand-file-name "funcs" jw-lisp-directory))
  (add-to-list 'load-path (expand-file-name "lang" jw-lisp-directory)))

;; Require all core parts
(require 'jw-core-lib)
(require 'jw-core-emacs-settings)
(require 'jw-core-ui)
(require 'jw-core-keybindings)
(require 'jw-core-evil)
(when (bound-and-true-p IS-DARWIN)
  (require 'jw-core-darwin))
(require 'jw-core-autocomplete)
(require 'jw-core-ivy)
(require 'jw-core-org)

(require 'jw-lang-aurora)
(require 'jw-lang-coq)
(require 'jw-lang-go)
(require 'jw-lang-haskell)
(require 'jw-lang-java)
(require 'jw-lang-markdown)
(require 'jw-lang-nix)
(require 'jw-lang-rust)
(require 'jw-lang-sql)

(provide 'init)
;;; init.el ends here
