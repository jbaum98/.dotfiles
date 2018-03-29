;;; init.el --- Emacs Initialization File
;;
;; Copyright (c) 2018 Jake Waksbaum
;;
;; Author: Jake Waksbaum <jake.waksbaum@gmail.com>
;; URL: https://github.com/jbaum98/.dotfiles
;;
;; This file is not part of GNU Emacs.

;; Make startup faster by reducing the frequency of garbage collection.

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
;; (package-initialize)

(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs ready in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))

(setq gc-cons-threshold most-positive-fixnum)
(add-hook 'after-init-hook
          #'(lambda () (setq gc-cons-threshold
                        (car (get 'gc-cons-threshold 'standard-value)))))

(add-to-list 'load-path "~/.emacs.d/lisp")
(setq load-prefer-newer t)
(require 'config-lib)
(jakemaks/ensure-config)
