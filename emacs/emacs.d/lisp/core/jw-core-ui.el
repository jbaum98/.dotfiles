;;; jw-core-ui --- Make it look nice  -*- lexical-binding: t; -*-

;;; Commentary:

;; Here we make Emacs look the way we want. Everything in this file
;; will be loaded on demand, which means there should be very few
;; packages here.

;;; Code:

(eval-when-compile
  (require 'use-package))

(defvar after-make-console-frame-hooks '()
  "Hooks to run after creating a new TTY frame.")

(defvar after-make-window-system-frame-hooks '()
  "Hooks to run after creating a new window system frame.")

(defun run-after-make-frame-hooks (frame)
  "Run configured hooks in response to the newly-created FRAME.
Selectively runs either `after-make-console-frame-hooks' or
`after-make-window-system-frame-hooks'"
  (with-selected-frame frame
    (run-hooks (if window-system
                   'after-make-window-system-frame-hooks
                 'after-make-console-frame-hooks))))

(add-hook 'after-make-frame-functions 'run-after-make-frame-hooks)

(defconst jw/initial-frame (selected-frame)
  "The frame (if any) active during Emacs initialization.")

(add-hook 'after-init-hook
          (lambda () (when jw/initial-frame
                  (run-after-make-frame-hooks jw/initial-frame))))

;; Enable the mouse even in the terminal.
(add-hook 'after-make-console-frame-hooks 'xterm-mouse-mode)
(add-hook 'after-make-window-system-frame-hooks
          (lambda ()
            (tool-bar-mode -1)
            (scroll-bar-mode -1)
            (blink-cursor-mode -1)
            (menu-bar-mode -1)))

(add-hook 'after-make-window-system-frame-hooks
          (lambda ()
            (let ((face "Iosevka 14"))
              (add-to-list 'default-frame-alist (cons 'font face))
              (set-frame-font face 'keep-size))))

;; Go straight to the scratch buffer.
(setf inhibit-splash-screen t
      initial-scratch-message "")

;; Control over modes displayed in the modeline.
(use-package diminish
  :ensure
  :commands diminsh)

;; Show the column number in the modeline.
(column-number-mode t)

;; Solarized is easy on the eyes.
(use-package solarized-theme
  :ensure
  :config (load-theme 'solarized-light t))

;; Display pretty symbols
(use-package prog-mode
  :defer 3
  :commands global-prettify-symbols-mode
  :config
  (setq-default prettify-symbols-unprettify-at-point 'right-edge)
  (global-prettify-symbols-mode))

(provide 'jw-core-ui)
;;; jw-core-ui.el ends here
