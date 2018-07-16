;;; jw-core-ui --- Make it look nice  -*- lexical-binding: t; -*-

;;; Commentary:

;; Here we make Emacs look the way we want. Everything in this file
;; will be loaded on demand, which means there should be very few
;; packages here.

;;; Code:

(eval-when-compile
  (require 'use-package))

;; We want to hide pretty much everything but the text.
(when window-system
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (blink-cursor-mode -1))

(when (not (memq window-system '(mac ns)))
  (menu-bar-mode -1))

;; Enable the mouse even in the terminal.
(when (not window-system)
  (xterm-mouse-mode 1))

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

;;; Fonts
(when window-system
  (let ((face "Iosevka 14"))
    (add-to-list 'default-frame-alist (cons 'font face))
    (set-frame-font face 'keep-size)))

;; Display pretty symbols
(use-package prog-mode
  :defer 3
  :commands global-prettify-symbols-mode
  :config
  (setf prettify-symbols-unprettify-at-point 'right-edge)
  (global-prettify-symbols-mode))

(provide 'jw-core-ui)
;;; jw-core-ui.el ends here
