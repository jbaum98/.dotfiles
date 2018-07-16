;;; jw-core-keybindings.el --- A humble take on Spacemacs  -*- lexical-binding: t; -*-

;;; Commentary:
;;

;;; Code:

(eval-when-compile
  (require 'use-package))

(defvar jw-leader-map (make-sparse-keymap)
  "Base keymap for all leader key commands.")

(defvar jw-leader-key "SPC"
  "The leader key in Evil normal, visual and motion states.")

(defvar jw-emacs-leader-key "M-m"
  "The leader key accessible in the Evil Emacs and insert states.")

(defvar jw-ex-command-key ";"
  "The key used for Vim Ex commands.")

(defvar jw-command-key "SPC"
  "The key used for Emacs commands (after pressing on the leader key).")

;; Use which-key to show what keybindings are available.
(use-package which-key
  :ensure
  :demand
  :commands
  which-key-mode
  which-key-add-key-based-replacements
  which-key-add-major-mode-key-based-replacements
  :init
  (setf which-key-idle-delay 0.5
        which-key-popup-type 'minibuffer)
  :config
  (which-key-mode))

;; Instantly show in-progress key combinations.
(setf echo-keystrokes 0.02)

;; Make <ESC> key quit as much as possible
(define-key minibuffer-local-map
  (kbd "<escape>") 'keyboard-escape-quit)
(define-key minibuffer-local-ns-map
  (kbd "<escape>") 'keyboard-escape-quit)
(define-key minibuffer-local-completion-map
  (kbd "<escape>") 'keyboard-escape-quit)
(define-key minibuffer-local-must-match-map
  (kbd "<escape>") 'keyboard-escape-quit)
(define-key minibuffer-local-isearch-map
  (kbd "<escape>") 'keyboard-escape-quit)

;; Here's how we declare prefixes so ~which-key~ can tell us about them.
(defun jw-declare-prefix (prefix name)
  "Declare a which-key PREFIX.
PREFIX is a string describing a key sequence.  NAME is a string
used as the prefix command."
  (declare (indent defun))
  (let* ((full-prefix (concat jw-leader-key " " prefix))
         (full-prefix-emacs (concat jw-emacs-leader-key " " prefix)))
    (which-key-add-key-based-replacements
      full-prefix-emacs name
      full-prefix name)))

(defun jw/declare-prefix-for-mode (mode prefix name)
  "Declare a which-key prefix PREFIX for MODE.
MODE is the mode in which this prefix command should be added.
PREFIX is a string describing a key sequence.
NAME is a symbol name used as the prefix command."
  (declare (indent defun))
  (let ((full-prefix (concat jw-leader-key " " prefix))
        (full-prefix-emacs (concat jw-emacs-leader-key " " prefix)))
    (which-key-add-major-mode-key-based-replacements mode
      full-prefix-emacs name
      full-prefix name)))

;; Here's how we set up keymaps for specific modes that we can get to
;; with <leader>m.
(defun jw//init-major-mode-map (mode)
  "Return a keymap for major MODE that's activated by the leader keys."
  (let* ((mode-map-sym (intern (format "%s-map" mode)))
         (jw-map-sym (intern (format "jw-%s-map" mode)))
         jw-map-val)

    ;; Use existing keymap if it exists.
    (unless (boundp jw-map-sym)
      (set jw-map-sym (make-sparse-keymap)))
    (setq jw-map-val (symbol-value jw-map-sym))

    (with-eval-after-load 'evil
      `(progn
         ;; All evil states with `M-m m'
         (evil-define-key '(normal insert visual operator motion emacs)
           ,mode-map-sym
           (kbd (concat jw-emacs-leader-key " m")) ,jw-map-sym)
         ;; Non inserting evil states with SPC-m
         (evil-define-key '(normal visual operator motion)
           ,mode-map-sym
           (kbd (concat jw-leader-key " m")) ,jw-map-sym)))
    jw-map-val))

;; And finally, here's how we define keybindings.
(defun jw//define-keys (keymap key def &rest bindings)
  "In KEYMAP define KEY to DEF as well as all BINDINGS.
`kbd' is applied to all KEYs.  BINDINGS is additional KEY-DEF pairs.
Always defines C-g as `keyboard-quit'."
  (declare (indent 1))
  (define-key keymap (kbd "C-g") 'keyboard-quit)
  (while key
    (define-key keymap (kbd key) def)
    (setf key (pop bindings)
          def (pop bindings))))

(defun jw/define-leader-keys (key def &rest bindings)
  "Set KEY to DEF in `jw-leader-map'.
BINDINGS is additional key-definition pairs.  `kbd' is used for
every KEY."
  (declare (indent 0))
  (apply 'jw//define-keys jw-leader-map key def bindings))

(defun jw/define-leader-keys-for-major-mode (mode key def &rest bindings)
  "Add KEY and DEF as key bindings in major-MODE.
The keymap used for KEY is activated by SPC-m and under `M-m m'
for the major-mode MODE.
BINDINGS are additions KEY-DEF pairs. `kbd' is applied to every KEY."
  (declare (indent defun))
  (apply 'jw//define-keys (jw//init-major-mode-map mode) key def bindings))

(with-eval-after-load "which-key"
  (mapc (lambda (x) (apply #'jw-declare-prefix x))
        '((","   "leader")
          ("a"   "applications")
          ("as"  "shells")
          ("b"   "buffers")
          ("c"   "compile/comments")
          ("C"   "capture/colors")
          ("e"   "errors")
          ("f"   "files")
          ("fC"  "files/convert")
          ("fe"  "emacs")
          ("fv"  "variables")
          ("g"   "git/versions-control")
          ("h"   "help")
          ("hd"  "help-describe")
          ("i"   "insertion")
          ("j"   "jump/join/split")
          ("k"   "lisp")
          ("n"   "narrow/numbers")
          ("q"   "quit")
          ("s"   "search/symbol")
          ("sa"  "ag")
          ("sg"  "grep")
          ("sk"  "ack")
          ("sr"  "ripgrep")
          ("t"   "toggles")
          ("tC"  "colors")
          ("tE"  "editing-styles")
          ("th"  "highlight")
          ("tm"  "modeline")
          ("T"   "UI toggles/themes")
          ("C-t" "other toggles")
          ("w"   "windows")
          ("wp"  "popup")
          ("x"   "text")
          ("xa"  "align")
          ("xd"  "delete")
          ("xl"  "lines")
          ("xm"  "move")
          ("xw"  "words")
          ("z"   "zoom"))))

;; General purpose
(with-eval-after-load "which-key"
  (jw/define-leader-keys
    "u" 'universal-argument
    "!" 'shell-command))

;; Buffers
(use-package jw-funcs-buffer
  :ensure nil
  :bind
  (:map jw-leader-map
        ("TAB" . jw/alternate-buffer)
        ("bd"  . jw/kill-this-buffer)
        ("be"  . jw/safe-erase-buffer)
        ("bn"  . next-buffer)
        ("bm"  . jw/goto-message-buffer)
        ("bN"  . jw/new-empty-buffer)
        ("bP"  . jw/copy-clipboard-to-whole-buffer)
        ("bp"  . previous-buffer)
        ("bR"  . jw/safe-revert-buffer)
        ("bs"  . jw/switch-to-scratch-buffer)
        ("bY"  . jw/copy-whole-buffer-to-clipboard)
        ("bw"  . read-only-mode)
        ("b1"  . buffer-to-window-1)
        ("b2"  . buffer-to-window-2)
        ("b3"  . buffer-to-window-3)
        ("b4"  . buffer-to-window-4)
        ("b5"  . buffer-to-window-5)
        ("b6"  . buffer-to-window-6)
        ("b7"  . buffer-to-window-7)
        ("b8"  . buffer-to-window-8)
        ("b9"  . buffer-to-window-9)))

;; Errors
(use-package jw-funcs-error
  :ensure nil ; local package
  :bind
  (:map jw-leader-map
        ("en" . jw/next-error)
        ("ep" . jw/previous-error)))

;; Files
(use-package jw-funcs-file
  :ensure nil ; local package
  :bind
  (:map jw-leader-map
        ("fc"  . jw/copy-file)
        ("fD"  . jw/delete-current-buffer-file)
        ("fei" . jw/find-user-init-file)
        ("fed" . jw/find-user-init-file)
        ("feD" . jw/ediff-dotfile-and-template)
        ("fev" . jw/display-and-copy-emacs-version)
        ("fCd" . jw/unix2dos)
        ("fCu" . jw/dos2unix)
        ("fG"  . rgrep)
        ("ff"  . find-file)
        ("fl"  . find-file-literally)
        ("fE"  . jw/sudo-edit)
        ("fo"  . jw/open-file-or-directory-in-external-app)
        ("fR"  . jw/rename-current-buffer-file)
        ("fS"  . evil-write-all)
        ("fs"  . save-buffer)
        ("fvd" . add-dir-local-variable)
        ("fvf" . add-file-local-variable)
        ("fvp" . add-file-local-variable-prop-line)
        ("fy"  . jw/show-and-copy-buffer-filename)))

;; Help
(with-eval-after-load "which-key"
  (jw/define-leader-keys
    "hdb" 'describe-bindings
    "hdc" 'describe-char
    "hdf" 'describe-function
    "hdk" 'describe-key
    "hdp" 'describe-package
    "hdt" 'describe-theme
    "hdv" 'describe-variable
    "hN"  'view-emacs-news))

;; Navigation and Jumping
(with-eval-after-load "which-key"
  (jw/define-leader-keys
    "jf" 'find-function
    "jv" 'find-variable))

(use-package jw-funcs-compilation
  :ensure nil ; local package
  :bind
  (:map jw-leader-map
        ("cC" . compile)
        ("ck" . kill-compilation)
        ("cr" . recompile)
        ("cd" . jw/close-compilation-window)))

;; Narrow and widen
(with-eval-after-load "which-key"
  (jw/define-leader-keys
    "nr" 'narrow-to-region
    "np" 'narrow-to-page
    "nf" 'narrow-to-defun
    "nw" 'widen))

;; Windows
(use-package jw-funcs-window
  :ensure nil ; local package
  :bind
  (:map jw-leader-map
        ("w TAB"  . jw/alternate-window)
        ("w2"  . jw/layout-double-columns)
        ("w3"  . jw/layout-triple-columns)
        ("wb"  . jw/switch-to-minibuffer-window)
        ("wd"  . jw/delete-window)
        ("wt"  . jw/toggle-current-window-dedication)
        ("wf"  . follow-mode)
        ("wF"  . make-frame)
        ("wH"  . evil-window-move-far-left)
        ("wh"  . evil-window-left)
        ("wJ"  . evil-window-move-very-bottom)
        ("wj"  . evil-window-down)
        ("wK"  . evil-window-move-very-top)
        ("wk"  . evil-window-up)
        ("wL"  . evil-window-move-far-right)
        ("wl"  . evil-window-right)
        ("wm"  . jw/toggle-maximize-buffer)
        ("wo"  . other-frame)
        ("wr"  . jw/rotate-windows-forward)
        ("wR"  . jw/rotate-windows-backward)
        ("ws"  . split-window-below)
        ("wS"  . split-window-below-and-focus)
        ("w-"  . split-window-below)
        ("wv"  . split-window-right)
        ("wV"  . split-window-right-and-focus)
        ("ww"  . other-window)
        ("w/"  . split-window-right)
        ("w="  . balance-windows)
        ("w+"  . jw/window-layout-toggle)
        ("w_"  . jw/maximize-horizontally)))

(use-package winner
  :demand
  :commands winner-mode
  :bind
  (:map jw-leader-map
        ("wU"  . winner-redo)
        ("wu"  . winner-undo))
  :config
  (winner-mode))

;; Alignment
(use-package jw-funcs-align
  :ensure nil ; local package
  :bind
  (:map jw-leader-map
        ("xa&" . jw/align-repeat-ampersand)
        ("xa(" . jw/align-repeat-left-paren)
        ("xa)" . jw/align-repeat-right-paren)
        ("xa," . jw/align-repeat-comma)
        ("xa." . jw/align-repeat-decimal)
        ("xa:" . jw/align-repeat-colon)
        ("xa;" . jw/align-repeat-semicolon)
        ("xa=" . jw/align-repeat-equal)
        ("xa\\" . jw/align-repeat-backslash)
        ("xaa" . align)
        ("xac" . align-current)
        ("xam" . jw/align-repeat-math-oper)
        ("xar" . jw/align-repeat)
        ("xa|" . jw/align-repeat-bar)
        ("xc"  . count-region)
        ("xdw" . delete-trailing-whitespace)
        ("xjc" . set-justification-center)
        ("xjf" . set-justification-full)
        ("xjl" . set-justification-left)
        ("xjn" . set-justification-none)
        ("xjr" . set-justification-right)
        ("xlc" . jw/sort-lines-by-column)
        ("xlC" . jw/sort-lines-by-column-reverse)
        ("xld" . jw/duplicate-line-or-region)
        ("xls" . jw/sort-lines)
        ("xlS" . jw/sort-lines-reverse)
        ("xlu" . jw/uniquify-lines)
        ("xtc" . transpose-chars)
        ("xtl" . transpose-lines)
        ("xtw" . transpose-words)
        ("xU"  . upcase-region)
        ("xu"  . downcase-region)
        ("xwc" . jw/count-words-analysis)
        ("x TAB" . indent-rigidly)))

(with-eval-after-load "which-key"
  (jw/define-leader-keys
    "qq" 'save-buffers-kill-emacs))

(provide 'jw-core-keybindings)
;;; jw-core-keybindings.el ends here
