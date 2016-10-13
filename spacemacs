;; -*- mode: emacs-lisp -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

(defun dotspacemacs/layers ()
  "Configuration Layers declaration.
You should not put any user code in this function besides modifying the variable
values."
  (setq-default
   ;; Base distribution to use. This is a layer contained in the directory
   ;; `+distribution'. For now available distributions are `spacemacs-base'
   ;; or `spacemacs'. (default 'spacemacs)
   dotspacemacs-distribution 'spacemacs-base
   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (i.e. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path '()
   ;; List of configuration layers to load. If it is the symbol `all' instead
   ;; of a list then all discovered layers will be installed.
   dotspacemacs-configuration-layers
   (append (if (eq system-type 'darwin) '(osx) '())
   '(
     ;; ----------------------------------------------------------------
     ;; Example of useful layers you may want to use right away.
     ;; Uncomment some layer names and press <SPC f e R> (Vim style) or
     ;; <M-m f e R> (Emacs style) to install them.
     ;; ----------------------------------------------------------------
     auto-completion
     semantic
     git
     (shell :variables
            shell-default-height 30
            shell-default-position 'bottom)
     syntax-checking
     version-control
     c-c++
     ;clojure
     emacs-lisp
     extra-langs
     haskell
     ;go
     html
     helm
     java
     myjava
     javascript
     latex
     markdown
     (org :variables
          org-enable-github-support t)
     python
     pdf-tools
     (ruby :variables
           ruby-test-runner 'rspec)
     ;react
     ruby-on-rails
     ;rust
     shell-scripts
     ;swift
     yaml
     spacemacs-layouts
     typography
     ))
   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages then consider to create a layer, you can also put the
   ;; configuration in `dotspacemacs/config'.
   dotspacemacs-additional-packages '()
   ;; A list of packages and/or extensions that will not be install and loaded.
   dotspacemacs-excluded-packages '()
   ;; If non-nil spacemacs will delete any orphan packages, i.e. packages that
   ;; are declared in a layer which is not a member of
   ;; the list `dotspacemacs-configuration-layers'. (default t)
   dotspacemacs-delete-orphan-packages t))

(defun dotspacemacs/init ()
  "Initialization function.
This function is called at the very startup of Spacemacs initialization
before layers configuration.
You should not put any user code in there besides modifying the variable
values."
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
   ;; If non nil ELPA repositories are contacted via HTTPS whenever it's
   ;; possible. Set it to nil if you have no way to use HTTPS in your
   ;; environment, otherwise it is strongly recommended to let it set to t.
   ;; This variable has no effect if Emacs is launched with the parameter
   ;; `--insecure' which forces the value of this variable to nil.
   ;; (default t)
   dotspacemacs-elpa-https t
   ;; Maximum allowed time in seconds to contact an ELPA repository.
   dotspacemacs-elpa-timeout 5
   ;; If non nil then spacemacs will check for updates at startup
   ;; when the current branch is not `develop'. (default t)
   dotspacemacs-check-for-update t
   ;; One of `vim', `emacs' or `hybrid'. Evil is always enabled but if the
   ;; variable is `emacs' then the `holy-mode' is enabled at startup. `hybrid'
   ;; uses emacs key bindings for vim's insert mode, but otherwise leaves evil
   ;; unchanged. (default 'vim)
   dotspacemacs-editing-style 'vim
   ;; If non nil output loading progress in `*Messages*' buffer. (default nil)
   dotspacemacs-verbose-loading nil
   ;; Specify the startup banner. Default value is `official', it displays
   ;; the official spacemacs logo. An integer value is the index of text
   ;; banner, `random' chooses a random text banner in `core/banners'
   ;; directory. A string value must be a path to an image format supported
   ;; by your Emacs build.
   ;; If the value is nil then no banner is displayed. (default 'official)
   dotspacemacs-startup-banner nil
   ;; List of items to show in the startup buffer. If nil it is disabled.
   ;; Possible values are: `recents' `bookmarks' `projects'.
   ;; (default '(recents projects))
   dotspacemacs-startup-lists '(recents projects)
   ;; Number of recent files to show in the startup buffer. Ignored if
   ;; `dotspacemacs-startup-lists' doesn't include `recents'. (default 5)
   dotspacemacs-startup-recent-list-size 5
   ;; Default major mode of the scratch buffer (default `text-mode')
   dotspacemacs-scratch-mode 'org-mode
   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press <SPC> T n to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(solarized-light
                         solarized-dark
                         spacemacs-dark
                         spacemacs-light
                         leuven
                         monokai
                         zenburn)
   ;; If non nil the cursor color matches the state color.
   dotspacemacs-colorize-cursor-according-to-state t
   ;; Default font. `powerline-scale' allows to quickly tweak the mode-line
   ;; size to make separators look not too crappy.
   dotspacemacs-default-font '("Consolas"
                               :size 14
                               :weight normal
                               :width normal
                               :powerline-scale 1.1)
   ;; The leader key
   dotspacemacs-leader-key "SPC"
   ;; The leader key accessible in `emacs state' and `insert state'
   ;; (default "M-m")
   dotspacemacs-emacs-leader-key "M-m"
   ;; Major mode leader key is a shortcut key which is the equivalent of
   ;; pressing `<leader> m`. Set it to `nil` to disable it. (default ",")
   dotspacemacs-major-mode-leader-key ","
   ;; Major mode leader key accessible in `emacs state' and `insert state'.
   ;; (default "C-M-m)
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"
   ;; These variables control whether separate commands are bound in the GUI to
   ;; the key pairs C-i, TAB and C-m, RET.
   ;; Setting it to a non-nil value, allows for separate commands under <C-i>
   ;; and TAB or <C-m> and RET.
   ;; In the terminal, these pairs are generally indistinguishable, so this only
   ;; works in the GUI. (default nil)
   dotspacemacs-distinguish-gui-tab nil
   ;; (Not implemented) dotspacemacs-distinguish-gui-ret nil
   ;; The command key used for Evil commands (ex-commands) and
   ;; Emacs commands (M-x).
   ;; By default the command key is `:' so ex-commands are executed like in Vim
   ;; with `:' and Emacs commands are executed with `<leader> :'.
   dotspacemacs-emacs-command-key ";"
   ;; If non nil `Y' is remapped to `y$'. (default t)
   dotspacemacs-remap-Y-to-y$ t
   ;; Name of the default layout (default "Default")
   dotspacemacs-default-layout-name "Default"
   ;; If non nil the default layout name is displayed in the mode-line.
   ;; (default nil)
   dotspacemacs-display-default-layout nil
   ;; If non nil then the last auto saved layouts are resume automatically upon
   ;; start. (default nil)
   dotspacemacs-auto-resume-layouts nil
   ;; Location where to auto-save files. Possible values are `original' to
   ;; auto-save the file in-place, `cache' to auto-save the file to another
   ;; file stored in the cache directory and `nil' to disable auto-saving.
   ;; (default 'cache)
   dotspacemacs-auto-save-file-location 'cache
   ;; Maximum number of rollback slots to keep in the cache. (default 5)
   dotspacemacs-max-rollback-slots 5
   ;; If non nil then `ido' replaces `helm' for some commands. For now only
   ;; `find-files' (SPC f f), `find-spacemacs-file' (SPC f e s), and
   ;; `find-contrib-file' (SPC f e c) are replaced. (default nil)
   dotspacemacs-use-ido t
   ;; If non nil, `helm' will try to miminimize the space it uses. (default nil)
   dotspacemacs-helm-resize nil
   ;; if non nil, the helm header is hidden when there is only one source.
   ;; (default nil)
   dotspacemacs-helm-no-header nil
   ;; define the position to display `helm', options are `bottom', `top',
   ;; `left', or `right'. (default 'bottom)
   dotspacemacs-helm-position 'bottom
   ;; If non nil the paste micro-state is enabled. When enabled pressing `p`
   ;; several times cycle between the kill ring content. (default nil)
   dotspacemacs-enable-paste-micro-state nil
   ;; Which-key delay in seconds. The which-key buffer is the popup listing
   ;; the commands bound to the current keystroke sequence. (default 0.4)
   dotspacemacs-which-key-delay 0.4
   ;; Which-key frame position. Possible values are `right', `bottom' and
   ;; `right-then-bottom'. right-then-bottom tries to display the frame to the
   ;; right; if there is insufficient space it displays it at the bottom.
   ;; (default 'bottom)
   dotspacemacs-which-key-position 'bottom
   ;; If non nil a progress bar is displayed when spacemacs is loading. This
   ;; may increase the boot time on some systems and emacs builds, set it to
   ;; nil to boost the loading time. (default t)
   dotspacemacs-loading-progress-bar nil
   ;; If non nil the frame is fullscreen when Emacs starts up. (default nil)
   ;; (Emacs 24.4+ only)
   dotspacemacs-fullscreen-at-startup nil
   ;; If non nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
   ;; Use to disable fullscreen animations in OSX. (default nil)
   dotspacemacs-fullscreen-use-non-native nil
   ;; If non nil the frame is maximized when Emacs starts up.
   ;; Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.
   ;; (default nil) (Emacs 24.4+ only)
   dotspacemacs-maximized-at-startup nil
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's active or selected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-active-transparency 90
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's inactive or deselected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-inactive-transparency 90
   ;; If non nil unicode symbols are displayed in the mode line. (default t)
   dotspacemacs-mode-line-unicode-symbols t
   ;; If non nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters the
   ;; point when it reaches the top or bottom of the screen. (default t)
   dotspacemacs-smooth-scrolling t
   ;; If non nil line numbers are turned on in all `prog-mode' and `text-mode'
   ;; derivatives. If set to `relative', also turns on relative line numbers.
   ;; (default nil)
   dotspacemacs-line-numbers t
   ;; If non-nil smartparens-strict-mode will be enabled in programming modes.
   ;; (default nil)
   dotspacemacs-smartparens-strict-mode nil
   ;; Select a scope to highlight delimiters. Possible values are `any',
   ;; `current', `all' or `nil'. Default is `all' (highlight any scope and
   ;; emphasis the current one). (default 'all)
   dotspacemacs-highlight-delimiters 'all
   ;; If non nil advises quit functions to keep server open when quitting.
   ;; (default nil)
   dotspacemacs-persistent-server nil
   ;; List of search tool executable names. Spacemacs uses the first installed
   ;; tool of the list. Supported tools are `ag', `pt', `ack' and `grep'.
   ;; (default '("ag" "pt" "ack" "grep"))
   dotspacemacs-search-tools '("ag" "pt" "ack" "grep")
   ;; The default package repository used if no explicit repository has been
   ;; specified with an installed package.
   ;; Not used for now. (default nil)
   dotspacemacs-default-package-repository nil
   ;; Delete whitespace while saving buffer. Possible values are `all'
   ;; to aggressively delete empty line and long sequences of whitespace,
   ;; `trailing' to delete only the whitespace at end of lines, `changed'to
   ;; delete only whitespace for changed lines or `nil' to disable cleanup.
   ;; (default nil)
   dotspacemacs-whitespace-cleanup 'all
   ))

(defun dotspacemacs/user-init ()
  "Initialization function for user code.
It is called immediately after `dotspacemacs/init'.  You are free to put any
user code."
  )

(defun dotspacemacs/user-config ()
  "Configuration function for user code.
 This function is called at the very end of Spacemacs initialization after
layers configuration. You are free to put any user code."

  ;; Make evil-mode up/down operate in screen lines instead of logical lines
  (define-key evil-motion-state-map "j" 'evil-next-visual-line)
  (define-key evil-motion-state-map "k" 'evil-previous-visual-line)
  ;; Also in visual mode
  (define-key evil-visual-state-map "j" 'evil-next-visual-line)
  (define-key evil-visual-state-map "k" 'evil-previous-visual-line)

  (add-hook 'c-mode-hook
            (lambda ()
              (if (or (file-exists-p "makefile")
                      (file-exists-p "Makefile"))
                  (set (make-local-variable 'compile-command)
                       (concat "make -k "
                               (file-name-sans-extension buffer-file-name))))))

  (add-hook 'haskell-mode-hook
            (lambda ()
              (set (make-local-variable 'compile-command)
                   (concat "ghc " buffer-file-name))))

  (setq-default shell-default-shell 'eshell)

  (advice-add 'magit-key-mode :filter-args
              (lambda (arguments)
                (if (eq (car arguments) 'pulling)
                    (list 'pulling (list "--rebase"))
                  arguments)))
  (setq-default
   ;; js2-mode
   js2-basic-offset 2
   js-indent-level 2
   ;; web-mode
   css-indent-offset 2
   web-mode-markup-indent-offset 2
   web-mode-css-indent-offset 2
   web-mode-code-indent-offset 2
   web-mode-attr-indent-offset 2)
  (with-eval-after-load 'web-mode
    (add-to-list 'web-mode-indentation-params '("lineup-args" . nil))
    (add-to-list 'web-mode-indentation-params '("lineup-concats" . nil))
    (add-to-list 'web-mode-indentation-params '("lineup-calls" . nil)))

  )
;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(TeX-engine (quote xetex))
 '(exec-path-from-shell-arguments (quote ("-l")))
 '(flycheck-javac-infer-classpath (quote ("." "/opt/stdlib.jar")))
 '(flycheck-locate-config-file-functions
   (quote
    (flycheck-locate-config-file-home flycheck-locate-config-file-ancestor-directories flycheck-locate-config-file-by-path
                                      (lambda
                                        (filename _checker)
                                        (projectile-file-truename filename)))))
 '(haskell-font-lock-symbols t)
 '(hindent-style "gibiansky")
 '(org-babel-load-languages
   (quote
    ((C . t)
     (ruby . t)
     (python . t)
     (gnuplot . t)
     (calc . t)
     (clojure . t)
     (css . t)
     (ditaa . t)
     (emacs-lisp . t)
     (haskell . t)
     (java . t)
     (js . t)
     (latex . t)
     (org . t)
     (sass . t)
     (scala . t)
     (shell . t)
     (sql . t)
     (octave . t))))
 '(org-display-custom-times t)
 '(org-highlight-latex-and-related (quote (latex script entities)))
 '(org-latex-active-timestamp-format "%s")
 '(org-latex-classes
   (quote
    (("article" "\\documentclass[11pt]{article}"
      ("\\section{%s}" . "\\section*{%s}")
      ("\\subsection{%s}" . "\\subsection*{%s}")
      ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
      ("\\paragraph{%s}" . "\\paragraph*{%s}")
      ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
     ("report" "\\documentclass[11pt]{report}"
      ("\\part{%s}" . "\\part*{%s}")
      ("\\chapter{%s}" . "\\chapter*{%s}")
      ("\\section{%s}" . "\\section*{%s}")
      ("\\subsection{%s}" . "\\subsection*{%s}")
      ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))
     ("book" "\\documentclass[11pt]{book}"
      ("\\part{%s}" . "\\part*{%s}")
      ("\\chapter{%s}" . "\\chapter*{%s}")
      ("\\section{%s}" . "\\section*{%s}")
      ("\\subsection{%s}" . "\\subsection*{%s}")
      ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))
     ("lab-report" "\\documentclass[11pt,titlepage]{report}
[NO-DEFAULT-PACKAGES]
[PACKAGES]

\\makeatletter
\\newcommand\\partners[1]{\\renewcommand\\@partners{#1}}
\\newcommand\\@partners{\\@latex@error{No \\noexpand\\partners{} given}\\@ehc}
\\newcommand\\labnumber[1]{\\renewcommand\\@labnumber{#1}}
\\newcommand\\@labnumber{\\@latex@error{No \\noexpand\\labnumber{} given}\\@ehc}


\\renewcommand{\\maketitle}{
\\begin{titlepage}
    \\begin{center}
        \\vspace*{1cm}

        \\Huge
        \\textbf{\\@title}

        \\vspace{0.5cm}
        \\LARGE
        Lab \\#\\@labnumber{}

        \\vspace{1.5cm}

        \\textbf{\\@author}

        \\textbf{\\@partners}

    \\end{center}
\\end{titlepage}

\\LARGE
\\textbf{\\@title}
\\normalsize
}
\\makeatother
"
      ("\\section*{%s}" . "\\section*{%s}")
      ("\\subsection*{%s}" . "\\subsection*{%s}")
      (" \\subsubsection*{%s}" . " \\subsubsection*{%s}"))
     ("homework" "\\documentclass{article}

%\\usepackage[utf8]{inputenc}

\\usepackage{fontspec}
\\usepackage{fancyhdr}
\\usepackage{extramarks}
\\usepackage[fleqn]{amsmath}
\\usepackage{amsthm}
\\usepackage{amsfonts}
\\usepackage{pgf,tikz}
\\usepackage{mathrsfs}
\\usepackage{mathtools}
\\usepackage[plain]{algorithm}
\\usepackage{algpseudocode}
\\usepackage{xparse}
\\usepackage{xstring}
\\usepackage{esvect}
\\usepackage{subcaption}
\\usepackage[makeroom]{cancel}
\\usepackage{siunitx}

\\usetikzlibrary{automata,positioning,arrows}

%
% Basic Document Settings
%

\\topmargin=-0.45in
\\evensidemargin=0in
\\oddsidemargin=0in
\\textwidth=6.5in
\\textheight=9.0in
\\headsep=0.25in

\\linespread{1.1}

\\pagestyle{fancy}
\\lhead{\\hmwkAuthorName}
\\chead{\\hmwkClass\\ (\\hmwkClassInstructor\\ \\hmwkClassTime): \\hmwkTitle}
\\rhead{\\firstxmark}
\\lfoot{\\lastxmark}
\\cfoot{\\thepage}

\\renewcommand\\headrulewidth{0.4pt}
\\renewcommand\\footrulewidth{0.4pt}

\\setlength\\parindent{0pt}

%
% Create Problem Sections
%

\\newcommand{\\enterProblemHeader}[1]{
  \\nobreak\\extramarks{}{Problem #1 continued on next page\\ldots}\\nobreak{}
  \\nobreak\\extramarks{Problem #1 (continued)}{Problem #1 continued on next page\\ldots}\\nobreak{}
}

\\newcommand{\\exitProblemHeader}[1]{
  \\nobreak\\extramarks{Problem #1 (continued)}{Problem #1 continued on next page\\ldots}\\nobreak{}
  \\nobreak\\extramarks{Problem #1}{}\\nobreak{}
}

\\setcounter{secnumdepth}{0}
\\newcounter{partCounter}
\\newcommand{\\homeworkProblemCounter}{}
\\def\\homeworkSection{}

%
% Homework Problem Environment
%
% This environment takes an optional argument. When given, it will adjust the
% problem counter. This is useful for when the problems given for your
% assignment aren't sequential. See the last 3 problems of this template for an
% example.
%
\\DeclareDocumentEnvironment{homeworkProblem}{m o}{
  \\renewcommand{\\homeworkProblemCounter}{#1}
  \\IfValueT{#2} {\\global\\def\\homeworkSection{#2}}
  \\StrLeft{\\homeworkSection}{1}[\\firstchar]
  \\IfInteger{\\firstchar}{\\def\\secchar{§}}{\\def\\secchar{}}
  \\IfValueTF{\\homeworkSection} {
    \\section{\\secchar\\homeworkSection{} \\#\\homeworkProblemCounter}
  } {
    \\section{\\homeworkProblemCounter}
  }
  \\setcounter{partCounter}{1}
  \\enterProblemHeader{\\homeworkProblemCounter}
}{
  \\exitProblemHeader{\\homeworkProblemCounter}
}

%
% Homework Details
% - Title
% - Due date
% - Class
% - Section/Time
% - Instructor
% - Author
%

%
% Title Page
%

\\title{
  \\vspace{2in}
  \\textmd{\\textbf{\\hmwkClass:\\ \\hmwkTitle}}\\\\
  \\normalsize\\vspace{0.1in}\\small{Due\\ on\\ \\hmwkDueDate}\\\\
  \\vspace{0.1in}\\large{\\textit{\\hmwkClassInstructor\\ \\hspace{1.5in} \\hmwkClassTime}}
  \\vspace{3in}
}

\\author{
  \\textbf{\\hmwkAuthorName}
  \\ifcsname hmwkCollaborator\\endcsname
    \\\\ \\textit{Collaborators: \\hmwkCollaborator}
  \\fi
}
\\date{}

\\renewcommand{\\part}[1]{\\textbf{\\large Part \\Alph{partCounter}}\\stepcounter{partCounter}\\\\}

%
% Various Helper Commands
%

% Useful for algorithms
\\newcommand{\\alg}[1]{\\textsc{\\bfseries \\footnotesize #1}}

% For derivatives
\\newcommand{\\deriv}[1]{\\frac{\\mathrm{d}}{\\mathrm{d}x} (#1)}

% For partial derivatives
\\newcommand{\\pderiv}[2]{\\frac{\\partial}{\\partial #1} (#2)}

% Integral dx
\\newcommand{\\dx}{\\mathrm{d}x}

% Alias for the Solution section header
\\newcommand{\\solution}{\\textbf{\\large Solution}}

% Probability commands: Expectation, Variance, Covariance, Bias
\\newcommand{\\E}{\\mathrm{E}}
\\newcommand{\\Var}{\\mathrm{Var}}
\\newcommand{\\Cov}{\\mathrm{Cov}}
\\newcommand{\\Bias}{\\mathrm{Bias}}

\\newcommand{\\real}{\\mathbb{R}}

\\renewcommand{\\i}{\\hat{\\imath}}
\\renewcommand{\\j}{\\hat{\\jmath}}
\\renewcommand{\\k}{\\hat{k}}

\\newcommand{\\uv}[1]{\\hat{#1}}
\\newcommand{\\valg}[1]{\\left\\langle #1 \\right\\rangle}
\\newcommand{\\norm}[1]{\\left|\\left|#1\\right|\\right|}

\\newenvironment{mat}[1] {
 \\left[\\begin{array}{#1}
}{
  \\end{array}\\right]
}

\\newenvironment{detmat}[1] {
  \\left|\\begin{array}{#1}
}{
  \\end{array}\\right|
}

\\DeclareMathOperator{\\proj}{proj}

\\definecolor{zzttqq}{rgb}{0.6,0.2,0.}
\\definecolor{qqqqff}{rgb}{0.,0.,1.}
\\definecolor{uuuuuu}{rgb}{0.26666666666666666,0.26666666666666666,0.26666666666666666}"
      ("\\section{%s}" . "\\section*{%s}")
      ("\\subsection{%s}" . "\\subsection*{%s}")
      ("\\subsubsection{%s}" . "\\subsubsuection*{%s}")
      ("\\paragraph{%s}" . "\\paragraph*{%}")
      ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))))
 '(org-latex-create-formula-image-program (quote imagemagick))
 '(org-latex-default-packages-alist
(quote
 (("AUTO" "inputenc" nil)
  ("T1" "fontenc" t)
  ("" "fixltx2e" nil)
  ("" "graphicx" t)
  ("" "grffile" t)
  ("" "longtable" nil)
  ("" "wrapfig" nil)
  ("" "rotating" nil)
  ("normalem" "ulem" t)
  ("" "amsmath" t)
  ("" "textcomp" t)
  ("" "amssymb" t)
  ("" "capt-of" nil)
  ("" "hyperref" nil))))
'(org-latex-packages-alist
(quote
 (("" "hyperref" nil)
  ("" "amsmath" t)
  ("" "amssymb" t)
  ("" "siunitx" t)
  ("" "fontspec" t)
  ("margin=1in" "geometry" nil)
  "\\setlength{\\parindent}{0cm}"
  ("" "titlesec" t)
  "\\titleformat*{\\section}{\\LARGE\\bfseries}" "\\titleformat*{\\subsection}{\\normalsize\\bfseries}")))
'(org-latex-pdf-process
(quote
 ("xelatex -interaction nonstopmode %f" "xelatex -interaction nonstopmode %f")))
 '(org-pretty-entities t)
 '(org-read-date-force-compatible-dates nil)
 '(org-return-follows-link t)
 '(org-startup-with-inline-images t)
 '(org-time-stamp-custom-formats (quote ("<%B %e, %Y>" . "<%B %e, %Y %H:%M>")))
'(package-selected-packages
(quote
 (pug-mode helm-themes helm-swoop helm-mode-manager helm-make helm-hoogle helm-ag ace-jump-helm-line yapfify thrift py-isort pdf-tools tablist ox-gfm org-projectile org org-download livid-mode skewer-mode simple-httpd live-py-mode intero hlint-refactor git-link goto-chg eshell-z diminish company-shell company-ghci company-emacs-eclim ace-window avy cdlatex wolfram-mode stan-mode scad-mode qml-mode matlab-mode julia-mode arduino-mode undo-tree pcre2el log4e gntp json-snatcher request flx fringe-helper web-completion-data dash-functional pos-tip inflections edn paredit peg eval-sexp-fu highlight spinner queue pkg-info epl popup alert git-commit eclim rake tern ghc s toml-mode racer rust-mode flycheck-rust company-racer deferred py-yapf swift-mode haml-mode auctex package-build go-eldoc company-go go-mode hydra js2-mode f magit-popup auto-complete gitignore-mode with-editor yasnippet async inf-ruby markdown-mode clojure-mode packed anaconda-mode flycheck haskell-mode git-gutter company projectile helm helm-core multiple-cursors json-reformat magit pythonic bind-key evil srefactor orgit magit-gitflow helm-flx git-gutter-fringe+ git-gutter+ evil-magit company-quickhelp clj-refactor yaml-mode xterm-color ws-butler which-key web-mode web-beautify use-package typo toc-org tagedit stickyfunc-enhance spacemacs-theme solarized-theme smeargle slim-mode shm shell-pop scss-mode sass-mode rvm ruby-tools ruby-test-mode rubocop rspec-mode robe restart-emacs rbenv quelpa pyvenv python pytest pyenv-mode projectile-rails popwin pip-requirements persp-mode page-break-lines org-repo-todo org-present org-pomodoro org-plus-contrib org-bullets nix-mode multi-term mmm-mode markdown-toc macrostep less-css-mode json-mode js2-refactor js-doc jade-mode ido-vertical-mode hy-mode htmlize hl-todo hindent help-fns+ helm-pydoc helm-projectile helm-nixos-options helm-gitignore helm-descbinds helm-css-scss helm-company helm-c-yasnippet haskell-snippets gnuplot gitconfig-mode gitattributes-mode git-timemachine git-messenger git-gutter-fringe gh-md flycheck-pos-tip flycheck-haskell fish-mode fill-column-indicator feature-mode eyebrowse exec-path-from-shell evil-visualstar evil-surround evil-escape eshell-prompt-extras esh-help emmet-mode emacs-eclim elisp-slime-nav disaster diff-hl cython-mode company-web company-tern company-statistics company-nixos-options company-ghc company-cabal company-c-headers company-auctex company-anaconda coffee-mode cmm-mode cmake-mode clang-format cider-eval-sexp-fu cider chruby bundler bind-map auto-yasnippet auto-compile align-cljlet ac-ispell)))
'(safe-local-variable-values
(quote
 ((TeX-command-extra-options . "-shell-escape")
  (global-flycheck-mode . t)
  (flycheck-disabled-checkers haskell-stack-ghc)
  (flycheck-disabled-checkers . haskell-stack-ghc)
  (flycheck-ghc-search-path
   ("lib"))
  (flycheck-mode t)
  (flycheck-mode . t)
  (flycheck-ghc-search-path . "lib")
  (flycheck-disabled-checkers
   (quote
    (haskell-stack-ghc)))
  (flycheck-disabled-checkers
   (haskell-stack-ghc))
  (flycheck-ghc-search-path "lib")))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-tooltip-common ((t (:inherit company-tooltip :weight bold :underline nil))))
 '(company-tooltip-common-selection ((t (:inherit company-tooltip-selection :weight bold :underline nil)))))
