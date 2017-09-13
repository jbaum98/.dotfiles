;;; funcs.el --- ocaml Layer functions File for Spacemacs
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defun spacemacs/merlin-locate ()
  (interactive)
  (let ((merlin-locate-in-new-window 'never))
    (merlin-locate)))

(defun spacemacs/merlin-locate-other-window ()
  (interactive)
  (let ((merlin-locate-in-new-window 'always))
    (merlin-locate)))
