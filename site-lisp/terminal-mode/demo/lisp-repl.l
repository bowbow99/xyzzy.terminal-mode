;;; -*- mode: lisp; package: terminal-mode.lisp-repl -*-
;;;
;;; terminal-mode/demo/lisp-repl.l --- Demo: Lisp REPL

;;; Commentary:

;;; Code:

(eval-when (:execute :compile-toplevel :load-toplevel)
  (require "terminal-mode"))

(defpackage :terminal-mode.lisp-repl
  (:use :lisp :editor
    :ansify.util
    :ansify.ext))

(eval-when (:execute :compile-toplevel :load-toplevel)
  (ansify::install))

(in-package :terminal-mode.lisp-repl)



(defun lisp-repl-read-eval-print (input)
  (format t "~&=> ~S" (eval (read-from-string input)))
  t)

(defun lisp-repl-prompt ()
  (format nil "~A> " (package-name *package*)))

(defun lisp-repl ()
  (interactive)
  (let* ((buffer (find-buffer "*Lisp REPL*"))
         (init   (not buffer)))
    (when init
      (setf buffer (create-new-buffer "*Lisp REPL*"))
      (setup-temp-buffer buffer))
    (switch-to-buffer buffer)
    (when init
      (lisp-mode)
      (setf -processor- 'lisp-repl-read-eval-print
            -prompt-    'lisp-repl-prompt)
      (terminal-mode))))



(provide "terminal-mode/demo/lisp-repl")
;;; terminal-mode/demo/lisp-repl.l ends here.
