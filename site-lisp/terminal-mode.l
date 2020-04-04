;;; -*- mode: lisp; package: terminal-mode -*-
;;;
;;; terminal-mode.l --- Minor-mode for Terminal-like buffer.

;;; Commentary:

;;; Code:

(eval-when (:execute :compile-toplevel :load-toplevel)
  (require "ansify"))

(defpackage :terminal-mode
  (:nicknames "term")
  (:use :lisp :editor))

(eval-when (:execute :compile-toplevel :load-toplevel)
  (ansify::install :terminal-mode))

(in-package :terminal-mode)


;;;; ユーザー設定用変数

(defvar-local -prompt-text-attribute- '(:bold t)
  "*プロンプト部分の装飾。
指定の形式は set-text-attribute 参照。")



;;;; 実装用変数

(defvar-local -processor- 'princ
  "入力を処理する関数。
引数として入力された文字列（string）が与えられる。")

(defvar-local -prompt- ">>> "
  "プロンプト文字列、もしくは文字列を返す関数。")


;;;; Prompt and Input Area

(defun prompt-string (&optional (prompt -prompt-))
  (etypecase prompt
    (string prompt)
    ((or symbol function)
      (handler-case
          (funcall prompt)
        (error (e) ">>> ")))))

(defun print-prompt (&optional (point (point)) &key (move t))
  (with-output-to-buffer ((selected-buffer) point)
    (format t "~&~A" (prompt-string))
    (let ((start (point))
          (end   (buffer-stream-point *standard-output*)))
      (apply #'set-text-attribute start end 'prompt -prompt-text-attribute-)
      (when move
        (goto-char end)))))

(defun find-last-prompt ()
  (save-excursion
    (multiple-value-bind (start end tag)
        (find-text-attribute 'prompt :from-end t)
      (values start end))))

(defun input-area ()
  (multiple-value-bind (start end)
      (find-last-prompt)
    (values end
            (point-max))))

(defun input-text ()
  (save-restriction
    (widen)
    (multiple-value-call #'buffer-substring (input-area))))


;;;; Narrow to Input Area

(defvar *narrowed-input-area* nil)  ;(BUFFER FROM TO)

(defun narrow-to-input-area-p ()
  (and terminal-mode
       ))

(defun %narrow-to-input-area (from to)
  (narrow-to-region from to)
  (setf *narrowed-input-area* (list (selected-buffer) from to)))

(defun %widen-from-input-area (buffer)
  (save-window-excursion
    (save-excursion
      (with-set-buffer
        (let ((win (get-buffer-window buffer)))
          (if win
            (set-window win)
            (set-buffer buffer))
          (widen)
          (setf *narrowed-input-area* nil))))))

;; *pre-command-hook*
(defun narrow-to-input-area ()
  (when (narrow-to-input-area-p)
    (multiple-value-bind (from to)
        (input-area)
      (when (and from to)
        (%narrow-to-input-area from to)))))
(add-hook '*pre-command-hook* 'narrow-to-input-area)

;; *post-command-hook*
(defun widen-from-input-area ()
  (when *narrowed-input-area*
    (%widen-from-input-area (first *narrowed-input-area*))))
(add-hook '*post-command-hook* 'widen-from-input-area)



;;;; Enter: Process Input

(defun original-command (key)
  (let ((bound (mapcar #'(lambda (x)
                           (when (and (keymapp x)
                                      (not (eql x *terminal-mode-keymap*)))
                             (lookup-keymap x key)))
                       (append (list (current-selection-keymap))
                               (minor-mode-map)
                               (list (local-keymap))
                               (list *global-keymap*)))))
    (or (find-if-not #'keymapp bound)
        (find-if #'identity bound))))

(defun %process-input (input)
  (let* ((output (make-string-output-stream))
         (result (let ((*standard-output* output))
                   (funcall -processor- input))))
    (when result
      (goto-char (point-max))
      (with-output-to-selected-buffer
        (format t "~&~A" (get-output-stream-string output))
        (goto-char (buffer-stream-point *standard-output*))))
    result))

(defun process-input-or-original ()
  (interactive)
  (if (%process-input (input-text))
    (print-prompt (point-max))
    (call-interactively (original-command *last-command-char*))))




;;;; Keymap

(defvar *terminal-mode-keymap* nil)

(unless *terminal-mode-keymap*
  (let ((kmap (make-sparse-keymap)))
    (define-key kmap #\RET 'process-input-or-original)
    (setf *terminal-mode-keymap* kmap)))




;;;; Minor Mode

(defvar-local terminal-mode nil)

(defvar-local -mode-label- "Term")

(defun terminal-mode (&optional (arg nil sv))
  (interactive "*p")
  (if (ed::toggle-mode 'terminal-mode arg sv)
    (terminal-mode-start)
    (terminal-mode-exit))
  (update-mode-line t))

(pushnew '(terminal-mode . -mode-label-) *minor-mode-alist* :key #'car)

(defun terminal-mode-start ()
  (set-minor-mode-map *terminal-mode-keymap*)
  (erase-buffer (selected-buffer))
  (print-prompt))

(defun terminal-mode-exit ()
  (unset-minor-mode-map *terminal-mode-keymap*))


(provide "terminal-mode")
;;; terminal-mode.l ends here.
