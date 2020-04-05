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




;;;; Utilities

(si:*fset '%slot-value #'si:*slot-value)
(defsetf %slot-value si:*set-slot-value)

(defmacro with-struct ((structure &rest slot-bindings) object &body body)
  `(let ((,structure ,object))
     (symbol-macrolet
         (,@(mapcar (lambda (slot)
                      `(,slot (%slot-value ,structure ,(intern (string slot) :keyword))))
                    slot-bindings))
       ,@body)))




;;;; Prompt and Input Area

(defun prompt-string (&optional (prompt -prompt-))
  (etypecase prompt
    (string prompt)
    ((or symbol function)
      (handler-case
          (funcall prompt)
        (error (e) ">>> ")))))

(defvar *before-print-prompt-hook* nil)

(defvar *after-print-prompt-hook* nil)

(defun print-prompt (&optional (point (point)) &key (move t))
  (run-hooks '*before-print-prompt-hook*)
  (with-output-to-buffer ((selected-buffer) point)
    (format t "~&~A" (prompt-string))
    (let ((start (point))
          (end   (buffer-stream-point *standard-output*)))
      (apply #'set-text-attribute start end 'prompt -prompt-text-attribute-)
      (run-hooks '*after-print-prompt-hook*)
      (when move
        (goto-char end)))))

(defun find-last-prompt ()
  (save-restriction
    (save-excursion
      (widen)
      (multiple-value-bind (start end tag)
          (find-text-attribute 'prompt :from-end t)
        (values start end)))))

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
  (when (and *narrowed-input-area*
             (not (deleted-buffer-p (car *narrowed-input-area*))))
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
  (let ((input (input-text)))
    (if (%process-input input)
      (progn
        (print-prompt (point-max))
        (history-update input))
      (call-interactively (original-command *last-command-char*)))))


;;;; History

(defstruct history
  past
  index
  modified)

(defun new-history ()
  (setf -input-history- (make-history :index -1)))

(defvar-local -input-history- nil)

(defun history-update (new-input)
  (with-struct (history past present index modified)
      -input-history-
    (setf past     (cons new-input past)
          index    -1
          modified nil)))

(defun replace-input-with (new-input)
  (multiple-value-bind (from to)
      (input-area)
    (delete-region from to)
    (goto-char from)
    (insert new-input)))

(defun history-nth-input (index)
  (with-struct (history  past modified)
      -input-history-
    (let ((modified (find index modified :key #'car)))

      (cond (modified     (cdr modified))
            ((= index -1) "")
            (t            (nth index past))))))

(defun store-modified-input (modified-input index)
  (with-struct (history modified)
      -input-history-
    (setf modified (cons (cons index modified-input)
                         (remove index modified :key #'car)))))

(defun history-cycle-to (n)
  (with-struct (history index past modified)
      -input-history-
    ;; Check index (-1 <= index <= (length past))
    (let ((new-index (+ index n))
          (end (1- (length past))))
      (cond ((< new-index -1)  (setf new-index -1))
            ((> new-index end) (setf new-index end)))
      ;; Save modified input
      (let ((stored  (history-nth-input new-index))
            (current (input-text)))
        (unless (string= stored current)
          (store-modified-input current index)))
      ;; Replace input text with history
      (replace-input-with (history-nth-input new-index))
      (setf index new-index))))




(defun history-previous-input (&optional (arg 1))
  (interactive)
  (history-cycle-to arg))

(defun history-next-input (&optional (arg 1))
  (interactive)
  (history-cycle-to (* -1 arg)))

(defun original-or-history-previous-input (&optional (arg 1))
  (interactive)
  (or (call-interactively (original-command *last-command-char*))
      (history-previosu-input arg)))

(defun original-or-history-next-input (&optional (arg 1))
  (interactive)
  (or (call-interactively (original-command *last-command-char*))
      (history-next-input arg)))



;;;; Debugging

(defmacro debug (&rest forms)
  `(let ((#1=#:form/result
           (list ,@(mapcar (lambda (form)
                             `(let ((#2=#:result nil)
                                    (#3=#:datum  nil))
                                (handler-case
                                    (setf #2# :returned
                                          #3# (multiple-value-list ,form))
                                  (error (err)
                                    (setf #2# :failed
                                          #3# err)))
                                (list ',form #2# #3#)))
                           (cdr forms)))))
     (dolist (x #1#)
       (msgbox (case (second x)
                 (:returned "~S~2%>>> ~S~2%~{=> ~S~^~%~}")
                 (:failed   "~S~2%>>> ~S~2%!! ~A"))
               ',(first forms) (first x) (third x)))))


(defun test ()
  (interactive)
  (debug "Test" -input-history-))







;;;; Keymap

(defparameter *terminal-mode-keymap* nil)

(unless *terminal-mode-keymap*
  (let ((kmap (make-sparse-keymap)))
    (define-key kmap #\RET 'process-input-or-original)
    (define-key kmap #\C-p 'original-or-history-previous-input)
    (define-key kmap #\C-n 'original-or-history-next-input)
    (define-key kmap #\C-t 'test)
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
  (new-history)
  (print-prompt))

(defun terminal-mode-exit ()
  (unset-minor-mode-map *terminal-mode-keymap*))




(let ((buf (find-buffer "*Term Test*")))
  (unless buf
    (setf buf (create-new-buffer "*Term Test*"))
    (setup-temp-buffer buf))
  (save-excursion
    (with-set-buffer
      (erase-buffer buf)
      (set-buffer buf)
      (lisp-mode)
      (terminal-mode t)))
  #+nil
  (when(yes-or-no-p "Switch to *Term Test* buffer?")
    (set-buffer buf)))







(provide "terminal-mode")
;;; terminal-mode.l ends here.
