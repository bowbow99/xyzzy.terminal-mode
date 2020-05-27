;;; -*- mode: lisp; package: terminal-mode -*-
;;;
;;; terminal-mode.l --- Minor-mode for Terminal-like buffer.

;;; Author:   bowbow99 <bowbow99@gmail.com>
;;; Version:  0.0.0
;;; Category: Utilities Library
;;; Depends:  ansify
;;; License:  MIT (see COPYING.mit)

;;; ChangeLog:
;; 2020-04-09 bowbow99 <bowbow99@gmail.com>
;; 	* 作ってみた

;;; Commentary:
;; バッファ上でターミナルのように
;; 
;; - プロンプトを表示し
;; - ユーザから入力を受け付け
;; - 入力を処理し
;; - 結果を出力する
;; - の繰り返し
;; 
;; を実現するためのマイナーモードです。

;; このマイナーモード自体は特に役に立ちません。コマンドプロンプトや
;; REPLのようなものを実装する際にこのマイナーモードを利用すると、
;; ユーザの入力を処理する部分（といくらかのコマンド）だけを実装する
;; ことでそれっぽく動かせる。というものです。

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

(defvar-local -input-complete-p-function- nil
  "入力が完了しているかどうかを判断する関数。")



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
  "入力エリアの開始位置と終了位置を多値で返します。"
  (multiple-value-bind (start end)
      (find-last-prompt)
    (values end
            (point-max))))

(defun input-text ()
  "入力されている文字列を返します。"
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
  (let* ((above (cdr (member *terminal-mode-keymap*
                             (append (list (current-selection-keymap))
                                     (minor-mode-map)
                                     (list (local-keymap))
                                     (list *global-keymap*)))))
         (bound (mapcar (lambda (x)
                          (lookup-keymap x key))
                        above)))
    (or (find-if-not #'keymapp bound)
        (find-if #'identity bound))))

(defun call-original-command-interactively ()
  (call-interactively (original-command *last-command-char*)))

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

(defun process-input ()
  (interactive)
  (let* ((input (input-text))
         (result (%process-input input)))
    (when result
      (print-prompt (point-max))
      (history-update input))
    result))

(defun process-input-or-original ()
  (interactive)
  (if (or (null -input-complete-p-function-)
          (funcall -input-complete-p-function-))
    (process-input)
    (call-original-command-interactively)))



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
  "入力履歴のひとつ前を選択します。"
  (interactive)
  (history-cycle-to arg))

(defun history-next-input (&optional (arg 1))
  "入力履歴のひとつ後を選択します"
  (interactive)
  (history-cycle-to (* -1 arg)))

(defun original-or-history-previous-input (&optional (arg 1))
  "元のコマンドを実行します。元のコマンドが実行できなかった場合は入力れ力の一つ前を選択します。"
  (interactive)
  (or (call-original-command-interactively)
      (history-previosu-input arg)))

(defun original-or-history-next-input (&optional (arg 1))
  "元のコマンドを実行します。元のコマンドが実行できなかった場合は入力れ力の一つ後を選択します。"
  (interactive)
  (or (call-original-command-interactively)
      (history-next-input arg)))



;;;; Misc. Commands

(defun clear-terminal ()
  "バッファの内容を全て削除します。"
  (interactive "*")
  (erase-buffer (selected-buffer))
  (print-prompt))

(defun kill-input ()
  "入力されている内容を kill します。"
  (interactive "*")
  (multiple-value-call #'kill-region (input-area)))



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

(defvar *terminal-mode-keymap* nil)

(progn
  (unless *terminal-mode-keymap*
    (setf *terminal-mode-keymap* (make-sparse-keymap)))
  (macrolet ((bind (key command)
               `(define-key *terminal-mode-keymap* ,key ',command)))
    (bind #\RET process-input-or-original)
    (bind #\LFD process-input)
    (bind #\C-p original-or-history-previous-input)
    (bind #\C-n original-or-history-next-input)
    (bind #\C-l clear-terminal)
    (bind #\C-w kill-input)
    (bind #\C-t test)))



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




;;;; for manual testing
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
  (when (yes-or-no-p "Switch to *Term Test* buffer?")
    (set-buffer buf)))


#| TODOs:

- history search
- scroll mode
  - like GNU screen's copy mode
  - make buffer read-only
  - let cursor go anywhere
  - dedicated keymap
- process-input on Enter
  - configurable input-complete-p function
  - process only if cursor at end of input area
- sometimes temporary narrowing exposed
  - going into mini-buffer (interactive)
  - completing with ac-mode
- More misc. commands
  - repeat C-a to go to beginning of input area
  - repeat C-e to go to end of input area

|#


(provide "terminal-mode")
;;; terminal-mode.l ends here.
