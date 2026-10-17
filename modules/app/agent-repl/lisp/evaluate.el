;;; evaluate.el --- the editor's one evaluation handler -*- lexical-binding: t; -*-

;;; Commentary:

;; AN AGENT EVALUATES ELISP IN THE RUNNING EDITOR THROUGH THE DAEMON
;; (agentrepl.v1 EvaluateInEditor).  The daemon pushes the text on the
;; workspace's host stream (`HostEvaluate'), host.el hands it here, and the
;; outcome rides back on AnswerEditorEvaluation.
;;
;; ERRORS ARE CAUGHT IN THIS ONE HANDLER (editor_evaluation.proto): the text is
;; read and evaluated inside a single `condition-case', never wrapped by the
;; caller, so no request bypasses it and text that does not even read is
;; answered as a read error rather than breaking a wrapper.  Every form is
;; read before any is evaluated, so text that does not read has no effect.
;;
;; The outcome is the `EditorEvaluationOutcome' plist wire-host.el encodes:
;;
;;   (:outcome (:arm :value      :value (:printed P :readable B))
;;             (:arm :read-error :value (:message M))
;;             (:arm :eval-error :value (:error-symbol S :data-printed D :message M))
;;    :messages LINES)
;;
;; LINES are the `*Messages*' lines the evaluation added, oldest first, read
;; through a marker so a log truncated at its top meanwhile still answers only
;; the new lines.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(defun agent-repl-evaluate--read-forms (text)
  "Read every form in TEXT, in order.
Signals the reader's own error when TEXT does not read."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert text)
    (goto-char (point-min))
    (let (forms)
      (forward-comment (buffer-size))
      (while (not (eobp))
        (push (read (current-buffer)) forms)
        (forward-comment (buffer-size)))
      (nreverse forms))))

(defun agent-repl-evaluate--printed (value)
  "Answer VALUE's `EditorEvaluationValue' plist (:printed P :readable B).
Printed with `print-circle', so a circular value prints finitely; it is
readable when its print reads back as an `equal' value."
  (let* ((printed (let ((print-circle t) (print-length nil) (print-level nil))
                    (prin1-to-string value)))
         (readable (condition-case nil
                       (let ((read (read-from-string printed)))
                         (and (= (cdr read) (length printed))
                              (equal (car read) value)))
                     (error nil))))
    (list :printed printed :readable (and readable t))))

(defun agent-repl-evaluate--messages-since (marker)
  "Answer the `*Messages*' lines added after MARKER, oldest first."
  (with-current-buffer (messages-buffer)
    (let ((added (buffer-substring-no-properties marker (point-max))))
      (split-string added "\n" t))))

(defun agent-repl-evaluate-text (text)
  "Read and evaluate TEXT in the current buffer, answering its outcome plist.
Every form is read first, then each is evaluated with lexical binding;
the value is the last form's, nil for text holding none.  See the file
commentary for the plist's shape."
  (let ((marker (with-current-buffer (messages-buffer)
                  (copy-marker (point-max)))))
    (unwind-protect
        (let ((outcome
               (condition-case err
                   (let ((forms (agent-repl-evaluate--read-forms text)))
                     (condition-case err
                         (let (value)
                           (dolist (form forms)
                             (setq value (eval form t)))
                           (list :arm :value :value (agent-repl-evaluate--printed value)))
                       (t
                        (list :arm :eval-error
                              :value (list :error-symbol (prin1-to-string (car err))
                                           :data-printed (let ((print-circle t))
                                                           (prin1-to-string (cdr err)))
                                           :message (error-message-string err))))))
                 (t
                  (list :arm :read-error :value (list :message (error-message-string err)))))))
          (list :outcome outcome
                :messages (agent-repl-evaluate--messages-since marker)))
      (set-marker marker nil))))

(provide 'agent-repl-evaluate)

;;; evaluate.el ends here
