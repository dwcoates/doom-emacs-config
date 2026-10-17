;;; test-evaluate.el --- ERT tests for agent-repl evaluate.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Run with:
;;   AGENT_REPL_FORBID_VENDOR_CALLS=1 emacs -batch -Q -l ert \
;;     -l lisp/test-evaluate.el -f ert-run-tests-batch-and-exit
;;
;; evaluate.el is the editor's one evaluation handler; these tests pin each
;; outcome it answers -- a value, readable or not, a read error, an
;; evaluation error -- and the `*Messages*' lines it reports.

;;; Code:

(load (expand-file-name "test-helpers.el" (file-name-directory
                                            (or load-file-name buffer-file-name)))
      nil t)

(defun agent-repl-test-evaluate--outcome (text)
  "Evaluate TEXT and answer its outcome oneof alone."
  (plist-get (agent-repl-evaluate-text text) :outcome))

(ert-deftest agent-repl-test-evaluate-answers-the-last-forms-value ()
  "Every form is evaluated, and the value is the last form's."
  (should (equal (agent-repl-test-evaluate--outcome "(setq agent-repl-test-evaluate--x 1) (+ agent-repl-test-evaluate--x 2)")
                 '(:arm :value :value (:printed "3" :readable t)))))

(ert-deftest agent-repl-test-evaluate-empty-text-is-nil ()
  "Text holding no form, comments included, evaluates to nil."
  (should (equal (agent-repl-test-evaluate--outcome "  ;; nothing here\n")
                 '(:arm :value :value (:printed "nil" :readable t)))))

(ert-deftest agent-repl-test-evaluate-strings-print-readably ()
  "A string's print is its `prin1' form, which reads back."
  (should (equal (agent-repl-test-evaluate--outcome "(concat \"a\" \"b\")")
                 '(:arm :value :value (:printed "\"ab\"" :readable t)))))

(ert-deftest agent-repl-test-evaluate-an-unreadable-object-is-not-readable ()
  "A buffer prints as `#<...>', which does not read back."
  (let ((outcome (agent-repl-test-evaluate--outcome "(current-buffer)")))
    (should (eq (plist-get outcome :arm) :value))
    (should (string-prefix-p "#<buffer" (plist-get (plist-get outcome :value) :printed)))
    (should-not (plist-get (plist-get outcome :value) :readable))))

(ert-deftest agent-repl-test-evaluate-a-circular-value-prints-finitely ()
  "A circular list prints through `print-circle' rather than looping."
  (let ((outcome (agent-repl-test-evaluate--outcome "(let ((l (list 1))) (setcdr l l) l)")))
    (should (equal (plist-get (plist-get outcome :value) :printed) "#1=(1 . #1#)"))))

(ert-deftest agent-repl-test-evaluate-evaluates-with-lexical-binding ()
  "Forms are evaluated lexically, so a closure captures its variable."
  (should (equal (agent-repl-test-evaluate--outcome "(let ((n 4)) (funcall (lambda () n)))")
                 '(:arm :value :value (:printed "4" :readable t)))))

(ert-deftest agent-repl-test-evaluate-runs-in-the-current-buffer ()
  "The caller's buffer is current while the text is evaluated."
  (with-temp-buffer
    (rename-buffer "agent-repl-test-evaluate-buffer" t)
    (should (equal (agent-repl-test-evaluate--outcome "(buffer-name)")
                   (list :arm :value
                         :value (list :printed (prin1-to-string (buffer-name)) :readable t))))))

(ert-deftest agent-repl-test-evaluate-unbalanced-text-is-a-read-error ()
  "Text that does not read answers a read error."
  (should (equal (plist-get (agent-repl-test-evaluate--outcome "(+ 1") :arm) :read-error)))

(ert-deftest agent-repl-test-evaluate-a-read-error-evaluates-nothing ()
  "Every form reads before any is evaluated, so a bad tail has no effect."
  (defvar agent-repl-test-evaluate--touched nil)
  (setq agent-repl-test-evaluate--touched nil)
  (agent-repl-evaluate-text "(setq agent-repl-test-evaluate--touched t) (")
  (should-not agent-repl-test-evaluate--touched))

(ert-deftest agent-repl-test-evaluate-a-signal-is-an-eval-error ()
  "An error the evaluation signals answers its symbol, data and message."
  (should (equal (agent-repl-test-evaluate--outcome "(error \"boom %d\" 7)")
                 '(:arm :eval-error
                   :value (:error-symbol "error" :data-printed "(\"boom 7\")" :message "boom 7")))))

(ert-deftest agent-repl-test-evaluate-a-void-variable-is-an-eval-error ()
  "A void variable names its own error symbol and data."
  (let ((value (plist-get (agent-repl-test-evaluate--outcome "agent-repl-test-evaluate--never-bound")
                          :value)))
    (should (equal (plist-get value :error-symbol) "void-variable"))
    (should (equal (plist-get value :data-printed) "(agent-repl-test-evaluate--never-bound)"))))

(ert-deftest agent-repl-test-evaluate-an-uncaught-throw-is-an-eval-error ()
  "A `throw' nothing catches is caught by the one handler too."
  (should (equal (plist-get (plist-get (agent-repl-test-evaluate--outcome "(throw 'nowhere 1)") :value)
                            :error-symbol)
                 "no-catch")))

(ert-deftest agent-repl-test-evaluate-reports-the-messages-it-added ()
  "The `*Messages*' lines the evaluation added ride, oldest first."
  (message "before the evaluation")
  (should (equal (plist-get (agent-repl-evaluate-text "(message \"one\") (message \"two\") 1")
                            :messages)
                 '("one" "two"))))

(ert-deftest agent-repl-test-evaluate-reports-messages-on-an-error ()
  "Lines added before an error ride with the error."
  (should (equal (plist-get (agent-repl-evaluate-text "(message \"partway\") (error \"stop\")")
                            :messages)
                 '("partway"))))

(ert-deftest agent-repl-test-evaluate-reports-no-messages-when-none-were-added ()
  "An evaluation that logs nothing reports no lines."
  (message "an earlier line")
  (should (null (plist-get (agent-repl-evaluate-text "(+ 1 1)") :messages))))

(provide 'test-evaluate)

;;; test-evaluate.el ends here
