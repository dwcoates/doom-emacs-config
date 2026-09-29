;;; test-notifications.el --- Tests for agent-repl notifications -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for notifications.el -- Emacs's two parts in the daemon's desktop
;; notifications: reporting whether Emacs is focused, and selecting the
;; workspace a clicked banner names.

;;; Code:

(load (expand-file-name "test-helpers.el" (file-name-directory
                                            (or load-file-name buffer-file-name)))
      nil t)

;;;; ---- Tests: reporting the focus ----

(defmacro agent-repl-test-notifications--with-reporter (focused conn &rest body)
  "Run BODY with the focus reporter's state fresh, Emacs FOCUSED, and CONN live.
Every `ReportEditorFocus' call is recorded into `calls' as (CONN . REQUEST)
with its callbacks in `answers', so a test answers each one itself."
  (declare (indent 2))
  `(let ((agent-repl--focus-report-in-flight nil)
         (agent-repl--focus-report-owed nil)
         (agent-repl--focus-report-timer nil)
         (focused-now ,focused)
         (calls nil)
         (answers nil))
     (cl-letf (((symbol-function 'agent-repl-link-live) (lambda () ,conn))
               ((symbol-function 'agent-repl--emacs-focused-p) (lambda (&optional _ws) focused-now))
               ((symbol-function 'agent-repl-rpc-report-editor-focus)
                (lambda (c request &rest keys)
                  (push (cons c request) calls)
                  (push keys answers))))
       ,@body)))

(defun agent-repl-test-notifications--answer (keys response)
  "Answer the recorded call whose callbacks are KEYS with RESPONSE."
  (funcall (plist-get keys :on-response) response))

(ert-deftest agent-repl-test-focus-report-sends-the-current-focus ()
  "A report tells the live daemon whether Emacs is focused now."
  (agent-repl-test-notifications--with-reporter t 'conn
    (agent-repl--focus-report)
    (should (equal calls '((conn . (:focus (:arm :focused))))))))

(ert-deftest agent-repl-test-focus-report-sends-unfocused ()
  "An unfocused Emacs reports the unfocused arm."
  (agent-repl-test-notifications--with-reporter nil 'conn
    (agent-repl--focus-report)
    (should (equal calls '((conn . (:focus (:arm :unfocused))))))))

(ert-deftest agent-repl-test-focus-report-with-no-link-sends-nothing ()
  "With no link standing there is nothing to tell: the next stream states it."
  (agent-repl-test-notifications--with-reporter t nil
    (agent-repl--focus-report)
    (should-not calls)
    (should-not agent-repl--focus-report-in-flight)))

(ert-deftest agent-repl-test-focus-report-in-flight-owes-the-next ()
  "A change while a report is in flight sends nothing yet and is owed."
  (agent-repl-test-notifications--with-reporter t 'conn
    (agent-repl--focus-report)
    (agent-repl--focus-report)
    (should (= (length calls) 1))
    (should agent-repl--focus-report-owed)))

(ert-deftest agent-repl-test-focus-report-sends-the-owed-current-focus-when-answered ()
  "Once the in-flight report is answered, the owed one sends the focus held NOW."
  (agent-repl-test-notifications--with-reporter t 'conn
    (agent-repl--focus-report)
    (setq focused-now nil)
    (agent-repl--focus-report)
    (agent-repl-test-notifications--answer (car answers) '(:arm :success :value nil))
    (should (equal (car calls) '(conn . (:focus (:arm :unfocused)))))
    (should (= (length calls) 2))))

(ert-deftest agent-repl-test-focus-report-answered-with-nothing-owed-ends ()
  "An answered report with nothing owed leaves no report in flight."
  (agent-repl-test-notifications--with-reporter t 'conn
    (agent-repl--focus-report)
    (agent-repl-test-notifications--answer (car answers) '(:arm :success :value nil))
    (should-not agent-repl--focus-report-in-flight)
    (should (= (length calls) 1))))

(ert-deftest agent-repl-test-focus-report-refusal-is-recorded-at-info ()
  "A no_emacs_stream refusal is the ordinary reconnect gap, recorded at INFO."
  (agent-repl-test-notifications--with-reporter t 'conn
    (let ((infos nil))
      (cl-letf (((symbol-function 'agent-repl--info)
                 (lambda (_scope fmt &rest args) (push (apply #'format fmt args) infos))))
        (agent-repl--focus-report)
        (agent-repl-test-notifications--answer
         (car answers) '(:arm :error :value (:cause (:arm :no-emacs-stream :value nil)))))
      (should (seq-some (lambda (line) (string-match-p "focus-report refused focused=t" line)) infos))
      (should-not agent-repl--focus-report-in-flight))))

(ert-deftest agent-repl-test-focus-report-transport-failure-is-an-error ()
  "A transport failure is recorded at ERROR with its detail, and the report ends."
  (agent-repl-test-notifications--with-reporter t 'conn
    (let ((errors nil))
      (cl-letf (((symbol-function 'agent-repl--error)
                 (lambda (_scope fmt &rest args) (push (apply #'format fmt args) errors))))
        (agent-repl--focus-report)
        (funcall (plist-get (car answers) :on-failure) '(:kind :connect :message "refused")))
      (should (seq-some (lambda (line) (string-match-p "focus-report failed focused=t detail=.*refused" line)) errors))
      (should-not agent-repl--focus-report-in-flight))))

(ert-deftest agent-repl-test-focus-report-unknown-arm-is-an-error ()
  "An answer arm the reporter does not know is recorded at ERROR."
  (agent-repl-test-notifications--with-reporter t 'conn
    (let ((errors nil))
      (cl-letf (((symbol-function 'agent-repl--error)
                 (lambda (_scope fmt &rest args) (push (apply #'format fmt args) errors))))
        (agent-repl--focus-report)
        (agent-repl-test-notifications--answer (car answers) '(:arm :mystery :value nil)))
      (should (seq-some (lambda (line) (string-match-p "focus-report unknown-arm" line)) errors)))))

(ert-deftest agent-repl-test-focus-changed-coalesces-into-one-timer ()
  "Two focus changes in one command loop schedule ONE report."
  (agent-repl-test-notifications--with-reporter t 'conn
    (let ((scheduled 0))
      (cl-letf (((symbol-function 'run-at-time)
                 (lambda (&rest _) (cl-incf scheduled) (timer-create))))
        (agent-repl--focus-changed)
        (agent-repl--focus-changed))
      (should (= scheduled 1)))))

(ert-deftest agent-repl-test-focus-report-on-link-reports ()
  "A daemon that becomes the live one is told the focus Emacs holds now."
  (agent-repl-test-notifications--with-reporter t 'conn
    (agent-repl--focus-report-on-link 'old 'new)
    (should (equal calls '((conn . (:focus (:arm :focused))))))))

(ert-deftest agent-repl-test-focus-reporter-rides-the-link-hooks ()
  "The reporter is on the link-up and promotion hooks, and on focus changes."
  (should (memq #'agent-repl--focus-report-on-link agent-repl-link-up-functions))
  (should (memq #'agent-repl--focus-report-on-link agent-repl-link-promote-functions))
  (should (advice-function-member-p #'agent-repl--focus-changed after-focus-change-function)))

;;;; ---- Tests: agent-repl--notification-activate ----

(ert-deftest agent-repl-test-activate-jumps-to-workspace ()
  "A banner click selects the workspace's tab, through workspace.el's boundary.
THE CLICK ACTION of the host stream's notification policy: decider and
actor are one process, so no daemon round-trip and no SelectWorkspace of
its own — the tab switch that follows issues that verb."
  (let (jumped)
    (cl-letf (((symbol-function 'agent-repl--log) (lambda (&rest _) nil))
              ((symbol-function 'agent-repl--ws-switch)
               (lambda (ws &rest _) (setq jumped ws)))
              ((symbol-function 'select-frame-set-input-focus) (lambda (&rest _) nil))
              ((symbol-function 'selected-frame) (lambda () 'frame)))
      (agent-repl--notification-activate "ws-a")
      (should (equal jumped "ws-a")))))

(ert-deftest agent-repl-test-activate-focuses-selected-frame ()
  "Activate should focus the selected frame so Emacs comes forward."
  (let (focused)
    (cl-letf (((symbol-function 'agent-repl--log) (lambda (&rest _) nil))
              ((symbol-function 'agent-repl--ws-switch) (lambda (&rest _) nil))
              ((symbol-function 'selected-frame) (lambda () 'the-frame))
              ((symbol-function 'select-frame-set-input-focus)
               (lambda (frame) (setq focused frame))))
      (agent-repl--notification-activate "ws-a")
      (should (eq focused 'the-frame)))))

(ert-deftest agent-repl-test-activate-nil-ws-focuses-without-jump ()
  "Activate with a nil WS should focus Emacs but not attempt a jump."
  (let ((jumped nil) (focused nil))
    (cl-letf (((symbol-function 'agent-repl--log) (lambda (&rest _) nil))
              ((symbol-function 'agent-repl--ws-switch)
               (lambda (&rest _) (setq jumped t)))
              ((symbol-function 'selected-frame) (lambda () 'frame))
              ((symbol-function 'select-frame-set-input-focus)
               (lambda (&rest _) (setq focused t))))
      (agent-repl--notification-activate nil)
      (should-not jumped)
      (should focused))))

(ert-deftest agent-repl-test-activate-unknown-ws-warns ()
  "An activation naming no workspace is a WARNING, not a silent no-op:
the click happened and went nowhere, which is worth the record."
  (let (warned)
    (cl-letf (((symbol-function 'agent-repl--log) (lambda (&rest _) nil))
              ((symbol-function 'agent-repl--warn)
               (lambda (_ws fmt &rest args) (setq warned (apply #'format fmt args))))
              ((symbol-function 'agent-repl--ws-switch)
               (lambda (&rest _) (error "an unknown workspace must not be switched to")))
              ((symbol-function 'selected-frame) (lambda () 'frame))
              ((symbol-function 'select-frame-set-input-focus) (lambda (&rest _) nil)))
      (agent-repl--notification-activate "")
      (should (string-search "activate-unknown-workspace" warned)))))

;;;; ---- Tests: agent-repl--emacs-focused-p ----

(ert-deftest agent-repl-test-emacs-focused-p-with-no-workspace-records-centrally ()
  "With no WS, the focus observation names the CENTRAL scope explicitly.
Desktop focus is a global fact.  Passing nil down the workspace-owned
rung with nothing else naming a workspace earns
`elisp.core.log-routing-error' at ERROR -- correctly, since missing
attribution at a call site is a defect no directory can supply -- so this
call site states the central scope instead of leaving one missing."
  ;; Arrange.
  (let ((scopes nil))
    (cl-letf (((symbol-function 'agent-repl--log-verbose)
               (lambda (ws &rest _) (push ws scopes))))
      ;; Act.
      (let ((noninteractive t))
        (agent-repl--emacs-focused-p))
      ;; Assert.
      (should (equal scopes (list agent-repl--global-log-scope))))))

(ert-deftest agent-repl-test-emacs-focused-p-with-a-workspace-records-on-it ()
  "With a WS, the focus observation is attributed to that workspace."
  ;; Arrange.
  (let ((scopes nil))
    (cl-letf (((symbol-function 'agent-repl--log-verbose)
               (lambda (ws &rest _) (push ws scopes))))
      ;; Act.
      (let ((noninteractive t))
        (agent-repl--emacs-focused-p "ws1"))
      ;; Assert.
      (should (equal scopes (list "ws1"))))))

(ert-deftest agent-repl-test-emacs-focused-p-nil-in-batch ()
  "Under `noninteractive', emacs-focused-p returns nil without scanning frames."
  (cl-letf (((symbol-function 'frame-list)
             (lambda () (error "frame-list must not run under noninteractive"))))
    (let ((noninteractive t))
      (should-not (agent-repl--emacs-focused-p)))))

(ert-deftest agent-repl-test-emacs-focused-p-true-when-frame-focused ()
  "emacs-focused-p is non-nil when a frame holds OS focus."
  (cl-letf (((symbol-function 'frame-list) (lambda () (list 'f1)))
            ((symbol-function 'frame-focus-state) (lambda (_frame) t)))
    (let ((noninteractive nil))
      (should (agent-repl--emacs-focused-p)))))

(ert-deftest agent-repl-test-emacs-focused-p-false-when-no-frame-focused ()
  "emacs-focused-p is nil when every frame reports no focus."
  (cl-letf (((symbol-function 'frame-list) (lambda () (list 'f1 'f2)))
            ((symbol-function 'frame-focus-state) (lambda (_frame) nil)))
    (let ((noninteractive nil))
      (should-not (agent-repl--emacs-focused-p)))))

(ert-deftest agent-repl-test-emacs-focused-p-scans-all-frames ()
  "emacs-focused-p is non-nil when a non-selected background frame is focused."
  (cl-letf (((symbol-function 'frame-list) (lambda () (list 'f1 'f2)))
            ((symbol-function 'frame-focus-state)
             (lambda (frame) (when (eq frame 'f2) t))))
    (let ((noninteractive nil))
      (should (agent-repl--emacs-focused-p)))))

(ert-deftest agent-repl-test-emacs-focused-p-unknown-counts-as-focused ()
  "A frame whose focus is `unknown' counts as focused (conservative suppress)."
  (cl-letf (((symbol-function 'frame-list) (lambda () (list 'f1)))
            ((symbol-function 'frame-focus-state) (lambda (_frame) 'unknown)))
    (let ((noninteractive nil))
      (should (agent-repl--emacs-focused-p)))))

(provide 'test-notifications)

;;; test-notifications.el ends here
