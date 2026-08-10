;;; test-recovery-slo.el --- ERT tests for recovery-slo.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for the 3s workspace recovery budget.  Three boundaries are mocked
;; throughout, because none of them exists in batch Emacs: the webview script
;; channel the page is asked through, the webview sweep the forced path
;; drives, and the ensure/reattach path it drives beside it.  What the module
;; owes its caller is exactly WHICH record it emits, WHEN it forces, and WHAT
;; it re-verifies — so the mocks record calls and the log sink is captured.
;;
;; Run with:
;;   emacs -batch -Q -l ert -l test-recovery-slo.el -f ert-run-tests-batch-and-exit

;;; Code:

(load (expand-file-name "test-helpers.el" (file-name-directory
                                            (or load-file-name buffer-file-name)))
      nil t)

(require 'cl-lib)

;;;; ---- Helpers -----------------------------------------------------------

(defvar agent-repl-test--slo-logs nil
  "List of (LEVEL . LINE) the captured log sink recorded, in call order.")

(defvar agent-repl-test--slo-forced nil
  "List of (KIND . WS) the mocked forced-recovery halves recorded.")

(defvar agent-repl-test--slo-probe-answer ""
  "The JSON string the mocked page probe answers every poll with.")

(defvar agent-repl-test--slo-real-read-script
  (symbol-function 'agent-repl--frontend-webview-read-script)
  "The REAL read path, captured before this suite's mock shadows it.
The crash-hazard tests are about what the genuine path does and does not
inject, which a mock standing in its place cannot witness.")

(defun agent-repl-test--slo-page-reply (script answer)
  "Build the envelope a real page returns for SCRIPT, carrying ANSWER.
Mimics the page rather than short-circuiting it: the workspace the reply
is attributed to is read back OUT of the script, which is the whole
mechanism that replaced the closure the module used to correlate with."
  (should (string-match "ws:\\(\"\\(?:[^\"\\\\]\\|\\\\.\\)*\"\\)" script))
  (json-encode (list (cons "ws" (json-parse-string (match-string 1 script)))
                     (cons "report" answer))))

(defun agent-repl-test--slo-lines (level)
  "Return the captured lines emitted at LEVEL."
  (mapcar #'cdr (cl-remove-if-not (lambda (e) (eq (car e) level))
                                  agent-repl-test--slo-logs)))

(defun agent-repl-test--slo-record (level)
  "Return the single `recovery-slo:' record at LEVEL, or nil when none."
  (car (cl-remove-if-not (lambda (l) (string-prefix-p "recovery-slo: ws=" l))
                         (agent-repl-test--slo-lines level))))

(defmacro agent-repl-test--with-slo (&rest body)
  "Run BODY with a clean SLO state, captured logs, and every boundary mocked.
The clock is real: every assertion here is about ORDER and OUTCOME, and
the one time-dependent decision — the budget — is exercised by moving the
attempt's own `:started-at' rather than by waiting."
  (declare (indent 0))
  `(let ((agent-repl--recovery-slo-attempts (make-hash-table :test 'equal))
         (agent-repl--recovery-slo-timer nil)
         (agent-repl-test--slo-logs nil)
         (agent-repl-test--slo-forced nil)
         (agent-repl-test--slo-probe-answer ""))
     (cl-letf (((symbol-function 'agent-repl--log)
                (lambda (_ws fmt &rest args)
                  (push (cons 'info (apply #'format fmt args)) agent-repl-test--slo-logs)))
               ((symbol-function 'agent-repl--log-verbose)
                (lambda (_ws fmt &rest args)
                  (push (cons 'verbose (apply #'format fmt args)) agent-repl-test--slo-logs)))
               ((symbol-function 'agent-repl--warn)
                (lambda (_ws fmt &rest args)
                  (push (cons 'warn (apply #'format fmt args)) agent-repl-test--slo-logs)))
               ((symbol-function 'agent-repl--frontend-webview-read-script)
                (lambda (_buf script callback)
                  (funcall callback
                           (agent-repl-test--slo-page-reply
                            script agent-repl-test--slo-probe-answer))))
               ((symbol-function 'agent-repl--webview-recovery-sweep)
                (lambda (_reason &optional _force)
                  (push (cons 'sweep t) agent-repl-test--slo-forced) 1))
               ((symbol-function 'agent-repl--frontend-ensure-workspace)
                (lambda (ws) (push (cons 'ensure ws) agent-repl-test--slo-forced)))
               ((symbol-function 'run-at-time)
                (lambda (&rest _args) nil)))
       (unwind-protect (progn ,@body)
         (setq agent-repl-test--slo-logs (nreverse agent-repl-test--slo-logs))))))

(defmacro agent-repl-test--with-slo-ws (ws &rest body)
  "Register WS with a live frontend buffer for BODY, cleaning up after."
  (declare (indent 1))
  `(let ((buf (generate-new-buffer ,ws)))
     (unwind-protect
         (progn
           (puthash ,ws (list :project-dir "/w") agent-repl--workspaces)
           (agent-repl--ws-put ,ws :frontend-buffer buf)
           ,@body)
       (when (buffer-live-p buf) (kill-buffer buf))
       (remhash ,ws agent-repl--workspaces))))

(defun agent-repl-test--slo-satisfy (ws &rest signals)
  "Stamp SIGNALS for WS, defaulting to the whole conjunction."
  (dolist (signal (or signals agent-repl-recovery-slo-signals))
    (agent-repl--recovery-slo-note ws signal)))

(defun agent-repl-test--slo-age (ws ms)
  "Backdate WS's open attempt by MS milliseconds."
  (let ((attempt (gethash ws agent-repl--recovery-slo-attempts)))
    (puthash ws (plist-put attempt :started-at
                           (- (plist-get attempt :started-at) (/ ms 1000.0)))
             agent-repl--recovery-slo-attempts)))

;;;; ---- The conjunction ---------------------------------------------------

(ert-deftest agent-repl-test-recovery-slo-conjunction-needs-every-signal ()
  "A conjunction missing one signal is outstanding on exactly that one."
  ;; Arrange
  (let ((attempt (list :started-at 0.0 :emacs 0.1 :wire 0.2)))
    ;; Act
    (let ((outstanding (agent-repl--recovery-slo-outstanding attempt)))
      ;; Assert
      (should (equal outstanding '(webapp))))))

(ert-deftest agent-repl-test-recovery-slo-conjunction-satisfied-by-all-three ()
  "All three stamps leave nothing outstanding."
  ;; Arrange
  (let ((attempt (list :started-at 0.0 :emacs 0.1 :webapp 0.3 :wire 0.2)))
    ;; Act + Assert
    (should (null (agent-repl--recovery-slo-outstanding attempt))))) ;

(ert-deftest agent-repl-test-recovery-slo-total-is-the-last-signal ()
  "The total gap is the LAST signal to land, not the first."
  ;; Arrange
  (let ((attempt (list :started-at 0.0 :emacs 0.1 :webapp 1.5 :wire 0.2)))
    ;; Act + Assert
    (should (= (agent-repl--recovery-slo-total-ms attempt) 1500))))

(ert-deftest agent-repl-test-recovery-slo-unstamped-delta-is-not-zero ()
  "An unstamped signal reports -1, never a zero that would read as instant."
  ;; Arrange
  (let ((attempt (list :started-at 0.0 :emacs 0.1)))
    ;; Act + Assert
    (should (= (agent-repl--recovery-slo-delta-ms attempt 'webapp) -1))))

;;;; ---- The page probe -----------------------------------------------------

(ert-deftest agent-repl-test-recovery-slo-probe-rejects-socket-open-alone ()
  "An open socket with no adoption and no content does NOT satisfy the page."
  ;; Arrange
  (let ((raw (json-serialize '(:socketOpen t :adopted :false :realDataFrames 0
                               :satisfied :false))))
    ;; Act + Assert
    (should-not (agent-repl--recovery-slo-probe-satisfied-p raw))))

(ert-deftest agent-repl-test-recovery-slo-probe-rejects-adoption-without-data ()
  "Adoption without a single content frame does NOT satisfy the page."
  ;; Arrange
  (let ((raw (json-serialize '(:socketOpen t :adopted t :realDataFrames 0
                               :satisfied :false))))
    ;; Act + Assert
    (should-not (agent-repl--recovery-slo-probe-satisfied-p raw))))

(ert-deftest agent-repl-test-recovery-slo-probe-accepts-adoption-with-data ()
  "Adoption plus real content is what satisfies the page half."
  ;; Arrange
  (let ((raw (json-serialize '(:socketOpen t :adopted t :realDataFrames 3
                               :satisfied t))))
    ;; Act + Assert
    (should (agent-repl--recovery-slo-probe-satisfied-p raw))))

(ert-deftest agent-repl-test-recovery-slo-probe-rejects-a-page-that-said-nothing ()
  "A page with no probe hook answers the empty string, which is not proof."
  ;; Arrange + Act + Assert
  (should-not (agent-repl--recovery-slo-probe-satisfied-p "")))

(ert-deftest agent-repl-test-recovery-slo-probe-hook-name-matches-webapp ()
  "The probe name lisp calls is the one the webapp plants on `window'.
A rename on either side silently turns the page signal into a permanent
budget breach, so the contract is asserted against the source."
  ;; Arrange
  (let* ((probe-ts (expand-file-name "webapp/src/recovery-probe.ts"
                                     agent-repl--frontend-root))
         (source (progn
                   (should (file-exists-p probe-ts))
                   (with-temp-buffer
                     (insert-file-contents probe-ts)
                     (buffer-string)))))
    ;; Act + Assert
    (should (string-match-p
             (regexp-quote (format "export const RECOVERY_PROBE_HOOK = \"%s\";"
                                   agent-repl-frontend-recovery-probe-hook))
             source))))

;;;; ---- The record ---------------------------------------------------------

(ert-deftest agent-repl-test-recovery-slo-record-carries-every-signal-delta ()
  "The canonical record names all three per-signal deltas and the total."
  (agent-repl-test--with-slo
    (agent-repl-test--with-slo-ws "slo-record"
      ;; Arrange
      (setq agent-repl-test--slo-probe-answer
            (json-serialize '(:adopted t :realDataFrames 1)))
      (agent-repl--recovery-slo-open "slo-record")
      (agent-repl-test--slo-satisfy "slo-record")
      ;; Act
      (agent-repl--recovery-slo-check "slo-record")
      ;; Assert
      (let ((record (agent-repl-test--slo-record 'info)))
        (should record)
        (should (string-match-p "outcome=recovered" record))
        (should (string-match-p "emacs_ms=[0-9]+" record))
        (should (string-match-p "webapp_ms=[0-9]+" record))
        (should (string-match-p "wire_ms=[0-9]+" record))
        (should (string-match-p "total_ms=[0-9]+" record))))))

(ert-deftest agent-repl-test-recovery-slo-record-is-emitted-once ()
  "One record per workspace per recovery, not one per poll."
  (agent-repl-test--with-slo
    (agent-repl-test--with-slo-ws "slo-once"
      ;; Arrange
      (agent-repl--recovery-slo-open "slo-once")
      (agent-repl-test--slo-satisfy "slo-once")
      ;; Act
      (agent-repl--recovery-slo-check "slo-once")
      (agent-repl--recovery-slo-check "slo-once")
      ;; Assert
      (should (= 1 (length (cl-remove-if-not
                            (lambda (l) (string-prefix-p "recovery-slo: ws=" l))
                            (agent-repl-test--slo-lines 'info))))))))

;;;; ---- The budget ---------------------------------------------------------

(ert-deftest agent-repl-test-recovery-slo-inside-budget-does-not-force ()
  "A workspace recovered inside its budget is never forced."
  (agent-repl-test--with-slo
    (agent-repl-test--with-slo-ws "slo-fast"
      ;; Arrange
      (agent-repl--recovery-slo-open "slo-fast")
      (agent-repl-test--slo-satisfy "slo-fast")
      ;; Act
      (should (eq (agent-repl--recovery-slo-check "slo-fast") 'recovered))
      ;; Assert
      (should (null agent-repl-test--slo-forced))
      (should (null (agent-repl-test--slo-lines 'warn))))))

(ert-deftest agent-repl-test-recovery-slo-breach-warns-naming-the-outstanding-signal ()
  "A budget breach warns loudly, naming WHICH signal is still missing."
  (agent-repl-test--with-slo
    (agent-repl-test--with-slo-ws "slo-slow"
      ;; Arrange
      (agent-repl--recovery-slo-open "slo-slow")
      (agent-repl-test--slo-satisfy "slo-slow" 'emacs 'wire)
      (agent-repl-test--slo-age "slo-slow" (1+ agent-repl-recovery-slo-budget-ms))
      ;; Act
      (should (eq (agent-repl--recovery-slo-check "slo-slow") 'breached))
      ;; Assert
      (let ((record (agent-repl-test--slo-record 'warn)))
        (should record)
        (should (string-match-p "outcome=budget-breach" record))
        (should (string-match-p "outstanding=webapp" record))))))

(ert-deftest agent-repl-test-recovery-slo-breach-drives-the-existing-recovery ()
  "The forced path drives BOTH existing halves: the sweep and the ensure."
  (agent-repl-test--with-slo
    (agent-repl-test--with-slo-ws "slo-force"
      ;; Arrange
      (agent-repl--recovery-slo-open "slo-force")
      (agent-repl-test--slo-age "slo-force" (1+ agent-repl-recovery-slo-budget-ms))
      ;; Act
      (agent-repl--recovery-slo-check "slo-force")
      ;; Assert
      (should (assq 'sweep agent-repl-test--slo-forced))
      (should (equal (cdr (assq 'ensure agent-repl-test--slo-forced)) "slo-force")))))

(ert-deftest agent-repl-test-recovery-slo-breached-workspace-is-not-forced-twice ()
  "A workspace already forced is left to its re-verification, never re-forced."
  (agent-repl-test--with-slo
    (agent-repl-test--with-slo-ws "slo-twice"
      ;; Arrange
      (agent-repl--recovery-slo-open "slo-twice")
      (agent-repl-test--slo-age "slo-twice" (1+ agent-repl-recovery-slo-budget-ms))
      (agent-repl--recovery-slo-check "slo-twice")
      (setq agent-repl-test--slo-forced nil)
      ;; Act
      (should (eq (agent-repl--recovery-slo-check "slo-twice") 'pending))
      ;; Assert
      (should (null agent-repl-test--slo-forced)))))

;;;; ---- The re-verification -------------------------------------------------

(ert-deftest agent-repl-test-recovery-slo-reverify-reports-a-repaired-workspace ()
  "A forced workspace whose conjunction is now satisfied reports as such."
  (agent-repl-test--with-slo
    (agent-repl-test--with-slo-ws "slo-repaired"
      ;; Arrange
      (agent-repl--recovery-slo-open "slo-repaired")
      (agent-repl-test--slo-age "slo-repaired" (1+ agent-repl-recovery-slo-budget-ms))
      (agent-repl--recovery-slo-check "slo-repaired")
      (agent-repl-test--slo-satisfy "slo-repaired")
      ;; Act
      (agent-repl--recovery-slo-reverify "slo-repaired")
      ;; Assert
      (let ((record (or (agent-repl-test--slo-record 'info) "")))
        (should (string-match-p "outcome=forced-recovered" record))
        (should (string-match-p "forced=yes" record))))))

(ert-deftest agent-repl-test-recovery-slo-reverify-never-claims-an-unrepaired-one ()
  "A forced workspace still missing a signal is reported unrecovered, loudly."
  (agent-repl-test--with-slo
    (agent-repl-test--with-slo-ws "slo-unrepaired"
      ;; Arrange
      (agent-repl--recovery-slo-open "slo-unrepaired")
      (agent-repl-test--slo-age "slo-unrepaired" (1+ agent-repl-recovery-slo-budget-ms))
      (agent-repl--recovery-slo-check "slo-unrepaired")
      (agent-repl-test--slo-satisfy "slo-unrepaired" 'emacs 'wire)
      ;; Act
      (agent-repl--recovery-slo-reverify "slo-unrepaired")
      ;; Assert
      (let ((records (cl-remove-if-not
                      (lambda (l) (string-match-p "outcome=forced-unrecovered" l))
                      (agent-repl-test--slo-lines 'warn))))
        (should (= 1 (length records)))
        (should (string-match-p "outstanding=webapp" (car records)))))))

(ert-deftest agent-repl-test-recovery-slo-reverify-closes-the-attempt ()
  "The re-verification closes the attempt either way — no attempt outlives it."
  (agent-repl-test--with-slo
    (agent-repl-test--with-slo-ws "slo-closed"
      ;; Arrange
      (agent-repl--recovery-slo-open "slo-closed")
      (agent-repl-test--slo-age "slo-closed" (1+ agent-repl-recovery-slo-budget-ms))
      (agent-repl--recovery-slo-check "slo-closed")
      ;; Act
      (agent-repl--recovery-slo-reverify "slo-closed")
      ;; Assert
      (should (null (gethash "slo-closed" agent-repl--recovery-slo-attempts))))))

;;;; ---- Stamping ------------------------------------------------------------

(ert-deftest agent-repl-test-recovery-slo-note-without-an-attempt-is-dropped ()
  "A stamp with no open attempt measures nothing and is not retained."
  (agent-repl-test--with-slo
    ;; Arrange + Act
    (agent-repl--recovery-slo-note-wire "slo-orphan")
    ;; Assert
    (should (null (gethash "slo-orphan" agent-repl--recovery-slo-attempts)))))

(ert-deftest agent-repl-test-recovery-slo-first-stamp-wins ()
  "A signal already stamped is not re-stamped by a later frame."
  (agent-repl-test--with-slo
    ;; Arrange
    (agent-repl--recovery-slo-open "slo-first")
    (agent-repl--recovery-slo-note-wire "slo-first")
    (let ((first (plist-get (gethash "slo-first" agent-repl--recovery-slo-attempts) :wire)))
      ;; Act
      (agent-repl--recovery-slo-note-wire "slo-first")
      ;; Assert
      (should (= first (plist-get (gethash "slo-first" agent-repl--recovery-slo-attempts)
                                  :wire))))))

(ert-deftest agent-repl-test-recovery-slo-open-discards-the-previous-attempt ()
  "A second outage opens a fresh attempt rather than inheriting old stamps."
  (agent-repl-test--with-slo
    ;; Arrange
    (agent-repl--recovery-slo-open "slo-reopen")
    (agent-repl-test--slo-satisfy "slo-reopen")
    ;; Act
    (agent-repl--recovery-slo-open "slo-reopen")
    ;; Assert
    (should (equal (agent-repl--recovery-slo-outstanding
                    (gethash "slo-reopen" agent-repl--recovery-slo-attempts))
                   agent-repl-recovery-slo-signals))))

;;;; ---- The crash hazards --------------------------------------------------

;; WHY THIS SECTION EXISTS.  The first version of this module crashed the
;; user's Emacs twice with SIGSEGV inside `print_object'.  The mechanism was
;; the NS port's `nsxwidget_webkit_execute_script', which captures the
;; callback into an Objective-C block the garbage collector cannot see: the
;; per-tick closure this module used to correlate a reply with a workspace
;; was collected while WebKit still held it, and the completion handler then
;; resurrected the dangling object into an input event that
;; `xwidget-event-handler' prints with %S.  These tests hold the module to
;; the properties that make that class of crash unreachable, and they run
;; against the REAL `agent-repl--frontend-webview-read-script' rather than
;; the mock the rest of the suite uses — a mocked injection path cannot
;; witness what is or is not injected.

(defmacro agent-repl-test--with-slo-real-injection (widget-fn injected &rest body)
  "Run BODY with the real read path, WIDGET-FN resolving widgets into INJECTED.
INJECTED is bound to a list of (WIDGET SCRIPT CALLBACK), newest first."
  (declare (indent 2))
  `(let ((,injected nil))
     (cl-letf (((symbol-function 'agent-repl--frontend-webview-read-script)
                agent-repl-test--slo-real-read-script)
               ((symbol-function 'agent-repl--frontend-webview-live-widget)
                ,widget-fn)
               ((symbol-function 'agent-repl--frontend-webview-execute-script-value)
                (lambda (&rest args) (push args ,injected))))
       ,@body)))

(ert-deftest agent-repl-test-recovery-slo-killed-buffer-mid-tick-injects-nothing ()
  "A workspace whose webview buffer died between ticks is not injected into."
  (agent-repl-test--with-slo
    (agent-repl-test--with-slo-ws "ws1"
      (agent-repl--recovery-slo-open "ws1")
      (agent-repl-test--with-slo-real-injection
          (lambda (_buf) (error "the widget must not even be resolved")) injected
        ;; Arrange: the buffer dies after the attempt opened, as a kill
        ;; racing a tick does in production.
        (kill-buffer (agent-repl--ws-get "ws1" :frontend-buffer))
        ;; Act
        (agent-repl--recovery-slo-poll-webapp "ws1")
        ;; Assert
        (should (null injected))))))

(ert-deftest agent-repl-test-recovery-slo-dead-widget-mid-tick-injects-nothing ()
  "A live buffer whose WKWebView has gone away is not injected into."
  (agent-repl-test--with-slo
    (agent-repl-test--with-slo-ws "ws1"
      (agent-repl--recovery-slo-open "ws1")
      (agent-repl-test--with-slo-real-injection (lambda (_buf) nil) injected
        ;; Act
        (agent-repl--recovery-slo-poll-webapp "ws1")
        ;; Assert
        (should (null injected))))))

(ert-deftest agent-repl-test-recovery-slo-renavigated-widget-is-re-resolved ()
  "Each tick injects into the widget resolved NOW, never one held from before."
  (agent-repl-test--with-slo
    (agent-repl-test--with-slo-ws "ws1"
      (agent-repl--recovery-slo-open "ws1")
      (let ((widgets (list 'first-widget 'second-widget)))
        (agent-repl-test--with-slo-real-injection
            (lambda (_buf) (pop widgets)) injected
          ;; Act: two ticks across a re-navigation that replaced the widget.
          (agent-repl--recovery-slo-poll-webapp "ws1")
          (agent-repl--recovery-slo-poll-webapp "ws1")
          ;; Assert
          (should (equal (mapcar #'car (reverse injected))
                         '(first-widget second-widget))))))))

(ert-deftest agent-repl-test-recovery-slo-probe-callback-is-an-interned-symbol ()
  "The callback handed across the xwidget boundary is a symbol, never a closure.
A closure is what the NS port cannot keep alive; an interned symbol is
permanently GC-rooted and survives however long the page takes to reply."
  (agent-repl-test--with-slo
    (agent-repl-test--with-slo-ws "ws1"
      (agent-repl--recovery-slo-open "ws1")
      (agent-repl-test--with-slo-real-injection (lambda (_buf) 'live-widget) injected
        ;; Act
        (agent-repl--recovery-slo-poll-webapp "ws1")
        ;; Assert
        (let ((callback (nth 2 (car injected))))
          (should (symbolp callback))
          (should (intern-soft (symbol-name callback))))))))

(ert-deftest agent-repl-test-recovery-slo-stored-state-is-scalars-only ()
  "Every value the attempts table holds is a scalar, so printing it is safe.
An xwidget-derived value reaching this table would eventually be printed
by the record or a warning, which is the crash this module has to make
unreachable."
  (agent-repl-test--with-slo
    (agent-repl-test--with-slo-ws "ws1"
      (setq agent-repl-test--slo-probe-answer
            (json-encode '((adopted . t) (realDataFrames . 2))))
      (agent-repl--recovery-slo-open "ws1")
      ;; Act: drive a whole attempt, so the table holds everything it ever does.
      (agent-repl-test--slo-satisfy "ws1" 'emacs 'wire)
      (agent-repl--recovery-slo-poll-webapp "ws1")
      (let ((attempt (gethash "ws1" agent-repl--recovery-slo-attempts)))
        ;; Assert
        (should attempt)
        (cl-loop for (key value) on attempt by #'cddr
                 do (should (keywordp key))
                 do (should (or (stringp value) (numberp value) (eq value t))))))))

(ert-deftest agent-repl-test-recovery-slo-poll-cadence-is-the-defended-constant ()
  "The tick cadence is 500ms: six samples inside the 3s budget.
Pinned because it is a rate driven into live WebKit views once per
mounted webview, and a silent return to a finer cadence would multiply
that pressure for resolution the record does not report."
  (should (equal agent-repl-recovery-slo-poll-ms 500))
  (should (>= (/ agent-repl-recovery-slo-budget-ms
                 agent-repl-recovery-slo-poll-ms)
              6)))

(provide 'test-recovery-slo)
;;; test-recovery-slo.el ends here
