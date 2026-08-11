;;; test-recovery-slo.el --- ERT tests for recovery-slo.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for the 3s workspace recovery budget.  Three boundaries are mocked
;; throughout, because none of them exists in batch Emacs: the webview script
;; channel the page is asked through, the per-workspace page repair the
;; forced path drives, and the ensure/reattach path it drives beside it.  What the module
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

(defvar agent-repl-test--slo-probe-present t
  "Whether the mocked page carries the probe hook at all.
Independent of the answer, exactly as the page's own `typeof' check is:
a page running a bundle older than webapp/src/recovery-probe.ts answers
with `present' false, which is a different fact from an empty report.")

(defvar agent-repl-test--slo-probe-ready-state "complete"
  "The `document.readyState' the mocked page answers with.
A page the host just re-navigated reports `interactive' with no globals
planted yet, which is the state that must NOT be recorded as a stale
bundle.")

(defun agent-repl-test--slo-page-reply (script answer)
  "Build the envelope a real page returns for SCRIPT, carrying ANSWER.
Mimics the page rather than short-circuiting it: the workspace the reply
is attributed to is read back OUT of the script, which is the whole
mechanism that replaced the closure the module used to correlate with."
  (should (string-match "ws:\\(\"\\(?:[^\"\\\\]\\|\\\\.\\)*\"\\)" script))
  (json-encode (list (cons "ws" (json-parse-string (match-string 1 script)))
                     (cons "present" (if agent-repl-test--slo-probe-present t :false))
                     (cons "rs" agent-repl-test--slo-probe-ready-state)
                     (cons "report" (if agent-repl-test--slo-probe-present answer "")))))

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
         (agent-repl-test--slo-probe-answer "")
         (agent-repl-test--slo-probe-present t)
         (agent-repl-test--slo-probe-ready-state "complete")
         (agent-repl--recovery-slo-excluded (make-hash-table :test 'equal))
         ;; The link is UP unless a test says otherwise: the fast path is
         ;; the ordinary case, and an attempt armed over a live link is
         ;; answerable from the instant it opens.
         (agent-repl-recovery-slo-link-up-function (lambda () t)))
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
               ((symbol-function 'agent-repl--webview-recovery-repair-workspace)
                (lambda (ws _reason)
                  (push (cons 'repair ws) agent-repl-test--slo-forced) 'driven))
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
  "Backdate WS's open attempt by MS milliseconds.
BOTH anchors move: the outage instant and the instant recovery became
answerable, so an aged attempt reads as one whose link came back at the
start and whose RECOVERY has been running MS — which is the elapsed the
budget rules on."
  (let ((attempt (gethash ws agent-repl--recovery-slo-attempts)))
    (dolist (key '(:started-at :answerable-at))
      (when (plist-get attempt key)
        (setq attempt (plist-put attempt key
                                 (- (plist-get attempt key) (/ ms 1000.0))))))
    (puthash ws attempt agent-repl--recovery-slo-attempts)))

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
  (let ((attempt (list :started-at 0.0 :answerable-at 0.0 :emacs 0.1 :webapp 1.5 :wire 0.2)))
    ;; Act + Assert
    (should (= (agent-repl--recovery-slo-total-ms attempt) 1500))))

(ert-deftest agent-repl-test-recovery-slo-unstamped-delta-is-not-zero ()
  "An unstamped signal reports -1, never a zero that would read as instant."
  ;; Arrange
  (let ((attempt (list :started-at 0.0 :answerable-at 0.0 :emacs 0.1)))
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
  "The forced path drives BOTH existing halves: the page repair and the ensure."
  (agent-repl-test--with-slo
    (agent-repl-test--with-slo-ws "slo-force"
      ;; Arrange
      (agent-repl--recovery-slo-open "slo-force")
      (agent-repl-test--slo-age "slo-force" (1+ agent-repl-recovery-slo-budget-ms))
      ;; Act
      (agent-repl--recovery-slo-check "slo-force")
      ;; Assert
      (should (equal (cdr (assq 'repair agent-repl-test--slo-forced)) "slo-force"))
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

;;;; ---- What counts as WIRE EVIDENCE ---------------------------------------
;;
;; THE PIN.  These tests exist so the wire signal's evidence source cannot
;; drift back to "whatever arm arrived first".  It did once: the stamp fired
;; for every frame carrying a `workspace', which made a busy workspace's
;; `typingDelta' the de-facto evidence, and the day a daemon-side fold silenced
;; typing inside async windows every affected workspace started reporting
;; `wire_ms=-1'.  A carrier list that grows an incidental arm re-opens exactly
;; that, silently, so the list is asserted here by content and not merely
;; exercised through whatever happens to be in it.

(ert-deftest agent-repl-test-recovery-slo-wire-carriers-are-the-guaranteed-arms ()
  "The wire signal's evidence is the per-workspace arms the daemon owes."
  ;; Arrange / Act / Assert
  (should (equal agent-repl-recovery-slo-wire-carriers
                 '("workspaceState" "sessionView"))))

(ert-deftest agent-repl-test-recovery-slo-typing-delta-is-not-wire-evidence ()
  "A `typingDelta' is incidental traffic and cannot stand in for the carrier."
  (agent-repl-test--with-slo
    ;; Arrange
    (agent-repl--recovery-slo-open "slo-typing")
    ;; Act
    (agent-repl--recovery-slo-note-wire-frame "typingDelta" "slo-typing")
    ;; Assert
    (should (null (plist-get (gethash "slo-typing" agent-repl--recovery-slo-attempts)
                             :wire)))))

(ert-deftest agent-repl-test-recovery-slo-workspace-state-is-wire-evidence ()
  "A `workspaceState' stamps the wire signal for the workspace it names."
  (agent-repl-test--with-slo
    ;; Arrange
    (agent-repl--recovery-slo-open "slo-carrier")
    ;; Act
    (agent-repl--recovery-slo-note-wire-frame "workspaceState" "slo-carrier")
    ;; Assert
    (should (plist-get (gethash "slo-carrier" agent-repl--recovery-slo-attempts)
                       :wire))))

(ert-deftest agent-repl-test-recovery-slo-session-view-is-wire-evidence ()
  "A `sessionView' stamps the wire signal for the workspace it names."
  (agent-repl-test--with-slo
    ;; Arrange
    (agent-repl--recovery-slo-open "slo-sview")
    ;; Act
    (agent-repl--recovery-slo-note-wire-frame "sessionView" "slo-sview")
    ;; Assert
    (should (plist-get (gethash "slo-sview" agent-repl--recovery-slo-attempts)
                       :wire))))

(ert-deftest agent-repl-test-recovery-slo-carrier-with-no-workspace-stamps-nothing ()
  "A carrier arm naming no workspace has nothing to attribute and stamps nothing."
  (agent-repl-test--with-slo
    ;; Arrange
    (agent-repl--recovery-slo-open "slo-unnamed")
    ;; Act
    (agent-repl--recovery-slo-note-wire-frame "workspaceState" nil)
    ;; Assert
    (should (null (plist-get (gethash "slo-unnamed" agent-repl--recovery-slo-attempts)
                             :wire)))))

;;;; ---- Scope: only workspaces that can recover are measured ---------------

(ert-deftest agent-repl-test-recovery-slo-unrecoverable-workspace-is-not-armed ()
  "A workspace with no page and no live session opens NO attempt."
  (agent-repl-test--with-slo
    (agent-repl-test--with-slo-ws "slo-scope-out"
      ;; Arrange: refused a page, and the daemon holds no session for it.
      (cl-letf (((symbol-function 'agent-repl--frontend-precreate-refusal)
                 (lambda (_ws) nil))
                ((symbol-function 'agent-repl--frontend-session-controller-live-p)
                 (lambda (_ws) nil)))
        ;; Act
        (agent-repl--recovery-slo-on-link-down)
        ;; Assert
        (should-not (gethash "slo-scope-out" agent-repl--recovery-slo-attempts))))))

(ert-deftest agent-repl-test-recovery-slo-unrecoverable-workspace-is-recorded ()
  "Exclusion is STATED — a not-measured record, never a silent skip."
  (agent-repl-test--with-slo
    (agent-repl-test--with-slo-ws "slo-scope-said"
      ;; Arrange
      (cl-letf (((symbol-function 'agent-repl--frontend-precreate-refusal)
                 (lambda (_ws) :merge-completed)))
        ;; Act
        (agent-repl--recovery-slo-on-link-down)
        ;; Assert
        (should (member "recovery-slo: ws=slo-scope-said outcome=not-measured \
reason=merge-completed"
                        (mapcar #'cdr agent-repl-test--slo-logs)))))))

(ert-deftest agent-repl-test-recovery-slo-exclusion-is-recorded-once-per-outage ()
  "Three armings of one bounce state the same non-measurement ONCE."
  (agent-repl-test--with-slo
    (agent-repl-test--with-slo-ws "slo-scope-once"
      ;; Arrange
      (cl-letf (((symbol-function 'agent-repl--frontend-precreate-refusal)
                 (lambda (_ws) :not-gui)))
        ;; Act
        (agent-repl--recovery-slo-on-link-down)
        (agent-repl--recovery-slo-on-link-up)
        ;; Assert
        (should (= 1 (cl-count-if
                      (lambda (e) (string-match-p "outcome=not-measured" (cdr e)))
                      agent-repl-test--slo-logs)))))))

(ert-deftest agent-repl-test-recovery-slo-mounted-webview-is-armed ()
  "A workspace whose page is already mounted IS recoverable, so it is armed."
  (agent-repl-test--with-slo
    (agent-repl-test--with-slo-ws "slo-scope-in"
      ;; Arrange: `:already-mounted' is the refusal that means YES here.
      (cl-letf (((symbol-function 'agent-repl--frontend-precreate-refusal)
                 (lambda (_ws) :already-mounted))
                ((symbol-function 'agent-repl--frontend-session-controller-live-p)
                 (lambda (_ws) nil)))
        ;; Act
        (agent-repl--recovery-slo-on-link-down)
        ;; Assert
        (should (gethash "slo-scope-in" agent-repl--recovery-slo-attempts))))))

(ert-deftest agent-repl-test-recovery-slo-live-session-without-a-page-is-armed ()
  "No page but a live session controller still has a wire to recover."
  (agent-repl-test--with-slo
    (agent-repl-test--with-slo-ws "slo-scope-session"
      ;; Arrange
      (cl-letf (((symbol-function 'agent-repl--frontend-precreate-refusal)
                 (lambda (_ws) nil))
                ((symbol-function 'agent-repl--frontend-session-controller-live-p)
                 (lambda (_ws) t)))
        ;; Act
        (agent-repl--recovery-slo-on-link-down)
        ;; Assert
        (should (gethash "slo-scope-session" agent-repl--recovery-slo-attempts))))))

(ert-deftest agent-repl-test-recovery-slo-exclusion-reason-comes-from-the-eligibility-source ()
  "THE DRIFT GUARD: every refusal the eligibility source can give is honored.
The scope answer is not a second opinion about who is owed a page — it
is the SAME one, so a refusal keyword added there must exclude here
without this module being touched."
  (agent-repl-test--with-slo
    (agent-repl-test--with-slo-ws "slo-drift"
      (dolist (refusal '(:not-live :not-gui :merge-completed :open-fenced
                         :no-xwidget :some-future-refusal))
        ;; Arrange
        (cl-letf (((symbol-function 'agent-repl--frontend-precreate-refusal)
                   (lambda (_ws) refusal))
                  ((symbol-function 'agent-repl--frontend-session-controller-live-p)
                   (lambda (_ws) t)))
          ;; Act + Assert
          (should (eq refusal (agent-repl--recovery-slo-exclusion "slo-drift"))))))))

;;;; ---- Answerability: the budget measures recovery, not the outage --------

(ert-deftest agent-repl-test-recovery-slo-budget-does-not-run-while-the-link-is-down ()
  "A daemon still down past the budget is PENDING, never a breach.
The live records showed every workspace on the host breaching three
seconds into a thirteen-second restart, with nothing to stamp and
nothing to force against."
  (agent-repl-test--with-slo
    (agent-repl-test--with-slo-ws "slo-down-long"
      ;; Arrange
      (let ((agent-repl-recovery-slo-link-up-function (lambda () nil)))
        (agent-repl--recovery-slo-open "slo-down-long")
        (agent-repl-test--slo-age "slo-down-long"
                                  (* 10 agent-repl-recovery-slo-budget-ms))
        ;; Act
        (let ((outcome (agent-repl--recovery-slo-check "slo-down-long")))
          ;; Assert
          (should (eq outcome 'pending))
          (should-not agent-repl-test--slo-forced))))))

(ert-deftest agent-repl-test-recovery-slo-clock-rebases-on-the-link-open-edge ()
  "Deltas are counted from the instant recovery became answerable."
  (agent-repl-test--with-slo
    (agent-repl-test--with-slo-ws "slo-rebase"
      ;; Arrange: armed 8s ago, over a link that was down the whole time.
      (let ((agent-repl-recovery-slo-link-up-function (lambda () nil)))
        (agent-repl--recovery-slo-open "slo-rebase" (- (float-time) 8.0)))
      ;; Act: the link opens now, then the conjunction lands.
      (agent-repl--recovery-slo-on-link-open)
      (agent-repl-test--slo-satisfy "slo-rebase")
      ;; Assert
      (let ((attempt (gethash "slo-rebase" agent-repl--recovery-slo-attempts)))
        (should (< (agent-repl--recovery-slo-delta-ms attempt 'emacs) 1000))
        (should (>= (agent-repl--recovery-slo-outage-ms attempt) 8000))))))

(ert-deftest agent-repl-test-recovery-slo-first-link-open-wins ()
  "A flapping reconnect ladder does not keep pushing answerability forward."
  (agent-repl-test--with-slo
    (agent-repl-test--with-slo-ws "slo-flap"
      ;; Arrange
      (let ((agent-repl-recovery-slo-link-up-function (lambda () nil)))
        (agent-repl--recovery-slo-open "slo-flap"))
      (agent-repl--recovery-slo-on-link-open)
      (let ((first (plist-get (gethash "slo-flap" agent-repl--recovery-slo-attempts)
                              :answerable-at)))
        ;; Act
        (agent-repl--recovery-slo-on-link-open)
        ;; Assert
        (should (equal first
                       (plist-get (gethash "slo-flap"
                                           agent-repl--recovery-slo-attempts)
                                  :answerable-at)))))))

(ert-deftest agent-repl-test-recovery-slo-attempt-armed-over-a-live-link-is-answerable-at-once ()
  "THE FAST PATH IS UNCHANGED: armed while connected, the anchors coincide."
  (agent-repl-test--with-slo
    (agent-repl-test--with-slo-ws "slo-fast"
      ;; Act
      (agent-repl--recovery-slo-open "slo-fast")
      ;; Assert
      (let ((attempt (gethash "slo-fast" agent-repl--recovery-slo-attempts)))
        (should (equal (plist-get attempt :started-at)
                       (plist-get attempt :answerable-at)))
        (should (= 0 (agent-repl--recovery-slo-outage-ms attempt)))))))

;;;; ---- Arming: one budget per workspace, at the earliest evidence ---------

(defun agent-repl-test--slo-started-at (ws)
  "Return WS's open attempt's start instant, or nil when none is open."
  (plist-get (gethash ws agent-repl--recovery-slo-attempts) :started-at))

(ert-deftest agent-repl-test-recovery-slo-announcement-arms-every-live-workspace ()
  "An expected-restart window arms EVERY live workspace, dated at the window."
  (agent-repl-test--with-slo
    (agent-repl-test--with-slo-ws "slo-ann-a"
      (agent-repl-test--with-slo-ws "slo-ann-b"
        ;; Arrange
        (let ((armed-at (- (float-time) 1.25)))
          ;; Act
          (agent-repl--recovery-slo-on-restart-announcement armed-at)
          ;; Assert
          (should (equal (agent-repl-test--slo-started-at "slo-ann-a") armed-at))
          (should (equal (agent-repl-test--slo-started-at "slo-ann-b") armed-at)))))))

(ert-deftest agent-repl-test-recovery-slo-announcement-clock-is-the-window-not-the-decode ()
  "An announced restart is dated from the window opening, not from arming."
  (agent-repl-test--with-slo
    (agent-repl-test--with-slo-ws "slo-ann-clock"
      ;; Arrange: the announcement describes an outage that began 2s ago.
      (let ((armed-at (- (float-time) 2.0)))
        ;; Act
        (agent-repl--recovery-slo-on-restart-announcement armed-at)
        ;; Assert
        (should (equal (agent-repl-test--slo-started-at "slo-ann-clock") armed-at))))))

(ert-deftest agent-repl-test-recovery-slo-unannounced-down-edge-still-arms ()
  "An unexpected drop, with no announcement anywhere, is still measured."
  (agent-repl-test--with-slo
    (agent-repl-test--with-slo-ws "slo-drop"
      ;; Act
      (agent-repl--recovery-slo-on-link-down)
      ;; Assert
      (should (agent-repl-test--slo-started-at "slo-drop")))))

(ert-deftest agent-repl-test-recovery-slo-announcement-then-down-edge-arms-once ()
  "Announcement then down edge is ONE attempt, kept at the earlier start."
  (agent-repl-test--with-slo
    (agent-repl-test--with-slo-ws "slo-both"
      ;; Arrange
      (let ((armed-at (- (float-time) 1.0)))
        (agent-repl--recovery-slo-on-restart-announcement armed-at)
        ;; Act: the drop the announcement predicted now actually happens.
        (agent-repl--recovery-slo-on-link-down)
        ;; Assert
        (should (equal (agent-repl-test--slo-started-at "slo-both") armed-at))))))

(ert-deftest agent-repl-test-recovery-slo-down-edge-then-announcement-moves-start-back ()
  "An announcement decoded after the drop dates the outage from the announcement."
  (agent-repl-test--with-slo
    (agent-repl-test--with-slo-ws "slo-late-ann"
      ;; Arrange
      (agent-repl--recovery-slo-on-link-down)
      (let ((armed-at (- (agent-repl-test--slo-started-at "slo-late-ann") 0.5)))
        ;; Act
        (agent-repl--recovery-slo-on-restart-announcement armed-at)
        ;; Assert
        (should (equal (agent-repl-test--slo-started-at "slo-late-ann") armed-at))))))

(ert-deftest agent-repl-test-recovery-slo-second-arming-keeps-collected-stamps ()
  "A second piece of evidence never discards the stamps already collected."
  (agent-repl-test--with-slo
    (agent-repl-test--with-slo-ws "slo-keep"
      ;; Arrange
      (agent-repl--recovery-slo-on-link-down)
      (agent-repl-test--slo-satisfy "slo-keep" 'emacs)
      ;; Act
      (agent-repl--recovery-slo-on-link-up)
      ;; Assert
      (should (equal (agent-repl--recovery-slo-outstanding
                      (gethash "slo-keep" agent-repl--recovery-slo-attempts))
                     '(webapp wire))))))

(ert-deftest agent-repl-test-recovery-slo-link-up-arms-a-workspace-with-no-attempt ()
  "The link-up backstop still arms a workspace no earlier evidence covered."
  (agent-repl-test--with-slo
    (agent-repl-test--with-slo-ws "slo-backstop"
      ;; Act
      (agent-repl--recovery-slo-on-link-up)
      ;; Assert
      (should (agent-repl-test--slo-started-at "slo-backstop")))))

;;;; ---- The emit path, end to end -----------------------------------------

(ert-deftest agent-repl-test-recovery-slo-announced-bounce-emits-one-record-per-workspace ()
  "An announced bounce that then satisfies the conjunction EMITS the record.
The defect this pins: arming that missed the announced restart produced no
record at all for the one bounce the verification loop measures."
  (agent-repl-test--with-slo
    (agent-repl-test--with-slo-ws "slo-e2e-a"
      (agent-repl-test--with-slo-ws "slo-e2e-b"
        ;; Arrange: the window opens, then each half of the conjunction lands.
        (setq agent-repl-test--slo-probe-answer
              (json-serialize '(:adopted t :realDataFrames 3)))
        (agent-repl--recovery-slo-on-restart-announcement (float-time))
        (agent-repl--recovery-slo-note-emacs "slo-e2e-a")
        (agent-repl--recovery-slo-note-emacs "slo-e2e-b")
        (agent-repl--recovery-slo-note-wire "slo-e2e-a")
        (agent-repl--recovery-slo-note-wire "slo-e2e-b")
        ;; Act
        (agent-repl--recovery-slo-tick)
        ;; Assert
        (let ((records (cl-remove-if-not
                        (lambda (l) (string-prefix-p "recovery-slo: ws=" l))
                        (agent-repl-test--slo-lines 'info))))
          (should (= 1 (length (cl-remove-if-not
                                (lambda (l) (string-match-p "ws=slo-e2e-a " l))
                                records))))
          (should (= 1 (length (cl-remove-if-not
                                (lambda (l) (string-match-p "ws=slo-e2e-b " l))
                                records))))
          (should-not (agent-repl-test--slo-started-at "slo-e2e-a"))
          (should-not (agent-repl-test--slo-started-at "slo-e2e-b")))))))

(ert-deftest agent-repl-test-recovery-slo-emacs-stamp-lands-when-armed-by-announcement ()
  "The emacs stamp of the RECONNECT SNAPSHOT lands, because arming preceded it.
Ordering defect this pins: the emacs signal is stamped WHILE the snapshot is
applied, and the link-up hook runs after that apply — so a workspace armed
only there could never be stamped by the reconnect that opened it."
  (agent-repl-test--with-slo
    (agent-repl-test--with-slo-ws "slo-order"
      ;; Arrange: window opens BEFORE the snapshot lands.
      (agent-repl--recovery-slo-on-restart-announcement (float-time))
      ;; Act: the snapshot applies (stamping emacs), then the link-up hook runs.
      (agent-repl--recovery-slo-note-emacs "slo-order")
      (agent-repl--recovery-slo-on-link-up)
      ;; Assert
      (should (plist-get (gethash "slo-order" agent-repl--recovery-slo-attempts)
                         :emacs)))))

(ert-deftest agent-repl-test-recovery-slo-lead-batch-stamps-before-the-rest-arrives ()
  "A LEAD-batch workspace is stamped while later connect batches are in flight.
This is what makes a workspace\\='s recovery independent of roster size: the
stamp is per workspace applied, never per whole snapshot decoded."
  (agent-repl-test--with-slo
    (agent-repl-test--with-slo-ws "slo-lead"
      (agent-repl-test--with-slo-ws "slo-tail"
        ;; Arrange: the recovery window is open, nothing has been applied.
        (agent-repl--recovery-slo-on-restart-announcement (float-time))
        ;; Act: only the lead batch's workspace has been applied so far.
        (agent-repl--recovery-slo-note-emacs "slo-lead")
        ;; Assert: it is stamped, and the workspace still in flight is not.
        (should (plist-get (gethash "slo-lead" agent-repl--recovery-slo-attempts)
                           :emacs))
        (should-not (plist-get (gethash "slo-tail" agent-repl--recovery-slo-attempts)
                               :emacs))))))

(ert-deftest agent-repl-test-recovery-slo-record-field-set-is-pinned ()
  "The record's field set and order are a contract; this pins them exactly."
  (agent-repl-test--with-slo
    (agent-repl-test--with-slo-ws "slo-fields"
      ;; Arrange
      (setq agent-repl-test--slo-probe-answer
            (json-serialize '(:adopted t :realDataFrames 1)))
      (agent-repl--recovery-slo-open "slo-fields")
      (agent-repl-test--slo-satisfy "slo-fields")
      ;; Act
      (agent-repl--recovery-slo-check "slo-fields")
      ;; Assert
      (let ((record (agent-repl-test--slo-record 'info)))
        (should record)
        (should (string-match-p
                 (concat "\\`recovery-slo: ws=slo-fields outcome=recovered "
                         "emacs_ms=-?[0-9]+ webapp_ms=-?[0-9]+ wire_ms=-?[0-9]+ "
                         "total_ms=-?[0-9]+ outage_ms=-?[0-9]+ budget_ms=3000 forced=no "
                         "probe=present outstanding=none\\'")
                 record))))))

(ert-deftest agent-repl-test-recovery-slo-breach-record-field-set-is-pinned ()
  "A breach past the 3000ms budget warns with outstanding= and forces."
  (agent-repl-test--with-slo
    (agent-repl-test--with-slo-ws "slo-breach-fields"
      ;; Arrange
      (agent-repl--recovery-slo-open "slo-breach-fields")
      (agent-repl-test--slo-satisfy "slo-breach-fields" 'emacs)
      (agent-repl-test--slo-age "slo-breach-fields"
                                (1+ agent-repl-recovery-slo-budget-ms))
      ;; Act
      (agent-repl--recovery-slo-check "slo-breach-fields")
      ;; Assert
      (let ((record (agent-repl-test--slo-record 'warn)))
        (should record)
        (should (string-match-p
                 (concat "\\`recovery-slo: ws=slo-breach-fields outcome=budget-breach "
                         "emacs_ms=-?[0-9]+ webapp_ms=-1 wire_ms=-1 "
                         "total_ms=-1 outage_ms=-?[0-9]+ budget_ms=3000 forced=no "
                         "probe=present outstanding=webapp,wire\\'")
                 record))
        (should (equal (cdr (assq 'repair agent-repl-test--slo-forced))
                       "slo-breach-fields"))
        (should (equal (cdr (assq 'ensure agent-repl-test--slo-forced))
                       "slo-breach-fields"))))))

;;;; ---- Telling an absent probe from a slow one ----------------------------

(ert-deftest agent-repl-test-recovery-slo-page-without-the-probe-records-absent ()
  "A page carrying no probe hook is recorded as probe=absent, not silently -1."
  (agent-repl-test--with-slo
    (agent-repl-test--with-slo-ws "slo-no-probe"
      ;; Arrange
      (setq agent-repl-test--slo-probe-present nil)
      (agent-repl--recovery-slo-open "slo-no-probe")
      (agent-repl-test--slo-satisfy "slo-no-probe" 'emacs 'wire)
      (agent-repl-test--slo-age "slo-no-probe" (1+ agent-repl-recovery-slo-budget-ms))
      ;; Act
      (agent-repl--recovery-slo-check "slo-no-probe")
      ;; Assert
      (should (string-match-p "probe=absent" (agent-repl-test--slo-record 'warn))))))

(ert-deftest agent-repl-test-recovery-slo-force-repairs-only-the-breaching-page ()
  "The force never issues a host-wide sweep, which would re-break every peer.
A whole-host sweep per breach re-navigates other workspaces' pages, and a
re-navigated page loses both its probe hook and its recovery epoch."
  (agent-repl-test--with-slo
    (agent-repl-test--with-slo-ws "slo-scoped"
      (cl-letf (((symbol-function 'agent-repl--webview-recovery-sweep)
                 (lambda (&rest _) (error "the force must not sweep the host"))))
        ;; Arrange
        (agent-repl--recovery-slo-open "slo-scoped")
        (agent-repl-test--slo-age "slo-scoped" (1+ agent-repl-recovery-slo-budget-ms))
        ;; Act
        (agent-repl--recovery-slo-check "slo-scoped")
        ;; Assert
        (should (equal (cdr (assq 'repair agent-repl-test--slo-forced)) "slo-scoped"))))))

(ert-deftest agent-repl-test-recovery-slo-probe-states-are-strings ()
  "Every probe state is a string, so nothing non-scalar can reach the table."
  (dolist (state agent-repl-recovery-slo-probe-states)
    (should (stringp state))))

(ert-deftest agent-repl-test-recovery-slo-loading-ranks-below-absent ()
  "`loading' is weaker evidence than `absent', so a verdict can still land."
  (should (< (cl-position "loading" agent-repl-recovery-slo-probe-states :test #'equal)
             (cl-position "absent" agent-repl-recovery-slo-probe-states :test #'equal))))

(ert-deftest agent-repl-test-recovery-slo-loading-page-records-loading-not-absent ()
  "A page still building its document is probe=loading, never probe=absent."
  (agent-repl-test--with-slo
    (agent-repl-test--with-slo-ws "slo-loading"
      ;; Arrange: the re-navigated page has no globals yet and says so.
      (setq agent-repl-test--slo-probe-present nil)
      (setq agent-repl-test--slo-probe-ready-state "interactive")
      (agent-repl--recovery-slo-open "slo-loading")
      (agent-repl-test--slo-satisfy "slo-loading" 'emacs 'wire)
      (agent-repl-test--slo-age "slo-loading" (1+ agent-repl-recovery-slo-budget-ms))
      ;; Act
      (agent-repl--recovery-slo-check "slo-loading")
      ;; Assert
      (should (string-match-p "probe=loading" (agent-repl-test--slo-record 'warn))))))

(ert-deftest agent-repl-test-recovery-slo-loading-verdict-is-not-latched ()
  "A page recorded loading is still polled, and a finished one overwrites it."
  (agent-repl-test--with-slo
    (agent-repl-test--with-slo-ws "slo-loading-then-done"
      ;; Arrange
      (setq agent-repl-test--slo-probe-present nil)
      (setq agent-repl-test--slo-probe-ready-state "loading")
      (agent-repl--recovery-slo-open "slo-loading-then-done")
      (agent-repl--recovery-slo-poll-webapp "slo-loading-then-done")
      ;; Act: the document finishes, still carrying no hook.
      (setq agent-repl-test--slo-probe-ready-state "complete")
      (agent-repl--recovery-slo-poll-webapp "slo-loading-then-done")
      ;; Assert
      (should (equal "absent"
                     (plist-get (gethash "slo-loading-then-done"
                                         agent-repl--recovery-slo-attempts)
                                :probe))))))

(ert-deftest agent-repl-test-recovery-slo-answering-page-reports-present-with-numbers ()
  "A live page whose probe answers with real data reports present and measured."
  (agent-repl-test--with-slo
    (agent-repl-test--with-slo-ws "slo-live"
      ;; Arrange
      (setq agent-repl-test--slo-probe-answer
            (json-encode '((adopted . t) (realDataFrames . 3))))
      (agent-repl--recovery-slo-open "slo-live")
      (agent-repl-test--slo-satisfy "slo-live" 'emacs 'wire)
      ;; Act
      (agent-repl--recovery-slo-check "slo-live")
      ;; Assert
      (let ((record (agent-repl-test--slo-record 'info)))
        (should (string-match-p "probe=present" record))
        (should (string-match-p "outcome=recovered" record))
        (should-not (string-match-p "webapp_ms=-1" record))))))

(ert-deftest agent-repl-test-recovery-slo-page-that-never-answers-records-silent ()
  "A workspace whose page never replies is recorded as probe=silent."
  (agent-repl-test--with-slo
    (agent-repl-test--with-slo-ws "slo-silent"
      ;; Arrange: the read path injects and the page never calls back.
      (cl-letf (((symbol-function 'agent-repl--frontend-webview-read-script)
                 (lambda (_buf _script _callback) t)))
        (agent-repl--recovery-slo-open "slo-silent")
        (agent-repl-test--slo-satisfy "slo-silent" 'emacs 'wire)
        (agent-repl-test--slo-age "slo-silent" (1+ agent-repl-recovery-slo-budget-ms))
        ;; Act
        (agent-repl--recovery-slo-check "slo-silent"))
      ;; Assert
      (should (string-match-p "probe=silent" (agent-repl-test--slo-record 'warn))))))

(ert-deftest agent-repl-test-recovery-slo-answering-page-not-yet-recovered-records-present ()
  "A page whose probe answers `not yet' is probe=present with webapp_ms=-1."
  (agent-repl-test--with-slo
    (agent-repl-test--with-slo-ws "slo-not-yet"
      ;; Arrange: the probe exists and reports an unsatisfied report.
      (setq agent-repl-test--slo-probe-answer
            (json-serialize '(:adopted t :realDataFrames 0)))
      (agent-repl--recovery-slo-open "slo-not-yet")
      (agent-repl-test--slo-satisfy "slo-not-yet" 'emacs 'wire)
      (agent-repl-test--slo-age "slo-not-yet" (1+ agent-repl-recovery-slo-budget-ms))
      ;; Act
      (agent-repl--recovery-slo-check "slo-not-yet")
      ;; Assert
      (let ((record (agent-repl-test--slo-record 'warn)))
        (should (string-match-p "probe=present" record))
        (should (string-match-p "webapp_ms=-1" record))))))

(ert-deftest agent-repl-test-recovery-slo-probe-presence-is-never-downgraded ()
  "A page that proved it carries the probe keeps that proof through silence."
  (agent-repl-test--with-slo
    ;; Arrange
    (agent-repl--recovery-slo-open "slo-monotonic")
    (agent-repl--recovery-slo-note-probe "slo-monotonic" "present")
    ;; Act
    (agent-repl--recovery-slo-note-probe "slo-monotonic" "absent")
    ;; Assert
    (should (equal "present" (plist-get (gethash "slo-monotonic"
                                             agent-repl--recovery-slo-attempts)
                                    :probe)))))

(ert-deftest agent-repl-test-recovery-slo-forced-attempt-is-still-polled ()
  "The tick keeps asking a forced page, which is what makes re-verification see it.
The reply crosses the xwidget boundary as an input event, so the poll
that produces the answer must be issued at least one tick BEFORE the
re-verification reads it."
  (agent-repl-test--with-slo
    (agent-repl-test--with-slo-ws "slo-forced-poll"
      ;; Arrange: breach, so the attempt is :forced.
      (agent-repl--recovery-slo-open "slo-forced-poll")
      (agent-repl-test--slo-age "slo-forced-poll" (1+ agent-repl-recovery-slo-budget-ms))
      (agent-repl--recovery-slo-check "slo-forced-poll")
      (setq agent-repl-test--slo-probe-answer
            (json-serialize '(:adopted t :realDataFrames 1)))
      ;; Act: one further tick, which is the only thing that polls now.
      (should (eq (agent-repl--recovery-slo-check "slo-forced-poll") 'pending))
      ;; Assert: the page's answer landed in the attempt.
      (should (plist-get (gethash "slo-forced-poll" agent-repl--recovery-slo-attempts)
                         :webapp)))))

(ert-deftest agent-repl-test-recovery-slo-reverify-reads-a-page-a-tick-already-polled ()
  "Re-verification reports the page signal a preceding tick's reply stamped."
  (agent-repl-test--with-slo
    (agent-repl-test--with-slo-ws "slo-reverify-page"
      ;; Arrange: breach, then the page comes back and a tick polls it.
      (agent-repl--recovery-slo-open "slo-reverify-page")
      (agent-repl-test--slo-satisfy "slo-reverify-page" 'emacs 'wire)
      (agent-repl-test--slo-age "slo-reverify-page"
                                (1+ agent-repl-recovery-slo-budget-ms))
      (agent-repl--recovery-slo-check "slo-reverify-page")
      (setq agent-repl-test--slo-probe-answer
            (json-serialize '(:adopted t :realDataFrames 1)))
      (agent-repl--recovery-slo-check "slo-reverify-page")
      ;; Act
      (agent-repl--recovery-slo-reverify "slo-reverify-page")
      ;; Assert
      (should (string-match-p "outcome=forced-recovered"
                              (or (agent-repl-test--slo-record 'info) ""))))))

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
