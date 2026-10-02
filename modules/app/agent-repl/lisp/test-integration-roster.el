;;; test-integration-roster.el --- Integration: roster.el against the fake daemon -*- lexical-binding: t; -*-

;;; Commentary:

;; Scenarios 9, 10 and 11 of elisp-fanout.md §14.
;;
;; 9.  Every RosterRow.status arm the frozen contract declares decodes, and a
;;     breach (an unset status oneof) is logged ERROR, dropped, and leaves the
;;     stream standing.
;; 10. Tab reconciliation: rows appear, `closed' rows are torn down, a
;;     daemon-originated `current' change is a tab-SWITCH REQUEST (R8), and
;;     Emacs's own switch produces exactly ONE SelectWorkspace (re-selection
;;     is idempotent, so no loop forms).
;; 11. The FINISH EDGE — the roster row's turn-running → idle transition — as
;;     the one trigger for all four Emacs-local reactions.
;;
;; The roster is READ-ONLY for Emacs: the daemon authors it wholesale, and
;; Emacs's only two inputs to it are Register and Select.

;;; Code:

(require 'ert)
(require 'cl-lib)

(load (expand-file-name "test-integration-helpers.el"
                        (file-name-directory (or load-file-name buffer-file-name)))
      nil t)

;; Production names this suite depends on (roster.el and status.el, §8).
(declare-function agent-repl-roster-subscribe "roster")
(declare-function agent-repl-roster-reconcile "roster")
(declare-function agent-repl-status-tab-state "status")
(declare-function agent-repl-status-sync-attention "status")
(declare-function agent-repl-status-blink-tab "status")
(declare-function agent-repl-connect-open "connect")
(declare-function agent-repl-connect-close "connect")
(declare-function agent-repl--ws-by-ref-id "workspace")
(declare-function agent-repl--ws-known-p "workspace" (ws))
(declare-function agent-repl-roster-tab-order "roster")
(declare-function agent-repl--ws-put "workspace")
(declare-function agent-repl--ws-switch "workspace")
(declare-function agent-repl--ws-dir-owner "workspace")
(declare-function agent-repl-status-tab-glyph "status" (ws arm))
(declare-function agent-repl-status-attention-visible-p "status" (ws))
(declare-function agent-repl--tab-badge-str "status" (name arm))
(declare-function agent-repl-host-ref "host" (ws))
(declare-function agent-repl-host--on-workspace-activated "host" (&rest _))
(declare-function agent-repl-roster-echo-finished "roster" (ws))
(declare-function agent-repl-roster-refresh-magit "roster" (ws))
(declare-function agent-repl--input-said "input" (text attachments))
(defvar agent-repl-roster-view)
(defvar agent-repl-roster-update-functions)
(defvar agent-repl-roster-finish-functions)
(defvar agent-repl-host-last-selected-id)
(defvar agent-repl-link--primary)
(defvar persp-activated-functions)

;;;; ---- Fixtures ----

(defconst agent-repl-itest-roster--repo-dir
  (agent-repl-itest--fixture-dir "roster-repo")
  "The repository directory every fixture row lives under.")

(defun agent-repl-itest-roster--row (id name status &rest overrides)
  "Return a RosterRow protojson alist.
ID names the workspace, NAME its row label, STATUS the status arm's
protojson key symbol.  OVERRIDES replaces entries, so one test changes
exactly one fact.

Every non-optional field is populated.  `when' carries an unset oneof and
`detail' carries no lines, both of which the proto comments declare legal
— presence itself is the fact for RosterRowDetail's three lines."
  (append overrides
          `((workspace . ((workspace . ((id . ,id)
                                        (dir . ,(agent-repl-itest--fixture-dir (concat "roster-" id)))))))
            (name . ((text . ,name)))
            (,status . ())
            (current . ((current . :false)))
            (when . ())
            (detail . ())
            (closed . ((closed . :false)))
            (availability . ((available . ()))))))

(defun agent-repl-itest-roster--roster (rows &rest overrides)
  "Return a WorkspaceRoster protojson alist carrying ROWS in one repo section.
OVERRIDES replaces top-level entries (notably `current')."
  (append
   overrides
   `((repository
      . ((sections
          . [((key . ((repository . ((id . "repo-itest")
                                     (dir . ,agent-repl-itest-roster--repo-dir)))))
              (header . ((label . ((text . "itest-repo"))) (count . ((workspaces . 1)))))
              (rows . ((rows . ,(vconcat rows))))
             (expanded . ()))])))
     (task . ((sections . [])))
     (recentlyMerged . ((header . ((label . ((text . "recently merged"))) (count . ((workspaces . 1)))))
                         (rows . ((rows . []))))))))

(defun agent-repl-itest-roster--push (daemon roster)
  "Push ROSTER on DAEMON's roster stream."
  (agent-repl-itest--push daemon "roster" `((roster . ,roster))))

(defmacro agent-repl-itest-roster--with-subscription (daemon &rest body)
  "Subscribe the roster consumer to DAEMON, run BODY, then tear it down."
  (declare (indent 1) (debug (form body)))
  `(let ((conn (agent-repl-connect-open (agent-repl-itest-daemon-address ,daemon)))
         (agent-repl-roster-view nil)
         (agent-repl-roster-update-functions nil)
         (agent-repl-roster-finish-functions nil))
     (unwind-protect
         (progn
           (agent-repl-roster-subscribe conn)
           (agent-repl-itest--await-subscriber ,daemon "roster")
           ,@body)
       (agent-repl-connect-close conn))))

(defun agent-repl-itest-roster--await-view (daemon)
  "Block until a roster push has reached `agent-repl-roster-view'."
  (agent-repl-itest--wait-until (lambda () agent-repl-roster-view) nil
                                "a roster push to reach the view")
  (ignore daemon))

(defun agent-repl-itest-roster--roster-two-sections (rows-a rows-b)
  "Return a WorkspaceRoster carrying TWO repo sections, A then B, in order.
`--roster' only ever builds one section; the section-order findings need
a second one, with its own distinct repo key and label, to tell walk
order apart from declaration order."
  `((repository
     . ((sections
         . [((key . ((repository . ((id . "repo-a") (dir . ,(agent-repl-itest--fixture-dir "itest-repo-a"))))))
             (header . ((label . ((text . "repo-a"))) (count . ((workspaces . 1)))))
             (rows . ((rows . ,(vconcat rows-a))))
             (expanded . ()))
            ((key . ((repository . ((id . "repo-b") (dir . ,(agent-repl-itest--fixture-dir "itest-repo-b"))))))
             (header . ((label . ((text . "repo-b"))) (count . ((workspaces . 1)))))
             (rows . ((rows . ,(vconcat rows-b))))
             (expanded . ()))])))
    (task . ((sections . [])))
    (recentlyMerged . ((header . ((label . ((text . "recently merged"))) (count . ((workspaces . 1)))))
                        (rows . ((rows . [])))))))

(defun agent-repl-itest-roster--row-missing (id name status omit)
  "Return a RosterRow like `--row', but with field OMIT entirely absent.
Findings 48 pin the validation invariant against each non-optional field
in turn — the field must be MISSING, not merely empty, so this deletes
the key rather than nulling its value.

THE SPINE IS COPIED FIRST.  `assq-delete-all' deletes by `setcdr', and
the builders above return backquoted structure whose constant tail is a
SHARED LITERAL — deleting through it would strip the field from every
later call in the same Emacs process, silently poisoning the rest of the
suite."
  (assq-delete-all omit (copy-sequence (agent-repl-itest-roster--row id name status))))

(defun agent-repl-itest-roster--roster-missing (rows omit)
  "Return a WorkspaceRoster like `--roster', but with field OMIT absent.
The spine is copied for the reason `--row-missing' documents."
  (assq-delete-all omit (copy-sequence (agent-repl-itest-roster--roster rows))))

(defun agent-repl-itest-roster--push-invalid-carries-raw (daemon needle)
  "Return non-nil when a push-invalid ERROR log entry's context mentions NEEDLE.
Fanout §4: an invalid push is logged \"with the raw JSON in context\" —
the elisp side records the pre-decode payload (`context.arguments', per
`agent-repl--log-record') so the diagnosis has the body that failed, not
only the fact that something did."
  (seq-some
   (lambda (entry)
     (let* ((context (alist-get 'context entry))
            (arguments (alist-get 'arguments context)))
       (seq-some (lambda (arg) (and (stringp arg) (string-match-p (regexp-quote needle) arg)))
                 arguments)))
   (agent-repl-itest--log-entries daemon "elisp.rpc.push-invalid" "error")))

(defmacro agent-repl-itest-roster--with-real-select (&rest body)
  "Run BODY with a REAL `agent-repl--ws-switch' able to drive a real Select.
Doom's persp-mode is never loaded in this batch harness, so
`agent-repl--ws-add-activated-hook''s `with-eval-after-load' `persp-mode'
body (host.el's own registration of
`agent-repl-host--on-workspace-activated') never runs — the hook that
turns a tab switch into a SelectWorkspace would otherwise be entirely
absent from every integration test.  This macro registers it for real and
simulates ONLY the Doom/persp-mode primitive underneath it
\(`+workspace-switch', already a test-helpers no-op stub for every other
test in this harness\) as firing `persp-activated-functions' the way the
real one does; `agent-repl--ws-switch' and
`agent-repl-host--on-workspace-activated' themselves run completely
unstubbed."
  (declare (indent 0) (debug body))
  `(let ((agent-repl-itest-roster--current-ws nil))
     (add-hook 'persp-activated-functions #'agent-repl-host--on-workspace-activated)
     (unwind-protect
         (cl-letf (((symbol-function '+workspace-switch)
                    (lambda (name &rest _)
                      (setq agent-repl-itest-roster--current-ws name)
                      (run-hook-with-args 'persp-activated-functions)))
                   ((symbol-function '+workspace-current-name)
                    (lambda () agent-repl-itest-roster--current-ws)))
           ,@body)
       (remove-hook 'persp-activated-functions #'agent-repl-host--on-workspace-activated))))

;;;; ---- Scenario 9: every status arm ----

(defconst agent-repl-itest-roster--status-arms
  '((submitting . :submitting)
    (thinking . :thinking)
    (clearing . :clearing)
    (compacting . :compacting)
    (permission . :permission)
    (done . :done)
    (interrupted . :interrupted)
    (turnFailed . :turn-failed)
    (ready . :ready)
    (idleAsync . :idle-async)
    (vendorBlocked . :vendor-blocked)
    (apiRetrying . :api-retrying)
    (init . :init)
    (severed . :severed)
    (startFailed . :start-failed)
    (degraded . :degraded)
    (dead . :dead)
    (merging . :merging)
    (mergeQueued . :merge-queued)
    (mergeFailed . :merge-failed)
    (merged . :merged)
    (none . :none)
    (inactive . :inactive))
  "Every RosterRow.status arm frontend/v1/sidebar.proto declares, and the
keyword §8 pins for it.  The list is EXHAUSTIVE by contract: 22 arms, and
the roster's vocabulary is the ONE source for tab coloring and the sidebar
dot.  A 23rd arm appearing on the wire must be a loud failure, not a
silent default, which is why the suite pins the count as well as the
mapping.")

(ert-deftest agent-repl-itest-roster-declares-the-twenty-three-status-arms ()
  "The suite's arm table matches the contract's 23 arms exactly.
A drifted table would let a new arm ship untested, and the coloring would
silently fall through to `none'.  `merge_enqueuing' and `merge_conflict'
are retired (merge-landing.md, Landed change 1)."
  ;; Arrange / Act / Assert.
  (should (equal 23 (length agent-repl-itest-roster--status-arms))))

(ert-deftest agent-repl-itest-roster-every-status-arm-decodes-to-its-keyword ()
  "Each of the 22 status arms resolves to exactly one tab-state keyword.
The roster's per-row state vocabulary is the ONE source for tab coloring;
there is no HostWorkspace lifecycle axis to fall back on."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest-roster--with-subscription daemon
      (dolist (case agent-repl-itest-roster--status-arms)
        (let* ((arm (car case))
               (expected (cdr case))
               (ws-name (format "itest-roster-%s" arm)))
          (agent-repl--ws-put ws-name :project-dir (agent-repl-itest--fixture-dir ws-name))
          ;; Act.
          (agent-repl-itest-roster--push
           daemon (agent-repl-itest-roster--roster
                   (list (agent-repl-itest-roster--row ws-name ws-name arm))))
          ;; Assert.
          (agent-repl-itest--wait-until
           (lambda () (eq (agent-repl-status-tab-state ws-name) expected))
           nil (format "the %s arm to resolve to %s" arm expected))
          (should (eq (agent-repl-status-tab-state ws-name) expected)))))))

(ert-deftest agent-repl-itest-roster-row-without-a-status-is-refused ()
  "A row whose status oneof is unset is a contract breach: ERROR, dropped.
An unset oneof is an error BY DEFAULT — there is no neutral status to
fall back to, and inventing one would paint a wrong tab silently."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest-roster--with-subscription daemon
      ;; Act: every other field present, no status arm.
      (agent-repl-itest-roster--push
       daemon (agent-repl-itest-roster--roster
               (list `((workspace . ((workspace . ((id . "ws-x") (dir . ,(agent-repl-itest--fixture-dir "ws-x"))))))
                       (name . ((text . "ws-x")))
                       (current . ((current . :false)))
                       (when . ())
                       (detail . ())
                       (closed . ((closed . :false)))))))
      ;; Assert.
      (agent-repl-itest--await-log daemon "elisp.rpc.push-invalid" "error")
      (should (agent-repl-itest--logged-p daemon "elisp.rpc.push-invalid" "error")))))

(ert-deftest agent-repl-itest-roster-invalid-push-leaves-the-stream-standing ()
  "A dropped roster push does not end the stream; the next one arrives.
Losing the roster would lose the ONLY source of which workspaces exist."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest-roster--with-subscription daemon
      (agent-repl-itest-roster--push
       daemon (agent-repl-itest-roster--roster
               (list `((workspace . ((workspace . ((id . "ws-x") (dir . ,(agent-repl-itest--fixture-dir "ws-x"))))))
                       (name . ((text . "ws-x")))
                       (current . ((current . :false)))
                       (when . ())
                       (detail . ())
                       (closed . ((closed . :false)))))))
      (agent-repl-itest--await-log daemon "elisp.rpc.push-invalid" "error")
      ;; Act.
      (agent-repl-itest-roster--push
       daemon (agent-repl-itest-roster--roster
               (list (agent-repl-itest-roster--row "ws-ok" "ws-ok" 'ready))))
      ;; Assert.
      (agent-repl-itest-roster--await-view daemon)
      (should agent-repl-roster-view))))

(ert-deftest agent-repl-itest-roster-unknown-status-arm-never-reaches-emacs ()
  "An invented status arm is refused before it can be pushed at all.
The fake validates every push against the GENERATED response type, so
this pins that no such arm exists on the wire to be handled — the retired
`hibernated' vocabulary above all."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest-roster--with-subscription daemon
      ;; Act.
      (let ((result (agent-repl-itest--control
                     daemon "/_fake/push"
                     (json-serialize
                      `((stream . "roster")
                        (message
                         . ((roster
                             . ,(agent-repl-itest-roster--roster
                                 (list (agent-repl-itest-roster--row
                                        "ws-h" "ws-h" 'hibernated)))))))))))
        ;; Assert.
        (should (equal (car result) 400))))))

(ert-deftest agent-repl-itest-roster-attention-marker-triggers-the-blink ()
  "A row carrying `attention' blinks that tab per the canonical cadence.
RosterRowAttention's own comment is the ONE cadence spec; the webapp
sidebar and the Emacs tab-bar both implement exactly it."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest-roster--with-subscription daemon
      ;; The fixture blanks the update hook so scenarios do not see each
      ;; other's reactions; THIS scenario's subject IS that reaction, so it
      ;; puts the production consumer back and nothing else.
      (let ((blinked nil)
            (agent-repl-roster-update-functions
             (list #'agent-repl-status-sync-attention)))
        (agent-repl--ws-put "itest-attn" :project-dir (agent-repl-itest--fixture-dir "itest-attn"))
        (cl-letf (((symbol-function 'agent-repl-status-blink-tab)
                   (lambda (ws) (push ws blinked))))
          ;; Act.
          (agent-repl-itest-roster--push
           daemon (agent-repl-itest-roster--roster
                   (list (agent-repl-itest-roster--row
                          "itest-attn" "itest-attn" 'thinking
                          '(attention . ())))))
          ;; Assert.
          (agent-repl-itest--wait-until (lambda () blinked) nil "the attention blink")
          (should (member "itest-attn" blinked)))))))

(ert-deftest agent-repl-itest-roster-attention-blink-fires-the-exact-schedule ()
  "The attention blink cadence is EXACTLY 0/500/1000/1500/2000 ms, unstubbed.
RosterRowAttention's own comment is THE CANONICAL BLINK CADENCE: two
blinks — 500 ms on, 500 ms off, twice — then a steady marker until
cleared.  Driven by a REAL attention push through status.el's real
`agent-repl-status-blink-tab'; only `run-with-timer' is instrumented, to
record what it is asked to schedule, and every call still runs for real."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest-roster--with-subscription daemon
      (let ((captured nil)
            (real-run-with-timer (symbol-function 'run-with-timer))
            (agent-repl-roster-update-functions
             (list #'agent-repl-status-sync-attention)))
        (agent-repl--ws-put "itest-cadence" :project-dir (agent-repl-itest--fixture-dir "itest-cadence"))
        (cl-letf (((symbol-function 'run-with-timer)
                   (lambda (delay repeat fn &rest args)
                     (when (eq fn #'agent-repl-status--set-marker)
                       (push (cons delay (nth 1 args)) captured))
                     (apply real-run-with-timer delay repeat fn args))))
          ;; Act.
          (agent-repl-itest-roster--push
           daemon (agent-repl-itest-roster--roster
                   (list (agent-repl-itest-roster--row
                          "itest-cadence" "itest-cadence" 'thinking
                          '(attention . ())))))
          ;; Assert.
          (agent-repl-itest--wait-until (lambda () (= 5 (length captured))) nil
                                        "all five blink steps to be scheduled")
          (should (equal (reverse captured)
                         '((0.0 . t) (0.5 . nil) (1.0 . t) (1.5 . nil) (2.0 . t)))))))))

(ert-deftest agent-repl-itest-roster-attention-that-returns-restarts-the-cadence ()
  "An attention marker that leaves and returns RESTARTS the cadence.
status.el: a second blink RESTARTS the cadence rather than interleaving
with it, because each step is armed under a deterministic per-workspace
key that replaces its predecessor.  Two arrivals must leave exactly ONE
five-step schedule standing, not ten steps."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest-roster--with-subscription daemon
      (let ((captured nil)
            (real-run-with-timer (symbol-function 'run-with-timer))
            (agent-repl-roster-update-functions
             (list #'agent-repl-status-sync-attention)))
        (agent-repl--ws-put "itest-recadence" :project-dir (agent-repl-itest--fixture-dir "itest-recadence"))
        (cl-letf (((symbol-function 'run-with-timer)
                   (lambda (delay repeat fn &rest args)
                     (let ((timer (apply real-run-with-timer delay repeat fn args)))
                       (when (eq fn #'agent-repl-status--set-marker)
                         (push (list delay timer) captured))
                       timer))))
          (agent-repl-itest-roster--push
           daemon (agent-repl-itest-roster--roster
                   (list (agent-repl-itest-roster--row
                          "itest-recadence" "itest-recadence" 'thinking
                          '(attention . ())))))
          (agent-repl-itest--wait-until (lambda () (= 5 (length captured))) nil
                                        "the first schedule's five timers")
          (let ((first-schedule (copy-sequence captured)))
            ;; Act: the marker leaves, then arrives again.
            (agent-repl-itest-roster--push
             daemon (agent-repl-itest-roster--roster
                     (list (agent-repl-itest-roster--row
                            "itest-recadence" "itest-recadence" 'thinking))))
            (agent-repl-itest-roster--push
             daemon (agent-repl-itest-roster--roster
                     (list (agent-repl-itest-roster--row
                            "itest-recadence" "itest-recadence" 'thinking
                            '(attention . ())))))
            ;; Assert.
            (agent-repl-itest--wait-until (lambda () (= 10 (length captured))) nil
                                          "the second schedule's five more timers")
            ;; The first schedule's own timers were cancelled: the marker's
            ;; clear cancels a blink in flight, and the new blink's steps
            ;; replace their keys.  The 0 ms step has almost certainly
            ;; already FIRED, which also removes it from `timer-list', so it
            ;; is excluded, not asserted false.
            (dolist (entry first-schedule)
              (let ((delay (car entry)) (timer (cadr entry)))
                (unless (= delay 0.0)
                  (should-not (memq timer timer-list)))))))))))

(ert-deftest agent-repl-itest-roster-attention-still-present-does-not-reblink ()
  "A row that keeps `attention' across a re-push does not blink a second time.
Fanout §8: \"Attention present → blink once (below) then a steady marker
until the marker leaves the row\" — a repeated blink for the same unread
notification would be a spurious re-alert."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest-roster--with-subscription daemon
      ;; The fixture blanks the update hook; this scenario's subject IS the
      ;; production consumer's reaction, so it puts that one consumer back.
      (let ((blinked nil)
            (agent-repl-roster-update-functions
             (list #'agent-repl-status-sync-attention)))
        (agent-repl--ws-put "itest-attn2" :project-dir (agent-repl-itest--fixture-dir "itest-attn2"))
        (cl-letf (((symbol-function 'agent-repl-status-blink-tab)
                   (lambda (ws) (push ws blinked))))
          (agent-repl-itest-roster--push
           daemon (agent-repl-itest-roster--roster
                   (list (agent-repl-itest-roster--row
                          "itest-attn2" "itest-attn2" 'thinking
                          '(attention . ())))))
          (agent-repl-itest--wait-until (lambda () blinked) nil "the first blink")
          ;; Act: the same row, attention still present.
          (agent-repl-itest-roster--push
           daemon (agent-repl-itest-roster--roster
                   (list (agent-repl-itest-roster--row
                          "itest-attn2" "itest-attn2" 'thinking
                          '(attention . ())))))
          (agent-repl-itest-roster--await-view daemon)
          ;; Assert.
          (should (equal (length blinked) 1)))))))

(ert-deftest agent-repl-itest-roster-attention-clears-when-removed-from-the-row ()
  "A row that stops carrying `attention' clears the steady marker.
Fanout §8: \"a steady marker until the marker leaves the row\" — a marker
that never clears would flag a workspace with nothing left unread."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest-roster--with-subscription daemon
      ;; The fixture blanks the update hook; the marker's lifecycle IS that
      ;; consumer's work, so it is put back for this scenario.
      (let ((agent-repl-roster-update-functions
             (list #'agent-repl-status-sync-attention)))
      (agent-repl--ws-put "itest-attn3" :project-dir (agent-repl-itest--fixture-dir "itest-attn3"))
      (agent-repl-itest-roster--push
       daemon (agent-repl-itest-roster--roster
               (list (agent-repl-itest-roster--row
                      "itest-attn3" "itest-attn3" 'thinking
                      '(attention . ())))))
      (agent-repl-itest--wait-until
       (lambda () (agent-repl-status-attention-visible-p "itest-attn3"))
       nil "the marker to show steady")
      ;; Act.
      (agent-repl-itest-roster--push
       daemon (agent-repl-itest-roster--roster
               (list (agent-repl-itest-roster--row "itest-attn3" "itest-attn3" 'thinking))))
      ;; Assert.
      (agent-repl-itest--wait-until
       (lambda () (not (agent-repl-status-attention-visible-p "itest-attn3")))
       nil "the marker to clear")
      (should-not (agent-repl-status-attention-visible-p "itest-attn3"))))))

;;;; ---- Scenario 10: tab reconciliation ----

(ert-deftest agent-repl-itest-roster-open-rows-become-known-workspaces ()
  "A `closed = false' row means the workspace exists: open its buffers.
Tab REHYDRATION on connect: the daemon is the source of which workspaces
exist, and Emacs holds no durable roster snapshot of its own."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest-roster--with-subscription daemon
      ;; Act.
      (agent-repl-itest-roster--push
       daemon (agent-repl-itest-roster--roster
               (list (agent-repl-itest-roster--row "itest-open" "itest-open" 'ready))))
      ;; Assert.
      (agent-repl-itest--wait-until
       (lambda () (agent-repl--ws-by-ref-id "itest-open"))
       nil "the open row's tab to be opened")
      (should (equal (agent-repl--ws-by-ref-id "itest-open") "itest-open")))))

(ert-deftest agent-repl-itest-roster-closed-rows-are-torn-down ()
  "A `closed = true' row means tear the tab down; teardown is idempotent.
Merged, closed and killed rows all carry closed = true (E5); a nuked row
leaves the roster entirely."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest-roster--with-subscription daemon
      (agent-repl-itest-roster--push
       daemon (agent-repl-itest-roster--roster
               (list (agent-repl-itest-roster--row "itest-close" "itest-close" 'ready))))
      (agent-repl-itest--wait-until
       (lambda () (agent-repl--ws-by-ref-id "itest-close"))
       nil "the open row's tab to be opened")
      ;; Act.
      (agent-repl-itest-roster--push
       daemon (agent-repl-itest-roster--roster
               (list (agent-repl-itest-roster--row
                      "itest-close" "itest-close" 'merged
                      '(closed . ((closed . t)))))))
      ;; Assert.
      (agent-repl-itest--wait-until
       (lambda () (not (agent-repl--ws-by-ref-id "itest-close")))
       nil "the closed row's tab to be torn down")
      (should-not (agent-repl--ws-by-ref-id "itest-close")))))

(ert-deftest agent-repl-itest-roster-tab-order-is-the-walk-order ()
  "Tab order IS the roster walk order, strictly.
Client-authored ordering and hiding are DEAD — the resolver orders
(priority included), and Emacs reproduces that order and nothing else."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest-roster--with-subscription daemon
      ;; Act.
      (agent-repl-itest-roster--push
       daemon (agent-repl-itest-roster--roster
               (list (agent-repl-itest-roster--row "ws-a" "ws-a" 'ready)
                     (agent-repl-itest-roster--row "ws-b" "ws-b" 'ready)
                     (agent-repl-itest-roster--row "ws-c" "ws-c" 'ready))))
      ;; Assert.
      (agent-repl-itest--wait-until
       (lambda () (equal (agent-repl-roster-tab-order) '("ws-a" "ws-b" "ws-c")))
       nil "the tabs to take the roster's walk order")
      (should (equal (agent-repl-roster-tab-order) '("ws-a" "ws-b" "ws-c"))))))

(ert-deftest agent-repl-itest-roster-reordered-rows-reorder-the-tabs ()
  "A reordered roster reorders the tabs; nothing local pins the old order.
The daemon may reorder at any time — a priority change alone does it —
and Emacs follows rather than remembering."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest-roster--with-subscription daemon
      (agent-repl-itest-roster--push
       daemon (agent-repl-itest-roster--roster
               (list (agent-repl-itest-roster--row "ws-a" "ws-a" 'ready)
                     (agent-repl-itest-roster--row "ws-b" "ws-b" 'ready))))
      (agent-repl-itest--wait-until
       (lambda () (equal (agent-repl-roster-tab-order) '("ws-a" "ws-b")))
       nil "the initial tab order")
      ;; Act: the same two rows, the other way round.
      (agent-repl-itest-roster--push
       daemon (agent-repl-itest-roster--roster
               (list (agent-repl-itest-roster--row "ws-b" "ws-b" 'ready)
                     (agent-repl-itest-roster--row "ws-a" "ws-a" 'ready))))
      ;; Assert.
      (agent-repl-itest--wait-until
       (lambda () (equal (agent-repl-roster-tab-order) '("ws-b" "ws-a")))
       nil "the tabs to follow the new walk order")
      (should (equal (agent-repl-roster-tab-order) '("ws-b" "ws-a"))))))

(ert-deftest agent-repl-itest-roster-closed-rows-get-no-tab-at-all ()
  "A row that arrives already `closed' never gets a tab.
Membership is the whole `closed = false' rule; a merged row arriving on a
fresh connect must not open a tab just because it is on the roster."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest-roster--with-subscription daemon
      ;; Act.
      (agent-repl-itest-roster--push
       daemon (agent-repl-itest-roster--roster
               (list (agent-repl-itest-roster--row "ws-open" "ws-open" 'ready)
                     (agent-repl-itest-roster--row
                      "ws-gone" "ws-gone" 'merged
                      '(closed . ((closed . t)))))))
      ;; Assert.
      (agent-repl-itest--wait-until
       (lambda () (equal (agent-repl-roster-tab-order) '("ws-open")))
       nil "only the open row to get a tab")
      (should (null (agent-repl--ws-by-ref-id "ws-gone"))))))

(ert-deftest agent-repl-itest-roster-daemon-originated-current-switches-the-tab ()
  "A `current' Emacs did not originate is a tab-SWITCH REQUEST (R8).
Cross-workspace navigation from the webapp calls SelectWorkspace, and
Emacs reacts to the resulting roster change; re-selection is idempotent
so no loop forms."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest-roster--with-subscription daemon
      (let ((switched nil)
            (agent-repl-host-last-selected-id nil))
        (agent-repl--ws-put "itest-cur" :project-dir (agent-repl-itest--fixture-dir "itest-cur"))
        (cl-letf (((symbol-function 'agent-repl--ws-switch)
                   (lambda (ws &rest _) (push ws switched))))
          ;; Act.
          (agent-repl-itest-roster--push
           daemon (agent-repl-itest-roster--roster
                   (list (agent-repl-itest-roster--row
                          "itest-cur" "itest-cur" 'ready
                          '(current . ((current . t)))))
                   `(current . ((workspace . ((id . "itest-cur")
                                              (dir . ,(agent-repl-itest--fixture-dir "itest-cur"))))))))
          ;; Assert.
          (agent-repl-itest--wait-until (lambda () switched) nil
                                        "the daemon-originated tab switch")
          (should (member "itest-cur" switched)))))))

(ert-deftest agent-repl-itest-roster-own-selection-does-not-re-select ()
  "A `current' Emacs ITSELF originated causes no second SelectWorkspace.
Emacs records its own last-selected id precisely so the roster echo of
its own act is not mistaken for a request."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest-roster--with-subscription daemon
      (let ((switched nil)
            (agent-repl-host-last-selected-id "itest-own"))
        (agent-repl--ws-put "itest-own" :project-dir (agent-repl-itest--fixture-dir "itest-own"))
        (cl-letf (((symbol-function 'agent-repl--ws-switch)
                   (lambda (ws &rest _) (push ws switched))))
          ;; Act: the roster echoes back the selection Emacs made.
          (agent-repl-itest-roster--push
           daemon (agent-repl-itest-roster--roster
                   (list (agent-repl-itest-roster--row
                          "itest-own" "itest-own" 'ready
                          '(current . ((current . t)))))
                   `(current . ((workspace . ((id . "itest-own")
                                              (dir . ,(agent-repl-itest--fixture-dir "itest-own"))))))))
          (agent-repl-itest-roster--await-view daemon)
          ;; Assert: no switch, and no SelectWorkspace of our own.
          (should (null switched))
          (should (null (agent-repl-itest--calls daemon "SelectWorkspace"))))))))

(ert-deftest agent-repl-itest-roster-row-rename-keeps-the-same-workspace ()
  "A row whose NAME changed but whose ref id did not is the same workspace.
The id is the identity; the name is display text the daemon may re-derive
at any time."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest-roster--with-subscription daemon
      (agent-repl-itest-roster--push
       daemon (agent-repl-itest-roster--roster
               (list (agent-repl-itest-roster--row "itest-ren" "old-name" 'ready))))
      (agent-repl-itest--wait-until
       (lambda () (agent-repl--ws-by-ref-id "itest-ren"))
       nil "the initial row's tab")
      ;; Act.
      (agent-repl-itest-roster--push
       daemon (agent-repl-itest-roster--roster
               (list (agent-repl-itest-roster--row "itest-ren" "new-name" 'ready))))
      ;; Assert: one workspace, under its new name.
      (agent-repl-itest--wait-until
       (lambda () (equal (agent-repl--ws-by-ref-id "itest-ren") "new-name"))
       nil "the renamed row's tab")
      (should (equal (agent-repl--ws-by-ref-id "itest-ren") "new-name")))))

(ert-deftest agent-repl-itest-roster-children-walk-depth-first-before-the-next-sibling ()
  "A row's children are walked depth-first: parent, then child, THEN sibling.
Fanout §8: \"rows depth-first (row, then its children)\" — a family drawn
out of order would tuck a child under the wrong sibling's tab."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest-roster--with-subscription daemon
      (let ((child (agent-repl-itest-roster--row "ws-child" "ws-child" 'ready)))
        ;; Act.
        (agent-repl-itest-roster--push
         daemon (agent-repl-itest-roster--roster
                 (list (agent-repl-itest-roster--row
                        "ws-parent" "ws-parent" 'ready
                        `(children . ,(vector child)))
                       (agent-repl-itest-roster--row "ws-sibling" "ws-sibling" 'ready))))
        ;; Assert.
        (agent-repl-itest--wait-until
         (lambda () (equal (agent-repl-roster-tab-order)
                           '("ws-parent" "ws-child" "ws-sibling")))
         nil "parent, then child, then the next sibling")
        (should (equal (agent-repl-roster-tab-order)
                       '("ws-parent" "ws-child" "ws-sibling")))))))

(ert-deftest agent-repl-itest-roster-recently-merged-rows-walk-after-every-repo-section ()
  "An open `recently_merged' row is walked AFTER every repo section's rows.
Fanout §8: \"then `recently_merged.rows'\" — a merged-but-still-open row
tucked ahead of the repo section would land its tab in the wrong slot."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest-roster--with-subscription daemon
      (let ((merged-row (agent-repl-itest-roster--row "ws-merged-open" "ws-merged-open" 'ready)))
        ;; Act.
        (agent-repl-itest-roster--push
         daemon (agent-repl-itest-roster--roster
                 (list (agent-repl-itest-roster--row "ws-repo" "ws-repo" 'ready))
                 `(recentlyMerged
                   . ((header . ((label . ((text . "recently merged"))) (count . ((workspaces . 1)))))
                      (rows . ((rows . ,(vector merged-row))))))))
        ;; Assert.
        (agent-repl-itest--wait-until
         (lambda () (equal (agent-repl-roster-tab-order) '("ws-repo" "ws-merged-open")))
         nil "the repo section's row, then the recently-merged row")
        (should (equal (agent-repl-roster-tab-order) '("ws-repo" "ws-merged-open")))))))

(ert-deftest agent-repl-itest-roster-two-repo-sections-walk-in-the-resolver-order ()
  "Two repository sections walk in the resolver's order, A before B.
Fanout §8: \"walk `repository.sections' in order\" (sidebar.proto: \"Order
is the resolver's; clients do not re-sort\") — Emacs must never re-sort
sections by, say, label."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest-roster--with-subscription daemon
      ;; Act.
      (agent-repl-itest-roster--push
       daemon (agent-repl-itest-roster--roster-two-sections
               (list (agent-repl-itest-roster--row "ws-a1" "ws-a1" 'ready))
               (list (agent-repl-itest-roster--row "ws-b1" "ws-b1" 'ready))))
      ;; Assert.
      (agent-repl-itest--wait-until
       (lambda () (equal (agent-repl-roster-tab-order) '("ws-a1" "ws-b1")))
       nil "section A's row before section B's")
      (should (equal (agent-repl-roster-tab-order) '("ws-a1" "ws-b1"))))))

(ert-deftest agent-repl-itest-roster-task-view-is-ignored-for-tab-order ()
  "The task view's rows and order never reach the tab bar; no duplicate tabs.
Fanout §8: \"The task view is ignored (the same rows regrouped)\" —
walking it too would double every tab and let its order override the
repository view's."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest-roster--with-subscription daemon
      ;; Act: the task view regroups the SAME two workspaces, reversed.
      (agent-repl-itest-roster--push
       daemon (agent-repl-itest-roster--roster
               (list (agent-repl-itest-roster--row "ws-t1" "ws-t1" 'ready)
                     (agent-repl-itest-roster--row "ws-t2" "ws-t2" 'ready))
               `(task
                 . ((sections
                     . [((key . ((taskId . "task-1")))
                         (header . ((label . ((text . "task")))
                                    (done . ((done . :false)))))
                         (rows
                          . ((rows
                              . ,(vector
                                  (agent-repl-itest-roster--row "ws-t2" "ws-t2" 'ready)
                                  (agent-repl-itest-roster--row "ws-t1" "ws-t1" 'ready))))))])))))
      ;; Assert.
      (agent-repl-itest--wait-until
       (lambda () (equal (agent-repl-roster-tab-order) '("ws-t1" "ws-t2")))
       nil "the repository view's order, never the task view's")
      (should (equal (agent-repl-roster-tab-order) '("ws-t1" "ws-t2")))
      (should (equal (length (agent-repl-roster-tab-order)) 2)))))

(ert-deftest agent-repl-itest-roster-colliding-names-get-the-repo-label-suffix ()
  "Two rows named identically in different repos both get \"·<repo label>\".
Fanout §8: \"on collision within the roster, append '·<repo label>'\" —
without it, two same-named workspaces would draw as indistinguishable
tabs."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest-roster--with-subscription daemon
      ;; Act.
      (agent-repl-itest-roster--push
       daemon (agent-repl-itest-roster--roster-two-sections
               (list (agent-repl-itest-roster--row "ws-fix-a" "fix" 'ready))
               (list (agent-repl-itest-roster--row "ws-fix-b" "fix" 'ready))))
      ;; Assert.
      (agent-repl-itest--wait-until
       (lambda () (equal (length (agent-repl-roster-tab-order)) 2))
       nil "both colliding rows to open a tab")
      (should (equal (agent-repl-roster-tab-order) '("fix·repo-a" "fix·repo-b"))))))

(ert-deftest agent-repl-itest-roster-current-matching-selected-tab-does-not-switch ()
  "`current' naming the ALREADY-SELECTED tab switches nothing, whatever
`agent-repl-host-last-selected-id' says.
Fanout §8: switch only when `current' differs from BOTH the selected
tab's ref id AND `agent-repl-host-last-selected-id' — matching either one
is enough to skip."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest-roster--with-subscription daemon
      (let ((switched nil)
            (agent-repl-host-last-selected-id "some-other-id"))
        (agent-repl--ws-put "itest-cur-eq" :project-dir (agent-repl-itest--fixture-dir "itest-cur-eq"))
        (cl-letf (((symbol-function 'agent-repl--ws-switch)
                   (lambda (ws &rest _) (push ws switched)))
                  ((symbol-function '+workspace-current-name)
                   (lambda () "itest-cur-eq")))
          ;; Act.
          (agent-repl-itest-roster--push
           daemon (agent-repl-itest-roster--roster
                   (list (agent-repl-itest-roster--row
                          "itest-cur-eq" "itest-cur-eq" 'ready
                          '(current . ((current . t)))))
                   `(current . ((workspace . ((id . "itest-cur-eq")
                                              (dir . ,(agent-repl-itest--fixture-dir "itest-cur-eq"))))))))
          (agent-repl-itest-roster--await-view daemon)
          ;; Assert.
          (should (null switched)))))))

(ert-deftest agent-repl-itest-roster-roster-without-current-does-not-switch-or-error ()
  "A roster carrying no `current' at all switches nothing and logs no error.
sidebar.proto: \"UNSET when there is none\" — an absent selection is
legal, never a contract breach."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest-roster--with-subscription daemon
      (let ((switched nil))
        (agent-repl--ws-put "itest-nocur" :project-dir (agent-repl-itest--fixture-dir "itest-nocur"))
        (cl-letf (((symbol-function 'agent-repl--ws-switch)
                   (lambda (ws &rest _) (push ws switched))))
          ;; Act: `--roster' emits no `current' key at all unless overridden.
          (agent-repl-itest-roster--push
           daemon (agent-repl-itest-roster--roster
                   (list (agent-repl-itest-roster--row "itest-nocur" "itest-nocur" 'ready))))
          (agent-repl-itest-roster--await-view daemon)
          ;; Assert.
          (should (null switched))
          (should-not (agent-repl-itest--logged-p daemon "elisp.rpc.push-invalid" "error")))))))

(ert-deftest agent-repl-itest-roster-emacs-own-switch-sends-exactly-one-select-workspace ()
  "Emacs's own tab switch sends ONE SelectWorkspace even once the echo returns.
Fanout §14 scenario 10: \"Emacs's own switch → exactly one SelectWorkspace\"
— host.el's activation hook fires the real Select, and the roster's echo
of that very selection must not fire a second one."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest-roster--with-subscription daemon
      (let ((agent-repl-link--primary conn)
            (agent-repl-host-last-selected-id nil))
        (agent-repl-itest-roster--push
         daemon (agent-repl-itest-roster--roster
                 (list (agent-repl-itest-roster--row "itest-ownswitch" "itest-ownswitch" 'ready))))
        (agent-repl-itest--wait-until
         (lambda () (agent-repl-host-ref "itest-ownswitch"))
         nil "the tab's host ref to attach")
        (agent-repl-itest-roster--with-real-select
          ;; Act: Emacs switches to the tab for real.
          (agent-repl--ws-switch "itest-ownswitch")
          (agent-repl-itest--await-call daemon "SelectWorkspace")
          ;; The daemon echoes the selection back on the roster.
          (agent-repl-itest-roster--push
           daemon (agent-repl-itest-roster--roster
                   (list (agent-repl-itest-roster--row
                          "itest-ownswitch" "itest-ownswitch" 'ready
                          '(current . ((current . t)))))
                   `(current . ((workspace . ((id . "itest-ownswitch")
                                              (dir . ,(agent-repl-itest--fixture-dir "itest-roster-itest-ownswitch"))))))))
          (agent-repl-itest-roster--await-view daemon)
          ;; Assert.
          (should (equal (length (agent-repl-itest--calls daemon "SelectWorkspace")) 1)))))))

(ert-deftest agent-repl-itest-roster-daemon-originated-switch-does-not-loop-into-a-second-select ()
  "R8's echo is idempotent for real: one daemon-originated switch, one Select.
A `current' Emacs did not ask for drives a REAL `agent-repl--ws-switch',
which (unstubbed) runs host.el's activation hook and sends a REAL
SelectWorkspace; the daemon's own echo of that selection must not loop
into a second one."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest-roster--with-subscription daemon
      (let ((agent-repl-link--primary conn)
            (agent-repl-host-last-selected-id nil))
        (agent-repl-itest-roster--push
         daemon (agent-repl-itest-roster--roster
                 (list (agent-repl-itest-roster--row "itest-r8loop" "itest-r8loop" 'ready))))
        (agent-repl-itest--wait-until
         (lambda () (agent-repl-host-ref "itest-r8loop"))
         nil "the tab's host ref to attach")
        (agent-repl-itest-roster--with-real-select
          ;; Act: the daemon names a `current' Emacs never asked for.
          (agent-repl-itest-roster--push
           daemon (agent-repl-itest-roster--roster
                   (list (agent-repl-itest-roster--row
                          "itest-r8loop" "itest-r8loop" 'ready
                          '(current . ((current . t)))))
                   `(current . ((workspace . ((id . "itest-r8loop")
                                              (dir . ,(agent-repl-itest--fixture-dir "itest-roster-itest-r8loop"))))))))
          (agent-repl-itest--await-call daemon "SelectWorkspace")
          ;; The daemon echoes Emacs's own resulting selection back.
          (agent-repl-itest-roster--push
           daemon (agent-repl-itest-roster--roster
                   (list (agent-repl-itest-roster--row
                          "itest-r8loop" "itest-r8loop" 'ready
                          '(current . ((current . t)))))
                   `(current . ((workspace . ((id . "itest-r8loop")
                                              (dir . ,(agent-repl-itest--fixture-dir "itest-roster-itest-r8loop"))))))))
          (agent-repl-itest-roster--await-view daemon)
          ;; Assert.
          (should (equal (length (agent-repl-itest--calls daemon "SelectWorkspace")) 1)))))))

;;;; ---- Validation: RosterRow and WorkspaceRoster non-optional fields ----

(ert-deftest agent-repl-itest-roster-row-missing-workspace-is-refused ()
  "A row with no `workspace' field at all is a contract breach: ERROR, dropped.
Fanout §0: a push missing a non-optional field is a validation-invariant
breach, exactly like an unset oneof."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest-roster--with-subscription daemon
      ;; Act.
      (agent-repl-itest-roster--push
       daemon (agent-repl-itest-roster--roster
               (list (agent-repl-itest-roster--row-missing
                      "missing-workspace-marker" "missing-workspace-marker" 'ready 'workspace))))
      ;; Assert.
      (agent-repl-itest--await-log daemon "elisp.rpc.push-invalid" "error")
      (should (agent-repl-itest--logged-p daemon "elisp.rpc.push-invalid" "error"))
      (should (agent-repl-itest-roster--push-invalid-carries-raw
               daemon "missing-workspace-marker")))))

(ert-deftest agent-repl-itest-roster-row-missing-name-is-refused ()
  "A row with no `name' field at all is a contract breach: ERROR, dropped."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest-roster--with-subscription daemon
      ;; Act.
      (agent-repl-itest-roster--push
       daemon (agent-repl-itest-roster--roster
               (list (agent-repl-itest-roster--row-missing
                      "missing-name-marker" "missing-name-marker" 'ready 'name))))
      ;; Assert.
      (agent-repl-itest--await-log daemon "elisp.rpc.push-invalid" "error")
      (should (agent-repl-itest--logged-p daemon "elisp.rpc.push-invalid" "error"))
      (should (agent-repl-itest-roster--push-invalid-carries-raw
               daemon "missing-name-marker")))))

(ert-deftest agent-repl-itest-roster-row-missing-current-is-refused ()
  "A row with no `current' field at all is a contract breach: ERROR, dropped."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest-roster--with-subscription daemon
      ;; Act.
      (agent-repl-itest-roster--push
       daemon (agent-repl-itest-roster--roster
               (list (agent-repl-itest-roster--row-missing
                      "missing-current-marker" "missing-current-marker" 'ready 'current))))
      ;; Assert.
      (agent-repl-itest--await-log daemon "elisp.rpc.push-invalid" "error")
      (should (agent-repl-itest--logged-p daemon "elisp.rpc.push-invalid" "error"))
      (should (agent-repl-itest-roster--push-invalid-carries-raw
               daemon "missing-current-marker")))))

(ert-deftest agent-repl-itest-roster-row-missing-closed-is-refused ()
  "A row with no `closed' field at all is a contract breach: ERROR, dropped."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest-roster--with-subscription daemon
      ;; Act.
      (agent-repl-itest-roster--push
       daemon (agent-repl-itest-roster--roster
               (list (agent-repl-itest-roster--row-missing
                      "missing-closed-marker" "missing-closed-marker" 'ready 'closed))))
      ;; Assert.
      (agent-repl-itest--await-log daemon "elisp.rpc.push-invalid" "error")
      (should (agent-repl-itest--logged-p daemon "elisp.rpc.push-invalid" "error"))
      (should (agent-repl-itest-roster--push-invalid-carries-raw
               daemon "missing-closed-marker")))))

(ert-deftest agent-repl-itest-roster-roster-missing-repository-is-refused ()
  "A `WorkspaceRoster' with no `repository' field at all: ERROR, dropped.
Fanout §0: a message missing a non-optional field is a contract breach,
never a silently-empty view."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest-roster--with-subscription daemon
      ;; Act.
      (agent-repl-itest-roster--push
       daemon (agent-repl-itest-roster--roster-missing
               (list (agent-repl-itest-roster--row
                      "ws-no-repository-marker" "ws-no-repository-marker" 'ready))
               'repository))
      ;; Assert.
      (agent-repl-itest--await-log daemon "elisp.rpc.push-invalid" "error")
      (should (agent-repl-itest--logged-p daemon "elisp.rpc.push-invalid" "error"))
      ;; The raw-context needle is the recently-merged header, NOT the row's
      ;; marker: the rows live inside `repository', so deleting that field
      ;; deletes them with it and no row text is on the wire at all.  What is
      ;; left of the roster is what the raw context must still carry.
      (should (agent-repl-itest-roster--push-invalid-carries-raw
               daemon "recently merged")))))

(ert-deftest agent-repl-itest-roster-roster-missing-recently-merged-is-refused ()
  "A `WorkspaceRoster' with no `recentlyMerged' field at all: ERROR, dropped."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest-roster--with-subscription daemon
      ;; Act.
      (agent-repl-itest-roster--push
       daemon (agent-repl-itest-roster--roster-missing
               (list (agent-repl-itest-roster--row
                      "ws-no-merged-marker" "ws-no-merged-marker" 'ready))
               'recentlyMerged))
      ;; Assert.
      (agent-repl-itest--await-log daemon "elisp.rpc.push-invalid" "error")
      (should (agent-repl-itest--logged-p daemon "elisp.rpc.push-invalid" "error"))
      (should (agent-repl-itest-roster--push-invalid-carries-raw
               daemon "ws-no-merged-marker")))))

;;;; ---- Scenario 11: the finish edge ----

(ert-deftest agent-repl-itest-roster-running-to-settled-fires-the-finish-edge ()
  "thinking → done is THE FINISH EDGE, and it fires once.
All three Emacs-local reactions ride this transition: the unfocused
banner, the cross-workspace echo and the magit refresh."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest-roster--with-subscription daemon
      (let ((finished nil))
        (agent-repl--ws-put "itest-fin" :project-dir (agent-repl-itest--fixture-dir "itest-fin"))
        (add-hook 'agent-repl-roster-finish-functions
                  (lambda (ws) (push ws finished)))
        (agent-repl-itest-roster--push
         daemon (agent-repl-itest-roster--roster
                 (list (agent-repl-itest-roster--row "itest-fin" "itest-fin" 'thinking))))
        (agent-repl-itest--wait-until
         (lambda () (eq (agent-repl-status-tab-state "itest-fin") :thinking))
         nil "the running state")
        ;; Act.
        (agent-repl-itest-roster--push
         daemon (agent-repl-itest-roster--roster
                 (list (agent-repl-itest-roster--row "itest-fin" "itest-fin" 'done))))
        ;; Assert.
        (agent-repl-itest--wait-until (lambda () finished) nil "the finish edge")
        (should (equal finished (list "itest-fin")))))))

(ert-deftest agent-repl-itest-roster-finish-edge-fires-once-per-transition ()
  "A repeated settled push does NOT re-fire the finish edge.
The edge is the TRANSITION, not the state: pushing the same view again is
a no-change push and must produce no reaction."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest-roster--with-subscription daemon
      (let ((count 0))
        (agent-repl--ws-put "itest-fin1" :project-dir (agent-repl-itest--fixture-dir "itest-fin1"))
        (add-hook 'agent-repl-roster-finish-functions
                  (lambda (_ws) (setq count (1+ count))))
        (agent-repl-itest-roster--push
         daemon (agent-repl-itest-roster--roster
                 (list (agent-repl-itest-roster--row "itest-fin1" "itest-fin1" 'thinking))))
        (agent-repl-itest--wait-until
         (lambda () (eq (agent-repl-status-tab-state "itest-fin1") :thinking))
         nil "the running state")
        (agent-repl-itest-roster--push
         daemon (agent-repl-itest-roster--roster
                 (list (agent-repl-itest-roster--row "itest-fin1" "itest-fin1" 'ready))))
        (agent-repl-itest--wait-until (lambda () (> count 0)) nil "the finish edge")
        ;; Act.
        (agent-repl-itest-roster--push
         daemon (agent-repl-itest-roster--roster
                 (list (agent-repl-itest-roster--row "itest-fin1" "itest-fin1" 'ready))))
        (agent-repl-itest-roster--await-view daemon)
        ;; Assert.
        (should (equal count 1))))))

(ert-deftest agent-repl-itest-roster-running-to-running-does-not-fire ()
  "permission → thinking is running-to-running: no finish edge.
RUNNING is {submitting thinking clearing compacting permission}; a move
inside that set is not a finish."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest-roster--with-subscription daemon
      (let ((count 0))
        (agent-repl--ws-put "itest-fin2" :project-dir (agent-repl-itest--fixture-dir "itest-fin2"))
        (add-hook 'agent-repl-roster-finish-functions
                  (lambda (_ws) (setq count (1+ count))))
        (agent-repl-itest-roster--push
         daemon (agent-repl-itest-roster--roster
                 (list (agent-repl-itest-roster--row
                        "itest-fin2" "itest-fin2" 'permission))))
        (agent-repl-itest--wait-until
         (lambda () (eq (agent-repl-status-tab-state "itest-fin2") :permission))
         nil "the permission state")
        ;; Act.
        (agent-repl-itest-roster--push
         daemon (agent-repl-itest-roster--roster
                 (list (agent-repl-itest-roster--row
                        "itest-fin2" "itest-fin2" 'thinking))))
        (agent-repl-itest--wait-until
         (lambda () (eq (agent-repl-status-tab-state "itest-fin2") :thinking))
         nil "the thinking state")
        ;; Assert.
        (should (equal count 0))))))

(ert-deftest agent-repl-itest-roster-idle-async-is-a-settled-state ()
  "thinking → idle_async IS a finish: the turn ended, work continues.
SETTLED is {ready done interrupted idle-async} — detached work running is
not the turn running."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest-roster--with-subscription daemon
      (let ((finished nil))
        (agent-repl--ws-put "itest-fin3" :project-dir (agent-repl-itest--fixture-dir "itest-fin3"))
        (add-hook 'agent-repl-roster-finish-functions
                  (lambda (ws) (push ws finished)))
        (agent-repl-itest-roster--push
         daemon (agent-repl-itest-roster--roster
                 (list (agent-repl-itest-roster--row "itest-fin3" "itest-fin3" 'thinking))))
        (agent-repl-itest--wait-until
         (lambda () (eq (agent-repl-status-tab-state "itest-fin3") :thinking))
         nil "the running state")
        ;; Act.
        (agent-repl-itest-roster--push
         daemon (agent-repl-itest-roster--roster
                 (list (agent-repl-itest-roster--row
                        "itest-fin3" "itest-fin3" 'idleAsync))))
        ;; Assert.
        (agent-repl-itest--wait-until (lambda () finished) nil "the finish edge")
        (should (equal finished (list "itest-fin3")))))))

(ert-deftest agent-repl-itest-roster-finish-edge-fires-for-every-running-source-and-the-interrupted-target ()
  "Every RUNNING source finishes into a SETTLED target, table-driven.
Fanout §8: RUNNING = {submitting thinking clearing compacting permission},
SETTLED = {ready done interrupted idle-async}; thinking->done/ready/
idle_async are pinned elsewhere, so this covers the remaining sources
\(submitting, clearing, compacting, permission\) and the `interrupted'
target."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest-roster--with-subscription daemon
      (dolist (case '((submitting . done) (clearing . done) (compacting . done)
                      (permission . done) (thinking . interrupted)))
        (let* ((source (car case))
               (target (cdr case))
               (ws (format "itest-fe-%s-%s" source target))
               (finished nil))
          (agent-repl--ws-put ws :project-dir (agent-repl-itest--fixture-dir ws))
          (add-hook 'agent-repl-roster-finish-functions
                    (let ((ws ws)) (lambda (w) (when (equal w ws) (push w finished)))))
          (agent-repl-itest-roster--push
           daemon (agent-repl-itest-roster--roster
                   (list (agent-repl-itest-roster--row ws ws source))))
          (agent-repl-itest--wait-until
           (lambda () (agent-repl-status-tab-state ws)) nil
           (format "the %s running state" source))
          ;; Act.
          (agent-repl-itest-roster--push
           daemon (agent-repl-itest-roster--roster
                   (list (agent-repl-itest-roster--row ws ws target))))
          ;; Assert.
          (agent-repl-itest--wait-until
           (lambda () finished) nil (format "the %s -> %s finish edge" source target))
          (should (equal finished (list ws))))))))

(ert-deftest agent-repl-itest-roster-settled-to-settled-does-not-fire ()
  "ready -> done is settled-to-settled: no finish edge fires.
Fanout §8 (\"once per edge\"): only a RUNNING -> SETTLED move is a finish;
a move within SETTLED never is."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest-roster--with-subscription daemon
      (let ((count 0))
        (agent-repl--ws-put "itest-settled-settled" :project-dir (agent-repl-itest--fixture-dir "itest-settled-settled"))
        (add-hook 'agent-repl-roster-finish-functions
                  (lambda (_ws) (setq count (1+ count))))
        (agent-repl-itest-roster--push
         daemon (agent-repl-itest-roster--roster
                 (list (agent-repl-itest-roster--row
                        "itest-settled-settled" "itest-settled-settled" 'ready))))
        (agent-repl-itest--wait-until
         (lambda () (eq (agent-repl-status-tab-state "itest-settled-settled") :ready))
         nil "the ready state")
        ;; Act.
        (agent-repl-itest-roster--push
         daemon (agent-repl-itest-roster--roster
                 (list (agent-repl-itest-roster--row
                        "itest-settled-settled" "itest-settled-settled" 'done))))
        (agent-repl-itest--wait-until
         (lambda () (eq (agent-repl-status-tab-state "itest-settled-settled") :done))
         nil "the done state")
        ;; Assert.
        (should (equal count 0))))))

(ert-deftest agent-repl-itest-roster-first-sighting-already-settled-does-not-fire ()
  "A row first pushed already SETTLED fires no finish edge.
Fanout §8 (\"once per edge\"): a first sighting (no previous status) is
not a transition, whatever the arriving state is."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest-roster--with-subscription daemon
      (let ((finished nil))
        (agent-repl--ws-put "itest-first-settled" :project-dir (agent-repl-itest--fixture-dir "itest-first-settled"))
        (add-hook 'agent-repl-roster-finish-functions
                  (lambda (ws) (push ws finished)))
        ;; Act: the very first push already carries a SETTLED status.
        (agent-repl-itest-roster--push
         daemon (agent-repl-itest-roster--roster
                 (list (agent-repl-itest-roster--row
                        "itest-first-settled" "itest-first-settled" 'ready))))
        (agent-repl-itest--wait-until
         (lambda () (eq (agent-repl-status-tab-state "itest-first-settled") :ready))
         nil "the ready state")
        (agent-repl-itest-roster--await-view daemon)
        ;; Assert.
        (should (null finished))))))

(ert-deftest agent-repl-itest-roster-finish-edge-echoes-a-message-when-not-selected ()
  "Reaction (2): the cross-workspace echo message fires when WS is not selected.
Fanout §8: \"cross-workspace echo `message' when WS is not the selected
tab\" — `agent-repl-roster-echo-finished' is a no-op for the selected
tab; if the guard or the text broke, an unselected workspace would finish
silently."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest-roster--with-subscription daemon
      ;; The fixture blanks the finish hook; this scenario's subject IS the
      ;; production consumer's reaction, so it puts that one consumer back.
      (let ((agent-repl-roster-finish-functions
             (list #'agent-repl-roster-echo-finished)))
      (agent-repl--ws-put "itest-fin-echo" :project-dir (agent-repl-itest--fixture-dir "itest-fin-echo"))
      (agent-repl-itest-roster--push
       daemon (agent-repl-itest-roster--roster
               (list (agent-repl-itest-roster--row "itest-fin-echo" "itest-fin-echo" 'thinking))))
      (agent-repl-itest--wait-until
       (lambda () (eq (agent-repl-status-tab-state "itest-fin-echo") :thinking))
       nil "the running state")
      ;; THE ECHO IS OBSERVED AT THE `message' CALL, not in the echo area:
      ;; `current-message' is unconditionally nil under `-batch', where there
      ;; is no echo area to hold one, so reading it back could never see this
      ;; reaction however well it worked.
      (let ((echoed nil))
        (cl-letf* ((real (symbol-function 'message))
                   ((symbol-function 'message)
                    (lambda (format-string &rest args)
                      (let ((text (and format-string (apply #'format format-string args))))
                        (when text (push text echoed))
                        (apply real format-string args)))))
          ;; Act.
          (agent-repl-itest-roster--push
           daemon (agent-repl-itest-roster--roster
                   (list (agent-repl-itest-roster--row "itest-fin-echo" "itest-fin-echo" 'done))))
          ;; Assert.
          (agent-repl-itest--wait-until
           (lambda () (member "Agent finished in workspace: itest-fin-echo" echoed))
           nil "the cross-workspace echo message")
          (should (member "Agent finished in workspace: itest-fin-echo" echoed))))))))

(ert-deftest agent-repl-itest-roster-finish-edge-refreshes-magit-for-the-workspaces-dir ()
  "Reaction (3): the finish edge refreshes magit-status for WS's directory.
Fanout §8: \"magit-status refresh for the dir\" —
`agent-repl-roster-refresh-magit' must pass the WORKSPACE'S dir, not a
stale or global one."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest-roster--with-subscription daemon
      ;; The fixture blanks the finish hook; the magit refresh IS this
      ;; scenario's subject, so the production consumer goes back on alone.
      (let ((refreshed nil)
            (agent-repl-roster-finish-functions
             (list #'agent-repl-roster-refresh-magit)))
        (cl-letf (((symbol-function 'agent-repl--refresh-magit-status-for-dir)
                   (lambda (dir &optional _ws) (push dir refreshed))))
          (agent-repl-itest-roster--push
           daemon (agent-repl-itest-roster--roster
                   (list (agent-repl-itest-roster--row
                          "itest-fin-magit" "itest-fin-magit" 'thinking))))
          (agent-repl-itest--wait-until
           (lambda () (eq (agent-repl-status-tab-state "itest-fin-magit") :thinking))
           nil "the running state")
          ;; Act.
          (agent-repl-itest-roster--push
           daemon (agent-repl-itest-roster--roster
                   (list (agent-repl-itest-roster--row
                          "itest-fin-magit" "itest-fin-magit" 'done))))
          ;; Assert.
          (agent-repl-itest--wait-until (lambda () refreshed) nil "the magit refresh")
          (should (member (agent-repl-itest--fixture-dir "roster-itest-fin-magit") refreshed)))))))

;;;; ---- Paint: badges and glyphs through the render path ----

(ert-deftest agent-repl-itest-roster-priority-badge-draws-before-the-name ()
  "A row's priority badge label draws BEFORE the name in the tab-bar badge run.
Fanout §8: \"Priority badge label draws before the name.\"
`RosterRowPriorityBadge.label' is a plain wire string, not a nested text
message — the fixture must set it that way."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest-roster--with-subscription daemon
      ;; Act.
      (agent-repl-itest-roster--push
       daemon (agent-repl-itest-roster--roster
               (list (agent-repl-itest-roster--row
                      "itest-badge" "itest-badge" 'ready
                      '(priority . ((label . "P1")))))))
      (agent-repl-itest--wait-until
       (lambda ()
         (and (agent-repl--ws-known-p "itest-badge")
              (eq (agent-repl-status-tab-state "itest-badge") :ready)))
       nil "the row to resolve")
      ;; Assert.
      (should (string-prefix-p
               "P1" (agent-repl--tab-badge-str "itest-badge" (agent-repl-status-tab-state
                                                              "itest-badge")))))))

(ert-deftest agent-repl-itest-roster-inactive-arm-draws-the-question-mark-glyph ()
  "The `inactive' arm draws \"?\", read through `agent-repl-status-tab-state'.
Fanout §8: \"inactive → none with a '?' glyph\" — inactive rows still need
SOME visual marker even though they take no lifecycle color."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest-roster--with-subscription daemon
      ;; Act.
      (agent-repl-itest-roster--push
       daemon (agent-repl-itest-roster--roster
               (list (agent-repl-itest-roster--row
                      "itest-glyph-inactive" "itest-glyph-inactive" 'inactive))))
      (agent-repl-itest--wait-until
       (lambda ()
         (and (agent-repl--ws-known-p "itest-glyph-inactive")
              (eq (agent-repl-status-tab-state "itest-glyph-inactive") :inactive)))
       nil "the inactive state")
      ;; Assert.
      (should (equal (agent-repl-status-tab-glyph
                      "itest-glyph-inactive"
                      (agent-repl-status-tab-state "itest-glyph-inactive"))
                     "?")))))

(ert-deftest agent-repl-itest-roster-merge-failed-arm-draws-its-glyph ()
  "The `merge_failed' arm draws its ✗ glyph through the render path.
A failed merge is turquoise AND glyphed, so the tab says both that
something went wrong and that it was the merge."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest-roster--with-subscription daemon
      ;; Act.
      (agent-repl-itest-roster--push
       daemon (agent-repl-itest-roster--roster
               (list (agent-repl-itest-roster--row
                      "itest-glyph-failed" "itest-glyph-failed" 'mergeFailed))))
      (agent-repl-itest--wait-until
       (lambda ()
         (and (agent-repl--ws-known-p "itest-glyph-failed")
              (eq (agent-repl-status-tab-state "itest-glyph-failed") :merge-failed)))
       nil "the merge-failed state")
      ;; Assert.
      (should (equal (agent-repl-status-tab-glyph
                      "itest-glyph-failed"
                      (agent-repl-status-tab-state "itest-glyph-failed"))
                     "✗")))))

(ert-deftest agent-repl-itest-roster-logs-the-roster-stream-open ()
  "The roster subscription is logged through the canonical ladder."
  ;; Arrange / Act.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest-roster--with-subscription daemon
      ;; Assert.
      (agent-repl-itest--await-log daemon "elisp.rpc.stream-open" "info")
      (should (agent-repl-itest--logged-p daemon "elisp.rpc.stream-open" "info")))))

;;;; ---- Audit-2 additions (R-SUITE-2) ----
;;
;; Findings 17-23 of docs/overhaul/reports/elisp-suite-audit-2.md.  Kept in
;; their own section so they merge cleanly beside concurrent edits above.

(declare-function agent-repl-wire-decode-watch-workspace-roster-response "wire-roster")
(declare-function agent-repl--emacs-focused-p "notifications")
(declare-function agent-repl--ws-get "workspace")

;; audit-2 #17
(ert-deftest agent-repl-itest-roster-row-that-left-the-roster-is-torn-down ()
  "A row that VANISHES from the roster loses its tab.
E5: \"nuked rows leave the roster\" — they are not marked `closed', they
are simply gone.  R8: \"Tabs derive from `closed = false' rows\", and
roster.el's reconcile \"tears down every roster-owned tab whose row is
gone\".  A client that only reacted to `closed' would keep a tab for a
workspace that no longer exists anywhere."
  ;; Arrange: two rows, both open.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest-roster--with-subscription daemon
      (agent-repl-itest-roster--push
       daemon (agent-repl-itest-roster--roster
               (list (agent-repl-itest-roster--row "itest-stay" "itest-stay" 'ready)
                     (agent-repl-itest-roster--row "itest-gone" "itest-gone" 'ready))))
      (agent-repl-itest--wait-until
       (lambda () (and (agent-repl--ws-by-ref-id "itest-stay")
                       (agent-repl--ws-by-ref-id "itest-gone")))
       nil "both rows' tabs to be opened")
      ;; Act: the second row is nuked — it leaves the roster entirely.
      (agent-repl-itest-roster--push
       daemon (agent-repl-itest-roster--roster
               (list (agent-repl-itest-roster--row "itest-stay" "itest-stay" 'ready))))
      ;; Assert.
      (agent-repl-itest--wait-until
       (lambda () (not (agent-repl--ws-by-ref-id "itest-gone")))
       nil "the vanished row's tab to be torn down")
      (should (null (agent-repl--ws-by-ref-id "itest-gone")))
      (should (equal (agent-repl--ws-by-ref-id "itest-stay") "itest-stay")))))

;; audit-2 #18
(ert-deftest agent-repl-itest-roster-tab-carries-the-rows-own-ref ()
  "A tab is created with the ROW'S ref: its `:dir' is the ref's dir.
fanout §8: \"workspace.el creates it with `:ref', `:dir' = ref.dir,
`:name'\".  A path is never an identity, and a tab whose dir was derived
anywhere but from the ref would send every later per-workspace rpc at the
wrong directory.  `:project-dir' is asserted beside `:dir' because it is
the identity key the rest of the system reads -- the durable log sink,
history, the composer's attachment root, panels and magit all key on it.

The ref is read off the WORKSPACE, which is the seam §8 gives tab creation
\(\"workspace.el creates it with `:ref', `:dir' = ref.dir, `:name'\").
host.el's own registry is not it: this fixture opens a bare conn and never
makes it the link primary, so `agent-repl-roster--subscribe-host' correctly
skips, and host.el is handed no ref to hold."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest-roster--with-subscription daemon
      ;; Act.
      (agent-repl-itest-roster--push
       daemon (agent-repl-itest-roster--roster
               (list (agent-repl-itest-roster--row "itest-ref" "itest-ref" 'ready))))
      ;; Assert.
      (agent-repl-itest--wait-until
       (lambda () (agent-repl--ws-by-ref-id "itest-ref"))
       nil "the row's tab to be opened")
      (let ((ws (agent-repl--ws-by-ref-id "itest-ref")))
        (should (equal (agent-repl--ws-get ws :project-dir) (agent-repl-itest--fixture-dir "roster-itest-ref")))
        (should (equal (agent-repl--ws-get ws :dir) (agent-repl-itest--fixture-dir "roster-itest-ref")))
        (should (equal (plist-get (agent-repl--ws-get ws :ref) :id) "itest-ref"))
        (should (equal (plist-get (agent-repl--ws-get ws :ref) :dir)
                       (agent-repl-itest--fixture-dir "roster-itest-ref")))))))

;; audit-2 #19
(ert-deftest agent-repl-itest-roster-finish-reactions-are-globally-registered ()
  "The two finish-edge reactions are registered at LOAD time, globally.
Every other finish-edge test binds `agent-repl-roster-finish-functions'
to exactly the consumer it exercises, so a production that dropped its
`add-hook' would pass all of them.  The GLOBAL value is the only place
the wiring itself is observable.  A deferred prompt is NOT a third: the
daemon holds it and runs it as its own turn (owner ruling, 2026-09-28),
so nothing on this edge releases one (test-prompt-queue.el pins that)."
  ;; Arrange / Act / Assert.
  (let ((registered (default-value 'agent-repl-roster-finish-functions)))
    (should-not (memq 'agent-repl-roster-notify-finished registered))
    (should (memq #'agent-repl-roster-echo-finished registered))
    (should (memq #'agent-repl-roster-refresh-magit registered))))

;; audit-2 #19
(ert-deftest agent-repl-itest-roster-attention-sync-is-globally-registered ()
  "`agent-repl-status-sync-attention' is registered on the update hook.
The attention marker is redrawn from every roster push; the reaction is
installed once at load time (status.el), and every attention test in this
suite re-installs it, so nothing else would notice its loss."
  ;; Arrange / Act / Assert.
  (should (memq #'agent-repl-status-sync-attention
                (default-value 'agent-repl-roster-update-functions))))

;; audit-2 #20
(ert-deftest agent-repl-itest-roster-closed-teardown-is-idempotent ()
  "Tearing a `closed' row down twice tears it down ONCE.
fanout §8: \"`closed' true → ensure no tab (teardown is idempotent)\".
The daemon re-pushes the whole roster on every change, so a closed row is
seen again and again — a teardown that ran per sighting would log and act
repeatedly against a tab that is already gone."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest-roster--with-subscription daemon
      (agent-repl-itest-roster--push
       daemon (agent-repl-itest-roster--roster
               (list (agent-repl-itest-roster--row "itest-idem" "itest-idem" 'ready))))
      (agent-repl-itest--wait-until
       (lambda () (agent-repl--ws-by-ref-id "itest-idem"))
       nil "the open row's tab")
      (agent-repl-itest-roster--push
       daemon (agent-repl-itest-roster--roster
               (list (agent-repl-itest-roster--row
                      "itest-idem" "itest-idem" 'merged
                      '(closed . ((closed . t)))))))
      (agent-repl-itest--await-log daemon "elisp.roster.tab-teardown")
      ;; Act: the same closed row is pushed again, plus a fresh open row so
      ;; the second push's application is observable.
      (agent-repl-itest-roster--push
       daemon (agent-repl-itest-roster--roster
               (list (agent-repl-itest-roster--row
                      "itest-idem" "itest-idem" 'merged
                      '(closed . ((closed . t))))
                     (agent-repl-itest-roster--row
                      "itest-idem-2" "itest-idem-2" 'ready))))
      (agent-repl-itest--wait-until
       (lambda () (agent-repl--ws-by-ref-id "itest-idem-2"))
       nil "the second push to be applied")
      ;; Assert: one teardown in total, and no error anywhere.
      (should (equal 1 (length (agent-repl-itest--log-entries
                                daemon "elisp.roster.tab-teardown"))))
      (should (null (agent-repl-itest--log-entries daemon "elisp.roster.push" "error"))))))

;; audit-2 #21
(ert-deftest agent-repl-itest-roster-current-naming-a-closed-row-does-not-switch ()
  "A `current' naming a row that is `closed' switches nothing.
frontend/v1/sidebar.proto: `current' is \"the last SelectWorkspace the
daemon received\", which can LAG a close.  There is no tab to switch to —
switching would call `agent-repl--ws-switch' on a workspace that was just
torn down — and a lagging pointer is not a contract breach either, so
nothing is dropped as invalid."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest-roster--with-subscription daemon
      (let ((switched nil)
            (agent-repl-host-last-selected-id nil))
        (cl-letf (((symbol-function 'agent-repl--ws-switch)
                   (lambda (ws &rest _) (push ws switched))))
          ;; Act.
          (agent-repl-itest-roster--push
           daemon (agent-repl-itest-roster--roster
                   (list (agent-repl-itest-roster--row
                          "itest-lag" "itest-lag" 'merged
                          '(closed . ((closed . t)))
                          '(current . ((current . t))))
                         (agent-repl-itest-roster--row
                          "itest-live" "itest-live" 'ready))
                   `(current . ((workspace . ((id . "itest-lag")
                                              (dir . ,(agent-repl-itest--fixture-dir "itest-roster-itest-lag"))))))))
          (agent-repl-itest--wait-until
           (lambda () (agent-repl--ws-by-ref-id "itest-live"))
           nil "the push to be applied")
          ;; Assert.
          (should (null switched))
          (should (null (agent-repl-itest--log-entries
                         daemon "elisp.rpc.push-invalid" "error"))))))))

;; audit-2 #21
(ert-deftest agent-repl-itest-roster-current-naming-an-absent-row-does-not-switch ()
  "A `current' naming a row that is not on the roster at all switches nothing.
A nuked workspace leaves the roster while the daemon's `current' pointer
still names it; the same lag, reached by the other way a row disappears."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest-roster--with-subscription daemon
      (let ((switched nil)
            (agent-repl-host-last-selected-id nil))
        (cl-letf (((symbol-function 'agent-repl--ws-switch)
                   (lambda (ws &rest _) (push ws switched))))
          ;; Act.
          (agent-repl-itest-roster--push
           daemon (agent-repl-itest-roster--roster
                   (list (agent-repl-itest-roster--row
                          "itest-present" "itest-present" 'ready))
                   `(current . ((workspace . ((id . "itest-vanished")
                                              (dir . ,(agent-repl-itest--fixture-dir "itest-roster-itest-vanished"))))))))
          (agent-repl-itest--wait-until
           (lambda () (agent-repl--ws-by-ref-id "itest-present"))
           nil "the push to be applied")
          ;; Assert.
          (should (null switched))
          (should (null (agent-repl-itest--log-entries
                         daemon "elisp.rpc.push-invalid" "error"))))))))

;; audit-2 #22
(ert-deftest agent-repl-itest-roster-two-status-arms-set-is-a-breach ()
  "A row whose status oneof has TWO arms set is a contract breach.
fanout §14 scenario 9 lists it explicitly; only the UNSET and the UNKNOWN
arm are covered above.  protojson cannot put two arms of one oneof on the
wire, so the fake refuses it before the transport and the DECODER is
pinned directly."
  ;; Arrange / Act / Assert.
  (should-error
   (agent-repl-wire-decode-watch-workspace-roster-response
    `((roster
       . ((repository
           . ((sections
               . [((key . ((repository . ((id . "repo-itest")
                                          (dir . ,agent-repl-itest-roster--repo-dir)))))
                   (header . ((label . ((text . "itest-repo"))) (count . ((workspaces . 1)))))
                   (rows
                    . ((rows
                        . [((workspace . ((workspace . ((id . "ws-x")
                                                        (dir . ,(agent-repl-itest--fixture-dir "ws-x"))))))
                            (name . ((text . "ws-x")))
                            (ready . nil)
                            (thinking . nil)
                            (current . ((current . :false)))
                            (when . nil)
                            (detail . nil)
                            (closed . ((closed . :false))))]))))])))
          (task . ((sections . [])))
          (recentlyMerged . ((header . ((label . ((text . "recently merged"))) (count . ((workspaces . 1)))))
                             (rows . ((rows . [])))))))))
   :type 'agent-repl-wire-error))

(defun agent-repl-itest-roster--host-live (composer)
  "Return a minimal valid live `HostWorkspace' alist with COMPOSER's arm set.
Every non-optional field of the live arm is populated, mirroring
`test-integration-host.el''s own fixture, so a push through this exercises
the composer gate alone."
  `((existing . ((id . ((value . "host-session-1")))
                 (live . ((generation . ((value . "gen-1")))
                          (shimAttached . t)
                          (claude . ((sessionId . "vendor-1")
                                     (configDir . "/home/itest/.claude")))
                          (backfill . ((done . ())))
                          (,composer . ())))))
    (naming . ())))

(defun agent-repl-itest-roster--push-snapshot (daemon roster)
  "Push ROSTER on DAEMON's roster stream as a SNAPSHOT.
Stored and replayed to the next subscriber, which is how a reconnect's
re-pull is staged without driving the real link/reconnect machinery that
test-integration-link.el owns."
  (agent-repl-itest--push daemon "roster" `((roster . ,roster)) nil t))

;; audit-3 #33
(ert-deftest agent-repl-itest-roster-rename-re-keys-the-host-entry ()
  "RULING: a renamed tab's host entry moves to the NEW name, never stays keyed to OLD.
WRONG today (roster.el:312): `agent-repl-roster--rename-tab' calls
`agent-repl--ws-rename-state' and `--ws-rename-persp' but never re-keys
`agent-repl-host--by-name'.  EXPECTED RED until that production fix lands
— fanout §8 \"a rename of the row renames the tab\" + §7 host state keyed
by WS-NAME + §9 REF via `agent-repl-host-ref' (nil -> `user-error' at every
verb and the composer)."
  ;; Arrange: a primary link, so the open tab starts a REAL host subscription.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest-roster--with-subscription daemon
      (let ((agent-repl-link--primary conn))
        (agent-repl-itest-roster--push
         daemon (agent-repl-itest-roster--roster
                 (list (agent-repl-itest-roster--row "itest-rekey" "old" 'ready))))
        (agent-repl-itest--wait-until
         (lambda () (agent-repl-host-ref "old")) nil "the old tab's host ref to attach")
        (let ((ref (agent-repl-host-ref "old")))
          ;; Act.
          (agent-repl-itest-roster--push
           daemon (agent-repl-itest-roster--roster
                   (list (agent-repl-itest-roster--row "itest-rekey" "new" 'ready))))
          (agent-repl-itest--wait-until
           (lambda () (equal (agent-repl--ws-by-ref-id "itest-rekey") "new"))
           nil "the renamed row's tab")
          ;; Assert: the ref moved to the new name and the old key is gone.
          (should (equal (agent-repl-host-ref "new") ref))
          (should (null (agent-repl-host-ref "old")))
          (should (not (member "old" (agent-repl--live-ws-names))))
          (should (equal (agent-repl-roster-tab-order) '("new")))
          ;; A following host push for the SAME id must update the NEW
          ;; name's composer gate — not a name nothing reads any more.
          (agent-repl-itest--push
           daemon "host" `((host . ,(agent-repl-itest-roster--host-live 'open)))
           "itest-rekey")
          (agent-repl-itest--wait-until
           (lambda () (eq (agent-repl-host-composer-gate "new") :open))
           nil "the renamed tab's composer gate to update")
          (should (eq (agent-repl-host-composer-gate "new") :open)))))))

;; audit-3 #33
(ert-deftest agent-repl-itest-roster-rename-colliding-with-a-tombstoned-name-is-refused-loudly-not-half-applied ()
  "RULING: a rename target colliding with a TOMBSTONED name is refused loudly.
`--ws-rename-state' signals `user-error' out of the push handler
(workspace.el:225) when the target name is already registered, live or
tombstoned — the push must be refused whole, never leave the old tab
renamed halfway into some third state.  (This precondition sits ahead of
the host re-key bug above and already holds independently of it.)"
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest-roster--with-subscription daemon
      ;; Arrange: two open rows, then tombstone one by closing it.
      (agent-repl-itest-roster--push
       daemon (agent-repl-itest-roster--roster
               (list (agent-repl-itest-roster--row "itest-collide-live" "live" 'ready)
                     (agent-repl-itest-roster--row "itest-collide-dead" "dead" 'ready))))
      (agent-repl-itest--wait-until
       (lambda () (and (agent-repl--ws-by-ref-id "itest-collide-live")
                       (agent-repl--ws-by-ref-id "itest-collide-dead")))
       nil "both rows' tabs")
      (agent-repl-itest-roster--push
       daemon (agent-repl-itest-roster--roster
               (list (agent-repl-itest-roster--row "itest-collide-live" "live" 'ready)
                     (agent-repl-itest-roster--row
                      "itest-collide-dead" "dead" 'merged '(closed . ((closed . t)))))))
      (agent-repl-itest--wait-until
       (lambda () (not (agent-repl--ws-by-ref-id "itest-collide-dead")))
       nil "the dead row's tab to be torn down")
      ;; Act: rename the live row onto the now-tombstoned name.
      (agent-repl-itest-roster--push
       daemon (agent-repl-itest-roster--roster
               (list (agent-repl-itest-roster--row "itest-collide-live" "dead" 'ready))))
      ;; Assert: refused loudly, and the old tab untouched — never half-renamed.
      ;; The refusal is recorded by `agent-repl-roster--rename-state', which
      ;; catches `--ws-rename-state''s `user-error' so it cannot escape the
      ;; push handler and abort the reconcile walk mid-list: the ruled shape
      ;; (fanout §0c, R-AUDIT3-PROD) is a WHOLE refusal logged at ERROR, not
      ;; an invalid push.  A `push-invalid' here would mean the signal
      ;; escaped and the tab bar describes a roster nobody finished reading.
      (agent-repl-itest--await-log daemon "elisp.roster.tab-rename-refused" "error")
      (should (agent-repl-itest--logged-p daemon "elisp.roster.tab-rename-refused" "error"))
      (should-not (agent-repl-itest--logged-p daemon "elisp.rpc.push-invalid" "error"))
      (should (equal (agent-repl--ws-by-ref-id "itest-collide-live") "live")))))

;; audit-3 #34
(ert-deftest agent-repl-itest-roster-when-active-at-ms-decodes-from-a-decimal-string ()
  "`when.active.atMs' rides the wire as a STRING and still decodes.
fanout §2: Go emits int64 as a decimal string; a decoder mis-typing
`atMs' as a bare number would refuse every real push while a fixture
that always sends unset `when' stays green."
  ;; Arrange / Act.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest-roster--with-subscription daemon
      (agent-repl-itest-roster--push
       daemon (agent-repl-itest-roster--roster
               (list (agent-repl-itest-roster--row
                      "itest-when-ac" "itest-when-ac" 'ready
                      '(when . ((active . ((atMs . "1700000000000")))))))))
      (agent-repl-itest--wait-until
       (lambda () (agent-repl-roster-row-for-ws "itest-when-ac")) nil "the row to be indexed")
      ;; Assert.
      (should-not (agent-repl-itest--logged-p daemon "elisp.rpc.push-invalid" "error"))
      (let ((when-value (plist-get (agent-repl-roster-row-for-ws "itest-when-ac") :when)))
        (should (eq (plist-get when-value :arm) :active))
        (should (= (plist-get (plist-get when-value :value) :at-ms) 1700000000000))))))

;; audit-3 #34
(ert-deftest agent-repl-itest-roster-when-merged-at-ms-decodes-from-a-decimal-string ()
  "`when.merged.atMs' rides the wire as a STRING and still decodes.
The `merged' arm of the same oneof, pinned separately from `active'."
  ;; Arrange / Act.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest-roster--with-subscription daemon
      (agent-repl-itest-roster--push
       daemon (agent-repl-itest-roster--roster
               (list (agent-repl-itest-roster--row
                      "itest-when-mg" "itest-when-mg" 'ready
                      '(when . ((merged . ((atMs . "1700000005000")))))))))
      (agent-repl-itest--wait-until
       (lambda () (agent-repl-roster-row-for-ws "itest-when-mg")) nil "the row to be indexed")
      ;; Assert.
      (should-not (agent-repl-itest--logged-p daemon "elisp.rpc.push-invalid" "error"))
      (let ((when-value (plist-get (agent-repl-roster-row-for-ws "itest-when-mg") :when)))
        (should (eq (plist-get when-value :arm) :merged))
        (should (= (plist-get (plist-get when-value :value) :at-ms) 1700000005000))))))

;; audit-3 #34
(ert-deftest agent-repl-itest-roster-detail-s-three-lines-all-ride-a-push ()
  "All three `detail' lines — branch, parentBranch, summary — ride one push.
fanout §5: \"detail with presence-optional lines\" — every fixture row
above sends an empty `detail', so a decoder mis-typing any one line would
refuse every real push while the suite stayed green."
  ;; Arrange / Act.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest-roster--with-subscription daemon
      (agent-repl-itest-roster--push
       daemon (agent-repl-itest-roster--roster
               (list (agent-repl-itest-roster--row
                      "itest-detail" "itest-detail" 'ready
                      '(detail . ((branch . ((name . "feature-x")))
                                  (parentBranch . ((name . "main")))
                                  (summary . ((text . "running the tests")))))))))
      (agent-repl-itest--wait-until
       (lambda () (agent-repl-roster-row-for-ws "itest-detail")) nil "the row to be indexed")
      ;; Assert.
      (should-not (agent-repl-itest--logged-p daemon "elisp.rpc.push-invalid" "error"))
      (let ((detail (plist-get (agent-repl-roster-row-for-ws "itest-detail") :detail)))
        (should (equal (plist-get (plist-get detail :branch) :name) "feature-x"))
        (should (equal (plist-get (plist-get detail :parent-branch) :name) "main"))
        (should (equal (plist-get (plist-get detail :summary) :text) "running the tests"))))))

;; audit-3 #35
(ert-deftest agent-repl-itest-roster-a-clean-stream-end-is-a-loud-error-the-view-is-kept-and-a-fresh-subscribe-stands ()
  "The roster stream ending CLEANLY (a bare end frame) is a loud ERROR.
fanout §3; roster.el's `elisp.roster.stream-close' ERROR path — audit-2
pinned this for the daemon (#2) and host (#12) streams only, never the
roster's own GLOBAL stream.  The last view is KEPT and a fresh subscribe
stands a fresh subscriber."
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest-roster--with-subscription daemon
      (agent-repl-itest-roster--push
       daemon (agent-repl-itest-roster--roster
               (list (agent-repl-itest-roster--row "itest-end-clean" "itest-end-clean" 'ready))))
      (agent-repl-itest-roster--await-view daemon)
      (let ((kept agent-repl-roster-view))
        ;; Act.
        (agent-repl-itest--end daemon "roster")
        ;; Assert.
        (agent-repl-itest--await-log daemon "elisp.roster.stream-close" "error")
        (should (equal agent-repl-roster-view kept))
        (agent-repl-roster-subscribe conn)
        (agent-repl-itest--await-subscriber daemon "roster")
        (should (equal 1 (length (agent-repl-itest--subscribers daemon "roster"))))))))

(ert-deftest agent-repl-itest-roster-a-clean-end-after-the-planned-ending-is-info ()
  "A clean end after the daemon's planned ending is a stand-down at INFO.
The daemon's last frame (`DaemonStreamEnding') says the end is PLANNED, so
roster.el writes no ERROR and keeps the last view."
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest-roster--with-subscription daemon
      (agent-repl-itest-roster--push
       daemon (agent-repl-itest-roster--roster
               (list (agent-repl-itest-roster--row "itest-end-planned" "itest-end-planned" 'ready))))
      (agent-repl-itest-roster--await-view daemon)
      (let ((kept agent-repl-roster-view))
        ;; Act.
        (agent-repl-itest--push daemon "roster" '((ending . ())))
        (agent-repl-itest--await-log daemon "elisp.roster.stream-ending" "info")
        (agent-repl-itest--end daemon "roster")
        ;; Assert.
        (agent-repl-itest--await-log daemon "elisp.roster.stream-close: reason=planned-ending" "info")
        (should-not (agent-repl-itest--logged-p daemon "elisp.roster.stream-close" "error"))
        (should (equal agent-repl-roster-view kept))))))

;; audit-3 #35
(ert-deftest agent-repl-itest-roster-an-aborted-stream-is-a-loud-error-the-view-is-kept-and-a-fresh-subscribe-stands ()
  "The roster stream ABORTING (no end frame at all) is a loud ERROR too.
The other producer-side end fanout §3 names: a dropped TCP connection with
no terminal envelope, which reaches roster.el's own catch-all branch
rather than the `:ended' one."
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest-roster--with-subscription daemon
      (agent-repl-itest-roster--push
       daemon (agent-repl-itest-roster--roster
               (list (agent-repl-itest-roster--row "itest-end-abort" "itest-end-abort" 'ready))))
      (agent-repl-itest-roster--await-view daemon)
      (let ((kept agent-repl-roster-view))
        ;; Act.
        (agent-repl-itest--end daemon "roster" nil nil t)
        ;; Assert.
        (agent-repl-itest--await-log daemon "elisp.roster.stream-close" "error")
        (should (equal agent-repl-roster-view kept))
        (agent-repl-roster-subscribe conn)
        (agent-repl-itest--await-subscriber daemon "roster")
        (should (equal 1 (length (agent-repl-itest--subscribers daemon "roster"))))))))

;; audit-3 #36
(ert-deftest agent-repl-itest-roster-closed-then-reopened-row-opens-once-and-a-repeat-adds-no-third ()
  "A row cycling open -> closed -> open re-opens exactly ONCE; a repeat adds none.
fanout §8 \"closed false -> ensure a tab exists\"; `--ws-del' tombstones and
`--open-tab' writes through `--ws-put', so a reopen after teardown must
open again rather than silently resurrecting the tombstone."
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest-roster--with-subscription daemon
      ;; Act: open.
      (agent-repl-itest-roster--push
       daemon (agent-repl-itest-roster--roster
               (list (agent-repl-itest-roster--row "itest-reopen" "itest-reopen" 'ready))))
      (agent-repl-itest--wait-until
       (lambda () (agent-repl--ws-by-ref-id "itest-reopen")) nil "the first open")
      ;; Act: close.
      (agent-repl-itest-roster--push
       daemon (agent-repl-itest-roster--roster
               (list (agent-repl-itest-roster--row
                      "itest-reopen" "itest-reopen" 'merged '(closed . ((closed . t)))))))
      (agent-repl-itest--wait-until
       (lambda () (not (agent-repl--ws-by-ref-id "itest-reopen"))) nil "the teardown")
      ;; Act: reopen the SAME id.
      (agent-repl-itest-roster--push
       daemon (agent-repl-itest-roster--roster
               (list (agent-repl-itest-roster--row "itest-reopen" "itest-reopen" 'ready))))
      (agent-repl-itest--wait-until
       (lambda () (agent-repl--ws-by-ref-id "itest-reopen")) nil "the reopen")
      ;; Assert: a LIVE name and exactly two `tab-open' records.
      (should (equal (agent-repl--ws-by-ref-id "itest-reopen") "itest-reopen"))
      (should (equal 2 (length (agent-repl-itest--log-entries daemon "elisp.roster.tab-open"))))
      ;; Act: the identical open push again.
      (agent-repl-itest-roster--push
       daemon (agent-repl-itest-roster--roster
               (list (agent-repl-itest-roster--row "itest-reopen" "itest-reopen" 'ready))))
      (agent-repl-itest-roster--await-view daemon)
      ;; Assert: no third `tab-open'.
      (should (equal 2 (length (agent-repl-itest--log-entries daemon "elisp.roster.tab-open")))))))

;; audit-3 #37
(ert-deftest agent-repl-itest-roster-a-replayed-snapshot-after-a-reconnect-touches-no-tab ()
  "The SAME rows replayed as a SNAPSHOT after a reconnect touch no tab.
elisp.md \"a reconnect re-opens and re-pulls\": a re-pull that changes
nothing must reconcile as `tab-kept' throughout — never a fresh
`tab-open' and never a `tab-teardown' — and the tab order is unchanged."
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest-roster--with-subscription daemon
      (let ((roster (agent-repl-itest-roster--roster
                     (list (agent-repl-itest-roster--row "itest-reconn-a" "itest-reconn-a" 'ready)
                           (agent-repl-itest-roster--row "itest-reconn-b" "itest-reconn-b" 'ready)))))
        (agent-repl-itest-roster--push daemon roster)
        (agent-repl-itest--wait-until
         (lambda () (equal (agent-repl-roster-tab-order) '("itest-reconn-a" "itest-reconn-b")))
         nil "both rows' tabs to open")
        ;; Act: a SECOND connection stages the IDENTICAL roster as a
        ;; SNAPSHOT and subscribes -- exactly what a reconnect's re-pull
        ;; delivers, without driving the real link/reconnect machinery
        ;; test-integration-link.el owns.
        (let ((second (agent-repl-connect-open (agent-repl-itest-daemon-address daemon))))
          (unwind-protect
              (progn
                (agent-repl-itest-roster--push-snapshot daemon roster)
                (agent-repl-roster-subscribe second)
                (agent-repl-itest--await-subscriber daemon "roster" nil 1)
                (agent-repl-itest-roster--await-view daemon)
                ;; Assert.
                (should (equal (agent-repl-roster-tab-order)
                               '("itest-reconn-a" "itest-reconn-b")))
                (should (null (agent-repl-itest--log-entries daemon "elisp.roster.tab-teardown")))
                (should (equal 2 (length (agent-repl-itest--log-entries
                                          daemon "elisp.roster.tab-open")))))
            (agent-repl-connect-close second)))))))

;; audit-3 #38
(ert-deftest agent-repl-itest-roster-duplicate-ref-id-in-one-push-drops-the-whole-push ()
  "Two rows sharing one ref id in a single push are dropped WHOLE.
sidebar.proto RosterRow.workspace: \"its identity\"; roster.el's own
`reason=duplicate-ref-id' ERROR -- the push decodes fine (each row is
individually valid) and is dropped by roster.el's own invariant, not by
the wire decoder."
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest-roster--with-subscription daemon
      (agent-repl-itest-roster--push
       daemon (agent-repl-itest-roster--roster
               (list (agent-repl-itest-roster--row "itest-dup-control" "itest-dup-control" 'ready))))
      (agent-repl-itest-roster--await-view daemon)
      (let ((kept agent-repl-roster-view))
        ;; Act.
        (agent-repl-itest-roster--push
         daemon (agent-repl-itest-roster--roster
                 (list (agent-repl-itest-roster--row "itest-dup-id" "dup-name-1" 'ready)
                       (agent-repl-itest-roster--row "itest-dup-id" "dup-name-2" 'ready))))
        ;; Assert.
        (agent-repl-itest--await-log daemon "elisp.roster.push" "error")
        (should (seq-some
                 (lambda (entry)
                   (string-match-p "duplicate-ref-id" (or (alist-get 'message entry) "")))
                 (agent-repl-itest--log-entries daemon "elisp.roster.push" "error")))
        (should (null (agent-repl--ws-by-ref-id "itest-dup-id")))
        (should (equal agent-repl-roster-view kept))))))

;; audit-3 #39
(ert-deftest agent-repl-itest-roster-roster-missing-task-is-refused ()
  "A `WorkspaceRoster' with no `task' field at all is a contract breach.
sidebar.proto:60 marks `task' non-optional; audit-1 #49 pinned
`repository' and `recentlyMerged' only, leaving this field unguarded."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest-roster--with-subscription daemon
      ;; Act.
      (agent-repl-itest-roster--push
       daemon (agent-repl-itest-roster--roster-missing
               (list (agent-repl-itest-roster--row
                      "ws-no-task-marker" "ws-no-task-marker" 'ready))
               'task))
      ;; Assert.
      (agent-repl-itest--await-log daemon "elisp.rpc.push-invalid" "error")
      (should (agent-repl-itest--logged-p daemon "elisp.rpc.push-invalid" "error"))
      (should (agent-repl-itest-roster--push-invalid-carries-raw
               daemon "ws-no-task-marker")))))

;; audit-3 #42
(ert-deftest agent-repl-itest-roster-priority-badge-clears-when-the-row-drops-it ()
  "A row that stops carrying `priority' loses its badge on the next push.
sidebar.proto RosterRowPriorityBadge: \"UNSET = unprioritized (no badge)\"
-- a badge that never cleared would flag a workspace the resolver has
since deprioritized."
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest-roster--with-subscription daemon
      (agent-repl-itest-roster--push
       daemon (agent-repl-itest-roster--roster
               (list (agent-repl-itest-roster--row
                      "itest-badge-clear" "itest-badge-clear" 'ready
                      '(priority . ((label . "P1")))))))
      (agent-repl-itest--wait-until
       (lambda ()
         (and (agent-repl--ws-known-p "itest-badge-clear")
              (string-prefix-p
               "P1" (or (agent-repl--tab-badge-str
                         "itest-badge-clear"
                         (agent-repl-status-tab-state "itest-badge-clear"))
                        ""))))
       nil "the priority badge to draw")
      ;; Act.
      (agent-repl-itest-roster--push
       daemon (agent-repl-itest-roster--roster
               (list (agent-repl-itest-roster--row "itest-badge-clear" "itest-badge-clear" 'ready))))
      ;; Assert.
      (agent-repl-itest--wait-until
       (lambda () (not (string-prefix-p
                        "P1" (or (agent-repl--tab-badge-str
                                  "itest-badge-clear"
                                  (agent-repl-status-tab-state "itest-badge-clear"))
                                 ""))))
       nil "the priority badge to clear")
      (should-not (string-prefix-p
                   "P1" (or (agent-repl--tab-badge-str
                             "itest-badge-clear"
                             (agent-repl-status-tab-state "itest-badge-clear"))
                            ""))))))

(provide 'test-integration-roster)

;;; test-integration-roster.el ends here
