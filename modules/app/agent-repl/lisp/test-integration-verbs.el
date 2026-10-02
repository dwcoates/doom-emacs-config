;;; test-integration-verbs.el --- Integration: verbs.el against the fake daemon -*- lexical-binding: t; -*-

;;; Commentary:

;; Scenario 13 of elisp-fanout.md §14: every workspace and daemon-admin verb
;; Emacs wraps, asserted at the wire.
;;
;; Emacs commands are THIN WRAPPERS: send the request, await the ack, update
;; editor state.  Every piece of real machinery — git, worktrees, session
;; lifecycle, merge orchestration — is the daemon's, so what a verb test can
;; legitimately assert is the REQUEST it sent and the editor-state update it
;; made, never a git effect.
;;
;; Vocabulary note (doom-speak realigned to the contract): doom's old "kill"
;; is CLOSE, doom's old "nuke" is KILL, and NUKE destroys data.

;;; Code:

(require 'ert)
(require 'cl-lib)

(load (expand-file-name "test-integration-helpers.el"
                        (file-name-directory (or load-file-name buffer-file-name)))
      nil t)

;; Production names this suite depends on (verbs.el, §9; host.el, §7).
(declare-function agent-repl-verb-close "verbs")
(declare-function agent-repl-verb-kill "verbs")
(declare-function agent-repl-verb-nuke "verbs")
(declare-function agent-repl-verb-open "verbs")
(declare-function agent-repl-verb-merge "verbs")
(declare-function agent-repl-verb-restart "verbs")
(declare-function agent-repl-verb-create "verbs")
(declare-function agent-repl-verb-set-priority "verbs")
(declare-function agent-repl-verb-shutdown-schedule "verbs")
(declare-function agent-repl-verb-merge-queue "verbs")
(declare-function agent-repl-daemon-health "verbs")
(declare-function agent-repl-session-health "verbs")
(declare-function agent-repl-host-register "host")
(declare-function agent-repl-host-subscribe "host")
(declare-function agent-repl-host-unsubscribe "host")
(declare-function agent-repl-connect-open "connect")
(declare-function agent-repl-connect-close "connect")
(declare-function agent-repl-link-primary "daemon-link")
(declare-function agent-repl--ws-known-p "workspace")
(declare-function agent-repl--ws-live-p "workspace")
(defvar agent-repl--workspaces)
(declare-function agent-repl--ws-put "workspace")
(declare-function agent-repl-frontend-daemon-stop "daemon")
(declare-function agent-repl-link-successor "daemon-link")
(declare-function agent-repl-host-faults "host")
(declare-function agent-repl-roster-subscribe "roster")
(declare-function agent-repl-merge-queue-pause "verbs")
(declare-function agent-repl-merge-queue-resume "verbs")
(declare-function agent-repl-create-workspace "verbs")
(declare-function agent-repl-create-child-workspace "verbs")
(declare-function agent-repl-fork-workspace "verbs")
(declare-function agent-repl-create-oneshot "verbs")
(declare-function agent-repl-open-workspace "verbs")
(declare-function agent-repl-daemon-shutdown-schedule "verbs")
(declare-function agent-repl-daemon-shutdown-cancel "verbs")
(declare-function agent-repl-daemon-shutdown-now "verbs")
(defvar agent-repl-roster-view)
(defvar agent-repl-roster-update-functions)
(defvar agent-repl-roster-finish-functions)

;;;; ---- Fixtures ----

(defconst agent-repl-itest-verbs--ws "itest-verbs-ws"
  "The Doom workspace name every verb in this suite targets.")

(defconst agent-repl-itest-verbs--dir
  (agent-repl-itest--fixture-dir "verbs-ws")
  "The workspace directory registered for that workspace.")

(defconst agent-repl-itest-verbs--repo
  (list :id "repo-itest" :dir (agent-repl-itest--fixture-dir "verbs-repo"))
  "The RepositoryRef fixture, as it would arrive from the roster's sections.")

(defmacro agent-repl-itest-verbs--with-workspace (daemon ref &rest body)
  "Register and subscribe the suite's workspace on DAEMON, then run BODY.
REF is bound to the minted WorkspaceRef.  Every verb resolves its ref
through `agent-repl-host-ref', so the workspace must be registered first."
  (declare (indent 2) (debug (form symbolp body)))
  `(let ((conn (agent-repl-connect-open (agent-repl-itest-daemon-address ,daemon))))
     (unwind-protect
         (let ((,ref nil))
           ;; `agent-repl--workspaces' outlives a scenario, and a teardown
           ;; TOMBSTONES rather than removes, so a fixture that reused the
           ;; name would start already-dead.  The record is dropped outright
           ;; here, which no production path may do.
           (remhash agent-repl-itest-verbs--ws agent-repl--workspaces)
           (agent-repl--ws-put agent-repl-itest-verbs--ws
                               :project-dir agent-repl-itest-verbs--dir)
           (agent-repl-host-register conn agent-repl-itest-verbs--dir
                                     (lambda (minted) (setq ,ref minted)))
           (agent-repl-itest--wait-until (lambda () ,ref) nil
                                         "RegisterWorkspace to answer")
           (agent-repl-host-subscribe conn agent-repl-itest-verbs--ws ,ref)
           (agent-repl-itest--await-subscriber ,daemon "host" (plist-get ,ref :id))
           ,@body)
       (ignore-errors (agent-repl-host-unsubscribe agent-repl-itest-verbs--ws))
       (agent-repl-connect-close conn))))

(defun agent-repl-itest-verbs--body (daemon method)
  "Return DAEMON's first recorded request body for METHOD."
  (car (agent-repl-itest--call-bodies daemon method)))

(defmacro agent-repl-itest-verbs--with-primary (daemon var &rest body)
  "Run BODY with VAR bound to a connection on DAEMON standing in as the primary.
Several verbs address no particular workspace -- CreateWorkspace,
UpdateShutdownSchedule, UpdateMergeQueue, DaemonHealth, OpenWorkspace, and
`agent-repl-frontend-daemon-stop' -- so `agent-repl-verbs--conn' (and
`agent-repl-frontend-daemon-stop' directly) falls back to
`agent-repl-link-primary' rather than a workspace's own connection.  This
fixture stands that fallback up against DAEMON directly, without
daemon-link.el's own reconnect machinery."
  (declare (indent 2) (debug (form symbolp body)))
  `(let ((,var (agent-repl-connect-open (agent-repl-itest-daemon-address ,daemon))))
     (unwind-protect
         (cl-letf (((symbol-function 'agent-repl-link-primary) (lambda () ,var)))
           ,@body)
       (agent-repl-connect-close ,var))))

(defun agent-repl-itest-verbs--said (text)
  "Return the `UserSaid' plist carrying TEXT as its one text block.
A self-contained equivalent of verbs.el's private `agent-repl-verbs--said',
kept here so this suite does not reach into another module's internal
helper."
  (list :content (list :blocks (list (list :arm :text :value (list :text text))))))

(defun agent-repl-itest-verbs--host-live-with-faults (fault-detail)
  "Return a HostWorkspace protojson alist: a LIVE, open session with one fault.
FAULT-DETAIL is that fault's `detail' string.  Every non-optional field of
the live arm is populated, matching what `agent-repl-host--live' requires
to resolve at all.

THE FAULT NAMES ITS KIND.  \"`detail' supplements it and never replaces
it, so a fault with no kind is a contract breach\"
(`agent-repl-wire-decode-host-fault-kind'), and `opened_at_ms' is
required beside it -- a fixture missing either is refused while decoding,
which leaves the push landing NO host state at all."
  `((existing . ((id . ((value . "host-session-verbs")))
                 (live . ((generation . ((value . "gen-1")))
                          (shimAttached . t)
                          (claude . ((sessionId . "vendor-1")
                                     (configDir . "/home/itest/.claude")))
                          (backfill . ((done . ())))
                          (faults . [((detail . ,fault-detail)
                                      (openedAtMs . "1735689600000")
                                      (linkSevered . ()))])
                          (open . ())))))
    (naming . ())))

(defun agent-repl-itest-verbs--await-health (text)
  "Block until the health buffer carries TEXT.
THE BUFFER OUTLIVES THE SCENARIO -- every pull APPENDS into the one
`*agent-repl-health*\=' -- so waiting on the buffer's mere EXISTENCE is
satisfied instantly by the previous scenario's render and races the one
under test.  Waiting on the rendered TEXT is what actually observes this
pull.  Callers kill the buffer before acting so the text cannot be a
leftover either."
  (agent-repl-itest--wait-until
   (lambda ()
     (let ((buffer (get-buffer "*agent-repl-health*")))
       (and buffer
            (with-current-buffer buffer
              (string-match-p (regexp-quote text) (buffer-string))))))
   nil (format "the health buffer to carry %S" text)))

(defun agent-repl-itest-verbs--ack-logged-p (daemon op)
  "Return non-nil when DAEMON's log carries a success ack for OP.
Every verb logs through the SAME `elisp.verbs.ack' format string
(`agent-repl-verbs--send'), so the branch is read out of the expanded
MESSAGE field rather than the OPERATION slug, which cannot distinguish
them."
  (cl-some (lambda (record)
             (string-match-p (format "op=%s ws=" (regexp-quote op))
                             (or (alist-get 'message record) "")))
           (agent-repl-itest--log-entries daemon "elisp.verbs.ack" "info")))

;;;; ---- Per-verb requests ----

(ert-deftest agent-repl-itest-verbs-close-echoes-the-ref ()
  "CloseWorkspace sends the workspace's ref, echoed verbatim.
Close is a VIEW act: fast ack, tab gone, the daemon-to-shim session
UNTOUCHED."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest-verbs--with-workspace daemon ref
      ;; Act.
      (agent-repl-verb-close agent-repl-itest-verbs--ws)
      (agent-repl-itest--await-call daemon "CloseWorkspace")
      ;; Assert.
      (should (equal (agent-repl-itest--body-field
                      (agent-repl-itest-verbs--body daemon "CloseWorkspace")
                      'workspace 'id)
                     (plist-get ref :id))))))

(ert-deftest agent-repl-itest-verbs-close-success-tears-the-tab-down ()
  "A Close success removes the tab; the roster's reconciliation agrees.
Teardown is idempotent against the roster push that follows."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest-verbs--with-workspace daemon ref
      (ignore ref)
      ;; Act.
      (agent-repl-verb-close agent-repl-itest-verbs--ws)
      (agent-repl-itest--await-call daemon "CloseWorkspace")
      ;; Assert: LIVENESS is what a torn-down tab loses.  `--ws-del'
      ;; TOMBSTONES the entry by design — the identity record survives so
      ;; reverse-lookups and the revival picker still resolve it — so
      ;; `--ws-known-p' stays true and says nothing about the tab.
      (agent-repl-itest--wait-until
       (lambda () (not (agent-repl--ws-live-p agent-repl-itest-verbs--ws)))
       nil "the closed workspace's tab to go away")
      (should-not (agent-repl--ws-live-p agent-repl-itest-verbs--ws)))))

(ert-deftest agent-repl-itest-verbs-close-blocked-keeps-the-tab ()
  "A `blocked' close keeps the tab and raises NO dialog.
The refusal manifests in the WEBAPP FOOTER with daemon-composed reasons;
the response carries only the blocked cause arm.  Close REQUIRES QUIET —
undelivered user intent may never be silently discarded."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest--script daemon "CloseWorkspace"
                              '((error . ((blocked . ())))))
    (agent-repl-itest-verbs--with-workspace daemon ref
      (ignore ref)
      ;; Act.
      (agent-repl-verb-close agent-repl-itest-verbs--ws)
      (agent-repl-itest--await-call daemon "CloseWorkspace")
      ;; Assert: the workspace survives, and the fact is logged rather than
      ;; put in front of the user as an Emacs dialog.
      (agent-repl-itest--await-log daemon "elisp.verbs.close-blocked" "info")
      (should (agent-repl--ws-live-p agent-repl-itest-verbs--ws)))))

(ert-deftest agent-repl-itest-verbs-kill-echoes-the-ref ()
  "KillWorkspace sends the ref; it never blocks and never warns.
The big red button: forced session death, worktree and branch survive."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest-verbs--with-workspace daemon ref
      ;; Act.
      (agent-repl-verb-kill agent-repl-itest-verbs--ws)
      (agent-repl-itest--await-call daemon "KillWorkspace")
      ;; Assert.
      (should (equal (agent-repl-itest--body-field
                      (agent-repl-itest-verbs--body daemon "KillWorkspace")
                      'workspace 'id)
                     (plist-get ref :id))))))

(ert-deftest agent-repl-itest-verbs-nuke-echoes-the-ref ()
  "NukeWorkspace sends the ref; the daemon kills first, then destroys data.
A nuked row LEAVES the roster entirely rather than going closed (E5)."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest-verbs--with-workspace daemon ref
      ;; Act.
      (agent-repl-verb-nuke agent-repl-itest-verbs--ws)
      (agent-repl-itest--await-call daemon "NukeWorkspace")
      ;; Assert.
      (should (equal (agent-repl-itest--body-field
                      (agent-repl-itest-verbs--body daemon "NukeWorkspace")
                      'workspace 'id)
                     (plist-get ref :id))))))

(ert-deftest agent-repl-itest-verbs-open-echoes-a-closed-rows-ref ()
  "OpenWorkspace just opens; any revival happens under the hood.
Its ref comes from a CLOSED roster row, which is the only place Emacs can
learn about a workspace it has no tab for."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest-verbs--with-workspace daemon ref
      ;; Act.
      (agent-repl-verb-open ref)
      (agent-repl-itest--await-call daemon "OpenWorkspace")
      ;; Assert.
      (should (equal (agent-repl-itest--body-field
                      (agent-repl-itest-verbs--body daemon "OpenWorkspace")
                      'workspace 'id)
                     (plist-get ref :id))))))

(ert-deftest agent-repl-itest-verbs-merge-enqueues-and-keeps-no-state ()
  "MergeWorkspace success means ENQUEUED, and Emacs keeps no merge state.
The merge's whole life from there is the webapp feed's merge bubble and
the roster/footer; Emacs's durable merged memory was REMOVED."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest-verbs--with-workspace daemon ref
      ;; Act.
      (agent-repl-verb-merge agent-repl-itest-verbs--ws)
      (agent-repl-itest--await-call daemon "MergeWorkspace")
      ;; Assert: the request carried the ref, and the tab is untouched — a
      ;; merged tab is neither hidden nor greyed by Emacs.
      (should (equal (agent-repl-itest--body-field
                      (agent-repl-itest-verbs--body daemon "MergeWorkspace")
                      'workspace 'id)
                     (plist-get ref :id)))
      (should (agent-repl--ws-live-p agent-repl-itest-verbs--ws)))))

(ert-deftest agent-repl-itest-verbs-restart-sends-no-force-field ()
  "A restart sends the workspace alone: there is no `force' field.
The daemon owns everything the restart entails, webapp bounce included, and
every restart is immediate."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest-verbs--with-workspace daemon ref
      (ignore ref)
      ;; Act.
      (agent-repl-verb-restart agent-repl-itest-verbs--ws)
      (agent-repl-itest--await-call daemon "RestartWorkspace")
      ;; Assert.
      (should-not (string-match-p
                   "\"force\""
                   (car (agent-repl-itest--call-raw-bodies daemon "RestartWorkspace")))))))

;;;; ---- CreateWorkspace: the two forms and the creation facts ----

(ert-deftest agent-repl-itest-verbs-create-standard-sends-the-repository ()
  "The standard form names the repository; THE DAEMON names and creates all.
The RepositoryRef comes from the roster's repo sections, and no account
field exists — the account is DETERMINED by the repo-under-root rule."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest-verbs--with-workspace daemon ref
      (ignore ref)
      ;; Act.
      (agent-repl-verb-create agent-repl-itest-verbs--repo :standard)
      (agent-repl-itest--await-call daemon "CreateWorkspace")
      ;; Assert.
      (let ((body (agent-repl-itest-verbs--body daemon "CreateWorkspace")))
        (should (equal (agent-repl-itest--body-field body 'repository 'id) "repo-itest"))
        ;; `standard' with nothing set is `{}', which parses back to nil, so
        ;; PRESENCE is the assertion — the same shape the fork and priority
        ;; cases assert.
        (should (assq 'standard body))))))

(ert-deftest agent-repl-itest-verbs-create-standard-carries-the-initial-prompt ()
  "An initial prompt rides the standard form as UserSaid.
Creation and the first prompt are one act: no host materialization
round-trip exists."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest-verbs--with-workspace daemon ref
      (ignore ref)
      ;; Act.
      (agent-repl-verb-create agent-repl-itest-verbs--repo
                              :standard :initial-prompt "fix the flake")
      (agent-repl-itest--await-call daemon "CreateWorkspace")
      ;; Assert.
      (let* ((body (agent-repl-itest-verbs--body daemon "CreateWorkspace"))
             (blocks (agent-repl-itest--body-field
                      body 'standard 'initialPrompt 'content 'blocks)))
        (should (equal (agent-repl-itest--body-field (car blocks) 'text 'text)
                       "fix the flake"))))))

(ert-deftest agent-repl-itest-verbs-create-one-shot-sends-its-prompt-alone ()
  "The one-shot form rides the request as its prompt and nothing else.
ONE-SHOTS ride the wire now: Emacs supplies {prompt, model, parentage}
and the DAEMON owns naming, worktree and decoration.  There is no finish
choice at all -- completion is the repository's own directive."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest-verbs--with-workspace daemon ref
      (ignore ref)
      ;; Act.
      (agent-repl-verb-create agent-repl-itest-verbs--repo
                              :one-shot :prompt "land the fix")
      (agent-repl-itest--await-call daemon "CreateWorkspace")
      ;; Assert.
      (let ((one-shot (agent-repl-itest--body-field
                       (agent-repl-itest-verbs--body daemon "CreateWorkspace") 'oneShot)))
        (should (assq 'prompt one-shot))
        (should (equal (mapcar #'car one-shot) '(prompt)))))))

(ert-deftest agent-repl-itest-verbs-create-parent-carries-the-parents-ref ()
  "A child workspace names its parent by ref.
Parentage is a creation fact the daemon derives everything else from; the
ref is echoed, never rebuilt from a path."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest-verbs--with-workspace daemon ref
      ;; Act.
      (agent-repl-verb-create agent-repl-itest-verbs--repo :standard
                              :parent ref)
      (agent-repl-itest--await-call daemon "CreateWorkspace")
      ;; Assert.
      (should (equal (agent-repl-itest--body-field
                      (agent-repl-itest-verbs--body daemon "CreateWorkspace")
                      'parent 'workspace 'id)
                     (plist-get ref :id))))))

(ert-deftest agent-repl-itest-verbs-create-fork-is-a-presence-only-fact ()
  "`fork' is presence-only: set as an empty message, absent otherwise.
An empty message says the fact is TRUE by being there; there is no
boolean to get backwards."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest-verbs--with-workspace daemon ref
      ;; Act.
      (agent-repl-verb-create agent-repl-itest-verbs--repo :standard
                              :parent ref :fork t)
      (agent-repl-itest--await-call daemon "CreateWorkspace")
      ;; Assert: `{}' — present and empty.
      (let ((body (agent-repl-itest-verbs--body daemon "CreateWorkspace")))
        (should (assq 'fork (agent-repl-itest--body-field body 'parent)))))))

(ert-deftest agent-repl-itest-verbs-create-without-fork-omits-it ()
  "Without a fork, the field is ABSENT — never an empty-but-present marker.
PRESENCE, NEVER SENTINELS: the absence is the whole statement."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest-verbs--with-workspace daemon ref
      ;; Act.
      (agent-repl-verb-create agent-repl-itest-verbs--repo :standard
                              :parent ref)
      (agent-repl-itest--await-call daemon "CreateWorkspace")
      ;; Assert.
      (let ((body (agent-repl-itest-verbs--body daemon "CreateWorkspace")))
        (should-not (assq 'fork (agent-repl-itest--body-field body 'parent)))))))

(ert-deftest agent-repl-itest-verbs-create-priority-rides-as-an-arm ()
  "A creation priority rides as the WorkspacePriority level ARM.
Every state is a oneof, never an enum, so a priority is `{p1:{}}' rather
than a number to compare."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest-verbs--with-workspace daemon ref
      (ignore ref)
      ;; Act.
      (agent-repl-verb-create agent-repl-itest-verbs--repo :standard
                              :priority :p1)
      (agent-repl-itest--await-call daemon "CreateWorkspace")
      ;; Assert.
      (let ((body (agent-repl-itest-verbs--body daemon "CreateWorkspace")))
        (should (assq 'p1 (agent-repl-itest--body-field body 'priority)))))))

(ert-deftest agent-repl-itest-verbs-create-success-opens-no-tab-itself ()
  "Create success does nothing to the editor: the ROSTER push opens the tab.
The workspace appears on the roster and Emacs reacts — the same reactive
model as the webapp."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest-verbs--with-workspace daemon ref
      (ignore ref)
      (let ((before (agent-repl--ws-known-p "itest-verbs-created")))
        ;; Act.
        (agent-repl-verb-create agent-repl-itest-verbs--repo :standard)
        (agent-repl-itest--await-call daemon "CreateWorkspace")
        ;; Assert.
        (should (equal before (agent-repl--ws-known-p "itest-verbs-created")))))))

;;;; ---- SetWorkspacePriority ----

(ert-deftest agent-repl-itest-verbs-set-priority-sends-the-level-arm ()
  "Setting a priority sends its level arm beside the ref."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest-verbs--with-workspace daemon ref
      (ignore ref)
      ;; Act.
      (agent-repl-verb-set-priority agent-repl-itest-verbs--ws :p05)
      (agent-repl-itest--await-call daemon "SetWorkspacePriority")
      ;; Assert.
      (let ((body (agent-repl-itest-verbs--body daemon "SetWorkspacePriority")))
        (should (assq 'p05 (agent-repl-itest--body-field body 'priority)))))))

(ert-deftest agent-repl-itest-verbs-clear-priority-omits-the-field ()
  "CLEARING a priority is the ABSENCE of the field, never a sentinel level.
`priority' is `optional' for exactly this reason."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest-verbs--with-workspace daemon ref
      (ignore ref)
      ;; Act.
      (agent-repl-verb-set-priority agent-repl-itest-verbs--ws nil)
      (agent-repl-itest--await-call daemon "SetWorkspacePriority")
      ;; Assert.
      (let ((body (agent-repl-itest-verbs--body daemon "SetWorkspacePriority")))
        (should-not (assq 'priority body))))))

;;;; ---- Daemon-admin verbs ----

(ert-deftest agent-repl-itest-verbs-shutdown-schedule-sends-a-reason ()
  "A scheduled shutdown carries its REQUIRED reason.
Ruled 2026-08-29: the reason is required on schedule, so every client can
draw the standing banner with a cause."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest-verbs--with-workspace daemon ref
      (ignore ref)
      ;; Act.
      (agent-repl-verb-shutdown-schedule
       (list :arm :schedule :at-ms 1735689600000
             :reason (list :arm :deploy)))
      (agent-repl-itest--await-call daemon "UpdateShutdownSchedule")
      ;; Assert.
      (let ((body (agent-repl-itest-verbs--body daemon "UpdateShutdownSchedule")))
        (should (assq 'deploy (agent-repl-itest--body-field body 'schedule 'reason)))))))

(ert-deftest agent-repl-itest-verbs-shutdown-cancel-sends-the-cancel-arm ()
  "Cancelling a shutdown sends the `cancel' arm, which carries nothing."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest-verbs--with-workspace daemon ref
      (ignore ref)
      ;; Act.
      (agent-repl-verb-shutdown-schedule (list :arm :cancel))
      (agent-repl-itest--await-call daemon "UpdateShutdownSchedule")
      ;; Assert.
      (let ((body (agent-repl-itest-verbs--body daemon "UpdateShutdownSchedule")))
        (should (assq 'cancel body))))))

(ert-deftest agent-repl-itest-verbs-shutdown-now-carries-its-reason ()
  "An immediate shutdown also names a reason: this is how Emacs stops one.
Emacs never KILLS a daemon that answers; it asks it to drain and exit."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest-verbs--with-workspace daemon ref
      (ignore ref)
      ;; Act.
      (agent-repl-verb-shutdown-schedule
       (list :arm :now :reason (list :arm :operator :note "emacs")))
      (agent-repl-itest--await-call daemon "UpdateShutdownSchedule")
      ;; Assert.
      (let ((body (agent-repl-itest-verbs--body daemon "UpdateShutdownSchedule")))
        (should (equal (agent-repl-itest--body-field body 'now 'reason 'operator 'note)
                       "emacs"))))))

(ert-deftest agent-repl-itest-verbs-merge-queue-pause-sends-the-pause-arm ()
  "Pausing the merge queue sends the `pause' arm.
Operator control; the visible state rides the merge bubble's queue tab."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest-verbs--with-workspace daemon ref
      (ignore ref)
      ;; Act.
      (agent-repl-verb-merge-queue (list :arm :pause))
      (agent-repl-itest--await-call daemon "UpdateMergeQueue")
      ;; Assert.
      (let ((body (agent-repl-itest-verbs--body daemon "UpdateMergeQueue")))
        (should (assq 'pause body))
        ;; endpoint_update_merge_queue.proto, UpdateMergeQueuePause: "WHICH
        ;; repository's queue.  UNSET = every repository that has a queue
        ;; (the daemon-wide switch)."  The absence IS the daemon-wide
        ;; meaning, so an encoder that started emitting an empty
        ;; `repository' object would change what the request ASKS FOR.
        (should-not (assq 'repository
                          (agent-repl-itest--body-field body 'pause)))))))

(ert-deftest agent-repl-itest-verbs-merge-queue-evict-carries-the-ref ()
  "Evicting from the merge queue names the workspace by ref."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest-verbs--with-workspace daemon ref
      ;; Act.
      (agent-repl-verb-merge-queue (list :arm :evict :workspace ref))
      (agent-repl-itest--await-call daemon "UpdateMergeQueue")
      ;; Assert.
      (should (equal (agent-repl-itest--body-field
                      (agent-repl-itest-verbs--body daemon "UpdateMergeQueue")
                      'evict 'workspace 'id)
                     (plist-get ref :id))))))

(ert-deftest agent-repl-itest-verbs-daemon-health-prints-the-faults ()
  "An UNHEALTHY daemon verdict is an ANSWER, and its faults are rendered.
Never a transport error: a success arm carrying typed fault lists with
dynamic detail strings."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    ;; THE KIND IS A TYPED ARM: `detail' supplements it, so a fault without
    ;; one is a contract breach the codec refuses before rendering.
    (let* ((fault '((logSinkPoisoned . ()) (detail . "merge worker wedged")))
           (response `((success . ((unhealthy . ((faults . [,fault]))))))))
      (agent-repl-itest--script daemon "DaemonHealth" response))
    (agent-repl-itest-verbs--with-workspace daemon ref
      (ignore ref)
      (when (get-buffer "*agent-repl-health*") (kill-buffer "*agent-repl-health*"))
      ;; Act.
      (agent-repl-daemon-health)
      (agent-repl-itest--await-call daemon "DaemonHealth")
      ;; Assert.
      (agent-repl-itest-verbs--await-health "merge worker wedged")
      (with-current-buffer "*agent-repl-health*"
        (should (string-match-p "merge worker wedged" (buffer-string)))))))

(ert-deftest agent-repl-itest-verbs-session-health-echoes-the-ref ()
  "SessionHealth names one workspace and has its own fault vocabulary.
DaemonFault and SessionFault are deliberately SEPARATE vocabularies."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest-verbs--with-workspace daemon ref
      ;; Act.
      (agent-repl-session-health agent-repl-itest-verbs--ws)
      (agent-repl-itest--await-call daemon "SessionHealth")
      ;; Assert.
      (should (equal (agent-repl-itest--body-field
                      (agent-repl-itest-verbs--body daemon "SessionHealth")
                      'workspace 'id)
                     (plist-get ref :id))))))

(ert-deftest agent-repl-itest-verbs-error-arm-warns-rather-than-signals ()
  "A verb's error ARM is warned about, not raised as a transport failure.
The two are different facts: the daemon answered and refused, versus the
daemon could not be reached."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    ;; THE ARM IS THE REFUSAL, so the scripted error names one; an unset
    ;; cause would be a contract breach rather than a refusal to report.
    (agent-repl-itest--script daemon "MergeWorkspace"
                              '((error . ((alreadyQueued . ())))))
    (agent-repl-itest-verbs--with-workspace daemon ref
      (ignore ref)
      ;; Act.
      (agent-repl-verb-merge agent-repl-itest-verbs--ws)
      (agent-repl-itest--await-call daemon "MergeWorkspace")
      ;; Assert.
      (agent-repl-itest--await-log daemon "elisp.verbs.merge-refused" "warn")
      (should (agent-repl-itest--logged-p daemon "elisp.verbs.merge-refused" "warn")))))

(ert-deftest agent-repl-itest-verbs-transport-failure-logs-an-error ()
  "A verb whose daemon has gone away logs an ERROR, loudly.
Connection death is detected at the transport, and unary calls fail
loudly — no staleness machinery softens it."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest-verbs--with-workspace daemon ref
      (ignore ref)
      (agent-repl-itest--stop-daemon daemon t)
      ;; Act.
      (ignore-errors (agent-repl-verb-merge agent-repl-itest-verbs--ws))
      ;; Assert.
      (agent-repl-itest--wait-until
       (lambda () (agent-repl-itest--logged-p daemon "elisp.verbs.transport-failure" "error"))
       nil "the transport-failure log line")
      (should (agent-repl-itest--logged-p daemon "elisp.verbs.transport-failure" "error")))))

;;;; ---- Kill/Nuke success: tear the tab down (audit finding 69) ----

(ert-deftest agent-repl-itest-verbs-kill-success-tears-the-tab-down ()
  "A Kill success removes the tab, exactly like Close.
Pins fanout \"Kill/Nuke success -> tear the tab down\" (elisp-fanout.md
§9): a stuck tab after a successful forced kill would strand the user on
a session the daemon has already torn down."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest-verbs--with-workspace daemon ref
      (ignore ref)
      ;; Act.
      (agent-repl-verb-kill agent-repl-itest-verbs--ws)
      (agent-repl-itest--await-call daemon "KillWorkspace")
      ;; Assert: "Kill/Nuke success -> tear the tab down" (elisp-fanout.md §9).
      ;; LIVENESS is what a torn-down tab loses, exactly as the Close case
      ;; pins: `--ws-del' TOMBSTONES the entry so reverse-lookups and the
      ;; revival picker still resolve it, which leaves `--ws-known-p' true
      ;; and saying nothing about the tab.
      (agent-repl-itest--wait-until
       (lambda () (not (agent-repl--ws-live-p agent-repl-itest-verbs--ws)))
       nil "the killed workspace's tab to go away")
      (should-not (agent-repl--ws-live-p agent-repl-itest-verbs--ws)))))

(ert-deftest agent-repl-itest-verbs-nuke-success-tears-the-tab-down ()
  "A Nuke success removes the tab, exactly like Close and Kill.
Pins fanout \"Kill/Nuke success -> tear the tab down\" (elisp-fanout.md
§9): a nuked workspace whose tab lingers would let the user act on a
worktree and branch that are already deleted."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest-verbs--with-workspace daemon ref
      (ignore ref)
      ;; Act.
      (agent-repl-verb-nuke agent-repl-itest-verbs--ws)
      (agent-repl-itest--await-call daemon "NukeWorkspace")
      ;; Assert: "Kill/Nuke success -> tear the tab down" (elisp-fanout.md §9).
      ;; LIVENESS is what a torn-down tab loses, exactly as the Close case
      ;; pins: `--ws-del' TOMBSTONES the entry so reverse-lookups and the
      ;; revival picker still resolve it, which leaves `--ws-known-p' true
      ;; and saying nothing about the tab.
      (agent-repl-itest--wait-until
       (lambda () (not (agent-repl--ws-live-p agent-repl-itest-verbs--ws)))
       nil "the nuked workspace's tab to go away")
      (should-not (agent-repl--ws-live-p agent-repl-itest-verbs--ws)))))

;;;; ---- Success messages (audit finding 70) ----

(ert-deftest agent-repl-itest-verbs-restart-success-messages ()
  "A Restart success reports itself via `message'.
Pins fanout \"Restart success -> `message'\" (elisp-fanout.md §9): a
restart with no Messages-buffer feedback would leave the user unable to
tell the daemon ever heard the request."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest-verbs--with-workspace daemon ref
      (ignore ref)
      (let (messages)
        (cl-letf (((symbol-function 'message)
                   (lambda (fmt &rest args)
                     (push (apply #'format fmt args) messages)
                     nil)))
          ;; Act.
          (agent-repl-verb-restart agent-repl-itest-verbs--ws)
          (agent-repl-itest--await-call daemon "RestartWorkspace")
          ;; Assert: "Restart success -> `message'" (elisp-fanout.md §9).
          (agent-repl-itest--wait-until (lambda () messages) nil
                                        "the restart success message")
          (should (cl-some (lambda (m) (string-match-p "restart" m)) messages)))))))

(ert-deftest agent-repl-itest-verbs-merge-success-messages-merge-enqueued ()
  "A Merge success reports itself as exactly \"merge enqueued\".
Pins fanout \"Merge success -> `message \"merge enqueued\"'\"
(elisp-fanout.md §9): Emacs holds no merge state, so this message is the
whole of what the user learns from the ack."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest-verbs--with-workspace daemon ref
      (ignore ref)
      (let (messages)
        (cl-letf (((symbol-function 'message)
                   (lambda (fmt &rest args)
                     (push (apply #'format fmt args) messages)
                     nil)))
          ;; Act.
          (agent-repl-verb-merge agent-repl-itest-verbs--ws)
          (agent-repl-itest--await-call daemon "MergeWorkspace")
          ;; Assert: "Merge success -> `message \"merge enqueued\"'" (elisp-fanout.md §9).
          ;; WAIT FOR THE MESSAGE THIS TEST IS ABOUT, not for "some message":
          ;; the `elisp.verbs.send' info record `--send' writes on issuing the
          ;; call is already in this list before the daemon has even
          ;; answered (every rung below `agent-repl--error' emits through
          ;; `message', quietly, via `agent-repl--emit-message'), so waiting
          ;; on a non-empty list is satisfied by the send log and races the
          ;; success ack that produces "merge enqueued" itself.
          (agent-repl-itest--wait-until
           (lambda () (member "merge enqueued" messages))
           nil "the merge success message")
          (should (member "merge enqueued" messages)))))))

;;;; ---- Close `blocked' messaging, no dialog (audit finding 71) ----

(ert-deftest agent-repl-itest-verbs-close-blocked-messages-without-a-dialog ()
  "A `blocked' close messages the exact footer-pointer text and raises no dialog.
Pins fanout \"Close `blocked' -> log INFO + `message \"close blocked --
see the workspace footer\"', no dialog\" (elisp-fanout.md §9);
`y-or-n-p'/`yes-or-no-p' are wired to ERROR so any dialog attempt fails
this test loudly rather than hanging a batch run."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest--script daemon "CloseWorkspace" '((error . ((blocked . ())))))
    (agent-repl-itest-verbs--with-workspace daemon ref
      (ignore ref)
      (let (messages)
        (cl-letf (((symbol-function 'message)
                   (lambda (fmt &rest args)
                     (push (apply #'format fmt args) messages)
                     nil))
                  ((symbol-function 'y-or-n-p)
                   (lambda (&rest _) (error "agent-repl-itest: a dialog was raised")))
                  ((symbol-function 'yes-or-no-p)
                   (lambda (&rest _) (error "agent-repl-itest: a dialog was raised"))))
          ;; Act.
          (agent-repl-verb-close agent-repl-itest-verbs--ws)
          (agent-repl-itest--await-call daemon "CloseWorkspace")
          ;; Assert: the exact footer-pointer text, and no dialog function ran
          ;; (a run would have signalled out of the `cl-letf' stubs above).
          ;; WAIT FOR THE MESSAGE THIS TEST IS ABOUT, not for "some message":
          ;; every ladder rung below `agent-repl--error' emits through
          ;; `message' (quietly, via `agent-repl--emit-message'), so the
          ;; `elisp.verbs.send' info record close itself writes is already in
          ;; this list before the daemon has even been asked -- waiting on a
          ;; non-empty list is therefore satisfied by the send log and races
          ;; the refusal reply that produces the footer pointer.
          (agent-repl-itest--wait-until
           (lambda () (member "close blocked -- see the workspace footer" messages))
           nil "the close-blocked message")
          (should (member "close blocked -- see the workspace footer" messages)))))))

;;;; ---- Raw-wire explicit-false assertions (audit findings 72, 73) ----

(ert-deftest agent-repl-itest-verbs-merge-sends-own-branch-keep-open-false-on-the-raw-wire ()
  "Emacs's merge names its own branch as the source, `keep_open' spelled false.
Only the raw wire text can tell an explicit false from an omitted field
(merge-landing.md, Landed change 1)."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest-verbs--with-workspace daemon ref
      (ignore ref)
      ;; Act.
      (agent-repl-verb-merge agent-repl-itest-verbs--ws)
      (agent-repl-itest--await-call daemon "MergeWorkspace")
      ;; Assert.
      (let ((raw (car (agent-repl-itest--call-raw-bodies daemon "MergeWorkspace"))))
        (should (string-match-p
                 (regexp-quote "\"source\":{\"ownBranch\":{\"keepOpen\":false}}")
                 raw))))))

(ert-deftest agent-repl-itest-verbs-merge-keep-open-sends-keep-open-true ()
  "A keep-open merge reaches the daemon as `own_branch' with `keep_open' true."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest-verbs--with-workspace daemon ref
      (ignore ref)
      ;; Act.
      (agent-repl-verb-merge agent-repl-itest-verbs--ws t)
      (agent-repl-itest--await-call daemon "MergeWorkspace")
      ;; Assert.
      (should (eq (agent-repl-itest--body-field
                   (agent-repl-itest-verbs--body daemon "MergeWorkspace")
                   'source 'ownBranch 'keepOpen)
                  t)))))

;;;; ---- CreateWorkspaceRequest fields (audit finding 75) ----

(ert-deftest agent-repl-itest-verbs-create-sends-the-model ()
  "The session model at creation rides `model' on the wire.
Pins \"CreateWorkspaceRequest model\" (elisp-fanout.md §5): a picked
model that never reaches the wire would start every workspace on the
daemon's default regardless of what the user chose."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest-verbs--with-primary daemon conn
      (ignore conn)
      ;; Act.
      (agent-repl-verb-create agent-repl-itest-verbs--repo
                              :standard
                              :model "opus")
      (agent-repl-itest--await-call daemon "CreateWorkspace")
      ;; Assert.
      (let ((body (agent-repl-itest-verbs--body daemon "CreateWorkspace")))
        (should (equal (agent-repl-itest--body-field body 'model) "opus"))))))

(ert-deftest agent-repl-itest-verbs-create-allow-ungated-is-presence-only ()
  "Ungated consent rides `allowUngated' as a present, empty message.
Pins \"CreateWorkspaceRequest ... allow_ungated\" (elisp-fanout.md §5):
presence itself is the consent, never a boolean to get backwards."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest-verbs--with-primary daemon conn
      (ignore conn)
      ;; Act.
      (agent-repl-verb-create agent-repl-itest-verbs--repo
                              :standard
                              :allow-ungated t)
      (agent-repl-itest--await-call daemon "CreateWorkspace")
      ;; Assert: present, and empty (`{}').
      (let ((body (agent-repl-itest-verbs--body daemon "CreateWorkspace")))
        (should (assq 'allowUngated body))))))

(ert-deftest agent-repl-itest-verbs-create-standard-sends-the-base-ref ()
  "The standard form's base ref rides `standard.baseRef'.
Pins \"CreateWorkspaceRequest ... standard.base_ref\" (elisp-fanout.md
§5): an absent base ref means the repo's default resolution, so a SET
one must actually reach the daemon to be honored."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest-verbs--with-primary daemon conn
      (ignore conn)
      ;; Act.
      (agent-repl-verb-create agent-repl-itest-verbs--repo
                              :standard :base-ref "main")
      (agent-repl-itest--await-call daemon "CreateWorkspace")
      ;; Assert.
      (let ((body (agent-repl-itest-verbs--body daemon "CreateWorkspace")))
        (should (equal (agent-repl-itest--body-field body 'standard 'baseRef) "main"))))))

(ert-deftest agent-repl-itest-verbs-create-standard-sends-the-name ()
  "The standard form's user-supplied name rides `standard.name'.
Pins \"CreateWorkspaceRequest ... standard.name\" (elisp-fanout.md §5):
an absent name means the daemon mints one, so a supplied name must reach
the wire to override that."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest-verbs--with-primary daemon conn
      (ignore conn)
      ;; Act.
      (agent-repl-verb-create agent-repl-itest-verbs--repo
                              :standard :name "my-ws")
      (agent-repl-itest--await-call daemon "CreateWorkspace")
      ;; Assert.
      (let ((body (agent-repl-itest-verbs--body daemon "CreateWorkspace")))
        (should (equal (agent-repl-itest--body-field body 'standard 'name) "my-ws"))))))

(ert-deftest agent-repl-itest-verbs-create-standard-merge-actions-before-ws-merge-is-a-usersaid ()
  "The standard form's pre-merge action rides as a UserSaid, not a bare string.
Pins \"CreateWorkspaceRequest ... standard.merge_actions\"
(elisp-fanout.md §5): \"merge-action fields are UserSaid values\"."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest-verbs--with-primary daemon conn
      (ignore conn)
      ;; Act.
      (agent-repl-verb-create
       agent-repl-itest-verbs--repo
       :standard :merge-actions (list :before-ws-merge "run the linter"))
      (agent-repl-itest--await-call daemon "CreateWorkspace")
      ;; Assert.
      (let* ((body (agent-repl-itest-verbs--body daemon "CreateWorkspace"))
             (blocks (agent-repl-itest--body-field
                      body 'standard 'mergeActions 'beforeWsMerge 'content 'blocks)))
        (should (equal (agent-repl-itest--body-field (car blocks) 'text 'text)
                       "run the linter"))))))

;;;; ---- CreateWorkspaceStandard: unset prompt is absence (audit finding 76) ----

(ert-deftest agent-repl-itest-verbs-create-standard-with-no-prompt-omits-initial-prompt ()
  "An unset standard-form prompt leaves `initialPrompt' ABSENT, not an empty UserSaid.
Pins \"UNSET = an empty workspace; presence, never an empty UserSaid\"
(endpoint_create_workspace.proto, CreateWorkspaceStandard.initial_prompt):
an empty-but-present UserSaid would misrepresent a workspace with
nothing to say as one that said nothing."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest-verbs--with-primary daemon conn
      (ignore conn)
      ;; Act.
      (agent-repl-verb-create agent-repl-itest-verbs--repo
                              :standard)
      (agent-repl-itest--await-call daemon "CreateWorkspace")
      ;; Assert.
      (let ((body (agent-repl-itest-verbs--body daemon "CreateWorkspace")))
        (should-not (assq 'initialPrompt (agent-repl-itest--body-field body 'standard)))))))

;;;; ---- DrainReasonOperator note required non-blank (audit finding 77) ----

(ert-deftest agent-repl-itest-verbs-shutdown-schedule-blank-operator-note-refuses-before-send ()
  "A blank operator note is refused before send, with ZERO daemon calls.
Pins \"The note is REQUIRED non-blank -- a blank note is refused at the
request\" (drain_reason.proto, DrainReasonOperator): a blank note would
leave every client's standing drain banner naming no reason at all."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest-verbs--with-primary daemon conn
      (ignore conn)
      ;; Act / Assert.
      (should-error
       (agent-repl-verb-shutdown-schedule
        (list :arm :schedule :at-ms 1735689600000
              :reason (list :arm :operator :note "")))
       :type 'agent-repl-wire-error)
      (should (null (agent-repl-itest--calls daemon "UpdateShutdownSchedule"))))))

;;;; ---- UpdateShutdownSchedule.schedule.at_ms (audit finding 78) ----

(ert-deftest agent-repl-itest-verbs-shutdown-schedule-sends-the-at-ms ()
  "A scheduled shutdown carries its deadline instant on `schedule.atMs'.
Pins \"UpdateShutdownScheduleSchedule ... at_ms\"
(endpoint_update_shutdown_schedule.proto): a schedule without its own
deadline would leave every client's standing banner counting down to
nothing."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest-verbs--with-primary daemon conn
      (ignore conn)
      ;; Act.
      (agent-repl-verb-shutdown-schedule
       (list :arm :schedule :at-ms 1735689600000
             :reason (list :arm :deploy)))
      (agent-repl-itest--await-call daemon "UpdateShutdownSchedule")
      ;; Assert.
      (let ((body (agent-repl-itest-verbs--body daemon "UpdateShutdownSchedule")))
        (should (equal (format "%s" (agent-repl-itest--body-field body 'schedule 'atMs))
                       "1735689600000"))))))

;;;; ---- UpdateMergeQueue `resume' arm (audit finding 79) ----

(ert-deftest agent-repl-itest-verbs-merge-queue-resume-sends-the-resume-arm ()
  "Resuming the merge queue sends the `resume' arm.
Pins \"UpdateMergeQueue resume\" arm (endpoint_update_merge_queue.proto):
pause and evict are pinned elsewhere, so resume must land on the wire
too or the resume command would silently do nothing."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest-verbs--with-primary daemon conn
      (ignore conn)
      ;; Act.
      (agent-repl-verb-merge-queue (list :arm :resume))
      (agent-repl-itest--await-call daemon "UpdateMergeQueue")
      ;; Assert.
      (let ((body (agent-repl-itest-verbs--body daemon "UpdateMergeQueue")))
        (should (assq 'resume body))
        ;; endpoint_update_merge_queue.proto, UpdateMergeQueueResume: "UNSET
        ;; = every repository that has a queue (the daemon-wide switch)."
        (should-not (assq 'repository
                          (agent-repl-itest--body-field body 'resume)))))))

(ert-deftest agent-repl-itest-verbs-merge-queue-pause-scoped-names-the-repository ()
  "A pause that means ONE repository names it, rather than pausing everything.
Pins UpdateMergeQueuePause's `repository' (endpoint_update_merge_queue.proto,
added by landing 5): \"The queue is per repository, so a caller that means
one names it.\"  Without this the scoped pause silently becomes the
daemon-wide switch, which stops every other repository's queue too — the
kind of blast-radius defect no unscoped assertion can catch."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest-verbs--with-primary daemon conn
      (ignore conn)
      ;; Act.
      (agent-repl-verb-merge-queue
       (list :arm :pause :repository agent-repl-itest-verbs--repo))
      (agent-repl-itest--await-call daemon "UpdateMergeQueue")
      ;; Assert.
      (let ((body (agent-repl-itest-verbs--body daemon "UpdateMergeQueue")))
        ;; The RepositoryRef's own two fields, both of which ride: `id' is
        ;; "the sole supported repository identifier", `dir' is "NOT an
        ;; identifier" but travels with it (workspace.proto).
        (should (equal (agent-repl-itest--body-field body 'pause 'repository 'id)
                       "repo-itest"))
        (should (equal (agent-repl-itest--body-field body 'pause 'repository 'dir)
                       (agent-repl-itest--fixture-dir "verbs-repo")))))))

(ert-deftest agent-repl-itest-verbs-merge-queue-unknown-repository-is-reported ()
  "A scoped pause the daemon's registry cannot resolve is REFUSED by arm.
`UpdateMergeQueueUnknownRepository' is empty on purpose -- \"the echoed ref
is the only identity involved\" -- so the repository the operator is told
about is the one this pause sent, and it rides the log context."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest--script daemon "UpdateMergeQueue"
                              '((error . ((unknownRepository . ())))))
    (agent-repl-itest-verbs--with-primary daemon conn
      (ignore conn)
      (let (messages)
        (cl-letf (((symbol-function 'message)
                   (lambda (fmt &rest args)
                     (push (if args (apply #'format fmt args) fmt) messages)
                     nil)))
          ;; Act.
          (agent-repl-verb-merge-queue
           (list :arm :pause :repository agent-repl-itest-verbs--repo))
          (agent-repl-itest--await-call daemon "UpdateMergeQueue")
          (agent-repl-itest--await-log
           daemon "elisp.verbs.merge-queue-unknown-repository" "warn")
          ;; Assert.
          (should (agent-repl-itest--logged-p
                   daemon "elisp.verbs.merge-queue-unknown-repository" "warn"))
          (agent-repl-itest--wait-until
           (lambda ()
             (seq-some (lambda (m) (string-match-p "does not hold repository" m)) messages))
           nil "the unknown-repository message")
          (should (seq-some (lambda (m) (string-match-p "repo-itest" m)) messages)))))))

(ert-deftest agent-repl-itest-verbs-merge-queue-resume-scoped-names-the-repository ()
  "A resume that means ONE repository names it, rather than resuming everything.
Pins UpdateMergeQueueResume's `repository' (endpoint_update_merge_queue.proto,
added by landing 5): \"The queue is per repository, so a caller that means
one names it.\"  Resume carries the field independently of pause, so a
one-sided encoder would resume every repository after a scoped pause."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest-verbs--with-primary daemon conn
      (ignore conn)
      ;; Act.
      (agent-repl-verb-merge-queue
       (list :arm :resume :repository agent-repl-itest-verbs--repo))
      (agent-repl-itest--await-call daemon "UpdateMergeQueue")
      ;; Assert.
      (let ((body (agent-repl-itest-verbs--body daemon "UpdateMergeQueue")))
        (should (equal (agent-repl-itest--body-field body 'resume 'repository 'id)
                       "repo-itest"))
        (should (equal (agent-repl-itest--body-field body 'resume 'repository 'dir)
                       (agent-repl-itest--fixture-dir "verbs-repo")))))))

;;;; ---- Health rendering (audit finding 80) ----

(ert-deftest agent-repl-itest-verbs-daemon-health-healthy-renders-a-healthy-verdict ()
  "A HEALTHY daemon verdict renders as HEALTHY in the health buffer.
Pins \"agent-repl-daemon-health ... (render into *agent-repl-health*:
verdict, ...)\" (elisp-fanout.md §9): only the UNHEALTHY branch is
pinned elsewhere, leaving the healthy branch untested without this."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest--script daemon "DaemonHealth" '((success . ((healthy . ())))))
    (agent-repl-itest-verbs--with-primary daemon conn
      (ignore conn)
      (when (get-buffer "*agent-repl-health*") (kill-buffer "*agent-repl-health*"))
      ;; Act.
      (agent-repl-daemon-health)
      (agent-repl-itest--await-call daemon "DaemonHealth")
      ;; Assert.
      (agent-repl-itest-verbs--await-health "daemon: HEALTHY")
      (with-current-buffer "*agent-repl-health*"
        (should (string-match-p "daemon: HEALTHY" (buffer-string)))))))

(ert-deftest agent-repl-itest-verbs-session-health-unhealthy-prints-faults-and-standing-host-faults ()
  "SessionHealth renders its own pulled faults AND the host stream's standing faults.
Pins \"agent-repl-session-health ... (render into *agent-repl-health*:
verdict, each fault's detail, plus the host stream's standing faults for
the workspace)\" (elisp-fanout.md §9)."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest--script
     daemon "SessionHealth"
     ;; THE ARM IS THE FAULT CLASS: a SessionFault with no `kind' is a
     ;; contract breach the codec refuses while decoding, so the scripted
     ;; fault names one (`agent-repl-wire-decode-session-fault-kind').
     '((success . ((unhealthy . ((faults . [((detail . "pulled fault")
                                             (linkSevered . ()))])))))))
    (agent-repl-itest-verbs--with-workspace daemon ref
      (agent-repl-itest--push
       daemon "host"
       `((host . ,(agent-repl-itest-verbs--host-live-with-faults "standing fault")))
       (plist-get ref :id))
      (agent-repl-itest--wait-until
       (lambda () (agent-repl-host-faults agent-repl-itest-verbs--ws))
       nil "the standing host fault to reach host state")
      (when (get-buffer "*agent-repl-health*") (kill-buffer "*agent-repl-health*"))
      ;; Act.
      (agent-repl-session-health agent-repl-itest-verbs--ws)
      (agent-repl-itest--await-call daemon "SessionHealth")
      ;; Assert.
      (agent-repl-itest-verbs--await-health "pulled fault")
      (agent-repl-itest-verbs--await-health "standing fault")
      (with-current-buffer "*agent-repl-health*"
        (should (string-match-p "pulled fault" (buffer-string)))
        (should (string-match-p "standing fault" (buffer-string)))))))

;;;; ---- Ref resolution: unregistered workspace refuses before send (audit finding 81) ----

(ert-deftest agent-repl-itest-verbs-unregistered-workspace-refuses-before-send ()
  "A verb on an unregistered workspace name signals `user-error', sending nothing.
Pins \"Each resolves REF via `agent-repl-host-ref' (nil ->
`user-error')\" (elisp-fanout.md §9): a workspace with no daemon
identity cannot be addressed at all, and the failure must happen before
any bytes leave Emacs."
  ;; Arrange / Act / Assert.
  (agent-repl-itest--with-fake-daemon daemon
    (should-error (agent-repl-verb-close "itest-verbs-never-registered") :type 'user-error)
    (should (null (agent-repl-itest--calls daemon "CloseWorkspace")))))

;;;; ---- CONN resolution across a handover (audit finding 82) ----

(ert-deftest agent-repl-itest-verbs-verb-lands-on-the-successor-after-a-transfer ()
  "After a workspace transfers, its verb lands on the SUCCESSOR, not the primary.
Pins \"CONN via `agent-repl-host-conn' (falls back to
`agent-repl-link-primary')\" (elisp-fanout.md §9): once `transferred'
adopts the workspace onto the new daemon, `agent-repl-host-conn' must
resolve there, or every later verb would keep hammering a daemon that
already released the workspace."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon primary
    (agent-repl-itest-verbs--with-workspace primary ref
      (agent-repl-itest--with-second-daemon primary successor
        (let ((successor-conn (agent-repl-connect-open
                               (agent-repl-itest-daemon-address successor))))
          (unwind-protect
              (cl-letf (((symbol-function 'agent-repl-link-successor)
                         (lambda () successor-conn)))
                ;; Act: the transfer adopts the workspace onto the successor.
                (agent-repl-itest--push primary "host" '((transferred . ()))
                                        (plist-get ref :id))
                (agent-repl-itest--await-call successor "AdoptHostWorkspace")
                (agent-repl-itest--await-subscriber successor "host" (plist-get ref :id))
                (agent-repl-verb-close agent-repl-itest-verbs--ws)
                ;; Assert.
                (agent-repl-itest--await-call successor "CloseWorkspace")
                (should (null (agent-repl-itest--calls primary "CloseWorkspace"))))
            (agent-repl-connect-close successor-conn)))))))

;;;; ---- agent-repl-frontend-daemon-stop wire shape (audit finding 83) ----

(ert-deftest agent-repl-itest-verbs-frontend-daemon-stop-sends-update-shutdown-schedule-now-operator-emacs ()
  "`agent-repl-frontend-daemon-stop' sends UpdateShutdownSchedule{now, operator \"emacs\"}.
Pins \"`agent-repl-frontend-daemon-stop' = UpdateShutdownSchedule{now,
operator \"emacs\"}\" (elisp-fanout.md §11): Emacs never kills a daemon
that answers, so this is the only shutdown Emacs itself ever sends."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest-verbs--with-primary daemon conn
      (ignore conn)
      ;; Act.
      (agent-repl-frontend-daemon-stop)
      (agent-repl-itest--await-call daemon "UpdateShutdownSchedule")
      ;; Assert.
      (let ((body (agent-repl-itest-verbs--body daemon "UpdateShutdownSchedule")))
        (should (assq 'now body))
        (should (equal (agent-repl-itest--body-field body 'now 'reason 'operator 'note)
                       "emacs"))))))

;;;; ---- Success-ack log lines, named per verbs.el's vocabulary (finding 91, partial) ----

(ert-deftest agent-repl-itest-verbs-close-success-logs-its-ack ()
  "A Close success logs `elisp.verbs.ack' naming op=close.
verbs.el's vocabulary names CloseWorkspace \"a VIEW act\"; its success
ack must be findable in the production log by that op."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest-verbs--with-workspace daemon ref
      (ignore ref)
      ;; Act.
      (agent-repl-verb-close agent-repl-itest-verbs--ws)
      (agent-repl-itest--await-call daemon "CloseWorkspace")
      ;; Assert.
      (agent-repl-itest--wait-until
       (lambda () (agent-repl-itest-verbs--ack-logged-p daemon "close"))
       nil "the close success ack log line")
      (should (agent-repl-itest-verbs--ack-logged-p daemon "close")))))

(ert-deftest agent-repl-itest-verbs-kill-success-logs-its-ack ()
  "A Kill success logs `elisp.verbs.ack' naming op=kill.
verbs.el's vocabulary names KillWorkspace \"forced session death\"; its
success ack must be findable in the production log by that op."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest-verbs--with-workspace daemon ref
      (ignore ref)
      ;; Act.
      (agent-repl-verb-kill agent-repl-itest-verbs--ws)
      (agent-repl-itest--await-call daemon "KillWorkspace")
      ;; Assert.
      (agent-repl-itest--wait-until
       (lambda () (agent-repl-itest-verbs--ack-logged-p daemon "kill"))
       nil "the kill success ack log line")
      (should (agent-repl-itest-verbs--ack-logged-p daemon "kill")))))

(ert-deftest agent-repl-itest-verbs-nuke-success-logs-its-ack ()
  "A Nuke success logs `elisp.verbs.ack' naming op=nuke.
verbs.el's vocabulary names NukeWorkspace \"DATA DESTRUCTION\"; its
success ack must be findable in the production log by that op."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest-verbs--with-workspace daemon ref
      (ignore ref)
      ;; Act.
      (agent-repl-verb-nuke agent-repl-itest-verbs--ws)
      (agent-repl-itest--await-call daemon "NukeWorkspace")
      ;; Assert.
      (agent-repl-itest--wait-until
       (lambda () (agent-repl-itest-verbs--ack-logged-p daemon "nuke"))
       nil "the nuke success ack log line")
      (should (agent-repl-itest-verbs--ack-logged-p daemon "nuke")))))

(ert-deftest agent-repl-itest-verbs-open-success-logs-its-ack ()
  "An Open success logs `elisp.verbs.ack' naming op=open.
verbs.el's vocabulary names OpenWorkspace \"opens a closed row\"; its
success ack must be findable in the production log by that op."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest-verbs--with-workspace daemon ref
      (agent-repl-itest-verbs--with-primary daemon conn
        (ignore conn)
        ;; Act.
        (agent-repl-verb-open ref)
        (agent-repl-itest--await-call daemon "OpenWorkspace")
        ;; Assert.
        (agent-repl-itest--wait-until
         (lambda () (agent-repl-itest-verbs--ack-logged-p daemon "open"))
         nil "the open success ack log line")
        (should (agent-repl-itest-verbs--ack-logged-p daemon "open"))))))

(ert-deftest agent-repl-itest-verbs-merge-success-logs-its-ack ()
  "A Merge success logs `elisp.verbs.ack' naming op=merge.
verbs.el's vocabulary names MergeWorkspace's success as \"ENQUEUED\"; its
success ack must be findable in the production log by that op."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest-verbs--with-workspace daemon ref
      (ignore ref)
      ;; Act.
      (agent-repl-verb-merge agent-repl-itest-verbs--ws)
      (agent-repl-itest--await-call daemon "MergeWorkspace")
      ;; Assert.
      (agent-repl-itest--wait-until
       (lambda () (agent-repl-itest-verbs--ack-logged-p daemon "merge"))
       nil "the merge success ack log line")
      (should (agent-repl-itest-verbs--ack-logged-p daemon "merge")))))

(ert-deftest agent-repl-itest-verbs-restart-success-logs-its-ack ()
  "A Restart success logs `elisp.verbs.ack' naming op=restart.
verbs.el's vocabulary names RestartWorkspace as owning everything the
restart entails; its success ack must be findable in the production log
by that op."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest-verbs--with-workspace daemon ref
      (ignore ref)
      ;; Act.
      (agent-repl-verb-restart agent-repl-itest-verbs--ws)
      (agent-repl-itest--await-call daemon "RestartWorkspace")
      ;; Assert.
      (agent-repl-itest--wait-until
       (lambda () (agent-repl-itest-verbs--ack-logged-p daemon "restart"))
       nil "the restart success ack log line")
      (should (agent-repl-itest-verbs--ack-logged-p daemon "restart")))))

(ert-deftest agent-repl-itest-verbs-create-success-logs-its-ack ()
  "A Create success logs `elisp.verbs.ack' naming op=create.
verbs.el's vocabulary names CreateWorkspace as the daemon naming and
creating everything; its success ack must be findable in the production
log by that op."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest-verbs--with-primary daemon conn
      (ignore conn)
      ;; Act.
      (agent-repl-verb-create agent-repl-itest-verbs--repo
                              :standard)
      (agent-repl-itest--await-call daemon "CreateWorkspace")
      ;; Assert.
      (agent-repl-itest--wait-until
       (lambda () (agent-repl-itest-verbs--ack-logged-p daemon "create"))
       nil "the create success ack log line")
      (should (agent-repl-itest-verbs--ack-logged-p daemon "create")))))

;;;; ---- Audit-2 additions (R-SUITE-2) ----
;;
;; Findings 24-31 of docs/overhaul/reports/elisp-suite-audit-2.md.  Kept in
;; their own section so they merge cleanly beside concurrent edits above.

(declare-function agent-repl-link-connect "daemon-link")
(declare-function agent-repl-link-teardown "daemon-link")
(declare-function agent-repl-nuke-workspace "verbs")
(declare-function agent-repl-restart-workspace "verbs")
(declare-function agent-repl--ws-current-name "workspace")
(defvar agent-repl-link-up-functions)
(defvar agent-repl-link-down-functions)
(defvar agent-repl-link-handover-functions)
(defvar agent-repl-link-drain-functions)
(defvar agent-repl-link-no-daemon-functions)
(defvar agent-repl-link-drain)
(defvar agent-repl-link-drain-segment)

(defun agent-repl-itest-verbs--announce (daemon address)
  "Push `shutdown_announced' on DAEMON's daemon stream naming ADDRESS.
The handover arms on a verb ack are relayed into host.el's ONE adopt
walk, and that walk adopts only onto a successor daemon-link has
ACCEPTED, so the scenario has to stand a real dual attach."
  (agent-repl-itest--push
   daemon "daemon"
   `((shutdownAnnounced
      . ((address . ,address)
         (cause . ((selfMergeRollout . ())))
         (expectedOutageMs . "1500")
         (mintedAtMs . ,(format "%d" (truncate (* 1000 (float-time))))))))))

(defmacro agent-repl-itest-verbs--with-link (daemon &rest body)
  "Stand a real link on DAEMON around BODY, with every link hook scratched."
  (declare (indent 1) (debug (form body)))
  `(let ((agent-repl-link-up-functions nil)
         (agent-repl-link-down-functions nil)
         (agent-repl-link-handover-functions nil)
         (agent-repl-link-drain-functions nil)
         (agent-repl-link-no-daemon-functions nil)
         (agent-repl-link-drain nil)
         (agent-repl-link-drain-segment nil))
     (unwind-protect
         (progn
           (agent-repl-link-connect)
           (agent-repl-itest--await-subscriber ,daemon "daemon")
           ,@body)
       (agent-repl-link-teardown))))

(defun agent-repl-itest-verbs--log-arguments (daemon operation level)
  "Return every logged format ARGUMENT string for OPERATION at LEVEL on DAEMON.
`agent-repl--log-record' records each format argument as
`prin1-to-string' under `context.arguments', which is where fanout §0
puts dynamic values."
  (apply #'append
         (mapcar (lambda (record)
                   (alist-get 'arguments (alist-get 'context record)))
                 (agent-repl-itest--log-entries daemon operation level))))

;; audit-2 #24
(ert-deftest agent-repl-itest-verbs-merge-transferring-away-routes-to-the-handover ()
  "`transferring_away' on a verb ack is HANDOVER NEWS, not a refusal to report.
verbs.el `agent-repl-verbs--handover-arms' + fanout §7 HANDOVER REDIAL.
Reporting it would be telling the user something went wrong during the
one rollout that is supposed to be invisible — so it is INFO, nothing is
drawn, and the workspace is adopted onto the successor instead."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon primary
    (agent-repl-itest-verbs--with-link primary
      (agent-repl-itest-verbs--with-workspace primary ref
        (agent-repl-itest--with-second-daemon primary successor
          (let ((messages nil))
            (agent-repl-itest--script
             primary "MergeWorkspace"
             `((error . ((transferringAway
                          . ((address . ,(agent-repl-itest-daemon-address successor))))))))
            (cl-letf (((symbol-function 'message)
                       (lambda (fmt &rest args) (push (apply #'format fmt args) messages))))
              ;; Act.
              (agent-repl-verb-merge agent-repl-itest-verbs--ws)
              (agent-repl-itest--await-call primary "MergeWorkspace"))
            ;; Assert.
            (agent-repl-itest--await-log
             primary "elisp.verbs.merge-handover-refusal" "info")
            (should (null (agent-repl-itest--log-entries
                           primary "elisp.verbs.merge-refused" "warn")))
            (should-not (seq-some (lambda (m) (string-match-p "merge refused" m)) messages))
            (agent-repl-itest--await-call successor "AdoptHostWorkspace")
            (should (equal (agent-repl-itest--body-field
                            (car (agent-repl-itest--call-bodies
                                  successor "AdoptHostWorkspace"))
                            'workspace 'id)
                           (plist-get ref :id)))))))))

;; audit-2 #24
(ert-deftest agent-repl-itest-verbs-merge-not-yet-adopted-routes-to-the-handover ()
  "`not_yet_adopted' on a verb ack is the OTHER silent handover arm.
fanout §7: the successor has not finished adopting the workspace yet, so
the adopt is retried once its `WatchDaemon' is accepted.  Nothing is
drawn here either."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon primary
    (agent-repl-itest-verbs--with-link primary
      (agent-repl-itest-verbs--with-workspace primary ref
        (agent-repl-itest--with-second-daemon primary successor
          (let ((messages nil))
            (agent-repl-itest--script primary "MergeWorkspace"
                                      '((error . ((notYetAdopted . ())))))
            (cl-letf (((symbol-function 'message)
                       (lambda (fmt &rest args) (push (apply #'format fmt args) messages))))
              ;; Act: the refusal arrives before any successor stands.
              (agent-repl-verb-merge agent-repl-itest-verbs--ws)
              (agent-repl-itest--await-call primary "MergeWorkspace")
              (agent-repl-itest--await-log
               primary "elisp.verbs.merge-handover-refusal" "info")
              ;; The successor is announced and accepted only now.
              (agent-repl-itest-verbs--announce
               primary (agent-repl-itest-daemon-address successor)))
            ;; Assert.
            (should (null (agent-repl-itest--log-entries
                           primary "elisp.verbs.merge-refused" "warn")))
            (should-not (seq-some (lambda (m) (string-match-p "merge refused" m)) messages))
            (agent-repl-itest--await-call successor "AdoptHostWorkspace")
            (should (equal (agent-repl-itest--body-field
                            (car (agent-repl-itest--call-bodies
                                  successor "AdoptHostWorkspace"))
                            'workspace 'id)
                           (plist-get ref :id)))))))))

;; audit-2 #25
(ert-deftest agent-repl-itest-verbs-refusal-context-names-the-arm-keyword ()
  "A generic refusal's log CONTEXT carries the arm keyword itself.
fanout §0: \"Dynamic values go in the context\"; verbs.el logs `arm=%S
fields=%S'.  Asserting only the operation slug would pass with the arm
never recorded, which is exactly the fact a refusal reader needs."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest--script daemon "MergeWorkspace"
                              '((error . ((alreadyQueued . ())))))
    (agent-repl-itest-verbs--with-workspace daemon ref
      (ignore ref)
      ;; Act.
      (agent-repl-verb-merge agent-repl-itest-verbs--ws)
      (agent-repl-itest--await-log daemon "elisp.verbs.merge-refused" "warn")
      ;; Assert.
      (should (seq-some
               (lambda (s) (string-match-p ":already-queued" s))
               (agent-repl-itest-verbs--log-arguments
                daemon "elisp.verbs.merge-refused" "warn"))))))

;; audit-2 #25
(ert-deftest agent-repl-itest-verbs-refusal-fields-reach-the-context-and-the-message ()
  "A PAYLOAD-BEARING refusal arm's own fields reach both surfaces.
`CloseWorkspaceWorkspaceRefMismatch{registry_dir}' is useless without the
dir: verbs.el renders \"close refused: workspace-ref-mismatch <fields>\"
and logs the same fields in the context, so both are asserted."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest--script
     daemon "CloseWorkspace"
     '((error . ((workspaceRefMismatch . ((registryDir . "/x")))))))
    (agent-repl-itest-verbs--with-workspace daemon ref
      (ignore ref)
      (let ((messages nil))
        (cl-letf (((symbol-function 'message)
                   (lambda (fmt &rest args) (push (apply #'format fmt args) messages))))
          ;; Act.
          (agent-repl-verb-close agent-repl-itest-verbs--ws)
          (agent-repl-itest--await-log daemon "elisp.verbs.close-refused" "warn"))
        ;; Assert.
        (should (seq-some (lambda (s) (string-match-p "/x" s))
                          (agent-repl-itest-verbs--log-arguments
                           daemon "elisp.verbs.close-refused" "warn")))
        (should (seq-some
                 (lambda (m) (string-match-p "close refused: workspace-ref-mismatch" m))
                 messages))
        (should (seq-some (lambda (m) (string-match-p "/x" m)) messages))))))

;; audit-2 #26
(ert-deftest agent-repl-itest-verbs-fork-without-a-parent-refuses-before-send ()
  "A fork with NO parent is refused before send, with ZERO daemon calls.
`endpoint_create_workspace.proto': \"a fork without a parent is
unrepresentable\" — the fact lives INSIDE the parent by construction.
Silently dropping the fork would create an ordinary workspace where the
caller asked for a forked conversation, which is worse than a refusal.

The signal is `user-error', the kind fanout §9 gives every verbs-layer
pre-send guard (\"nil -> `user-error'\" for a missing ref, and see
`agent-repl-itest-verbs-unregistered-workspace-refuses-before-send').  The
neighbouring `agent-repl-wire-error' refusals are raised one layer down, by
the codec, on a request the verb did build; this one never reaches it."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest-verbs--with-primary daemon conn
      (ignore conn)
      ;; Act / Assert.
      (should-error
       (agent-repl-verb-create agent-repl-itest-verbs--repo :standard :fork t)
       :type 'user-error)
      (should (null (agent-repl-itest--calls daemon "CreateWorkspace"))))))

;; audit-2 #27
(ert-deftest agent-repl-itest-verbs-shutdown-now-without-a-reason-refuses-before-send ()
  "An immediate shutdown with NO reason is refused before send.
`endpoint_update_shutdown_schedule.proto' `UpdateShutdownScheduleNow':
\"Why — REQUIRED\".  Every client draws the standing banner from the
reason, so a `now' without one would leave the fleet announcing nothing."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest-verbs--with-primary daemon conn
      (ignore conn)
      ;; Act / Assert.
      (should-error
       (agent-repl-verb-shutdown-schedule (list :arm :now))
       :type 'agent-repl-wire-error)
      (should (null (agent-repl-itest--calls daemon "UpdateShutdownSchedule"))))))

;; audit-2 #28
(ert-deftest agent-repl-itest-verbs-evict-without-a-workspace-refuses-before-send ()
  "An eviction naming NO workspace is refused before send.
`UpdateMergeQueueEvict.workspace' is a non-optional message; §0: \"an
incomplete request errors before send\".  An evict with no target is not a
daemon-wide act — unlike pause, which says so by omitting `repository'."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest-verbs--with-primary daemon conn
      (ignore conn)
      ;; Act / Assert.
      (should-error
       (agent-repl-verb-merge-queue (list :arm :evict))
       :type 'agent-repl-wire-error)
      (should (null (agent-repl-itest--calls daemon "UpdateMergeQueue"))))))

;; audit-2 #29
(ert-deftest agent-repl-itest-verbs-every-priority-level-arm-rides-the-wire ()
  "All FOUR `WorkspacePriority' level arms encode as their own arm.
`workspace_priority.proto' declares p05, p1, p2 and p3; only p05 and p1
have ever ridden this suite, so an encoder that mapped the two lower
levels onto each other would pass.  THE ARM IS THE PRIORITY LEVEL."
  ;; Arrange.
  (let ((arms '((:p05 . p05) (:p1 . p1) (:p2 . p2) (:p3 . p3))))
    (agent-repl-itest--with-fake-daemon daemon
      (agent-repl-itest-verbs--with-workspace daemon ref
        (ignore ref)
        (let ((sent 0))
          (dolist (case arms)
            ;; Act.
            (agent-repl-verb-set-priority agent-repl-itest-verbs--ws (car case))
            (setq sent (1+ sent))
            (agent-repl-itest--await-call daemon "SetWorkspacePriority" sent)
            ;; Assert.
            (let ((body (nth (1- sent) (agent-repl-itest--call-bodies
                                        daemon "SetWorkspacePriority"))))
              (should (assq (cdr case)
                            (agent-repl-itest--body-field body 'priority))))))))))

;; audit-2 #30
(ert-deftest agent-repl-itest-verbs-nuke-declined-at-the-confirm-sends-nothing ()
  "Declining the nuke confirm sends NOTHING and keeps the tab.
fanout §9: \"`agent-repl-nuke-workspace' (y/n confirm: data
destruction)\".  This is the ONE verb that destroys data unrecoverably, so
the confirm is part of the contract rather than a courtesy."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest-verbs--with-workspace daemon ref
      (ignore ref)
      (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) nil))
                ((symbol-function 'y-or-n-p) (lambda (&rest _) nil)))
        ;; Act.
        (agent-repl-nuke-workspace agent-repl-itest-verbs--ws)
        ;; Assert.
        (should (null (agent-repl-itest--calls daemon "NukeWorkspace")))
        (should (agent-repl--ws-known-p agent-repl-itest-verbs--ws))))))

;; audit-2 #30
(ert-deftest agent-repl-itest-verbs-nuke-confirmed-sends-the-verb ()
  "CONFIRMING the nuke sends exactly one NukeWorkspace.
The other half of the confirm: a guard that refused both answers would
pass the declining test on its own."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest-verbs--with-workspace daemon ref
      (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t))
                ((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
        ;; Act.
        (agent-repl-nuke-workspace agent-repl-itest-verbs--ws)
        (agent-repl-itest--await-call daemon "NukeWorkspace")
        ;; Assert.
        (should (equal 1 (length (agent-repl-itest--calls daemon "NukeWorkspace"))))
        (should (equal (agent-repl-itest--body-field
                        (agent-repl-itest-verbs--body daemon "NukeWorkspace")
                        'workspace 'id)
                       (plist-get ref :id)))))))

;; audit-2 #30
(ert-deftest agent-repl-itest-verbs-interactive-restart-ignores-a-prefix ()
  "`SPC o C-c' with a PREFIX ARGUMENT is the same immediate restart on the raw wire.
There is no force mode: the prefix argument changes nothing, and the request
body carries no `force' field."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest-verbs--with-workspace daemon ref
      (ignore ref)
      (cl-letf (((symbol-function 'agent-repl--ws-current-name)
                 (lambda () agent-repl-itest-verbs--ws)))
        ;; Act.
        (let ((current-prefix-arg '(4)))
          (call-interactively #'agent-repl-restart-workspace))
        (agent-repl-itest--await-call daemon "RestartWorkspace")
        ;; Assert.
        (should-not (string-match-p
                     "\"force\""
                     (car (agent-repl-itest--call-raw-bodies daemon "RestartWorkspace"))))))))

;; audit-2 #31
(ert-deftest agent-repl-itest-verbs-session-health-error-arm-renders-no-verdict ()
  "A SessionHealth `error' arm is the question NOT BEING ANSWERED.
`endpoint_session_health.proto': \"error = the question could not be
ANSWERED (unknown workspace)\".  It is not an unhealthy verdict, so no
verdict may be rendered — a surface that drew \"healthy\" here would
report a workspace the daemon has never heard of as fine."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest--script daemon "SessionHealth"
                              '((error . ((unknownWorkspace . ())))))
    (agent-repl-itest-verbs--with-workspace daemon ref
      (ignore ref)
      (when (get-buffer "*agent-repl-health*") (kill-buffer "*agent-repl-health*"))
      (let ((messages nil))
        (cl-letf (((symbol-function 'message)
                   (lambda (fmt &rest args) (push (apply #'format fmt args) messages))))
          ;; Act.
          (agent-repl-session-health agent-repl-itest-verbs--ws)
          (agent-repl-itest--await-log daemon "elisp.verbs.session-health-refused" "warn"))
        ;; Assert: the arm is named, and NO verdict was drawn.
        (should (seq-some (lambda (s) (string-match-p ":unknown-workspace" s))
                          (agent-repl-itest-verbs--log-arguments
                           daemon "elisp.verbs.session-health-refused" "warn")))
        (should (seq-some
                 (lambda (m) (string-match-p "session-health refused: unknown-workspace" m))
                 messages))
        (should (null (get-buffer "*agent-repl-health*")))))))

;;;; ---- Audit-3 additions (R-SUITE-3) ----
;;
;; Findings 52-58 of docs/overhaul/reports/elisp-suite-audit-3.md.  Kept in
;; their own section so they merge cleanly beside concurrent edits above.

(defconst agent-repl-itest-verbs--repo-protojson
  `((id . "repo-itest") (dir . ,(agent-repl-itest--fixture-dir "verbs-repo")))
  "`agent-repl-itest-verbs--repo' as protojson, for building roster pushes.")

(cl-defun agent-repl-itest-verbs--roster-row (id dir &key closed name)
  "Return a minimal, valid RosterRow protojson alist for workspace ID/DIR.
Every non-optional field is populated, matching what `agent-repl-roster-apply'
requires to decode at all (an unset `status' oneof or a missing `closed'
message are the invariant breaches audit-2's roster findings pin
elsewhere).  CLOSED renders the row closed (`RosterRowClosed'); NAME
overrides the display name, defaulting to ID."
  `((workspace . ((workspace . ((id . ,id) (dir . ,dir)))))
    (name . ((text . ,(or name id))))
    (ready . ())
    (current . ((current . :false)))
    (when . ())
    (detail . ())
    (closed . ((closed . ,(if closed t :false))))
    (availability . ((available . ())))))

(defun agent-repl-itest-verbs--roster (rows)
  "Return a WorkspaceRoster protojson alist carrying ROWS in the fixture section.
The one repository section is keyed by `agent-repl-itest-verbs--repo',
mirroring `agent-repl-itest-verbs--with-workspace''s fixture repo, so a
scoped merge-queue action resolved from it names the SAME repository the
rest of this suite already asserts against."
  `((repository
     . ((sections
         . [((key . ((repository . ,agent-repl-itest-verbs--repo-protojson)))
             (header . ((label . ((text . "itest-repo"))) (count . ((workspaces . 1)))))
             (rows . ((rows . ,(vconcat rows))))
             (expanded . ()))])))
    (task . ((sections . [])))
    (recentlyMerged . ((header . ((label . ((text . "recently merged"))) (count . ((workspaces . 1)))))
                        (rows . ((rows . [])))))))

(defmacro agent-repl-itest-verbs--with-roster (daemon rows &rest body)
  "Subscribe to DAEMON's roster stream, push ROWS, wait for the view, run BODY.
`agent-repl-roster-view' and its hook lists are scratch bindings for the
scenario, exactly as `agent-repl-itest-roster--with-subscription' binds
them -- the roster is READ THROUGH THE WIRE and decoded by production
code, never hand-built as a decoded plist, so this fixture pins the same
schema the rest of the suite does."
  (declare (indent 2) (debug (form form body)))
  `(let ((agent-repl-itest-verbs--roster-conn
          (agent-repl-connect-open (agent-repl-itest-daemon-address ,daemon)))
         (agent-repl-roster-view nil)
         (agent-repl-roster-update-functions nil)
         (agent-repl-roster-finish-functions nil))
     (unwind-protect
         (progn
           (agent-repl-roster-subscribe agent-repl-itest-verbs--roster-conn)
           (agent-repl-itest--await-subscriber ,daemon "roster")
           (agent-repl-itest--push
            ,daemon "roster"
            (list (cons 'roster (agent-repl-itest-verbs--roster ,rows))))
           (agent-repl-itest--wait-until (lambda () agent-repl-roster-view) nil
                                         "the roster push to reach the view")
           ,@body)
       (agent-repl-connect-close agent-repl-itest-verbs--roster-conn))))

;;;; ---- #52: scoped merge-queue commands resolve REPOSITORY from the roster

;; audit-3 #52
(ert-deftest agent-repl-itest-verbs-merge-queue-pause-resolves-repository-from-the-roster ()
  "A scoped merge-queue pause resolves its repository from the ROSTER.
Pins fanout §9 + endpoint_update_merge_queue.proto \"a caller that means
one names it\": `agent-repl-verbs--merge-queue-repository' reads the
roster section holding the current workspace, so `pause.repository.id'
must equal that section's own key."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest-verbs--with-workspace daemon ref
      ;; The row must carry the fixture's workspace NAME.  `--roster-row'
      ;; defaults the display name to the ref ID, and a roster row whose name
      ;; differs from the tab already holding that id is a RENAME (fanout §8):
      ;; reconcile would rename `itest-verbs-ws' to the opaque id and
      ;; `agent-repl-host-rename' would move the host entry with it, after
      ;; which `agent-repl-host-ref' for the fixture name answers nil and the
      ;; verb refuses for want of a daemon identity.  Naming the row is what
      ;; makes the roster describe the SAME workspace this fixture registered.
      (agent-repl-itest-verbs--with-roster
       daemon (list (agent-repl-itest-verbs--roster-row
                     (plist-get ref :id) (plist-get ref :dir)
                     :name agent-repl-itest-verbs--ws))
        (cl-letf (((symbol-function 'agent-repl--ws-current-name)
                   (lambda () agent-repl-itest-verbs--ws)))
          ;; Act.
          (agent-repl-merge-queue-pause)
          (agent-repl-itest--await-call daemon "UpdateMergeQueue")
          ;; Assert.
          (should (equal (agent-repl-itest--body-field
                          (agent-repl-itest-verbs--body daemon "UpdateMergeQueue")
                          'pause 'repository 'id)
                         "repo-itest")))))))

;; audit-3 #52
(ert-deftest agent-repl-itest-verbs-merge-queue-pause-without-a-roster-repository-refuses-before-send ()
  "Without a roster repository for the workspace, pause refuses before send.
`agent-repl-verbs--merge-queue-repository' errors rather than fall back to
the daemon-wide switch: sending that instead would pause every OTHER
repository too on a caller who asked about just this one."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest-verbs--with-workspace daemon ref
      (ignore ref)
      (let ((agent-repl-roster-view nil))
        (cl-letf (((symbol-function 'agent-repl--ws-current-name)
                   (lambda () agent-repl-itest-verbs--ws)))
          ;; Act / Assert.
          (should-error (agent-repl-merge-queue-pause) :type 'user-error)
          (agent-repl-itest--await-log daemon "elisp.verbs.no-repository" "warn")
          (should (null (agent-repl-itest--calls daemon "UpdateMergeQueue"))))))))

;; audit-3 #52
(ert-deftest agent-repl-itest-verbs-merge-queue-pause-daemon-wide-omits-repository-on-the-raw-wire ()
  "A prefix-argument pause omits `repository' from the raw wire entirely.
The daemon-wide switch is the ABSENCE of the field (UpdateMergeQueuePause),
so a prefix argument must never resolve, and never send, a repository."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest-verbs--with-primary daemon conn
      (ignore conn)
      ;; Act.
      (agent-repl-merge-queue-pause t)
      (agent-repl-itest--await-call daemon "UpdateMergeQueue")
      ;; Assert.
      (let ((raw (car (agent-repl-itest--call-raw-bodies daemon "UpdateMergeQueue"))))
        (should-not (string-match-p (regexp-quote "\"repository\"") raw))))))

;; audit-3 #52
(ert-deftest agent-repl-itest-verbs-merge-queue-resume-resolves-repository-from-the-roster ()
  "A scoped merge-queue resume resolves its repository from the ROSTER.
The other half of pause's roster resolution: a resolver that only worked
for pause would leave resume silently daemon-wide."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest-verbs--with-workspace daemon ref
      ;; The row must carry the fixture's workspace NAME.  `--roster-row'
      ;; defaults the display name to the ref ID, and a roster row whose name
      ;; differs from the tab already holding that id is a RENAME (fanout §8):
      ;; reconcile would rename `itest-verbs-ws' to the opaque id and
      ;; `agent-repl-host-rename' would move the host entry with it, after
      ;; which `agent-repl-host-ref' for the fixture name answers nil and the
      ;; verb refuses for want of a daemon identity.  Naming the row is what
      ;; makes the roster describe the SAME workspace this fixture registered.
      (agent-repl-itest-verbs--with-roster
       daemon (list (agent-repl-itest-verbs--roster-row
                     (plist-get ref :id) (plist-get ref :dir)
                     :name agent-repl-itest-verbs--ws))
        (cl-letf (((symbol-function 'agent-repl--ws-current-name)
                   (lambda () agent-repl-itest-verbs--ws)))
          ;; Act.
          (agent-repl-merge-queue-resume)
          (agent-repl-itest--await-call daemon "UpdateMergeQueue")
          ;; Assert.
          (should (equal (agent-repl-itest--body-field
                          (agent-repl-itest-verbs--body daemon "UpdateMergeQueue")
                          'resume 'repository 'id)
                         "repo-itest")))))))

;; audit-3 #52
(ert-deftest agent-repl-itest-verbs-merge-queue-resume-without-a-roster-repository-refuses-before-send ()
  "Without a roster repository for the workspace, resume refuses before send.
Mirrors pause's refusal exactly, on the other admin verb."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest-verbs--with-workspace daemon ref
      (ignore ref)
      (let ((agent-repl-roster-view nil))
        (cl-letf (((symbol-function 'agent-repl--ws-current-name)
                   (lambda () agent-repl-itest-verbs--ws)))
          ;; Act / Assert.
          (should-error (agent-repl-merge-queue-resume) :type 'user-error)
          (agent-repl-itest--await-log daemon "elisp.verbs.no-repository" "warn")
          (should (null (agent-repl-itest--calls daemon "UpdateMergeQueue"))))))))

;; audit-3 #52
(ert-deftest agent-repl-itest-verbs-merge-queue-resume-daemon-wide-omits-repository-on-the-raw-wire ()
  "A prefix-argument resume omits `repository' from the raw wire entirely."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest-verbs--with-primary daemon conn
      (ignore conn)
      ;; Act.
      (agent-repl-merge-queue-resume t)
      (agent-repl-itest--await-call daemon "UpdateMergeQueue")
      ;; Assert.
      (let ((raw (car (agent-repl-itest--call-raw-bodies daemon "UpdateMergeQueue"))))
        (should-not (string-match-p (regexp-quote "\"repository\"") raw))))))

;;;; ---- #53: the interactive create family

;; audit-3 #53(a)
(ert-deftest agent-repl-itest-verbs-create-workspace-interactive-omits-parent ()
  "`agent-repl-create-workspace' omits `parent' entirely.
Pins fanout §9 \"the interactive create family\": a
plain create is a TOP-LEVEL workspace, never implicitly parented onto the
workspace the command was invoked from.  No git/gh boundary is reached by
this command -- the daemon owns creation -- so only the two Emacs readers
are stubbed."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest-verbs--with-workspace daemon ref
      ;; The DYNAMIC creation modes take their repository from the roster
      ;; section the CURRENT workspace's row sits in (owner ruling,
      ;; 2026-09-12), so the row has to be on the roster for one to run at
      ;; all -- and `completing-read' errors here because a dynamic create
      ;; asks for no repository.
      (agent-repl-itest-verbs--with-roster
          daemon (list (agent-repl-itest-verbs--roster-row
                        (plist-get ref :id) (plist-get ref :dir)
                        :name agent-repl-itest-verbs--ws))
        (cl-letf (((symbol-function 'agent-repl--ws-current-name)
                   (lambda () agent-repl-itest-verbs--ws))
                  ((symbol-function 'completing-read)
                   (lambda (&rest _)
                     (error "a dynamic create must not ask for a repository")))
                  ((symbol-function 'read-string)
                   (lambda (prompt &optional initial &rest _) (ignore prompt initial) "")))
          ;; Act.
          (agent-repl-create-workspace)
          (agent-repl-itest--await-call daemon "CreateWorkspace")
          ;; Assert.
          (should-not (assq 'parent (agent-repl-itest-verbs--body daemon "CreateWorkspace"))))))))

;; audit-3 #53(b)
(ert-deftest agent-repl-itest-verbs-create-child-workspace-interactive-names-the-parent-without-a-fork ()
  "`agent-repl-create-child-workspace' makes the new workspace a CHILD, no fork.
Pins fanout §9: `parent.workspace.id' must echo the CURRENT workspace's
ref verbatim, and `fork' must stay absent -- a plain child conversation is
fresh, not a resumed one."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest-verbs--with-workspace daemon ref
      ;; The DYNAMIC creation modes take their repository from the roster
      ;; section the CURRENT workspace's row sits in (owner ruling,
      ;; 2026-09-12), so the row has to be on the roster for one to run at
      ;; all -- and `completing-read' errors here because a dynamic create
      ;; asks for no repository.
      (agent-repl-itest-verbs--with-roster
          daemon (list (agent-repl-itest-verbs--roster-row
                        (plist-get ref :id) (plist-get ref :dir)
                        :name agent-repl-itest-verbs--ws))
        (cl-letf (((symbol-function 'agent-repl--ws-current-name)
                   (lambda () agent-repl-itest-verbs--ws))
                  ((symbol-function 'completing-read)
                   (lambda (&rest _)
                     (error "a dynamic create must not ask for a repository")))
                  ((symbol-function 'read-string)
                   (lambda (prompt &optional initial &rest _) (ignore prompt initial) "")))
          ;; Act.
          (agent-repl-create-child-workspace)
          (agent-repl-itest--await-call daemon "CreateWorkspace")
          ;; Assert.
          (let ((body (agent-repl-itest-verbs--body daemon "CreateWorkspace")))
            (should (equal (agent-repl-itest--body-field body 'parent 'workspace 'id)
                           (plist-get ref :id)))
            (should-not (assq 'fork (agent-repl-itest--body-field body 'parent)))))))))

;; audit-3 #53(c)
(ert-deftest agent-repl-itest-verbs-fork-workspace-interactive-sends-parent-and-fork ()
  "`agent-repl-fork-workspace' names the parent AND sets the presence-only fork.
A fork without a parent is unrepresentable by construction, so this
command's whole contract is that BOTH facts ride together."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest-verbs--with-workspace daemon ref
      ;; The DYNAMIC creation modes take their repository from the roster
      ;; section the CURRENT workspace's row sits in (owner ruling,
      ;; 2026-09-12), so the row has to be on the roster for one to run at
      ;; all -- and `completing-read' errors here because a dynamic create
      ;; asks for no repository.
      (agent-repl-itest-verbs--with-roster
          daemon (list (agent-repl-itest-verbs--roster-row
                        (plist-get ref :id) (plist-get ref :dir)
                        :name agent-repl-itest-verbs--ws))
        (cl-letf (((symbol-function 'agent-repl--ws-current-name)
                   (lambda () agent-repl-itest-verbs--ws))
                  ((symbol-function 'completing-read)
                   (lambda (&rest _)
                     (error "a dynamic create must not ask for a repository")))
                  ((symbol-function 'read-string)
                   (lambda (prompt &optional initial &rest _)
                     (ignore prompt initial) "forked prompt")))
          ;; Act.
          (agent-repl-fork-workspace)
          (agent-repl-itest--await-call daemon "CreateWorkspace")
          ;; Assert.
          (let ((body (agent-repl-itest-verbs--body daemon "CreateWorkspace")))
            (should (equal (agent-repl-itest--body-field body 'parent 'workspace 'id)
                           (plist-get ref :id)))
            (should (assq 'fork (agent-repl-itest--body-field body 'parent)))))))))

;; audit-3 #53(e)
(ert-deftest agent-repl-itest-verbs-create-oneshot-with-a-model-prefix-sends-the-chosen-model ()
  "A model-prefix one-shot sends the picker's CHOSEN candidate as `model'.
`agent-repl-oneshot-model-candidates' backs the picker; without this a
prefix that asked for a model choice could silently create on the
daemon's default instead."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest-verbs--with-workspace daemon ref
      ;; A one-shot derives its repository (owner ruling, 2026-09-12), so the
      ;; only picker it may open is the model one this prefix asks for.
      (agent-repl-itest-verbs--with-roster
          daemon (list (agent-repl-itest-verbs--roster-row
                        (plist-get ref :id) (plist-get ref :dir)
                        :name agent-repl-itest-verbs--ws))
        (cl-letf (((symbol-function 'agent-repl--ws-current-name)
                   (lambda () agent-repl-itest-verbs--ws))
                  ((symbol-function 'completing-read)
                   (lambda (prompt &rest _)
                     (if (string-prefix-p "Model" prompt) "haiku"
                       (error "a one-shot must not ask for a repository"))))
                  ((symbol-function 'read-string)
                   (lambda (prompt &optional initial &rest _)
                     (ignore prompt initial) "one-shot with a model")))
          ;; Act.
          (agent-repl-create-oneshot t)
          (agent-repl-itest--await-call daemon "CreateWorkspace")
          ;; Assert.
          (should (equal (agent-repl-itest--body-field
                          (agent-repl-itest-verbs--body daemon "CreateWorkspace")
                          'model)
                         "haiku")))))))

;;;; ---- #54: OpenWorkspace picks a CLOSED roster row; shutdown-schedule reads

;; audit-3 #54
(ert-deftest agent-repl-itest-verbs-open-workspace-interactive-picks-a-closed-row ()
  "`agent-repl-open-workspace' completes over CLOSED rows and opens the one PICKED.
Pins fanout §9 \"(completing-read over closed rows)\": OpenWorkspace must
carry the CLOSED row's own ref -- id AND dir -- never the open row's, even
though the open row is present on the same roster."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest-verbs--with-primary daemon conn
      (ignore conn)
      (agent-repl-itest-verbs--with-roster
       daemon (list (agent-repl-itest-verbs--roster-row
                     "itest-open-ws" (agent-repl-itest--fixture-dir "itest-open-ws"))
                    (agent-repl-itest-verbs--roster-row
                     "itest-closed-ws" (agent-repl-itest--fixture-dir "itest-closed-ws") :closed t))
        (let (offered)
          (cl-letf (((symbol-function 'completing-read)
                     (lambda (prompt candidates &rest _)
                       (ignore prompt)
                       (setq offered candidates)
                       "itest-closed-ws")))
            ;; Act.
            (agent-repl-open-workspace)
            (agent-repl-itest--await-call daemon "OpenWorkspace"))
          ;; Assert: only the CLOSED row was a candidate at all.
          (should (equal offered '("itest-closed-ws")))
          (let ((body (agent-repl-itest-verbs--body daemon "OpenWorkspace")))
            (should (equal (agent-repl-itest--body-field body 'workspace 'id)
                           "itest-closed-ws"))
            (should (equal (agent-repl-itest--body-field body 'workspace 'dir)
                           (agent-repl-itest--fixture-dir "itest-closed-ws")))))))))

;; audit-3 #54
(ert-deftest agent-repl-itest-verbs-open-workspace-interactive-with-no-closed-rows-refuses-before-send ()
  "With NO closed rows on the roster, `agent-repl-open-workspace' refuses
before send, prompting for nothing at all."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest-verbs--with-primary daemon conn
      (ignore conn)
      (agent-repl-itest-verbs--with-roster
       daemon (list (agent-repl-itest-verbs--roster-row
                     "itest-open-ws" (agent-repl-itest--fixture-dir "itest-open-ws")))
        (cl-letf (((symbol-function 'completing-read)
                   (lambda (&rest _)
                     (error "agent-repl-itest: no closed row should prompt"))))
          ;; Act / Assert.
          (should-error (agent-repl-open-workspace) :type 'user-error)
          (should (null (agent-repl-itest--calls daemon "OpenWorkspace"))))))))

;; audit-3 #54
(ert-deftest agent-repl-itest-verbs-daemon-shutdown-schedule-interactive-converts-minutes-to-at-ms ()
  "`agent-repl-daemon-shutdown-schedule' converts its MINUTES argument to `atMs'.
The interactive layer is what a keybinding actually reaches; `at-ms' must
be an epoch instant MINUTES from now, not the bare minute count."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest-verbs--with-primary daemon conn
      (ignore conn)
      (let ((before (truncate (* 1000 (float-time)))))
        (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) "deploy"))
                  ((symbol-function 'read-string) (lambda (&rest _) "")))
          ;; Act.
          (agent-repl-daemon-shutdown-schedule 5)
          (agent-repl-itest--await-call daemon "UpdateShutdownSchedule")
          ;; Assert: at-ms lands within [now, now + 5min] plus generous slack
          ;; for the time the test itself took to run.
          (let* ((body (agent-repl-itest-verbs--body daemon "UpdateShutdownSchedule"))
                 (at-ms (string-to-number
                         (format "%s" (agent-repl-itest--body-field body 'schedule 'atMs)))))
            (should (>= at-ms (+ before (* 5 60 1000))))
            (should (<= at-ms (+ before (* 5 60 1000) 60000)))))))))

;; audit-3 #54
(ert-deftest agent-repl-itest-verbs-daemon-shutdown-now-interactive-blank-operator-note-refuses-before-send ()
  "`agent-repl-daemon-shutdown-now' with a BLANK operator note refuses before send.
`agent-repl-verbs--read-drain-reason' enforces the note's non-blankness
at the READER, before any request is even built."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest-verbs--with-primary daemon conn
      (ignore conn)
      (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) "operator"))
                ((symbol-function 'read-string) (lambda (&rest _) "")))
        ;; Act / Assert.
        (should-error (agent-repl-daemon-shutdown-now) :type 'user-error)
        (should (null (agent-repl-itest--calls daemon "UpdateShutdownSchedule")))))))

;;;; ---- #55: every typed fault KIND rides the health path, with its payload

;; audit-3 #55
(ert-deftest agent-repl-itest-verbs-daemon-health-every-fault-kind-renders ()
  "Every `DaemonFault' kind, with its own payload, renders its detail.
`DaemonFault' declares SIX typed, payload-bearing kind arms; the
pre-existing suite scripts only `wsmReadOnly', so an encoder/decoder
regression on any of the other five would go uncaught."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest-verbs--with-primary daemon conn
      (ignore conn)
      (when (get-buffer "*agent-repl-health*") (kill-buffer "*agent-repl-health*"))
      (let ((kinds
             `((adoptionWindowExpired
                . ((workspace . ((id . "aw-ws") (dir . "/tmp/aw-ws")))))
               (logSinkPoisoned . ((sink . "emacs.jsonl")))
               (successorSpawnFailed . ((detail . "bind: address in use")))
               (promptsDirMissing . ((path . "/var/prompts")))
               (wsmReadOnly . ())))
            (n 0))
        (dolist (kind kinds)
          (setq n (1+ n))
          (let* ((detail (format "daemon-fault-detail-%d" n))
                 (fault `((,(car kind) . ,(cdr kind)) (detail . ,detail)))
                 (response `((success . ((unhealthy . ((faults . [,fault]))))))))
            ;; Act.
            (agent-repl-itest--script daemon "DaemonHealth" response)
            (agent-repl-daemon-health)
            (agent-repl-itest--await-call daemon "DaemonHealth" n)
            ;; Assert.
            (agent-repl-itest-verbs--await-health detail)))
        (should (null (agent-repl-itest--log-entries
                       daemon "elisp.verbs.health-unknown-arm" "error")))))))

;; audit-3 #55
(ert-deftest agent-repl-itest-verbs-session-health-every-fault-kind-renders ()
  "Every `SessionFault' kind, with its own payload, renders its detail.
`SessionFault' declares THIRTEEN typed kind arms -- its own
vocabulary, deliberately separate from `DaemonFault''s -- and the
pre-existing suite scripts only `linkSevered'."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest-verbs--with-workspace daemon ref
      (ignore ref)
      (when (get-buffer "*agent-repl-health*") (kill-buffer "*agent-repl-health*"))
      (let ((kinds
             `((shimStartFailed . ((exitCode . 1) (stderrTail . "panic: oops")))
               (shimDied . ((exitCode . 137)))
               (linkSevered . ())
               (resumeFailed . ((cause . "transcript truncated")))
               (bounceDied . ())
               (bounceUnknown . ())
               (classifierFailed . ((detail . "classifier timed out")))
               (shimReported . ((component . "hooks") (kind . "permission-denied")))
               (conversationAbandoned . ((vendorSessionId . "vs-9")))
               (sessionAbsent . ())
               (watchOpenRefused . ((operation . "WatchTranscript") (handle . "h-3")))
               (daemonStateUnreadable . ((cause . "store closed")))
               (adoptionWindowExpired . ((adoptionWindow . "30s")))))
            (n 0))
        (dolist (kind kinds)
          (setq n (1+ n))
          (let* ((detail (format "session-fault-detail-%d" n))
                 (fault `((,(car kind) . ,(cdr kind)) (detail . ,detail)))
                 (response `((success . ((unhealthy . ((faults . [,fault]))))))))
            ;; Act.
            (agent-repl-itest--script daemon "SessionHealth" response)
            (agent-repl-session-health agent-repl-itest-verbs--ws)
            (agent-repl-itest--await-call daemon "SessionHealth" n)
            ;; Assert.
            (agent-repl-itest-verbs--await-health detail)))
        (should (null (agent-repl-itest--log-entries
                       daemon "elisp.verbs.health-unknown-arm" "error")))))))

;;;; ---- #56: a transport failure's exact per-op message

;; audit-3 #56
(ert-deftest agent-repl-itest-verbs-transport-failure-messages-the-exact-per-op-text ()
  "A transport failure messages EXACTLY \"merge failed -- the daemon did not
answer\", not merely an ERROR log line.
Pins fanout §9 `agent-repl--error' + `message': the pre-existing
`agent-repl-itest-verbs-transport-failure-logs-an-error' asserts only the
log side, leaving the user-facing text (`--send''s `:on-failure') unpinned."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest-verbs--with-workspace daemon ref
      (ignore ref)
      (agent-repl-itest--stop-daemon daemon t)
      (let (messages)
        (cl-letf (((symbol-function 'message)
                   (lambda (fmt &rest args) (push (apply #'format fmt args) messages))))
          ;; Act.
          (ignore-errors (agent-repl-verb-merge agent-repl-itest-verbs--ws))
          ;; Assert.
          (agent-repl-itest--wait-until
           (lambda ()
             (seq-some (lambda (m)
                         (string-match-p
                          (regexp-quote "merge failed -- the daemon did not answer") m))
                       messages))
           nil "the exact merge transport-failure message")
          (should (seq-some
                   (lambda (m)
                     (string-match-p
                      (regexp-quote "merge failed -- the daemon did not answer") m))
                   messages)))))))

;;;; ---- #57: restart success's exact text

;; audit-3 #57
(ert-deftest agent-repl-itest-verbs-restart-success-messages-the-exact-under-way-text ()
  "A Restart success messages EXACTLY \"agent-repl: restart under way\".
The pre-existing `agent-repl-itest-verbs-restart-success-messages' asserts
only a \"restart\" substring, which the `elisp.verbs.send op=restart' INFO
line ALSO satisfies -- that line reaches `message' too, quietly, via
`agent-repl--emit-message' -- so a success handler that never fired would
still pass it.  Only the exact string proves the SUCCESS branch ran, and
that it never says \"scheduled\" for a restart that is immediate."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest-verbs--with-workspace daemon ref
      (ignore ref)
      (let (messages)
        (cl-letf (((symbol-function 'message)
                   (lambda (fmt &rest args) (push (apply #'format fmt args) messages))))
          ;; Act.
          (agent-repl-verb-restart agent-repl-itest-verbs--ws)
          (agent-repl-itest--await-call daemon "RestartWorkspace")
          ;; Assert.
          (agent-repl-itest--wait-until
           (lambda () (member "agent-repl: restart under way" messages))
           nil "the exact restart message")
          (should (member "agent-repl: restart under way" messages))
          (should-not (member "agent-repl: restart scheduled" messages)))))))

;;;; ---- #58: admin-verb refusal arms and their op-named slugs

;; audit-3 #58
(ert-deftest agent-repl-itest-verbs-shutdown-cancel-nothing-scheduled-warns-with-the-op-named-slug ()
  "Cancelling with NOTHING scheduled logs `elisp.verbs.shutdown-schedule-refused'
at WARN and messages the arm by name.
`agent-repl-daemon-shutdown-cancel' issues `agent-repl-verb-shutdown-schedule'
under `:op \"shutdown-schedule\"', so that -- not a bare \"shutdown\" -- is
the slug the arm-generic refusal handler actually writes."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest--script daemon "UpdateShutdownSchedule"
                              '((error . ((nothingScheduled . ())))))
    (agent-repl-itest-verbs--with-primary daemon conn
      (ignore conn)
      (let (messages)
        (cl-letf (((symbol-function 'message)
                   (lambda (fmt &rest args) (push (apply #'format fmt args) messages))))
          ;; Act.
          (agent-repl-daemon-shutdown-cancel)
          (agent-repl-itest--await-call daemon "UpdateShutdownSchedule")
          (agent-repl-itest--await-log
           daemon "elisp.verbs.shutdown-schedule-refused" "warn")
          ;; Assert.
          (should (agent-repl-itest--logged-p
                   daemon "elisp.verbs.shutdown-schedule-refused" "warn"))
          (should (seq-some
                   (lambda (m) (string-match-p
                               "shutdown-schedule refused: nothing-scheduled" m))
                   messages)))))))

;; audit-3 #58
(ert-deftest agent-repl-itest-verbs-merge-queue-pause-already-paused-warns-with-the-op-named-slug ()
  "Pausing an ALREADY-PAUSED queue logs `elisp.verbs.merge-queue-refused' at
WARN and messages the arm by name."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest--script daemon "UpdateMergeQueue"
                              '((error . ((alreadyPaused . ())))))
    (agent-repl-itest-verbs--with-primary daemon conn
      (ignore conn)
      (let (messages)
        (cl-letf (((symbol-function 'message)
                   (lambda (fmt &rest args) (push (apply #'format fmt args) messages))))
          ;; Act.
          (agent-repl-verb-merge-queue (list :arm :pause))
          (agent-repl-itest--await-call daemon "UpdateMergeQueue")
          (agent-repl-itest--await-log daemon "elisp.verbs.merge-queue-refused" "warn")
          ;; Assert.
          (should (agent-repl-itest--logged-p
                   daemon "elisp.verbs.merge-queue-refused" "warn"))
          (should (seq-some
                   (lambda (m) (string-match-p "merge-queue refused: already-paused" m))
                   messages)))))))

;; audit-3 #58
(ert-deftest agent-repl-itest-verbs-merge-queue-resume-not-paused-warns-with-the-op-named-slug ()
  "Resuming a queue that is NOT paused logs `elisp.verbs.merge-queue-refused'
at WARN and messages the arm by name."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest--script daemon "UpdateMergeQueue"
                              '((error . ((notPaused . ())))))
    (agent-repl-itest-verbs--with-primary daemon conn
      (ignore conn)
      (let (messages)
        (cl-letf (((symbol-function 'message)
                   (lambda (fmt &rest args) (push (apply #'format fmt args) messages))))
          ;; Act.
          (agent-repl-verb-merge-queue (list :arm :resume))
          (agent-repl-itest--await-call daemon "UpdateMergeQueue")
          (agent-repl-itest--await-log daemon "elisp.verbs.merge-queue-refused" "warn")
          ;; Assert.
          (should (agent-repl-itest--logged-p
                   daemon "elisp.verbs.merge-queue-refused" "warn"))
          (should (seq-some
                   (lambda (m) (string-match-p "merge-queue refused: not-paused" m))
                   messages)))))))

;; audit-3 #58
(ert-deftest agent-repl-itest-verbs-merge-queue-evict-no-such-queued-merge-warns-with-the-op-named-slug ()
  "Evicting a workspace with NO queued merge logs
`elisp.verbs.merge-queue-refused' at WARN and messages the arm by name."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest--script daemon "UpdateMergeQueue"
                              '((error . ((noSuchQueuedMerge . ())))))
    (agent-repl-itest-verbs--with-workspace daemon ref
      (let (messages)
        (cl-letf (((symbol-function 'message)
                   (lambda (fmt &rest args) (push (apply #'format fmt args) messages))))
          ;; Act.
          (agent-repl-verb-merge-queue (list :arm :evict :workspace ref))
          (agent-repl-itest--await-call daemon "UpdateMergeQueue")
          (agent-repl-itest--await-log daemon "elisp.verbs.merge-queue-refused" "warn")
          ;; Assert.
          (should (agent-repl-itest--logged-p
                   daemon "elisp.verbs.merge-queue-refused" "warn"))
          (should (seq-some
                   (lambda (m) (string-match-p
                               "merge-queue refused: no-such-queued-merge" m))
                   messages)))))))

(provide 'test-integration-verbs)

;;; test-integration-verbs.el ends here
