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

;;;; ---- Fixtures ----

(defconst agent-repl-itest-verbs--ws "itest-verbs-ws"
  "The Doom workspace name every verb in this suite targets.")

(defconst agent-repl-itest-verbs--dir "/tmp/itest-verbs-ws"
  "The workspace directory registered for that workspace.")

(defconst agent-repl-itest-verbs--repo
  '(:id "repo-itest" :dir "/tmp/itest-verbs-repo")
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

(ert-deftest agent-repl-itest-verbs-restart-sends-force-false-explicitly ()
  "A graceful restart sends `force' false, not an absent field.
`force' is a plain bool, so false is a VALUE the daemon must receive; the
daemon owns everything the restart entails, webapp bounce included."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest-verbs--with-workspace daemon ref
      (ignore ref)
      ;; Act.
      (agent-repl-verb-restart agent-repl-itest-verbs--ws nil)
      (agent-repl-itest--await-call daemon "RestartWorkspace")
      ;; Assert: protojson omits a false bool, so its absence IS false — and
      ;; the assertion is that no `true' was sent.
      (should-not (eq (agent-repl-itest--body-field
                       (agent-repl-itest-verbs--body daemon "RestartWorkspace")
                       'force)
                      t)))))

(ert-deftest agent-repl-itest-verbs-restart-force-sends-force-true ()
  "A forced restart sends `force' true: interrupt and bounce.
The agent is NOT resumed afterwards — continuing is the user's next
prompt."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest-verbs--with-workspace daemon ref
      (ignore ref)
      ;; Act.
      (agent-repl-verb-restart agent-repl-itest-verbs--ws t)
      (agent-repl-itest--await-call daemon "RestartWorkspace")
      ;; Assert.
      (should (eq (agent-repl-itest--body-field
                   (agent-repl-itest-verbs--body daemon "RestartWorkspace")
                   'force)
                  t)))))

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
      (agent-repl-verb-create agent-repl-itest-verbs--repo :standard
                              :initial-prompt "fix the flake")
      (agent-repl-itest--await-call daemon "CreateWorkspace")
      ;; Assert.
      (let* ((body (agent-repl-itest-verbs--body daemon "CreateWorkspace"))
             (blocks (agent-repl-itest--body-field
                      body 'standard 'initialPrompt 'content 'blocks)))
        (should (equal (agent-repl-itest--body-field (car blocks) 'text 'text)
                       "fix the flake"))))))

(ert-deftest agent-repl-itest-verbs-create-one-shot-self-merge-sends-its-finish ()
  "The one-shot form's `self_merge' finish arm rides the request.
ONE-SHOTS ride the wire now: Emacs supplies {prompt, model, parentage}
and the DAEMON owns naming, worktree, decoration and postprocessing."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest-verbs--with-workspace daemon ref
      (ignore ref)
      ;; Act.
      (agent-repl-verb-create agent-repl-itest-verbs--repo :one-shot
                              :prompt "land the fix"
                              :finish :self-merge)
      (agent-repl-itest--await-call daemon "CreateWorkspace")
      ;; Assert.
      (let ((body (agent-repl-itest-verbs--body daemon "CreateWorkspace")))
        ;; `self_merge' is an EMPTY message: present and `{}', which parses
        ;; back to nil, so presence is the whole assertion.
        (should (assq 'selfMerge (agent-repl-itest--body-field body 'oneShot)))))))

(ert-deftest agent-repl-itest-verbs-create-one-shot-open-pr-sends-its-flags ()
  "The `open_pr' finish arm carries both of its bools explicitly.
`self_certified' and `add_to_merge_queue' are plain bools: false is a
value the daemon must receive, not an absence."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest-verbs--with-workspace daemon ref
      (ignore ref)
      ;; Act.
      (agent-repl-verb-create agent-repl-itest-verbs--repo :one-shot
                              :prompt "land the fix"
                              :finish :open-pr
                              :self-certified t
                              :add-to-merge-queue t)
      (agent-repl-itest--await-call daemon "CreateWorkspace")
      ;; Assert.
      (let ((body (agent-repl-itest-verbs--body daemon "CreateWorkspace")))
        (should (eq (agent-repl-itest--body-field
                     body 'oneShot 'openPr 'selfCertified)
                    t))
        (should (eq (agent-repl-itest--body-field
                     body 'oneShot 'openPr 'addToMergeQueue)
                    t))))))

(ert-deftest agent-repl-itest-verbs-create-parent-carries-the-parents-ref ()
  "A child workspace names its parent by ref.
Parentage is a creation fact the daemon derives everything else from; the
ref is echoed, never rebuilt from a path."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest-verbs--with-workspace daemon ref
      ;; Act.
      (agent-repl-verb-create agent-repl-itest-verbs--repo :standard :parent ref)
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
      (agent-repl-verb-create agent-repl-itest-verbs--repo :standard :parent ref)
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
      (agent-repl-verb-create agent-repl-itest-verbs--repo :standard :priority :p1)
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
       (list :arm :schedule :at-ms 1735689600000 :reason '(:arm :deploy)))
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
      (agent-repl-verb-shutdown-schedule '(:arm :cancel))
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
       (list :arm :now :reason '(:arm :operator :note "emacs")))
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
      (agent-repl-verb-merge-queue '(:arm :pause))
      (agent-repl-itest--await-call daemon "UpdateMergeQueue")
      ;; Assert.
      (should (assq 'pause (agent-repl-itest-verbs--body daemon "UpdateMergeQueue"))))))

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
      ;; Act.
      (agent-repl-daemon-health)
      (agent-repl-itest--await-call daemon "DaemonHealth")
      ;; Assert.
      (agent-repl-itest--wait-until
       (lambda () (get-buffer "*agent-repl-health*"))
       nil "the health buffer")
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

(provide 'test-integration-verbs)

;;; test-integration-verbs.el ends here
