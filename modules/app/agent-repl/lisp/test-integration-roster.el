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
(declare-function agent-repl-connect-open "connect")
(declare-function agent-repl-connect-close "connect")
(declare-function agent-repl--ws-by-ref-id "workspace")
(declare-function agent-repl-roster-tab-order "roster")
(declare-function agent-repl--ws-put "workspace")
(declare-function agent-repl--ws-switch "workspace")
(declare-function agent-repl--ws-dir-owner "workspace")
(defvar agent-repl-roster-view)
(defvar agent-repl-roster-update-functions)
(defvar agent-repl-roster-finish-functions)
(defvar agent-repl-host-last-selected-id)

;;;; ---- Fixtures ----

(defconst agent-repl-itest-roster--repo-dir "/tmp/itest-roster-repo"
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
                                        (dir . ,(concat "/tmp/itest-roster-" id))))))
            (name . ((text . ,name)))
            (,status . ())
            (current . ((current . :false)))
            (when . ())
            (detail . ())
            (closed . ((closed . :false))))))

(defun agent-repl-itest-roster--roster (rows &rest overrides)
  "Return a WorkspaceRoster protojson alist carrying ROWS in one repo section.
OVERRIDES replaces top-level entries (notably `current')."
  (append
   overrides
   `((repository
      . ((sections
          . [((key . ((repository . ((id . "repo-itest")
                                     (dir . ,agent-repl-itest-roster--repo-dir)))))
              (header . ((label . ((text . "itest-repo")))))
              (rows . ((rows . ,(vconcat rows)))))])))
     (task . ((sections . [])))
     (recentlyMerged . ((header . ((label . ((text . "recently merged")))))
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

;;;; ---- Scenario 9: every status arm ----

(defconst agent-repl-itest-roster--status-arms
  '((submitting . :submitting)
    (thinking . :thinking)
    (clearing . :clearing)
    (compacting . :compacting)
    (permission . :permission)
    (done . :done)
    (interrupted . :interrupted)
    (ready . :ready)
    (idleAsync . :idle-async)
    (vendorBlocked . :vendor-blocked)
    (init . :init)
    (severed . :severed)
    (startFailed . :start-failed)
    (degraded . :degraded)
    (dead . :dead)
    (mergeEnqueuing . :merge-enqueuing)
    (merging . :merging)
    (mergeQueued . :merge-queued)
    (mergeConflict . :merge-conflict)
    (mergeFailed . :merge-failed)
    (merged . :merged)
    (none . :none)
    (inactive . :inactive))
  "Every RosterRow.status arm frontend/v1/sidebar.proto declares, and the
keyword §8 pins for it.  The list is EXHAUSTIVE by contract: 23 arms, and
the roster's vocabulary is the ONE source for tab coloring and the sidebar
dot.  A 24th arm appearing on the wire must be a loud failure, not a
silent default, which is why the suite pins the count as well as the
mapping.")

(ert-deftest agent-repl-itest-roster-declares-every-status-arm ()
  "The suite's arm table matches the contract's arm count exactly.
A drifted table would let a new arm ship untested, and the coloring would
silently fall through to `none'."
  ;; Arrange / Act / Assert.
  (should (equal 23 (length agent-repl-itest-roster--status-arms))))

(ert-deftest agent-repl-itest-roster-every-status-arm-decodes-to-its-keyword ()
  "Each of the 23 status arms resolves to exactly one tab-state keyword.
The roster's per-row state vocabulary is the ONE source for tab coloring;
there is no HostWorkspace lifecycle axis to fall back on."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest-roster--with-subscription daemon
      (dolist (case agent-repl-itest-roster--status-arms)
        (let* ((arm (car case))
               (expected (cdr case))
               (ws-name (format "itest-roster-%s" arm)))
          (agent-repl--ws-put ws-name :project-dir (format "/tmp/%s" ws-name))
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
               (list `((workspace . ((workspace . ((id . "ws-x") (dir . "/tmp/ws-x")))))
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
               (list `((workspace . ((workspace . ((id . "ws-x") (dir . "/tmp/ws-x")))))
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
      (let ((blinked nil))
        (agent-repl--ws-put "itest-attn" :project-dir "/tmp/itest-attn")
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
        (agent-repl--ws-put "itest-cur" :project-dir "/tmp/itest-cur")
        (cl-letf (((symbol-function 'agent-repl--ws-switch)
                   (lambda (ws &rest _) (push ws switched))))
          ;; Act.
          (agent-repl-itest-roster--push
           daemon (agent-repl-itest-roster--roster
                   (list (agent-repl-itest-roster--row
                          "itest-cur" "itest-cur" 'ready
                          '(current . ((current . t)))))
                   '(current . ((workspace . ((id . "itest-cur")
                                              (dir . "/tmp/itest-cur")))))))
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
        (agent-repl--ws-put "itest-own" :project-dir "/tmp/itest-own")
        (cl-letf (((symbol-function 'agent-repl--ws-switch)
                   (lambda (ws &rest _) (push ws switched))))
          ;; Act: the roster echoes back the selection Emacs made.
          (agent-repl-itest-roster--push
           daemon (agent-repl-itest-roster--roster
                   (list (agent-repl-itest-roster--row
                          "itest-own" "itest-own" 'ready
                          '(current . ((current . t)))))
                   '(current . ((workspace . ((id . "itest-own")
                                              (dir . "/tmp/itest-own")))))))
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

;;;; ---- Scenario 11: the finish edge ----

(ert-deftest agent-repl-itest-roster-running-to-settled-fires-the-finish-edge ()
  "thinking → done is THE FINISH EDGE, and it fires once.
All four Emacs-local reactions ride this transition: the unfocused
banner, the cross-workspace echo, the magit refresh and the deferred
drain."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest-roster--with-subscription daemon
      (let ((finished nil))
        (agent-repl--ws-put "itest-fin" :project-dir "/tmp/itest-fin")
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
        (agent-repl--ws-put "itest-fin1" :project-dir "/tmp/itest-fin1")
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
        (agent-repl--ws-put "itest-fin2" :project-dir "/tmp/itest-fin2")
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
        (agent-repl--ws-put "itest-fin3" :project-dir "/tmp/itest-fin3")
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

(ert-deftest agent-repl-itest-roster-logs-the-roster-stream-open ()
  "The roster subscription is logged through the canonical ladder."
  ;; Arrange / Act.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest-roster--with-subscription daemon
      ;; Assert.
      (agent-repl-itest--await-log daemon "elisp.rpc.stream-open" "info")
      (should (agent-repl-itest--logged-p daemon "elisp.rpc.stream-open" "info")))))

(provide 'test-integration-roster)

;;; test-integration-roster.el ends here
