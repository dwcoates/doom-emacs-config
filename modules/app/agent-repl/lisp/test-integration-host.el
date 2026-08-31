;;; test-integration-host.el --- Integration: host.el against the fake daemon -*- lexical-binding: t; -*-

;;; Commentary:

;; Scenarios 3, 4, 5 (host half), 7 and 15 of elisp-fanout.md §14.
;;
;; 3.  The host stream: snapshot then whole-replace pushes; every composer arm
;;     resolving to its gate value; naming resolving to the display title;
;;     faults reaching the health surface; shim_attached false having NO
;;     treatment at all (a parked workspace is invisible by design).
;; 4.  The notification policy, which is EMACS'S — the daemon publishes the
;;     fact and never asks whether Emacs is focused.
;; 5.  The host half of a handover: `transferred' → AdoptHostWorkspace on the
;;     NEW connection, then the old stream cancelled, then re-subscribe.
;; 7.  `reload_webapp' reloading exactly one workspace's webview.
;; 15. A push missing a non-optional field: ERROR, dropped, stream standing.

;;; Code:

(require 'ert)
(require 'cl-lib)

(load (expand-file-name "test-integration-helpers.el"
                        (file-name-directory (or load-file-name buffer-file-name)))
      nil t)

;; Production names this suite depends on (host.el, §7; status.el, §8;
;; frontend.el, §12; popup.el, §12; daemon-link.el, §6).
(declare-function agent-repl-host-register "host")
(declare-function agent-repl-host-select "host")
(declare-function agent-repl-host-subscribe "host")
(declare-function agent-repl-host-unsubscribe "host")
(declare-function agent-repl-host-forget "host")
(declare-function agent-repl-host-ref "host")
(declare-function agent-repl-host-conn "host")
(declare-function agent-repl-host-state "host")
(declare-function agent-repl-host-backfill "host")
(declare-function agent-repl-host-faults "host")
(declare-function agent-repl-host-composer-gate "host")
(declare-function agent-repl-host-display-title "host")
(declare-function agent-repl-connect-open "connect")
(declare-function agent-repl-connect-close "connect")
(declare-function agent-repl-status-blink-tab "status")
(declare-function agent-repl-frontend-reload-webview "frontend")
(declare-function agent-repl-popup-open "popup")
(declare-function agent-repl-link-successor "daemon-link")
(declare-function agent-repl--ws-put "workspace")
(declare-function agent-repl--notify "notifications")
(declare-function agent-repl--ws-switch "workspace")
(declare-function agent-repl--ws-current-name "workspace")
(declare-function agent-repl--emacs-focused-p "notifications")
(declare-function agent-repl-link-connect "daemon-link")
(declare-function agent-repl-link-teardown "daemon-link")
(declare-function agent-repl-wire-decode-watch-host-workspace-response "wire-host")
(declare-function agent-repl-status--set-marker "status")
(declare-function agent-repl-session-health "verbs")
(declare-function agent-repl--frontend-precreate-webview "frontend")
(declare-function agent-repl--frontend-xwidget-available-p "frontend")
(declare-function agent-repl--call-in-background-workspace "agent-repl-worktree")
(defvar agent-repl-host-update-functions)
(defvar agent-repl-host-last-selected-id)
(defvar agent-repl-link-up-functions)
(defvar agent-repl-link-down-functions)
(defvar agent-repl-link-handover-functions)
(defvar agent-repl-link-drain-functions)
(defvar agent-repl-link-no-daemon-functions)
(defvar agent-repl-link-drain)
(defvar agent-repl-link-drain-segment)
(defvar agent-repl-verbs-health-buffer)
(defvar agent-repl-itest-webview-urls)
(defvar dired-directory)

(require 'url-util)

;;;; ---- Fixtures ----

(defconst agent-repl-itest-host--ws "itest-host-ws"
  "The Doom workspace name this suite's host state is keyed by.")

(defun agent-repl-itest-host--live (composer &rest overrides)
  "Return a HostWorkspace protojson alist whose session is LIVE.
COMPOSER is the composer arm's protojson key symbol (`open', `merging',
`draining', `restarting', `mergeParked').  OVERRIDES replaces entries of
the live arm, so one test changes exactly one fact.

Every non-optional field is populated: an unset one is ILLEGAL on this
contract, so a fixture missing one would test the breach path by
accident."
  (let ((live (append overrides
                      `((generation . ((value . "gen-1")))
                        (shimAttached . t)
                        (claude . ((sessionId . "vendor-1")
                                   (configDir . "/home/itest/.claude")))
                        (backfill . ((done . ())))
                        (,composer . ())))))
    `((existing . ((id . ((value . "host-session-1")))
                   (live . ,live)))
      (naming . ()))))

(defun agent-repl-itest-host--push-host (daemon workspace-id host &optional snapshot)
  "Push HOST (a HostWorkspace alist) on DAEMON's host stream for WORKSPACE-ID."
  (agent-repl-itest--push daemon "host" `((host . ,host)) workspace-id snapshot))

(defmacro agent-repl-itest-host--with-subscription (daemon ref &rest body)
  "Register and subscribe the suite's workspace on DAEMON, then run BODY.
REF is bound to the WorkspaceRef the daemon minted.  The subscription is
always cancelled afterwards — a client cancel IS the graceful close."
  (declare (indent 2) (debug (form symbolp body)))
  `(let ((conn (agent-repl-connect-open (agent-repl-itest-daemon-address ,daemon))))
     (unwind-protect
         (let ((,ref nil))
           (agent-repl--ws-put agent-repl-itest-host--ws :project-dir "/tmp/itest-host-ws")
           (agent-repl-host-register
            conn "/tmp/itest-host-ws"
            (lambda (minted) (setq ,ref minted)))
           (agent-repl-itest--wait-until (lambda () ,ref) nil
                                         "RegisterWorkspace to answer")
           (agent-repl-host-subscribe conn agent-repl-itest-host--ws ,ref)
           (agent-repl-itest--await-subscriber
            ,daemon "host" (plist-get ,ref :id))
           ,@body)
       ;; FORGET, not merely unsubscribe: host state is keyed by workspace
       ;; name and outlives a subscription by design, so leaving it behind
       ;; would let one scenario's pushed state answer the next scenario's
       ;; accessors -- which is exactly what a `no push yet' assertion means.
       (ignore-errors (agent-repl-host-forget agent-repl-itest-host--ws))
       (agent-repl-connect-close conn))))

(defun agent-repl-itest-host--push-invalid-context-strings (daemon)
  "Return every logged argument string from DAEMON's `elisp.rpc.push-invalid'
entries.  `agent-repl-rpc--stream' logs the offending push's RAW parsed JSON
alist in its context (fanout §4: \"with the raw JSON in context\"); the
harness records that as `prin1-to-string' of each format argument under
`context.arguments' (core.el's `agent-repl--log-record')."
  (apply #'append
         (mapcar (lambda (record) (alist-get 'arguments (alist-get 'context record)))
                 (agent-repl-itest--log-entries daemon "elisp.rpc.push-invalid" "error"))))

(defun agent-repl-itest-host--dired-buffer-for (dir)
  "Return a live dired buffer visiting DIR, or nil.
Used to observe `agent-repl-popup-open' UN-STUBBED: the shared subroutine
opens a directory with `dired-noselect', which this walks the buffer list
to find rather than assuming any particular buffer name."
  (seq-find (lambda (buf)
              (with-current-buffer buf
                (and (eq major-mode 'dired-mode)
                     (equal (file-name-as-directory (expand-file-name dired-directory))
                            (file-name-as-directory (expand-file-name dir))))))
            (buffer-list)))

;;;; ---- Scenario 3: the host stream ----

(ert-deftest agent-repl-itest-host-subscribes-once-per-open-workspace ()
  "One WatchHostWorkspace subscription exists per OPEN workspace.
elisp.md: there is no global host-workspace stream; the per-workspace
channel is a distinct type by ruling, which is why WatchDaemon exists
separately."
  ;; Arrange / Act.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest-host--with-subscription daemon ref
      ;; Assert.
      (should (equal 1 (length (agent-repl-itest--subscribers
                                daemon "host" (plist-get ref :id))))))))

(ert-deftest agent-repl-itest-host-echoes-the-registered-ref-on-subscribe ()
  "The subscription echoes the ref the daemon minted, verbatim.
A path is never an identity; the ref comes from RegisterWorkspace's
success and is handed back unchanged."
  ;; Arrange / Act.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest-host--with-subscription daemon ref
      (let ((body (car (agent-repl-itest--call-bodies daemon "WatchHostWorkspace"))))
        ;; Assert.
        (should (equal (agent-repl-itest--body-field body 'workspace 'id)
                       (plist-get ref :id)))
        (should (equal (agent-repl-itest--body-field body 'workspace 'dir)
                       (plist-get ref :dir)))))))

(ert-deftest agent-repl-itest-host-replays-the-snapshot-on-subscribe ()
  "A subscription receives the snapshot FIRST, then pushes.
Streams are \"now\", never \"since\": there is no resume token, so the
snapshot is how a fresh subscription learns the standing state."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (let* ((conn (agent-repl-connect-open (agent-repl-itest-daemon-address daemon)))
           (ref nil))
      (unwind-protect
          (progn
            (agent-repl--ws-put agent-repl-itest-host--ws :project-dir "/tmp/itest-host-ws")
            (agent-repl-host-register conn "/tmp/itest-host-ws"
                                      (lambda (minted) (setq ref minted)))
            (agent-repl-itest--wait-until (lambda () ref) nil "RegisterWorkspace to answer")
            ;; The standing state is stored BEFORE anyone subscribes.
            (agent-repl-itest-host--push-host
             daemon (plist-get ref :id) (agent-repl-itest-host--live 'open) t)
            ;; Act.
            (agent-repl-host-subscribe conn agent-repl-itest-host--ws ref)
            ;; Assert.
            (agent-repl-itest--wait-until
             (lambda () (agent-repl-host-state agent-repl-itest-host--ws))
             nil "the snapshot to reach host state")
            (should (agent-repl-host-state agent-repl-itest-host--ws)))
        (ignore-errors (agent-repl-host-unsubscribe agent-repl-itest-host--ws))
        (agent-repl-connect-close conn)))))

(ert-deftest agent-repl-itest-host-push-whole-replaces-the-state ()
  "Each push replaces the host state WHOLE; nothing is merged.
PUSH CADENCE: event-driven, whole-view, no ticks — a consumer that
merged would keep a fact the daemon has retracted."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest-host--with-subscription daemon ref
      (let ((id (plist-get ref :id)))
        (agent-repl-itest-host--push-host daemon id (agent-repl-itest-host--live 'open))
        (agent-repl-itest--wait-until
         (lambda () (eq (agent-repl-host-composer-gate agent-repl-itest-host--ws) :open))
         nil "the first push's gate")
        ;; Act: a second, whole push carrying a different composer arm.
        (agent-repl-itest-host--push-host daemon id (agent-repl-itest-host--live 'merging))
        ;; Assert.
        (agent-repl-itest--wait-until
         (lambda () (eq (agent-repl-host-composer-gate agent-repl-itest-host--ws) :merging))
         nil "the second push's gate")
        (should (eq (agent-repl-host-composer-gate agent-repl-itest-host--ws) :merging))))))

(ert-deftest agent-repl-itest-host-every-composer-arm-resolves-to-its-gate ()
  "Each composer arm resolves to exactly one gate value.
The RESOLVED ARM IS THE GATE (elisp.md): Emacs renders a fixed treatment
per arm and never maps values, which is why the old composed \"gate
sentence\" died."
  ;; Arrange: the five arms the LIVE standing declares.
  (let ((cases '((open . :open)
                 (merging . :merging)
                 (draining . :draining)
                 (restarting . :restarting)
                 (mergeParked . :merge-parked))))
    (agent-repl-itest--with-fake-daemon daemon
      (agent-repl-itest-host--with-subscription daemon ref
        (dolist (case cases)
          (let ((arm (car case))
                (expected (cdr case)))
            ;; Act.
            (agent-repl-itest-host--push-host
             daemon (plist-get ref :id) (agent-repl-itest-host--live arm))
            ;; Assert.
            (agent-repl-itest--wait-until
             (lambda () (eq (agent-repl-host-composer-gate agent-repl-itest-host--ws) expected))
             nil (format "the %s arm to resolve to %s" arm expected))
            (should (eq (agent-repl-host-composer-gate agent-repl-itest-host--ws)
                        expected))))))))

(ert-deftest agent-repl-itest-host-session-none-gates-as-no-session ()
  "A registered workspace with no session ever created gates `:no-session'.
The gate exists only on the LIVE arm; the other standings are blocked by
their own nature, and the daemon starts or revives implicitly on submit."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest-host--with-subscription daemon ref
      ;; Act.
      (agent-repl-itest-host--push-host
       daemon (plist-get ref :id) '((none . ()) (naming . ())))
      ;; Assert.
      (agent-repl-itest--wait-until
       (lambda () (eq (agent-repl-host-composer-gate agent-repl-itest-host--ws) :no-session))
       nil "the none arm to resolve to :no-session")
      (should (eq (agent-repl-host-composer-gate agent-repl-itest-host--ws) :no-session)))))

(ert-deftest agent-repl-itest-host-terminal-standing-gates-as-terminal ()
  "A terminal session gates `:terminal'.
HostSessionTerminal carries only `rehydratable'; the composer's own
treatment for this gate is the composer suite's business."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest-host--with-subscription daemon ref
      ;; Act.
      (agent-repl-itest-host--push-host
       daemon (plist-get ref :id)
       '((existing . ((id . ((value . "host-session-1")))
                      (terminal . ((rehydratable . t)))))
         (naming . ())))
      ;; Assert.
      (agent-repl-itest--wait-until
       (lambda () (eq (agent-repl-host-composer-gate agent-repl-itest-host--ws) :terminal))
       nil "the terminal arm to resolve to :terminal")
      (should (eq (agent-repl-host-composer-gate agent-repl-itest-host--ws) :terminal)))))

(ert-deftest agent-repl-itest-host-gate-is-unknown-before-any-push ()
  "Before the first host push the gate is `:unknown', not `:open'.
No push yet is a distinct fact from an open composer; §7 has the composer
SEND under it while logging INFO, because the daemon is the authority."
  ;; Arrange / Act.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest-host--with-subscription daemon ref
      ;; Assert: subscribed, but nothing pushed yet.
      (should ref)
      (should (eq (agent-repl-host-composer-gate agent-repl-itest-host--ws) :unknown)))))

(ert-deftest agent-repl-itest-host-shim-detached-keeps-the-gate-open ()
  "`shim_attached' false has NO treatment: a parked workspace looks live.
Hibernation does not exist on the wire — the frontend cannot distinguish
parked from idle, on purpose, and typing revives under the hood."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest-host--with-subscription daemon ref
      ;; Act.
      (agent-repl-itest-host--push-host
       daemon (plist-get ref :id)
       (agent-repl-itest-host--live 'open '(shimAttached . :false)))
      ;; Assert.
      (agent-repl-itest--wait-until
       (lambda () (eq (agent-repl-host-composer-gate agent-repl-itest-host--ws) :open))
       nil "a detached shim to still gate :open")
      (should (eq (agent-repl-host-composer-gate agent-repl-itest-host--ws) :open)))))

(ert-deftest agent-repl-itest-host-naming-title-becomes-the-display-title ()
  "`naming.title' names the workspace's buffers when it is present.
Naming sits BESIDE the session oneof because buffers need a name in every
standing; both fields are unset until derived."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest-host--with-subscription daemon ref
      ;; Act.
      (agent-repl-itest-host--push-host
       daemon (plist-get ref :id)
       `((none . ()) (naming . ((slug . "itest-slug") (title . "Itest Title")))))
      ;; Assert.
      (agent-repl-itest--wait-until
       (lambda () (equal (agent-repl-host-display-title agent-repl-itest-host--ws)
                         "Itest Title"))
       nil "naming.title to become the display title")
      (should (equal (agent-repl-host-display-title agent-repl-itest-host--ws)
                     "Itest Title")))))

(ert-deftest agent-repl-itest-host-naming-slug-is-the-title-fallback ()
  "With no title, `naming.slug' names the buffers.
Absence is expressed by field presence, never by an empty string, so an
unset title genuinely means \"not derived yet\"."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest-host--with-subscription daemon ref
      ;; Act.
      (agent-repl-itest-host--push-host
       daemon (plist-get ref :id)
       `((none . ()) (naming . ((slug . "itest-slug")))))
      ;; Assert.
      (agent-repl-itest--wait-until
       (lambda () (equal (agent-repl-host-display-title agent-repl-itest-host--ws)
                         "itest-slug"))
       nil "naming.slug to become the display title")
      (should (equal (agent-repl-host-display-title agent-repl-itest-host--ws)
                     "itest-slug")))))

(ert-deftest agent-repl-itest-host-standing-faults-are-exposed ()
  "Generation-scoped `faults' are exposed for doctor output.
HostFault carries a dynamic `detail' plus `opened_at_ms'; fault windows
scope to the generation, which is why the generation rides the live arm."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest-host--with-subscription daemon ref
      ;; Act.
      (agent-repl-itest-host--push-host
       daemon (plist-get ref :id)
       (agent-repl-itest-host--live
        'open '(faults . [((detail . "store socket unreachable")
                           (openedAtMs . "1735689600000"))])))
      ;; Assert.
      (agent-repl-itest--wait-until
       (lambda () (agent-repl-host-faults agent-repl-itest-host--ws))
       nil "the standing fault to reach host state")
      (should (agent-repl-host-faults agent-repl-itest-host--ws)))))

(ert-deftest agent-repl-itest-host-backfill-failed-arm-is-exposed ()
  "The `backfill' arm is exposed as the never-blue signal it is.
Its four arms are none | pending | done | failed{detail}; the comment's
known gap is that a non-parse sidecar read error still shows as pending."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest-host--with-subscription daemon ref
      ;; Act.
      (agent-repl-itest-host--push-host
       daemon (plist-get ref :id)
       (agent-repl-itest-host--live
        'open '(backfill . ((failed . ((detail . "sidecar parse error")))))))
      ;; Assert.
      (agent-repl-itest--wait-until
       (lambda () (eq (agent-repl-host-backfill agent-repl-itest-host--ws) :failed))
       nil "the failed backfill arm")
      (should (eq (agent-repl-host-backfill agent-repl-itest-host--ws) :failed)))))

(ert-deftest agent-repl-itest-host-every-backfill-arm-resolves-to-its-keyword ()
  "Each of `backfill''s four arms exposes its own keyword, not just `failed'.
proto `HostBackfill': \"THE ARM IS THE STATE\" — none | pending | done |
failed{detail} are four distinct facts and a suite that only ever drives
`failed' cannot tell a decoder collapsing them apart from one that keeps
them straight."
  ;; Arrange: the four arms the proto declares.
  (let ((cases '((none . :none) (pending . :pending) (done . :done)
                 (failed . :failed))))
    (agent-repl-itest--with-fake-daemon daemon
      (agent-repl-itest-host--with-subscription daemon ref
        (dolist (case cases)
          (let* ((arm (car case))
                 (expected (cdr case))
                 (payload (if (eq arm 'failed) '((detail . "sidecar parse error")) '())))
            ;; Act.
            (agent-repl-itest-host--push-host
             daemon (plist-get ref :id)
             (agent-repl-itest-host--live 'open `(backfill . ((,arm . ,payload)))))
            ;; Assert.
            (agent-repl-itest--wait-until
             (lambda () (eq (agent-repl-host-backfill agent-repl-itest-host--ws) expected))
             nil (format "the %s backfill arm" arm))
            (should (eq (agent-repl-host-backfill agent-repl-itest-host--ws) expected))))))))

(ert-deftest agent-repl-itest-host-live-with-no-vendor-conversation-is-accepted ()
  "A LIVE session with `vendor_info' entirely unset is a legal push, not a breach.
proto `HostSessionLive.vendor_info': \"Unset while no vendor conversation
exists yet.\"  A decoder treating this oneof as required would refuse every
freshly-started session's very first host push."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest-host--with-subscription daemon ref
      ;; Act: a live push with no `claude' arm (or any other vendor arm) at all.
      (agent-repl-itest-host--push-host
       daemon (plist-get ref :id)
       `((existing . ((id . ((value . "host-session-1")))
                      (live . ((generation . ((value . "gen-1")))
                               (shimAttached . t)
                               (backfill . ((done . ())))
                               (open . ())))))
         (naming . ())))
      ;; Assert: the gate resolves normally — this is an accepted push.
      (agent-repl-itest--wait-until
       (lambda () (eq (agent-repl-host-composer-gate agent-repl-itest-host--ws) :open))
       nil "a vendorless live push to still gate :open")
      (should (eq (agent-repl-host-composer-gate agent-repl-itest-host--ws) :open))
      (should-not (agent-repl-itest--logged-p daemon "elisp.rpc.push-invalid" "error")))))

(ert-deftest agent-repl-itest-host-naming-absent-falls-back-to-the-row-name ()
  "With `naming' entirely empty, buffers are named from the roster row name.
fanout §7: \"buffer titles use `naming.title', else `naming.slug', else the
row name\" — `agent-repl-itest-host--ws' stands in for that row name here,
since the suite has no separate roster row of its own."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest-host--with-subscription daemon ref
      ;; Act: the fixture's `naming' is already the empty message.
      (agent-repl-itest-host--push-host
       daemon (plist-get ref :id) (agent-repl-itest-host--live 'open))
      ;; Assert.
      (agent-repl-itest--wait-until
       (lambda () (equal (agent-repl-host-display-title agent-repl-itest-host--ws)
                         agent-repl-itest-host--ws))
       nil "absent naming to fall back to the row name")
      (should (equal (agent-repl-host-display-title agent-repl-itest-host--ws)
                     agent-repl-itest-host--ws)))))

(ert-deftest agent-repl-itest-host-standing-faults-reach-the-health-buffer ()
  "`agent-repl-session-health' prints the host stream's standing fault detail.
fanout §14 scenario 3 / verbs.el §9: SessionHealth renders \"the host
stream's standing faults for the workspace\" beside the pulled verdict — a
doctor reading only the accessor, never the rendered buffer, would miss a
regression that drops the fault from the printed report."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest-host--with-subscription daemon ref
      (agent-repl-itest--script daemon "SessionHealth" '((success . ((healthy . ())))))
      (agent-repl-itest-host--push-host
       daemon (plist-get ref :id)
       (agent-repl-itest-host--live
        'open '(faults . [((detail . "store socket unreachable")
                           (openedAtMs . "1735689600000"))])))
      (agent-repl-itest--wait-until
       (lambda () (agent-repl-host-faults agent-repl-itest-host--ws))
       nil "the standing fault to reach host state")
      (when (get-buffer agent-repl-verbs-health-buffer) (kill-buffer agent-repl-verbs-health-buffer))
      ;; Act.
      (agent-repl-session-health agent-repl-itest-host--ws)
      ;; Assert.
      (agent-repl-itest--wait-until
       (lambda ()
         (and (get-buffer agent-repl-verbs-health-buffer)
              (with-current-buffer agent-repl-verbs-health-buffer
                (string-match-p "store socket unreachable" (buffer-string)))))
       nil "the standing fault's detail to reach the health buffer")
      (should (with-current-buffer agent-repl-verbs-health-buffer
                (string-match-p "store socket unreachable" (buffer-string)))))))

(defun agent-repl-itest-host--composer-arm-of (host)
  "Return HOST's live composer arm keyword, or nil.
Mirrors host.el's own private walk (`agent-repl-host--live') so the hook
test below can inspect the exact plist the hook was called with, rather
than the accessor's CURRENT (possibly later) state."
  (let ((session (plist-get host :session)))
    (when (eq (plist-get session :arm) :existing)
      (let ((standing (plist-get (plist-get session :value) :standing)))
        (when (eq (plist-get standing :arm) :live)
          (plist-get (plist-get (plist-get standing :value) :composer) :arm))))))

(ert-deftest agent-repl-itest-host-update-functions-runs-after-every-host-push ()
  "`agent-repl-host-update-functions' runs with (WS HOST-PLIST) after each push.
fanout §7: \"`agent-repl-host-update-functions' (WS HOST-PLIST) runs after
every host push.\""
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest-host--with-subscription daemon ref
      (let ((calls nil)
            (agent-repl-host-update-functions nil))
        (add-hook 'agent-repl-host-update-functions
                  (lambda (ws host) (push (cons ws host) calls)))
        ;; Act: two whole pushes, carrying different composer arms.
        (agent-repl-itest-host--push-host
         daemon (plist-get ref :id) (agent-repl-itest-host--live 'open))
        (agent-repl-itest--wait-until (lambda () calls) nil "the first hook invocation")
        (agent-repl-itest-host--push-host
         daemon (plist-get ref :id) (agent-repl-itest-host--live 'merging))
        (agent-repl-itest--wait-until (lambda () (= 2 (length calls))) nil
                                      "the second hook invocation")
        ;; Assert: two invocations, WS on both, carrying the decoded plists
        ;; in push order.
        (should (= 2 (length calls)))
        (let ((ordered (reverse calls)))
          (should (equal (mapcar #'car ordered)
                         (list agent-repl-itest-host--ws agent-repl-itest-host--ws)))
          (should (eq (agent-repl-itest-host--composer-arm-of (cdr (nth 0 ordered))) :open))
          (should (eq (agent-repl-itest-host--composer-arm-of (cdr (nth 1 ordered))) :merging)))))))

(ert-deftest agent-repl-itest-host-unsubscribe-cancels-the-daemon-side-subscriber ()
  "`(agent-repl-host-unsubscribe WS)' empties the daemon's subscriber list.
fanout §7: \"one WatchHostWorkspace per open workspace; unsubscribe
cancels.\""
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest-host--with-subscription daemon ref
      (should (equal 1 (length (agent-repl-itest--subscribers
                                daemon "host" (plist-get ref :id)))))
      ;; Act.
      (agent-repl-host-unsubscribe agent-repl-itest-host--ws)
      ;; Assert.
      (agent-repl-itest--wait-until
       (lambda () (null (agent-repl-itest--subscribers daemon "host" (plist-get ref :id))))
       nil "the daemon to observe the unsubscribe")
      (should (null (agent-repl-itest--subscribers daemon "host" (plist-get ref :id)))))))

(ert-deftest agent-repl-itest-host-register-error-arm-answers-on-done-with-nil ()
  "RegisterWorkspace's error arm calls ON-DONE with nil and logs ERROR.
fanout §7: \"error arm → `agent-repl--error' and ON-DONE nil.\""
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest--script daemon "RegisterWorkspace" '((error . ())))
    (let ((conn (agent-repl-connect-open (agent-repl-itest-daemon-address daemon)))
          (done-called nil)
          (result :never))
      (unwind-protect
          (progn
            (agent-repl--ws-put agent-repl-itest-host--ws :project-dir "/tmp/itest-host-ws")
            ;; Act.
            (agent-repl-host-register
             conn "/tmp/itest-host-ws"
             (lambda (ref) (setq done-called t result ref)))
            (agent-repl-itest--wait-until (lambda () done-called) nil
                                          "RegisterWorkspace to answer")
            ;; Assert.
            (should (null result))
            (should (agent-repl-itest--logged-p daemon "elisp.host.register-refused" "error")))
        (agent-repl-connect-close conn)))))

(ert-deftest agent-repl-itest-host-select-sends-the-ref-and-records-it ()
  "A tab switch sends SelectWorkspace echoing the ref.
The daemon stamps `current', records last-selected and CLEARS the
attention marker; Emacs's ordinary tab switch is the clearing act and no
dedicated ack verb exists."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest-host--with-subscription daemon ref
      ;; Act.
      (agent-repl-host-select agent-repl-itest-host--ws)
      (agent-repl-itest--await-call daemon "SelectWorkspace")
      ;; Assert.
      (let ((body (car (agent-repl-itest--call-bodies daemon "SelectWorkspace"))))
        (should (equal (agent-repl-itest--body-field body 'workspace 'id)
                       (plist-get ref :id)))
        (should (equal agent-repl-host-last-selected-id (plist-get ref :id)))))))

;;;; ---- Scenario 4: the notification policy ----
;;
;; The daemon publishes the FACT and never asks whether Emacs is focused; each
;; surface applies the policy it alone has the knowledge for.  Emacs's policy
;; has three cases, and EVERY typed kind follows the same one — so the kinds
;; are a table and the three focus states are three tests.

(defconst agent-repl-itest-host--notification-kinds
  '((agent-addressed . ((agentAddressed . ())))
    (permission-requested . ((permissionRequested . ((toolName . "Bash")))))
    (question-asked . ((questionAsked . ((header . "Which approach?"))))))
  "Every HostNotificationKind arm endpoint_watch_host_workspace.proto declares.

`question_asked{header}' landed with landing 3 and its comment says the
SAME attention treatment as a permission ask.  That is the whole point of
the table: a kind with its own policy would be a defect, so each arm is
driven through the identical three cases rather than getting bespoke
handling.")

(ert-deftest agent-repl-itest-host-declares-every-notification-kind ()
  "The suite's kind table matches the contract's arm count exactly.
A drifted table would let a newly landed kind ship with no policy test at
all, which is how a kind quietly acquires a different treatment."
  ;; Arrange / Act / Assert.
  (should (equal 3 (length agent-repl-itest-host--notification-kinds))))

(defun agent-repl-itest-host--push-notification (daemon workspace-id kind)
  "Push a `notification' carrying KIND on DAEMON's host stream for WORKSPACE-ID."
  (agent-repl-itest--push
   daemon "host"
   `((notification . ((text . "Agent needs you")
                      (atMs . "1735689600000")
                      (kind . ,kind))))
   workspace-id))

(ert-deftest agent-repl-itest-host-unfocused-notification-posts-a-banner ()
  "Emacs UNFOCUSED → an OS desktop banner carrying the pushed text.
A banner is only useful when the user is looking elsewhere, and only this
process knows whether they are."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest-host--with-subscription daemon ref
      (dolist (case agent-repl-itest-host--notification-kinds)
        (let ((notified nil))
          (cl-letf (((symbol-function 'agent-repl--emacs-focused-p) (lambda (&rest _) nil))
                    ((symbol-function 'agent-repl-status-blink-tab)
                     (lambda (&rest _) (error "an unfocused Emacs must not blink a tab")))
                    ;; `&rest' because the unfocused arm threads a
                    ;; per-notification ACTIVATE (R-CLICK) behind the three
                    ;; presentation arguments.
                    ((symbol-function 'agent-repl--notify)
                     (lambda (_ws _title message &rest _) (push message notified))))
            ;; Act.
            (agent-repl-itest-host--push-notification
             daemon (plist-get ref :id) (cdr case))
            ;; Assert.
            (agent-repl-itest--wait-until
             (lambda () notified) nil
             (format "the %s desktop banner" (car case)))
            (should (equal (car notified) "Agent needs you"))))))))

(ert-deftest agent-repl-itest-host-focused-unselected-tab-blinks ()
  "Focused with the tab NOT selected → the tab-bar entry blinks.
THE CANONICAL BLINK CADENCE is specified once, on frontend.v1
RosterRowAttention, and both surfaces implement exactly that spec."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest-host--with-subscription daemon ref
      (dolist (case agent-repl-itest-host--notification-kinds)
        (let ((blinked nil))
          (cl-letf (((symbol-function 'agent-repl--emacs-focused-p) (lambda (&rest _) t))
                    ((symbol-function 'agent-repl--ws-current-name)
                     (lambda (&rest _) "some-other-ws"))
                    ((symbol-function 'agent-repl--notify)
                     (lambda (&rest _) (error "a focused Emacs must not post a banner")))
                    ((symbol-function 'agent-repl-status-blink-tab)
                     (lambda (ws) (push ws blinked))))
            ;; Act.
            (agent-repl-itest-host--push-notification
             daemon (plist-get ref :id) (cdr case))
            ;; Assert.
            (agent-repl-itest--wait-until
             (lambda () blinked) nil (format "the %s tab blink" (car case)))
            (should (equal (car blinked) agent-repl-itest-host--ws))))))))

(ert-deftest agent-repl-itest-host-selected-tab-notification-does-nothing ()
  "The tab already SELECTED → nothing at all is drawn.
The footer's activity line already shows it, so a banner or a blink here
would be noise about something the user is looking straight at."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest-host--with-subscription daemon ref
      (dolist (case agent-repl-itest-host--notification-kinds)
        (cl-letf (((symbol-function 'agent-repl--emacs-focused-p) (lambda (&rest _) t))
                  ((symbol-function 'agent-repl--ws-current-name)
                   (lambda (&rest _) agent-repl-itest-host--ws))
                  ((symbol-function 'agent-repl--notify)
                   (lambda (&rest _) (error "a selected tab must not post a banner")))
                  ((symbol-function 'agent-repl-status-blink-tab)
                   (lambda (&rest _) (error "a selected tab must not blink"))))
          ;; Act.
          (agent-repl-itest-host--push-notification
           daemon (plist-get ref :id) (cdr case))
          ;; Assert: the push was handled, and handling it drew nothing.  The
          ;; log line is the only observable, which is the point.
          (agent-repl-itest--await-log daemon "elisp.host.notification-selected" "info")
          (should (agent-repl-itest--logged-p
                   daemon "elisp.host.notification-selected" "info")))))))

(ert-deftest agent-repl-itest-host-permission-kind-logs-its-tool-name ()
  "A permission ask's gated tool name goes into the LOG CONTEXT.
There is no permission-answering surface in Emacs at all (permission.el
dies), so the tool name is diagnostic rather than drawn — dynamic values
belong in the context, never only in the message."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest-host--with-subscription daemon ref
      (cl-letf (((symbol-function 'agent-repl--emacs-focused-p) (lambda (&rest _) nil))
                ((symbol-function 'agent-repl--notify) (lambda (&rest _) nil)))
        ;; Act.
        (agent-repl-itest-host--push-notification
         daemon (plist-get ref :id)
         '((permissionRequested . ((toolName . "Bash")))))
        ;; Assert.
        (agent-repl-itest--await-log daemon "elisp.host.notification-desktop" "info")
        (let ((entries (agent-repl-itest--log-entries
                        daemon "elisp.host.notification-desktop" "info")))
          (should (seq-some (lambda (record)
                              (string-match-p "Bash" (or (alist-get 'message record) "")))
                            entries)))))))

(ert-deftest agent-repl-itest-host-question-kind-carries-its-header ()
  "`question_asked' carries a `header', and it reaches the log context.
The header is the question batch's own summary line; like a permission
ask's tool name it is programmatic semantics beside the composed text.
Asserting only the arm name (\"question-asked\") in the message would pass
even if the HEADER ITSELF never made it anywhere — the dynamic value must
actually appear on the record, per fanout §0: \"Dynamic values go in the
context, never only in the message.\""
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest-host--with-subscription daemon ref
      (cl-letf (((symbol-function 'agent-repl--emacs-focused-p) (lambda (&rest _) nil))
                ((symbol-function 'agent-repl--notify) (lambda (&rest _) nil)))
        ;; Act.
        (agent-repl-itest-host--push-notification
         daemon (plist-get ref :id)
         '((questionAsked . ((header . "Which approach?")))))
        ;; Assert.
        (agent-repl-itest--await-log daemon "elisp.host.notification-desktop" "info")
        (let ((entries (agent-repl-itest--log-entries
                        daemon "elisp.host.notification-desktop" "info")))
          (should (seq-some
                   (lambda (record)
                     (string-match-p "question-asked" (or (alist-get 'message record) "")))
                   entries))
          (should (seq-some
                   (lambda (record)
                     (or (string-match-p "Which approach\\?" (or (alist-get 'message record) ""))
                         (seq-some (lambda (arg) (string-match-p "Which approach\\?" arg))
                                   (alist-get 'arguments (alist-get 'context record)))))
                   entries)))))))

(ert-deftest agent-repl-itest-host-unfocused-notification-click-selects-the-tab ()
  "The banner's click raises the frame and selects the workspace's tab.
elisp.md states this at the arm: decider and actor are one process, so it
is plain elisp with no daemon round-trip.

KNOWN PRODUCTION GAP — this test currently FAILS by design, and the
failure is the report: `agent-repl--notify' takes exactly (WS TITLE
MESSAGE) and host.el passes no activation callback, so nothing carries
the click anywhere.  The stub takes `&rest' so the day an activation
argument is threaded through, this passes without being rewritten."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest-host--with-subscription daemon ref
      (let ((activate nil)
            (switched nil))
        (cl-letf (((symbol-function 'agent-repl--emacs-focused-p) (lambda (&rest _) nil))
                  ((symbol-function 'agent-repl--notify)
                   (lambda (_ws _title _message &rest extra)
                     (setq activate (seq-find #'functionp extra))))
                  ((symbol-function 'agent-repl--ws-switch)
                   (lambda (ws &rest _) (setq switched ws))))
          (agent-repl-itest-host--push-notification
           daemon (plist-get ref :id) '((agentAddressed . ())))
          (agent-repl-itest--wait-until
           (lambda () activate) nil
           "an activation callback on the desktop notification")
          ;; Act.
          (funcall activate)
          ;; Assert.
          (should (equal switched agent-repl-itest-host--ws)))))))

(ert-deftest agent-repl-itest-host-blink-cadence-fires-the-exact-schedule ()
  "The blink cadence is EXACTLY 0/500/1000/1500/2000 ms, unstubbed end to end.
elisp.md: \"THE CANONICAL BLINK CADENCE is specified ONCE, on `frontend.v1'
`RosterRowAttention': two blinks — 500 ms on, 500 ms off, twice — then a
steady marker until cleared.\"  Driven here by a REAL `notification' push
through host.el's real `agent-repl-status-blink-tab' — every other test in
this suite stubs that function away, so none of them can catch a divergent
cadence; only `run-with-timer' is instrumented, to record what it is asked
to schedule, and every call still runs for real."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest-host--with-subscription daemon ref
      (let ((captured nil)
            (real-run-with-timer (symbol-function 'run-with-timer)))
        (cl-letf (((symbol-function 'agent-repl--emacs-focused-p) (lambda (&rest _) t))
                  ((symbol-function 'agent-repl--ws-current-name) (lambda (&rest _) "some-other-ws"))
                  ((symbol-function 'agent-repl--notify)
                   (lambda (&rest _) (error "a focused Emacs must not post a banner")))
                  ((symbol-function 'run-with-timer)
                   (lambda (delay repeat fn &rest args)
                     (when (eq fn #'agent-repl-status--set-marker)
                       (push (cons delay (nth 1 args)) captured))
                     (apply real-run-with-timer delay repeat fn args))))
          ;; Act.
          (agent-repl-itest-host--push-notification
           daemon (plist-get ref :id) '((agentAddressed . ())))
          ;; Assert.
          (agent-repl-itest--wait-until (lambda () (= 5 (length captured))) nil
                                        "all five blink steps to be scheduled")
          (should (equal (reverse captured)
                         '((0.0 . t) (0.5 . nil) (1.0 . t) (1.5 . nil) (2.0 . t)))))))))

(ert-deftest agent-repl-itest-host-blink-re-entrant-push-restarts-the-cadence ()
  "A second notification while a blink is in flight RESTARTS the cadence.
status.el: \"a second call ... RESTART[s] the cadence rather than
interleaving with it, because each step is armed under a deterministic
per-workspace key that replaces its predecessor.\"  Two pushes back to
back must leave exactly ONE five-step schedule standing, not ten steps."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest-host--with-subscription daemon ref
      (let ((captured nil)
            (real-run-with-timer (symbol-function 'run-with-timer)))
        (cl-letf (((symbol-function 'agent-repl--emacs-focused-p) (lambda (&rest _) t))
                  ((symbol-function 'agent-repl--ws-current-name) (lambda (&rest _) "some-other-ws"))
                  ((symbol-function 'agent-repl--notify)
                   (lambda (&rest _) (error "a focused Emacs must not post a banner")))
                  ((symbol-function 'run-with-timer)
                   (lambda (delay repeat fn &rest args)
                     (let ((timer (apply real-run-with-timer delay repeat fn args)))
                       (when (eq fn #'agent-repl-status--set-marker)
                         (push (list delay timer) captured))
                       timer))))
          ;; Act: two notifications back to back.
          (agent-repl-itest-host--push-notification
           daemon (plist-get ref :id) '((agentAddressed . ())))
          (agent-repl-itest--wait-until (lambda () (= 5 (length captured))) nil
                                        "the first schedule's five timers")
          (let ((first-schedule (copy-sequence captured)))
            (agent-repl-itest-host--push-notification
             daemon (plist-get ref :id) '((agentAddressed . ())))
            ;; Assert.
            (agent-repl-itest--wait-until (lambda () (= 10 (length captured))) nil
                                          "the second schedule's five more timers")
            (should (equal 10 (length captured)))
            ;; The first schedule's own timer objects were cancelled by the
            ;; second call's `agent-repl--register-timer', which replaces a
            ;; key's held timer rather than stacking a second one beside it.
            ;; The 0 ms step has almost certainly already FIRED by the time
            ;; both pushes have landed, which also removes it from
            ;; `timer-list' — that step is excluded, not asserted false.
            (dolist (entry first-schedule)
              (let ((delay (car entry)) (timer (cadr entry)))
                (unless (= delay 0.0)
                  (should-not (memq timer timer-list)))))))))))

;;;; ---- Scenario 7: reload_webapp ----

(ert-deftest agent-repl-itest-host-reload-webapp-reloads-that-workspace ()
  "`reload_webapp' reloads THIS workspace's webview against the SAME daemon.
It is empty by design: no address, because the daemon is not changing.
It is a PUSH, never a terminal frame."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest-host--with-subscription daemon ref
      (let ((reloaded nil))
        (cl-letf (((symbol-function 'agent-repl-frontend-reload-webview)
                   (lambda (ws) (push ws reloaded))))
          ;; Act.
          (agent-repl-itest--push daemon "host" '((reloadWebapp . ()))
                                  (plist-get ref :id))
          ;; Assert.
          (agent-repl-itest--wait-until (lambda () reloaded) nil "the webview reload")
          (should (equal reloaded (list agent-repl-itest-host--ws))))))))

(ert-deftest agent-repl-itest-host-reload-webapp-leaves-the-stream-standing ()
  "A `reload_webapp' push does not end the stream.
`transferred' and `reload_webapp' are PUSHES, not endings — a standing
stream stands until the CLIENT cancels."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest-host--with-subscription daemon ref
      (cl-letf (((symbol-function 'agent-repl-frontend-reload-webview) #'ignore))
        ;; Act.
        (agent-repl-itest--push daemon "host" '((reloadWebapp . ()))
                                (plist-get ref :id))
        (agent-repl-itest--await-call daemon "WatchHostWorkspace")
        ;; Assert: the subscription is still registered on the daemon side.
        (should (equal 1 (length (agent-repl-itest--subscribers
                                  daemon "host" (plist-get ref :id)))))))))

(ert-deftest agent-repl-itest-host-open-in-editor-uses-the-shared-popup ()
  "`open_in_editor' reaches the ONE shared editor-popup subroutine.
Divergence between call sites is a defect: the consistency requirement is
code-level, the same ruling as the blink cadence.  Nothing acks."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest-host--with-subscription daemon ref
      (let ((opened nil))
        (cl-letf (((symbol-function 'agent-repl-popup-open)
                   (lambda (path &optional line) (push (list path line) opened))))
          ;; Act.
          (agent-repl-itest--push
           daemon "host"
           '((openInEditor . ((path . "/tmp/itest-host-ws/plan.md") (line . 42))))
           (plist-get ref :id))
          ;; Assert: the uint32 line rides through as a number.
          (agent-repl-itest--wait-until (lambda () opened) nil "the editor popup")
          (should (equal (car opened) (list "/tmp/itest-host-ws/plan.md" 42))))))))

(ert-deftest agent-repl-itest-host-open-in-editor-directory-with-no-line-opens-dired ()
  "An `open_in_editor' push with a directory and no `line' opens it in dired.
proto `HostOpenInEditor.line': \"UNSET = the file's top (or a directory).\"
fanout §7: \"`open_in_editor' ... a directory opens in dired.  Log INFO
with the path.\"  Runs the REAL `agent-repl-popup-open', not a stub — the
sibling test above proves only that host.el calls the shared subroutine,
never that the subroutine itself does the right thing with a directory."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest-host--with-subscription daemon ref
      (let ((dir (make-temp-file "agent-repl-itest-open-in-editor-" t)))
        (unwind-protect
            (progn
              ;; Act: no `line' field at all, and the real popup subroutine.
              (agent-repl-itest--push
               daemon "host" `((openInEditor . ((path . ,dir))))
               (plist-get ref :id))
              ;; Assert: a dired buffer for DIR.
              (agent-repl-itest--wait-until
               (lambda () (agent-repl-itest-host--dired-buffer-for dir))
               nil "the directory to open in dired")
              (should (agent-repl-itest-host--dired-buffer-for dir))
              ;; Assert: the INFO log carries the path.
              (agent-repl-itest--await-log daemon "elisp.host.open-in-editor" "info")
              (should (seq-some
                       (lambda (record)
                         (string-match-p (regexp-quote dir) (or (alist-get 'message record) "")))
                       (agent-repl-itest--log-entries daemon "elisp.host.open-in-editor" "info"))))
          (let ((buf (agent-repl-itest-host--dired-buffer-for dir)))
            (when (buffer-live-p buf) (kill-buffer buf)))
          (delete-directory dir t))))))

(ert-deftest agent-repl-itest-host-webview-url-carries-only-workspace-and-dir ()
  "The webview URL is EXACTLY http://<owning daemon>/?workspace=<id>&dir=<dir>.
Kickoff ruling: \"The webview URL is `http://<daemon.addr>/?workspace=<id>
&dir=<dir>'.\"  fanout §12: \"Nothing else rides the URL\" — no `composer'
flag and nothing else, ever, in this suite's non-dev-mode mount."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest-host--with-subscription daemon ref
      (agent-repl--ws-put agent-repl-itest-host--ws :frontend 'gui)
      (cl-letf (((symbol-function 'agent-repl--frontend-xwidget-available-p) (lambda () t))
                ((symbol-function 'agent-repl--call-in-background-workspace)
                 (lambda (_ws fn) (funcall fn))))
        ;; Act.
        (agent-repl--frontend-precreate-webview agent-repl-itest-host--ws)
        ;; Assert.
        (agent-repl-itest--wait-until (lambda () agent-repl-itest-webview-urls) nil
                                      "the webview mount to record a URL")
        (should (equal agent-repl-itest-webview-urls
                       (list (format "http://%s/?workspace=%s&dir=%s"
                                    (agent-repl-itest-daemon-address daemon)
                                    (url-hexify-string (plist-get ref :id))
                                    (url-hexify-string (plist-get ref :dir))))))))))

;;;; ---- Scenario 5, host half: the handover ----

(ert-deftest agent-repl-itest-host-transferred-adopts-on-the-successor ()
  "`transferred' → AdoptHostWorkspace on the NEW connection.
Ordering is enforced BY REFUSAL, not convention: the old stream is
cancelled only once the adopt succeeds."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon primary
    (agent-repl-itest-host--with-subscription primary ref
      (agent-repl-itest--with-second-daemon primary successor
        (let ((successor-conn (agent-repl-connect-open
                               (agent-repl-itest-daemon-address successor))))
          (unwind-protect
              (cl-letf (((symbol-function 'agent-repl-link-successor)
                         (lambda () successor-conn)))
                ;; Act.
                (agent-repl-itest--push primary "host" '((transferred . ()))
                                        (plist-get ref :id))
                ;; Assert: the adopt landed on the SUCCESSOR, not the old daemon.
                (agent-repl-itest--await-call successor "AdoptHostWorkspace")
                (let ((body (car (agent-repl-itest--call-bodies
                                  successor "AdoptHostWorkspace"))))
                  (should (equal (agent-repl-itest--body-field body 'workspace 'id)
                                 (plist-get ref :id))))
                (should (null (agent-repl-itest--calls primary "AdoptHostWorkspace"))))
            (agent-repl-connect-close successor-conn)))))))

(ert-deftest agent-repl-itest-host-transferred-resubscribes-on-the-successor ()
  "After adopting, the workspace's stream is re-opened on the successor.
Emacs's obligation, in order: adopt on the new connection, cancel the old
stream, re-subscribe on the new one."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon primary
    (agent-repl-itest-host--with-subscription primary ref
      (agent-repl-itest--with-second-daemon primary successor
        (let ((successor-conn (agent-repl-connect-open
                               (agent-repl-itest-daemon-address successor))))
          (unwind-protect
              (cl-letf (((symbol-function 'agent-repl-link-successor)
                         (lambda () successor-conn)))
                ;; Act.
                (agent-repl-itest--push primary "host" '((transferred . ()))
                                        (plist-get ref :id))
                ;; Assert.
                (agent-repl-itest--await-subscriber successor "host" (plist-get ref :id))
                (should (equal 1 (length (agent-repl-itest--subscribers
                                          successor "host" (plist-get ref :id))))))
            (agent-repl-connect-close successor-conn)))))))

(ert-deftest agent-repl-itest-host-transferred-cancels-the-old-stream ()
  "The OLD daemon observes its stream going away once adoption succeeded.
A client cancel is the graceful close, and the old daemon sees exactly
that — no verb announces it."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon primary
    (agent-repl-itest-host--with-subscription primary ref
      (agent-repl-itest--with-second-daemon primary successor
        (let ((successor-conn (agent-repl-connect-open
                               (agent-repl-itest-daemon-address successor))))
          (unwind-protect
              (cl-letf (((symbol-function 'agent-repl-link-successor)
                         (lambda () successor-conn)))
                ;; Act.
                (agent-repl-itest--push primary "host" '((transferred . ()))
                                        (plist-get ref :id))
                ;; Assert.
                (agent-repl-itest--wait-until
                 (lambda () (null (agent-repl-itest--subscribers
                                   primary "host" (plist-get ref :id))))
                 nil "the old daemon to observe the cancel")
                (should (null (agent-repl-itest--subscribers
                               primary "host" (plist-get ref :id)))))
            (agent-repl-connect-close successor-conn)))))))

(ert-deftest agent-repl-itest-host-transferred-without-a-successor-logs-error ()
  "`transferred' with no successor connection is an ERROR, stream kept.
There is nowhere to adopt, and dropping the stream would lose the only
channel carrying this workspace's state."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon primary
    (agent-repl-itest-host--with-subscription primary ref
      (cl-letf (((symbol-function 'agent-repl-link-successor) (lambda () nil)))
        ;; Act.
        (agent-repl-itest--push primary "host" '((transferred . ()))
                                (plist-get ref :id))
        ;; Assert.
        (agent-repl-itest--await-log primary "elisp.host.transferred-without-successor"
                                     "error")
        (should (agent-repl-itest--logged-p
                 primary "elisp.host.transferred-without-successor" "error"))
        (should (equal 1 (length (agent-repl-itest--subscribers
                                  primary "host" (plist-get ref :id)))))))))

(ert-deftest agent-repl-itest-host-transferred-adopts-before-cancelling-the-old-stream ()
  "Ordering is enforced BY OBSERVING THE IN-FLIGHT ADOPT, not by convention.
elisp.md: \"Emacs's obligation, in order: call `AdoptHostWorkspace' ... then
cancel the old stream and re-subscribe on the new connection.\"  The gate
withholds the adopt's ANSWER (the call is still recorded) so the scenario
can assert the old stream is STILL standing and the new one has NO
subscriber yet, then release it and assert the re-subscribe follows."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon primary
    (agent-repl-itest-host--with-subscription primary ref
      (agent-repl-itest--with-second-daemon primary successor
        (let ((successor-conn (agent-repl-connect-open
                               (agent-repl-itest-daemon-address successor))))
          (unwind-protect
              (cl-letf (((symbol-function 'agent-repl-link-successor)
                         (lambda () successor-conn)))
                (agent-repl-itest--gate successor "AdoptHostWorkspace")
                ;; Act.
                (agent-repl-itest--push primary "host" '((transferred . ()))
                                        (plist-get ref :id))
                (agent-repl-itest--await-call successor "AdoptHostWorkspace")
                ;; Assert: the adopt is IN FLIGHT — the old stream still
                ;; stands and the successor has not been subscribed yet.
                (should (equal 1 (length (agent-repl-itest--subscribers
                                          primary "host" (plist-get ref :id)))))
                (should (equal 0 (length (agent-repl-itest--subscribers
                                          successor "host" (plist-get ref :id)))))
                ;; Act: let the adopt answer.
                (agent-repl-itest--release-gate successor "AdoptHostWorkspace")
                ;; Assert: the re-subscribe follows, and only then.
                (agent-repl-itest--await-subscriber successor "host" (plist-get ref :id))
                (agent-repl-itest--wait-until
                 (lambda () (null (agent-repl-itest--subscribers
                                   primary "host" (plist-get ref :id))))
                 nil "the old stream to be cancelled after the adopt lands")
                (should (null (agent-repl-itest--subscribers
                               primary "host" (plist-get ref :id)))))
            (agent-repl-connect-close successor-conn)))))))

(ert-deftest agent-repl-itest-host-transferred-adopt-error-keeps-the-old-stream ()
  "AdoptHostWorkspace's error arm logs ERROR and keeps the OLD stream standing.
fanout §7: \"error arm → ERROR log, keep the old stream.\""
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon primary
    (agent-repl-itest-host--with-subscription primary ref
      (agent-repl-itest--with-second-daemon primary successor
        (let ((successor-conn (agent-repl-connect-open
                               (agent-repl-itest-daemon-address successor))))
          (unwind-protect
              (cl-letf (((symbol-function 'agent-repl-link-successor)
                         (lambda () successor-conn)))
                (agent-repl-itest--script successor "AdoptHostWorkspace" '((error . ())))
                ;; Act.
                (agent-repl-itest--push primary "host" '((transferred . ()))
                                        (plist-get ref :id))
                ;; Assert.
                (agent-repl-itest--await-log primary "elisp.host.adopt-refused" "error")
                (should (agent-repl-itest--logged-p primary "elisp.host.adopt-refused" "error"))
                (should (equal 1 (length (agent-repl-itest--subscribers
                                          primary "host" (plist-get ref :id)))))
                (should (equal 0 (length (agent-repl-itest--subscribers
                                          successor "host" (plist-get ref :id))))))
            (agent-repl-connect-close successor-conn)))))))

(ert-deftest agent-repl-itest-host-transferred-updates-the-workspace-conn ()
  "After a successful transfer, `agent-repl-host-conn' names the SUCCESSOR.
fanout §7: \"success → cancel the old stream, subscribe on NEW, update
`:conn'.\"  A verb sent afterwards (SelectWorkspace) must land on the
successor's own recorded calls, never the old daemon's."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon primary
    (agent-repl-itest-host--with-subscription primary ref
      (agent-repl-itest--with-second-daemon primary successor
        (let ((successor-conn (agent-repl-connect-open
                               (agent-repl-itest-daemon-address successor))))
          (unwind-protect
              (cl-letf (((symbol-function 'agent-repl-link-successor)
                         (lambda () successor-conn)))
                ;; Act.
                (agent-repl-itest--push primary "host" '((transferred . ()))
                                        (plist-get ref :id))
                (agent-repl-itest--await-subscriber successor "host" (plist-get ref :id))
                ;; Assert: `:conn' is the successor.
                (should (eq (agent-repl-host-conn agent-repl-itest-host--ws) successor-conn))
                ;; Act: a tab switch after the transfer.
                (agent-repl-host-select agent-repl-itest-host--ws)
                ;; Assert: it lands on the successor, never the old daemon.
                (agent-repl-itest--await-call successor "SelectWorkspace")
                (should (null (agent-repl-itest--calls primary "SelectWorkspace"))))
            (agent-repl-connect-close successor-conn)))))))

(ert-deftest agent-repl-itest-host-transferred-adopts-through-the-real-link ()
  "The handover runs end to end through `daemon-link.el', with NO stub anywhere.
elisp.md: \"there is no daemon<->daemon channel — coordination is CLIENT
RELAY ... and Emacs is the relay.\"  The three tests above all stub
`agent-repl-link-successor'; here `shutdown_announced{address}' dual-attaches
for real over a genuine `WatchDaemon' stream, and the transfer adopts onto
the resulting real successor connection."
  ;; Arrange: connect the link for real, isolated from every OTHER global
  ;; link hook so only this scenario's own reactions run.
  (agent-repl-itest--with-fake-daemon primary
    (agent-repl-itest-host--with-subscription primary ref
      (let ((agent-repl-link-up-functions nil)
            (agent-repl-link-down-functions nil)
            (agent-repl-link-handover-functions nil)
            (agent-repl-link-drain-functions nil)
            (agent-repl-link-no-daemon-functions nil)
            (agent-repl-link-drain nil)
            (agent-repl-link-drain-segment nil))
        (unwind-protect
            (progn
              (agent-repl-link-connect)
              (agent-repl-itest--await-subscriber primary "daemon")
              (agent-repl-itest--with-second-daemon primary successor
                ;; Act: announce the successor for real, then release the
                ;; workspace — no `agent-repl-link-successor' stub anywhere.
                (agent-repl-itest--push
                 primary "daemon"
                 `((shutdownAnnounced
                    . ((address . ,(agent-repl-itest-daemon-address successor))
                       (cause . ((selfMergeRollout . ())))
                       (expectedOutageMs . "1500")
                       (mintedAtMs . ,(format "%d" (truncate (* 1000 (float-time)))))))))
                (agent-repl-itest--wait-until
                 (lambda () (agent-repl-link-successor)) nil
                 "the real WatchDaemon dual-attach to open the successor connection")
                (agent-repl-itest--push primary "host" '((transferred . ()))
                                        (plist-get ref :id))
                ;; Assert: the adopt landed on the successor.
                (agent-repl-itest--await-call successor "AdoptHostWorkspace")
                (should (equal (agent-repl-itest--body-field
                                (car (agent-repl-itest--call-bodies successor "AdoptHostWorkspace"))
                                'workspace 'id)
                               (plist-get ref :id)))
                (agent-repl-itest--wait-until
                 (lambda () (null (agent-repl-itest--subscribers
                                   primary "host" (plist-get ref :id))))
                 nil "the old daemon to observe the cancel")
                (agent-repl-itest--await-subscriber successor "host" (plist-get ref :id))))
          (agent-repl-link-teardown))))))

;;;; ---- Scenario 15: validation ----

(ert-deftest agent-repl-itest-host-push-without-naming-is-refused ()
  "A HostWorkspace push missing `naming' is a contract breach: ERROR, dropped.
THE VALIDATION INVARIANT on the consumer side: a push carrying an unset
non-optional field makes the consumer raise a loud error itself."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest-host--with-subscription daemon ref
      ;; Act: a legal-JSON push with no `naming' at all.
      (agent-repl-itest--push daemon "host" '((host . ((none . ()))))
                              (plist-get ref :id))
      ;; Assert.
      (agent-repl-itest--await-log daemon "elisp.rpc.push-invalid" "error")
      (should (agent-repl-itest--logged-p daemon "elisp.rpc.push-invalid" "error")))))

(ert-deftest agent-repl-itest-host-invalid-push-leaves-the-stream-standing ()
  "A dropped push does not end the stream; the next one still arrives.
The stream continues by design — one bad frame is not a reason to lose
every later fact about the workspace."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest-host--with-subscription daemon ref
      (let ((id (plist-get ref :id)))
        (agent-repl-itest--push daemon "host" '((host . ((none . ())))) id)
        (agent-repl-itest--await-log daemon "elisp.rpc.push-invalid" "error")
        ;; Act: a well-formed push after the bad one.
        (agent-repl-itest-host--push-host daemon id (agent-repl-itest-host--live 'open))
        ;; Assert.
        (agent-repl-itest--wait-until
         (lambda () (eq (agent-repl-host-composer-gate agent-repl-itest-host--ws) :open))
         nil "the push after the invalid one")
        (should (eq (agent-repl-host-composer-gate agent-repl-itest-host--ws) :open))))))

(ert-deftest agent-repl-itest-host-push-with-unset-session-oneof-is-refused ()
  "A HostWorkspace push with an unset `session' oneof is a contract breach.
fanout §0: \"a push ... with an unset oneof ... is a contract breach: signal
`agent-repl-wire-error' and log ERROR.\"  `/_fake/push' does not itself
enforce this invariant (only unknown-field strictness), so this pins
ELISP's own decoder, not the fake's."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest-host--with-subscription daemon ref
      ;; Act: `naming' present, but neither `none' nor `existing' set.
      (agent-repl-itest--push daemon "host" '((host . ((naming . ()))))
                              (plist-get ref :id))
      ;; Assert.
      (agent-repl-itest--await-log daemon "elisp.rpc.push-invalid" "error")
      (should (agent-repl-itest--logged-p daemon "elisp.rpc.push-invalid" "error"))
      ;; Assert (fanout §4): the context carries the RAW pushed JSON.
      (should (seq-some (lambda (arg) (string-match-p "naming" arg))
                        (agent-repl-itest-host--push-invalid-context-strings daemon))))))

(ert-deftest agent-repl-itest-host-push-existing-without-id-is-refused ()
  "`existing' without its non-optional `id' is a contract breach.
proto `HostSessionExisting.id': a required message field; fanout §2: \"a
non-optional MESSAGE field absent → `agent-repl-wire-error'.\""
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest-host--with-subscription daemon ref
      ;; Act: `existing.live' well-formed, but `existing.id' entirely absent.
      (agent-repl-itest--push
       daemon "host"
       '((host . ((existing . ((live . ((generation . ((value . "gen-1")))
                                        (shimAttached . t)
                                        (backfill . ((done . ())))
                                        (open . ())))))
                  (naming . ()))))
       (plist-get ref :id))
      ;; Assert.
      (agent-repl-itest--await-log daemon "elisp.rpc.push-invalid" "error")
      (should (agent-repl-itest--logged-p daemon "elisp.rpc.push-invalid" "error"))
      (should (seq-some (lambda (arg) (string-match-p "live" arg))
                        (agent-repl-itest-host--push-invalid-context-strings daemon))))))

(ert-deftest agent-repl-itest-host-push-existing-with-unset-standing-is-refused ()
  "`existing' with neither `live' nor `terminal' set is a contract breach.
proto `HostSessionExisting.standing': \"THE ARM IS THE STANDING\" — an
unset oneof here is as much a breach as anywhere else in the contract."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest-host--with-subscription daemon ref
      ;; Act: `existing.id' present, but its `standing' oneof is unset.
      (agent-repl-itest--push
       daemon "host"
       '((host . ((existing . ((id . ((value . "host-session-1")))))
                  (naming . ()))))
       (plist-get ref :id))
      ;; Assert.
      (agent-repl-itest--await-log daemon "elisp.rpc.push-invalid" "error")
      (should (agent-repl-itest--logged-p daemon "elisp.rpc.push-invalid" "error"))
      (should (seq-some (lambda (arg) (string-match-p "host-session-1" arg))
                        (agent-repl-itest-host--push-invalid-context-strings daemon))))))

(ert-deftest agent-repl-itest-host-notification-without-kind-is-refused ()
  "A `notification' push missing its non-optional `kind' is a contract breach.
proto `HostWorkspaceNotification.kind': \"THE ARM IS THE KIND\" — `kind' is
an ordinary required message field, not a oneof, so its absence is the
required-message-field breach rather than an unset-oneof one."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest-host--with-subscription daemon ref
      (cl-letf (((symbol-function 'agent-repl--notify)
                 (lambda (&rest _) (error "an unset `kind' must draw nothing")))
                ((symbol-function 'agent-repl-status-blink-tab)
                 (lambda (&rest _) (error "an unset `kind' must draw nothing"))))
        ;; Act: `text' and `atMs' present, `kind' entirely absent.
        (agent-repl-itest--push
         daemon "host"
         '((notification . ((text . "Agent needs you") (atMs . "1735689600000"))))
         (plist-get ref :id))
        ;; Assert: the log, no banner and no blink, and the stream standing.
        (agent-repl-itest--await-log daemon "elisp.rpc.push-invalid" "error")
        (should (agent-repl-itest--logged-p daemon "elisp.rpc.push-invalid" "error"))
        (should (seq-some (lambda (arg) (string-match-p "Agent needs you" arg))
                          (agent-repl-itest-host--push-invalid-context-strings daemon)))
        (should (equal 1 (length (agent-repl-itest--subscribers
                                  daemon "host" (plist-get ref :id)))))))))

(ert-deftest agent-repl-itest-host-unknown-push-arm-is-refused ()
  "The DECODER ALONE refuses any `WatchHostWorkspaceResponse' push arm the
schema does not declare — a decoder pin, not a control-plane one.
Asserting only that the fake's `/_fake/push' answers 400 for an unknown
arm (the fake's own protojson strictness) would pass against ANY elisp,
including one whose decoder has no such check at all; calling elisp's own
`agent-repl-wire-decode-watch-host-workspace-response' directly is what
actually pins IT."
  ;; Arrange / Act / Assert.
  (should-error
   (agent-repl-wire-decode-watch-host-workspace-response '((hibernated . nil)))
   :type 'agent-repl-wire-error))

(provide 'test-integration-host)

;;; test-integration-host.el ends here
