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
(declare-function agent-repl-host-on-link-up "host")
(declare-function agent-repl-host-on-link-down "host")
(declare-function agent-repl--initialize-input-buffer "panels")
(declare-function agent-repl--create-buffer "core")
(declare-function agent-repl--input-buffer-name "core")
(declare-function agent-repl--input-buffer-name-for-id "panels")
(declare-function agent-repl--history-restore "history")
(defvar agent-repl-host-update-functions)
(defvar agent-repl-host-last-selected-id)
(defvar agent-repl-host-handover-retry-delay)
(defvar agent-repl-connect-unary-timeout-seconds)
(defvar agent-repl-link-up-functions)
(defvar agent-repl-link-down-functions)
(defvar agent-repl-link-handover-functions)
(defvar agent-repl-link-drain-functions)
(defvar agent-repl-link-no-daemon-functions)
(defvar agent-repl-link-drain)
(defvar agent-repl-link-drain-segment)
(defvar agent-repl-verbs-health-buffer)
(defvar agent-repl-itest-webview-urls)
(defvar agent-repl--input-buffer-re)
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
           (agent-repl--ws-put agent-repl-itest-host--ws :project-dir (agent-repl-itest--fixture-dir "itest-host-ws"))
           (agent-repl-host-register
            conn (agent-repl-itest--fixture-dir "itest-host-ws")
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
            (agent-repl--ws-put agent-repl-itest-host--ws :project-dir (agent-repl-itest--fixture-dir "itest-host-ws"))
            (agent-repl-host-register conn (agent-repl-itest--fixture-dir "itest-host-ws")
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
                           (openedAtMs . "1735689600000")
                           (linkSevered . ()))])))
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
                           (openedAtMs . "1735689600000")
                           (linkSevered . ()))])))
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
    (agent-repl-itest--script daemon "RegisterWorkspace"
                              '((error . ((notAWorktree . ())))))
    (let ((conn (agent-repl-connect-open (agent-repl-itest-daemon-address daemon)))
          (done-called nil)
          (result :never))
      (unwind-protect
          (progn
            (agent-repl--ws-put agent-repl-itest-host--ws :project-dir (agent-repl-itest--fixture-dir "itest-host-ws"))
            ;; Act.
            (agent-repl-host-register
             conn (agent-repl-itest--fixture-dir "itest-host-ws")
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
           `((openInEditor . ((path . ,(expand-file-name "plan.md" (agent-repl-itest--fixture-dir "itest-host-ws"))) (line . 42))))
           (plist-get ref :id))
          ;; Assert: the uint32 line rides through as a number.
          (agent-repl-itest--wait-until (lambda () opened) nil "the editor popup")
          (should (equal (car opened) (list (expand-file-name "plan.md" (agent-repl-itest--fixture-dir "itest-host-ws")) 42))))))))

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

(ert-deftest agent-repl-itest-host-webview-url-carries-workspace-dir-and-log-level ()
  "The URL carries workspace identity plus the page's logging threshold.
Kickoff ruling: \"The webview URL is `http://<daemon.addr>/?workspace=<id>
&dir=<dir>'.\"  Logging adds `log_level'; no `composer' flag or other view
state rides this suite's non-dev-mode mount.  The HOST is the workspace's
own `ws-<id>.localhost' on the daemon's port (2026-09-23): pages sharing
the daemon's bare address shared WebKit's six-connection pool, and six
standing page streams starved every later request."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest-host--with-subscription daemon ref
      (agent-repl--ws-put agent-repl-itest-host--ws :frontend 'gui)
      (cl-letf (((symbol-function 'agent-repl--frontend-xwidget-available-p) (lambda () t))
                ((symbol-function 'agent-repl--frontend-getenv) (lambda (_name) nil))
                ;; The xwidget itself is an external boundary and this test is
                ;; about the URL the mount asks for, not the widget behind it.
                ((symbol-function 'agent-repl--frontend-webview-live-widget)
                 (lambda (&rest _) nil))
                ((symbol-function 'agent-repl--call-in-background-workspace)
                 (lambda (_ws fn) (funcall fn))))
        ;; Act.
        (agent-repl--frontend-precreate-webview agent-repl-itest-host--ws)
        ;; Assert.
        (agent-repl-itest--wait-until (lambda () agent-repl-itest-webview-urls) nil
                                      "the webview mount to record a URL")
        (should (equal agent-repl-itest-webview-urls
                       (list (format "http://ws-%s.localhost:%s/?workspace=%s&dir=%s&log_level=info"
                                    (agent-repl--frontend-page-host-label (plist-get ref :id))
                                    (car (last (split-string (agent-repl-itest-daemon-address daemon) ":")))
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

(ert-deftest agent-repl-itest-host-transferred-while-the-successor-is-pending-adopts-on-acceptance ()
  "THE REAL ORDERING: the transfer notice beats the successor\='s acceptance.
The outgoing daemon announces the stand-down and transfers every FREE
workspace about a millisecond later — before this Emacs has read the
announcement, dialed the successor and had its `WatchDaemon' accepted.
So `transferred' lands while `agent-repl-link-successor' still answers
nil, and the adopt must be LATCHED onto daemon-link\='s acceptance seam
rather than dropped as a missing-successor breach."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon primary
    (agent-repl-itest-host--with-subscription primary ref
      (agent-repl-itest--with-second-daemon primary successor
        (let ((successor-conn (agent-repl-connect-open
                               (agent-repl-itest-daemon-address successor)))
              (agent-repl-link-handover-functions nil))
          (unwind-protect
              (cl-letf (((symbol-function 'agent-repl-link-successor) (lambda () nil))
                        ((symbol-function 'agent-repl-link-successor-pending-p)
                         (lambda () t)))
                ;; Act: the push arrives inside the acceptance window.
                (agent-repl-itest--push primary "host" '((transferred . ()))
                                        (plist-get ref :id))
                (agent-repl-itest--await-log
                 primary "elisp.host.transferred-awaiting-successor" "info")
                (should (null (agent-repl-itest--calls successor "AdoptHostWorkspace")))
                ;; Act: the successor proves it is listening.
                (run-hook-with-args 'agent-repl-link-handover-functions
                                    nil successor-conn)
                ;; Assert.
                (agent-repl-itest--await-call successor "AdoptHostWorkspace")
                (let ((body (car (agent-repl-itest--call-bodies
                                  successor "AdoptHostWorkspace"))))
                  (should (equal (agent-repl-itest--body-field body 'workspace 'id)
                                 (plist-get ref :id)))))
            (agent-repl-connect-close successor-conn)))))))

(ert-deftest agent-repl-itest-host-transferred-before-the-announcement-waits ()
  "`transferred' ahead of the announcement is a WAIT, and the stream is kept.
The daemon announces the stand-down and THEN transfers each free
workspace, and the two pushes ride different streams — so a transfer this
Emacs decodes first is a notice that overtook its own announcement, never
a missing one.  It waits on daemon-link's acceptance seam; dropping the
stream would lose the only channel carrying this workspace's state, and
calling it an error dropped the adopt with it."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon primary
    (agent-repl-itest-host--with-subscription primary ref
      (cl-letf (((symbol-function 'agent-repl-link-successor) (lambda () nil))
                ((symbol-function 'agent-repl-link-successor-pending-p) (lambda () nil)))
        ;; Act.
        (agent-repl-itest--push primary "host" '((transferred . ()))
                                (plist-get ref :id))
        ;; Assert.
        (agent-repl-itest--await-log primary "elisp.host.transferred-before-the-announcement"
                                     "warn")
        (should-not (agent-repl-itest--logged-p
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
fanout §7: \"error arm → ERROR log, keep the old stream.\"  The scripted
refusal names a REAL cause arm: `AdoptHostWorkspaceError.cause' is a
oneof over the daemon\='s own refusal sites, so an error carrying no arm
at all is not a legal message and is refused as a contract breach one
layer earlier (`elisp.rpc.response-invalid'), which would test the
decoder rather than this arm."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon primary
    (agent-repl-itest-host--with-subscription primary ref
      (agent-repl-itest--with-second-daemon primary successor
        (let ((successor-conn (agent-repl-connect-open
                               (agent-repl-itest-daemon-address successor))))
          (unwind-protect
              (cl-letf (((symbol-function 'agent-repl-link-successor)
                         (lambda () successor-conn)))
                (agent-repl-itest--script
                 successor "AdoptHostWorkspace"
                 '((error . ((unknownWorkspace . ())))))
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

;;;; ---- Audit-2 additions (R-SUITE-2) ----
;;
;; Findings 6-16 of docs/overhaul/reports/elisp-suite-audit-2.md.  Kept in
;; their own section so they merge cleanly beside concurrent edits above.

(declare-function agent-repl-host-stream "host")
(declare-function agent-repl-host-handle-refusal "host")
(declare-function agent-repl-link-successor "daemon-link")
(declare-function agent-repl-link-dial-successor "daemon-link")
(declare-function agent-repl-connect-connection-address "connect")
(declare-function agent-repl--ws-get "workspace")
(defvar agent-repl-itest-notifications)
(defvar persp-activated-functions)
(defvar persp-before-deactivate-functions)

(defun agent-repl-itest-host--announce (daemon address)
  "Push `shutdown_announced' on DAEMON's daemon stream naming ADDRESS.
The host suite's own copy of the announcement: the refusal-driven
handover paths need a REAL dual attach through `daemon-link.el', and the
link suite's helper is not loaded here."
  (agent-repl-itest--push
   daemon "daemon"
   `((shutdownAnnounced
      . ((address . ,address)
         (cause . ((selfMergeRollout . ())))
         (expectedOutageMs . "1500")
         (mintedAtMs . ,(format "%d" (truncate (* 1000 (float-time))))))))))

(defmacro agent-repl-itest-host--with-link-and-subscription (daemon ref &rest body)
  "Stand a real link on DAEMON and subscribe the suite's workspace on it.
The refusal-driven handover needs BOTH: `agent-repl-link-dial-successor'
attaches the successor to the link's own primary, and the acceptance seam
that wakes a deferred adopt is the link's handover hook.  Every link hook
is scratch-bound, so only this scenario's reactions run."
  (declare (indent 2) (debug (form symbolp body)))
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
           (agent-repl-itest-host--with-subscription ,daemon ,ref ,@body))
       (agent-repl-link-teardown))))

(defmacro agent-repl-itest-host--with-two-subscriptions (daemon ref-a ref-b &rest body)
  "Register and subscribe TWO workspaces on DAEMON, then run BODY.
REF-A and REF-B are bound to the refs the daemon minted for
`agent-repl-itest-host--ws' and its `-b' sibling.  Ownership and
per-workspace fan-out are only observable with a second workspace in the
picture: with one, a broadcast implementation is indistinguishable from a
targeted one."
  (declare (indent 3) (debug (form symbolp symbolp body)))
  `(let ((conn (agent-repl-connect-open (agent-repl-itest-daemon-address ,daemon)))
         (ws-b (concat agent-repl-itest-host--ws "-b")))
     (unwind-protect
         (let ((,ref-a nil) (,ref-b nil))
           (agent-repl--ws-put agent-repl-itest-host--ws :project-dir (agent-repl-itest--fixture-dir "itest-host-ws"))
           (agent-repl--ws-put ws-b :project-dir (agent-repl-itest--fixture-dir "itest-host-ws-b"))
           (agent-repl-host-register conn (agent-repl-itest--fixture-dir "itest-host-ws")
                                     (lambda (minted) (setq ,ref-a minted)))
           (agent-repl-host-register conn (agent-repl-itest--fixture-dir "itest-host-ws-b")
                                     (lambda (minted) (setq ,ref-b minted)))
           (agent-repl-itest--wait-until (lambda () (and ,ref-a ,ref-b)) nil
                                         "both RegisterWorkspace calls to answer")
           (agent-repl-host-subscribe conn agent-repl-itest-host--ws ,ref-a)
           (agent-repl-host-subscribe conn ws-b ,ref-b)
           (agent-repl-itest--await-subscriber ,daemon "host" (plist-get ,ref-a :id))
           (agent-repl-itest--await-subscriber ,daemon "host" (plist-get ,ref-b :id))
           ,@body)
       (ignore-errors (agent-repl-host-forget agent-repl-itest-host--ws))
       (ignore-errors (agent-repl-host-forget ws-b))
       (agent-repl-connect-close conn))))

;; audit-2 #6
(ert-deftest agent-repl-itest-host-select-transferring-away-redials-the-successor ()
  "A `transferring_away{address}' refusal of SelectWorkspace self-heals.
fanout §7 HANDOVER REDIAL; elisp.md \"ordering is enforced BY REFUSAL ...
a lagging client self-heals from the refusal\".  NO announcement precedes
this: the refusal is the first news of the handover, and the address it
carries is the whole recovery — dial it, adopt onto it, move the
workspace's connection there and drop the old stream."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon primary
    (agent-repl-itest-host--with-link-and-subscription primary ref
      (agent-repl-itest--with-second-daemon primary successor
        (let ((address (agent-repl-itest-daemon-address successor)))
          (agent-repl-itest--script
           primary "SelectWorkspace"
           `((error . ((transferringAway . ((address . ,address)))))))
          ;; Act.
          (agent-repl-host-select agent-repl-itest-host--ws)
          ;; Assert: the redial, the adopt, the moved connection, the cancel.
          (agent-repl-itest--await-log primary "elisp.host.redial" "info")
          (agent-repl-itest--await-subscriber successor "daemon")
          (agent-repl-itest--await-call successor "AdoptHostWorkspace")
          (agent-repl-itest--wait-until
           (lambda ()
             (equal address (agent-repl-connect-connection-address
                             (agent-repl-host-conn agent-repl-itest-host--ws))))
           nil "the workspace's connection to move to the successor")
          (should (equal address (agent-repl-connect-connection-address
                                  (agent-repl-host-conn agent-repl-itest-host--ws))))
          (agent-repl-itest--wait-until
           (lambda () (null (agent-repl-itest--subscribers
                             primary "host" (plist-get ref :id))))
           nil "the old host stream to be cancelled")
          (should (null (agent-repl-itest--subscribers
                         primary "host" (plist-get ref :id)))))))))

;; audit-2 #7
(ert-deftest agent-repl-itest-host-adopt-not-yet-adopted-is-retried ()
  "`not_yet_adopted' from the successor's adopt is retried, not reported.
fanout §7: \"`not_yet_adopted' → INFO, retry the adopt once the
successor's WatchDaemon is accepted.\"  The successor refuses the first
adopt because it has not taken the workspace over yet — nothing is wrong,
so the arm is news rather than a failure and the walk runs again."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon primary
    (agent-repl-itest-host--with-link-and-subscription primary ref
      (agent-repl-itest--with-second-daemon primary successor
        (agent-repl-itest--script successor "AdoptHostWorkspace"
                                  '((error . ((notYetAdopted . ())))))
        (agent-repl-itest-host--announce
         primary (agent-repl-itest-daemon-address successor))
        (agent-repl-itest--wait-until #'agent-repl-link-successor nil
                                      "the successor to be ACCEPTED")
        ;; Act: the release lands while the successor still refuses.
        (agent-repl-itest--push primary "host" '((transferred . ()))
                                (plist-get ref :id))
        (agent-repl-itest--await-call successor "AdoptHostWorkspace")
        (agent-repl-itest--await-log primary "elisp.host.not-yet-adopted" "info")
        ;; The successor finishes taking the workspace over.  The success arm
        ;; is scripted EXPLICITLY: an empty `{}' is a response with no oneof
        ;; arm set, which the decoder refuses as a contract breach rather than
        ;; reading as an acceptance.
        (agent-repl-itest--script successor "AdoptHostWorkspace"
                                  '((success . ())))
        ;; Assert: a SECOND adopt lands, and the stream ends up there.
        (agent-repl-itest--await-call successor "AdoptHostWorkspace" 2)
        (agent-repl-itest--await-subscriber successor "host" (plist-get ref :id))))))

;; audit-2 #7
(ert-deftest agent-repl-itest-host-not-yet-adopted-defers-to-acceptance ()
  "`not_yet_adopted' with NO successor standing adopts once one is ACCEPTED.
`agent-repl-host--adopt-on-acceptance': \"NO BUSY LOOP AND NO POLL\" — one
self-removing function on the handover hook, woken by the acceptance
itself.  EXACTLY ONE adopt may land: a retry that re-armed itself would
adopt again on every later handover."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon primary
    (agent-repl-itest-host--with-link-and-subscription primary ref
      (agent-repl-itest--with-second-daemon primary successor
        ;; Act: the refusal arrives while nothing is standing.
        (agent-repl-host-handle-refusal agent-repl-itest-host--ws
                                        '(:arm :not-yet-adopted :value nil))
        (should (null (agent-repl-itest--calls successor "AdoptHostWorkspace")))
        ;; The successor is announced and accepted only now.
        (agent-repl-itest-host--announce
         primary (agent-repl-itest-daemon-address successor))
        ;; Assert: one adopt, after acceptance.
        (agent-repl-itest--await-call successor "AdoptHostWorkspace")
        (agent-repl-itest--await-subscriber successor "host" (plist-get ref :id))
        (should (equal 1 (length (agent-repl-itest--calls
                                  successor "AdoptHostWorkspace"))))))))

;; audit-2 #8
(ert-deftest agent-repl-itest-host-adoption-redials-the-webview-at-the-successor ()
  "Adoption navigates the webview to the SUCCESSOR's address.
fanout §7: host.el \"updates the workspace's `:conn' to the successor
FIRST and then calls `agent-repl-frontend-reload-webview', so the webview
navigates to `http://<successor>/?workspace=<id>&dir=<dir>'\" — on the
workspace's own `ws-<id>.localhost' host at the successor's port.  The widget
is NAVIGATED, never remounted, so the observable is the URL the navigate
was asked for."
  ;; Arrange: a mounted webview on the primary.
  (agent-repl-itest--with-fake-daemon primary
    (agent-repl-itest-host--with-subscription primary ref
      (agent-repl-itest--with-second-daemon primary successor
        (let ((successor-conn (agent-repl-connect-open
                               (agent-repl-itest-daemon-address successor)))
              (navigated nil))
          (unwind-protect
              (cl-letf (((symbol-function 'agent-repl-link-successor)
                         (lambda () successor-conn))
                        ((symbol-function 'agent-repl--frontend-xwidget-available-p)
                         (lambda () t))
                        ((symbol-function 'agent-repl--frontend-getenv) (lambda (_name) nil))
                        ((symbol-function 'agent-repl--frontend-webview-live-widget)
                         (lambda (&rest _) 'fake-widget))
                        ;; The mount arms the load watcher on whatever the live
                        ;; widget accessor answers, and the stub widget above is
                        ;; a symbol rather than an xwidget.  The watcher is the
                        ;; open-progress ladder, not the redial under test.
                        ((symbol-function 'agent-repl--frontend-watch-load) #'ignore)
                        ((symbol-function 'agent-repl--frontend-webview-navigate-widget)
                         (lambda (_widget url) (push url navigated)))
                        ((symbol-function 'agent-repl--call-in-background-workspace)
                         (lambda (_ws fn) (funcall fn))))
                (agent-repl--ws-put agent-repl-itest-host--ws :frontend 'gui)
                (agent-repl--frontend-precreate-webview agent-repl-itest-host--ws)
                ;; Act.
                (agent-repl-itest--push primary "host" '((transferred . ()))
                                        (plist-get ref :id))
                ;; Assert: exactly one navigation, at the successor.
                (agent-repl-itest--wait-until (lambda () navigated) nil
                                              "the webview redial")
                (should (equal 1 (length navigated)))
                (should (equal (car navigated)
                               (format "http://ws-%s.localhost:%s/?workspace=%s&dir=%s&log_level=info"
                                       (agent-repl--frontend-page-host-label (plist-get ref :id))
                                       (car (last (split-string (agent-repl-itest-daemon-address successor) ":")))
                                       (url-hexify-string (plist-get ref :id))
                                       (url-hexify-string (plist-get ref :dir)))))
                ;; AND THE REST OF THE WALK IS AWAITED INSIDE THE STUBS.
                ;; The redial now happens BEFORE the adopt is issued (see
                ;; host.el's rendezvous note), so the navigation this test
                ;; waits for no longer implies the adopt has landed --
                ;; leaving here on the navigation alone would unwind the
                ;; external-boundary stubs while the walk is still running.
                (agent-repl-itest--await-subscriber successor "host" (plist-get ref :id)))
            (agent-repl-connect-close successor-conn)))))))

;; audit-2 #8
(ert-deftest agent-repl-itest-host-conn-moves-before-the-webview-reload ()
  "`:conn' is the successor's ALREADY when the webview reload is called.
host.el: \"`:conn' MUST move before the reload because frontend.el derives
the page URL from `agent-repl-host-conn' — reloading first would navigate
the webview straight back at the daemon that just released the
workspace.\"  The order is only observable from inside the reload."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon primary
    (agent-repl-itest-host--with-subscription primary ref
      (agent-repl-itest--with-second-daemon primary successor
        (let ((successor-conn (agent-repl-connect-open
                               (agent-repl-itest-daemon-address successor)))
              (conn-at-reload 'unset))
          (unwind-protect
              (cl-letf (((symbol-function 'agent-repl-link-successor)
                         (lambda () successor-conn))
                        ((symbol-function 'agent-repl-frontend-reload-webview)
                         (lambda (ws) (setq conn-at-reload (agent-repl-host-conn ws)))))
                ;; Act.
                (agent-repl-itest--push primary "host" '((transferred . ()))
                                        (plist-get ref :id))
                ;; Assert.
                (agent-repl-itest--wait-until
                 (lambda () (not (eq conn-at-reload 'unset))) nil
                 "the webview reload to be called")
                (should (eq conn-at-reload successor-conn)))
            (agent-repl-connect-close successor-conn)))))))

(ert-deftest agent-repl-itest-host-webview-is-redialed-before-the-successor-sees-the-adopt ()
  "The successor's rendezvous can COMPLETE, because its second participant
already exists when its first one calls.

THE DEADLOCK THIS PINS, measured in the e2e sandbox on a handover of a
workspace with a live panel: the successor's `AdoptHostWorkspace' is a
RENDEZVOUS that completes only when every participant the outgoing daemon
snapshotted at announcement has called, and for an OPEN workspace those
are the host AND the reloaded page.  The webapp never redials a successor
of its own, so the reloaded page is the host's to produce — and producing
it only once the adopt ANSWERED made the host's call wait for itself.
The daemon logged `AdoptHostWorkspace timed out after 10s' and `the
caller gave up before the rendezvous completed', and the outgoing daemon
then sat out its whole adoption window before exiting, so the promotion
Emacs makes on the primary stream's close never came.

The claim is an ORDERING ACROSS TWO PROCESSES, which is why it belongs
here rather than only in the unit suite: by the time the SUCCESSOR
DAEMON has observed the adopt on the wire, this Emacs must already have
navigated the page at it."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon primary
    (agent-repl-itest-host--with-subscription primary ref
      (agent-repl-itest--with-second-daemon primary successor
        (let ((successor-conn (agent-repl-connect-open
                               (agent-repl-itest-daemon-address successor)))
              (reloaded nil))
          (unwind-protect
              (cl-letf (((symbol-function 'agent-repl-link-successor)
                         (lambda () successor-conn))
                        ((symbol-function 'agent-repl-frontend-reload-webview)
                         (lambda (_ws) (setq reloaded t))))
                ;; Act.
                (agent-repl-itest--push primary "host" '((transferred . ()))
                                        (plist-get ref :id))
                ;; Assert.
                (agent-repl-itest--await-call successor "AdoptHostWorkspace")
                (should reloaded))
            (agent-repl-connect-close successor-conn)))))))

;; audit-2 #9
(ert-deftest agent-repl-itest-host-transferring-away-without-address-is-a-breach ()
  "`transferring_away' carrying NO address is a breach: ERROR, no dial.
`address' is a plain string on the wire, so an empty one is the daemon's
zero value rather than an absence it is allowed to send.  There is
nothing to dial, and dialing anything would be inventing a daemon."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon primary
    (agent-repl-itest-host--with-link-and-subscription primary ref
      (agent-repl-itest--with-second-daemon primary successor
        (agent-repl-itest--script primary "SelectWorkspace"
                                  '((error . ((transferringAway . ())))))
        ;; Act.
        (agent-repl-host-select agent-repl-itest-host--ws)
        ;; Assert.
        (agent-repl-itest--await-log
         primary "elisp.host.transferring-away-without-address" "error")
        (should (null (agent-repl-link-successor)))
        (should (null (agent-repl-itest--subscribers successor "daemon")))
        (should (null (agent-repl-itest--calls successor "AdoptHostWorkspace")))))))

;; audit-2 #10
(ert-deftest agent-repl-itest-host-transfer-of-one-workspace-leaves-the-other ()
  "A `transferred' for A moves ONLY A; B keeps flowing from the old daemon.
daemon.md handover step 3: \"each workspace's updates flow ONLY from the
daemon that currently owns it\".  During the dual-attach window the old
daemon still owns everything it has not released, so a client that
adopted the whole fleet on the first release would take workspaces away
from the daemon still serving them."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon primary
    (agent-repl-itest-host--with-two-subscriptions primary ref-a ref-b
      (agent-repl-itest--with-second-daemon primary successor
        (let ((successor-conn (agent-repl-connect-open
                               (agent-repl-itest-daemon-address successor))))
          (unwind-protect
              (cl-letf (((symbol-function 'agent-repl-link-successor)
                         (lambda () successor-conn))
                        ((symbol-function 'agent-repl-frontend-reload-webview) #'ignore))
                ;; Act: only A is released.
                (agent-repl-itest--push primary "host" '((transferred . ()))
                                        (plist-get ref-a :id))
                (agent-repl-itest--await-subscriber
                 successor "host" (plist-get ref-a :id))
                ;; Assert: B never moved.
                (should (equal 1 (length (agent-repl-itest--subscribers
                                          primary "host" (plist-get ref-b :id)))))
                (should (null (agent-repl-itest--subscribers
                               successor "host" (plist-get ref-b :id))))
                (should (equal 1 (length (agent-repl-itest--calls
                                          successor "AdoptHostWorkspace"))))
                ;; And B's own state still arrives from the OLD daemon.
                (agent-repl-itest-host--push-host
                 primary (plist-get ref-b :id) (agent-repl-itest-host--live 'merging))
                (agent-repl-itest--wait-until
                 (lambda () (eq (agent-repl-host-composer-gate ws-b) :merging))
                 nil "B's gate to still be updated by the old daemon")
                (should (eq (agent-repl-host-composer-gate ws-b) :merging)))
            (agent-repl-connect-close successor-conn)))))))

;; audit-2 #11
(ert-deftest agent-repl-itest-host-reload-webapp-reloads-only-that-workspace ()
  "`reload_webapp' reloads EXACTLY the pushed workspace's webview.
Scenario 7 says \"for that workspace only\"; with a single workspace
subscribed a reload-everything implementation is indistinguishable from
the contract, so the assertion needs a second one to leave alone."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest-host--with-two-subscriptions daemon ref-a ref-b
      (let ((reloaded nil))
        (cl-letf (((symbol-function 'agent-repl-frontend-reload-webview)
                   (lambda (ws) (push ws reloaded))))
          ;; Act: A only.  B's own stream is pushed a state afterwards and
          ;; waited for, so a broadcast reload would have been recorded by
          ;; the time the assertion runs.
          (agent-repl-itest--push daemon "host" '((reloadWebapp . ()))
                                  (plist-get ref-a :id))
          (agent-repl-itest-host--push-host
           daemon (plist-get ref-b :id) (agent-repl-itest-host--live 'merging))
          (agent-repl-itest--wait-until
           (lambda () (eq (agent-repl-host-composer-gate ws-b) :merging))
           nil "B's own later push to be applied")
          ;; Assert.
          (should (equal reloaded (list agent-repl-itest-host--ws))))))))

;; audit-2 #12
(ert-deftest agent-repl-itest-host-stream-ended-by-the-producer-is-an-error ()
  "A producer-side END of a STANDING host stream is a failure.
fanout §3: \"an end frame or process death on a standing stream is always
a failure\".  The link's own reconnect owns the recovery, so host.el
records the fact and drops the dead stream — and KEEPS the last state,
because an outage the reconnect will cover must not blank the editor."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest-host--with-subscription daemon ref
      (agent-repl-itest-host--push-host
       daemon (plist-get ref :id) (agent-repl-itest-host--live 'open))
      (agent-repl-itest--wait-until
       (lambda () (agent-repl-host-state agent-repl-itest-host--ws))
       nil "the host push to apply")
      ;; Act: a CLEAN end frame, no error at all.
      (agent-repl-itest--end daemon "host" (plist-get ref :id))
      ;; Assert.
      (agent-repl-itest--await-log daemon "elisp.host.stream-lost" "error")
      (agent-repl-itest--wait-until
       (lambda () (null (agent-repl-host-stream agent-repl-itest-host--ws)))
       nil "the dead stream to be dropped")
      (should (null (agent-repl-host-stream agent-repl-itest-host--ws)))
      (should (agent-repl-host-state agent-repl-itest-host--ws)))))

(ert-deftest agent-repl-itest-host-stream-ended-after-the-planned-ending-is-info ()
  "A clean end after the daemon's planned ending is a stand-down, not a loss.
The daemon's last frame (`DaemonStreamEnding') says the end is PLANNED, so
host.el records it at INFO and drops the stream to follow the live daemon."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest-host--with-subscription daemon ref
      (agent-repl-itest-host--push-host
       daemon (plist-get ref :id) (agent-repl-itest-host--live 'open))
      (agent-repl-itest--wait-until
       (lambda () (agent-repl-host-state agent-repl-itest-host--ws))
       nil "the host push to apply")
      ;; Act: the planned ending, then a clean end frame.
      (agent-repl-itest--push daemon "host" '((ending . ())) (plist-get ref :id))
      (agent-repl-itest--await-log daemon "elisp.host.stream-ending" "info")
      (agent-repl-itest--end daemon "host" (plist-get ref :id))
      ;; Assert.
      (agent-repl-itest--await-log daemon "elisp.host.stream-ended-planned" "info")
      (should-not (agent-repl-itest--logged-p daemon "elisp.host.stream-lost" "error")))))

;; audit-2 #12
(ert-deftest agent-repl-itest-host-stream-aborted-by-the-producer-is-an-error ()
  "A producer-side ABORT of a standing host stream is a failure too.
The same contract as the clean end frame, reached by the other
producer-side death: no end frame is written at all, the transport simply
goes away."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest-host--with-subscription daemon ref
      (agent-repl-itest-host--push-host
       daemon (plist-get ref :id) (agent-repl-itest-host--live 'open))
      (agent-repl-itest--wait-until
       (lambda () (agent-repl-host-state agent-repl-itest-host--ws))
       nil "the host push to apply")
      ;; Act.
      (agent-repl-itest--end daemon "host" (plist-get ref :id) nil t)
      ;; Assert.
      (agent-repl-itest--await-log daemon "elisp.host.stream-lost" "error")
      (agent-repl-itest--wait-until
       (lambda () (null (agent-repl-host-stream agent-repl-itest-host--ws)))
       nil "the dead stream to be dropped")
      (should (null (agent-repl-host-stream agent-repl-itest-host--ws)))
      (should (agent-repl-host-state agent-repl-itest-host--ws)))))

;; audit-2 #13
(ert-deftest agent-repl-itest-host-two-session-arms-set-is-a-breach ()
  "A `session' oneof with TWO arms set is a contract breach.
fanout §14 scenario 9 \"two arms → ERROR\"; §0 \"a oneof with two arms
set\".  The fake refuses this before the wire — protojson cannot even
represent it — so the DECODER is pinned directly, exactly as the
`hibernated' arm is."
  ;; Arrange / Act / Assert.
  (should-error
   (agent-repl-wire-decode-watch-host-workspace-response
    '((host . ((none . nil)
               (existing . ((id . ((value . "host-session-1")))
                            (live . ((generation . ((value . "gen-1")))
                                     (shimAttached . t)
                                     (backfill . ((done . nil)))
                                     (open . nil)))))
               (naming . nil)))))
   :type 'agent-repl-wire-error))

;; audit-2 #13
(ert-deftest agent-repl-itest-host-two-composer-arms-set-is-a-breach ()
  "A `composer' oneof with both `open' and `merging' set is a breach.
The same rule one level deeper: the standing session's composer gate is a
oneof too, and \"exactly one arm\" is not weaker inside a nested message."
  ;; Arrange / Act / Assert.
  (should-error
   (agent-repl-wire-decode-watch-host-workspace-response
    '((host . ((existing . ((id . ((value . "host-session-1")))
                            (live . ((generation . ((value . "gen-1")))
                                     (shimAttached . t)
                                     (backfill . ((done . nil)))
                                     (open . nil)
                                     (merging . nil)))))
               (naming . nil)))))
   :type 'agent-repl-wire-error))

;; audit-2 #14
(ert-deftest agent-repl-itest-host-select-refusal-context-carries-the-arm ()
  "A NON-handover Select refusal names its arm and its payload in the log.
fanout §0: \"Dynamic values go in the context.\"  Landing 4's relay says
the host decoder accepts every new `<Rpc>Error' arm, so the arms that are
NOT handover signals must still be reported with the fields they carry —
`workspace_ref_mismatch{registry_dir}' is useless without the dir."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest-host--with-subscription daemon ref
      (agent-repl-itest--script
       daemon "SelectWorkspace"
       '((error . ((workspaceRefMismatch . ((registryDir . "/x")))))))
      ;; Act.
      (agent-repl-host-select agent-repl-itest-host--ws)
      ;; Assert.
      (agent-repl-itest--await-log daemon "elisp.host.select-refused" "error")
      (let ((arguments (apply #'append
                              (mapcar (lambda (record)
                                        (alist-get 'arguments
                                                   (alist-get 'context record)))
                                      (agent-repl-itest--log-entries
                                       daemon "elisp.host.select-refused" "error")))))
        (should (seq-some (lambda (s) (string-match-p ":workspace-ref-mismatch" s))
                          arguments))
        (should (seq-some (lambda (s) (string-match-p "/x" s)) arguments))))))

;; audit-2 #14
(ert-deftest agent-repl-itest-host-select-refusal-does-not-record-the-selection ()
  "A REFUSED SelectWorkspace does not become the last selected workspace.
`agent-repl-host-last-selected-id' is what Emacs believes the daemon
stamped as `current'; a refusal is the daemon saying it stamped nothing,
so recording the id anyway leaves Emacs disagreeing with the daemon about
which workspace is current — and an `unknown_workspace' refusal would
record an id the registry does not even hold."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest-host--with-subscription daemon ref
      (agent-repl-itest--script daemon "SelectWorkspace"
                                '((error . ((unknownWorkspace . ())))))
      (let ((agent-repl-host-last-selected-id nil))
        ;; Act.
        (agent-repl-host-select agent-repl-itest-host--ws)
        (agent-repl-itest--await-log daemon "elisp.host.select-refused" "error")
        ;; Assert.
        (should (null agent-repl-host-last-selected-id))))))

;; audit-2 #15
(ert-deftest agent-repl-itest-host-unfocused-banner-reaches-the-real-backend ()
  "The unfocused banner is recorded by PRODUCTION's own notifier backend.
The harness installs `agent-repl-notify-make-fake-backend', which fixes
the backend's arity and its recorded shape (WS TITLE MESSAGE ACTIVATE)
beside the caller — a test that stubs `agent-repl--notify' instead
asserts the message text and nothing about the contract R-CLICK fixed."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest-host--with-subscription daemon ref
      (cl-letf (((symbol-function 'agent-repl--emacs-focused-p) (lambda (&rest _) nil))
                ((symbol-function 'agent-repl-status-blink-tab)
                 (lambda (&rest _) (error "an unfocused Emacs must not blink a tab"))))
        ;; Act.
        (agent-repl-itest-host--push-notification
         daemon (plist-get ref :id) '((agentAddressed . ())))
        ;; Assert: through the backend the harness installed.
        (agent-repl-itest--wait-until (lambda () agent-repl-itest-notifications) nil
                                      "the desktop banner to reach the backend")
        (let ((record (car agent-repl-itest-notifications)))
          (should (equal (nth 0 record) agent-repl-itest-host--ws))
          (should (equal (nth 1 record)
                         (agent-repl-host-display-title agent-repl-itest-host--ws)))
          (should (equal (nth 2 record) "Agent needs you"))
          (should (functionp (nth 3 record))))))))

;; audit-2 #16
(ert-deftest agent-repl-itest-host-naming-title-renames-the-input-buffer ()
  "A `naming.title' push RENAMES the workspace's buffers.
fanout §7: \"buffer titles use `naming.title', else `naming.slug', else
the row name\".  TITLES NAME THE BUFFERS — an accessor that answers the
right string while every buffer keeps its old name satisfies nothing the
user can see."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest-host--with-subscription daemon ref
      (let ((buffer (generate-new-buffer " *itest-host-input*")))
        (unwind-protect
            (let ((before (buffer-name buffer)))
              (agent-repl--ws-put agent-repl-itest-host--ws :input-buffer buffer)
              ;; Act.
              (agent-repl-itest-host--push-host
               daemon (plist-get ref :id)
               '((existing . ((id . ((value . "host-session-1")))
                              (live . ((generation . ((value . "gen-1")))
                                       (shimAttached . t)
                                       (backfill . ((done . ())))
                                       (open . ())))))
                 (naming . ((title . "Refactor the codec")))))
              ;; Assert.
              (agent-repl-itest--wait-until
               (lambda () (and (buffer-live-p buffer)
                               (not (equal (buffer-name buffer) before))))
               nil "the input buffer to be renamed from the pushed title")
              (should (string-match-p (regexp-quote "Refactor the codec")
                                      (buffer-name buffer))))
          (kill-buffer buffer))))))

;; audit-2 #46
(ert-deftest agent-repl-itest-host-registers-the-workspace-activation-hook ()
  "PRODUCTION installs the tab-switch hook once persp-mode loads.
fanout §7: `agent-repl-host--on-workspace-activated' is \"called from
workspace.el's perspective-activated hook\", registered through
`agent-repl--ws-add-activated-hook' — a `with-eval-after-load' on
persp-mode.  persp-mode is absent in this batch harness, so that form has
never run in any test: the roster suite registers the function BY HAND to
get a real SelectWorkspace, which means a production that dropped its
registration would break every tab switch and no test would notice.

Providing the feature is what runs the queued form, and it is the only
way to observe the registration without persp-mode itself.  The hook
variables are scratch-bound and the feature is withdrawn afterwards, so
nothing here leaks into another scenario."
  ;; Arrange.
  (let ((persp-activated-functions nil)
        (persp-before-deactivate-functions nil)
        (already (featurep 'persp-mode)))
    (unwind-protect
        (progn
          ;; Act: loading persp-mode is exactly what the registration waits on.
          (unless already (provide 'persp-mode))
          ;; Assert.
          (should (memq #'agent-repl-host--on-workspace-activated
                        persp-activated-functions)))
      (unless already (setq features (delq 'persp-mode features))))))

;;;; ---- Audit-3 additions (R-SUITE-3) ----
;;
;; Findings 18-32 of docs/overhaul/reports/elisp-suite-audit-3.md.  Kept in
;; their own section so they merge cleanly beside concurrent edits above.
;;
;; Finding #30 ("`transferring_away' naming an address DIFFERENT from the
;; standing successor", real link) is DISPUTED and has no test here: see
;; this suite's final report for the evidence (it contradicts the very
;; guard test-integration-link.el's own finding #8 of this same audit pins
;; as PRODUCTION's deliberate behavior).

;; audit-3 #18
(ert-deftest agent-repl-itest-host-not-yet-adopted-retry-is-paced-by-the-defcustom ()
  "The `not_yet_adopted' retry is scheduled at EXACTLY the defcustom's delay.
R-RED-HOST \"paced by `agent-repl-host-handover-retry-delay'\" -- the
existing retried test (audit-2 #7) only asserts `>= 2' adopts, so a
synchronous tight loop would pass it.  `run-at-time' is intercepted so the
retry NEVER races a real timer: the recorded delay must equal the
defcustom, no second adopt may land before the interception is fired BY
HAND, and exactly one more adopt must land once it is."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon primary
    (agent-repl-itest-host--with-link-and-subscription primary ref
      (agent-repl-itest--with-second-daemon primary successor
        (agent-repl-itest--script successor "AdoptHostWorkspace"
                                  '((error . ((notYetAdopted . ())))))
        (agent-repl-itest-host--announce
         primary (agent-repl-itest-daemon-address successor))
        (agent-repl-itest--wait-until #'agent-repl-link-successor nil
                                      "the successor to be ACCEPTED")
        (let ((scheduled nil)
              (real-run-at-time (symbol-function 'run-at-time)))
          (cl-letf (((symbol-function 'run-at-time)
                     (lambda (delay repeat fn &rest args)
                       (if (eq fn #'agent-repl-host-handle-refusal)
                           (progn (push (cons delay args) scheduled) nil)
                         (apply real-run-at-time delay repeat fn args)))))
            ;; Act: the first refusal.
            (agent-repl-itest--push primary "host" '((transferred . ()))
                                    (plist-get ref :id))
            (agent-repl-itest--await-call successor "AdoptHostWorkspace")
            ;; `elisp.host.not-yet-adopted' logs only from INSIDE
            ;; `agent-repl-host-handle-refusal', which the interception below
            ;; keeps from running at all until fired by hand -- awaiting it
            ;; here would time out.  `adopt-handover-refusal' is what
            ;; `agent-repl-host--on-refused' logs BEFORE scheduling the retry.
            (agent-repl-itest--await-log primary "elisp.host.adopt-handover-refusal" "info")
            ;; Assert: paced by exactly the defcustom, and not re-scheduled.
            (agent-repl-itest--wait-until (lambda () scheduled) nil
                                          "the retry to be scheduled")
            (should (equal 1 (length scheduled)))
            (should (equal (caar scheduled) agent-repl-host-handover-retry-delay))
            (should (equal 1 (length (agent-repl-itest--calls
                                      successor "AdoptHostWorkspace"))))
            ;; The successor finishes taking the workspace over.
            (agent-repl-itest--script successor "AdoptHostWorkspace" '((success . ())))
            ;; Act: fire the schedule BY HAND -- no real timer, no race.
            (apply #'agent-repl-host-handle-refusal (cdar scheduled))
            ;; Assert: EXACTLY two adopts total.
            (agent-repl-itest--await-log primary "elisp.host.not-yet-adopted" "info")
            (agent-repl-itest--await-call successor "AdoptHostWorkspace" 2)
            (should (equal 2 (length (agent-repl-itest--calls
                                      successor "AdoptHostWorkspace"))))))))))

;; audit-3 #19
(ert-deftest agent-repl-itest-host-adopt-itself-transferring-away-redials ()
  "`transferring_away{address}' answered on ADOPT ITSELF redials there too.
`endpoint_adopt_host_workspace.proto' declares the same handover arm on
every per-workspace rpc's error type, but only SelectWorkspace exercised
it before this.  The FIRST successor's own adopt call is refused, naming a
SECOND, different daemon -- and the redial walk that ordinarily answers a
Select refusal answers this one exactly the same way."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon primary
    (agent-repl-itest-host--with-subscription primary ref
      (agent-repl-itest--with-second-daemon primary successor
        (agent-repl-itest--with-second-daemon primary other
          (let* ((successor-conn (agent-repl-connect-open
                                  (agent-repl-itest-daemon-address successor)))
                 (other-conn (agent-repl-connect-open
                              (agent-repl-itest-daemon-address other)))
                 (other-address (agent-repl-itest-daemon-address other))
                 (dialed nil))
            (unwind-protect
                (cl-letf (((symbol-function 'agent-repl-link-successor)
                           (lambda () successor-conn))
                          ((symbol-function 'agent-repl-link-dial-successor)
                           (lambda (address) (push address dialed) other-conn)))
                  (agent-repl-itest--script
                   successor "AdoptHostWorkspace"
                   `((error . ((transferringAway . ((address . ,other-address)))))))
                  (agent-repl-itest--gate other "AdoptHostWorkspace")
                  (unwind-protect
                      (progn
                        ;; Act.
                        (agent-repl-itest--push primary "host" '((transferred . ()))
                                                (plist-get ref :id))
                        ;; Assert: the refusal is INFO, the redial follows, and
                        ;; it lands on the NAMED daemon while the old stream
                        ;; stands -- caught mid-flight by the gate.
                        (agent-repl-itest--await-log
                         primary "elisp.host.adopt-handover-refusal" "info")
                        (agent-repl-itest--await-log primary "elisp.host.redial" "info")
                        (agent-repl-itest--await-call other "AdoptHostWorkspace")
                        (should (equal dialed (list other-address)))
                        (should (equal 1 (length (agent-repl-itest--subscribers
                                                  primary "host" (plist-get ref :id)))))
                        (should (equal 1 (length (agent-repl-itest--calls
                                                  successor "AdoptHostWorkspace")))))
                    (agent-repl-itest--release-gate other "AdoptHostWorkspace"))
                  ;; Assert: released, the adopt lands, at the NAMED address.
                  (agent-repl-itest--await-subscriber other "host" (plist-get ref :id)))
              (agent-repl-connect-close successor-conn)
              (agent-repl-connect-close other-conn))))))))

;; audit-3 #20
(defconst agent-repl-itest-host--adopt-non-handover-refusal-arms
  '((workspaceRefMismatch ((registryDir . "/x")) ":workspace-ref-mismatch")
    (noTransferAnnounced () ":no-transfer-announced")
    (participantNotExpected () ":participant-not-expected"))
  "Every `AdoptHostWorkspaceError' cause arm that is NOT handover news.
`unknown_workspace' is pinned by the register-refused sibling tests
already; `transferring_away' and `not_yet_adopted' are handover arms and
route through `agent-repl-host-handle-refusal', never through here.")

(ert-deftest agent-repl-itest-host-adopt-non-handover-refusals-are-reported ()
  "Each non-handover AdoptHostWorkspace refusal arm is `elisp.host.adopt-refused'.
Landing-4 relay; fanout §0: dynamic values ride the log CONTEXT.  Only
`unknown_workspace' ever rode through host.el's adopt path before this;
the other two arms never had a test at all, and `workspace_ref_mismatch'
is useless without its `registry_dir'."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon primary
    (agent-repl-itest-host--with-subscription primary ref
      (agent-repl-itest--with-second-daemon primary successor
        (let ((successor-conn (agent-repl-connect-open
                               (agent-repl-itest-daemon-address successor))))
          (unwind-protect
              (cl-letf (((symbol-function 'agent-repl-link-successor)
                         (lambda () successor-conn)))
                (dolist (case agent-repl-itest-host--adopt-non-handover-refusal-arms)
                  (cl-destructuring-bind (wire-key payload keyword-string) case
                    (let ((before (length (agent-repl-itest--log-entries
                                           primary "elisp.host.adopt-refused" "error"))))
                      (agent-repl-itest--script
                       successor "AdoptHostWorkspace"
                       `((error . ((,wire-key . ,payload)))))
                      ;; Act.
                      (agent-repl-itest--push primary "host" '((transferred . ()))
                                              (plist-get ref :id))
                      ;; Assert: waits for a NEW entry -- an earlier iteration's
                      ;; own `adopt-refused' record already satisfies "any
                      ;; entry exists" the moment this loop reaches its second
                      ;; case, which is exactly the flake the harness forbids.
                      (agent-repl-itest--wait-until
                       (lambda () (> (length (agent-repl-itest--log-entries
                                              primary "elisp.host.adopt-refused" "error"))
                                    before))
                       nil (format "the %s arm to be reported" wire-key))
                      (let ((arguments
                             (apply #'append
                                    (mapcar (lambda (record)
                                              (alist-get 'arguments
                                                         (alist-get 'context record)))
                                            (nthcdr before
                                                    (agent-repl-itest--log-entries
                                                     primary "elisp.host.adopt-refused"
                                                     "error"))))))
                        (should (seq-some (lambda (s) (string-match-p
                                                       (regexp-quote keyword-string) s))
                                          arguments))
                        (when (eq wire-key 'workspaceRefMismatch)
                          (should (seq-some (lambda (s) (string-match-p "/x" s))
                                            arguments)))))
                    (should (equal 1 (length (agent-repl-itest--subscribers
                                              primary "host" (plist-get ref :id)))))
                    (should (equal 0 (length (agent-repl-itest--subscribers
                                              successor "host" (plist-get ref :id))))))))
            (agent-repl-connect-close successor-conn)))))))

;; audit-3 #21
(ert-deftest agent-repl-itest-host-adopt-transport-failure-keeps-the-old-stream ()
  "A TRANSPORT failure of AdoptHostWorkspace keeps the OLD stream and `:conn'.
host.el: the old stream is \"kept standing on every failure path\" -- a
gated call the client gives up on (a shrunk unary timeout) is a transport
failure, distinct from every scripted `error' arm covered above.

AMENDED: THE PAGE IS PUT BACK RATHER THAN NEVER MOVED.  The webview is
now redialed BEFORE the adopt is issued, because the reloaded page is the
participant the successor's rendezvous waits on and only the host can
produce it (host.el, and the deadlock recorded there).  So a failed adopt
can no longer be answered by the page never having moved; it is answered
by the page being back on the daemon that still serves the workspace,
which is the same guarantee stated about the END state.  The `:conn' and
the standing stream assertions are unchanged."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon primary
    (agent-repl-itest-host--with-subscription primary ref
      (agent-repl-itest--with-second-daemon primary successor
        (let ((successor-conn (agent-repl-connect-open
                               (agent-repl-itest-daemon-address successor)))
              (conn-at-reload nil)
              (agent-repl-connect-unary-timeout-seconds 0.3))
          (unwind-protect
              (cl-letf (((symbol-function 'agent-repl-link-successor)
                         (lambda () successor-conn))
                        ((symbol-function 'agent-repl-frontend-reload-webview)
                         (lambda (ws) (push (agent-repl-host-conn ws) conn-at-reload))))
                (agent-repl-itest--gate successor "AdoptHostWorkspace")
                (unwind-protect
                    (progn
                      ;; Act.
                      (agent-repl-itest--push primary "host" '((transferred . ()))
                                              (plist-get ref :id))
                      ;; Assert.
                      (agent-repl-itest--await-log primary "elisp.host.adopt-failed" "error")
                      (should (equal 1 (length (agent-repl-itest--subscribers
                                                primary "host" (plist-get ref :id)))))
                      (should-not (eq (agent-repl-host-conn agent-repl-itest-host--ws)
                                      successor-conn))
                      ;; The LAST navigation the page was given is the URL it
                      ;; is sitting on, and it is not the successor's.
                      (should conn-at-reload)
                      (should-not (eq (car conn-at-reload) successor-conn)))
                  (agent-repl-itest--release-gate successor "AdoptHostWorkspace")))
            (agent-repl-connect-close successor-conn)))))))

;; audit-3 #22
(ert-deftest agent-repl-itest-host-register-transport-failure-answers-on-done-nil ()
  "RegisterWorkspace's TRANSPORT failure calls ON-DONE with nil too.
audit-1 #33 pinned the scripted `error' ARM only; a gated call the client
gives up on is a different fact and reaches ON-DONE through `:on-failure'
instead."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest--gate daemon "RegisterWorkspace")
    (let ((conn (agent-repl-connect-open (agent-repl-itest-daemon-address daemon)))
          (done-called nil) (result :never)
          (agent-repl-connect-unary-timeout-seconds 0.3))
      (unwind-protect
          (progn
            (agent-repl--ws-put agent-repl-itest-host--ws :project-dir (agent-repl-itest--fixture-dir "itest-host-ws"))
            ;; Act.
            (agent-repl-host-register
             conn (agent-repl-itest--fixture-dir "itest-host-ws")
             (lambda (ref) (setq done-called t result ref)))
            ;; Assert.
            (agent-repl-itest--wait-until (lambda () done-called) nil
                                          "RegisterWorkspace to answer via the failure path")
            (should (null result))
            (should (agent-repl-itest--logged-p daemon "elisp.host.register-failed" "error")))
        (agent-repl-itest--release-gate daemon "RegisterWorkspace")
        (agent-repl-connect-close conn)))))

;; audit-3 #23
(ert-deftest agent-repl-itest-host-link-up-skips-a-workspace-without-a-dir ()
  "Link-up WARNS and skips a live workspace with no `:project-dir', not the rest.
fanout §7: \"for every live workspace register its dir, then subscribe\" --
`:project-dir' is the one fact link-up cannot invent, so a workspace
missing it is the one entry the walk cannot touch, and the others must
still go through."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (let ((ws-no-dir (concat agent-repl-itest-host--ws "-no-dir"))
          (conn (agent-repl-connect-open (agent-repl-itest-daemon-address daemon))))
      (unwind-protect
          (progn
            (agent-repl--ws-put agent-repl-itest-host--ws :project-dir (agent-repl-itest--fixture-dir "itest-host-ws"))
            (agent-repl--ws-put ws-no-dir :project-dir nil)
            ;; Act.
            (agent-repl-host-on-link-up conn)
            ;; Assert.
            (agent-repl-itest--await-log daemon "elisp.host.link-up-skipped" "warn")
            (agent-repl-itest--await-call daemon "RegisterWorkspace")
            (should (equal 1 (length (agent-repl-itest--calls daemon "RegisterWorkspace"))))
            (let ((body (car (agent-repl-itest--call-bodies daemon "RegisterWorkspace"))))
              (should (equal (agent-repl-itest--body-field body 'dir) (agent-repl-itest--fixture-dir "itest-host-ws")))))
        (ignore-errors (agent-repl-host-forget agent-repl-itest-host--ws))
        (agent-repl-connect-close conn)))))

;; audit-3 #23
(ert-deftest agent-repl-itest-host-link-up-register-refusal-skips-the-subscribe ()
  "A refused link-up REGISTER logs ERROR and never opens the subscription.
fanout §7 `link-up-register-failed'; a workspace whose register failed has
no ref to subscribe with, so nothing must call `WatchHostWorkspace' for
it."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest--script daemon "RegisterWorkspace"
                              '((error . ((notAWorktree . ())))))
    (let ((conn (agent-repl-connect-open (agent-repl-itest-daemon-address daemon))))
      (unwind-protect
          (progn
            (agent-repl--ws-put agent-repl-itest-host--ws :project-dir (agent-repl-itest--fixture-dir "itest-host-ws"))
            ;; Act.
            (agent-repl-host-on-link-up conn)
            ;; Assert.
            (agent-repl-itest--await-log daemon "elisp.host.link-up-register-failed" "error")
            (should (null (agent-repl-itest--subscribers daemon "host")))
            (should (equal 1 (length (agent-repl-itest--calls daemon "RegisterWorkspace")))))
        (ignore-errors (agent-repl-host-forget agent-repl-itest-host--ws))
        (agent-repl-connect-close conn)))))

(ert-deftest agent-repl-itest-host-link-up-navigates-the-webview-at-the-new-daemon ()
  "After a daemon restart the page is navigated at the daemon that is SERVING.

THE DEFECT THIS PINS, measured in the e2e sandbox: a daemon Emacs
relaunches listens on a FRESH PORT, and the page\='s url names the port it
was opened at.  Stopping the daemon and ensuring another healed the tab
bar, the roster and the composer while the webview went on dialing the
daemon that had exited -- the page reported
`failureArms=[daemonUnreachable]\=' at a url naming the dead port, and the
only way out was the user reaching for `SPC o l\='.

The observable is the URL the navigate was ASKED FOR, because the widget
is navigated rather than remounted."
  ;; Arrange: a mounted webview, and a SECOND daemon to come back on.
  (agent-repl-itest--with-fake-daemon primary
    (agent-repl-itest-host--with-subscription primary _ref
      (agent-repl-itest--with-second-daemon primary successor
        (let ((fresh (agent-repl-connect-open
                      (agent-repl-itest-daemon-address successor)))
              (navigated nil))
          (unwind-protect
              (cl-letf (((symbol-function 'agent-repl--frontend-xwidget-available-p)
                         (lambda () t))
                        ((symbol-function 'agent-repl--frontend-webview-live-widget)
                         (lambda (&rest _) 'fake-widget))
                        ((symbol-function 'agent-repl--frontend-watch-load) #'ignore)
                        ((symbol-function 'agent-repl--frontend-webview-navigate-widget)
                         (lambda (_widget url) (push url navigated)))
                        ((symbol-function 'agent-repl--call-in-background-workspace)
                         (lambda (_ws fn) (funcall fn))))
                (agent-repl--ws-put agent-repl-itest-host--ws :frontend 'gui)
                (agent-repl--ws-put agent-repl-itest-host--ws :project-dir
                                    (agent-repl-itest--fixture-dir "itest-host-ws"))
                (agent-repl--frontend-precreate-webview agent-repl-itest-host--ws)
                (setq navigated nil)
                ;; Act.
                (agent-repl-host-on-link-up fresh)
                ;; Assert.
                (agent-repl-itest--await-call successor "RegisterWorkspace")
                (agent-repl-itest--wait-until (lambda () navigated) nil
                                              "the webview to be re-pointed")
                ;; The workspace's own page host, at the NEW daemon's port.
                (should (string-match-p
                         (format "\\`http://ws-[a-z0-9-]+\\.localhost:%s/"
                                 (car (last (split-string (agent-repl-itest-daemon-address successor) ":"))))
                         (car navigated))))
            (agent-repl-connect-close fresh)))))))

;; audit-3 #24
(ert-deftest agent-repl-itest-host-select-with-no-ref-sends-nothing ()
  "Select on a workspace with no ref yet sends nothing at all.
host.el: \"an unregistered workspace has no identity to select\" -- a
select attempted before RegisterWorkspace has even answered must not
synthesize a call."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (let ((ws (concat agent-repl-itest-host--ws "-unregistered")))
      (agent-repl--ws-put ws :project-dir (agent-repl-itest--fixture-dir ws))
      ;; Act.
      (agent-repl-host-select ws)
      ;; Assert: nothing was ever sent, and nothing ever will be -- there
      ;; is no pending callback left to race.
      (should (null (agent-repl-itest--calls daemon "SelectWorkspace"))))))

;; audit-3 #24
(ert-deftest agent-repl-itest-host-select-with-no-connection-warns-and-sends-nothing ()
  "Select with a ref but NO connection anywhere WARNS and sends nothing.
host.el `elisp.host.select-skipped reason=no-connection' -- distinct from
the no-ref case: the workspace IS registered, but neither its own `:conn'
nor a standing primary exists to send on."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest-host--with-subscription daemon ref
      (agent-repl-host-on-link-down (agent-repl-host-conn agent-repl-itest-host--ws))
      (should (null (agent-repl-host-conn agent-repl-itest-host--ws)))
      (cl-letf (((symbol-function 'agent-repl-link-primary) (lambda () nil)))
        ;; Act.
        (agent-repl-host-select agent-repl-itest-host--ws)
        ;; Assert.
        (agent-repl-itest--await-log daemon "elisp.host.select-skipped" "warn")
        (should (agent-repl-itest--logged-p daemon "elisp.host.select-skipped" "warn"))
        (should (null (agent-repl-itest--calls daemon "SelectWorkspace")))))))

;; audit-3 #24
(ert-deftest agent-repl-itest-host-select-falls-back-to-the-standing-primary ()
  "With `:conn' cleared, Select falls back to `(agent-repl-link-primary)'.
host.el: `(or (agent-repl-host-conn ws) (agent-repl-link-primary))' -- a
link-down clears the per-workspace `:conn', but once a fresh primary
stands, the very next select must find it there rather than staying
skipped forever."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (let ((conn (agent-repl-connect-open (agent-repl-itest-daemon-address daemon)))
          (ref nil))
      (unwind-protect
          (progn
            (agent-repl--ws-put agent-repl-itest-host--ws :project-dir (agent-repl-itest--fixture-dir "itest-host-ws"))
            (agent-repl-host-register conn (agent-repl-itest--fixture-dir "itest-host-ws")
                                      (lambda (r) (setq ref r)))
            (agent-repl-itest--wait-until (lambda () ref) nil "RegisterWorkspace to answer")
            (agent-repl-host-subscribe conn agent-repl-itest-host--ws ref)
            (agent-repl-itest--await-subscriber daemon "host" (plist-get ref :id))
            (agent-repl-host-on-link-down conn)
            (should (null (agent-repl-host-conn agent-repl-itest-host--ws)))
            (cl-letf (((symbol-function 'agent-repl-link-primary) (lambda () conn)))
              ;; Act.
              (agent-repl-host-select agent-repl-itest-host--ws)
              ;; Assert.
              (agent-repl-itest--await-call daemon "SelectWorkspace")
              (should (equal (agent-repl-itest--body-field
                              (car (agent-repl-itest--call-bodies daemon "SelectWorkspace"))
                              'workspace 'id)
                             (plist-get ref :id)))))
        (ignore-errors (agent-repl-host-forget agent-repl-itest-host--ws))
        (agent-repl-connect-close conn)))))

;; audit-3 #25
(ert-deftest agent-repl-itest-host-unfocused-wins-over-the-tab-being-selected ()
  "UNFOCUSED takes precedence even when the pushed workspace IS the current tab.
`endpoint_watch_host_workspace.proto' orders the notification policy's
cases UNFOCUSED first; a decoder that checked \"selected\" before focus
would draw nothing for a notification that arrives while the user has
stepped away with this tab still the last one shown."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest-host--with-subscription daemon ref
      (let ((notified nil))
        (cl-letf (((symbol-function 'agent-repl--emacs-focused-p) (lambda (&rest _) nil))
                  ((symbol-function 'agent-repl--ws-current-name)
                   (lambda (&rest _) agent-repl-itest-host--ws))
                  ((symbol-function 'agent-repl-status-blink-tab)
                   (lambda (&rest _) (error "unfocused must not blink")))
                  ((symbol-function 'agent-repl--notify)
                   (lambda (_ws _title message &rest _) (push message notified))))
          ;; Act.
          (agent-repl-itest-host--push-notification
           daemon (plist-get ref :id) '((agentAddressed . ())))
          ;; Assert.
          (agent-repl-itest--wait-until (lambda () notified) nil "the desktop banner")
          (should (equal (car notified) "Agent needs you"))
          (should-not (agent-repl-itest--logged-p
                       daemon "elisp.host.notification-selected" "info")))))))

;; audit-3 #26
(ert-deftest agent-repl-itest-host-renamed-input-buffer-stays-an-agent-panel ()
  "A titled input buffer is STILL an agent panel by every predicate.
host.el `--apply-naming': \"keeps the name matching
`agent-repl--input-buffer-re'\" -- an accessor that answers the display
title correctly while the rename breaks the regexp, the id lookup, or the
asterisk-stripping rule would still fail every caller that finds the
composer BY NAME rather than by the buffer object it already holds."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest-host--with-subscription daemon ref
      (let ((buffer (agent-repl--create-buffer agent-repl-itest-host--ws "-input")))
        (unwind-protect
            (progn
              (agent-repl--ws-put agent-repl-itest-host--ws :input-buffer buffer)
              ;; Act 1: a title carrying `*', the name form's own delimiter.
              (agent-repl-itest-host--push-host
               daemon (plist-get ref :id)
               `((none . ()) (naming . ((title . "Fix *the* codec")))))
              (agent-repl-itest--wait-until
               (lambda () (string-match-p (regexp-quote "Fix the codec")
                                          (buffer-name buffer)))
               nil "the asterisks to be stripped from the title")
              ;; Assert: still a matched, resolvable agent panel.
              (should (string-match-p agent-repl--input-buffer-re (buffer-name buffer)))
              (should (equal (agent-repl--input-buffer-name-for-id agent-repl-itest-host--ws)
                             (buffer-name buffer)))
              (should-not (string-match-p "\\*the\\*" (buffer-name buffer)))
              ;; Act 2: a SECOND, different title renames again.
              (agent-repl-itest-host--push-host
               daemon (plist-get ref :id)
               `((none . ()) (naming . ((title . "Refactor the parser")))))
              (agent-repl-itest--wait-until
               (lambda () (string-match-p (regexp-quote "Refactor the parser")
                                          (buffer-name buffer)))
               nil "the second title to take effect")
              (should (string-match-p agent-repl--input-buffer-re (buffer-name buffer)))
              (should (equal (agent-repl--input-buffer-name-for-id agent-repl-itest-host--ws)
                             (buffer-name buffer)))
              ;; Act 3: a title EQUAL to WS itself yields the bare canonical name.
              (agent-repl-itest-host--push-host
               daemon (plist-get ref :id)
               `((none . ()) (naming . ((title . ,agent-repl-itest-host--ws)))))
              (agent-repl-itest--wait-until
               (lambda () (equal (buffer-name buffer)
                                 (agent-repl--input-buffer-name agent-repl-itest-host--ws nil)))
               nil "a title equal to WS to yield the bare canonical name")
              (should (equal (buffer-name buffer)
                             (agent-repl--input-buffer-name agent-repl-itest-host--ws nil)))
              ;; Act 4: `naming: {}' (whole-replace) reverts to the canonical name.
              (agent-repl-itest-host--push-host
               daemon (plist-get ref :id) (agent-repl-itest-host--live 'open))
              (agent-repl-itest--wait-until
               (lambda () (equal (buffer-name buffer)
                                 (agent-repl--input-buffer-name agent-repl-itest-host--ws nil)))
               nil "an empty naming to revert the buffer to its canonical name")
              (should (equal (buffer-name buffer)
                             (agent-repl--input-buffer-name agent-repl-itest-host--ws nil))))
          (when (buffer-live-p buffer) (kill-buffer buffer)))))))

;; audit-3 #27
(ert-deftest agent-repl-itest-host-input-buffer-created-after-a-title-push-carries-it ()
  "An input buffer created AFTER a `naming.title' push is named WITH it.
host.el docstring: the input buffer's name is \"built at creation from the
title the daemon has by then\" -- a naming push that lands before the
composer exists must not be lost the moment the composer IS created.

EXPECTED RED until the parallel production fix lands (R-AUDIT3-PROD #27):
`agent-repl--initialize-input-buffer' creates the buffer through
`agent-repl--create-buffer', which always writes the BARE canonical name,
never `agent-repl--input-buffer-name' with the title host.el already has
in `agent-repl-host-state' by then."
  ;; Arrange: the title arrives before any composer exists.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest-host--with-subscription daemon ref
      (agent-repl-itest-host--push-host
       daemon (plist-get ref :id)
       `((none . ()) (naming . ((title . "Refactor the codec")))))
      (agent-repl-itest--wait-until
       (lambda () (equal (agent-repl-host-display-title agent-repl-itest-host--ws)
                         "Refactor the codec"))
       nil "the title to reach host state before the composer exists")
      ;; Act: the composer is created only NOW, through production.
      (cl-letf (((symbol-function 'agent-repl-input-mode) #'ignore)
                ((symbol-function 'agent-repl--history-restore) #'ignore))
        (unwind-protect
            (progn
              (agent-repl--initialize-input-buffer agent-repl-itest-host--ws)
              ;; Assert: the buffer's name carries the title the daemon
              ;; already had.
              (let ((buffer (agent-repl--ws-get agent-repl-itest-host--ws :input-buffer)))
                (should (string-match-p (regexp-quote "Refactor the codec")
                                        (buffer-name buffer)))))
          (let ((buffer (agent-repl--ws-get agent-repl-itest-host--ws :input-buffer)))
            (when (buffer-live-p buffer) (kill-buffer buffer))))))))

;; audit-3 #28
(ert-deftest agent-repl-itest-host-two-input-buffers-may-share-a-title ()
  "Two workspaces handed the SAME vendor title both stay live composers.
host.el: \"UNIQUE-OK ... a rename that ERRORED on the collision would
strand the second composer\" -- `rename-buffer' is called with UNIQUE
non-nil for exactly this reason."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest-host--with-two-subscriptions daemon ref-a ref-b
      (let ((buffer-a (agent-repl--create-buffer agent-repl-itest-host--ws "-input"))
            (buffer-b (agent-repl--create-buffer ws-b "-input")))
        (unwind-protect
            (progn
              (agent-repl--ws-put agent-repl-itest-host--ws :input-buffer buffer-a)
              (agent-repl--ws-put ws-b :input-buffer buffer-b)
              ;; Act: the SAME title, pushed for both workspaces.
              (agent-repl-itest-host--push-host
               daemon (plist-get ref-a :id)
               `((none . ()) (naming . ((title . "Same Vendor Title")))))
              (agent-repl-itest-host--push-host
               daemon (plist-get ref-b :id)
               `((none . ()) (naming . ((title . "Same Vendor Title")))))
              ;; Assert: both live, both still agent panels, distinct names.
              (agent-repl-itest--wait-until
               (lambda () (and (string-match-p "Same Vendor Title" (buffer-name buffer-a))
                               (string-match-p "Same Vendor Title" (buffer-name buffer-b))))
               nil "both buffers to carry the shared title")
              (should (buffer-live-p buffer-a))
              (should (buffer-live-p buffer-b))
              (should-not (agent-repl-itest--logged-p daemon "elisp.rpc.push-invalid" "error"))
              (should (string-match-p agent-repl--input-buffer-re (buffer-name buffer-a)))
              (should (string-match-p agent-repl--input-buffer-re (buffer-name buffer-b)))
              (should-not (equal (buffer-name buffer-a) (buffer-name buffer-b)))
              (should (equal (agent-repl--input-buffer-name-for-id agent-repl-itest-host--ws)
                             (buffer-name buffer-a)))
              (should (equal (agent-repl--input-buffer-name-for-id ws-b)
                             (buffer-name buffer-b))))
          (when (buffer-live-p buffer-a) (kill-buffer buffer-a))
          (when (buffer-live-p buffer-b) (kill-buffer buffer-b)))))))

;; audit-3 #29
(defconst agent-repl-itest-host--fault-kinds
  '((shimStartFailed . :shim-start-failed)
    (shimDied . :shim-died)
    (linkSevered . :link-severed)
    (resumeFailed . :resume-failed)
    (bounceDied . :bounce-died)
    (bounceUnknown . :bounce-unknown)
    (classifierFailed . :classifier-failed)
    (shimReported . :shim-reported)
    (conversationAbandoned . :conversation-abandoned)
    (sessionAbsent . :session-absent)
    (watchOpenRefused . :watch-open-refused)
    (daemonStateUnreadable . :daemon-state-unreadable)
    (adoptionWindowExpired . :adoption-window-expired))
  "Every `HostFault.kind' arm `endpoint_watch_host_workspace.proto' declares.")

(ert-deftest agent-repl-itest-host-declares-every-fault-kind ()
  "The suite's fault-kind table matches the contract's arm count exactly.
A drifted table would let a newly landed kind ship with no decode test at
all, which is how a kind quietly collapses into another one."
  ;; Arrange / Act / Assert.
  (should (equal 13 (length agent-repl-itest-host--fault-kinds))))

(ert-deftest agent-repl-itest-host-every-fault-kind-reaches-the-decoded-plist ()
  "Each of `HostFault''s thirteen kinds decodes to its OWN `:kind' arm.
Landing-4 relay declares thirteen arms; only `link_severed' ever rode a push
in this suite before, and `agent-repl-verbs--fault-lines' prints `detail'
alone -- so a decoder collapsing every OTHER kind into the same arm would
still pass every existing test."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest-host--with-subscription daemon ref
      (dolist (case agent-repl-itest-host--fault-kinds)
        (let ((wire-key (car case)) (expected (cdr case)))
          ;; Act.
          (agent-repl-itest-host--push-host
           daemon (plist-get ref :id)
           (agent-repl-itest-host--live
            'open `(faults . [((detail . "fault detail")
                               (openedAtMs . "1735689600000")
                               (,wire-key . ()))])))
          ;; Assert.
          (agent-repl-itest--wait-until
           (lambda ()
             (let ((fault (car (agent-repl-host-faults agent-repl-itest-host--ws))))
               (eq (plist-get (plist-get fault :kind) :arm) expected)))
           nil (format "the %s kind to decode to %s" wire-key expected))
          (should (eq (plist-get (plist-get (car (agent-repl-host-faults
                                                   agent-repl-itest-host--ws))
                                             :kind)
                                  :arm)
                      expected)))))))

(ert-deftest agent-repl-itest-host-fault-kind-name-reaches-the-health-buffer ()
  "A standing fault's KIND, not only its detail, must reach the health buffer.
proto `HostFault': \"`detail' SUPPLEMENTS the typed kind, never replaces
it.\"  `agent-repl-session-health''s own \"standing host faults\" rendering
prints the kind beside the detail (audit-3 #29 ruling), through the one
shared `agent-repl-verbs--fault-line' formatter the daemon and session
verdicts also use, so a doctor reading only the health buffer can tell a
`shim_died' fault from a `bounce_unknown' one."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest-host--with-subscription daemon ref
      (agent-repl-itest--script daemon "SessionHealth" '((success . ((healthy . ())))))
      (agent-repl-itest-host--push-host
       daemon (plist-get ref :id)
       (agent-repl-itest-host--live
        'open '(faults . [((detail . "store socket unreachable")
                           (openedAtMs . "1735689600000")
                           (bounceUnknown . ()))])))
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
                (string-match-p "bounce-unknown\\|bounceUnknown\\|bounce_unknown"
                                (buffer-string)))))))

(ert-deftest agent-repl-itest-host-fault-with-no-kind-is-a-breach ()
  "A HostFault carrying `detail' and `openedAtMs' but NO kind arm is refused.
`HostFault.kind': \"THE ARM IS THE FAULT CLASS ... a fault with no kind is
a contract breach\" -- R-POLISH found this refused already; nothing pinned
it until now."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest-host--with-subscription daemon ref
      ;; Act: a legal-JSON fault with no kind arm set at all.
      (agent-repl-itest-host--push-host
       daemon (plist-get ref :id)
       (agent-repl-itest-host--live
        'open '(faults . [((detail . "store socket unreachable")
                           (openedAtMs . "1735689600000"))])))
      ;; Assert.
      (agent-repl-itest--await-log daemon "elisp.rpc.push-invalid" "error")
      (should (agent-repl-itest--logged-p daemon "elisp.rpc.push-invalid" "error")))))

;; audit-3 #31
(ert-deftest agent-repl-itest-host-select-not-yet-adopted-retries-onto-the-standing-successor ()
  "`not_yet_adopted' on SELECT is INFO, never `select-refused', and self-heals.
`endpoint_select_workspace.proto' `SelectWorkspaceNotYetAdopted' -- the
SAME retry the adopt path exercises (audit-2 #7), driven end to end
through SelectWorkspace's own refusal for once, with a successor already
standing to retry onto."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon primary
    (agent-repl-itest-host--with-link-and-subscription primary ref
      (agent-repl-itest--with-second-daemon primary successor
        (agent-repl-itest-host--announce
         primary (agent-repl-itest-daemon-address successor))
        (agent-repl-itest--wait-until #'agent-repl-link-successor nil
                                      "the successor to be ACCEPTED")
        (agent-repl-itest--script primary "SelectWorkspace"
                                  '((error . ((notYetAdopted . ())))))
        (agent-repl-itest--script successor "AdoptHostWorkspace" '((success . ())))
        ;; Act.
        (agent-repl-host-select agent-repl-itest-host--ws)
        ;; Assert.
        (agent-repl-itest--await-log primary "elisp.host.select-handover-refusal" "info")
        (should-not (agent-repl-itest--logged-p primary "elisp.host.select-refused" "error"))
        (agent-repl-itest--await-call successor "AdoptHostWorkspace")
        (should (equal 1 (length (agent-repl-itest--calls
                                  successor "AdoptHostWorkspace"))))))))

;; audit-3 #32
(ert-deftest agent-repl-itest-host-link-down-leaves-a-transferred-workspace-untouched ()
  "Link-down forgets ONLY the workspaces still owned by the dying connection.
fanout §7 \"On link down: mark streams gone\"; host.el `elisp.host.link-down
workspaces=N' -- A already lives on the SUCCESSOR when the PRIMARY dies, so
counting every subscribed workspace rather than every workspace OWNED BY
the dying conn would over-count and blank an editor the outage never
touched."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon primary
    (agent-repl-itest-host--with-two-subscriptions primary ref-a ref-b
      (agent-repl-itest--with-second-daemon primary successor
        (let ((successor-conn (agent-repl-connect-open
                               (agent-repl-itest-daemon-address successor))))
          (unwind-protect
              (cl-letf (((symbol-function 'agent-repl-link-successor)
                         (lambda () successor-conn))
                        ((symbol-function 'agent-repl-frontend-reload-webview) #'ignore))
                ;; A transfers to the successor; B stays on the primary.
                (agent-repl-itest--push primary "host" '((transferred . ()))
                                        (plist-get ref-a :id))
                (agent-repl-itest--await-subscriber successor "host" (plist-get ref-a :id))
                (should (eq (agent-repl-host-conn agent-repl-itest-host--ws) successor-conn))
                ;; Act: the PRIMARY connection dies.
                (agent-repl-host-on-link-down conn)
                ;; Assert: A survives untouched; B is dropped; the count is 1.
                (should (eq (agent-repl-host-conn agent-repl-itest-host--ws) successor-conn))
                (should (agent-repl-host-stream agent-repl-itest-host--ws))
                (should (null (agent-repl-host-conn ws-b)))
                (should (null (agent-repl-host-stream ws-b)))
                (should (agent-repl-itest--logged-p primary "elisp.host.link-down" "warn"))
                (let ((entries (agent-repl-itest--log-entries
                                primary "elisp.host.link-down" "warn")))
                  (should (seq-some (lambda (r) (string-match-p
                                                 "workspaces=1"
                                                 (or (alist-get 'message r) "")))
                                    entries))))
            (agent-repl-connect-close successor-conn)))))))

;;;; ---- Audit-3 #30 (R-SUITE-3) ----
;;
;; Finding 30 has two halves.  The SECOND half -- the `awaiting-successor'
;; branch, otherwise reached only through a hand-called `handle-refusal' -- is
;; pinned here against the real link.
;;
;; The FIRST half, as the audit words it ("assert WatchDaemon and adopt land
;; on B, not A"), is NOT pinnable as written, and the test below pins what
;; production actually does instead.  host.el's `--redial-successor' docstring
;; does say "a successor already standing at a DIFFERENT address is a stale
;; handover, so the dial is made either way" -- but the dial it makes goes
;; through `agent-repl-link-dial-successor', which delegates to
;; `agent-repl-link--attach-successor', whose second `cond' arm refuses a
;; changed address outright: it logs `elisp.link.successor-address-changed'
;; ERROR and leaves the standing successor untouched.  That guard is not
;; incidental -- audit-3 #8 pins it, in this same document, as the CORRECT
;; behavior of the announced path.  So host.el's docstring and daemon-link.el's
;; guard contradict each other, and only a ruling can say which one is the
;; contract.  Pinning the audit's wording would pin a fiction; pinning the
;; guard's behavior documents the conflict where the next reader will find it.

;; audit-3 #30
(ert-deftest agent-repl-itest-host-redial-to-a-different-address-hits-the-link-guard ()
  "A `transferring_away' naming an address OTHER than the standing successor.
RULED (audit-3 #30): DAEMON-LINK'S GUARD WINS.  host.el asks
`agent-repl-link-dial-successor' to redial, that lands in
`agent-repl-link--attach-successor', and a `transferring_away' naming an
address OTHER than the standing successor is logged ERROR
\(`elisp.link.successor-address-changed\') and NOT dialed -- the same rule
audit-3 #8 pins for the announced path, so one rule covers both.  The
standing successor is kept and no adopt reaches the third daemon."
  ;; Arrange: successor A is announced and ACCEPTED on the real link.
  (agent-repl-itest--with-fake-daemon primary
    (agent-repl-itest-host--with-link-and-subscription primary ref
      (agent-repl-itest--with-second-daemon primary successor-a
        (agent-repl-itest-host--announce
         primary (agent-repl-itest-daemon-address successor-a))
        (agent-repl-itest--wait-until #'agent-repl-link-successor nil
                                      "successor A to be ACCEPTED")
        ;; A THIRD daemon, at an address the link has never been told about.
        (let ((daemon-b (agent-repl-itest--start-daemon)))
          (unwind-protect
              (let ((address-b (agent-repl-itest-daemon-address daemon-b)))
                (agent-repl-itest--script
                 primary "SelectWorkspace"
                 `((error . ((transferringAway . ((address . ,address-b)))))))
                ;; Act.
                (agent-repl-host-select agent-repl-itest-host--ws)
                ;; Assert: host.el asked for the redial ...
                (agent-repl-itest--await-log primary "elisp.host.redial" "info")
                ;; ... and the link guard refused it, naming both addresses.
                (agent-repl-itest--await-log
                 primary "elisp.link.successor-address-changed" "error")
                ;; The standing successor is STILL A.
                (should (equal (agent-repl-itest-daemon-address successor-a)
                               (agent-repl-connect-connection-address
                                (agent-repl-link-successor))))
                ;; And nothing was ever adopted onto B.
                (should (null (agent-repl-itest--calls
                               daemon-b "AdoptHostWorkspace")))
                (should (null (agent-repl-itest--subscribers daemon-b "daemon"))))
            (agent-repl-itest--stop-daemon daemon-b)))))))

;; audit-3 #30
(ert-deftest agent-repl-itest-host-redial-awaiting-acceptance-defers-the-adopt ()
  "A redial that STANDS but is not yet accepted defers the adopt.
host.el: \"Answers nil while the dial has not been ACCEPTED yet -- the
caller must then wait for acceptance rather than adopt onto an unproven
daemon.\"  The `elisp.host.awaiting-successor' branch is otherwise reached
only through a hand-called `agent-repl-host-handle-refusal'; here the
refusal comes off the real wire and the successor's own `WatchDaemon' is
withheld, so the dial is genuinely pending.  This fails if Emacs ever
adopts onto a daemon that has not proven it is listening."
  ;; Arrange: the successor exists but will not accept its watch yet.
  (agent-repl-itest--with-fake-daemon primary
    (agent-repl-itest-host--with-link-and-subscription primary ref
      (agent-repl-itest--with-second-daemon primary successor
        (agent-repl-itest--gate successor "WatchDaemon")
        (unwind-protect
            (let ((address (agent-repl-itest-daemon-address successor)))
              (agent-repl-itest--script
               primary "SelectWorkspace"
               `((error . ((transferringAway . ((address . ,address)))))))
              ;; Act.
              (agent-repl-host-select agent-repl-itest-host--ws)
              ;; Assert: the dial stands, unaccepted, and the adopt WAITS.
              (agent-repl-itest--await-log primary "elisp.host.redial" "info")
              (agent-repl-itest--await-log
               primary "elisp.host.awaiting-successor" "info")
              (should (null (agent-repl-link-successor)))
              (should (null (agent-repl-itest--calls
                             successor "AdoptHostWorkspace")))
              ;; The workspace is still served by the daemon it has.
              (should (equal (agent-repl-itest-daemon-address primary)
                             (agent-repl-connect-connection-address
                              (agent-repl-host-conn agent-repl-itest-host--ws))))
              ;; Releasing the watch accepts the successor, which wakes the
              ;; deferred adopt -- exactly once.
              (agent-repl-itest--release-gate successor "WatchDaemon")
              (agent-repl-itest--await-call successor "AdoptHostWorkspace")
              (agent-repl-itest--await-subscriber
               successor "host" (plist-get ref :id)))
          (ignore-errors
            (agent-repl-itest--release-gate successor "WatchDaemon")))))))

(provide 'test-integration-host)

;;; test-integration-host.el ends here
