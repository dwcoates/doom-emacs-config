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
(defvar agent-repl-host-update-functions)
(defvar agent-repl-host-last-selected-id)

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
       (ignore-errors (agent-repl-host-unsubscribe agent-repl-itest-host--ws))
       (agent-repl-connect-close conn))))

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
                    ((symbol-function 'agent-repl--notify)
                     (lambda (_ws _title message) (push message notified))))
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
ask's tool name it is programmatic semantics beside the composed text."
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

(ert-deftest agent-repl-itest-host-unknown-push-arm-is-refused ()
  "A push arm the schema does not declare never reaches Emacs at all.
The fake validates every push against the GENERATED response type, so an
invented arm is refused at the control plane — which is itself the pin
that no such arm exists to be handled."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest-host--with-subscription daemon ref
      ;; Act.
      (let ((result (agent-repl-itest--control
                     daemon "/_fake/push"
                     (json-serialize
                      `((stream . "host")
                        (workspace_id . ,(plist-get ref :id))
                        (message . ((hibernated . ()))))))))
        ;; Assert: no `hibernated' arm exists anywhere on the wire.
        (should (equal (car result) 400))))))

(provide 'test-integration-host)

;;; test-integration-host.el ends here
