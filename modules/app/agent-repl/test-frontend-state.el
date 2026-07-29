;;; test-frontend-state.el --- ERT tests for frontend-state.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Pure-elisp tests for the frontend.v1 state application layer:
;; RenderState -> render keyword mapping, WorkspaceState application into
;; the workspace.el pushed-state key, StateSnapshot resync, and
;; DegradedNotice surfacing.  No processes, no network — the frame plists
;; are constructed directly (the transport's decode is covered in
;; test-frontend-uds.el).  `message' is shadowed where echo-area output is
;; asserted.
;;
;; One edge case per test, AAA structure.
;;
;; Run with:
;;   emacs -batch -Q -l ert -l test-frontend-state.el -f ert-run-tests-batch-and-exit

;;; Code:

(load (expand-file-name "test-helpers.el" (file-name-directory
                                            (or load-file-name buffer-file-name)))
      nil t)

;; Transport before state: frontend-state.el registers its handlers into
;; frontend-uds.el at load, so load the transport first.
(load (expand-file-name "frontend-uds.el" (file-name-directory
                                            (or load-file-name buffer-file-name)))
      nil t)
(defconst agent-repl-test--frontend-state-file
  (expand-file-name "frontend-state.el"
                    (file-name-directory
                     (or load-file-name buffer-file-name)))
  "Absolute frontend-state module path captured while this test file loads.")
(load agent-repl-test--frontend-state-file nil t)

(require 'cl-lib)

;;;; ---- RenderState -> keyword: one test per enum value -----------------

(defmacro agent-repl-test--deftest-state-map (name state keyword)
  "Define a mapping test NAME asserting STATE maps to render KEYWORD."
  `(ert-deftest ,name ()
     ,(format "RenderState %s maps to %s." state keyword)
     (should (eq (agent-repl--frontend-state->keyword ,state) ,keyword))))

(agent-repl-test--deftest-state-map
 agent-repl-test-state-map-init "RENDER_STATE_INIT" :init)
(agent-repl-test--deftest-state-map
 agent-repl-test-state-map-idle "RENDER_STATE_IDLE" :idle)
(agent-repl-test--deftest-state-map
 agent-repl-test-state-map-idle-async "RENDER_STATE_IDLE_ASYNC" :idle-async)
(agent-repl-test--deftest-state-map
 agent-repl-test-state-map-thinking "RENDER_STATE_THINKING" :thinking)
(agent-repl-test--deftest-state-map
 agent-repl-test-state-map-permission "RENDER_STATE_PERMISSION" :permission)
(agent-repl-test--deftest-state-map
 agent-repl-test-state-map-done "RENDER_STATE_DONE" :done)
(agent-repl-test--deftest-state-map
 agent-repl-test-state-map-stop-failed "RENDER_STATE_STOP_FAILED" :vendor-blocked)
(agent-repl-test--deftest-state-map
 agent-repl-test-state-map-ready "RENDER_STATE_READY" :ready)
(agent-repl-test--deftest-state-map
 agent-repl-test-state-map-vendor-blocked "RENDER_STATE_VENDOR_BLOCKED" :vendor-blocked)
(agent-repl-test--deftest-state-map
 agent-repl-test-state-map-merging "RENDER_STATE_MERGING" :merging)
(agent-repl-test--deftest-state-map
 agent-repl-test-state-map-merge-queued "RENDER_STATE_MERGE_QUEUED" :merge-queued)
(agent-repl-test--deftest-state-map
 agent-repl-test-state-map-merge-conflict "RENDER_STATE_MERGE_CONFLICT" :merge-conflict)
(agent-repl-test--deftest-state-map
 agent-repl-test-state-map-merge-failed "RENDER_STATE_MERGE_FAILED" :merge-failed)
(agent-repl-test--deftest-state-map
 agent-repl-test-state-map-merged "RENDER_STATE_MERGED" :merged)
(agent-repl-test--deftest-state-map
 agent-repl-test-state-map-dead "RENDER_STATE_DEAD" :dead)
(agent-repl-test--deftest-state-map
 agent-repl-test-state-map-degraded "RENDER_STATE_DEGRADED" :degraded)
(agent-repl-test--deftest-state-map
 agent-repl-test-state-map-severed "RENDER_STATE_SEVERED" :severed)
(agent-repl-test--deftest-state-map
 agent-repl-test-state-map-hibernated "RENDER_STATE_HIBERNATED" :hibernated)

(ert-deftest agent-repl-test-state-map-severed-and-hibernated-are-distinct ()
  "The two halves of the old RENDER_STATE_DORMANT decode to DIFFERENT keywords.
A single state used to mean both \"we put this session to sleep on
purpose\" and \"the backend substrate is broken\", so the idle sweeper
reclaiming memory from an untouched workspace was indistinguishable from
a dead shim.  Collapsing them back onto one keyword here would restore
that exactly, whatever the color tables said."
  ;; Act / Assert
  (should-not (eq (agent-repl--frontend-state->keyword "RENDER_STATE_SEVERED")
                  (agent-repl--frontend-state->keyword "RENDER_STATE_HIBERNATED"))))

(ert-deftest agent-repl-test-state-map-unspecified-errors ()
  "RENDER_STATE_UNSPECIFIED is unresolved and errors (no fallback keyword)."
  ;; Act / Assert
  (should-error (agent-repl--frontend-state->keyword "RENDER_STATE_UNSPECIFIED")))

(ert-deftest agent-repl-test-state-map-unknown-errors ()
  "An unknown RenderState string errors (no fallback keyword)."
  ;; Act / Assert
  (should-error (agent-repl--frontend-state->keyword "RENDER_STATE_WAT")))

(ert-deftest agent-repl-test-state-map-nil-errors ()
  "A nil state (protojson omits a default enum) errors, never defaults."
  ;; Act / Assert
  (should-error (agent-repl--frontend-state->keyword nil)))

;;;; ---- WorkspaceState application --------------------------------------

(defun agent-repl-test--register-ws (name &optional dir)
  "Register live workspace NAME owning DIR, so a pushed frame resolves to it.
The daemon names workspaces by CWD; `agent-repl--frontend-ws-name' maps that
back to the persp NAME, and a name with no live workspace behind it is
dropped rather than stub-created."
  (agent-repl--ws-put name :project-dir (or dir (concat "/tmp/" name))))

(ert-deftest agent-repl-test-apply-workspace-state-stores-keyword ()
  "Applying a WorkspaceState stores the mapped keyword under :pushed-render-state."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (agent-repl-test--register-ws "ws1")
    (agent-repl-test--register-ws "a")
    (agent-repl-test--register-ws "b")
    (agent-repl-test--register-ws "ws1")
    ;; Act
    (agent-repl--frontend-apply-workspace-state
     '(:workspace "ws1" :state "RENDER_STATE_THINKING"))
    ;; Assert
    (should (eq (agent-repl--ws-get "ws1" :pushed-render-state) :thinking))))

(ert-deftest agent-repl-test-apply-workspace-state-returns-keyword ()
  "Applying a WorkspaceState returns the mapped keyword."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (agent-repl-test--register-ws "ws1")
    (agent-repl-test--register-ws "a")
    (agent-repl-test--register-ws "b")
    ;; Act / Assert
    (should (eq (agent-repl--frontend-apply-workspace-state
                '(:workspace "ws1" :state "RENDER_STATE_DONE"))
               :done))))

(ert-deftest agent-repl-test-apply-workspace-state-overwrites ()
  "A later WorkspaceState overwrites the earlier pushed state (daemon is truth)."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (agent-repl-test--register-ws "ws1")
    (agent-repl-test--register-ws "a")
    (agent-repl-test--register-ws "b")
    (agent-repl--frontend-apply-workspace-state
     '(:workspace "ws1" :state "RENDER_STATE_THINKING"))
    ;; Act
    (agent-repl--frontend-apply-workspace-state
     '(:workspace "ws1" :state "RENDER_STATE_IDLE"))
    ;; Assert
    (should (eq (agent-repl--ws-get "ws1" :pushed-render-state) :idle))))

(ert-deftest agent-repl-test-apply-workspace-state-stores-inputs ()
  "The resolution inputs are stored under :pushed-render-state-meta."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (agent-repl-test--register-ws "ws1")
    (agent-repl-test--register-ws "a")
    (agent-repl-test--register-ws "b")
    (agent-repl-test--register-ws "ws1")
    ;; Act
    (agent-repl--frontend-apply-workspace-state
     '(:workspace "ws1" :state "RENDER_STATE_IDLE_ASYNC"
       :turnActive t :liveTaskCount "3" :mergePhase "none"
       :causeKind "task_started" :causeSeq "42"))
    ;; Assert
    (let ((meta (agent-repl--ws-get "ws1" :pushed-render-state-meta)))
      (should (eq (plist-get meta :turn-active) t))
      (should (equal (plist-get meta :live-task-count) "3"))
      (should (equal (plist-get meta :cause-kind) "task_started"))
      (should (equal (plist-get meta :cause-seq) "42")))))

(ert-deftest agent-repl-test-apply-workspace-state-missing-workspace-errors ()
  "A WorkspaceState with no workspace fails loudly (invariant violation)."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (agent-repl-test--register-ws "ws1")
    (agent-repl-test--register-ws "a")
    (agent-repl-test--register-ws "b")
    ;; Act / Assert
    (should-error (agent-repl--frontend-apply-workspace-state
                   '(:state "RENDER_STATE_IDLE")))))

(ert-deftest agent-repl-test-apply-workspace-state-blank-workspace-errors ()
  "A WorkspaceState with a blank workspace fails loudly."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (agent-repl-test--register-ws "ws1")
    (agent-repl-test--register-ws "a")
    (agent-repl-test--register-ws "b")
    ;; Act / Assert
    (should-error (agent-repl--frontend-apply-workspace-state
                   '(:workspace "" :state "RENDER_STATE_IDLE")))))

(ert-deftest agent-repl-test-apply-workspace-state-bad-state-errors ()
  "A WorkspaceState carrying an unmappable state fails loudly."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (agent-repl-test--register-ws "ws1")
    (agent-repl-test--register-ws "a")
    (agent-repl-test--register-ws "b")
    ;; Act / Assert
    (should-error (agent-repl--frontend-apply-workspace-state
                   '(:workspace "ws1" :state "RENDER_STATE_UNSPECIFIED")))))

;;;; ---- StateSnapshot resync --------------------------------------------

(ert-deftest agent-repl-test-apply-snapshot-applies-every-workspace ()
  "A StateSnapshot applies the pushed state for every WorkspaceState in it."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (agent-repl-test--register-ws "ws1")
    (agent-repl-test--register-ws "a")
    (agent-repl-test--register-ws "b")
    ;; Act
    (agent-repl--frontend-apply-snapshot
     '(:workspaces ((:workspace "a" :state "RENDER_STATE_THINKING")
                    (:workspace "b" :state "RENDER_STATE_MERGED"))))
    ;; Assert
    (should (eq (agent-repl--ws-get "a" :pushed-render-state) :thinking))
    (should (eq (agent-repl--ws-get "b" :pushed-render-state) :merged))))

(ert-deftest agent-repl-test-apply-snapshot-returns-count ()
  "A StateSnapshot returns the count of workspace states applied."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (agent-repl-test--register-ws "ws1")
    (agent-repl-test--register-ws "a")
    (agent-repl-test--register-ws "b")
    ;; Act / Assert
    (should (= (agent-repl--frontend-apply-snapshot
                '(:workspaces ((:workspace "a" :state "RENDER_STATE_IDLE")
                               (:workspace "b" :state "RENDER_STATE_IDLE"))))
               2))))

(ert-deftest agent-repl-test-apply-snapshot-empty-is-zero ()
  "An empty StateSnapshot applies nothing and returns 0."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (agent-repl-test--register-ws "ws1")
    (agent-repl-test--register-ws "a")
    (agent-repl-test--register-ws "b")
    ;; Act / Assert
    (should (= (agent-repl--frontend-apply-snapshot '(:workspaces nil)) 0))))

(ert-deftest agent-repl-test-apply-snapshot-applies-sessions-and-tolerates-catalogs ()
  "A snapshot applies workspaces AND rebuilds the SessionView store from
`:sessions'; the `:catalogs' array (no handler here) does not break it."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (agent-repl-test--register-ws "ws1")
    (agent-repl-test--register-ws "a")
    (agent-repl-test--register-ws "b")
    (clrhash agent-repl--frontend-session-views)
    ;; Act — sessions carry ids now; catalogs present but unhandled here.
    (agent-repl--frontend-apply-snapshot
     '(:workspaces ((:workspace "a" :state "RENDER_STATE_IDLE"))
       :sessions ((:sessionId "s_a" :workspace "a" :model "haiku"))
       :catalogs ((:workspace "a" :tasks nil))))
    ;; Assert — the workspace state applied AND the session view is stored.
    (should (eq (agent-repl--ws-get "a" :pushed-render-state) :idle))
    (should (equal (plist-get (agent-repl--frontend-session-view "s_a") :model) "haiku"))))

(ert-deftest agent-repl-test-apply-snapshot-rebuilds-session-store ()
  "A snapshot REBUILDS the roster wholesale, dropping stale entries a bounced
daemon no longer knows."
  ;; Arrange — a stale view lingers before the snapshot.
  (agent-repl-test--with-clean-state
    (agent-repl-test--register-ws "ws1")
    (agent-repl-test--register-ws "a")
    (agent-repl-test--register-ws "b")
    (clrhash agent-repl--frontend-session-views)
    (agent-repl--frontend-store-session-view '(:sessionId "s_stale" :workspace "old"))
    ;; Act
    (agent-repl--frontend-apply-snapshot
     '(:workspaces nil :sessions ((:sessionId "s_new" :workspace "a"))))
    ;; Assert — the stale entry is gone, only the snapshot's roster remains.
    (should-not (agent-repl--frontend-session-view "s_stale"))
    (should (agent-repl--frontend-session-view "s_new"))))

(ert-deftest agent-repl-test-unowned-workspace-state-is-retained-for-startup-safety ()
  "A pre-restore workspace path is not rendered, but its daemon fact is retained."
  (agent-repl-test--with-clean-state
    (let ((agent-repl--frontend-workspace-state-views
           (make-hash-table :test 'equal))
          (state '(:workspace "/not-restored"
                   :sessionId "s_busy"
                   :state "RENDER_STATE_THINKING"
                   :turnActive t)))
      (should-not (agent-repl--frontend-apply-workspace-state state))
      (should (equal (agent-repl--frontend-workspace-state-views-all)
                     (list state))))))

(ert-deftest agent-repl-test-snapshot-replaces-workspace-state-safety-store ()
  "A reconnect snapshot drops raw states the daemon no longer reports."
  (agent-repl-test--with-clean-state
    (let ((agent-repl--frontend-workspace-state-views
           (make-hash-table :test 'equal)))
      (puthash "/stale" '(:workspace "/stale")
               agent-repl--frontend-workspace-state-views)
      (agent-repl--frontend-apply-snapshot
       '(:workspaces ((:workspace "/current"
                       :sessionId "s_1"
                       :state "RENDER_STATE_IDLE"))))
      (should-not (gethash "/stale"
                           agent-repl--frontend-workspace-state-views))
      (should (gethash "/current"
                       agent-repl--frontend-workspace-state-views)))))

(ert-deftest agent-repl-test-apply-snapshot-applies-daemon-view ()
  "A snapshot's `:daemon' member routes into the boot-id note (give-up reset)."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (agent-repl-test--register-ws "ws1")
    (agent-repl-test--register-ws "a")
    (agent-repl-test--register-ws "b")
    (let ((noted nil))
      (cl-letf (((symbol-function 'agent-repl--frontend-note-boot-id)
                 (lambda (boot-id) (setq noted boot-id))))
        ;; Act
        (agent-repl--frontend-apply-snapshot
         '(:workspaces nil :daemon (:bootId "b_1" :protocolVersion "1")))
        ;; Assert
        (should (equal noted "b_1"))))))

;;;; ---- SessionView store + handler -------------------------------------

(ert-deftest agent-repl-test-apply-session-view-stores-by-id ()
  "The sessionView handler upserts the view into the store, keyed by id."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (agent-repl-test--register-ws "ws1")
    (agent-repl-test--register-ws "a")
    (agent-repl-test--register-ws "b")
    (clrhash agent-repl--frontend-session-views)
    ;; Act
    (agent-repl--frontend-apply-session-view
     '(:sessionId "s_1" :workspace "/w" :claudeSessionId "cli-1"))
    ;; Assert
    (should (equal (plist-get (agent-repl--frontend-session-view "s_1") :claudeSessionId) "cli-1"))))

(ert-deftest agent-repl-test-store-session-view-missing-id-errors ()
  "A SessionView with no id fails loudly (No-Silent-Fallbacks)."
  ;; Arrange / Act / Assert
  (should-error (agent-repl--frontend-store-session-view '(:workspace "/w"))))

(ert-deftest agent-repl-test-live-session-id-for-cwd-finds-non-terminal ()
  "The cwd correlation returns the non-terminal session bound to that cwd."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (agent-repl-test--register-ws "ws1")
    (agent-repl-test--register-ws "a")
    (agent-repl-test--register-ws "b")
    (clrhash agent-repl--frontend-session-views)
    (agent-repl--frontend-store-session-view '(:sessionId "s_dead" :workspace "/w" :terminal t))
    (agent-repl--frontend-store-session-view '(:sessionId "s_live" :workspace "/w"))
    ;; Act / Assert — the terminal one is skipped, the live one is returned.
    (should (equal (agent-repl--frontend-live-session-id-for-cwd "/w") "s_live"))))

(ert-deftest agent-repl-test-live-session-id-for-cwd-nil-when-none ()
  "The cwd correlation returns nil when no live session is bound to the cwd."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (agent-repl-test--register-ws "ws1")
    (agent-repl-test--register-ws "a")
    (agent-repl-test--register-ws "b")
    (clrhash agent-repl--frontend-session-views)
    ;; Act / Assert
    (should (null (agent-repl--frontend-live-session-id-for-cwd "/nope")))))

(ert-deftest agent-repl-test-apply-daemon-view-notes-boot-id ()
  "The daemonView handler routes its bootId into `--frontend-note-boot-id'."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (agent-repl-test--register-ws "ws1")
    (agent-repl-test--register-ws "a")
    (agent-repl-test--register-ws "b")
    (let ((noted nil))
      (cl-letf (((symbol-function 'agent-repl--frontend-note-boot-id)
                 (lambda (boot-id) (setq noted boot-id))))
        ;; Act
        (agent-repl--frontend-apply-daemon-view '(:bootId "b_9" :protocolVersion "1"))
        ;; Assert
        (should (equal noted "b_9"))))))

(ert-deftest agent-repl-test-apply-daemon-view-stores-the-view ()
  "The daemonView handler stores the view for the readiness/staleness reads."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (agent-repl-test--register-ws "ws1")
    (agent-repl-test--register-ws "a")
    (agent-repl-test--register-ws "b")
    (let ((agent-repl--frontend-last-daemon-view nil))
      (cl-letf (((symbol-function 'agent-repl--frontend-note-boot-id) #'ignore))
        ;; Act
        (agent-repl--frontend-apply-daemon-view '(:bootId "b_9" :daemonVersion "v2"))
        ;; Assert
        (should (equal (plist-get (agent-repl--frontend-daemon-view) :daemonVersion)
                       "v2"))))))

(ert-deftest agent-repl-test-daemon-view-nil-before-any-frame ()
  "The daemon-view accessor is nil until the first `DaemonView' is pushed."
  ;; Arrange
  (let ((agent-repl--frontend-last-daemon-view nil))
    ;; Act / Assert
    (should (null (agent-repl--frontend-daemon-view)))))

(ert-deftest agent-repl-test-apply-snapshot-stores-the-daemon-view ()
  "A StateSnapshot's `:daemon' arm lands in the daemon-view store."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (agent-repl-test--register-ws "ws1")
    (agent-repl-test--register-ws "a")
    (agent-repl-test--register-ws "b")
    (let ((agent-repl--frontend-last-daemon-view nil))
      (cl-letf (((symbol-function 'agent-repl--frontend-note-boot-id) #'ignore))
        ;; Act
        (agent-repl--frontend-apply-snapshot '(:daemon (:bootId "b_1")))
        ;; Assert
        (should (equal (plist-get (agent-repl--frontend-daemon-view) :bootId) "b_1"))))))

;;;; ---- DaemonView binary mtime (staleness source) ----------------------

(ert-deftest agent-repl-test-daemon-view-mtime-parses-protojson-string ()
  "An int64 mtime arrives as a protojson STRING and converts to seconds."
  ;; Arrange
  (let ((agent-repl--frontend-last-daemon-view '(:daemonBinaryMtimeMs "1700000000000")))
    ;; Act / Assert
    (should (equal 1700000000
                   (agent-repl--frontend-daemon-view-binary-mtime-seconds)))))

(ert-deftest agent-repl-test-daemon-view-mtime-accepts-a-number ()
  "A numerically-decoded mtime converts to seconds just the same."
  ;; Arrange
  (let ((agent-repl--frontend-last-daemon-view '(:daemonBinaryMtimeMs 1700000000000)))
    ;; Act / Assert
    (should (equal 1700000000
                   (agent-repl--frontend-daemon-view-binary-mtime-seconds)))))

(ert-deftest agent-repl-test-daemon-view-mtime-nil-without-a-view ()
  "No pushed view yields nil, never a guessed mtime."
  ;; Arrange
  (let ((agent-repl--frontend-last-daemon-view nil))
    ;; Act / Assert
    (should (null (agent-repl--frontend-daemon-view-binary-mtime-seconds)))))

(ert-deftest agent-repl-test-daemon-view-mtime-nil-when-field-absent ()
  "A view predating the mtime field yields nil."
  ;; Arrange
  (let ((agent-repl--frontend-last-daemon-view '(:bootId "b_1")))
    ;; Act / Assert
    (should (null (agent-repl--frontend-daemon-view-binary-mtime-seconds)))))

(ert-deftest agent-repl-test-daemon-view-mtime-nil-when-nonpositive ()
  "A zero mtime (the daemon's boot-time self-stat failed) yields nil."
  ;; Arrange
  (let ((agent-repl--frontend-last-daemon-view '(:daemonBinaryMtimeMs "0")))
    ;; Act / Assert
    (should (null (agent-repl--frontend-daemon-view-binary-mtime-seconds)))))

(ert-deftest agent-repl-test-daemon-view-mtime-nil-when-unparsable ()
  "A non-numeric mtime yields nil rather than a coerced zero."
  ;; Arrange
  (let ((agent-repl--frontend-last-daemon-view '(:daemonBinaryMtimeMs "garbage")))
    ;; Act / Assert
    (should (null (agent-repl--frontend-daemon-view-binary-mtime-seconds)))))

;;;; ---- SessionInit store + handler (slash-menu source) -----------------

(ert-deftest agent-repl-test-apply-session-init-stores-by-id ()
  "The sessionInit handler stores its SystemInit keyed by session id."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (agent-repl-test--register-ws "ws1")
    (agent-repl-test--register-ws "a")
    (agent-repl-test--register-ws "b")
    (clrhash agent-repl--frontend-session-inits)
    ;; Act
    (agent-repl--frontend-apply-session-init
     '(:sessionId "s_1" :workspace "/w" :init (:slashCommands ("commit" "review"))))
    ;; Assert
    (should (equal (plist-get (agent-repl--frontend-session-init "s_1") :slashCommands)
                   '("commit" "review")))))

(ert-deftest agent-repl-test-store-session-init-missing-id-errors ()
  "A SessionInitView with no id fails loudly (No-Silent-Fallbacks)."
  ;; Arrange / Act / Assert
  (should-error (agent-repl--frontend-store-session-init '(:workspace "/w" :init (:slashCommands ())))))

(ert-deftest agent-repl-test-session-init-nil-for-unknown ()
  "The session-init accessor returns nil for a session with no pushed init."
  (agent-repl-test--with-clean-state
    (agent-repl-test--register-ws "ws1")
    (agent-repl-test--register-ws "a")
    (agent-repl-test--register-ws "b")
    (clrhash agent-repl--frontend-session-inits)
    (should (null (agent-repl--frontend-session-init "s_nope")))))

(ert-deftest agent-repl-test-apply-snapshot-rebuilds-session-inits ()
  "A StateSnapshot rebuilds the session-init roster from its :inits list."
  (agent-repl-test--with-clean-state
    (agent-repl-test--register-ws "ws1")
    (agent-repl-test--register-ws "a")
    (agent-repl-test--register-ws "b")
    (clrhash agent-repl--frontend-session-inits)
    ;; Arrange — a stale init that the snapshot must drop.
    (agent-repl--frontend-store-session-init
     '(:sessionId "s_stale" :init (:slashCommands ("old"))))
    ;; Act
    (agent-repl--frontend-apply-snapshot
     '(:inits ((:sessionId "s_new" :init (:slashCommands ("new"))))))
    ;; Assert — stale gone, new present.
    (should-not (agent-repl--frontend-session-init "s_stale"))
    (should (equal (plist-get (agent-repl--frontend-session-init "s_new") :slashCommands)
                   '("new")))))

(ert-deftest agent-repl-test-state-registers-session-init-handler ()
  "The sessionInit oneof arm is wired to its handler."
  (should (eq (cdr (assoc "sessionInit" agent-repl--uds-frame-handlers))
              #'agent-repl--frontend-apply-session-init)))

(ert-deftest agent-repl-test-state-registers-session-view-handler ()
  "The sessionView oneof arm is wired to its handler."
  (should (eq (cdr (assoc "sessionView" agent-repl--uds-frame-handlers))
              #'agent-repl--frontend-apply-session-view)))

(ert-deftest agent-repl-test-state-registers-daemon-view-handler ()
  "The daemonView oneof arm is wired to its handler."
  (should (eq (cdr (assoc "daemonView" agent-repl--uds-frame-handlers))
              #'agent-repl--frontend-apply-daemon-view)))

;;;; ---- SessionView.death: the reader it never had (F4) ----------------
;;
;; These replace the five DegradedNotice tests that stood here. The banner
;; is retired: degradation is a self-resolving failure card on the
;; conversation plane plus a move on the SSM's degraded axis, so there is no
;; echo-area handler left to test.

(ert-deftest agent-repl-test-session-death-surfaces ()
  "A terminal SessionView's classified death reaches the echo area."
  ;; Arrange
  (agent-repl-test--with-clean-state
   (clrhash agent-repl--frontend-surfaced-deaths)
   (let (echoed)
     (cl-letf (((symbol-function 'message)
                (lambda (fmt &rest args) (setq echoed (apply #'format fmt args)))))
       ;; Act
       (agent-repl--frontend-apply-session-view
        '(:sessionId "s1" :workspace "/w" :terminal t
          :death (:errorClass "ERROR_CLASS_INTERNAL"
                  :errorType "session.shim_died"
                  :message "the agent process exited")))
       ;; Assert
       (should (string-match-p "the agent process exited" echoed))))))

(ert-deftest agent-repl-test-session-death-surfaces-only-once ()
  "A re-pushed terminal SessionView does not re-announce the death."
  ;; Arrange — a terminal view is re-pushed on every snapshot, so without the
  ;; latch one honest report becomes recurring noise.
  (agent-repl-test--with-clean-state
   (clrhash agent-repl--frontend-surfaced-deaths)
   (let ((count 0)
         (view '(:sessionId "s1" :workspace "/w" :terminal t
                 :death (:errorClass "ERROR_CLASS_INTERNAL"
                         :errorType "session.shim_died"
                         :message "the agent process exited"))))
     (cl-letf (((symbol-function 'message) (lambda (&rest _) (setq count (1+ count)))))
       ;; Act
       (agent-repl--frontend-apply-session-view view)
       (agent-repl--frontend-apply-session-view view)
       ;; Assert
       (should (equal count 1))))))

(ert-deftest agent-repl-test-live-session-surfaces-no-death ()
  "A living session announces nothing."
  ;; Arrange
  (agent-repl-test--with-clean-state
   (clrhash agent-repl--frontend-surfaced-deaths)
   (let (echoed)
     (cl-letf (((symbol-function 'message)
                (lambda (fmt &rest args) (setq echoed (apply #'format fmt args)))))
       ;; Act
       (agent-repl--frontend-apply-session-view '(:sessionId "s1" :workspace "/w"))
       ;; Assert
       (should (null echoed))))))

(ert-deftest agent-repl-test-session-death-carries-the-raw-reason ()
  "The death's source detail rides the surfaced text as evidence."
  ;; Arrange
  (agent-repl-test--with-clean-state
   (clrhash agent-repl--frontend-surfaced-deaths)
   (let (echoed)
     (cl-letf (((symbol-function 'message)
                (lambda (fmt &rest args) (setq echoed (apply #'format fmt args)))))
       ;; Act
       (agent-repl--frontend-apply-session-view
        '(:sessionId "s1" :workspace "/w" :terminal t
          :death (:errorClass "ERROR_CLASS_INTERNAL"
                  :errorType "session.ended_unclassified"
                  :message "the session ended"
                  :sourceDetail "some ancient reason")))
       ;; Assert
       (should (string-match-p "some ancient reason" echoed))))))

;;;; ---- Handler registration wiring -------------------------------------

(ert-deftest agent-repl-test-state-load-does-not-dial-before-lazy-daemon ()
  "Loading state handlers does not race the lazy daemon's startup owner."
  ;; Arrange
  (let ((dials 0))
    (cl-letf (((symbol-function 'agent-repl-uds-connect)
               (lambda (&rest _) (cl-incf dials))))
      ;; Act
      (load agent-repl-test--frontend-state-file nil t))
    ;; Assert
    (should (= dials 0))))

(ert-deftest agent-repl-test-state-registers-workspace-state-handler ()
  "Loading frontend-state.el registers the workspaceState handler."
  (should (eq (cdr (assoc "workspaceState" agent-repl--uds-frame-handlers))
              #'agent-repl--frontend-apply-workspace-state)))

(ert-deftest agent-repl-test-state-registers-snapshot-handler ()
  "Loading frontend-state.el registers the snapshot handler."
  (should (eq (cdr (assoc "snapshot" agent-repl--uds-frame-handlers))
              #'agent-repl--frontend-apply-snapshot)))

(ert-deftest agent-repl-test-state-degraded-notice-arm-stays-retired ()
  "The degradedNotice arm is RETIRED (step 11): no handler, and no longer a
known or ignored frame field at all (the wire arm itself is gone —
reserved 8/\"degraded_notice\" in frontend.proto), so a push from a daemon
old enough to still send it is now the loud unknown-field signal rather
than a settled no-op."
  (should-not (assoc "degradedNotice" agent-repl--uds-frame-handlers))
  (should-not (member "degradedNotice" agent-repl--uds-known-frame-fields))
  (should-not (member "degradedNotice" agent-repl--uds-ignored-frame-fields)))

(ert-deftest agent-repl-test-state-dispatch-end-to-end ()
  "A decoded workspaceState frame dispatched through the transport applies state."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (agent-repl-test--register-ws "ws1")
    (agent-repl-test--register-ws "a")
    (agent-repl-test--register-ws "b")
    ;; Act — decode + dispatch via the real registered handler
    (agent-repl--uds-dispatch-frame
     (agent-repl--uds-decode-frame
      "{\"workspaceState\":{\"workspace\":\"ws1\",\"state\":\"RENDER_STATE_PERMISSION\"}}"))
    ;; Assert
    (should (eq (agent-repl--ws-get "ws1" :pushed-render-state) :permission))))

;;;; ---- State-transition hook -------------------------------------------

(ert-deftest agent-repl-test-state-transition-hook-fires-on-push ()
  "Applying a WorkspaceState runs the state-transition hook with (ws new prev)."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (agent-repl-test--register-ws "ws1")
    (agent-repl-test--register-ws "a")
    (agent-repl-test--register-ws "b")
    ;; `let*': the hook-list init form closes over `captured', which must be
    ;; bound FIRST (a plain `let' evaluates all inits in the outer scope, so
    ;; the closure would capture an empty lexical env and never see `captured').
    (let* (captured
           (agent-repl-ws-state-transition-functions
            (list (lambda (ws new prev) (setq captured (list ws new prev))))))
      ;; Act
      (agent-repl--frontend-apply-workspace-state
       '(:workspace "ws1" :state "RENDER_STATE_MERGING"))
      ;; Assert
      (should (equal captured '("ws1" :merging nil))))))

(ert-deftest agent-repl-test-state-transition-hook-previous-keyword ()
  "The hook receives the prior pushed keyword as PREVIOUS on a later push."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (agent-repl-test--register-ws "ws1")
    (agent-repl-test--register-ws "a")
    (agent-repl-test--register-ws "b")
    (let* (captured
           (agent-repl-ws-state-transition-functions
            (list (lambda (ws new prev) (setq captured (list ws new prev))))))
      (agent-repl--frontend-apply-workspace-state
       '(:workspace "ws1" :state "RENDER_STATE_MERGING"))
      ;; Act — second push transitions merging -> merge-conflict
      (agent-repl--frontend-apply-workspace-state
       '(:workspace "ws1" :state "RENDER_STATE_MERGE_CONFLICT"))
      ;; Assert
      (should (equal captured '("ws1" :merge-conflict :merging))))))

(ert-deftest agent-repl-test-state-transition-hook-subscriber-error-isolated ()
  "A signaling subscriber is caught + logged; state application still succeeds."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (agent-repl-test--register-ws "ws1")
    (agent-repl-test--register-ws "a")
    (agent-repl-test--register-ws "b")
    (let ((agent-repl-ws-state-transition-functions
           (list (lambda (&rest _) (error "boom")))))
      ;; Act
      (agent-repl--frontend-apply-workspace-state
       '(:workspace "ws1" :state "RENDER_STATE_MERGED"))
      ;; Assert — the pushed state landed despite the broken subscriber
      (should (eq (agent-repl--ws-get "ws1" :pushed-render-state) :merged)))))

;;;; ---- Session-ready latch ---------------------------------------------

(ert-deftest agent-repl-test-latch-fires-agent-ready-on-first-push ()
  "The FIRST pushed WorkspaceState arms the :agent-ready latch bit."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (agent-repl-test--register-ws "ws1")
    (agent-repl-test--register-ws "a")
    (agent-repl-test--register-ws "b")
    (let (calls)
      (cl-letf (((symbol-function 'agent-repl--latch-and-maybe-fire-loaded)
                 (lambda (ws key &optional _m) (push (list ws key) calls))))
        ;; Act
        (agent-repl--frontend-apply-workspace-state
         '(:workspace "ws1" :state "RENDER_STATE_INIT"))
        ;; Assert
        (should (equal calls '(("ws1" :agent-ready))))))))

(ert-deftest agent-repl-test-latch-fires-only-once-per-workspace ()
  "A second pushed state for the same workspace does not re-arm the latch."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (agent-repl-test--register-ws "ws1")
    (agent-repl-test--register-ws "a")
    (agent-repl-test--register-ws "b")
    (let (calls)
      (cl-letf (((symbol-function 'agent-repl--latch-and-maybe-fire-loaded)
                 (lambda (ws key &optional _m) (push (list ws key) calls))))
        (agent-repl--frontend-apply-workspace-state
         '(:workspace "ws1" :state "RENDER_STATE_INIT"))
        ;; Act — second push
        (agent-repl--frontend-apply-workspace-state
         '(:workspace "ws1" :state "RENDER_STATE_IDLE"))
        ;; Assert — latch fired exactly once
        (should (= (length calls) 1))))))

;;;; ---- Tests: wire CWDs never reach the workspace log sink ----

;; `agent-repl-test--with-log-sink-on' lives in test-helpers.el: every suite
;; that asserts a path cannot violate the log routing invariant needs it,
;; because the batch harness disables the sink and the ladder then skips
;; identity resolution altogether.

(ert-deftest agent-repl-test-apply-session-view-with-unowned-cwd-does-not-signal ()
  "A SessionView naming a CWD no live workspace owns must not abort the filter.
Frame handlers run inside the connection's process filter, and a wire CWD can
never index the workspace hash, so routing one into the logging ladder made
the daemon's own snapshot replay kill the filter.  The sink must be ENABLED
for this to mean anything: with it off the ladder never resolves an identity
at all, which is why the existing frame tests could not see this."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-log-sink-on
      (clrhash agent-repl--frontend-session-views)
      ;; Act / Assert
      (should (agent-repl--frontend-apply-session-view
               '(:sessionId "s_unowned" :workspace "/nowhere/unowned"))))))

(ert-deftest agent-repl-test-surface-session-death-with-unowned-cwd-does-not-signal ()
  "The no-death branch runs for every replayed frame and must stay silent.
It is the chattiest path in the handler, so it is the one a wire CWD reaches
first."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-log-sink-on
      ;; Act / Assert
      (should-not (agent-repl--frontend-surface-session-death
                   "s_unowned" '(:workspace "/nowhere/unowned"))))))

(ert-deftest agent-repl-test-apply-session-init-with-unowned-cwd-does-not-signal ()
  "The sessionInit handler carries the same wire CWD field as sessionView."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-log-sink-on
      (clrhash agent-repl--frontend-session-inits)
      ;; Act / Assert
      (should (agent-repl--frontend-apply-session-init
               '(:sessionId "s_init_unowned" :workspace "/nowhere/unowned"
                 :init (:slashCommands nil :skills nil)))))))

(provide 'test-frontend-state)

;;; test-frontend-state.el ends here

;;;; ---- The inbound workspace key ---------------------------------------
;;
;; Every daemon frame names its workspace by the session CWD; Emacs keys
;; workspaces by persp NAME.  Feeding the path straight to `agent-repl--ws-put'
;; does not fail — it STUB-CREATES an entry under the path, so the pushed state
;; lands somewhere the tab-bar never reads and every workspace sits at its
;; disconnected colour while the session works normally.

(ert-deftest agent-repl-test-inbound-frame-resolves-cwd-to-workspace-name ()
  "A frame naming the session CWD applies to the workspace that owns it."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "doom" :project-dir "/Users/x/.config/doom")
    ;; Act — the daemon always names the workspace by cwd.
    (agent-repl--frontend-apply-workspace-state
     '(:workspace "/Users/x/.config/doom" :state "RENDER_STATE_THINKING"))
    ;; Assert — the state reached the NAME the renderer reads.
    (should (eq (agent-repl--ws-get "doom" :pushed-render-state) :thinking))))

(ert-deftest agent-repl-test-inbound-frame-does-not-stub-create-under-the-path ()
  "Resolving must not leave a path-keyed stub behind."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "doom" :project-dir "/Users/x/.config/doom")
    ;; Act
    (agent-repl--frontend-apply-workspace-state
     '(:workspace "/Users/x/.config/doom" :state "RENDER_STATE_DONE"))
    ;; Assert — the path is not a workspace.
    (should-not (agent-repl--ws-get "/Users/x/.config/doom" :pushed-render-state))))

(ert-deftest agent-repl-test-inbound-frame-for-an-unowned-cwd-is-dropped ()
  "A cwd no live workspace owns is dropped, never stub-created."
  ;; Arrange
  (agent-repl-test--with-clean-state
    ;; Act — Emacs has nothing open for this path.
    (agent-repl--frontend-apply-workspace-state
     '(:workspace "/Users/x/not-open" :state "RENDER_STATE_THINKING"))
    ;; Assert — no entry invented to hold it.
    (should-not (agent-repl--ws-known-p "/Users/x/not-open"))))

(ert-deftest agent-repl-test-inbound-frame-accepts-a-workspace-name ()
  "A frame already naming a known workspace still applies."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "doom" :project-dir "/Users/x/.config/doom")
    ;; Act
    (agent-repl--frontend-apply-workspace-state
     '(:workspace "doom" :state "RENDER_STATE_IDLE"))
    ;; Assert
    (should (eq (agent-repl--ws-get "doom" :pushed-render-state) :idle))))

;;;; ---- The two context cuts --------------------------------------------

(ert-deftest agent-repl-test-apply-workspace-state-decodes-clearing ()
  "RENDER_STATE_CLEARING decodes to the :clearing keyword."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (agent-repl-test--register-ws "ws1")
    ;; Act
    (agent-repl--frontend-apply-workspace-state
     '(:workspace "ws1" :state "RENDER_STATE_CLEARING"))
    ;; Assert
    (should (eq (agent-repl--ws-get "ws1" :pushed-render-state) :clearing))))

(ert-deftest agent-repl-test-apply-workspace-state-decodes-compacting ()
  "RENDER_STATE_COMPACTING decodes to the :compacting keyword."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (agent-repl-test--register-ws "ws1")
    ;; Act
    (agent-repl--frontend-apply-workspace-state
     '(:workspace "ws1" :state "RENDER_STATE_COMPACTING"))
    ;; Assert
    (should (eq (agent-repl--ws-get "ws1" :pushed-render-state) :compacting))))
