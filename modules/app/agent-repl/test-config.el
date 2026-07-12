;;; test-config.el --- Tests for agent-repl config.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for the early-orphan-cherry-pick recovery defined in
;; `config.el'.  The recovery runs at the top of config.el (before any
;; module file is `require'd) and must therefore not depend on any
;; other agent-repl module having loaded successfully — its only
;; dependencies are built-in Elisp and the early-boundary wrappers
;; `agent-repl--early-git-string' / `agent-repl--early-git-exit-code'
;; (registered in `agent-repl--external-boundary-functions').
;;
;; Per AGENTS.md "No External Processes or External State in Tests",
;; these tests stub the local helpers `--early-cherry-pick-head-at' and
;; `--early-abort-cherry-pick' (which themselves call the registered
;; wrappers).  Stubbing at this layer keeps the production wrappers
;; intact (guard-armed by test-helpers.el) so a regression that bypasses
;; the helpers and reaches a wrapper directly would fail loudly with
;; EXTERNAL BOUNDARY UNMOCKED.
;;
;; The snapshot file is the one piece of real disk IO retained — the
;; recovery's contract is "read and rewrite ~/.claude-emacs/workspaces.el",
;; so a temp file in `temporary-file-directory' models that contract
;; (same pattern as the existing snapshot tests in test-commands.el).

;;; Code:

(require 'ert)
(require 'cl-lib)

;; Load shared stubs first so `config.el' can be loaded in -Q.
(let ((dir (file-name-directory (or load-file-name buffer-file-name))))
  (load (expand-file-name "test-helpers.el" dir) nil t))

;; `config.el' calls `--load-module' for every sub-file; stub it to a
;; no-op so we get the early-recovery defuns + invocation without
;; loading the full module.
(unless (fboundp 'load!)
  (defmacro load! (&rest _args)
    "Test-only no-op stub: skip the sub-module loads in config.el."
    nil))

;; Loading `config.el' also invokes `agent-repl--early-recover-orphan-cherry-picks'
;; against the host's real `~/.claude-emacs/workspaces.el', which is
;; undesirable in a test run.  Suppress that invocation by binding the
;; function to a no-op for the duration of the load.
(cl-letf (((symbol-function 'message) #'ignore))
  (let ((dir (file-name-directory (or load-file-name buffer-file-name))))
    (defun agent-repl--early-recover-orphan-cherry-picks () nil)
    (load (expand-file-name "config.el" dir) nil t)))

;;;; ---- Test helpers ----

(defmacro agent-repl-test--with-snapshot-fixture (path content &rest body)
  "Write CONTENT (a sexp) to PATH, run BODY, then remove PATH on exit."
  (declare (indent 2))
  `(progn
     (with-temp-file ,path
       (let ((print-length nil) (print-level nil))
         (prin1 ,content (current-buffer))))
     (unwind-protect (progn ,@body)
       (when (file-exists-p ,path) (delete-file ,path)))))

(defmacro agent-repl-test--with-redirected-snapshot (snap-path &rest body)
  "Redirect the snapshot lookup to SNAP-PATH while BODY runs.
The recovery resolves the snapshot via
`agent-repl--early-workspace-snapshot-file'; override that to return
SNAP-PATH so tests read/write a controlled temp file instead of the real
`~/.claude-emacs/workspaces.el'."
  (declare (indent 1))
  `(cl-letf (((symbol-function 'agent-repl--early-workspace-snapshot-file)
              (lambda () ,snap-path)))
     ,@body))

(defun agent-repl-test--read-snapshot (path)
  "Return the parsed sexp at PATH (used by tests to inspect rewrites)."
  (with-temp-buffer
    (insert-file-contents path)
    (goto-char (point-min))
    (read (current-buffer))))

;;;; ---- Tests: --early-workspace-snapshot-file ----

(ert-deftest agent-repl-config-test-early-snapshot-file-honors-env ()
  "The early snapshot resolver honors AGENT_REPL_STATE_DIR."
  (let ((process-environment (cons "AGENT_REPL_STATE_DIR=/tmp/statetest" process-environment)))
    (should (equal (agent-repl--early-workspace-snapshot-file)
                   (expand-file-name "/tmp/statetest/workspaces.el")))))

(ert-deftest agent-repl-config-test-early-snapshot-file-defaults-to-claude-emacs ()
  "The early snapshot resolver defaults under ~/.claude-emacs when the
override is unset."
  ;; A bare \"AGENT_REPL_STATE_DIR\" entry (no =) makes getenv return nil.
  (let ((process-environment (cons "AGENT_REPL_STATE_DIR" process-environment)))
    (should (equal (agent-repl--early-workspace-snapshot-file)
                   (expand-file-name "workspaces.el"
                                     (expand-file-name "~/.claude-emacs"))))))

;;;; ---- Tests: --early-recover-orphan-cherry-picks ----

(ert-deftest agent-repl-config-test-early-recovery/empty-in-flight-is-noop ()
  "Empty `:in-flight-merges' in the snapshot is a no-op — no helper
calls, no rewrite."
  (let ((snap (make-temp-file "agent-snap-"))
        (head-calls 0)
        (abort-calls 0))
    (agent-repl-test--with-snapshot-fixture snap
        '(:workspaces (("ws-a" :project-dir "/tmp/a"))
          :merge-queue nil
          :in-flight-merges nil)
      (agent-repl-test--with-redirected-snapshot snap
        (cl-letf (((symbol-function 'agent-repl--early-cherry-pick-head-at)
                   (lambda (_) (cl-incf head-calls) nil))
                  ((symbol-function 'agent-repl--early-abort-cherry-pick)
                   (lambda (_) (cl-incf abort-calls) 0))
                  ((symbol-function 'message) #'ignore))
          (agent-repl--early-recover-orphan-cherry-picks))
        (should (= 0 head-calls))
        (should (= 0 abort-calls))))))

(ert-deftest agent-repl-config-test-early-recovery/missing-file-is-noop ()
  "Snapshot file absent → recovery is a silent no-op (no probe, no abort)."
  (let ((head-calls 0)
        (abort-calls 0))
    (agent-repl-test--with-redirected-snapshot "/definitely/not/a/real/path.el"
      (cl-letf (((symbol-function 'agent-repl--early-cherry-pick-head-at)
                 (lambda (_) (cl-incf head-calls) nil))
                ((symbol-function 'agent-repl--early-abort-cherry-pick)
                 (lambda (_) (cl-incf abort-calls) 0))
                ((symbol-function 'message) #'ignore))
        (agent-repl--early-recover-orphan-cherry-picks))
      (should (= 0 head-calls))
      (should (= 0 abort-calls)))))

(ert-deftest agent-repl-config-test-early-recovery/aborts-when-cherry-pick-head-exists ()
  "An in-flight entry whose target-dir has a live CHERRY_PICK_HEAD must
trigger abort, then enqueue the source ws onto :merge-queue at the
back with :halt-until-human nil."
  (let ((snap (make-temp-file "agent-snap-"))
        (abort-called-with nil))
    (agent-repl-test--with-snapshot-fixture snap
        '(:workspaces (("ws-a" :project-dir "/tmp/a"))
          :merge-queue nil
          :in-flight-merges ((:source-ws "ws-a" :target-dir "/tmp/a" :started-at 1.0)))
      (agent-repl-test--with-redirected-snapshot snap
        (cl-letf (((symbol-function 'agent-repl--early-cherry-pick-head-at)
                   (lambda (dir) (concat dir "/.git/CHERRY_PICK_HEAD")))
                  ((symbol-function 'agent-repl--early-abort-cherry-pick)
                   (lambda (dir) (setq abort-called-with dir) 0))
                  ((symbol-function 'message) #'ignore))
          (agent-repl--early-recover-orphan-cherry-picks))
        (should (equal abort-called-with "/tmp/a"))
        (let* ((raw (agent-repl-test--read-snapshot snap))
               (mq (plist-get raw :merge-queue))
               (ifm (plist-get raw :in-flight-merges)))
          (should (null ifm))
          (should (= 1 (length mq)))
          (should (equal (plist-get (car mq) :source-ws) "ws-a"))
          (should-not (plist-get (car mq) :halt-until-human)))))))

(ert-deftest agent-repl-config-test-early-recovery/carries-target-dir-onto-reenqueued-entry ()
  "A recovered orphan re-enqueues with `:target-dir' set to the in-flight
target dir so the merge rejoins its own per-target+repo bucket."
  (let ((snap (make-temp-file "agent-snap-")))
    (agent-repl-test--with-snapshot-fixture snap
        '(:workspaces (("ws-a" :project-dir "/tmp/a"))
          :merge-queue nil
          :in-flight-merges ((:source-ws "ws-a" :target-dir "/tmp/a" :started-at 1.0)))
      (agent-repl-test--with-redirected-snapshot snap
        (cl-letf (((symbol-function 'agent-repl--early-cherry-pick-head-at)
                   (lambda (dir) (concat dir "/.git/CHERRY_PICK_HEAD")))
                  ((symbol-function 'agent-repl--early-abort-cherry-pick)
                   (lambda (_) 0))
                  ((symbol-function 'message) #'ignore))
          (agent-repl--early-recover-orphan-cherry-picks))
        (let* ((raw (agent-repl-test--read-snapshot snap))
               (mq (plist-get raw :merge-queue)))
          (should (= 1 (length mq)))
          (should (equal (plist-get (car mq) :target-dir) "/tmp/a")))))))

(ert-deftest agent-repl-config-test-early-recovery/no-cherry-pick-head-just-clears-bookkeeping ()
  "Entry whose target-dir has NO CHERRY_PICK_HEAD must NOT trigger an
abort (would error on bare `cherry-pick --abort') — only clears the
bookkeeping entry from :in-flight-merges."
  (let ((snap (make-temp-file "agent-snap-"))
        (abort-called 0))
    (agent-repl-test--with-snapshot-fixture snap
        '(:workspaces (("ws-a" :project-dir "/tmp/a"))
          :merge-queue nil
          :in-flight-merges ((:source-ws "ws-a" :target-dir "/tmp/a" :started-at 1.0)))
      (agent-repl-test--with-redirected-snapshot snap
        (cl-letf (((symbol-function 'agent-repl--early-cherry-pick-head-at)
                   (lambda (_) nil))
                  ((symbol-function 'agent-repl--early-abort-cherry-pick)
                   (lambda (_) (cl-incf abort-called) 0))
                  ((symbol-function 'message) #'ignore))
          (agent-repl--early-recover-orphan-cherry-picks))
        (should (= 0 abort-called))
        (let* ((raw (agent-repl-test--read-snapshot snap))
               (mq (plist-get raw :merge-queue))
               (ifm (plist-get raw :in-flight-merges)))
          (should (null ifm))
          (should (null mq)))))))

(ert-deftest agent-repl-config-test-early-recovery/skips-malformed-entries ()
  "Entries missing :source-ws or :target-dir are skipped — recovery
must not even probe for CHERRY_PICK_HEAD against a partial entry."
  (let ((snap (make-temp-file "agent-snap-"))
        (head-calls 0)
        (abort-calls 0))
    (agent-repl-test--with-snapshot-fixture snap
        '(:workspaces nil
          :merge-queue nil
          :in-flight-merges ((:source-ws nil :target-dir "/tmp/x" :started-at 1.0)
                             (:source-ws "ws-b" :target-dir nil :started-at 2.0)))
      (agent-repl-test--with-redirected-snapshot snap
        (cl-letf (((symbol-function 'agent-repl--early-cherry-pick-head-at)
                   (lambda (_) (cl-incf head-calls) nil))
                  ((symbol-function 'agent-repl--early-abort-cherry-pick)
                   (lambda (_) (cl-incf abort-calls) 0))
                  ((symbol-function 'message) #'ignore))
          (agent-repl--early-recover-orphan-cherry-picks))
        (should (= 0 head-calls))
        (should (= 0 abort-calls))
        (let* ((raw (agent-repl-test--read-snapshot snap))
               (mq (plist-get raw :merge-queue))
               (ifm (plist-get raw :in-flight-merges)))
          (should (null ifm))
          (should (null mq)))))))

;;;; ---- Tests: loaded-version SHA ----

(ert-deftest agent-repl-config-test-version/defvar-defaults-nil ()
  "`agent-repl--version' is declared (the batch load leaves it nil since
the refresh `setq' is gated behind `noninteractive')."
  (should (boundp 'agent-repl--version)))

(ert-deftest agent-repl-config-test-compute-version/returns-trimmed-sha ()
  "`--compute-version' returns the SHA produced by the early-git wrapper."
  (let ((agent-repl--config-file "/tmp/doom/modules/app/agent-repl/config.el"))
    (cl-letf (((symbol-function 'agent-repl--early-git-string)
               (lambda (&rest _args) "deadbeefcafef00d")))
      (should (equal (agent-repl--compute-version) "deadbeefcafef00d")))))

(ert-deftest agent-repl-config-test-compute-version/passes-config-dir-to-git ()
  "`--compute-version' runs `rev-parse HEAD' in the config file's directory
so a linked worktree reports its own SHA."
  (let ((agent-repl--config-file "/tmp/doom/modules/app/agent-repl/config.el")
        (captured nil))
    (cl-letf (((symbol-function 'agent-repl--early-git-string)
               (lambda (&rest args) (setq captured args) "abc123")))
      (agent-repl--compute-version)
      (should (equal captured
                     '("-C" "/tmp/doom/modules/app/agent-repl/"
                       "rev-parse" "HEAD"))))))

(ert-deftest agent-repl-config-test-compute-version/empty-sha-is-nil ()
  "An empty string from git (not a repo, etc.) maps to nil, not \"\"."
  (let ((agent-repl--config-file "/tmp/doom/modules/app/agent-repl/config.el"))
    (cl-letf (((symbol-function 'agent-repl--early-git-string)
               (lambda (&rest _args) "")))
      (should (null (agent-repl--compute-version))))))

(ert-deftest agent-repl-config-test-compute-version/nil-config-file-is-nil ()
  "When the config-file path is unknown, `--compute-version' returns nil
without shelling out to git."
  (let ((agent-repl--config-file nil)
        (git-called nil))
    (cl-letf (((symbol-function 'agent-repl--early-git-string)
               (lambda (&rest _args) (setq git-called t) "abc")))
      (should (null (agent-repl--compute-version)))
      (should-not git-called))))

(ert-deftest agent-repl-config-test-version-command/messages-and-returns-sha ()
  "`agent-repl-version' messages and returns the cached SHA."
  (let ((agent-repl--version "feedface1234")
        (messaged nil))
    (cl-letf (((symbol-function 'message)
               (lambda (fmt &rest args) (setq messaged (apply #'format fmt args)))))
      (should (equal (agent-repl-version) "feedface1234"))
      (should (equal messaged "agent-repl version: feedface1234")))))

(ert-deftest agent-repl-config-test-version-command/unknown-when-nil ()
  "`agent-repl-version' reports the \"unknown\" sentinel when the cached
SHA is nil."
  (let ((agent-repl--version nil)
        (messaged nil))
    (cl-letf (((symbol-function 'message)
               (lambda (fmt &rest args) (setq messaged (apply #'format fmt args)))))
      (should (equal (agent-repl-version) "unknown"))
      (should (equal messaged "agent-repl version: unknown")))))

;;;; ---- Tests: bootstrap-phase emission ----
;;
;; config.el runs before core.el defines the log-severity ladder, and is also
;; the code that reports core.el failing to load.  `--boot-info' / `--boot-warn'
;; must therefore hold the quiet/loud bifurcation on BOTH sides of that
;; boundary: delegating to the ladder once it exists, and degrading to a
;; correctly-pitched bare `message' when it does not.
;;
;; Note this file loads config.el with `load!' stubbed out, so core.el is
;; genuinely absent here — the fallback branch is the default state, and the
;; delegating branch is the one that must be simulated.

(defun agent-repl-test-config--capture-emission (thunk)
  "Run THUNK with `message' stubbed; return a plist (:text T :echoed BOOL).
:echoed is non-nil only when `inhibit-message' was nil at `message' time,
i.e. only when the line actually reached the echo area / modeline."
  (let ((text nil) (echoed nil))
    (cl-letf (((symbol-function 'message)
               (lambda (fmt &rest args)
                 (setq text (apply #'format fmt args)
                       echoed (not inhibit-message)))))
      (funcall thunk))
    (list :text text :echoed echoed)))

(ert-deftest agent-repl-config-test-boot-info/fallback-never-echoes ()
  "Pre-core, `--boot-info' still emits but must NOT reach the echo area."
  (let ((res (agent-repl-test-config--capture-emission
              (lambda () (agent-repl--boot-info "starting up")))))
    (should (string-match-p "\\[agent-repl\\] starting up" (plist-get res :text)))
    (should-not (plist-get res :echoed))))

(ert-deftest agent-repl-config-test-boot-info/fallback-expands-format-args ()
  "Pre-core, `--boot-info' expands its &rest ARGS into FMT."
  (let ((res (agent-repl-test-config--capture-emission
              (lambda () (agent-repl--boot-info "loaded %d of %d" 3 7)))))
    (should (string-match-p "loaded 3 of 7" (plist-get res :text)))))

(ert-deftest agent-repl-config-test-boot-info/delegates-once-core-loaded ()
  "Once core.el defines the ladder, `--boot-info' routes through it."
  (let ((delegated nil))
    (cl-letf (((symbol-function 'agent-repl--info)
               (lambda (ws fmt &rest args)
                 (setq delegated (list ws (apply #'format fmt args))))))
      (agent-repl--boot-info "hello %s" "world")
      (should (equal delegated '(nil "hello world"))))))

(ert-deftest agent-repl-config-test-boot-warn/fallback-reaches-echo-area ()
  "Pre-core, `--boot-warn' MUST still reach the echo area — a failing load
is exactly the thing the user has to see."
  (let ((res (agent-repl-test-config--capture-emission
              (lambda () (agent-repl--boot-warn "core.el exploded")))))
    (should (plist-get res :echoed))
    (should (string-match-p "WARNING: core.el exploded" (plist-get res :text)))))

(ert-deftest agent-repl-config-test-boot-warn/delegates-once-core-loaded ()
  "Once core.el defines the ladder, `--boot-warn' routes through it."
  (let ((delegated nil))
    (cl-letf (((symbol-function 'agent-repl--warn)
               (lambda (ws fmt &rest args)
                 (setq delegated (list ws (apply #'format fmt args))))))
      (agent-repl--boot-warn "bad %s" "thing")
      (should (equal delegated '(nil "bad thing"))))))

(provide 'test-config)

;;; test-config.el ends here
