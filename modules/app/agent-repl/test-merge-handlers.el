;;; test-merge-handlers.el --- Tests for merge-handlers.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Unit tests for the repo-routed `/create-or-update-workspace merge' dispatch layer.
;; Covers:
;;   - `agent-repl--read-merge-handler-config-file' (file parsing)
;;   - `agent-repl--lookup-merge-handler-override' (defcustom lookup)
;;   - `agent-repl--resolve-merge-handler' (precedence + fallbacks)
;;   - `agent-repl--dispatch-merge-handler' (registry invocation + UDS re-route)
;;   - `agent-repl--resolve-merge-handler-symbol' (symbol-only resolution)
;;   - the daemon-routed cherry-pick path (geometry, command shaping, resume)
;;   - `agent-repl--register-merge-handler' (registry mutation)

;;; Code:

(load (expand-file-name "test-helpers.el" (file-name-directory
                                            (or load-file-name buffer-file-name)))
      nil t)

;;;; ---- Fixture helpers ----

(defmacro agent-repl-test--with-temp-repo (var &rest body)
  "Bind VAR to a fresh temp directory acting as a repo root, run BODY, then clean.
The directory is created with `make-temp-file' (deleted on exit)."
  (declare (indent 1))
  `(let ((,var (make-temp-file "agent-repl-test-repo-" t)))
     (unwind-protect
         (progn ,@body)
       (when (file-directory-p ,var)
         (delete-directory ,var t)))))

(defun agent-repl-test--seed-merge-config (repo-root content)
  "Write CONTENT (string) to REPO-ROOT's `.claude/emacs/workspace-merge.eld'."
  (let* ((dir (expand-file-name ".claude/emacs" repo-root))
         (path (expand-file-name "workspace-merge.eld" dir)))
    (make-directory dir t)
    (with-temp-file path (insert content))
    path))

(defmacro agent-repl-test--with-clean-registry (&rest body)
  "Run BODY with an EMPTY merge-handler registry.
Restores the prior registry on exit so tests don't bleed.  `cherry-pick'
is intentionally absent: it is no longer a local handler (the daemon runs
it), and the resolver still returns the `cherry-pick' SYMBOL as its
default regardless of registry membership."
  (declare (indent 0))
  `(let ((agent-repl--merge-handler-registry nil)
         (agent-repl-workspace-merge-handler-overrides nil))
     ,@body))

;;;; ---- Tests: read-merge-handler-config-file ----

(ert-deftest agent-repl-test-merge-config-reads-valid-alist ()
  "A well-formed alist file is parsed into its content."
  (agent-repl-test--with-temp-repo root
    (agent-repl-test--seed-merge-config
     root "((handler . create-pr) (args . (:add-to-merge-queue t)))")
    (let ((config (agent-repl--read-merge-handler-config-file root)))
      (should (equal (alist-get 'handler config) 'create-pr))
      (should (equal (alist-get 'args config)
                     '(:add-to-merge-queue t))))))

(ert-deftest agent-repl-test-merge-config-returns-nil-when-missing ()
  "A repo with no .claude/emacs/workspace-merge.eld file returns nil."
  (agent-repl-test--with-temp-repo root
    (should-not (agent-repl--read-merge-handler-config-file root))))

(ert-deftest agent-repl-test-merge-config-returns-nil-when-malformed ()
  "A file containing unreadable junk returns nil (does not raise)."
  (agent-repl-test--with-temp-repo root
    (agent-repl-test--seed-merge-config root "this is (((not valid sexp")
    (should-not (agent-repl--read-merge-handler-config-file root))))

(ert-deftest agent-repl-test-merge-config-returns-nil-when-not-alist ()
  "A file whose top-level sexp is not a cons returns nil."
  (agent-repl-test--with-temp-repo root
    (agent-repl-test--seed-merge-config root "42")
    (should-not (agent-repl--read-merge-handler-config-file root))))

(ert-deftest agent-repl-test-merge-config-returns-nil-when-repo-root-nil ()
  "A nil repo-root short-circuits to nil without touching the filesystem."
  (should-not (agent-repl--read-merge-handler-config-file nil)))

(ert-deftest agent-repl-test-merge-config-returns-nil-when-repo-missing ()
  "A repo-root pointing at a nonexistent directory returns nil."
  (should-not (agent-repl--read-merge-handler-config-file
               "/nonexistent/agent-repl-test/repo")))

;;;; ---- Tests: lookup-merge-handler-override ----

(ert-deftest agent-repl-test-merge-override-finds-exact-path ()
  "An override entry matching the canonical path is returned."
  (agent-repl-test--with-temp-repo root
    (let ((agent-repl-workspace-merge-handler-overrides
           (list (cons root '((handler . noop))))))
      (let ((config (agent-repl--lookup-merge-handler-override root)))
        (should (equal (alist-get 'handler config) 'noop))))))

(ert-deftest agent-repl-test-merge-override-matches-trailing-slash ()
  "A path with trailing slash matches a canonical entry without one."
  (agent-repl-test--with-temp-repo root
    (let* ((with-slash (file-name-as-directory root))
           (agent-repl-workspace-merge-handler-overrides
            (list (cons root '((handler . noop))))))
      (should (agent-repl--lookup-merge-handler-override with-slash)))))

(ert-deftest agent-repl-test-merge-override-returns-nil-no-match ()
  "No matching override returns nil."
  (agent-repl-test--with-temp-repo root
    (let ((agent-repl-workspace-merge-handler-overrides
           '(("/some/other/repo" . ((handler . noop))))))
      (should-not (agent-repl--lookup-merge-handler-override root)))))

(ert-deftest agent-repl-test-merge-override-returns-nil-when-repo-root-nil ()
  "A nil repo-root short-circuits."
  (let ((agent-repl-workspace-merge-handler-overrides
         '(("/some/repo" . ((handler . noop))))))
    (should-not (agent-repl--lookup-merge-handler-override nil))))

;;;; ---- Tests: resolve-merge-handler ----

(ert-deftest agent-repl-test-resolve-falls-back-to-cherry-pick ()
  "With no config anywhere, resolver returns `(cherry-pick . nil)'."
  (agent-repl-test--with-clean-registry
    (agent-repl-test--with-temp-repo root
      (should (equal (agent-repl--resolve-merge-handler root)
                     '(cherry-pick))))))

(ert-deftest agent-repl-test-resolve-uses-eld-when-present ()
  "Resolver returns the .eld-declared handler symbol + args."
  (agent-repl-test--with-clean-registry
    (agent-repl--register-merge-handler 'noop (lambda (&rest _) nil))
    (agent-repl-test--with-temp-repo root
      (agent-repl-test--seed-merge-config
       root "((handler . noop) (args . (:silent t)))")
      (let ((res (agent-repl--resolve-merge-handler root)))
        (should (eq (car res) 'noop))
        (should (equal (cdr res) '(:silent t)))))))

(ert-deftest agent-repl-test-resolve-uses-override-when-no-eld ()
  "With no .eld, the defcustom override supplies the handler."
  (agent-repl-test--with-clean-registry
    (agent-repl--register-merge-handler 'noop (lambda (&rest _) nil))
    (agent-repl-test--with-temp-repo root
      (let ((agent-repl-workspace-merge-handler-overrides
             (list (cons root '((handler . noop) (args . (:k v)))))))
        (let ((res (agent-repl--resolve-merge-handler root)))
          (should (eq (car res) 'noop))
          (should (equal (cdr res) '(:k v))))))))

(ert-deftest agent-repl-test-resolve-eld-wins-over-override ()
  "When both .eld and defcustom override are set, .eld wins."
  (agent-repl-test--with-clean-registry
    (agent-repl--register-merge-handler 'noop (lambda (&rest _) nil))
    (agent-repl-test--with-temp-repo root
      (agent-repl-test--seed-merge-config root "((handler . noop))")
      (let ((agent-repl-workspace-merge-handler-overrides
             (list (cons root '((handler . cherry-pick))))))
        (should (eq (car (agent-repl--resolve-merge-handler root))
                    'noop))))))

(ert-deftest agent-repl-test-resolve-falls-back-on-unknown-symbol ()
  "An unknown handler symbol falls back to cherry-pick (does not raise)."
  (agent-repl-test--with-clean-registry
    (agent-repl-test--with-temp-repo root
      (agent-repl-test--seed-merge-config
       root "((handler . totally-fake-handler))")
      (should (eq (car (agent-repl--resolve-merge-handler root))
                  'cherry-pick)))))

;;;; ---- Tests: dispatch-merge-handler ----

(ert-deftest agent-repl-test-dispatch-invokes-resolved-handler ()
  "Dispatcher invokes the resolved handler with TARGET-WS + ARGS."
  (agent-repl-test--with-clean-registry
    (let ((captured nil))
      (agent-repl--register-merge-handler
       'noop (lambda (ws args)
               (setq captured (list :ws ws :args args))))
      (agent-repl-test--with-temp-repo root
        (agent-repl-test--seed-merge-config
         root "((handler . noop) (args . (:hello world)))")
        (cl-letf (((symbol-function 'agent-repl--main-worktree-path)
                   (lambda (dir) dir)))
          (agent-repl--dispatch-merge-handler "DWC/foo" root)
          (should (equal (plist-get captured :ws) "DWC/foo"))
          (should (equal (plist-get captured :args) '(:hello world))))))))

(ert-deftest agent-repl-test-dispatch-cherry-pick-routes-over-uds ()
  "With no config, dispatcher routes cherry-pick to the daemon UDS dispatcher."
  (agent-repl-test--with-clean-registry
    (let ((captured nil))
      (cl-letf (((symbol-function 'agent-repl--merge-dispatch-cherry-pick-over-uds)
                 (lambda (ws) (setq captured ws)))
                ((symbol-function 'agent-repl--main-worktree-path)
                 (lambda (dir) dir)))
        (agent-repl-test--with-temp-repo root
          (agent-repl--dispatch-merge-handler "DWC/foo" root)
          (should (equal captured "DWC/foo")))))))

(ert-deftest agent-repl-test-dispatch-normalises-repo-root-to-main-worktree-for-eld ()
  "Dispatcher canonicalises REPO-ROOT through `--main-worktree-path' before
reading the `.eld' file, so a sibling-worktree caller still hits the main
worktree's checked-in config.  Without normalisation a worktree path whose
tree happens not to carry the file would silently fall through to
cherry-pick."
  (agent-repl-test--with-clean-registry
    (let ((captured nil))
      (agent-repl--register-merge-handler
       'noop (lambda (ws args)
               (setq captured (list :ws ws :args args))))
      (agent-repl-test--with-temp-repo main
        (agent-repl-test--with-temp-repo worktree
          (agent-repl-test--seed-merge-config
           main "((handler . noop) (args . (:from main)))")
          (cl-letf (((symbol-function 'agent-repl--main-worktree-path)
                     (lambda (dir)
                       (when (equal dir worktree) main))))
            (agent-repl--dispatch-merge-handler "DWC/foo" worktree)
            (should (equal (plist-get captured :ws) "DWC/foo"))
            (should (equal (plist-get captured :args) '(:from main)))))))))

(ert-deftest agent-repl-test-dispatch-normalises-repo-root-to-main-worktree-for-override ()
  "Dispatcher canonicalises REPO-ROOT through `--main-worktree-path' before
consulting the defcustom override, so a sibling-worktree caller still
matches an override entry keyed by the main repo path."
  (agent-repl-test--with-clean-registry
    (let ((captured nil))
      (agent-repl--register-merge-handler
       'noop (lambda (ws args)
               (setq captured (list :ws ws :args args))))
      (agent-repl-test--with-temp-repo main
        (agent-repl-test--with-temp-repo worktree
          (let ((agent-repl-workspace-merge-handler-overrides
                 (list (cons main '((handler . noop)
                                    (args . (:via override)))))))
            (cl-letf (((symbol-function 'agent-repl--main-worktree-path)
                       (lambda (dir)
                         (when (equal dir worktree) main))))
              (agent-repl--dispatch-merge-handler "DWC/foo" worktree)
              (should (equal (plist-get captured :ws) "DWC/foo"))
              (should (equal (plist-get captured :args)
                             '(:via override))))))))))

(ert-deftest agent-repl-test-dispatch-falls-back-to-repo-root-when-main-worktree-nil ()
  "When `--main-worktree-path' returns nil (git unavailable / not a repo),
dispatcher falls back to the caller-supplied REPO-ROOT for resolution.
Preserves legacy behaviour for tests and any non-git invocation."
  (agent-repl-test--with-clean-registry
    (let ((captured nil))
      (agent-repl--register-merge-handler
       'noop (lambda (ws args)
               (setq captured (list :ws ws :args args))))
      (agent-repl-test--with-temp-repo root
        (agent-repl-test--seed-merge-config
         root "((handler . noop) (args . (:from root)))")
        (cl-letf (((symbol-function 'agent-repl--main-worktree-path)
                   (lambda (_dir) nil)))
          (agent-repl--dispatch-merge-handler "DWC/foo" root)
          (should (equal (plist-get captured :ws) "DWC/foo"))
          (should (equal (plist-get captured :args) '(:from root))))))))

(ert-deftest agent-repl-test-dispatch-errors-on-missing-registry-entry ()
  "Dispatcher signals user-error if a forced non-cherry-pick symbol has no fn.
`onto-master' is forced by the caller (not chosen by the resolver), so an
empty registry leaves it unregistered — the defensive guard fires.  (The
cherry-pick default no longer reaches the registry: it routes over UDS.)"
  (let ((agent-repl--merge-handler-registry nil)
        (agent-repl-workspace-merge-handler-overrides nil))
    (cl-letf (((symbol-function 'agent-repl--main-worktree-path)
               (lambda (dir) dir)))
      (agent-repl-test--with-temp-repo root
        (should-error
         (agent-repl--dispatch-merge-handler "DWC/foo" root t)
         :type 'user-error)))))

;;;; ---- Tests: register-merge-handler ----

(ert-deftest agent-repl-test-register-replaces-existing-entry ()
  "Re-registering a symbol replaces the old function rather than duplicating."
  (agent-repl-test--with-clean-registry
    (agent-repl--register-merge-handler 'noop (lambda (&rest _) 'first))
    (agent-repl--register-merge-handler 'noop (lambda (&rest _) 'second))
    (let ((matches (cl-remove-if-not
                    (lambda (entry) (eq (car entry) 'noop))
                    agent-repl--merge-handler-registry)))
      (should (= (length matches) 1))
      (should (eq (funcall (cdr (car matches))) 'second)))))

;;;; ---- Tests: resolve-merge-handler-symbol ----

(ert-deftest agent-repl-test-resolve-symbol-defaults-to-cherry-pick ()
  "With no config, the symbol resolver returns `cherry-pick'."
  (agent-repl-test--with-clean-registry
    (cl-letf (((symbol-function 'agent-repl--main-worktree-path) (lambda (dir) dir)))
      (agent-repl-test--with-temp-repo root
        (should (eq (agent-repl--resolve-merge-handler-symbol root) 'cherry-pick))))))

(ert-deftest agent-repl-test-resolve-symbol-onto-master-when-forced ()
  "ONTO-MASTER forces the `onto-master' symbol regardless of config."
  (agent-repl-test--with-clean-registry
    (should (eq (agent-repl--resolve-merge-handler-symbol "/any/repo" t)
                'onto-master))))

;;;; ---- Tests: cherry-pick geometry ----

(ert-deftest agent-repl-test-cherry-pick-geometry-resolves-all-three ()
  "Geometry resolves (:source-branch :source-dir :target-dir) from the ws."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "DWC/foo" :project-dir "/src")
    (cl-letf (((symbol-function 'agent-repl--merge-target-dir-for-ws)
               (lambda (_ws) "/tgt"))
              ((symbol-function 'agent-repl--workspace-branch)
               (lambda (_ws) "DWC/foo")))
      (let ((geom (agent-repl--merge-cherry-pick-geometry "DWC/foo")))
        (should (equal (plist-get geom :source-branch) "DWC/foo"))
        (should (equal (plist-get geom :source-dir) "/src"))
        (should (equal (plist-get geom :target-dir) "/tgt"))))))

(ert-deftest agent-repl-test-cherry-pick-geometry-missing-target-errors ()
  "A nil merge target hard-errors before any command could be sent."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "DWC/foo" :project-dir "/src")
    (cl-letf (((symbol-function 'agent-repl--merge-target-dir-for-ws)
               (lambda (_ws) nil))
              ((symbol-function 'agent-repl--workspace-branch)
               (lambda (_ws) "DWC/foo")))
      (should-error (agent-repl--merge-cherry-pick-geometry "DWC/foo")
                    :type 'user-error))))

(ert-deftest agent-repl-test-cherry-pick-geometry-missing-branch-errors ()
  "A nil source branch hard-errors (No-Silent-Fallbacks)."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "DWC/foo" :project-dir "/src")
    (cl-letf (((symbol-function 'agent-repl--merge-target-dir-for-ws)
               (lambda (_ws) "/tgt"))
              ((symbol-function 'agent-repl--workspace-branch)
               (lambda (_ws) nil)))
      (should-error (agent-repl--merge-cherry-pick-geometry "DWC/foo")
                    :type 'user-error))))

(ert-deftest agent-repl-test-cherry-pick-geometry-missing-source-dir-errors ()
  "A missing :project-dir hard-errors."
  (agent-repl-test--with-clean-state
    (cl-letf (((symbol-function 'agent-repl--merge-target-dir-for-ws)
               (lambda (_ws) "/tgt"))
              ((symbol-function 'agent-repl--workspace-branch)
               (lambda (_ws) "DWC/foo")))
      (should-error (agent-repl--merge-cherry-pick-geometry "DWC/foo")
                    :type 'user-error))))

;;;; ---- Tests: cherry-pick dispatch over UDS ----

(defmacro agent-repl-test--with-mocked-merge-geometry (&rest body)
  "Run BODY with the cherry-pick geometry + base git wrappers stubbed.
Source-dir /src, target-dir /tgt, branch DWC/foo, base BASE-SHA."
  (declare (indent 0))
  `(cl-letf (((symbol-function 'agent-repl--merge-target-dir-for-ws)
              (lambda (_ws) "/tgt"))
             ((symbol-function 'agent-repl--workspace-branch)
              (lambda (_ws) "DWC/foo"))
             ((symbol-function 'agent-repl--cherry-pick-base)
              (lambda (_root _branch) "BASE-SHA")))
     ,@body))

(ert-deftest agent-repl-test-cherry-pick-dispatch-shapes-command ()
  "The daemon dispatch sends mergeWorkspace with the geometry fields."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "DWC/foo" :project-dir "/src")
    (let (sent)
      (agent-repl-test--with-mocked-merge-geometry
        (cl-letf (((symbol-function 'agent-repl--uds-send-command)
                   (lambda (field payload &optional ws &rest _)
                     (setq sent (list :field field :payload payload :ws ws))
                     "req-1"))
                  ((symbol-function 'agent-repl--uds-track-command)
                   (lambda (&rest _) "req-1")))
          (agent-repl--merge-dispatch-cherry-pick-over-uds "DWC/foo")
          (should (equal (plist-get sent :field) "mergeWorkspace"))
          (should (equal (plist-get sent :ws) "DWC/foo"))
          (let ((p (plist-get sent :payload)))
            (should (equal (plist-get p :handler) "cherry-pick"))
            (should (equal (plist-get p :sourceBranch) "DWC/foo"))
            (should (equal (plist-get p :sourceDir) "/src"))
            (should (equal (plist-get p :targetDir) "/tgt"))))))))

(ert-deftest agent-repl-test-cherry-pick-dispatch-stashes-geometry-and-marker ()
  "The dispatch stashes target/branch/base + the :daemon-merge-dispatched marker."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "DWC/foo" :project-dir "/src")
    (agent-repl-test--with-mocked-merge-geometry
      (cl-letf (((symbol-function 'agent-repl--uds-send-command)
                 (lambda (&rest _) "req-1"))
                ((symbol-function 'agent-repl--uds-track-command)
                 (lambda (&rest _) "req-1")))
        (agent-repl--merge-dispatch-cherry-pick-over-uds "DWC/foo")
        (should (equal (agent-repl--ws-get "DWC/foo" :resolved-target-dir) "/tgt"))
        (should (equal (agent-repl--ws-get "DWC/foo" :merge-target-branch) "DWC/foo"))
        (should (equal (agent-repl--ws-get "DWC/foo" :merge-base) "BASE-SHA"))
        (should (eq (agent-repl--ws-get "DWC/foo" :daemon-merge-dispatched) t))))))

(ert-deftest agent-repl-test-cherry-pick-dispatch-tracks-command ()
  "The dispatch tracks the sent request-id for ack-failure surfacing."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "DWC/foo" :project-dir "/src")
    (let (tracked)
      (agent-repl-test--with-mocked-merge-geometry
        (cl-letf (((symbol-function 'agent-repl--uds-send-command)
                   (lambda (&rest _) "req-9"))
                  ((symbol-function 'agent-repl--uds-track-command)
                   (lambda (req field ws &optional _cb)
                     (setq tracked (list req field ws)))))
          (agent-repl--merge-dispatch-cherry-pick-over-uds "DWC/foo")
          (should (equal tracked '("req-9" "mergeWorkspace" "DWC/foo"))))))))

(ert-deftest agent-repl-test-cherry-pick-dispatch-ack-failure-clears-marker ()
  "The tracked on-failure callback clears :daemon-merge-dispatched."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "DWC/foo" :project-dir "/src")
    (let (on-failure)
      (agent-repl-test--with-mocked-merge-geometry
        (cl-letf (((symbol-function 'agent-repl--uds-send-command)
                   (lambda (&rest _) "req-1"))
                  ((symbol-function 'agent-repl--uds-track-command)
                   (lambda (_req _field _ws &optional cb) (setq on-failure cb))))
          (agent-repl--merge-dispatch-cherry-pick-over-uds "DWC/foo")
          (should (eq (agent-repl--ws-get "DWC/foo" :daemon-merge-dispatched) t))
          ;; Simulate a rejected ack
          (funcall on-failure "branch not found")
          (should-not (agent-repl--ws-get "DWC/foo" :daemon-merge-dispatched)))))))

;;;; ---- Tests: resolve-and-continue over UDS ----

(ert-deftest agent-repl-test-resume-shapes-command ()
  "Resume sends mergeWorkspace with conflict_resolved_continue + geometry."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "DWC/foo" :project-dir "/src")
    (let (sent)
      (agent-repl-test--with-mocked-merge-geometry
        (cl-letf (((symbol-function 'agent-repl--uds-send-command)
                   (lambda (field payload &optional ws &rest _)
                     (setq sent (list :field field :payload payload :ws ws))
                     "req-1"))
                  ((symbol-function 'agent-repl--uds-track-command)
                   (lambda (&rest _) "req-1")))
          (agent-repl--merge-resume-over-uds "DWC/foo")
          (should (equal (plist-get sent :field) "mergeWorkspace"))
          (let ((p (plist-get sent :payload)))
            (should (eq (plist-get p :conflictResolvedContinue) t))
            (should (equal (plist-get p :sourceBranch) "DWC/foo"))
            (should (equal (plist-get p :sourceDir) "/src"))
            (should (equal (plist-get p :targetDir) "/tgt"))))))))

;;;; ---- Tests: DAEMON-PORT PENDING refresh-master-from-origin handler ----
;;
;; The handler now defers to the main thread to start async PR polling
;; rather than doing synchronous git work directly.  Tests mock
;; `agent-repl--pr-poll-start' to capture whether it was called and
;; with which arguments, and verify the dirty-main early-exit still
;; signals correctly.
;;
;; Git work (fetch, ff, checkout, close, magit-refresh) is now done
;; inside `agent-repl--pr-poll-on-merged' when the poll sentinel
;; detects a MERGED state — covered by the pr-poll tests below.

(defmacro agent-repl-test--with-poll-start-mock (&rest body)
  "Run BODY with `agent-repl--pr-poll-start' captured but not invoked.
Binds `poll-start-calls' to a list of `(ws project-dir main-dir)'
argument lists, one per call."
  (declare (indent 0))
  `(let ((poll-start-calls nil))
     (cl-letf (((symbol-function 'agent-repl--pr-poll-start)
                (lambda (ws project-dir main-dir)
                  (push (list ws project-dir main-dir)
                        poll-start-calls))))
       ,@body)))

(ert-deftest agent-repl-test-refresh-master-defers-poll-start-when-clean ()
  "Handler defers `--pr-poll-start' to the main thread when clean."
  (agent-repl-test--with-clean-state
    (let ((agent-repl--workspaces (make-hash-table :test 'equal)))
      (agent-repl--ws-put "foo" :project-dir "/repo/wt-foo")
      (cl-letf (((symbol-function 'agent-repl--main-worktree-path)
                 (lambda (_dir) "/repo/main"))
                ((symbol-function 'agent-repl--worktree-dirty-p)
                 (lambda (_dir) nil)))
        (agent-repl-test--with-poll-start-mock
          (agent-repl--merge-handler-refresh-master-from-origin "foo")
          (should (= 1 (length poll-start-calls)))
          (should (equal (car poll-start-calls)
                         '("foo" "/repo/wt-foo" "/repo/main"))))))))

(ert-deftest agent-repl-test-refresh-master-poll-uses-project-dir-as-gh-cwd ()
  "Poll is started with :project-dir as the `gh' working directory."
  (agent-repl-test--with-clean-state
    (let ((agent-repl--workspaces (make-hash-table :test 'equal)))
      (agent-repl--ws-put "foo" :project-dir "/repo/wt-foo")
      (cl-letf (((symbol-function 'agent-repl--main-worktree-path)
                 (lambda (_dir) "/repo/main"))
                ((symbol-function 'agent-repl--worktree-dirty-p)
                 (lambda (_dir) nil)))
        (agent-repl-test--with-poll-start-mock
          (agent-repl--merge-handler-refresh-master-from-origin "foo")
          (let ((call (car poll-start-calls)))
            (should (equal (nth 1 call) "/repo/wt-foo"))))))))

(ert-deftest agent-repl-test-refresh-master-poll-passes-main-dir ()
  "Poll is started with the resolved main worktree so on-merged can fetch."
  (agent-repl-test--with-clean-state
    (let ((agent-repl--workspaces (make-hash-table :test 'equal)))
      (agent-repl--ws-put "foo" :project-dir "/repo/wt-foo")
      (cl-letf (((symbol-function 'agent-repl--main-worktree-path)
                 (lambda (_dir) "/repo/main"))
                ((symbol-function 'agent-repl--worktree-dirty-p)
                 (lambda (_dir) nil)))
        (agent-repl-test--with-poll-start-mock
          (agent-repl--merge-handler-refresh-master-from-origin "foo")
          (let ((call (car poll-start-calls)))
            (should (equal (nth 2 call) "/repo/main"))))))))

(ert-deftest agent-repl-test-refresh-master-poll-nil-main-dir-when-unresolvable ()
  "When `--main-worktree-path' returns nil, poll-start still gets called
with nil main-dir (git work will be skipped by `--pr-poll-on-merged')."
  (agent-repl-test--with-clean-state
    (let ((agent-repl--workspaces (make-hash-table :test 'equal)))
      (agent-repl--ws-put "foo" :project-dir "/repo/wt-foo")
      (cl-letf (((symbol-function 'agent-repl--main-worktree-path)
                 (lambda (_dir) nil)))
        (agent-repl-test--with-poll-start-mock
          (agent-repl--merge-handler-refresh-master-from-origin "foo")
          (should (= 1 (length poll-start-calls)))
          (should (null (nth 2 (car poll-start-calls)))))))))

(ert-deftest agent-repl-test-refresh-master-skips-when-no-source-dir ()
  "Handler no-ops silently when neither :project-dir nor :source-ws-dir is set."
  (agent-repl-test--with-clean-state
    (let ((agent-repl--workspaces (make-hash-table :test 'equal)))
      ;; "foo" has no :project-dir at all
      (agent-repl--ws-put "foo" :repl-state nil)
      (cl-letf (((symbol-function 'agent-repl--main-worktree-path)
                 (lambda (_dir) (error "should not be called"))))
        (agent-repl-test--with-poll-start-mock
          (agent-repl--merge-handler-refresh-master-from-origin "foo")
          (should-not poll-start-calls))))))

(ert-deftest agent-repl-test-refresh-master-errors-on-dirty-main ()
  "A dirty main worktree signals `user-error' so the merge-async failure
path can re-enqueue with :halt-until-human t."
  (agent-repl-test--with-clean-state
    (let ((agent-repl--workspaces (make-hash-table :test 'equal)))
      (agent-repl--ws-put "foo" :project-dir "/repo/wt-foo")
      (cl-letf (((symbol-function 'agent-repl--main-worktree-path)
                 (lambda (_dir) "/repo/main"))
                ((symbol-function 'agent-repl--worktree-dirty-p)
                 (lambda (_dir) t)))
        (agent-repl-test--with-poll-start-mock
          (should-error
           (agent-repl--merge-handler-refresh-master-from-origin "foo")
           :type 'user-error)
          (should-not poll-start-calls))))))

(ert-deftest agent-repl-test-refresh-master-does-not-mark-merged-when-dirty ()
  "Dirty main worktree signals out before any state mutation."
  (agent-repl-test--with-clean-state
    (let ((agent-repl--workspaces (make-hash-table :test 'equal)))
      (agent-repl--ws-put "foo" :project-dir "/repo/wt-foo")
      (cl-letf (((symbol-function 'agent-repl--main-worktree-path)
                 (lambda (_dir) "/repo/main"))
                ((symbol-function 'agent-repl--worktree-dirty-p)
                 (lambda (_dir) t)))
        (agent-repl-test--with-poll-start-mock
          (should-error
           (agent-repl--merge-handler-refresh-master-from-origin "foo")
           :type 'user-error)
          (should-not (agent-repl--ws-get "foo" :merge-completed))
          (should-not (eq (agent-repl--ws-get "foo" :repl-state) :merged)))))))

(ert-deftest agent-repl-test-refresh-master-handler-registered ()
  "The handler symbol is wired into the registry."
  (should (assq 'refresh-master-from-origin
                agent-repl--merge-handler-registry)))

;;;; ---- Tests: PR polling infrastructure ----
;;
;; Shared mock macro for the git/close side effects used by
;; `agent-repl--pr-poll-on-merged'.

(defmacro agent-repl-test--with-on-merged-mocks (&rest body)
  "Run BODY with the git/close side effects of `--pr-poll-on-merged' captured.
Binds `captured' plist with keys :fetch :ff :checkout :close-then
:close :magit-refresh."
  (declare (indent 0))
  `(let ((captured (list :fetch nil :ff nil :checkout nil :close-then nil
                         :close nil :magit-refresh nil)))
     (cl-letf
         (((symbol-function 'agent-repl--git-exit-code)
           (lambda (&rest args)
             (plist-put captured :fetch (cons args (plist-get captured :fetch)))
             0))
          ((symbol-function 'agent-repl--maybe-fast-forward-master)
           (lambda (dir) (plist-put captured :ff dir)))
          ((symbol-function 'agent-repl--checkout-master-in-worktree)
           (lambda (dir) (plist-put captured :checkout dir) t))
          ((symbol-function 'agent-repl--gns-sockets-close-then)
           (lambda (ws thunk)
             (plist-put captured :close-then ws)
             (funcall thunk)))
          ((symbol-function 'agent-repl--close-workspace)
           (lambda (ws &optional preserve)
             (plist-put captured :close (list ws preserve))))
          ((symbol-function 'agent-repl--refresh-magit-status-for-dir)
           (lambda (dir &optional ws)
             (plist-put captured :magit-refresh (list dir ws)))))
       ,@body)))

;;;; ---- Tests: pr-poll-cancel ----

(ert-deftest agent-repl-test-pr-poll-cancel-removes-active-timer ()
  "Cancel removes the timer from the hash table."
  (let ((agent-repl--active-pr-polls (make-hash-table :test 'equal))
        (fake-timer (cons 'timer nil)))
    (puthash "foo" fake-timer agent-repl--active-pr-polls)
    (cl-letf (((symbol-function 'cancel-timer) (lambda (_t) nil)))
      (agent-repl--pr-poll-cancel "foo"))
    (should-not (gethash "foo" agent-repl--active-pr-polls))))

(ert-deftest agent-repl-test-pr-poll-cancel-noops-when-no-poll ()
  "Cancel is a no-op and does not error when no poll is active."
  (let ((agent-repl--active-pr-polls (make-hash-table :test 'equal)))
    (should-not (agent-repl--pr-poll-cancel "nonexistent"))))

;;;; ---- Tests: pr-poll-handle-result ----

(ert-deftest agent-repl-test-pr-poll-handle-result-merged-calls-on-merged ()
  "MERGED state cancels the poll and calls `--pr-poll-on-merged'."
  (let ((agent-repl--active-pr-polls (make-hash-table :test 'equal))
        (on-merged-called nil))
    (cl-letf (((symbol-function 'agent-repl--pr-poll-cancel)
               (lambda (ws) (setq on-merged-called (list :cancel ws))))
              ((symbol-function 'agent-repl--pr-poll-on-merged)
               (lambda (ws main-dir)
                 (setq on-merged-called (list :merged ws main-dir)))))
      (agent-repl--pr-poll-handle-result
       "foo" "/repo/wt" "/repo/main"
       "{\"state\":\"MERGED\",\"mergedAt\":\"2024-01-01\",\"number\":42}")
      (should (equal on-merged-called '(:merged "foo" "/repo/main"))))))

(ert-deftest agent-repl-test-pr-poll-handle-result-closed-calls-on-failed ()
  "CLOSED state cancels the poll and calls `--pr-poll-on-failed'."
  (let ((agent-repl--active-pr-polls (make-hash-table :test 'equal))
        (on-failed-called nil))
    (cl-letf (((symbol-function 'agent-repl--pr-poll-cancel)
               (lambda (_ws) nil))
              ((symbol-function 'agent-repl--pr-poll-on-failed)
               (lambda (ws) (setq on-failed-called ws))))
      (agent-repl--pr-poll-handle-result
       "foo" "/repo/wt" "/repo/main"
       "{\"state\":\"CLOSED\",\"mergedAt\":null,\"number\":42}")
      (should (equal on-failed-called "foo")))))

(ert-deftest agent-repl-test-pr-poll-handle-result-open-does-nothing ()
  "OPEN state leaves poll running and calls neither on-merged nor on-failed."
  (let ((agent-repl--active-pr-polls (make-hash-table :test 'equal))
        (side-effects nil))
    (cl-letf (((symbol-function 'agent-repl--pr-poll-cancel)
               (lambda (_ws) (push :cancel side-effects)))
              ((symbol-function 'agent-repl--pr-poll-on-merged)
               (lambda (&rest _) (push :merged side-effects)))
              ((symbol-function 'agent-repl--pr-poll-on-failed)
               (lambda (&rest _) (push :failed side-effects))))
      (agent-repl--pr-poll-handle-result
       "foo" "/repo/wt" "/repo/main"
       "{\"state\":\"OPEN\",\"mergedAt\":null,\"number\":42}")
      (should-not side-effects))))

(ert-deftest agent-repl-test-pr-poll-handle-result-invalid-json-does-nothing ()
  "Unparseable output leaves the poll running — transient gh failures
should not abort the polling loop."
  (let ((agent-repl--active-pr-polls (make-hash-table :test 'equal))
        (side-effects nil))
    (cl-letf (((symbol-function 'agent-repl--pr-poll-cancel)
               (lambda (_ws) (push :cancel side-effects)))
              ((symbol-function 'agent-repl--pr-poll-on-merged)
               (lambda (&rest _) (push :merged side-effects)))
              ((symbol-function 'agent-repl--pr-poll-on-failed)
               (lambda (&rest _) (push :failed side-effects))))
      (agent-repl--pr-poll-handle-result
       "foo" "/repo/wt" "/repo/main"
       "not json at all")
      (should-not side-effects))))

;;;; ---- Tests: pr-poll-start ----

(ert-deftest agent-repl-test-pr-poll-start-fires-immediate-tick ()
  "The first tick is fired synchronously on poll start."
  (let ((agent-repl--active-pr-polls (make-hash-table :test 'equal))
        (tick-count 0))
    (cl-letf (((symbol-function 'agent-repl--pr-poll-tick)
               (lambda (_ws _dir _main) (cl-incf tick-count)))
              ((symbol-function 'run-with-timer)
               (lambda (&rest _) nil)))
      (agent-repl--pr-poll-start "foo" "/repo/wt" "/repo/main")
      (should (= 1 tick-count)))))

(ert-deftest agent-repl-test-pr-poll-start-registers-repeating-timer ()
  "After the immediate tick, a repeating timer is registered."
  (let ((agent-repl--active-pr-polls (make-hash-table :test 'equal))
        (timer-args nil))
    (cl-letf (((symbol-function 'agent-repl--pr-poll-tick)
               (lambda (&rest _) nil))
              ((symbol-function 'run-with-timer)
               (lambda (&rest args) (setq timer-args args) 'fake-timer)))
      (agent-repl--pr-poll-start "foo" "/repo/wt" "/repo/main")
      ;; run-with-timer called with (delay repeat fn args...)
      (should timer-args)
      (should (eq (nth 1 timer-args) agent-repl-pr-poll-interval))
      (should (gethash "foo" agent-repl--active-pr-polls)))))

(ert-deftest agent-repl-test-pr-poll-start-cancels-existing-before-starting ()
  "Starting a new poll for a workspace cancels any pre-existing poll."
  (let ((agent-repl--active-pr-polls (make-hash-table :test 'equal))
        (cancel-called-for nil))
    (puthash "foo" 'old-timer agent-repl--active-pr-polls)
    (cl-letf (((symbol-function 'cancel-timer)
               (lambda (_timer) (setq cancel-called-for t)))
              ((symbol-function 'agent-repl--pr-poll-tick)
               (lambda (&rest _) nil))
              ((symbol-function 'run-with-timer)
               (lambda (&rest _) 'new-timer)))
      (agent-repl--pr-poll-start "foo" "/repo/wt" "/repo/main")
      (should cancel-called-for))))

;;;; ---- Tests: pr-poll-on-merged ----

(ert-deftest agent-repl-test-pr-poll-on-merged-marks-ws-merged ()
  ":merge-completed t and :repl-state :merged after a successful poll merge."
  (agent-repl-test--with-clean-state
    (let ((agent-repl--workspaces (make-hash-table :test 'equal)))
      (agent-repl--ws-put "foo" :project-dir "/repo/wt-foo")
      (agent-repl-test--with-on-merged-mocks
        (agent-repl--pr-poll-on-merged "foo" "/repo/main")
        (should (eq (agent-repl--ws-get "foo" :merge-completed) t))
        (should (eq (agent-repl--ws-get "foo" :repl-state) :merged))
        (should-not (agent-repl--ws-get "foo" :merging))))))

(ert-deftest agent-repl-test-pr-poll-on-merged-fetches-origin ()
  "on-merged runs `git fetch origin master' in the main worktree."
  (agent-repl-test--with-clean-state
    (let ((agent-repl--workspaces (make-hash-table :test 'equal)))
      (agent-repl--ws-put "foo" :project-dir "/repo/wt-foo")
      (agent-repl-test--with-on-merged-mocks
        (agent-repl--pr-poll-on-merged "foo" "/repo/main")
        (let ((calls (plist-get captured :fetch)))
          (should (= 1 (length calls)))
          (should (equal (car calls)
                         (list "/repo/main" "fetch" "origin"
                               agent-repl-master-branch-name))))))))

(ert-deftest agent-repl-test-pr-poll-on-merged-calls-ff-and-checkout ()
  "on-merged invokes fast-forward and checkout on the main worktree."
  (agent-repl-test--with-clean-state
    (let ((agent-repl--workspaces (make-hash-table :test 'equal)))
      (agent-repl--ws-put "foo" :project-dir "/repo/wt-foo")
      (agent-repl-test--with-on-merged-mocks
        (agent-repl--pr-poll-on-merged "foo" "/repo/main")
        (should (equal (plist-get captured :ff) "/repo/main"))
        (should (equal (plist-get captured :checkout) "/repo/main"))))))

(ert-deftest agent-repl-test-pr-poll-on-merged-records-merge-target-from-main ()
  ":merge-target-name is read from the main worktree's branch."
  (agent-repl-test--with-clean-state
    (let ((agent-repl--workspaces (make-hash-table :test 'equal)))
      (agent-repl--ws-put "foo" :project-dir "/repo/wt-foo")
      (cl-letf (((symbol-function 'agent-repl--git-branch-of-dir)
                 (lambda (_dir) "master")))
        (agent-repl-test--with-on-merged-mocks
          (agent-repl--pr-poll-on-merged "foo" "/repo/main")
          (should (equal (agent-repl--ws-get "foo" :merge-target-name)
                         "master")))))))

(ert-deftest agent-repl-test-pr-poll-on-merged-merge-target-falls-back ()
  "When the main branch can't be read, :merge-target-name falls back to master-name."
  (agent-repl-test--with-clean-state
    (let ((agent-repl--workspaces (make-hash-table :test 'equal)))
      (agent-repl--ws-put "foo" :project-dir "/repo/wt-foo")
      (cl-letf (((symbol-function 'agent-repl--git-branch-of-dir)
                 (lambda (_dir) nil)))
        (agent-repl-test--with-on-merged-mocks
          (agent-repl--pr-poll-on-merged "foo" "/repo/main")
          (should (equal (agent-repl--ws-get "foo" :merge-target-name)
                         agent-repl-master-branch-name)))))))

(ert-deftest agent-repl-test-pr-poll-on-merged-skips-git-when-no-main-dir ()
  "When main-dir is nil, fetch/ff/checkout are skipped but state is still set."
  (agent-repl-test--with-clean-state
    (let ((agent-repl--workspaces (make-hash-table :test 'equal)))
      (agent-repl--ws-put "foo" :project-dir "/repo/wt-foo")
      (agent-repl-test--with-on-merged-mocks
        (agent-repl--pr-poll-on-merged "foo" nil)
        (should-not (plist-get captured :fetch))
        (should-not (plist-get captured :ff))
        (should-not (plist-get captured :checkout))
        (should (eq (agent-repl--ws-get "foo" :merge-completed) t))))))

(ert-deftest agent-repl-test-pr-poll-on-merged-closes-via-gns-sockets ()
  "Teardown is funnelled through `--gns-sockets-close-then'."
  (agent-repl-test--with-clean-state
    (let ((agent-repl--workspaces (make-hash-table :test 'equal)))
      (agent-repl--ws-put "foo" :project-dir "/repo/wt-foo")
      (agent-repl-test--with-on-merged-mocks
        (agent-repl--pr-poll-on-merged "foo" "/repo/main")
        (should (equal (plist-get captured :close-then) "foo"))
        (should (equal (plist-get captured :close)
                       '("foo" preserve-entry)))))))

(ert-deftest agent-repl-test-pr-poll-on-merged-refreshes-magit ()
  "on-merged triggers `--refresh-magit-status-for-dir' for the main worktree."
  (agent-repl-test--with-clean-state
    (let ((agent-repl--workspaces (make-hash-table :test 'equal)))
      (agent-repl--ws-put "foo" :project-dir "/repo/wt-foo")
      (agent-repl-test--with-on-merged-mocks
        (agent-repl--pr-poll-on-merged "foo" "/repo/main")
        (should (equal (plist-get captured :magit-refresh)
                       '("/repo/main" "foo")))))))

(ert-deftest agent-repl-test-pr-poll-on-merged-skips-magit-when-no-main-dir ()
  "When main-dir is nil, no magit refresh is attempted."
  (agent-repl-test--with-clean-state
    (let ((agent-repl--workspaces (make-hash-table :test 'equal)))
      (agent-repl--ws-put "foo" :project-dir "/repo/wt-foo")
      (agent-repl-test--with-on-merged-mocks
        (agent-repl--pr-poll-on-merged "foo" nil)
        (should-not (plist-get captured :magit-refresh))))))

;;;; ---- Tests: pr-poll-on-failed ----

(ert-deftest agent-repl-test-pr-poll-on-failed-marks-merge-failed ()
  "on-failed sets :merge-failed t and :repl-state :merge-failed."
  (agent-repl-test--with-clean-state
    (let ((agent-repl--workspaces (make-hash-table :test 'equal)))
      (agent-repl--ws-put "foo" :project-dir "/repo/wt-foo")
      (cl-letf (((symbol-function 'agent-repl--reopen-workspace-from-state)
                 (lambda (_ws) nil))
                ((symbol-function 'agent-repl--dispatch-prompt-command)
                 (lambda (_ws _msg) nil)))
        (agent-repl--pr-poll-on-failed "foo")
        (should (eq (agent-repl--ws-get "foo" :merge-failed) t))
        (should (eq (agent-repl--ws-get "foo" :repl-state) :merge-failed))
        (should-not (agent-repl--ws-get "foo" :merging))
        (should-not (agent-repl--ws-get "foo" :merge-completed))))))

(ert-deftest agent-repl-test-pr-poll-on-failed-reopens-workspace ()
  "on-failed calls `--reopen-workspace-from-state' to restore the UI."
  (agent-repl-test--with-clean-state
    (let ((agent-repl--workspaces (make-hash-table :test 'equal))
          (reopened nil))
      (agent-repl--ws-put "foo" :project-dir "/repo/wt-foo")
      (cl-letf (((symbol-function 'agent-repl--reopen-workspace-from-state)
                 (lambda (ws) (setq reopened ws)))
                ((symbol-function 'agent-repl--dispatch-prompt-command)
                 (lambda (_ws _msg) nil)))
        (agent-repl--pr-poll-on-failed "foo")
        (should (equal reopened "foo"))))))

(ert-deftest agent-repl-test-pr-poll-on-failed-dispatches-prompt ()
  "on-failed dispatches a failure prompt to the revived workspace."
  (agent-repl-test--with-clean-state
    (let ((agent-repl--workspaces (make-hash-table :test 'equal))
          (prompt-sent nil))
      (agent-repl--ws-put "foo" :project-dir "/repo/wt-foo")
      (cl-letf (((symbol-function 'agent-repl--reopen-workspace-from-state)
                 (lambda (_ws) nil))
                ((symbol-function 'agent-repl--dispatch-prompt-command)
                 (lambda (ws msg) (setq prompt-sent (list ws msg)))))
        (agent-repl--pr-poll-on-failed "foo")
        (should (equal (car prompt-sent) "foo"))
        (should (stringp (cadr prompt-sent)))))))

;;;; ---- Tests: default override is empty (cherry-pick everywhere) ----

(ert-deftest agent-repl-test-default-overrides-are-empty ()
  "The default value of `--workspace-merge-handler-overrides' is nil, so no
repo is special-cased away from the cherry-pick default."
  (let ((default (eval (car (get 'agent-repl-workspace-merge-handler-overrides
                                  'standard-value)))))
    (should-not default)))

(ert-deftest agent-repl-test-default-overrides-omit-explanation-engine ()
  "The explanation-engine repo no longer carries a special-case override —
it merges via the cherry-pick default like every other repo."
  (let ((default (eval (car (get 'agent-repl-workspace-merge-handler-overrides
                                  'standard-value)))))
    (should-not (assoc "~/workspace/ChessCom/explanation-engine" default))))

;;;; ---- Tests: onto-master handler + per-command routing ----

(ert-deftest agent-repl-test-onto-master-handler-registered ()
  "The `onto-master' handler symbol is wired into the registry."
  (should (assq 'onto-master agent-repl--merge-handler-registry)))

(ert-deftest agent-repl-test-dispatch-onto-master-flag-forces-handler ()
  "A non-nil ONTO-MASTER arg forces the `onto-master' handler even when the
repo's `.eld' prescribes a different handler (cherry-pick)."
  (agent-repl-test--with-clean-registry
    (let ((captured nil))
      (agent-repl--register-merge-handler
       'onto-master (lambda (ws _args) (setq captured ws)))
      (agent-repl-test--with-temp-repo root
        ;; .eld says cherry-pick, but the flag must win.
        (agent-repl-test--seed-merge-config root "((handler . cherry-pick))")
        (cl-letf (((symbol-function 'agent-repl--main-worktree-path)
                   (lambda (dir) dir)))
          (agent-repl--dispatch-merge-handler "DWC/foo" root t)
          (should (equal captured "DWC/foo")))))))

(ert-deftest agent-repl-test-dispatch-without-onto-master-uses-cherry-pick ()
  "Without the onto-master flag, dispatch resolves the default cherry-pick and
routes it over UDS (NOT the forced onto-master handler)."
  (agent-repl-test--with-clean-registry
    (let ((captured nil))
      (agent-repl--register-merge-handler
       'onto-master (lambda (_ws _args) (setq captured 'onto-master)))
      (cl-letf (((symbol-function 'agent-repl--merge-dispatch-cherry-pick-over-uds)
                 (lambda (_ws) (setq captured 'cherry-pick)))
                ((symbol-function 'agent-repl--main-worktree-path)
                 (lambda (dir) dir)))
        (agent-repl-test--with-temp-repo root
          (agent-repl--dispatch-merge-handler "DWC/foo" root nil)
          (should (eq captured 'cherry-pick)))))))

(ert-deftest agent-repl-test-onto-master-skips-when-no-source-dir ()
  "Handler no-ops (no finalize) when neither :project-dir nor :source-ws-dir set."
  (agent-repl-test--with-clean-state
    (let ((agent-repl--workspaces (make-hash-table :test 'equal))
          (finalized nil))
      (agent-repl--ws-put "foo" :repl-state nil)
      (cl-letf (((symbol-function 'agent-repl--main-worktree-path)
                 (lambda (_dir) (error "should not be called")))
                ((symbol-function 'agent-repl--finalize-merged-workspace)
                 (lambda (&rest _) (setq finalized t))))
        (agent-repl--merge-handler-onto-master "foo")
        (should-not finalized)))))

(ert-deftest agent-repl-test-onto-master-errors-on-dirty-main ()
  "A dirty main worktree signals `user-error' before any git advance."
  (agent-repl-test--with-clean-state
    (let ((agent-repl--workspaces (make-hash-table :test 'equal))
          (finalized nil))
      (agent-repl--ws-put "foo" :project-dir "/repo/wt-foo")
      (cl-letf (((symbol-function 'agent-repl--main-worktree-path)
                 (lambda (_dir) "/repo/main"))
                ((symbol-function 'agent-repl--worktree-dirty-p)
                 (lambda (_dir) t))
                ((symbol-function 'agent-repl--git-exit-code)
                 (lambda (&rest _) (error "git should not run on dirty main")))
                ((symbol-function 'agent-repl--finalize-merged-workspace)
                 (lambda (&rest _) (setq finalized t))))
        (should-error (agent-repl--merge-handler-onto-master "foo")
                      :type 'user-error)
        (should-not finalized)))))

(ert-deftest agent-repl-test-onto-master-rebases-diverged-and-finalizes ()
  "Divergence rebases local master onto origin/master, then finalizes."
  (agent-repl-test--with-clean-state
    (let ((agent-repl--workspaces (make-hash-table :test 'equal))
          (rebased nil)
          (finalized nil))
      (agent-repl--ws-put "foo" :project-dir "/repo/wt-foo")
      (cl-letf (((symbol-function 'agent-repl--main-worktree-path)
                 (lambda (_dir) "/repo/main"))
                ((symbol-function 'agent-repl--master-worktree-path)
                 (lambda (_dir) "/repo/main"))
                ((symbol-function 'agent-repl--worktree-dirty-p)
                 (lambda (_dir) nil))
                ((symbol-function 'agent-repl--git-exit-code)
                 ;; fetch -> 0; is-ancestor -> 1 (diverged).
                 (lambda (&rest args) (if (member "merge-base" args) 1 0)))
                ((symbol-function 'agent-repl--git-string-quiet)
                 (lambda (&rest _) ""))
                ((symbol-function 'agent-repl--rebase-with-auto-resolve)
                 (lambda (_ws wt onto) (setq rebased (list wt onto)) t))
                ((symbol-function 'agent-repl--finalize-merged-workspace)
                 (lambda (ws main-dir) (setq finalized (list ws main-dir)))))
        (agent-repl--merge-handler-onto-master "foo")
        ;; The rebase targeted origin/master in the master worktree.
        (should (equal rebased '("/repo/main" "origin/master")))
        (should (equal finalized '("foo" "/repo/main")))))))

(ert-deftest agent-repl-test-onto-master-errors-when-rebase-declines ()
  "A diverged rebase the resolver declines signals `user-error' and never finalizes."
  (agent-repl-test--with-clean-state
    (let ((agent-repl--workspaces (make-hash-table :test 'equal))
          (finalized nil))
      (agent-repl--ws-put "foo" :project-dir "/repo/wt-foo")
      (cl-letf (((symbol-function 'agent-repl--main-worktree-path)
                 (lambda (_dir) "/repo/main"))
                ((symbol-function 'agent-repl--master-worktree-path)
                 (lambda (_dir) "/repo/main"))
                ((symbol-function 'agent-repl--worktree-dirty-p)
                 (lambda (_dir) nil))
                ((symbol-function 'agent-repl--git-exit-code)
                 (lambda (&rest args) (if (member "merge-base" args) 1 0)))
                ((symbol-function 'agent-repl--git-string-quiet)
                 (lambda (&rest _) ""))
                ;; Resolver declines the rebase (conceptual conflict).
                ((symbol-function 'agent-repl--rebase-with-auto-resolve)
                 (lambda (&rest _) nil))
                ((symbol-function 'agent-repl--finalize-merged-workspace)
                 (lambda (&rest _) (setq finalized t))))
        (should-error (agent-repl--merge-handler-onto-master "foo")
                      :type 'user-error)
        (should-not finalized)))))

(ert-deftest agent-repl-test-onto-master-errors-on-divergence-without-master-worktree ()
  "Divergence with no master worktree cannot be rebased, so it signals `user-error'."
  (agent-repl-test--with-clean-state
    (let ((agent-repl--workspaces (make-hash-table :test 'equal))
          (rebased nil)
          (finalized nil))
      (agent-repl--ws-put "foo" :project-dir "/repo/wt-foo")
      (cl-letf (((symbol-function 'agent-repl--main-worktree-path)
                 (lambda (_dir) "/repo/main"))
                ((symbol-function 'agent-repl--master-worktree-path)
                 (lambda (_dir) nil))
                ((symbol-function 'agent-repl--worktree-dirty-p)
                 (lambda (_dir) nil))
                ((symbol-function 'agent-repl--git-exit-code)
                 (lambda (&rest args) (if (member "merge-base" args) 1 0)))
                ((symbol-function 'agent-repl--git-string-quiet)
                 (lambda (&rest _) ""))
                ((symbol-function 'agent-repl--rebase-with-auto-resolve)
                 (lambda (&rest _) (setq rebased t) t))
                ((symbol-function 'agent-repl--finalize-merged-workspace)
                 (lambda (&rest _) (setq finalized t))))
        (should-error (agent-repl--merge-handler-onto-master "foo")
                      :type 'user-error)
        ;; No worktree holds master, so no rebase is even attempted.
        (should-not rebased)
        (should-not finalized)))))

(ert-deftest agent-repl-test-onto-master-ff-and-finalizes ()
  "Happy path: clean main, ancestor, ff succeeds in the master worktree, then
finalize tears the workspace down on the (synchronous-in-test) main thread."
  (agent-repl-test--with-clean-state
    (let ((agent-repl--workspaces (make-hash-table :test 'equal))
          (git-calls nil)
          (finalized nil))
      (agent-repl--ws-put "foo" :project-dir "/repo/wt-foo")
      (cl-letf (((symbol-function 'agent-repl--main-worktree-path)
                 (lambda (_dir) "/repo/main"))
                ((symbol-function 'agent-repl--master-worktree-path)
                 (lambda (_dir) "/repo/main"))
                ((symbol-function 'agent-repl--worktree-dirty-p)
                 (lambda (_dir) nil))
                ((symbol-function 'agent-repl--git-exit-code)
                 (lambda (&rest args) (push args git-calls) 0))
                ;; No cee-agent paths in the merged delta.
                ((symbol-function 'agent-repl--git-string-quiet)
                 (lambda (&rest _) ""))
                ((symbol-function 'agent-repl--finalize-merged-workspace)
                 (lambda (ws main-dir) (setq finalized (list ws main-dir)))))
        (agent-repl--merge-handler-onto-master "foo")
        (should (equal finalized '("foo" "/repo/main")))
        ;; A fast-forward merge was issued in the master worktree.
        (should (cl-some (lambda (args)
                           (and (member "merge" args) (member "--ff-only" args)))
                         git-calls))))))

;;;; ---- Tests: merge-delta-touches-dir-p ----

(ert-deftest agent-repl-test-merge-delta-touches-dir-p-true-when-under-prefix ()
  "Returns non-nil when a changed path is under the directory prefix."
  (cl-letf (((symbol-function 'agent-repl--git-string-quiet)
             (lambda (&rest _)
               "README.md\napps/cee-agent/main.go\npkg/util.go")))
    (should (agent-repl--merge-delta-touches-dir-p
             "/repo/main" "master" "origin/master" "apps/cee-agent/"))))

(ert-deftest agent-repl-test-merge-delta-touches-dir-p-accepts-prefix-sans-slash ()
  "A DIR-PREFIX without a trailing slash still matches paths under it."
  (cl-letf (((symbol-function 'agent-repl--git-string-quiet)
             (lambda (&rest _) "apps/cee-agent/scripts/run.sh")))
    (should (agent-repl--merge-delta-touches-dir-p
             "/repo/main" "master" "origin/master" "apps/cee-agent"))))

(ert-deftest agent-repl-test-merge-delta-touches-dir-p-false-when-only-sibling ()
  "A path that shares a name prefix but not the directory does not match."
  (cl-letf (((symbol-function 'agent-repl--git-string-quiet)
             (lambda (&rest _) "apps/cee-agent-extra/main.go\nREADME.md")))
    (should-not (agent-repl--merge-delta-touches-dir-p
                 "/repo/main" "master" "origin/master" "apps/cee-agent/"))))

(ert-deftest agent-repl-test-merge-delta-touches-dir-p-false-when-empty ()
  "An empty diff (no changed paths) returns nil."
  (cl-letf (((symbol-function 'agent-repl--git-string-quiet)
             (lambda (&rest _) "")))
    (should-not (agent-repl--merge-delta-touches-dir-p
                 "/repo/main" "master" "origin/master" "apps/cee-agent/"))))

(ert-deftest agent-repl-test-merge-delta-touches-dir-p-samples-given-refs ()
  "The diff is taken between the supplied FROM-REF and TO-REF in MAIN-DIR."
  (let ((seen nil))
    (cl-letf (((symbol-function 'agent-repl--git-string-quiet)
               (lambda (&rest args) (setq seen args) "")))
      (agent-repl--merge-delta-touches-dir-p
       "/repo/main" "master" "origin/master" "apps/cee-agent/")
      (should (equal seen
                     '("-C" "/repo/main" "diff" "--name-only"
                       "master" "origin/master"))))))

;;;; ---- Tests: onto-master cee-agent reinstall-and-bounce ----

(ert-deftest agent-repl-test-onto-master-bounces-cee-agent-when-touched ()
  "A merged delta touching apps/cee-agent runs reinstall-and-bounce in the
MAIN worktree after the fast-forward, then finalizes."
  (agent-repl-test--with-clean-state
    (let ((agent-repl--workspaces (make-hash-table :test 'equal))
          (bounce-dir nil)
          (finalized nil))
      (agent-repl--ws-put "foo" :project-dir "/repo/wt-foo")
      (cl-letf (((symbol-function 'agent-repl--main-worktree-path)
                 (lambda (_dir) "/repo/main"))
                ((symbol-function 'agent-repl--master-worktree-path)
                 (lambda (_dir) "/repo/master-wt"))
                ((symbol-function 'agent-repl--worktree-dirty-p)
                 (lambda (_dir) nil))
                ((symbol-function 'agent-repl--git-exit-code)
                 (lambda (&rest _) 0))
                ((symbol-function 'agent-repl--git-string-quiet)
                 (lambda (&rest _) "apps/cee-agent/main.go"))
                ((symbol-function
                  'agent-repl--cee-agent-reinstall-and-bounce-exit-code)
                 (lambda (worktree) (setq bounce-dir worktree) 0))
                ((symbol-function 'agent-repl--finalize-merged-workspace)
                 (lambda (ws main-dir) (setq finalized (list ws main-dir)))))
        (agent-repl--merge-handler-onto-master "foo")
        ;; Script ran in the MAIN worktree, not the master worktree.
        (should (equal bounce-dir "/repo/main"))
        (should (equal finalized '("foo" "/repo/main")))))))

(ert-deftest agent-repl-test-onto-master-skips-bounce-when-not-touched ()
  "A merged delta that does not touch apps/cee-agent never runs the script."
  (agent-repl-test--with-clean-state
    (let ((agent-repl--workspaces (make-hash-table :test 'equal))
          (bounced nil)
          (finalized nil))
      (agent-repl--ws-put "foo" :project-dir "/repo/wt-foo")
      (cl-letf (((symbol-function 'agent-repl--main-worktree-path)
                 (lambda (_dir) "/repo/main"))
                ((symbol-function 'agent-repl--master-worktree-path)
                 (lambda (_dir) "/repo/main"))
                ((symbol-function 'agent-repl--worktree-dirty-p)
                 (lambda (_dir) nil))
                ((symbol-function 'agent-repl--git-exit-code)
                 (lambda (&rest _) 0))
                ((symbol-function 'agent-repl--git-string-quiet)
                 (lambda (&rest _) "pkg/util.go\nREADME.md"))
                ((symbol-function
                  'agent-repl--cee-agent-reinstall-and-bounce-exit-code)
                 (lambda (&rest _) (setq bounced t) 0))
                ((symbol-function 'agent-repl--finalize-merged-workspace)
                 (lambda (ws main-dir) (setq finalized (list ws main-dir)))))
        (agent-repl--merge-handler-onto-master "foo")
        (should-not bounced)
        (should (equal finalized '("foo" "/repo/main")))))))

(ert-deftest agent-repl-test-onto-master-bounce-failure-is-non-fatal ()
  "A non-zero script exit does NOT signal and still finalizes (trunk already
advanced, so the workspace must not be revived)."
  (agent-repl-test--with-clean-state
    (let ((agent-repl--workspaces (make-hash-table :test 'equal))
          (finalized nil))
      (agent-repl--ws-put "foo" :project-dir "/repo/wt-foo")
      (cl-letf (((symbol-function 'agent-repl--main-worktree-path)
                 (lambda (_dir) "/repo/main"))
                ((symbol-function 'agent-repl--master-worktree-path)
                 (lambda (_dir) "/repo/main"))
                ((symbol-function 'agent-repl--worktree-dirty-p)
                 (lambda (_dir) nil))
                ((symbol-function 'agent-repl--git-exit-code)
                 (lambda (&rest _) 0))
                ((symbol-function 'agent-repl--git-string-quiet)
                 (lambda (&rest _) "apps/cee-agent/main.go"))
                ((symbol-function
                  'agent-repl--cee-agent-reinstall-and-bounce-exit-code)
                 (lambda (&rest _) 1))
                ((symbol-function 'agent-repl--finalize-merged-workspace)
                 (lambda (ws main-dir) (setq finalized (list ws main-dir)))))
        (agent-repl--merge-handler-onto-master "foo")
        (should (equal finalized '("foo" "/repo/main")))))))

(ert-deftest agent-repl-test-onto-master-bounce-timeout-is-non-fatal ()
  "A `timeout' result from the bounce wrapper does NOT signal and still
finalizes — the trunk already advanced, so the workspace must not be
revived, and a `%d'-style log on the symbol must not crash the handler."
  (agent-repl-test--with-clean-state
    (let ((agent-repl--workspaces (make-hash-table :test 'equal))
          (finalized nil))
      (agent-repl--ws-put "foo" :project-dir "/repo/wt-foo")
      (cl-letf (((symbol-function 'agent-repl--main-worktree-path)
                 (lambda (_dir) "/repo/main"))
                ((symbol-function 'agent-repl--master-worktree-path)
                 (lambda (_dir) "/repo/main"))
                ((symbol-function 'agent-repl--worktree-dirty-p)
                 (lambda (_dir) nil))
                ((symbol-function 'agent-repl--git-exit-code)
                 (lambda (&rest _) 0))
                ((symbol-function 'agent-repl--git-string-quiet)
                 (lambda (&rest _) "apps/cee-agent/main.go"))
                ((symbol-function
                  'agent-repl--cee-agent-reinstall-and-bounce-exit-code)
                 (lambda (&rest _) 'timeout))
                ((symbol-function 'agent-repl--finalize-merged-workspace)
                 (lambda (ws main-dir) (setq finalized (list ws main-dir)))))
        (agent-repl--merge-handler-onto-master "foo")
        (should (equal finalized '("foo" "/repo/main")))))))

(ert-deftest agent-repl-test-run-cee-agent-bounce-returns-wrapper-timeout ()
  "`--onto-master-run-cee-agent-bounce' returns the wrapper's `timeout'
symbol unchanged so callers can distinguish an overrun from a clean
exit, and the `timeout' branch's log must not raise."
  (agent-repl-test--with-clean-state
    (let ((agent-repl--workspaces (make-hash-table :test 'equal)))
      (agent-repl--ws-put "foo" :project-dir "/repo/wt-foo")
      (cl-letf (((symbol-function
                  'agent-repl--cee-agent-reinstall-and-bounce-exit-code)
                 (lambda (&rest _) 'timeout)))
        (should (eq 'timeout
                    (agent-repl--onto-master-run-cee-agent-bounce
                     "foo" "/repo/main")))))))

(ert-deftest agent-repl-test-run-cee-agent-bounce-returns-wrapper-exit-code ()
  "`--onto-master-run-cee-agent-bounce' returns the wrapper's integer
exit code unchanged on a normal (non-timeout) finish."
  (agent-repl-test--with-clean-state
    (let ((agent-repl--workspaces (make-hash-table :test 'equal)))
      (agent-repl--ws-put "foo" :project-dir "/repo/wt-foo")
      (cl-letf (((symbol-function
                  'agent-repl--cee-agent-reinstall-and-bounce-exit-code)
                 (lambda (&rest _) 3)))
        (should (= 3 (agent-repl--onto-master-run-cee-agent-bounce
                      "foo" "/repo/main")))))))

(ert-deftest agent-repl-test-onto-master-samples-touch-before-ff ()
  "The cee-agent touch is sampled between local master and origin/master
BEFORE the fast-forward (so the delta is non-empty)."
  (agent-repl-test--with-clean-state
    (let ((agent-repl--workspaces (make-hash-table :test 'equal))
          (diff-args nil))
      (agent-repl--ws-put "foo" :project-dir "/repo/wt-foo")
      (cl-letf (((symbol-function 'agent-repl--main-worktree-path)
                 (lambda (_dir) "/repo/main"))
                ((symbol-function 'agent-repl--master-worktree-path)
                 (lambda (_dir) "/repo/main"))
                ((symbol-function 'agent-repl--worktree-dirty-p)
                 (lambda (_dir) nil))
                ((symbol-function 'agent-repl--git-exit-code)
                 (lambda (&rest _) 0))
                ((symbol-function 'agent-repl--git-string-quiet)
                 (lambda (&rest args) (setq diff-args args) ""))
                ((symbol-function 'agent-repl--finalize-merged-workspace)
                 (lambda (&rest _) nil)))
        (agent-repl--merge-handler-onto-master "foo")
        (should (member "master" diff-args))
        (should (member "origin/master" diff-args))))))

(provide 'test-merge-handlers)

;;; test-merge-handlers.el ends here
