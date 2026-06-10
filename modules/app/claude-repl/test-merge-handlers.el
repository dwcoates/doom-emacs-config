;;; test-merge-handlers.el --- Tests for merge-handlers.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Unit tests for the repo-routed `/workspace-merge' dispatch layer.
;; Covers:
;;   - `claude-repl--read-merge-handler-config-file' (file parsing)
;;   - `claude-repl--lookup-merge-handler-override' (defcustom lookup)
;;   - `claude-repl--resolve-merge-handler' (precedence + fallbacks)
;;   - `claude-repl--dispatch-merge-handler' (registry invocation)
;;   - `claude-repl--merge-handler-cherry-pick' (default handler)
;;   - `claude-repl--register-merge-handler' (registry mutation)

;;; Code:

(load (expand-file-name "test-helpers.el" (file-name-directory
                                            (or load-file-name buffer-file-name)))
      nil t)

;;;; ---- Fixture helpers ----

(defmacro claude-repl-test--with-temp-repo (var &rest body)
  "Bind VAR to a fresh temp directory acting as a repo root, run BODY, then clean.
The directory is created with `make-temp-file' (deleted on exit)."
  (declare (indent 1))
  `(let ((,var (make-temp-file "claude-repl-test-repo-" t)))
     (unwind-protect
         (progn ,@body)
       (when (file-directory-p ,var)
         (delete-directory ,var t)))))

(defun claude-repl-test--seed-merge-config (repo-root content)
  "Write CONTENT (string) to REPO-ROOT's `.claude/emacs/workspace-merge.eld'."
  (let* ((dir (expand-file-name ".claude/emacs" repo-root))
         (path (expand-file-name "workspace-merge.eld" dir)))
    (make-directory dir t)
    (with-temp-file path (insert content))
    path))

(defmacro claude-repl-test--with-clean-registry (&rest body)
  "Run BODY with a registry containing only `cherry-pick'.
Restores the prior registry on exit so tests don't bleed."
  (declare (indent 0))
  `(let ((claude-repl--merge-handler-registry
          (list (cons 'cherry-pick
                      #'claude-repl--merge-handler-cherry-pick)))
         (claude-repl-workspace-merge-handler-overrides nil))
     ,@body))

;;;; ---- Tests: read-merge-handler-config-file ----

(ert-deftest claude-repl-test-merge-config-reads-valid-alist ()
  "A well-formed alist file is parsed into its content."
  (claude-repl-test--with-temp-repo root
    (claude-repl-test--seed-merge-config
     root "((handler . create-pr) (args . (:add-to-merge-queue t)))")
    (let ((config (claude-repl--read-merge-handler-config-file root)))
      (should (equal (alist-get 'handler config) 'create-pr))
      (should (equal (alist-get 'args config)
                     '(:add-to-merge-queue t))))))

(ert-deftest claude-repl-test-merge-config-returns-nil-when-missing ()
  "A repo with no .claude/emacs/workspace-merge.eld file returns nil."
  (claude-repl-test--with-temp-repo root
    (should-not (claude-repl--read-merge-handler-config-file root))))

(ert-deftest claude-repl-test-merge-config-returns-nil-when-malformed ()
  "A file containing unreadable junk returns nil (does not raise)."
  (claude-repl-test--with-temp-repo root
    (claude-repl-test--seed-merge-config root "this is (((not valid sexp")
    (should-not (claude-repl--read-merge-handler-config-file root))))

(ert-deftest claude-repl-test-merge-config-returns-nil-when-not-alist ()
  "A file whose top-level sexp is not a cons returns nil."
  (claude-repl-test--with-temp-repo root
    (claude-repl-test--seed-merge-config root "42")
    (should-not (claude-repl--read-merge-handler-config-file root))))

(ert-deftest claude-repl-test-merge-config-returns-nil-when-repo-root-nil ()
  "A nil repo-root short-circuits to nil without touching the filesystem."
  (should-not (claude-repl--read-merge-handler-config-file nil)))

(ert-deftest claude-repl-test-merge-config-returns-nil-when-repo-missing ()
  "A repo-root pointing at a nonexistent directory returns nil."
  (should-not (claude-repl--read-merge-handler-config-file
               "/nonexistent/claude-repl-test/repo")))

;;;; ---- Tests: lookup-merge-handler-override ----

(ert-deftest claude-repl-test-merge-override-finds-exact-path ()
  "An override entry matching the canonical path is returned."
  (claude-repl-test--with-temp-repo root
    (let ((claude-repl-workspace-merge-handler-overrides
           (list (cons root '((handler . noop))))))
      (let ((config (claude-repl--lookup-merge-handler-override root)))
        (should (equal (alist-get 'handler config) 'noop))))))

(ert-deftest claude-repl-test-merge-override-matches-trailing-slash ()
  "A path with trailing slash matches a canonical entry without one."
  (claude-repl-test--with-temp-repo root
    (let* ((with-slash (file-name-as-directory root))
           (claude-repl-workspace-merge-handler-overrides
            (list (cons root '((handler . noop))))))
      (should (claude-repl--lookup-merge-handler-override with-slash)))))

(ert-deftest claude-repl-test-merge-override-returns-nil-no-match ()
  "No matching override returns nil."
  (claude-repl-test--with-temp-repo root
    (let ((claude-repl-workspace-merge-handler-overrides
           '(("/some/other/repo" . ((handler . noop))))))
      (should-not (claude-repl--lookup-merge-handler-override root)))))

(ert-deftest claude-repl-test-merge-override-returns-nil-when-repo-root-nil ()
  "A nil repo-root short-circuits."
  (let ((claude-repl-workspace-merge-handler-overrides
         '(("/some/repo" . ((handler . noop))))))
    (should-not (claude-repl--lookup-merge-handler-override nil))))

;;;; ---- Tests: resolve-merge-handler ----

(ert-deftest claude-repl-test-resolve-falls-back-to-cherry-pick ()
  "With no config anywhere, resolver returns `(cherry-pick . nil)'."
  (claude-repl-test--with-clean-registry
    (claude-repl-test--with-temp-repo root
      (should (equal (claude-repl--resolve-merge-handler root)
                     '(cherry-pick))))))

(ert-deftest claude-repl-test-resolve-uses-eld-when-present ()
  "Resolver returns the .eld-declared handler symbol + args."
  (claude-repl-test--with-clean-registry
    (claude-repl--register-merge-handler 'noop (lambda (&rest _) nil))
    (claude-repl-test--with-temp-repo root
      (claude-repl-test--seed-merge-config
       root "((handler . noop) (args . (:silent t)))")
      (let ((res (claude-repl--resolve-merge-handler root)))
        (should (eq (car res) 'noop))
        (should (equal (cdr res) '(:silent t)))))))

(ert-deftest claude-repl-test-resolve-uses-override-when-no-eld ()
  "With no .eld, the defcustom override supplies the handler."
  (claude-repl-test--with-clean-registry
    (claude-repl--register-merge-handler 'noop (lambda (&rest _) nil))
    (claude-repl-test--with-temp-repo root
      (let ((claude-repl-workspace-merge-handler-overrides
             (list (cons root '((handler . noop) (args . (:k v)))))))
        (let ((res (claude-repl--resolve-merge-handler root)))
          (should (eq (car res) 'noop))
          (should (equal (cdr res) '(:k v))))))))

(ert-deftest claude-repl-test-resolve-eld-wins-over-override ()
  "When both .eld and defcustom override are set, .eld wins."
  (claude-repl-test--with-clean-registry
    (claude-repl--register-merge-handler 'noop (lambda (&rest _) nil))
    (claude-repl-test--with-temp-repo root
      (claude-repl-test--seed-merge-config root "((handler . noop))")
      (let ((claude-repl-workspace-merge-handler-overrides
             (list (cons root '((handler . cherry-pick))))))
        (should (eq (car (claude-repl--resolve-merge-handler root))
                    'noop))))))

(ert-deftest claude-repl-test-resolve-falls-back-on-unknown-symbol ()
  "An unknown handler symbol falls back to cherry-pick (does not raise)."
  (claude-repl-test--with-clean-registry
    (claude-repl-test--with-temp-repo root
      (claude-repl-test--seed-merge-config
       root "((handler . totally-fake-handler))")
      (should (eq (car (claude-repl--resolve-merge-handler root))
                  'cherry-pick)))))

;;;; ---- Tests: dispatch-merge-handler ----

(ert-deftest claude-repl-test-dispatch-invokes-resolved-handler ()
  "Dispatcher invokes the resolved handler with TARGET-WS + ARGS."
  (claude-repl-test--with-clean-registry
    (let ((captured nil))
      (claude-repl--register-merge-handler
       'noop (lambda (ws args)
               (setq captured (list :ws ws :args args))))
      (claude-repl-test--with-temp-repo root
        (claude-repl-test--seed-merge-config
         root "((handler . noop) (args . (:hello world)))")
        (cl-letf (((symbol-function 'claude-repl--main-worktree-path)
                   (lambda (dir) dir)))
          (claude-repl--dispatch-merge-handler "DWC/foo" root)
          (should (equal (plist-get captured :ws) "DWC/foo"))
          (should (equal (plist-get captured :args) '(:hello world))))))))

(ert-deftest claude-repl-test-dispatch-falls-back-to-cherry-pick ()
  "With no config, dispatcher invokes the registered cherry-pick handler."
  (claude-repl-test--with-clean-registry
    (let ((captured nil))
      (cl-letf (((symbol-function 'claude-repl--workspace-merge-into-source)
                 (lambda (ws silent auto-resolve)
                   (setq captured (list :ws ws :silent silent
                                        :auto-resolve auto-resolve))))
                ((symbol-function 'claude-repl--main-worktree-path)
                 (lambda (dir) dir)))
        (claude-repl-test--with-temp-repo root
          (claude-repl--dispatch-merge-handler "DWC/foo" root)
          (should (equal (plist-get captured :ws) "DWC/foo"))
          (should (eq (plist-get captured :silent) t))
          (should (eq (plist-get captured :auto-resolve) t)))))))

(ert-deftest claude-repl-test-dispatch-normalises-repo-root-to-main-worktree-for-eld ()
  "Dispatcher canonicalises REPO-ROOT through `--main-worktree-path' before
reading the `.eld' file, so a sibling-worktree caller still hits the main
worktree's checked-in config.  Without normalisation a worktree path whose
tree happens not to carry the file would silently fall through to
cherry-pick."
  (claude-repl-test--with-clean-registry
    (let ((captured nil))
      (claude-repl--register-merge-handler
       'noop (lambda (ws args)
               (setq captured (list :ws ws :args args))))
      (claude-repl-test--with-temp-repo main
        (claude-repl-test--with-temp-repo worktree
          (claude-repl-test--seed-merge-config
           main "((handler . noop) (args . (:from main)))")
          (cl-letf (((symbol-function 'claude-repl--main-worktree-path)
                     (lambda (dir)
                       (when (equal dir worktree) main))))
            (claude-repl--dispatch-merge-handler "DWC/foo" worktree)
            (should (equal (plist-get captured :ws) "DWC/foo"))
            (should (equal (plist-get captured :args) '(:from main)))))))))

(ert-deftest claude-repl-test-dispatch-normalises-repo-root-to-main-worktree-for-override ()
  "Dispatcher canonicalises REPO-ROOT through `--main-worktree-path' before
consulting the defcustom override, so a sibling-worktree caller still
matches an override entry keyed by the main repo path."
  (claude-repl-test--with-clean-registry
    (let ((captured nil))
      (claude-repl--register-merge-handler
       'noop (lambda (ws args)
               (setq captured (list :ws ws :args args))))
      (claude-repl-test--with-temp-repo main
        (claude-repl-test--with-temp-repo worktree
          (let ((claude-repl-workspace-merge-handler-overrides
                 (list (cons main '((handler . noop)
                                    (args . (:via override)))))))
            (cl-letf (((symbol-function 'claude-repl--main-worktree-path)
                       (lambda (dir)
                         (when (equal dir worktree) main))))
              (claude-repl--dispatch-merge-handler "DWC/foo" worktree)
              (should (equal (plist-get captured :ws) "DWC/foo"))
              (should (equal (plist-get captured :args)
                             '(:via override))))))))))

(ert-deftest claude-repl-test-dispatch-falls-back-to-repo-root-when-main-worktree-nil ()
  "When `--main-worktree-path' returns nil (git unavailable / not a repo),
dispatcher falls back to the caller-supplied REPO-ROOT for resolution.
Preserves legacy behaviour for tests and any non-git invocation."
  (claude-repl-test--with-clean-registry
    (let ((captured nil))
      (claude-repl--register-merge-handler
       'noop (lambda (ws args)
               (setq captured (list :ws ws :args args))))
      (claude-repl-test--with-temp-repo root
        (claude-repl-test--seed-merge-config
         root "((handler . noop) (args . (:from root)))")
        (cl-letf (((symbol-function 'claude-repl--main-worktree-path)
                   (lambda (_dir) nil)))
          (claude-repl--dispatch-merge-handler "DWC/foo" root)
          (should (equal (plist-get captured :ws) "DWC/foo"))
          (should (equal (plist-get captured :args) '(:from root))))))))

(ert-deftest claude-repl-test-dispatch-errors-on-missing-registry-entry ()
  "Dispatcher signals user-error if resolved symbol has no fn (registry empty)."
  (let ((claude-repl--merge-handler-registry nil)
        (claude-repl-workspace-merge-handler-overrides nil))
    (cl-letf (((symbol-function 'claude-repl--main-worktree-path)
               (lambda (dir) dir)))
      (claude-repl-test--with-temp-repo root
        (should-error
         (claude-repl--dispatch-merge-handler "DWC/foo" root)
         :type 'user-error)))))

;;;; ---- Tests: register-merge-handler ----

(ert-deftest claude-repl-test-register-replaces-existing-entry ()
  "Re-registering a symbol replaces the old function rather than duplicating."
  (claude-repl-test--with-clean-registry
    (claude-repl--register-merge-handler 'noop (lambda (&rest _) 'first))
    (claude-repl--register-merge-handler 'noop (lambda (&rest _) 'second))
    (let ((matches (cl-remove-if-not
                    (lambda (entry) (eq (car entry) 'noop))
                    claude-repl--merge-handler-registry)))
      (should (= (length matches) 1))
      (should (eq (funcall (cdr (car matches))) 'second)))))

;;;; ---- Tests: cherry-pick handler ----

(ert-deftest claude-repl-test-cherry-pick-handler-calls-merge-into-source ()
  "The cherry-pick handler invokes --workspace-merge-into-source with silent + auto-resolve."
  (let ((captured nil))
    (cl-letf (((symbol-function 'claude-repl--workspace-merge-into-source)
               (lambda (ws silent auto-resolve)
                 (setq captured (list ws silent auto-resolve)))))
      (claude-repl--merge-handler-cherry-pick "DWC/foo")
      (should (equal captured '("DWC/foo" t t))))))

;;;; ---- Tests: refresh-master-from-origin handler ----
;;
;; The handler now defers to the main thread to start async PR polling
;; rather than doing synchronous git work directly.  Tests mock
;; `claude-repl--pr-poll-start' to capture whether it was called and
;; with which arguments, and verify the dirty-main early-exit still
;; signals correctly.
;;
;; Git work (fetch, ff, checkout, close, magit-refresh) is now done
;; inside `claude-repl--pr-poll-on-merged' when the poll sentinel
;; detects a MERGED state — covered by the pr-poll tests below.

(defmacro claude-repl-test--with-poll-start-mock (&rest body)
  "Run BODY with `claude-repl--pr-poll-start' captured but not invoked.
Binds `poll-start-calls' to a list of `(ws project-dir main-dir)'
argument lists, one per call."
  (declare (indent 0))
  `(let ((poll-start-calls nil))
     (cl-letf (((symbol-function 'claude-repl--pr-poll-start)
                (lambda (ws project-dir main-dir)
                  (push (list ws project-dir main-dir)
                        poll-start-calls))))
       ,@body)))

(ert-deftest claude-repl-test-refresh-master-defers-poll-start-when-clean ()
  "Handler defers `--pr-poll-start' to the main thread when clean."
  (claude-repl-test--with-clean-state
    (let ((claude-repl--workspaces (make-hash-table :test 'equal)))
      (claude-repl--ws-put "foo" :project-dir "/repo/wt-foo")
      (cl-letf (((symbol-function 'claude-repl--main-worktree-path)
                 (lambda (_dir) "/repo/main"))
                ((symbol-function 'claude-repl--worktree-dirty-p)
                 (lambda (_dir) nil)))
        (claude-repl-test--with-poll-start-mock
          (claude-repl--merge-handler-refresh-master-from-origin "foo")
          (should (= 1 (length poll-start-calls)))
          (should (equal (car poll-start-calls)
                         '("foo" "/repo/wt-foo" "/repo/main"))))))))

(ert-deftest claude-repl-test-refresh-master-poll-uses-project-dir-as-gh-cwd ()
  "Poll is started with :project-dir as the `gh' working directory."
  (claude-repl-test--with-clean-state
    (let ((claude-repl--workspaces (make-hash-table :test 'equal)))
      (claude-repl--ws-put "foo" :project-dir "/repo/wt-foo")
      (cl-letf (((symbol-function 'claude-repl--main-worktree-path)
                 (lambda (_dir) "/repo/main"))
                ((symbol-function 'claude-repl--worktree-dirty-p)
                 (lambda (_dir) nil)))
        (claude-repl-test--with-poll-start-mock
          (claude-repl--merge-handler-refresh-master-from-origin "foo")
          (let ((call (car poll-start-calls)))
            (should (equal (nth 1 call) "/repo/wt-foo"))))))))

(ert-deftest claude-repl-test-refresh-master-poll-passes-main-dir ()
  "Poll is started with the resolved main worktree so on-merged can fetch."
  (claude-repl-test--with-clean-state
    (let ((claude-repl--workspaces (make-hash-table :test 'equal)))
      (claude-repl--ws-put "foo" :project-dir "/repo/wt-foo")
      (cl-letf (((symbol-function 'claude-repl--main-worktree-path)
                 (lambda (_dir) "/repo/main"))
                ((symbol-function 'claude-repl--worktree-dirty-p)
                 (lambda (_dir) nil)))
        (claude-repl-test--with-poll-start-mock
          (claude-repl--merge-handler-refresh-master-from-origin "foo")
          (let ((call (car poll-start-calls)))
            (should (equal (nth 2 call) "/repo/main"))))))))

(ert-deftest claude-repl-test-refresh-master-poll-nil-main-dir-when-unresolvable ()
  "When `--main-worktree-path' returns nil, poll-start still gets called
with nil main-dir (git work will be skipped by `--pr-poll-on-merged')."
  (claude-repl-test--with-clean-state
    (let ((claude-repl--workspaces (make-hash-table :test 'equal)))
      (claude-repl--ws-put "foo" :project-dir "/repo/wt-foo")
      (cl-letf (((symbol-function 'claude-repl--main-worktree-path)
                 (lambda (_dir) nil)))
        (claude-repl-test--with-poll-start-mock
          (claude-repl--merge-handler-refresh-master-from-origin "foo")
          (should (= 1 (length poll-start-calls)))
          (should (null (nth 2 (car poll-start-calls)))))))))

(ert-deftest claude-repl-test-refresh-master-skips-when-no-source-dir ()
  "Handler no-ops silently when neither :project-dir nor :source-ws-dir is set."
  (claude-repl-test--with-clean-state
    (let ((claude-repl--workspaces (make-hash-table :test 'equal)))
      ;; "foo" has no :project-dir at all
      (claude-repl--ws-put "foo" :repl-state nil)
      (cl-letf (((symbol-function 'claude-repl--main-worktree-path)
                 (lambda (_dir) (error "should not be called"))))
        (claude-repl-test--with-poll-start-mock
          (claude-repl--merge-handler-refresh-master-from-origin "foo")
          (should-not poll-start-calls))))))

(ert-deftest claude-repl-test-refresh-master-errors-on-dirty-main ()
  "A dirty main worktree signals `user-error' so the merge-async failure
path can re-enqueue with :halt-until-human t."
  (claude-repl-test--with-clean-state
    (let ((claude-repl--workspaces (make-hash-table :test 'equal)))
      (claude-repl--ws-put "foo" :project-dir "/repo/wt-foo")
      (cl-letf (((symbol-function 'claude-repl--main-worktree-path)
                 (lambda (_dir) "/repo/main"))
                ((symbol-function 'claude-repl--worktree-dirty-p)
                 (lambda (_dir) t)))
        (claude-repl-test--with-poll-start-mock
          (should-error
           (claude-repl--merge-handler-refresh-master-from-origin "foo")
           :type 'user-error)
          (should-not poll-start-calls))))))

(ert-deftest claude-repl-test-refresh-master-does-not-mark-merged-when-dirty ()
  "Dirty main worktree signals out before any state mutation."
  (claude-repl-test--with-clean-state
    (let ((claude-repl--workspaces (make-hash-table :test 'equal)))
      (claude-repl--ws-put "foo" :project-dir "/repo/wt-foo")
      (cl-letf (((symbol-function 'claude-repl--main-worktree-path)
                 (lambda (_dir) "/repo/main"))
                ((symbol-function 'claude-repl--worktree-dirty-p)
                 (lambda (_dir) t)))
        (claude-repl-test--with-poll-start-mock
          (should-error
           (claude-repl--merge-handler-refresh-master-from-origin "foo")
           :type 'user-error)
          (should-not (claude-repl--ws-get "foo" :merge-completed))
          (should-not (eq (claude-repl--ws-get "foo" :repl-state) :merged)))))))

(ert-deftest claude-repl-test-refresh-master-handler-registered ()
  "The handler symbol is wired into the registry."
  (should (assq 'refresh-master-from-origin
                claude-repl--merge-handler-registry)))

;;;; ---- Tests: PR polling infrastructure ----
;;
;; Shared mock macro for the git/close side effects used by
;; `claude-repl--pr-poll-on-merged'.

(defmacro claude-repl-test--with-on-merged-mocks (&rest body)
  "Run BODY with the git/close side effects of `--pr-poll-on-merged' captured.
Binds `captured' plist with keys :fetch :ff :checkout :close-then
:close :magit-refresh.  Always mocks `--events-record' to a no-op."
  (declare (indent 0))
  `(let ((captured (list :fetch nil :ff nil :checkout nil :close-then nil
                         :close nil :magit-refresh nil)))
     (cl-letf
         (((symbol-function 'claude-repl--events-record) (lambda (&rest _) nil))
          ((symbol-function 'claude-repl--git-exit-code)
           (lambda (&rest args)
             (plist-put captured :fetch (cons args (plist-get captured :fetch)))
             0))
          ((symbol-function 'claude-repl--maybe-fast-forward-master)
           (lambda (dir) (plist-put captured :ff dir)))
          ((symbol-function 'claude-repl--checkout-master-in-worktree)
           (lambda (dir) (plist-put captured :checkout dir) t))
          ((symbol-function 'claude-repl--gns-sockets-close-then)
           (lambda (ws thunk)
             (plist-put captured :close-then ws)
             (funcall thunk)))
          ((symbol-function 'claude-repl--close-workspace)
           (lambda (ws &optional preserve)
             (plist-put captured :close (list ws preserve))))
          ((symbol-function 'claude-repl--refresh-magit-status-for-dir)
           (lambda (dir &optional ws)
             (plist-put captured :magit-refresh (list dir ws)))))
       ,@body)))

;;;; ---- Tests: pr-poll-cancel ----

(ert-deftest claude-repl-test-pr-poll-cancel-removes-active-timer ()
  "Cancel removes the timer from the hash table."
  (let ((claude-repl--active-pr-polls (make-hash-table :test 'equal))
        (fake-timer (cons 'timer nil)))
    (puthash "foo" fake-timer claude-repl--active-pr-polls)
    (cl-letf (((symbol-function 'cancel-timer) (lambda (_t) nil)))
      (claude-repl--pr-poll-cancel "foo"))
    (should-not (gethash "foo" claude-repl--active-pr-polls))))

(ert-deftest claude-repl-test-pr-poll-cancel-noops-when-no-poll ()
  "Cancel is a no-op and does not error when no poll is active."
  (let ((claude-repl--active-pr-polls (make-hash-table :test 'equal)))
    (should-not (claude-repl--pr-poll-cancel "nonexistent"))))

;;;; ---- Tests: pr-poll-handle-result ----

(ert-deftest claude-repl-test-pr-poll-handle-result-merged-calls-on-merged ()
  "MERGED state cancels the poll and calls `--pr-poll-on-merged'."
  (let ((claude-repl--active-pr-polls (make-hash-table :test 'equal))
        (on-merged-called nil))
    (cl-letf (((symbol-function 'claude-repl--pr-poll-cancel)
               (lambda (ws) (setq on-merged-called (list :cancel ws))))
              ((symbol-function 'claude-repl--pr-poll-on-merged)
               (lambda (ws main-dir)
                 (setq on-merged-called (list :merged ws main-dir)))))
      (claude-repl--pr-poll-handle-result
       "foo" "/repo/wt" "/repo/main"
       "{\"state\":\"MERGED\",\"mergedAt\":\"2024-01-01\",\"number\":42}")
      (should (equal on-merged-called '(:merged "foo" "/repo/main"))))))

(ert-deftest claude-repl-test-pr-poll-handle-result-closed-calls-on-failed ()
  "CLOSED state cancels the poll and calls `--pr-poll-on-failed'."
  (let ((claude-repl--active-pr-polls (make-hash-table :test 'equal))
        (on-failed-called nil))
    (cl-letf (((symbol-function 'claude-repl--pr-poll-cancel)
               (lambda (_ws) nil))
              ((symbol-function 'claude-repl--pr-poll-on-failed)
               (lambda (ws) (setq on-failed-called ws))))
      (claude-repl--pr-poll-handle-result
       "foo" "/repo/wt" "/repo/main"
       "{\"state\":\"CLOSED\",\"mergedAt\":null,\"number\":42}")
      (should (equal on-failed-called "foo")))))

(ert-deftest claude-repl-test-pr-poll-handle-result-open-does-nothing ()
  "OPEN state leaves poll running and calls neither on-merged nor on-failed."
  (let ((claude-repl--active-pr-polls (make-hash-table :test 'equal))
        (side-effects nil))
    (cl-letf (((symbol-function 'claude-repl--pr-poll-cancel)
               (lambda (_ws) (push :cancel side-effects)))
              ((symbol-function 'claude-repl--pr-poll-on-merged)
               (lambda (&rest _) (push :merged side-effects)))
              ((symbol-function 'claude-repl--pr-poll-on-failed)
               (lambda (&rest _) (push :failed side-effects))))
      (claude-repl--pr-poll-handle-result
       "foo" "/repo/wt" "/repo/main"
       "{\"state\":\"OPEN\",\"mergedAt\":null,\"number\":42}")
      (should-not side-effects))))

(ert-deftest claude-repl-test-pr-poll-handle-result-invalid-json-does-nothing ()
  "Unparseable output leaves the poll running — transient gh failures
should not abort the polling loop."
  (let ((claude-repl--active-pr-polls (make-hash-table :test 'equal))
        (side-effects nil))
    (cl-letf (((symbol-function 'claude-repl--pr-poll-cancel)
               (lambda (_ws) (push :cancel side-effects)))
              ((symbol-function 'claude-repl--pr-poll-on-merged)
               (lambda (&rest _) (push :merged side-effects)))
              ((symbol-function 'claude-repl--pr-poll-on-failed)
               (lambda (&rest _) (push :failed side-effects))))
      (claude-repl--pr-poll-handle-result
       "foo" "/repo/wt" "/repo/main"
       "not json at all")
      (should-not side-effects))))

;;;; ---- Tests: pr-poll-start ----

(ert-deftest claude-repl-test-pr-poll-start-fires-immediate-tick ()
  "The first tick is fired synchronously on poll start."
  (let ((claude-repl--active-pr-polls (make-hash-table :test 'equal))
        (tick-count 0))
    (cl-letf (((symbol-function 'claude-repl--pr-poll-tick)
               (lambda (_ws _dir _main) (cl-incf tick-count)))
              ((symbol-function 'run-with-timer)
               (lambda (&rest _) nil)))
      (claude-repl--pr-poll-start "foo" "/repo/wt" "/repo/main")
      (should (= 1 tick-count)))))

(ert-deftest claude-repl-test-pr-poll-start-registers-repeating-timer ()
  "After the immediate tick, a repeating timer is registered."
  (let ((claude-repl--active-pr-polls (make-hash-table :test 'equal))
        (timer-args nil))
    (cl-letf (((symbol-function 'claude-repl--pr-poll-tick)
               (lambda (&rest _) nil))
              ((symbol-function 'run-with-timer)
               (lambda (&rest args) (setq timer-args args) 'fake-timer)))
      (claude-repl--pr-poll-start "foo" "/repo/wt" "/repo/main")
      ;; run-with-timer called with (delay repeat fn args...)
      (should timer-args)
      (should (eq (nth 1 timer-args) claude-repl-pr-poll-interval))
      (should (gethash "foo" claude-repl--active-pr-polls)))))

(ert-deftest claude-repl-test-pr-poll-start-cancels-existing-before-starting ()
  "Starting a new poll for a workspace cancels any pre-existing poll."
  (let ((claude-repl--active-pr-polls (make-hash-table :test 'equal))
        (cancel-called-for nil))
    (puthash "foo" 'old-timer claude-repl--active-pr-polls)
    (cl-letf (((symbol-function 'cancel-timer)
               (lambda (t) (setq cancel-called-for t)))
              ((symbol-function 'claude-repl--pr-poll-tick)
               (lambda (&rest _) nil))
              ((symbol-function 'run-with-timer)
               (lambda (&rest _) 'new-timer)))
      (claude-repl--pr-poll-start "foo" "/repo/wt" "/repo/main")
      (should cancel-called-for))))

;;;; ---- Tests: pr-poll-on-merged ----

(ert-deftest claude-repl-test-pr-poll-on-merged-marks-ws-merged ()
  ":merge-completed t and :repl-state :merged after a successful poll merge."
  (claude-repl-test--with-clean-state
    (let ((claude-repl--workspaces (make-hash-table :test 'equal)))
      (claude-repl--ws-put "foo" :project-dir "/repo/wt-foo")
      (claude-repl-test--with-on-merged-mocks
        (claude-repl--pr-poll-on-merged "foo" "/repo/main")
        (should (eq (claude-repl--ws-get "foo" :merge-completed) t))
        (should (eq (claude-repl--ws-get "foo" :repl-state) :merged))
        (should-not (claude-repl--ws-get "foo" :merging))))))

(ert-deftest claude-repl-test-pr-poll-on-merged-fetches-origin ()
  "on-merged runs `git fetch origin master' in the main worktree."
  (claude-repl-test--with-clean-state
    (let ((claude-repl--workspaces (make-hash-table :test 'equal)))
      (claude-repl--ws-put "foo" :project-dir "/repo/wt-foo")
      (claude-repl-test--with-on-merged-mocks
        (claude-repl--pr-poll-on-merged "foo" "/repo/main")
        (let ((calls (plist-get captured :fetch)))
          (should (= 1 (length calls)))
          (should (equal (car calls)
                         (list "/repo/main" "fetch" "origin"
                               claude-repl-master-branch-name))))))))

(ert-deftest claude-repl-test-pr-poll-on-merged-calls-ff-and-checkout ()
  "on-merged invokes fast-forward and checkout on the main worktree."
  (claude-repl-test--with-clean-state
    (let ((claude-repl--workspaces (make-hash-table :test 'equal)))
      (claude-repl--ws-put "foo" :project-dir "/repo/wt-foo")
      (claude-repl-test--with-on-merged-mocks
        (claude-repl--pr-poll-on-merged "foo" "/repo/main")
        (should (equal (plist-get captured :ff) "/repo/main"))
        (should (equal (plist-get captured :checkout) "/repo/main"))))))

(ert-deftest claude-repl-test-pr-poll-on-merged-records-merge-target-from-main ()
  ":merge-target-name is read from the main worktree's branch."
  (claude-repl-test--with-clean-state
    (let ((claude-repl--workspaces (make-hash-table :test 'equal)))
      (claude-repl--ws-put "foo" :project-dir "/repo/wt-foo")
      (cl-letf (((symbol-function 'claude-repl--git-branch-of-dir)
                 (lambda (_dir) "master")))
        (claude-repl-test--with-on-merged-mocks
          (claude-repl--pr-poll-on-merged "foo" "/repo/main")
          (should (equal (claude-repl--ws-get "foo" :merge-target-name)
                         "master")))))))

(ert-deftest claude-repl-test-pr-poll-on-merged-merge-target-falls-back ()
  "When the main branch can't be read, :merge-target-name falls back to master-name."
  (claude-repl-test--with-clean-state
    (let ((claude-repl--workspaces (make-hash-table :test 'equal)))
      (claude-repl--ws-put "foo" :project-dir "/repo/wt-foo")
      (cl-letf (((symbol-function 'claude-repl--git-branch-of-dir)
                 (lambda (_dir) nil)))
        (claude-repl-test--with-on-merged-mocks
          (claude-repl--pr-poll-on-merged "foo" "/repo/main")
          (should (equal (claude-repl--ws-get "foo" :merge-target-name)
                         claude-repl-master-branch-name)))))))

(ert-deftest claude-repl-test-pr-poll-on-merged-skips-git-when-no-main-dir ()
  "When main-dir is nil, fetch/ff/checkout are skipped but state is still set."
  (claude-repl-test--with-clean-state
    (let ((claude-repl--workspaces (make-hash-table :test 'equal)))
      (claude-repl--ws-put "foo" :project-dir "/repo/wt-foo")
      (claude-repl-test--with-on-merged-mocks
        (claude-repl--pr-poll-on-merged "foo" nil)
        (should-not (plist-get captured :fetch))
        (should-not (plist-get captured :ff))
        (should-not (plist-get captured :checkout))
        (should (eq (claude-repl--ws-get "foo" :merge-completed) t))))))

(ert-deftest claude-repl-test-pr-poll-on-merged-closes-via-gns-sockets ()
  "Teardown is funnelled through `--gns-sockets-close-then'."
  (claude-repl-test--with-clean-state
    (let ((claude-repl--workspaces (make-hash-table :test 'equal)))
      (claude-repl--ws-put "foo" :project-dir "/repo/wt-foo")
      (claude-repl-test--with-on-merged-mocks
        (claude-repl--pr-poll-on-merged "foo" "/repo/main")
        (should (equal (plist-get captured :close-then) "foo"))
        (should (equal (plist-get captured :close)
                       '("foo" preserve-entry)))))))

(ert-deftest claude-repl-test-pr-poll-on-merged-refreshes-magit ()
  "on-merged triggers `--refresh-magit-status-for-dir' for the main worktree."
  (claude-repl-test--with-clean-state
    (let ((claude-repl--workspaces (make-hash-table :test 'equal)))
      (claude-repl--ws-put "foo" :project-dir "/repo/wt-foo")
      (claude-repl-test--with-on-merged-mocks
        (claude-repl--pr-poll-on-merged "foo" "/repo/main")
        (should (equal (plist-get captured :magit-refresh)
                       '("/repo/main" "foo")))))))

(ert-deftest claude-repl-test-pr-poll-on-merged-skips-magit-when-no-main-dir ()
  "When main-dir is nil, no magit refresh is attempted."
  (claude-repl-test--with-clean-state
    (let ((claude-repl--workspaces (make-hash-table :test 'equal)))
      (claude-repl--ws-put "foo" :project-dir "/repo/wt-foo")
      (claude-repl-test--with-on-merged-mocks
        (claude-repl--pr-poll-on-merged "foo" nil)
        (should-not (plist-get captured :magit-refresh))))))

;;;; ---- Tests: pr-poll-on-failed ----

(ert-deftest claude-repl-test-pr-poll-on-failed-marks-merge-failed ()
  "on-failed sets :merge-failed t and :repl-state :merge-failed."
  (claude-repl-test--with-clean-state
    (let ((claude-repl--workspaces (make-hash-table :test 'equal)))
      (claude-repl--ws-put "foo" :project-dir "/repo/wt-foo")
      (cl-letf (((symbol-function 'claude-repl--reopen-workspace-from-state)
                 (lambda (_ws) nil))
                ((symbol-function 'claude-repl--dispatch-prompt-command)
                 (lambda (_ws _msg) nil)))
        (claude-repl--pr-poll-on-failed "foo")
        (should (eq (claude-repl--ws-get "foo" :merge-failed) t))
        (should (eq (claude-repl--ws-get "foo" :repl-state) :merge-failed))
        (should-not (claude-repl--ws-get "foo" :merging))
        (should-not (claude-repl--ws-get "foo" :merge-completed))))))

(ert-deftest claude-repl-test-pr-poll-on-failed-reopens-workspace ()
  "on-failed calls `--reopen-workspace-from-state' to restore the UI."
  (claude-repl-test--with-clean-state
    (let ((claude-repl--workspaces (make-hash-table :test 'equal))
          (reopened nil))
      (claude-repl--ws-put "foo" :project-dir "/repo/wt-foo")
      (cl-letf (((symbol-function 'claude-repl--reopen-workspace-from-state)
                 (lambda (ws) (setq reopened ws)))
                ((symbol-function 'claude-repl--dispatch-prompt-command)
                 (lambda (_ws _msg) nil)))
        (claude-repl--pr-poll-on-failed "foo")
        (should (equal reopened "foo"))))))

(ert-deftest claude-repl-test-pr-poll-on-failed-dispatches-prompt ()
  "on-failed dispatches a failure prompt to the revived workspace."
  (claude-repl-test--with-clean-state
    (let ((claude-repl--workspaces (make-hash-table :test 'equal))
          (prompt-sent nil))
      (claude-repl--ws-put "foo" :project-dir "/repo/wt-foo")
      (cl-letf (((symbol-function 'claude-repl--reopen-workspace-from-state)
                 (lambda (_ws) nil))
                ((symbol-function 'claude-repl--dispatch-prompt-command)
                 (lambda (ws msg) (setq prompt-sent (list ws msg)))))
        (claude-repl--pr-poll-on-failed "foo")
        (should (equal (car prompt-sent) "foo"))
        (should (stringp (cadr prompt-sent)))))))

;;;; ---- Tests: default override is empty (cherry-pick everywhere) ----

(ert-deftest claude-repl-test-default-overrides-are-empty ()
  "The default value of `--workspace-merge-handler-overrides' is nil, so no
repo is special-cased away from the cherry-pick default."
  (let ((default (eval (car (get 'claude-repl-workspace-merge-handler-overrides
                                  'standard-value)))))
    (should-not default)))

(ert-deftest claude-repl-test-default-overrides-omit-explanation-engine ()
  "The explanation-engine repo no longer carries a special-case override —
it merges via the cherry-pick default like every other repo."
  (let ((default (eval (car (get 'claude-repl-workspace-merge-handler-overrides
                                  'standard-value)))))
    (should-not (assoc "~/workspace/ChessCom/explanation-engine" default))))

;;;; ---- Tests: onto-master handler + per-command routing ----

(ert-deftest claude-repl-test-onto-master-handler-registered ()
  "The `onto-master' handler symbol is wired into the registry."
  (should (assq 'onto-master claude-repl--merge-handler-registry)))

(ert-deftest claude-repl-test-dispatch-onto-master-flag-forces-handler ()
  "A non-nil ONTO-MASTER arg forces the `onto-master' handler even when the
repo's `.eld' prescribes a different handler (cherry-pick)."
  (claude-repl-test--with-clean-registry
    (let ((captured nil))
      (claude-repl--register-merge-handler
       'onto-master (lambda (ws _args) (setq captured ws)))
      (claude-repl-test--with-temp-repo root
        ;; .eld says cherry-pick, but the flag must win.
        (claude-repl-test--seed-merge-config root "((handler . cherry-pick))")
        (cl-letf (((symbol-function 'claude-repl--main-worktree-path)
                   (lambda (dir) dir)))
          (claude-repl--dispatch-merge-handler "DWC/foo" root t)
          (should (equal captured "DWC/foo")))))))

(ert-deftest claude-repl-test-dispatch-without-onto-master-uses-eld ()
  "Without the flag, dispatch still honors the repo's `.eld' handler."
  (claude-repl-test--with-clean-registry
    (let ((captured nil))
      (claude-repl--register-merge-handler
       'onto-master (lambda (_ws _args) (setq captured 'onto-master)))
      (cl-letf (((symbol-function 'claude-repl--workspace-merge-into-source)
                 (lambda (&rest _) (setq captured 'cherry-pick)))
                ((symbol-function 'claude-repl--main-worktree-path)
                 (lambda (dir) dir)))
        (claude-repl-test--with-temp-repo root
          (claude-repl--dispatch-merge-handler "DWC/foo" root nil)
          (should (eq captured 'cherry-pick)))))))

(ert-deftest claude-repl-test-onto-master-skips-when-no-source-dir ()
  "Handler no-ops (no finalize) when neither :project-dir nor :source-ws-dir set."
  (claude-repl-test--with-clean-state
    (let ((claude-repl--workspaces (make-hash-table :test 'equal))
          (finalized nil))
      (claude-repl--ws-put "foo" :repl-state nil)
      (cl-letf (((symbol-function 'claude-repl--main-worktree-path)
                 (lambda (_dir) (error "should not be called")))
                ((symbol-function 'claude-repl--finalize-merged-workspace)
                 (lambda (&rest _) (setq finalized t))))
        (claude-repl--merge-handler-onto-master "foo")
        (should-not finalized)))))

(ert-deftest claude-repl-test-onto-master-errors-on-dirty-main ()
  "A dirty main worktree signals `user-error' before any git advance."
  (claude-repl-test--with-clean-state
    (let ((claude-repl--workspaces (make-hash-table :test 'equal))
          (finalized nil))
      (claude-repl--ws-put "foo" :project-dir "/repo/wt-foo")
      (cl-letf (((symbol-function 'claude-repl--main-worktree-path)
                 (lambda (_dir) "/repo/main"))
                ((symbol-function 'claude-repl--worktree-dirty-p)
                 (lambda (_dir) t))
                ((symbol-function 'claude-repl--git-exit-code)
                 (lambda (&rest _) (error "git should not run on dirty main")))
                ((symbol-function 'claude-repl--finalize-merged-workspace)
                 (lambda (&rest _) (setq finalized t))))
        (should-error (claude-repl--merge-handler-onto-master "foo")
                      :type 'user-error)
        (should-not finalized)))))

(ert-deftest claude-repl-test-onto-master-errors-on-divergence ()
  "Local master diverged from origin/master signals `user-error' (ff-only,
no reset over divergent commits) and never finalizes."
  (claude-repl-test--with-clean-state
    (let ((claude-repl--workspaces (make-hash-table :test 'equal))
          (finalized nil))
      (claude-repl--ws-put "foo" :project-dir "/repo/wt-foo")
      (cl-letf (((symbol-function 'claude-repl--main-worktree-path)
                 (lambda (_dir) "/repo/main"))
                ((symbol-function 'claude-repl--worktree-dirty-p)
                 (lambda (_dir) nil))
                ((symbol-function 'claude-repl--git-exit-code)
                 (lambda (&rest args)
                   ;; fetch -> 0; is-ancestor -> 1 (diverged).
                   (if (member "merge-base" args) 1 0)))
                ((symbol-function 'claude-repl--finalize-merged-workspace)
                 (lambda (&rest _) (setq finalized t))))
        (should-error (claude-repl--merge-handler-onto-master "foo")
                      :type 'user-error)
        (should-not finalized)))))

(ert-deftest claude-repl-test-onto-master-ff-and-finalizes ()
  "Happy path: clean main, ancestor, ff succeeds in the master worktree, then
finalize tears the workspace down on the (synchronous-in-test) main thread."
  (claude-repl-test--with-clean-state
    (let ((claude-repl--workspaces (make-hash-table :test 'equal))
          (git-calls nil)
          (finalized nil))
      (claude-repl--ws-put "foo" :project-dir "/repo/wt-foo")
      (cl-letf (((symbol-function 'claude-repl--main-worktree-path)
                 (lambda (_dir) "/repo/main"))
                ((symbol-function 'claude-repl--master-worktree-path)
                 (lambda (_dir) "/repo/main"))
                ((symbol-function 'claude-repl--worktree-dirty-p)
                 (lambda (_dir) nil))
                ((symbol-function 'claude-repl--git-exit-code)
                 (lambda (&rest args) (push args git-calls) 0))
                ((symbol-function 'claude-repl--finalize-merged-workspace)
                 (lambda (ws main-dir) (setq finalized (list ws main-dir)))))
        (claude-repl--merge-handler-onto-master "foo")
        (should (equal finalized '("foo" "/repo/main")))
        ;; A fast-forward merge was issued in the master worktree.
        (should (cl-some (lambda (args)
                           (and (member "merge" args) (member "--ff-only" args)))
                         git-calls))))))

(provide 'test-merge-handlers)

;;; test-merge-handlers.el ends here
