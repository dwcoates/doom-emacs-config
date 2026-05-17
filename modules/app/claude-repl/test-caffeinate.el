;;; test-caffeinate.el --- ERT tests for caffeinate.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for the macOS sleep-prevention module.
;;
;; The caffeinate subprocess is faked via a `claude-repl-test--fake-process'
;; struct so the tests can drive transitions without spawning real
;; processes.  All process-mutating primitives the module uses
;; (`start-process', `delete-process', `process-live-p',
;; `set-process-query-on-exit-flag', `process-id', `executable-find')
;; are shadowed with `cl-letf' wrappers that route through the fake.
;;
;; Run with:
;;   emacs -batch -Q -l ert -l test-caffeinate.el -f ert-run-tests-batch-and-exit

;;; Code:

(load (expand-file-name "test-helpers.el" (file-name-directory
                                            (or load-file-name buffer-file-name)))
      nil t)

(require 'cl-lib)

;;;; ---- Fake process layer ---------------------------------------------------

(cl-defstruct claude-repl-test--fake-process
  name args live pid)

(defvar claude-repl-test--fake-spawn-log nil
  "List of (NAME PROGRAM &rest ARGS) tuples, one per faked `start-process'.")

(defvar claude-repl-test--fake-pid-counter 1000
  "Monotonic counter used to hand out fake PIDs.")

(defun claude-repl-test--fake-start-process (name _buffer program &rest args)
  "Faked `start-process': record the spawn and return a live fake process."
  (push (cons name (cons program args)) claude-repl-test--fake-spawn-log)
  (cl-incf claude-repl-test--fake-pid-counter)
  (make-claude-repl-test--fake-process
   :name name
   :args (cons program args)
   :live t
   :pid claude-repl-test--fake-pid-counter))

(defun claude-repl-test--fake-delete-process (proc)
  "Faked `delete-process': flip the live flag off."
  (when (claude-repl-test--fake-process-p proc)
    (setf (claude-repl-test--fake-process-live proc) nil)))

(defun claude-repl-test--fake-process-live-p (proc)
  "Faked `process-live-p' for our struct."
  (and (claude-repl-test--fake-process-p proc)
       (claude-repl-test--fake-process-live proc)))

(defun claude-repl-test--fake-process-id (proc)
  "Faked `process-id' for our struct."
  (when (claude-repl-test--fake-process-p proc)
    (claude-repl-test--fake-process-pid proc)))

(defun claude-repl-test--fake-set-process-query-on-exit-flag (_proc _flag)
  "No-op fake for `set-process-query-on-exit-flag'."
  nil)

(defmacro claude-repl-test--with-fake-caffeinate (&rest body)
  "Execute BODY with the caffeinate process primitives + system-type stubbed.
Forces `system-type' to `darwin' and `executable-find' to non-nil so
`claude-repl--caffeinate-supported-p' returns t inside BODY, even on
Linux CI.  Resets the spawn log and clears the module's process
handle before BODY and restores afterwards."
  (declare (indent 0))
  `(let ((claude-repl-test--fake-spawn-log nil)
         (claude-repl--caffeinate-process nil)
         (system-type 'darwin)
         (claude-repl-caffeinate-enabled t)
         (claude-repl-caffeinate-active-states '(:thinking))
         (claude-repl-caffeinate-program "caffeinate")
         (claude-repl-caffeinate-args '("-i")))
     (cl-letf (((symbol-function 'start-process)
                #'claude-repl-test--fake-start-process)
               ((symbol-function 'delete-process)
                #'claude-repl-test--fake-delete-process)
               ((symbol-function 'process-live-p)
                #'claude-repl-test--fake-process-live-p)
               ((symbol-function 'process-id)
                #'claude-repl-test--fake-process-id)
               ((symbol-function 'set-process-query-on-exit-flag)
                #'claude-repl-test--fake-set-process-query-on-exit-flag)
               ((symbol-function 'executable-find)
                (lambda (_program) "/usr/bin/caffeinate")))
       ,@body)))

;;;; ---- Tests: --caffeinate-supported-p -------------------------------------

(ert-deftest claude-repl-test-caffeinate-supported-p-on-darwin ()
  "Darwin + enabled + binary present → supported."
  (claude-repl-test--with-fake-caffeinate
    (should (claude-repl--caffeinate-supported-p))))

(ert-deftest claude-repl-test-caffeinate-supported-p-non-darwin ()
  "Non-Darwin platform short-circuits to nil even if binary is present."
  (claude-repl-test--with-fake-caffeinate
    (let ((system-type 'gnu/linux))
      (should-not (claude-repl--caffeinate-supported-p)))))

(ert-deftest claude-repl-test-caffeinate-supported-p-disabled ()
  "Disabling via custom flag short-circuits to nil."
  (claude-repl-test--with-fake-caffeinate
    (let ((claude-repl-caffeinate-enabled nil))
      (should-not (claude-repl--caffeinate-supported-p)))))

(ert-deftest claude-repl-test-caffeinate-supported-p-missing-binary ()
  "Missing caffeinate binary short-circuits to nil."
  (claude-repl-test--with-fake-caffeinate
    (cl-letf (((symbol-function 'executable-find) (lambda (_program) nil)))
      (should-not (claude-repl--caffeinate-supported-p)))))

;;;; ---- Tests: --caffeinate-any-active-p ------------------------------------

(ert-deftest claude-repl-test-caffeinate-any-active-p-empty ()
  "No workspaces → not active."
  (claude-repl-test--with-clean-state
    (should-not (claude-repl--caffeinate-any-active-p))))

(ert-deftest claude-repl-test-caffeinate-any-active-p-thinking ()
  "One workspace in :thinking → active."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "ws1" :claude-state :thinking)
    (should (claude-repl--caffeinate-any-active-p))))

(ert-deftest claude-repl-test-caffeinate-any-active-p-only-done ()
  "All workspaces in :done → not active."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "ws1" :claude-state :done)
    (claude-repl--ws-put "ws2" :claude-state :idle)
    (should-not (claude-repl--caffeinate-any-active-p))))

(ert-deftest claude-repl-test-caffeinate-any-active-p-mixed ()
  "Mixed states with at least one :thinking → active."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "ws1" :claude-state :done)
    (claude-repl--ws-put "ws2" :claude-state :thinking)
    (claude-repl--ws-put "ws3" :claude-state :idle)
    (should (claude-repl--caffeinate-any-active-p))))

(ert-deftest claude-repl-test-caffeinate-any-active-p-respects-active-states-custom ()
  "Customizing `claude-repl-caffeinate-active-states' shifts the predicate."
  (claude-repl-test--with-clean-state
    (let ((claude-repl-caffeinate-active-states '(:permission)))
      (claude-repl--ws-put "ws1" :claude-state :thinking)
      (should-not (claude-repl--caffeinate-any-active-p))
      (claude-repl--ws-put "ws1" :claude-state :permission)
      (should (claude-repl--caffeinate-any-active-p)))))

;;;; ---- Tests: --caffeinate-any-merging-p -----------------------------------

(ert-deftest claude-repl-test-caffeinate-any-merging-p-empty ()
  "No workspaces → not merging."
  (claude-repl-test--with-clean-state
    (should-not (claude-repl--caffeinate-any-merging-p))))

(ert-deftest claude-repl-test-caffeinate-any-merging-p-merging-in-flight ()
  "One workspace with `:merging t' → merging-active."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "ws1" :merging t)
    (should (claude-repl--caffeinate-any-merging-p))))

(ert-deftest claude-repl-test-caffeinate-any-merging-p-merge-queued ()
  "One workspace with `:repl-state :merge-queued' → merging-active."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "ws1" :repl-state :merge-queued)
    (should (claude-repl--caffeinate-any-merging-p))))

(ert-deftest claude-repl-test-caffeinate-any-merging-p-merge-completed-not-active ()
  "Workspace with `:merge-completed t' (and no in-flight markers) → not merging.
A finished merge has no more work for the editor."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "ws1" :merge-completed t)
    (claude-repl--ws-put "ws1" :repl-state :merged)
    (should-not (claude-repl--caffeinate-any-merging-p))))

(ert-deftest claude-repl-test-caffeinate-any-merging-p-merge-conflict-not-active ()
  "Workspace with `:repl-state :merge-conflict' → not merging.
A conflict awaiting human resolution is bottlenecked on the user — same
exclusion principle as `:permission'."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "ws1" :repl-state :merge-conflict)
    (should-not (claude-repl--caffeinate-any-merging-p))))

(ert-deftest claude-repl-test-caffeinate-any-merging-p-merge-failed-not-active ()
  "Workspace with `:repl-state :merge-failed' → not merging (terminal)."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "ws1" :repl-state :merge-failed)
    (should-not (claude-repl--caffeinate-any-merging-p))))

(ert-deftest claude-repl-test-caffeinate-any-merging-p-cleared-merging-not-active ()
  "Workspace where `:merging' was set then cleared → not merging."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "ws1" :merging t)
    (should (claude-repl--caffeinate-any-merging-p))
    (claude-repl--ws-put "ws1" :merging nil)
    (should-not (claude-repl--caffeinate-any-merging-p))))

;;;; ---- Tests: --caffeinate-any-active-p OR-composition --------------------

(ert-deftest claude-repl-test-caffeinate-any-active-p-merging-only ()
  "Workspace `:done' but `:merging t' → active via the merging branch.
This is the workspace-merge race the module exists to cover: Claude has
landed at `:done' but the sentinel-driven cherry-pick is still in flight."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "ws1" :claude-state :done)
    (claude-repl--ws-put "ws1" :merging t)
    (should (claude-repl--caffeinate-any-active-p))))

(ert-deftest claude-repl-test-caffeinate-any-active-p-merge-queued-only ()
  "Workspace `:done' but `:repl-state :merge-queued' → active via merging branch."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "ws1" :claude-state :done)
    (claude-repl--ws-put "ws1" :repl-state :merge-queued)
    (should (claude-repl--caffeinate-any-active-p))))

(ert-deftest claude-repl-test-caffeinate-any-active-p-both-resolved ()
  "Workspace `:done' AND `:merge-completed t' (and no in-flight) → not active."
  (claude-repl-test--with-clean-state
    (claude-repl--ws-put "ws1" :claude-state :done)
    (claude-repl--ws-put "ws1" :merge-completed t)
    (claude-repl--ws-put "ws1" :repl-state :merged)
    (should-not (claude-repl--caffeinate-any-active-p))))

;;;; ---- Tests: start / stop / running-p -------------------------------------

(ert-deftest claude-repl-test-caffeinate-start-spawns-once ()
  "Calling --caffeinate-start twice spawns only once."
  (claude-repl-test--with-fake-caffeinate
    (claude-repl--caffeinate-start)
    (claude-repl--caffeinate-start)
    (should (= 1 (length claude-repl-test--fake-spawn-log)))
    (should (claude-repl--caffeinate-running-p))))

(ert-deftest claude-repl-test-caffeinate-start-passes-program-and-args ()
  "Spawn uses `claude-repl-caffeinate-program' + `-args' verbatim."
  (claude-repl-test--with-fake-caffeinate
    (let ((claude-repl-caffeinate-program "caffeinate")
          (claude-repl-caffeinate-args '("-i" "-d")))
      (claude-repl--caffeinate-start)
      (let ((spawn (car claude-repl-test--fake-spawn-log)))
        (should (equal (car spawn) "claude-repl-caffeinate"))
        (should (equal (cdr spawn) '("caffeinate" "-i" "-d")))))))

(ert-deftest claude-repl-test-caffeinate-stop-kills-and-clears ()
  "Stop kills the process and clears the module handle."
  (claude-repl-test--with-fake-caffeinate
    (claude-repl--caffeinate-start)
    (should (claude-repl--caffeinate-running-p))
    (claude-repl--caffeinate-stop)
    (should-not (claude-repl--caffeinate-running-p))
    (should-not claude-repl--caffeinate-process)))

(ert-deftest claude-repl-test-caffeinate-stop-noop-when-not-running ()
  "Stop is idempotent: a no-op when nothing is live."
  (claude-repl-test--with-fake-caffeinate
    (claude-repl--caffeinate-stop)
    (should-not claude-repl--caffeinate-process)))

;;;; ---- Tests: --caffeinate-refresh -----------------------------------------

(ert-deftest claude-repl-test-caffeinate-refresh-starts-on-thinking ()
  "Refresh spawns caffeinate when a workspace is :thinking."
  (claude-repl-test--with-clean-state
    (claude-repl-test--with-fake-caffeinate
      (claude-repl--ws-put "ws1" :claude-state :thinking)
      (claude-repl--caffeinate-refresh)
      (should (claude-repl--caffeinate-running-p))
      (should (= 1 (length claude-repl-test--fake-spawn-log))))))

(ert-deftest claude-repl-test-caffeinate-refresh-stops-when-all-resolve ()
  "Refresh stops caffeinate once every workspace leaves an active state."
  (claude-repl-test--with-clean-state
    (claude-repl-test--with-fake-caffeinate
      (claude-repl--ws-put "ws1" :claude-state :thinking)
      (claude-repl--caffeinate-refresh)
      (should (claude-repl--caffeinate-running-p))
      (claude-repl--ws-put "ws1" :claude-state :done)
      (claude-repl--caffeinate-refresh)
      (should-not (claude-repl--caffeinate-running-p)))))

(ert-deftest claude-repl-test-caffeinate-refresh-keeps-running-with-survivor ()
  "Refresh keeps caffeinate alive while ANY workspace is still :thinking."
  (claude-repl-test--with-clean-state
    (claude-repl-test--with-fake-caffeinate
      (claude-repl--ws-put "ws1" :claude-state :thinking)
      (claude-repl--ws-put "ws2" :claude-state :thinking)
      (claude-repl--caffeinate-refresh)
      (should (claude-repl--caffeinate-running-p))
      (claude-repl--ws-put "ws1" :claude-state :done)
      (claude-repl--caffeinate-refresh)
      (should (claude-repl--caffeinate-running-p))
      (should (= 1 (length claude-repl-test--fake-spawn-log))))))

(ert-deftest claude-repl-test-caffeinate-refresh-noop-on-non-darwin ()
  "Refresh is inert on non-Darwin even if a workspace is :thinking."
  (claude-repl-test--with-clean-state
    (claude-repl-test--with-fake-caffeinate
      (let ((system-type 'gnu/linux))
        (claude-repl--ws-put "ws1" :claude-state :thinking)
        (claude-repl--caffeinate-refresh)
        (should-not (claude-repl--caffeinate-running-p))
        (should-not claude-repl-test--fake-spawn-log)))))

(ert-deftest claude-repl-test-caffeinate-refresh-noop-when-disabled ()
  "Refresh is inert when `claude-repl-caffeinate-enabled' is nil."
  (claude-repl-test--with-clean-state
    (claude-repl-test--with-fake-caffeinate
      (let ((claude-repl-caffeinate-enabled nil))
        (claude-repl--ws-put "ws1" :claude-state :thinking)
        (claude-repl--caffeinate-refresh)
        (should-not (claude-repl--caffeinate-running-p))))))

;;;; ---- Tests: advice integration with --ws-set-claude-state ----------------

(ert-deftest claude-repl-test-caffeinate-advice-on-set-claude-state-start ()
  "Setting :thinking through the typed setter triggers a caffeinate spawn."
  (claude-repl-test--with-clean-state
    (claude-repl-test--with-fake-caffeinate
      (claude-repl--ws-set-claude-state "ws1" :thinking)
      (should (claude-repl--caffeinate-running-p)))))

(ert-deftest claude-repl-test-caffeinate-advice-on-set-claude-state-stop ()
  "Setting :done through the typed setter after :thinking kills caffeinate."
  (claude-repl-test--with-clean-state
    (claude-repl-test--with-fake-caffeinate
      (claude-repl--ws-set-claude-state "ws1" :thinking)
      (should (claude-repl--caffeinate-running-p))
      (claude-repl--ws-set-claude-state "ws1" :done)
      (should-not (claude-repl--caffeinate-running-p)))))

(ert-deftest claude-repl-test-caffeinate-advice-on-ws-del-orphan-cleanup ()
  "Nuking a still-:thinking workspace stops caffeinate via the --ws-del advice."
  (claude-repl-test--with-clean-state
    (claude-repl-test--with-fake-caffeinate
      (claude-repl--ws-set-claude-state "ws1" :thinking)
      (should (claude-repl--caffeinate-running-p))
      (claude-repl--ws-del "ws1")
      (should-not (claude-repl--caffeinate-running-p)))))

;;;; ---- Tests: advice integration via --ws-put (merge keys) -----------------

(ert-deftest claude-repl-test-caffeinate-advice-on-ws-put-merging-start ()
  "Setting `:merging t' via the central setter triggers a caffeinate spawn."
  (claude-repl-test--with-clean-state
    (claude-repl-test--with-fake-caffeinate
      (claude-repl--ws-put "ws1" :merging t)
      (should (claude-repl--caffeinate-running-p)))))

(ert-deftest claude-repl-test-caffeinate-advice-on-ws-put-merging-clear ()
  "Clearing `:merging' after start stops caffeinate (no other active state)."
  (claude-repl-test--with-clean-state
    (claude-repl-test--with-fake-caffeinate
      (claude-repl--ws-put "ws1" :merging t)
      (should (claude-repl--caffeinate-running-p))
      (claude-repl--ws-put "ws1" :merging nil)
      (should-not (claude-repl--caffeinate-running-p)))))

(ert-deftest claude-repl-test-caffeinate-advice-on-ws-put-merge-queued ()
  "Setting `:repl-state :merge-queued' triggers a caffeinate spawn."
  (claude-repl-test--with-clean-state
    (claude-repl-test--with-fake-caffeinate
      (claude-repl--ws-put "ws1" :repl-state :merge-queued)
      (should (claude-repl--caffeinate-running-p)))))

(ert-deftest claude-repl-test-caffeinate-thinking-to-done-while-merging-keeps-alive ()
  "`:thinking → :done' transition while `:merging t' keeps caffeinate alive.
This is the canonical workspace-merge race: Claude lands on `:done' but
the sentinel-driven cherry-pick is still in flight, so the editor must
stay awake to finish the merge."
  (claude-repl-test--with-clean-state
    (claude-repl-test--with-fake-caffeinate
      (claude-repl--ws-set-claude-state "ws1" :thinking)
      (claude-repl--ws-put "ws1" :merging t)
      (should (claude-repl--caffeinate-running-p))
      (claude-repl--ws-set-claude-state "ws1" :done)
      (should (claude-repl--caffeinate-running-p))
      ;; Only once the merge actually completes do we release caffeinate.
      (claude-repl--ws-put "ws1" :merging nil)
      (claude-repl--ws-put "ws1" :merge-completed t)
      (claude-repl--ws-put "ws1" :repl-state :merged)
      (should-not (claude-repl--caffeinate-running-p)))))

(ert-deftest claude-repl-test-caffeinate-merge-completed-releases ()
  "A merge that lands on `:merge-completed t' (terminal) releases caffeinate."
  (claude-repl-test--with-clean-state
    (claude-repl-test--with-fake-caffeinate
      (claude-repl--ws-put "ws1" :merging t)
      (should (claude-repl--caffeinate-running-p))
      (claude-repl--ws-put "ws1" :merging nil)
      (claude-repl--ws-put "ws1" :merge-completed t)
      (should-not (claude-repl--caffeinate-running-p)))))

(ert-deftest claude-repl-test-caffeinate-merge-conflict-releases ()
  "`:repl-state :merge-conflict' releases caffeinate — user-bottlenecked."
  (claude-repl-test--with-clean-state
    (claude-repl-test--with-fake-caffeinate
      (claude-repl--ws-put "ws1" :merging t)
      (should (claude-repl--caffeinate-running-p))
      (claude-repl--ws-put "ws1" :merging nil)
      (claude-repl--ws-put "ws1" :repl-state :merge-conflict)
      (should-not (claude-repl--caffeinate-running-p)))))

(ert-deftest claude-repl-test-caffeinate-merge-failed-releases ()
  "`:repl-state :merge-failed' (terminal) releases caffeinate."
  (claude-repl-test--with-clean-state
    (claude-repl-test--with-fake-caffeinate
      (claude-repl--ws-put "ws1" :merging t)
      (should (claude-repl--caffeinate-running-p))
      (claude-repl--ws-put "ws1" :merging nil)
      (claude-repl--ws-put "ws1" :repl-state :merge-failed)
      (should-not (claude-repl--caffeinate-running-p)))))

(ert-deftest claude-repl-test-caffeinate-advice-ignores-unwatched-keys ()
  "`--ws-put' on a non-watched key (e.g. `:project-dir') does not spawn caffeinate.
Guards the key filter — without it, every plist mutation would trigger a
reconcile and produce noisy spawn churn under hot paths."
  (claude-repl-test--with-clean-state
    (claude-repl-test--with-fake-caffeinate
      (claude-repl--ws-put "ws1" :project-dir "/tmp/somewhere")
      (should-not (claude-repl--caffeinate-running-p))
      (should-not claude-repl-test--fake-spawn-log))))

(ert-deftest claude-repl-test-caffeinate-advice-on-ws-del-orphan-cleanup-merging ()
  "Nuking a mid-merge workspace stops caffeinate via the --ws-del advice."
  (claude-repl-test--with-clean-state
    (claude-repl-test--with-fake-caffeinate
      (claude-repl--ws-put "ws1" :merging t)
      (should (claude-repl--caffeinate-running-p))
      (claude-repl--ws-del "ws1")
      (should-not (claude-repl--caffeinate-running-p)))))

(provide 'test-caffeinate)

;;; test-caffeinate.el ends here
