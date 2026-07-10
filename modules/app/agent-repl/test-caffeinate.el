;;; test-caffeinate.el --- ERT tests for caffeinate.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for the macOS sleep-prevention module.
;;
;; The caffeinate subprocess is faked via a `agent-repl-test--fake-process'
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

(cl-defstruct agent-repl-test--fake-process
  name args live pid)

(defvar agent-repl-test--fake-spawn-log nil
  "List of (NAME PROGRAM &rest ARGS) tuples, one per faked `start-process'.")

(defvar agent-repl-test--fake-pid-counter 1000
  "Monotonic counter used to hand out fake PIDs.")

(defun agent-repl-test--fake-start-process (name _buffer program &rest args)
  "Faked `start-process': record the spawn and return a live fake process."
  (push (cons name (cons program args)) agent-repl-test--fake-spawn-log)
  (cl-incf agent-repl-test--fake-pid-counter)
  (make-agent-repl-test--fake-process
   :name name
   :args (cons program args)
   :live t
   :pid agent-repl-test--fake-pid-counter))

(defun agent-repl-test--fake-delete-process (proc)
  "Faked `delete-process': flip the live flag off."
  (when (agent-repl-test--fake-process-p proc)
    (setf (agent-repl-test--fake-process-live proc) nil)))

(defun agent-repl-test--fake-process-live-p (proc)
  "Faked `process-live-p' for our struct."
  (and (agent-repl-test--fake-process-p proc)
       (agent-repl-test--fake-process-live proc)))

(defun agent-repl-test--fake-process-id (proc)
  "Faked `process-id' for our struct."
  (when (agent-repl-test--fake-process-p proc)
    (agent-repl-test--fake-process-pid proc)))

(defun agent-repl-test--fake-set-process-query-on-exit-flag (_proc _flag)
  "No-op fake for `set-process-query-on-exit-flag'."
  nil)

(defmacro agent-repl-test--with-fake-caffeinate (&rest body)
  "Execute BODY with the caffeinate process primitives + system-type stubbed.
Forces `system-type' to `darwin' and `executable-find' to non-nil so
`agent-repl--caffeinate-supported-p' returns t inside BODY, even on
Linux CI.  Resets the spawn log and clears the module's process
handle before BODY and restores afterwards."
  (declare (indent 0))
  `(let ((agent-repl-test--fake-spawn-log nil)
         (agent-repl--caffeinate-process nil)
         (system-type 'darwin)
         (agent-repl-caffeinate-enabled t)
         (agent-repl-caffeinate-active-states '(:thinking))
         (agent-repl-caffeinate-program "caffeinate")
         (agent-repl-caffeinate-args '("-i")))
     (cl-letf (((symbol-function 'start-process)
                #'agent-repl-test--fake-start-process)
               ((symbol-function 'delete-process)
                #'agent-repl-test--fake-delete-process)
               ((symbol-function 'process-live-p)
                #'agent-repl-test--fake-process-live-p)
               ((symbol-function 'process-id)
                #'agent-repl-test--fake-process-id)
               ((symbol-function 'set-process-query-on-exit-flag)
                #'agent-repl-test--fake-set-process-query-on-exit-flag)
               ((symbol-function 'executable-find)
                (lambda (_program) "/usr/bin/caffeinate")))
       ,@body)))

;;;; ---- Tests: --caffeinate-supported-p -------------------------------------

(ert-deftest agent-repl-test-caffeinate-supported-p-on-darwin ()
  "Darwin + enabled + binary present → supported."
  (agent-repl-test--with-fake-caffeinate
    (should (agent-repl--caffeinate-supported-p))))

(ert-deftest agent-repl-test-caffeinate-supported-p-non-darwin ()
  "Non-Darwin platform short-circuits to nil even if binary is present."
  (agent-repl-test--with-fake-caffeinate
    (let ((system-type 'gnu/linux))
      (should-not (agent-repl--caffeinate-supported-p)))))

(ert-deftest agent-repl-test-caffeinate-supported-p-disabled ()
  "Disabling via custom flag short-circuits to nil."
  (agent-repl-test--with-fake-caffeinate
    (let ((agent-repl-caffeinate-enabled nil))
      (should-not (agent-repl--caffeinate-supported-p)))))

(ert-deftest agent-repl-test-caffeinate-supported-p-missing-binary ()
  "Missing caffeinate binary short-circuits to nil."
  (agent-repl-test--with-fake-caffeinate
    (cl-letf (((symbol-function 'executable-find) (lambda (_program) nil)))
      (should-not (agent-repl--caffeinate-supported-p)))))

;;;; ---- Tests: --caffeinate-any-active-p ------------------------------------

(ert-deftest agent-repl-test-caffeinate-any-active-p-empty ()
  "No workspaces → not active."
  (agent-repl-test--with-clean-state
    (should-not (agent-repl--caffeinate-any-active-p))))

(ert-deftest agent-repl-test-caffeinate-any-active-p-thinking ()
  "One workspace in :thinking → active."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws1" :claude-state :thinking)
    (should (agent-repl--caffeinate-any-active-p))))

(ert-deftest agent-repl-test-caffeinate-any-active-p-only-done ()
  "All workspaces in :done → not active."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws1" :claude-state :done)
    (agent-repl--ws-put "ws2" :claude-state :idle)
    (should-not (agent-repl--caffeinate-any-active-p))))

(ert-deftest agent-repl-test-caffeinate-any-active-p-mixed ()
  "Mixed states with at least one :thinking → active."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws1" :claude-state :done)
    (agent-repl--ws-put "ws2" :claude-state :thinking)
    (agent-repl--ws-put "ws3" :claude-state :idle)
    (should (agent-repl--caffeinate-any-active-p))))

(ert-deftest agent-repl-test-caffeinate-any-active-p-respects-active-states-custom ()
  "Customizing `agent-repl-caffeinate-active-states' shifts the predicate."
  (agent-repl-test--with-clean-state
    (let ((agent-repl-caffeinate-active-states '(:permission)))
      (agent-repl--ws-put "ws1" :claude-state :thinking)
      (should-not (agent-repl--caffeinate-any-active-p))
      (agent-repl--ws-put "ws1" :claude-state :permission)
      (should (agent-repl--caffeinate-any-active-p)))))

;;;; ---- Tests: --caffeinate-any-merging-p -----------------------------------

(ert-deftest agent-repl-test-caffeinate-any-merging-p-empty ()
  "No workspaces → not merging."
  (agent-repl-test--with-clean-state
    (should-not (agent-repl--caffeinate-any-merging-p))))

(ert-deftest agent-repl-test-caffeinate-any-merging-p-merging-in-flight ()
  "One workspace with `:merging t' → merging-active."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws1" :merging t)
    (should (agent-repl--caffeinate-any-merging-p))))

(ert-deftest agent-repl-test-caffeinate-any-merging-p-merge-queued ()
  "One workspace with `:repl-state :merge-queued' → merging-active."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws1" :repl-state :merge-queued)
    (should (agent-repl--caffeinate-any-merging-p))))

(ert-deftest agent-repl-test-caffeinate-any-merging-p-merge-completed-not-active ()
  "Workspace with `:merge-completed t' (and no in-flight markers) → not merging.
A finished merge has no more work for the editor."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws1" :merge-completed t)
    (agent-repl--ws-put "ws1" :repl-state :merged)
    (should-not (agent-repl--caffeinate-any-merging-p))))

(ert-deftest agent-repl-test-caffeinate-any-merging-p-merge-conflict-not-active ()
  "Workspace with `:repl-state :merge-conflict' → not merging.
A conflict awaiting human resolution is bottlenecked on the user — same
exclusion principle as `:permission'."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws1" :repl-state :merge-conflict)
    (should-not (agent-repl--caffeinate-any-merging-p))))

(ert-deftest agent-repl-test-caffeinate-any-merging-p-merge-failed-not-active ()
  "Workspace with `:repl-state :merge-failed' → not merging (terminal)."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws1" :repl-state :merge-failed)
    (should-not (agent-repl--caffeinate-any-merging-p))))

(ert-deftest agent-repl-test-caffeinate-any-merging-p-cleared-merging-not-active ()
  "Workspace where `:merging' was set then cleared → not merging."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws1" :merging t)
    (should (agent-repl--caffeinate-any-merging-p))
    (agent-repl--ws-put "ws1" :merging nil)
    (should-not (agent-repl--caffeinate-any-merging-p))))

;;;; ---- Tests: --caffeinate-any-active-p OR-composition --------------------

(ert-deftest agent-repl-test-caffeinate-any-active-p-merging-only ()
  "Workspace `:done' but `:merging t' → active via the merging branch.
This is the workspace-merge race the module exists to cover: Claude has
landed at `:done' but the sentinel-driven cherry-pick is still in flight."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws1" :claude-state :done)
    (agent-repl--ws-put "ws1" :merging t)
    (should (agent-repl--caffeinate-any-active-p))))

(ert-deftest agent-repl-test-caffeinate-any-active-p-merge-queued-only ()
  "Workspace `:done' but `:repl-state :merge-queued' → active via merging branch."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws1" :claude-state :done)
    (agent-repl--ws-put "ws1" :repl-state :merge-queued)
    (should (agent-repl--caffeinate-any-active-p))))

(ert-deftest agent-repl-test-caffeinate-any-active-p-both-resolved ()
  "Workspace `:done' AND `:merge-completed t' (and no in-flight) → not active."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws1" :claude-state :done)
    (agent-repl--ws-put "ws1" :merge-completed t)
    (agent-repl--ws-put "ws1" :repl-state :merged)
    (should-not (agent-repl--caffeinate-any-active-p))))

;;;; ---- Tests: start / stop / running-p -------------------------------------

(ert-deftest agent-repl-test-caffeinate-start-spawns-once ()
  "Calling --caffeinate-start twice spawns only once."
  (agent-repl-test--with-fake-caffeinate
    (agent-repl--caffeinate-start)
    (agent-repl--caffeinate-start)
    (should (= 1 (length agent-repl-test--fake-spawn-log)))
    (should (agent-repl--caffeinate-running-p))))

(ert-deftest agent-repl-test-caffeinate-start-passes-program-and-args ()
  "Spawn uses `agent-repl-caffeinate-program' + `-args' verbatim."
  (agent-repl-test--with-fake-caffeinate
    (let ((agent-repl-caffeinate-program "caffeinate")
          (agent-repl-caffeinate-args '("-i" "-d")))
      (agent-repl--caffeinate-start)
      (let ((spawn (car agent-repl-test--fake-spawn-log)))
        (should (equal (car spawn) "agent-repl-caffeinate"))
        (should (equal (cdr spawn) '("caffeinate" "-i" "-d")))))))

(ert-deftest agent-repl-test-caffeinate-stop-kills-and-clears ()
  "Stop kills the process and clears the module handle."
  (agent-repl-test--with-fake-caffeinate
    (agent-repl--caffeinate-start)
    (should (agent-repl--caffeinate-running-p))
    (agent-repl--caffeinate-stop)
    (should-not (agent-repl--caffeinate-running-p))
    (should-not agent-repl--caffeinate-process)))

(ert-deftest agent-repl-test-caffeinate-stop-noop-when-not-running ()
  "Stop is idempotent: a no-op when nothing is live."
  (agent-repl-test--with-fake-caffeinate
    (agent-repl--caffeinate-stop)
    (should-not agent-repl--caffeinate-process)))

;;;; ---- Tests: --caffeinate-refresh -----------------------------------------

(ert-deftest agent-repl-test-caffeinate-refresh-starts-on-thinking ()
  "Refresh spawns caffeinate when a workspace is :thinking."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-fake-caffeinate
      (agent-repl--ws-put "ws1" :claude-state :thinking)
      (agent-repl--caffeinate-refresh)
      (should (agent-repl--caffeinate-running-p))
      (should (= 1 (length agent-repl-test--fake-spawn-log))))))

(ert-deftest agent-repl-test-caffeinate-refresh-stops-when-all-resolve ()
  "Refresh stops caffeinate once every workspace leaves an active state."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-fake-caffeinate
      (agent-repl--ws-put "ws1" :claude-state :thinking)
      (agent-repl--caffeinate-refresh)
      (should (agent-repl--caffeinate-running-p))
      (agent-repl--ws-put "ws1" :claude-state :done)
      (agent-repl--caffeinate-refresh)
      (should-not (agent-repl--caffeinate-running-p)))))

(ert-deftest agent-repl-test-caffeinate-refresh-keeps-running-with-survivor ()
  "Refresh keeps caffeinate alive while ANY workspace is still :thinking."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-fake-caffeinate
      (agent-repl--ws-put "ws1" :claude-state :thinking)
      (agent-repl--ws-put "ws2" :claude-state :thinking)
      (agent-repl--caffeinate-refresh)
      (should (agent-repl--caffeinate-running-p))
      (agent-repl--ws-put "ws1" :claude-state :done)
      (agent-repl--caffeinate-refresh)
      (should (agent-repl--caffeinate-running-p))
      (should (= 1 (length agent-repl-test--fake-spawn-log))))))

(ert-deftest agent-repl-test-caffeinate-refresh-noop-on-non-darwin ()
  "Refresh is inert on non-Darwin even if a workspace is :thinking."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-fake-caffeinate
      (let ((system-type 'gnu/linux))
        (agent-repl--ws-put "ws1" :claude-state :thinking)
        (agent-repl--caffeinate-refresh)
        (should-not (agent-repl--caffeinate-running-p))
        (should-not agent-repl-test--fake-spawn-log)))))

(ert-deftest agent-repl-test-caffeinate-refresh-noop-when-disabled ()
  "Refresh is inert when `agent-repl-caffeinate-enabled' is nil."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-fake-caffeinate
      (let ((agent-repl-caffeinate-enabled nil))
        (agent-repl--ws-put "ws1" :claude-state :thinking)
        (agent-repl--caffeinate-refresh)
        (should-not (agent-repl--caffeinate-running-p))))))

;;;; ---- Tests: advice integration with --ws-set-claude-state ----------------

(ert-deftest agent-repl-test-caffeinate-advice-on-set-claude-state-start ()
  "Setting :thinking through the typed setter triggers a caffeinate spawn."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-fake-caffeinate
      (agent-repl--ws-set-claude-state "ws1" :thinking)
      (should (agent-repl--caffeinate-running-p)))))

(ert-deftest agent-repl-test-caffeinate-advice-on-set-claude-state-stop ()
  "Setting :done through the typed setter after :thinking kills caffeinate."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-fake-caffeinate
      (agent-repl--ws-set-claude-state "ws1" :thinking)
      (should (agent-repl--caffeinate-running-p))
      (agent-repl--ws-set-claude-state "ws1" :done)
      (should-not (agent-repl--caffeinate-running-p)))))

(ert-deftest agent-repl-test-caffeinate-advice-on-ws-del-orphan-cleanup ()
  "Nuking a still-:thinking workspace stops caffeinate via the --ws-del advice."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-fake-caffeinate
      (agent-repl--ws-set-claude-state "ws1" :thinking)
      (should (agent-repl--caffeinate-running-p))
      (agent-repl--ws-del "ws1")
      (should-not (agent-repl--caffeinate-running-p)))))

;;;; ---- Tests: advice integration via --ws-put (merge keys) -----------------

(ert-deftest agent-repl-test-caffeinate-advice-on-ws-put-merging-start ()
  "Setting `:merging t' via the central setter triggers a caffeinate spawn."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-fake-caffeinate
      (agent-repl--ws-put "ws1" :merging t)
      (should (agent-repl--caffeinate-running-p)))))

(ert-deftest agent-repl-test-caffeinate-advice-on-ws-put-merging-clear ()
  "Clearing `:merging' after start stops caffeinate (no other active state)."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-fake-caffeinate
      (agent-repl--ws-put "ws1" :merging t)
      (should (agent-repl--caffeinate-running-p))
      (agent-repl--ws-put "ws1" :merging nil)
      (should-not (agent-repl--caffeinate-running-p)))))

(ert-deftest agent-repl-test-caffeinate-advice-on-ws-put-merge-queued ()
  "Setting `:repl-state :merge-queued' triggers a caffeinate spawn."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-fake-caffeinate
      (agent-repl--ws-put "ws1" :repl-state :merge-queued)
      (should (agent-repl--caffeinate-running-p)))))

(ert-deftest agent-repl-test-caffeinate-thinking-to-done-while-merging-keeps-alive ()
  "`:thinking → :done' transition while `:merging t' keeps caffeinate alive.
This is the canonical workspace-merge race: Claude lands on `:done' but
the sentinel-driven cherry-pick is still in flight, so the editor must
stay awake to finish the merge."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-fake-caffeinate
      (agent-repl--ws-set-claude-state "ws1" :thinking)
      (agent-repl--ws-put "ws1" :merging t)
      (should (agent-repl--caffeinate-running-p))
      (agent-repl--ws-set-claude-state "ws1" :done)
      (should (agent-repl--caffeinate-running-p))
      ;; Only once the merge actually completes do we release caffeinate.
      (agent-repl--ws-put "ws1" :merging nil)
      (agent-repl--ws-put "ws1" :merge-completed t)
      (agent-repl--ws-put "ws1" :repl-state :merged)
      (should-not (agent-repl--caffeinate-running-p)))))

(ert-deftest agent-repl-test-caffeinate-merge-completed-releases ()
  "A merge that lands on `:merge-completed t' (terminal) releases caffeinate."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-fake-caffeinate
      (agent-repl--ws-put "ws1" :merging t)
      (should (agent-repl--caffeinate-running-p))
      (agent-repl--ws-put "ws1" :merging nil)
      (agent-repl--ws-put "ws1" :merge-completed t)
      (should-not (agent-repl--caffeinate-running-p)))))

(ert-deftest agent-repl-test-caffeinate-merge-conflict-releases ()
  "`:repl-state :merge-conflict' releases caffeinate — user-bottlenecked."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-fake-caffeinate
      (agent-repl--ws-put "ws1" :merging t)
      (should (agent-repl--caffeinate-running-p))
      (agent-repl--ws-put "ws1" :merging nil)
      (agent-repl--ws-put "ws1" :repl-state :merge-conflict)
      (should-not (agent-repl--caffeinate-running-p)))))

(ert-deftest agent-repl-test-caffeinate-merge-failed-releases ()
  "`:repl-state :merge-failed' (terminal) releases caffeinate."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-fake-caffeinate
      (agent-repl--ws-put "ws1" :merging t)
      (should (agent-repl--caffeinate-running-p))
      (agent-repl--ws-put "ws1" :merging nil)
      (agent-repl--ws-put "ws1" :repl-state :merge-failed)
      (should-not (agent-repl--caffeinate-running-p)))))

(ert-deftest agent-repl-test-caffeinate-advice-ignores-unwatched-keys ()
  "`--ws-put' on a non-watched key (e.g. `:project-dir') does not spawn caffeinate.
Guards the key filter — without it, every plist mutation would trigger a
reconcile and produce noisy spawn churn under hot paths."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-fake-caffeinate
      (agent-repl--ws-put "ws1" :project-dir "/tmp/somewhere")
      (should-not (agent-repl--caffeinate-running-p))
      (should-not agent-repl-test--fake-spawn-log))))

(ert-deftest agent-repl-test-caffeinate-advice-on-ws-del-orphan-cleanup-merging ()
  "Nuking a mid-merge workspace stops caffeinate via the --ws-del advice."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-fake-caffeinate
      (agent-repl--ws-put "ws1" :merging t)
      (should (agent-repl--caffeinate-running-p))
      (agent-repl--ws-del "ws1")
      (should-not (agent-repl--caffeinate-running-p)))))

(provide 'test-caffeinate)

;;; test-caffeinate.el ends here
