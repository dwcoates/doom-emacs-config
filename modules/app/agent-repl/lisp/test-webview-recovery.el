;;; test-webview-recovery.el --- ERT tests for webview-recovery.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for the host-driven webview reattach sweep.  The webview script
;; channel (`agent-repl--frontend-webview-execute-script') is the boundary
;; under mock throughout: batch Emacs has no xwidgets, and what the sweep
;; owes its caller is exactly WHICH buffers it hands WHICH script to.
;;
;; Run with:
;;   emacs -batch -Q -l ert -l test-webview-recovery.el -f ert-run-tests-batch-and-exit

;;; Code:

(load (expand-file-name "test-helpers.el" (file-name-directory
                                            (or load-file-name buffer-file-name)))
      nil t)

(require 'cl-lib)

;;;; ---- Helpers -----------------------------------------------------------

(defmacro agent-repl-test--with-recovery-sweep (calls &rest body)
  "Run BODY with a fresh debounce state, recording script calls in CALLS.
CALLS is bound to a variable holding a list of (BUFFER . SCRIPT) in call
order.  The debounce stamp is reset so a test never inherits another
test's sweep time."
  (declare (indent 1))
  `(let ((,calls nil)
         (agent-repl--webview-recovery-last-sweep nil))
     (cl-letf (((symbol-function 'agent-repl--frontend-webview-execute-script)
                (lambda (buf script) (setq ,calls (append ,calls (list (cons buf script)))))))
       ,@body)))

(defmacro agent-repl-test--with-recovery-ws (bindings &rest body)
  "Register workspaces from BINDINGS for BODY, killing their buffers after.
BINDINGS is a list of (VAR WS) — VAR is bound to a fresh buffer recorded
as WS's `:frontend-buffer'."
  (declare (indent 1))
  `(let ,(mapcar (lambda (b) `(,(car b) (generate-new-buffer ,(cadr b)))) bindings)
     (unwind-protect
         (progn
           ,@(mapcar (lambda (b)
                       `(progn
                          (puthash ,(cadr b) (list :project-dir "/w") agent-repl--workspaces)
                          (agent-repl--ws-put ,(cadr b) :frontend-buffer ,(car b))))
                     bindings)
           ,@body)
       ,@(mapcar (lambda (b) `(when (buffer-live-p ,(car b)) (kill-buffer ,(car b)))) bindings)
       ,@(mapcar (lambda (b) `(remhash ,(cadr b) agent-repl--workspaces)) bindings))))

;;;; ---- The script ---------------------------------------------------------

(ert-deftest agent-repl-test-webview-recovery-script-calls-the-guarded-hook ()
  "The script calls the webapp's recovery hook, guarded on it existing."
  ;; Arrange + Act
  (let ((script (agent-repl--webview-recovery-script "host_link_up")))
    ;; Assert
    (should (equal script
                   (concat "window.agentReplRecoverNow && "
                           "window.agentReplRecoverNow(\"host_link_up\");")))))

(ert-deftest agent-repl-test-webview-recovery-hook-name-matches-webapp ()
  "The hook name lisp calls is the one the webapp plants on `window'.
webapp/src/host.ts exports `RECOVER_HOOK'; a rename on either side
silently turns the host-driven reattach into a no-op."
  ;; Arrange
  (let* ((host-ts (expand-file-name "webapp/src/host.ts" agent-repl--frontend-root))
         (source (progn
                   (should (file-exists-p host-ts))
                   (with-temp-buffer
                     (insert-file-contents host-ts)
                     (buffer-string)))))
    ;; Act + Assert
    (should (string-match-p
             (regexp-quote (format "export const RECOVER_HOOK = \"%s\";"
                                   agent-repl-frontend-recover-hook))
             source))))

;;;; ---- The sweep ----------------------------------------------------------

(ert-deftest agent-repl-test-webview-recovery-sweep-drives-every-live-workspace ()
  "One script per live workspace webview, in one sweep."
  ;; Arrange
  (agent-repl-test--with-recovery-sweep calls
    (agent-repl-test--with-recovery-ws ((b1 "wsr1") (b2 "wsr2"))
      ;; Act
      (agent-repl--webview-recovery-sweep "host_link_up")
      ;; Assert
      (should (equal (sort (mapcar #'car calls) (lambda (a b) (string< (buffer-name a)
                                                                       (buffer-name b))))
                     (list b1 b2))))))

(ert-deftest agent-repl-test-webview-recovery-sweep-calls-each-webview-once ()
  "A single sweep hands each live webview exactly one script."
  ;; Arrange
  (agent-repl-test--with-recovery-sweep calls
    (agent-repl-test--with-recovery-ws ((b1 "wsr1"))
      ;; Act
      (agent-repl--webview-recovery-sweep "host_link_up")
      ;; Assert
      (should (equal calls
                     (list (cons b1 (agent-repl--webview-recovery-script "host_link_up"))))))))

(ert-deftest agent-repl-test-webview-recovery-sweep-skips-a-workspace-without-a-webview ()
  "A workspace whose panel was never opened is not asked to recover."
  ;; Arrange
  (agent-repl-test--with-recovery-sweep calls
    (unwind-protect
        (progn
          (puthash "wsr-nowv" (list :project-dir "/w") agent-repl--workspaces)
          ;; Act
          (should (equal 0 (agent-repl--webview-recovery-sweep "host_link_up")))
          ;; Assert
          (should (null calls)))
      (remhash "wsr-nowv" agent-repl--workspaces))))

(ert-deftest agent-repl-test-webview-recovery-sweep-skips-a-dead-webview ()
  "A recorded but killed webview buffer is not asked to recover."
  ;; Arrange
  (agent-repl-test--with-recovery-sweep calls
    (agent-repl-test--with-recovery-ws ((b1 "wsr1"))
      (kill-buffer b1)
      ;; Act
      (should (equal 0 (agent-repl--webview-recovery-sweep "host_link_up")))
      ;; Assert
      (should (null calls)))))

(ert-deftest agent-repl-test-webview-recovery-sweep-debounces-a-link-flap ()
  "A second sweep inside the debounce window does not run."
  ;; Arrange
  (agent-repl-test--with-recovery-sweep calls
    (agent-repl-test--with-recovery-ws ((b1 "wsr1"))
      (agent-repl--webview-recovery-sweep "host_link_up")
      ;; Act
      (let ((again (agent-repl--webview-recovery-sweep "host_link_up")))
        ;; Assert
        (should (null again))
        (should (equal 1 (length calls)))))))

(ert-deftest agent-repl-test-webview-recovery-sweep-runs-again-past-the-debounce-window ()
  "A sweep past the debounce window runs, so a later outage still recovers."
  ;; Arrange
  (agent-repl-test--with-recovery-sweep calls
    (agent-repl-test--with-recovery-ws ((b1 "wsr1"))
      (agent-repl--webview-recovery-sweep "host_link_up")
      (setq agent-repl--webview-recovery-last-sweep
            (- agent-repl--webview-recovery-last-sweep
               agent-repl-webview-recovery-debounce-seconds
               1.0))
      ;; Act
      (should (equal 1 (agent-repl--webview-recovery-sweep "host_link_up")))
      ;; Assert
      (should (equal 2 (length calls))))))

(ert-deftest agent-repl-test-webview-recovery-sweep-continues-past-a-failed-webview ()
  "One webview whose script fails does not strand the others."
  ;; Arrange
  (let ((agent-repl--webview-recovery-last-sweep nil)
        (driven nil))
    (agent-repl-test--with-recovery-ws ((b1 "wsr-bad") (b2 "wsr-ok"))
      (cl-letf (((symbol-function 'agent-repl--frontend-webview-execute-script)
                 (lambda (buf _script)
                   (if (eq buf b1)
                       (error "xwidget is gone")
                     (push buf driven)))))
        ;; Act
        (let ((count (agent-repl--webview-recovery-sweep "host_link_up")))
          ;; Assert
          (should (equal 1 count))
          (should (equal driven (list b2))))))))

(ert-deftest agent-repl-test-webview-recovery-subscribes-to-the-link-up-edge ()
  "The sweep is armed on the snapshot-applied edge, which is also startup's."
  ;; Arrange + Act + Assert
  (should (memq #'agent-repl--webview-recovery-on-link-up
                agent-repl-uds-snapshot-applied-functions)))

(ert-deftest agent-repl-test-webview-recovery-link-up-names-its-reason ()
  "The link-up subscriber names the host reason the page logs the repair under."
  ;; Arrange
  (agent-repl-test--with-recovery-sweep calls
    (agent-repl-test--with-recovery-ws ((b1 "wsr1"))
      ;; Act
      (agent-repl--webview-recovery-on-link-up)
      ;; Assert
      (should (equal (cdr (car calls))
                     (agent-repl--webview-recovery-script "host_link_up"))))))

(provide 'test-webview-recovery)
;;; test-webview-recovery.el ends here
