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

(defconst agent-repl-test--recovery-build "bid-deployed"
  "The deployed webapp build identity every sweep test compares against.")

(defvar agent-repl-test--recovery-uris nil
  "Alist of (BUFFER . URI) the mocked webview URI probe answers from.
A buffer with no entry is addressed at the deployed build, which is the
uninteresting case every test that is not about staleness wants.")

(defvar agent-repl-test--recovery-navigated nil
  "List of (BUFFER . URI) the mocked navigation recorded, in call order.")

(defun agent-repl-test--recovery-uri (buf)
  "Return the URI mocked for BUF, defaulting to the deployed build's address."
  (or (cdr (assq buf agent-repl-test--recovery-uris))
      (format "http://x/?workspace=%%2Fw&build=%s" agent-repl-test--recovery-build)))

(defmacro agent-repl-test--with-recovery-sweep (calls &rest body)
  "Run BODY with a fresh debounce state, recording script calls in CALLS.
CALLS is bound to a variable holding a list of (BUFFER . SCRIPT) in call
order.  The debounce stamp is reset so a test never inherits another
test's sweep time.  Every webview boundary is mocked: batch Emacs has no
xwidgets, so the widget a sweep acts on is the buffer itself, its URI
comes from `agent-repl-test--recovery-uris', and a re-navigation lands in
`agent-repl-test--recovery-navigated' instead of a WKWebView."
  (declare (indent 1))
  `(let ((,calls nil)
         (agent-repl--webview-recovery-last-sweep nil)
         (agent-repl-test--recovery-uris nil)
         (agent-repl-test--recovery-navigated nil))
     (cl-letf (((symbol-function 'agent-repl--frontend-webview-execute-script)
                (lambda (buf script) (setq ,calls (append ,calls (list (cons buf script))))))
               ((symbol-function 'agent-repl--frontend-build-id)
                (lambda () agent-repl-test--recovery-build))
               ((symbol-function 'agent-repl--frontend-webview-live-widget)
                (lambda (buf) buf))
               ((symbol-function 'agent-repl--frontend-webview-uri)
                #'agent-repl-test--recovery-uri)
               ((symbol-function 'agent-repl--frontend-webview-navigate-widget)
                (lambda (xw uri)
                  (setq agent-repl-test--recovery-navigated
                        (append agent-repl-test--recovery-navigated (list (cons xw uri))))
                  uri)))
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
  (agent-repl-test--with-recovery-sweep _calls
    (let ((driven nil))
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
            (should (equal driven (list b2)))))))))

;;;; ---- Bundle staleness ---------------------------------------------------

(ert-deftest agent-repl-test-webview-recovery-reloads-a-stale-bundle ()
  "A page addressed at a superseded build is re-navigated, not driven."
  ;; Arrange
  (agent-repl-test--with-recovery-sweep calls
    (agent-repl-test--with-recovery-ws ((b1 "wsr-stale"))
      (setq agent-repl-test--recovery-uris
            (list (cons b1 "http://x/?workspace=%2Fw&build=bid-old")))
      ;; Act
      (agent-repl--webview-recovery-sweep "deploy_refresh")
      ;; Assert
      (should (null calls))
      (should (equal (mapcar #'car agent-repl-test--recovery-navigated) (list b1))))))

(ert-deftest agent-repl-test-webview-recovery-reload-addresses-the-deployed-build ()
  "The reload goes to the DEPLOYED build's address, not the page's own."
  ;; Arrange
  (agent-repl-test--with-recovery-sweep _calls
    (agent-repl-test--with-recovery-ws ((b1 "wsr-stale"))
      (setq agent-repl-test--recovery-uris
            (list (cons b1 "http://x/?workspace=%2Fw&build=bid-old")))
      ;; Act
      (agent-repl--webview-recovery-sweep "deploy_refresh")
      ;; Assert
      (should (equal (cdr (car agent-repl-test--recovery-navigated))
                     (format "http://x/?workspace=%%2Fw&build=%s"
                             agent-repl-test--recovery-build))))))

(ert-deftest agent-repl-test-webview-recovery-drives-a-matching-bundle ()
  "A page already on the deployed build is driven through the hook, not reloaded."
  ;; Arrange
  (agent-repl-test--with-recovery-sweep calls
    (agent-repl-test--with-recovery-ws ((b1 "wsr-fresh"))
      ;; Act
      (agent-repl--webview-recovery-sweep "deploy_refresh")
      ;; Assert
      (should (equal (mapcar #'car calls) (list b1)))
      (should (null agent-repl-test--recovery-navigated)))))

(ert-deftest agent-repl-test-webview-recovery-reloads-a-page-with-no-build-identity ()
  "A bundle predating the stamped URL — and so the hook — is reloaded."
  ;; Arrange
  (agent-repl-test--with-recovery-sweep calls
    (agent-repl-test--with-recovery-ws ((b1 "wsr-prehook"))
      (setq agent-repl-test--recovery-uris
            (list (cons b1 "http://x/?workspace=%2Fw")))
      ;; Act
      (agent-repl--webview-recovery-sweep "deploy_refresh")
      ;; Assert
      (should (null calls))
      (should (equal (cdr (car agent-repl-test--recovery-navigated))
                     (format "http://x/?workspace=%%2Fw&build=%s"
                             agent-repl-test--recovery-build))))))

(ert-deftest agent-repl-test-webview-recovery-counts-driven-and-reloaded ()
  "The sweep's return value counts every webview it acted on, either way."
  ;; Arrange
  (agent-repl-test--with-recovery-sweep _calls
    (agent-repl-test--with-recovery-ws ((b1 "wsr-fresh") (b2 "wsr-stale"))
      (setq agent-repl-test--recovery-uris
            (list (cons b2 "http://x/?workspace=%2Fw&build=bid-old")))
      ;; Act / Assert
      (should (equal 2 (agent-repl--webview-recovery-sweep "deploy_refresh"))))))

(ert-deftest agent-repl-test-webview-recovery-counts-a-dead-webview-as-absent ()
  "A buffer whose WKWebView is gone is neither driven nor reloaded."
  ;; Arrange
  (agent-repl-test--with-recovery-sweep calls
    (agent-repl-test--with-recovery-ws ((b1 "wsr-dead"))
      (cl-letf (((symbol-function 'agent-repl--frontend-webview-live-widget)
                 (lambda (_buf) nil)))
        ;; Act
        (should (equal 0 (agent-repl--webview-recovery-sweep "deploy_refresh")))
        ;; Assert
        (should (null calls))
        (should (null agent-repl-test--recovery-navigated))))))

(ert-deftest agent-repl-test-webview-recovery-fresh-uri-appends-a-missing-param ()
  "A URI with no build param gains one rather than losing its query."
  ;; Arrange + Act + Assert
  (should (equal (agent-repl--webview-recovery-fresh-uri "http://x/?workspace=%2Fw" "b2")
                 "http://x/?workspace=%2Fw&build=b2")))

(ert-deftest agent-repl-test-webview-recovery-uri-build-reads-the-param ()
  "The page's build identity is read out of the URL it was addressed at."
  ;; Arrange + Act + Assert
  (should (equal (agent-repl--webview-recovery-uri-build
                  "http://x/?workspace=%2Fw&build=abc123")
                 "abc123")))

(ert-deftest agent-repl-test-webview-recovery-uri-build-is-nil-without-the-param ()
  "A URL carrying no build identity reports none, rather than guessing one."
  ;; Arrange + Act + Assert
  (should (null (agent-repl--webview-recovery-uri-build "http://x/?workspace=%2Fw"))))

;;;; ---- Which webviews a sweep reaches -------------------------------------

(ert-deftest agent-repl-test-webview-recovery-includes-a-hibernated-workspace-buffer ()
  "A backgrounded workspace's webview buffer is swept even when unfocused.
`agent-repl--frontend-live-webview-buffers' finds the buffers the buffer
name claims; the workspace record contributes the rest."
  ;; Arrange
  (agent-repl-test--with-recovery-sweep _calls
    (agent-repl-test--with-recovery-ws ((b1 "wsr-hibernated"))
      (cl-letf (((symbol-function 'agent-repl--frontend-live-webview-buffers)
                 (lambda () nil)))
        ;; Act + Assert
        (should (memq b1 (agent-repl--webview-recovery-buffers)))))))

(ert-deftest agent-repl-test-webview-recovery-buffers-are-deduplicated ()
  "A buffer named by both sources is swept once, not twice."
  ;; Arrange
  (agent-repl-test--with-recovery-sweep _calls
    (agent-repl-test--with-recovery-ws ((b1 "wsr-both"))
      (cl-letf (((symbol-function 'agent-repl--frontend-live-webview-buffers)
                 (lambda () (list b1))))
        ;; Act + Assert
        (should (equal (agent-repl--webview-recovery-buffers) (list b1)))))))

;;;; ---- The deploy-time entry point ----------------------------------------

(ert-deftest agent-repl-test-webview-recovery-deploy-refresh-respects-the-debounce ()
  "A deploy refresh inside the debounce window of a link-up sweep is skipped."
  ;; Arrange
  (agent-repl-test--with-recovery-sweep calls
    (agent-repl-test--with-recovery-ws ((b1 "wsr1"))
      (agent-repl--webview-recovery-on-link-up)
      ;; Act
      (let ((again (agent-repl-refresh-webviews)))
        ;; Assert
        (should (null again))
        (should (equal 1 (length calls)))))))

(ert-deftest agent-repl-test-webview-recovery-navigate-is-a-registered-boundary ()
  "The re-navigation wrapper is registered as an external boundary wrapper."
  ;; Arrange + Act + Assert
  (should (memq 'agent-repl--frontend-webview-navigate-widget
                agent-repl--external-boundary-functions)))

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

;;;; ---- Pre-creation -------------------------------------------------------

(defmacro agent-repl-test--with-precreate (precreated &rest body)
  "Run BODY with a clean pre-creation queue, recording mounts in PRECREATED.
PRECREATED is bound to a list of workspace names, in drain order.  The
mount itself is the boundary under mock: batch Emacs has no xwidgets."
  (declare (indent 1))
  `(let ((,precreated nil)
         (agent-repl--webview-precreate-queue nil)
         (agent-repl--webview-precreate-timer nil)
         (agent-repl-webview-precreate-stagger-seconds 0.01))
     (cl-letf (((symbol-function 'agent-repl--frontend-precreate-webview)
                (lambda (ws) (setq ,precreated (append ,precreated (list ws))) :pending)))
       (unwind-protect (progn ,@body)
         (when (timerp agent-repl--webview-precreate-timer)
           (cancel-timer agent-repl--webview-precreate-timer))))))

(defmacro agent-repl-test--with-pageless-ws (names &rest body)
  "Register each name in NAMES as a live gui workspace with NO webview buffer."
  (declare (indent 1))
  `(unwind-protect
       (progn
         (dolist (name ,names)
           (puthash name (list :project-dir "/w" :frontend 'gui) agent-repl--workspaces))
         ,@body)
     (dolist (name ,names) (remhash name agent-repl--workspaces))))

(ert-deftest agent-repl-test-webview-recovery-sweep-queues-absent-pages ()
  "A live workspace with no webview buffer is queued for pre-creation."
  ;; Arrange
  (agent-repl-test--with-precreate _created
    (agent-repl-test--with-recovery-sweep _calls
      (agent-repl-test--with-pageless-ws '("wsp1")
        ;; Act
        (agent-repl--webview-recovery-sweep "host_link_up")
        ;; Assert
        (should (equal '("wsp1") agent-repl--webview-precreate-queue))))))

(ert-deftest agent-repl-test-webview-recovery-sweep-leaves-mounted-workspaces-alone ()
  "A workspace that already holds a live webview buffer is not queued."
  ;; Arrange
  (agent-repl-test--with-precreate _created
    (agent-repl-test--with-recovery-sweep _calls
      (agent-repl-test--with-recovery-ws ((b1 "wsr1"))
        ;; Act
        (agent-repl--webview-recovery-sweep "host_link_up")
        ;; Assert
        (should-not (member "wsr1" agent-repl--webview-precreate-queue))))))

(ert-deftest agent-repl-test-webview-precreate-drain-mounts-one-per-tick ()
  "The drain mounts exactly one queued workspace and re-arms for the rest."
  ;; Arrange
  (agent-repl-test--with-precreate created
    (agent-repl-test--with-pageless-ws '("wsp1" "wsp2")
      (agent-repl--webview-precreate-schedule '("wsp1" "wsp2"))
      ;; Act
      (agent-repl--webview-precreate-drain)
      ;; Assert
      (should (equal '("wsp1") created))
      (should (timerp agent-repl--webview-precreate-timer)))))

(ert-deftest agent-repl-test-webview-precreate-drain-stops-when-the-queue-empties ()
  "The last drain leaves no timer behind."
  ;; Arrange
  (agent-repl-test--with-precreate _created
    (agent-repl-test--with-pageless-ws '("wsp1")
      (agent-repl--webview-precreate-schedule '("wsp1"))
      ;; Act
      (agent-repl--webview-precreate-drain)
      ;; Assert
      (should (null agent-repl--webview-precreate-timer)))))

(ert-deftest agent-repl-test-webview-precreate-skips-a-fenced-workspace ()
  "A terminally fenced workspace gets no page."
  ;; Arrange
  (agent-repl-test--with-precreate created
    (agent-repl-test--with-pageless-ws '("wsp1")
      (agent-repl--ws-put "wsp1" :open-fenced t)
      (agent-repl--webview-precreate-schedule '("wsp1"))
      ;; Act
      (agent-repl--webview-precreate-drain)
      ;; Assert
      (should (null created)))))

(ert-deftest agent-repl-test-webview-precreate-skips-a-workspace-closed-mid-drain ()
  "A workspace unregistered after queueing is re-checked and skipped."
  ;; Arrange
  (agent-repl-test--with-precreate created
    (agent-repl-test--with-pageless-ws '("wsp1")
      (agent-repl--webview-precreate-schedule '("wsp1"))
      (remhash "wsp1" agent-repl--workspaces)
      ;; Act
      (agent-repl--webview-precreate-drain)
      ;; Assert
      (should (null created)))))

(ert-deftest agent-repl-test-webview-precreate-schedule-does-not-queue-twice ()
  "A workspace already queued is not queued a second time."
  ;; Arrange
  (agent-repl-test--with-precreate _created
    (agent-repl-test--with-pageless-ws '("wsp1")
      (agent-repl--webview-precreate-schedule '("wsp1"))
      ;; Act
      (let ((added (agent-repl--webview-precreate-schedule '("wsp1"))))
        ;; Assert
        (should (equal 0 added))
        (should (equal '("wsp1") agent-repl--webview-precreate-queue))))))

(ert-deftest agent-repl-test-webview-precreate-drain-survives-a-failing-mount ()
  "A mount that signals is warned about and the queue keeps draining."
  ;; Arrange
  (let ((agent-repl--webview-precreate-queue nil)
        (agent-repl--webview-precreate-timer nil)
        (agent-repl-webview-precreate-stagger-seconds 0.01))
    (cl-letf (((symbol-function 'agent-repl--frontend-precreate-webview)
               (lambda (_ws) (error "boom"))))
      (agent-repl-test--with-pageless-ws '("wsp1" "wsp2")
        (agent-repl--webview-precreate-schedule '("wsp1" "wsp2"))
        ;; Act
        (agent-repl--webview-precreate-drain)
        ;; Assert
        (should (equal '("wsp2") agent-repl--webview-precreate-queue))
        (when (timerp agent-repl--webview-precreate-timer)
          (cancel-timer agent-repl--webview-precreate-timer))))))

(provide 'test-webview-recovery)
;;; test-webview-recovery.el ends here
