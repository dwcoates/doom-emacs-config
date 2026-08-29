;;; test-webview-recovery.el --- ERT tests for webview-recovery.el -*- lexical-binding: t; -*-

;;; Commentary:

;; What survives here is the PACED PRE-CREATION: eligibility, one mount per
;; tick, re-checked at the tick rather than trusted from queue time, and the
;; link-up edge that starts it.  The stale-webview sweep and every script it
;; drove are gone — a rebuilt webapp reaches a live page through the daemon's
;; own `reload_webapp' push — so their tests went with them.
;;
;; Run with:
;;   AGENT_REPL_FORBID_VENDOR_CALLS=1 emacs -batch -Q -l ert \
;;     -l lisp/test-webview-recovery.el -f ert-run-tests-batch-and-exit

;;; Code:

(load (expand-file-name "test-helpers.el" (file-name-directory
                                            (or load-file-name buffer-file-name)))
      nil t)

;;;; ---- Harness ----

(defvar agent-repl-test-wr--mounted nil
  "Workspaces the drain asked frontend.el to pre-create, newest last.")

(defmacro agent-repl-test-wr--with-queue (&rest body)
  "Run BODY with a fresh pre-creation queue and the mount recorded."
  (declare (indent 0))
  `(agent-repl-test--with-clean-state
     (let ((agent-repl--webview-precreate-queue nil)
           (agent-repl--webview-precreate-timer nil)
           (agent-repl-test-wr--mounted nil))
       (cl-letf (((symbol-function 'agent-repl--frontend-precreate-webview)
                  (lambda (ws) (push ws agent-repl-test-wr--mounted) :created))
                 ((symbol-function 'run-at-time)
                  (lambda (_delay _repeat _fn &rest _args) 'fake-timer)))
         ,@body))))

(defmacro agent-repl-test-wr--eligible (names &rest body)
  "Run BODY with exactly NAMES answering the pre-creation eligibility test."
  (declare (indent 1))
  `(cl-letf (((symbol-function 'agent-repl--frontend-precreate-refusal)
              (lambda (ws) (if (member ws ,names) nil :not-live)))
             ((symbol-function 'agent-repl--live-ws-names)
              (lambda () ,names)))
     ,@body))

;;;; ---- Eligibility ----

(ert-deftest agent-repl-test-wr-needed-p-follows-the-mounts-own-refusal ()
  "The queue asks the SAME question the mount does, so they cannot disagree."
  ;; Arrange
  (agent-repl-test-wr--with-queue
    (agent-repl-test-wr--eligible '("alpha")
      ;; Act / Assert
      (should (agent-repl--webview-precreate-needed-p "alpha")))))

(ert-deftest agent-repl-test-wr-a-refused-workspace-is-not-needed ()
  "A workspace the mount would refuse is never queued."
  ;; Arrange
  (agent-repl-test-wr--with-queue
    (agent-repl-test-wr--eligible '("alpha")
      ;; Act / Assert
      (should-not (agent-repl--webview-precreate-needed-p "beta")))))

(ert-deftest agent-repl-test-wr-missing-lists-only-eligible-workspaces ()
  "The owed set is the live workspaces the mount would accept."
  ;; Arrange
  (agent-repl-test-wr--with-queue
    (cl-letf (((symbol-function 'agent-repl--live-ws-names)
               (lambda () '("alpha" "beta")))
              ((symbol-function 'agent-repl--frontend-precreate-refusal)
               (lambda (ws) (if (equal ws "alpha") nil :already-mounted))))
      ;; Act / Assert
      (should (equal (agent-repl--webview-precreate-missing) '("alpha"))))))

;;;; ---- The queue ----

(ert-deftest agent-repl-test-wr-scheduling-queues-each-workspace ()
  "Every workspace handed in is queued."
  ;; Arrange
  (agent-repl-test-wr--with-queue
    ;; Act
    (agent-repl--webview-precreate-schedule '("alpha" "beta"))
    ;; Assert
    (should (equal agent-repl--webview-precreate-queue '("alpha" "beta")))))

(ert-deftest agent-repl-test-wr-scheduling-reports-how-many-it-queued ()
  "The count is what was ADDED, which is what a caller logs."
  ;; Arrange
  (agent-repl-test-wr--with-queue
    ;; Act / Assert
    (should (equal (agent-repl--webview-precreate-schedule '("alpha" "beta")) 2))))

(ert-deftest agent-repl-test-wr-scheduling-does-not-queue-a-workspace-twice ()
  "A workspace already owed a mount is not owed two."
  ;; Arrange
  (agent-repl-test-wr--with-queue
    (agent-repl--webview-precreate-schedule '("alpha"))
    ;; Act
    (agent-repl--webview-precreate-schedule '("alpha"))
    ;; Assert
    (should (equal agent-repl--webview-precreate-queue '("alpha")))))

(ert-deftest agent-repl-test-wr-scheduling-appends-rather-than-replaces ()
  "A second schedule landing mid-drain cannot drop what the first still owes."
  ;; Arrange
  (agent-repl-test-wr--with-queue
    (agent-repl--webview-precreate-schedule '("alpha"))
    ;; Act
    (agent-repl--webview-precreate-schedule '("beta"))
    ;; Assert
    (should (equal agent-repl--webview-precreate-queue '("alpha" "beta")))))

;;;; ---- The drain ----

(ert-deftest agent-repl-test-wr-the-drain-mounts-one-workspace-per-tick ()
  "One mount per tick is the whole point of the stagger."
  ;; Arrange
  (agent-repl-test-wr--with-queue
    (agent-repl-test-wr--eligible '("alpha" "beta")
      (agent-repl--webview-precreate-schedule '("alpha" "beta"))
      ;; Act
      (agent-repl--webview-precreate-drain)
      ;; Assert
      (should (equal agent-repl-test-wr--mounted '("alpha"))))))

(ert-deftest agent-repl-test-wr-the-drain-rearms-while-the-queue-is-nonempty ()
  "The chain keeps going until the queue is empty."
  ;; Arrange
  (agent-repl-test-wr--with-queue
    (agent-repl-test-wr--eligible '("alpha" "beta")
      (agent-repl--webview-precreate-schedule '("alpha" "beta"))
      ;; Act
      (agent-repl--webview-precreate-drain)
      ;; Assert
      (should (eq agent-repl--webview-precreate-timer 'fake-timer)))))

(ert-deftest agent-repl-test-wr-the-drain-stops-on-an-empty-queue ()
  "The last tick arms nothing."
  ;; Arrange
  (agent-repl-test-wr--with-queue
    (agent-repl-test-wr--eligible '("alpha")
      (agent-repl--webview-precreate-schedule '("alpha"))
      ;; Act
      (agent-repl--webview-precreate-drain)
      ;; Assert
      (should (null agent-repl--webview-precreate-timer)))))

(ert-deftest agent-repl-test-wr-the-drain-rechecks-eligibility-at-the-tick ()
  "A workspace closed while the queue drained gets no page."
  ;; Arrange
  (agent-repl-test-wr--with-queue
    (agent-repl-test-wr--eligible '("alpha")
      (agent-repl--webview-precreate-schedule '("alpha")))
    ;; Act — by the time the tick comes, nothing is eligible any more.
    (agent-repl-test-wr--eligible '()
      (agent-repl--webview-precreate-drain))
    ;; Assert
    (should (null agent-repl-test-wr--mounted))))

(ert-deftest agent-repl-test-wr-a-failing-mount-does-not-strand-the-queue ()
  "One workspace's failure must not take the rest of the queue with it."
  ;; Arrange
  (agent-repl-test-wr--with-queue
    (agent-repl-test-wr--eligible '("alpha" "beta")
      (agent-repl--webview-precreate-schedule '("alpha" "beta"))
      (cl-letf (((symbol-function 'agent-repl--frontend-precreate-webview)
                 (lambda (_ws) (error "mount blew up"))))
        ;; Act
        (agent-repl--webview-precreate-drain))
      ;; Assert
      (should (equal agent-repl--webview-precreate-queue '("beta"))))))

;;;; ---- The link-up edge ----

(ert-deftest agent-repl-test-wr-link-up-queues-every-owed-workspace ()
  "The link coming up is the first moment a page can be built at all."
  ;; Arrange
  (agent-repl-test-wr--with-queue
    (agent-repl-test-wr--eligible '("alpha" "beta")
      ;; Act
      (agent-repl--webview-precreate-on-link-up 'conn)
      ;; Assert
      (should (equal agent-repl--webview-precreate-queue '("alpha" "beta"))))))

(ert-deftest agent-repl-test-wr-link-up-queues-nothing-when-nothing-is-owed ()
  "Every page already mounted means the link-up owes no work."
  ;; Arrange
  (agent-repl-test-wr--with-queue
    (agent-repl-test-wr--eligible '()
      ;; Act
      (agent-repl--webview-precreate-on-link-up 'conn)
      ;; Assert
      (should (null agent-repl--webview-precreate-queue)))))

(ert-deftest agent-repl-test-wr-link-up-is-registered-on-the-link-hook ()
  "The pre-creation rides the daemon link's own up edge."
  ;; Act / Assert
  (should (memq #'agent-repl--webview-precreate-on-link-up
                agent-repl-link-up-functions)))

;;; test-webview-recovery.el ends here
