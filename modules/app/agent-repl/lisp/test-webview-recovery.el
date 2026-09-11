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
           (agent-repl--webview-precreate-parked nil)
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


;;;; ---- Holding for desktop focus ----
;;
;; Creating a WKWebView instantiates a native view and macOS activates the
;; owning process when it does, which `open -g' cannot suppress.  A link-up
;; pre-creates every eligible page seconds into every cold start, so an
;; Emacs launched deliberately in the background took the desktop from
;; whatever the user was looking at -- three background launches, three
;; focus moves.  These pin the hold and its release.
;;
;; `noninteractive' is bound to nil where the hold itself is under test: in
;; a batch process there is no application to activate and nothing holds,
;; which is what lets every drain test above run unchanged.

(defmacro agent-repl-test-wr--unfocused (&rest body)
  "Run BODY as an interactive Emacs that does NOT hold desktop focus."
  (declare (indent 0))
  `(let ((noninteractive nil))
     (cl-letf (((symbol-function 'agent-repl--emacs-focused-p) (lambda (&rest _) nil)))
       ,@body)))

(defmacro agent-repl-test-wr--focused (&rest body)
  "Run BODY as an interactive Emacs that DOES hold desktop focus."
  (declare (indent 0))
  `(let ((noninteractive nil))
     (cl-letf (((symbol-function 'agent-repl--emacs-focused-p) (lambda (&rest _) t)))
       ,@body)))

(ert-deftest agent-repl-test-wr-an-unfocused-drain-mounts-nothing ()
  "A background Emacs must not take the desktop to warm a page."
  ;; Arrange
  (agent-repl-test-wr--with-queue
    (agent-repl-test-wr--eligible '("alpha")
      (agent-repl--webview-precreate-schedule '("alpha"))
      ;; Act
      (agent-repl-test-wr--unfocused
        (agent-repl--webview-precreate-drain))
      ;; Assert
      (should (null agent-repl-test-wr--mounted)))))

(ert-deftest agent-repl-test-wr-an-unfocused-drain-keeps-the-queue-whole ()
  "Holding is not dropping: every owed page is still owed."
  ;; Arrange
  (agent-repl-test-wr--with-queue
    (agent-repl-test-wr--eligible '("alpha" "beta")
      (agent-repl--webview-precreate-schedule '("alpha" "beta"))
      ;; Act
      (agent-repl-test-wr--unfocused
        (agent-repl--webview-precreate-drain))
      ;; Assert
      (should (equal agent-repl--webview-precreate-queue '("alpha" "beta"))))))

(ert-deftest agent-repl-test-wr-an-unfocused-drain-arms-no-timer ()
  "A held drain does not spin: the focus edge is what wakes it."
  ;; Arrange
  (agent-repl-test-wr--with-queue
    (agent-repl-test-wr--eligible '("alpha")
      (agent-repl--webview-precreate-schedule '("alpha"))
      ;; Act
      (agent-repl-test-wr--unfocused
        (agent-repl--webview-precreate-drain))
      ;; Assert
      (should (null agent-repl--webview-precreate-timer)))))

(ert-deftest agent-repl-test-wr-an-unfocused-drain-is-recorded ()
  "A pre-creation that did not happen has to say why."
  ;; Arrange
  (agent-repl-test-wr--with-queue
    (let ((logged nil))
      (agent-repl-test-wr--eligible '("alpha")
        (agent-repl--webview-precreate-schedule '("alpha"))
        (cl-letf (((symbol-function 'agent-repl--info)
                   (lambda (_ws fmt &rest args) (push (apply #'format fmt args) logged))))
          ;; Act
          (agent-repl-test-wr--unfocused
            (agent-repl--webview-precreate-drain))))
      ;; Assert
      (should (seq-some (lambda (text)
                          (string-search "precreate-parked" text))
                        logged)))))

(ert-deftest agent-repl-test-wr-a-focused-drain-mounts ()
  "An Emacs the user is already looking at pre-creates as it always did."
  ;; Arrange
  (agent-repl-test-wr--with-queue
    (agent-repl-test-wr--eligible '("alpha")
      (agent-repl--webview-precreate-schedule '("alpha"))
      ;; Act
      (agent-repl-test-wr--focused
        (agent-repl--webview-precreate-drain))
      ;; Assert
      (should (equal agent-repl-test-wr--mounted '("alpha"))))))

(ert-deftest agent-repl-test-wr-a-batch-drain-never-holds ()
  "A batch process has no application to activate, so nothing holds."
  ;; Arrange / Act / Assert
  (should (null (agent-repl--webview-precreate-hold-p))))

(ert-deftest agent-repl-test-wr-gaining-focus-resumes-a-parked-drain ()
  "The first look is the moment the warm page was for."
  ;; Arrange
  (agent-repl-test-wr--with-queue
    (agent-repl-test-wr--eligible '("alpha")
      (agent-repl--webview-precreate-schedule '("alpha"))
      (agent-repl-test-wr--unfocused
        (agent-repl--webview-precreate-drain))
      ;; Act
      (agent-repl-test-wr--focused
        (agent-repl--webview-precreate-on-focus-change))
      ;; Assert
      (should (eq agent-repl--webview-precreate-timer 'fake-timer)))))

(ert-deftest agent-repl-test-wr-gaining-focus-clears-the-park ()
  "A resumed drain is no longer parked, so the next focus edge is a no-op."
  ;; Arrange
  (agent-repl-test-wr--with-queue
    (agent-repl-test-wr--eligible '("alpha")
      (agent-repl--webview-precreate-schedule '("alpha"))
      (agent-repl-test-wr--unfocused
        (agent-repl--webview-precreate-drain))
      ;; Act
      (agent-repl-test-wr--focused
        (agent-repl--webview-precreate-on-focus-change))
      ;; Assert
      (should (null agent-repl--webview-precreate-parked)))))

(ert-deftest agent-repl-test-wr-a-focus-edge-with-nothing-parked-does-nothing ()
  "Every focus change runs this; only a parked drain may be woken."
  ;; Arrange
  (agent-repl-test-wr--with-queue
    ;; Act
    (agent-repl-test-wr--focused
      (agent-repl--webview-precreate-on-focus-change))
    ;; Assert
    (should (null agent-repl--webview-precreate-timer))))

(ert-deftest agent-repl-test-wr-a-focus-edge-that-is-still-unfocused-keeps-the-park ()
  "`after-focus-change-function' also fires on LOSING focus."
  ;; Arrange
  (agent-repl-test-wr--with-queue
    (agent-repl-test-wr--eligible '("alpha")
      (agent-repl--webview-precreate-schedule '("alpha"))
      (agent-repl-test-wr--unfocused
        (agent-repl--webview-precreate-drain)
        ;; Act
        (agent-repl--webview-precreate-on-focus-change))
      ;; Assert
      (should agent-repl--webview-precreate-parked))))

(ert-deftest agent-repl-test-wr-the-focus-resume-is-registered ()
  "The release rides the same focus edge status.el repaints the tab bar on.
Asserted by CALLING the edge rather than by inspecting the advice chain:
what matters is that a real focus change reaches the resume."
  ;; Arrange
  (agent-repl-test-wr--with-queue
    (agent-repl-test-wr--eligible '("alpha")
      (agent-repl--webview-precreate-schedule '("alpha"))
      (agent-repl-test-wr--unfocused
        (agent-repl--webview-precreate-drain))
      ;; Act: the other handlers on this edge answer the frame, which has no
      ;; focus state in batch, so they no-op and leave ours the observation.
      (agent-repl-test-wr--focused
        (cl-letf (((symbol-function 'frame-focus-state) (lambda (&rest _) nil)))
          (funcall after-focus-change-function)))
      ;; Assert
      (should (null agent-repl--webview-precreate-parked)))))

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
