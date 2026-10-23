;;; test-webview-recovery.el --- ERT tests for webview-recovery.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Every workspace's slot and page, made ahead of the look in one immediate
;; pass: eligibility, the pass itself, its quit deferral, the focus park that
;; holds only the HOST's creation, and the link-up and roster edges that run
;; it.
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
  "Workspaces the pass asked frontend.el to pre-create, newest first.")

(defmacro agent-repl-test-wr--with-pass (&rest body)
  "Run BODY with the mount recorded."
  (declare (indent 0))
  `(agent-repl-test--with-clean-state
     (let ((agent-repl-test-wr--mounted nil))
       (cl-letf (((symbol-function 'agent-repl--frontend-precreate-webview)
                  (lambda (ws) (push ws agent-repl-test-wr--mounted) :created))
                 ((symbol-function 'agent-repl--panels-on-view-created) #'ignore))
         ,@body))))

(defmacro agent-repl-test-wr--eligible (names &rest body)
  "Run BODY with exactly NAMES answering the pre-creation eligibility test."
  (declare (indent 1))
  `(cl-letf (((symbol-function 'agent-repl--frontend-precreate-refusal)
              (lambda (ws) (if (member ws ,names) nil :not-live)))
             ((symbol-function 'agent-repl--live-ws-names)
              (lambda () ,names)))
     ,@body))

(defmacro agent-repl-test-wr--logging (logged &rest body)
  "Run BODY collecting every INFO record's text into LOGGED, newest first."
  (declare (indent 1))
  `(cl-letf (((symbol-function 'agent-repl--info)
              (agent-repl-test--record-into ,logged)))
     ,@body))

(defun agent-repl-test-wr--said-p (logged &rest parts)
  "Non-nil when a text of LOGGED holds every one of PARTS."
  (seq-some (lambda (text) (seq-every-p (lambda (part) (string-search part text)) parts))
            logged))

;;;; ---- Eligibility ----

(ert-deftest agent-repl-test-wr-needed-p-follows-the-mounts-own-refusal ()
  "The pass asks the SAME question the mount does, so they cannot disagree."
  ;; Arrange
  (agent-repl-test-wr--with-pass
    (agent-repl-test-wr--eligible '("alpha")
      ;; Act / Assert
      (should (agent-repl--webview-precreate-needed-p "alpha")))))

(ert-deftest agent-repl-test-wr-a-refused-workspace-is-not-needed ()
  "A workspace the mount would refuse is never made."
  ;; Arrange
  (agent-repl-test-wr--with-pass
    (agent-repl-test-wr--eligible '("alpha")
      ;; Act / Assert
      (should-not (agent-repl--webview-precreate-needed-p "beta")))))

(ert-deftest agent-repl-test-wr-missing-lists-only-eligible-workspaces ()
  "The owed set is the live workspaces the mount would accept."
  ;; Arrange
  (agent-repl-test-wr--with-pass
    (cl-letf (((symbol-function 'agent-repl--live-ws-names)
               (lambda () '("alpha" "beta")))
              ((symbol-function 'agent-repl--frontend-precreate-refusal)
               (lambda (ws) (if (equal ws "alpha") nil :already-mounted))))
      ;; Act / Assert
      (should (equal (agent-repl--webview-precreate-missing) '("alpha"))))))

;;;; ---- The pass ----

(ert-deftest agent-repl-test-wr-the-pass-makes-every-owed-workspace ()
  "Every owed workspace gets its slot and page in the one pass."
  ;; Arrange
  (agent-repl-test-wr--with-pass
    (agent-repl-test-wr--eligible '("alpha" "beta")
      ;; Act
      (agent-repl-webview-precreate-all)
      ;; Assert
      (should (equal (reverse agent-repl-test-wr--mounted) '("alpha" "beta"))))))

(ert-deftest agent-repl-test-wr-the-pass-answers-how-many-it-made ()
  "The pass reports the pages it made."
  ;; Arrange
  (agent-repl-test-wr--with-pass
    (agent-repl-test-wr--eligible '("alpha" "beta")
      ;; Act / Assert
      (should (= 2 (agent-repl-webview-precreate-all))))))

(ert-deftest agent-repl-test-wr-the-pass-makes-nothing-when-nothing-is-owed ()
  "Every page already made means the pass owes no work."
  ;; Arrange
  (agent-repl-test-wr--with-pass
    (agent-repl-test-wr--eligible '()
      ;; Act
      (agent-repl-webview-precreate-all)
      ;; Assert
      (should (null agent-repl-test-wr--mounted)))))

(ert-deftest agent-repl-test-wr-a-failing-mount-does-not-cost-the-rest ()
  "One workspace's failure must not cost the others their pages."
  ;; Arrange
  (agent-repl-test-wr--with-pass
    (agent-repl-test-wr--eligible '("alpha" "beta")
      (cl-letf (((symbol-function 'agent-repl--frontend-precreate-webview)
                 (lambda (ws)
                   (when (equal ws "alpha") (error "mount blew up"))
                   (push ws agent-repl-test-wr--mounted))))
        ;; Act
        (agent-repl-webview-precreate-all))
      ;; Assert
      (should (equal agent-repl-test-wr--mounted '("beta"))))))

(ert-deftest agent-repl-test-wr-a-failing-mount-is-warned-about ()
  "A mount that signals is recorded at WARN, never swallowed."
  ;; Arrange
  (agent-repl-test-wr--with-pass
    (agent-repl-test-wr--eligible '("alpha")
      (let (warned)
        (cl-letf (((symbol-function 'agent-repl--frontend-precreate-webview)
                   (lambda (_ws) (error "mount blew up")))
                  ((symbol-function 'agent-repl--warn)
                   (agent-repl-test--record-into warned)))
          ;; Act
          (agent-repl-webview-precreate-all))
        ;; Assert
        (should (agent-repl-test-wr--said-p warned "outcome=failed"))))))

(ert-deftest agent-repl-test-wr-a-created-view-is-announced-to-the-panels ()
  "Each created view is handed to the panels, whose restore may await it."
  ;; Arrange
  (agent-repl-test-wr--with-pass
    (agent-repl-test-wr--eligible '("alpha")
      (let (announced)
        (cl-letf (((symbol-function 'agent-repl--panels-on-view-created)
                   (lambda (ws) (push ws announced))))
          ;; Act
          (agent-repl-webview-precreate-all))
        ;; Assert
        (should (equal announced '("alpha")))))))

(ert-deftest agent-repl-test-wr-a-made-page-is-recorded ()
  "Each made page is recorded by its workspace."
  ;; Arrange
  (agent-repl-test-wr--with-pass
    (agent-repl-test-wr--eligible '("alpha")
      (let (logged)
        (agent-repl-test-wr--logging logged
          ;; Act
          (agent-repl-webview-precreate-all))
        ;; Assert
        (should (agent-repl-test-wr--said-p logged "precreate-created ws=alpha"))))))

;;;; ---- The pass's quit deferral ----
;;
;; A pass can run at the same instant a keypress arrives, so it is a place a
;; real `C-g' can land while Emacs is busy rather than waiting in a read.

(ert-deftest agent-repl-test-wr-the-pass-defers-a-quit-that-lands-in-it ()
  "A C-g arriving inside a pass is held for the command loop, not taken here."
  ;; Arrange
  (agent-repl-test-wr--with-pass
    (cl-letf (((symbol-function 'agent-repl--webview-precreate-pass)
               (lambda () (setq quit-flag t) 0)))
      ;; Act / Assert
      (should (agent-repl-test--quit-deferred-p
                (agent-repl-webview-precreate-all))))))

(ert-deftest agent-repl-test-wr-the-pass-mounts-despite-a-pending-quit ()
  "A quit already requested does not stop the pass it landed on."
  ;; Arrange
  (agent-repl-test-wr--with-pass
    (agent-repl-test-wr--eligible '("alpha")
      ;; Act
      (agent-repl-test--with-pending-quit
        (agent-repl-webview-precreate-all))
      ;; Assert
      (should (equal agent-repl-test-wr--mounted '("alpha"))))))

;;;; ---- The host regardless of focus ----

(ert-deftest agent-repl-test-wr-an-unfocused-emacs-with-no-host-makes-pages ()
  "THE HOST IS MADE REGARDLESS OF FOCUS (owner ruling, 2026-10-12): a visible
but unfocused Emacs with no host yet still makes every owed page."
  ;; Arrange
  (agent-repl-test-wr--with-pass
    (agent-repl-test-wr--eligible '("alpha")
      (let ((noninteractive nil))
        (cl-letf (((symbol-function 'visible-frame-list) (lambda () '(frame)))
                  ((symbol-function 'agent-repl--emacs-focused-p) (lambda (&rest _) nil)))
          ;; Act
          (agent-repl-webview-precreate-all)))
      ;; Assert
      (should (equal agent-repl-test-wr--mounted '("alpha"))))))

;;;; ---- The link-up and roster edges ----

(ert-deftest agent-repl-test-wr-link-up-makes-every-owed-workspace ()
  "The link coming up is the first moment a page can be built at all."
  ;; Arrange
  (agent-repl-test-wr--with-pass
    (agent-repl-test-wr--eligible '("alpha" "beta")
      ;; Act
      (agent-repl--webview-precreate-on-link-up 'conn)
      ;; Assert
      (should (equal (reverse agent-repl-test-wr--mounted) '("alpha" "beta"))))))

(ert-deftest agent-repl-test-wr-link-up-is-registered-on-the-link-hook ()
  "The pre-creation rides the daemon link's own up edge."
  ;; Act / Assert
  (should (memq #'agent-repl--webview-precreate-on-link-up
                agent-repl-link-up-functions)))

(ert-deftest agent-repl-test-wr-roster-update-makes-newly-registered ()
  "A cold start's workspaces arrive on the roster push, not at link-up."
  ;; Arrange
  (agent-repl-test-wr--with-pass
    (agent-repl-test-wr--eligible '("alpha" "beta")
      ;; Act
      (agent-repl--webview-precreate-on-roster-update 'roster)
      ;; Assert
      (should (equal (reverse agent-repl-test-wr--mounted) '("alpha" "beta"))))))

(ert-deftest agent-repl-test-wr-roster-update-skips-an-already-mounted-workspace ()
  "A workspace whose page is made is refused, so the push never makes it again."
  ;; Arrange
  (agent-repl-test-wr--with-pass
    ;; alpha is eligible; beta is not (its page is already made).
    (agent-repl-test-wr--eligible '("alpha")
      (cl-letf (((symbol-function 'agent-repl--live-ws-names)
                 (lambda () '("alpha" "beta"))))
        ;; Act
        (agent-repl--webview-precreate-on-roster-update 'roster)
        ;; Assert
        (should (equal agent-repl-test-wr--mounted '("alpha")))))))

(ert-deftest agent-repl-test-wr-roster-update-is-registered-on-the-roster-hook ()
  "The pre-creation also rides every accepted roster push."
  ;; Act / Assert
  (should (memq #'agent-repl--webview-precreate-on-roster-update
                agent-repl-roster-update-functions)))

;;; test-webview-recovery.el ends here
