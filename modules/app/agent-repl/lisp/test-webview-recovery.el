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

(defvar agent-repl-test-wr--host nil
  "What the stubbed `agent-repl-webhost-xwidget' answers: the host, or nil.")

(defmacro agent-repl-test-wr--with-pass (&rest body)
  "Run BODY with a fresh park, a host present, and the mount recorded."
  (declare (indent 0))
  `(agent-repl-test--with-clean-state
     (let ((agent-repl--webview-precreate-parked nil)
           (agent-repl-test-wr--mounted nil)
           (agent-repl-test-wr--host 'host-xwidget))
       (cl-letf (((symbol-function 'agent-repl--frontend-precreate-webview)
                  (lambda (ws) (push ws agent-repl-test-wr--mounted) :created))
                 ((symbol-function 'agent-repl-webhost-xwidget)
                  (lambda () agent-repl-test-wr--host))
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
              (lambda (_ws fmt &rest args) (push (apply #'format fmt args) ,logged))))
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
                   (lambda (_ws fmt &rest args) (push (apply #'format fmt args) warned))))
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

(ert-deftest agent-repl-test-wr-a-made-page-is-recorded-with-its-reason ()
  "Each made page says what let it be made."
  ;; Arrange
  (agent-repl-test-wr--with-pass
    (agent-repl-test-wr--eligible '("alpha")
      (let (logged)
        (agent-repl-test-wr--logging logged
          ;; Act
          (agent-repl-webview-precreate-all))
        ;; Assert
        (should (agent-repl-test-wr--said-p logged "precreate-created ws=alpha" "reason=batch"))))))

;;;; ---- The pass's quit deferral ----
;;
;; A pass runs off the desktop FOCUS EDGE, which is the same instant a
;; keypress arrives, so it is one of the likeliest places for a real `C-g' to
;; land while Emacs is busy rather than waiting in a read.

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

;;;; ---- Holding: only visible-but-unfocused ----
;;
;; Creating a WKWebView instantiates a native view and macOS activates the
;; owning process when doing so would bring a window forward.  That can
;; happen from ONE state only: a frame already on screen while Emacs is not
;; the focused app.  A HIDDEN Emacs (an `open -gj' launch) creating a view
;; stays hidden, and a FOCUSED Emacs has no focus to steal -- both proceed
;; and paint.  Only visible-but-unfocused holds.  These pin that invariant,
;; the created-reason log, and the focus-edge release.  Only the HOST's
;; creation holds: once the host exists, making a page creates no native view.
;;
;; The three macros below drive the guard's two inputs -- visibility and
;; focus -- directly, so a batch process (which has no real frames) can
;; exercise every desktop state.  `noninteractive' is bound to nil where
;; the hold itself is under test, since the hold short-circuits to nil in
;; batch and every pass test above relies on that.

(defmacro agent-repl-test-wr--visible-unfocused (&rest body)
  "Run BODY as a VISIBLE interactive Emacs that does NOT hold focus.
This is the one held state: creating a view here could foreground Emacs."
  (declare (indent 0))
  `(let ((noninteractive nil))
     (cl-letf (((symbol-function 'agent-repl--emacs-visible-p) (lambda (&rest _) t))
               ((symbol-function 'agent-repl--emacs-focused-p) (lambda (&rest _) nil)))
       ,@body)))

(defmacro agent-repl-test-wr--hidden (&rest body)
  "Run BODY as a HIDDEN interactive Emacs (no visible frame, unfocused).
Creating a view here cannot foreground a hidden app, so it must proceed."
  (declare (indent 0))
  `(let ((noninteractive nil))
     (cl-letf (((symbol-function 'agent-repl--emacs-visible-p) (lambda (&rest _) nil))
               ((symbol-function 'agent-repl--emacs-focused-p) (lambda (&rest _) nil)))
       ,@body)))

(defmacro agent-repl-test-wr--focused (&rest body)
  "Run BODY as a VISIBLE interactive Emacs that DOES hold desktop focus.
A focused app has no focus to steal, so creating a view must proceed."
  (declare (indent 0))
  `(let ((noninteractive nil))
     (cl-letf (((symbol-function 'agent-repl--emacs-visible-p) (lambda (&rest _) t))
               ((symbol-function 'agent-repl--emacs-focused-p) (lambda (&rest _) t)))
       ,@body)))

;;;; ---- The guard predicates ----

(ert-deftest agent-repl-test-wr-visible-frames-report-visible ()
  "A mapped frame is a positively-visible answer."
  ;; Arrange / Act / Assert
  (cl-letf (((symbol-function 'display-graphic-p) (lambda (&rest _) t))
            ((symbol-function 'visible-frame-list) (lambda () '(frame))))
    (should (agent-repl--emacs-visible-p))))

(ert-deftest agent-repl-test-wr-no-visible-frame-is-hidden ()
  "An empty `visible-frame-list' on a graphic display is the hidden case."
  ;; Arrange / Act / Assert
  (cl-letf (((symbol-function 'display-graphic-p) (lambda (&rest _) t))
            ((symbol-function 'visible-frame-list) (lambda () nil)))
    (should-not (agent-repl--emacs-visible-p))))

(ert-deftest agent-repl-test-wr-unreadable-visibility-counts-as-visible ()
  "A state we cannot read errs toward visible, so the guard parks."
  ;; Arrange — no window system to answer the question.
  (cl-letf (((symbol-function 'display-graphic-p) (lambda (&rest _) nil)))
    ;; Act / Assert
    (should (agent-repl--emacs-visible-p))))

(ert-deftest agent-repl-test-wr-can-foreground-only-when-visible-and-unfocused ()
  "The foregrounding state is exactly visible AND not focused."
  ;; Arrange / Act / Assert
  (cl-letf (((symbol-function 'agent-repl--emacs-visible-p) (lambda (&rest _) t))
            ((symbol-function 'agent-repl--emacs-focused-p) (lambda (&rest _) nil)))
    (should (agent-repl--emacs-can-foreground-p))))

(ert-deftest agent-repl-test-wr-hidden-cannot-foreground ()
  "A hidden app creating a view cannot raise itself."
  ;; Arrange / Act / Assert
  (cl-letf (((symbol-function 'agent-repl--emacs-visible-p) (lambda (&rest _) nil))
            ((symbol-function 'agent-repl--emacs-focused-p) (lambda (&rest _) nil)))
    (should-not (agent-repl--emacs-can-foreground-p))))

(ert-deftest agent-repl-test-wr-focused-cannot-foreground ()
  "A focused app has no focus to steal."
  ;; Arrange / Act / Assert
  (cl-letf (((symbol-function 'agent-repl--emacs-visible-p) (lambda (&rest _) t))
            ((symbol-function 'agent-repl--emacs-focused-p) (lambda (&rest _) t)))
    (should-not (agent-repl--emacs-can-foreground-p))))

;;;; ---- The pass honours the guard ----

(ert-deftest agent-repl-test-wr-a-visible-unfocused-pass-without-a-host-makes-nothing ()
  "A visible background Emacs must not take the desktop to create the host."
  ;; Arrange
  (agent-repl-test-wr--with-pass
    (setq agent-repl-test-wr--host nil)
    (agent-repl-test-wr--eligible '("alpha")
      ;; Act
      (agent-repl-test-wr--visible-unfocused
        (agent-repl-webview-precreate-all))
      ;; Assert
      (should (null agent-repl-test-wr--mounted)))))

(ert-deftest agent-repl-test-wr-a-visible-unfocused-pass-without-a-host-parks ()
  "The parked pass waits for the focus edge."
  ;; Arrange
  (agent-repl-test-wr--with-pass
    (setq agent-repl-test-wr--host nil)
    (agent-repl-test-wr--eligible '("alpha")
      ;; Act
      (agent-repl-test-wr--visible-unfocused
        (agent-repl-webview-precreate-all))
      ;; Assert
      (should agent-repl--webview-precreate-parked))))

(ert-deftest agent-repl-test-wr-a-park-is-recorded ()
  "A pass that did not happen has to say why, and name the state."
  ;; Arrange
  (agent-repl-test-wr--with-pass
    (setq agent-repl-test-wr--host nil)
    (agent-repl-test-wr--eligible '("alpha")
      (let (logged)
        (agent-repl-test-wr--logging logged
          ;; Act
          (agent-repl-test-wr--visible-unfocused
            (agent-repl-webview-precreate-all)))
        ;; Assert
        (should (agent-repl-test-wr--said-p logged "precreate-parked" "reason=visible-unfocused"))))))

(ert-deftest agent-repl-test-wr-a-standing-park-is-recorded-once ()
  "Every roster push re-runs the pass; a standing park says so once."
  ;; Arrange
  (agent-repl-test-wr--with-pass
    (setq agent-repl-test-wr--host nil)
    (agent-repl-test-wr--eligible '("alpha")
      (let (logged)
        (agent-repl-test-wr--logging logged
          ;; Act
          (agent-repl-test-wr--visible-unfocused
            (agent-repl-webview-precreate-all)
            (agent-repl-webview-precreate-all)))
        ;; Assert
        (should (= 1 (seq-count (lambda (text) (string-search "precreate-parked" text)) logged)))))))

(ert-deftest agent-repl-test-wr-a-visible-unfocused-pass-with-a-host-makes-pages ()
  "Once the host exists a page creates no native view, so nothing holds."
  ;; Arrange
  (agent-repl-test-wr--with-pass
    (agent-repl-test-wr--eligible '("alpha")
      ;; Act
      (agent-repl-test-wr--visible-unfocused
        (agent-repl-webview-precreate-all))
      ;; Assert
      (should (equal agent-repl-test-wr--mounted '("alpha"))))))

(ert-deftest agent-repl-test-wr-a-hidden-pass-without-a-host-makes-pages ()
  "A hidden Emacs creating the host cannot raise itself, so it proceeds."
  ;; Arrange
  (agent-repl-test-wr--with-pass
    (setq agent-repl-test-wr--host nil)
    (agent-repl-test-wr--eligible '("alpha")
      ;; Act
      (agent-repl-test-wr--hidden
        (agent-repl-webview-precreate-all))
      ;; Assert
      (should (equal agent-repl-test-wr--mounted '("alpha"))))))

(ert-deftest agent-repl-test-wr-a-hidden-page-is-recorded-as-hidden ()
  "The made record names the state that let it be made."
  ;; Arrange
  (agent-repl-test-wr--with-pass
    (setq agent-repl-test-wr--host nil)
    (agent-repl-test-wr--eligible '("alpha")
      (let (logged)
        (agent-repl-test-wr--logging logged
          ;; Act
          (agent-repl-test-wr--hidden
            (agent-repl-webview-precreate-all)))
        ;; Assert
        (should (agent-repl-test-wr--said-p logged "precreate-created" "reason=hidden"))))))

(ert-deftest agent-repl-test-wr-a-focused-pass-without-a-host-makes-pages ()
  "A focused Emacs has no focus to steal, so it proceeds."
  ;; Arrange
  (agent-repl-test-wr--with-pass
    (setq agent-repl-test-wr--host nil)
    (agent-repl-test-wr--eligible '("alpha")
      ;; Act
      (agent-repl-test-wr--focused
        (agent-repl-webview-precreate-all))
      ;; Assert
      (should (equal agent-repl-test-wr--mounted '("alpha"))))))

(ert-deftest agent-repl-test-wr-a-focused-page-is-recorded-as-focused ()
  "The made record names the state that let it be made."
  ;; Arrange
  (agent-repl-test-wr--with-pass
    (setq agent-repl-test-wr--host nil)
    (agent-repl-test-wr--eligible '("alpha")
      (let (logged)
        (agent-repl-test-wr--logging logged
          ;; Act
          (agent-repl-test-wr--focused
            (agent-repl-webview-precreate-all)))
        ;; Assert
        (should (agent-repl-test-wr--said-p logged "precreate-created" "reason=focused"))))))

(ert-deftest agent-repl-test-wr-a-batch-pass-never-holds ()
  "A batch process has no application to activate, so nothing holds."
  ;; Arrange / Act / Assert
  (should (null (agent-repl--webview-precreate-hold-p))))

(ert-deftest agent-repl-test-wr-the-editor-startup-still-holds-a-visible-unfocused-emacs ()
  "The editor's startup never lifts the focus park: a visible but unfocused
Emacs creates no host until it is looked at, so the startup cannot take
focus from another application.  Its tabs then open on the focus edge."
  ;; Arrange
  (let ((noninteractive nil)
        (agent-repl-startup--phase 'running))
    (cl-letf (((symbol-function 'agent-repl--emacs-can-foreground-p) (lambda () t)))
      ;; Act / Assert
      (should (agent-repl--webview-precreate-hold-p)))))

;;;; ---- The focus edge ----

(ert-deftest agent-repl-test-wr-gaining-focus-runs-a-parked-pass ()
  "The first look is the moment the parked pages were for."
  ;; Arrange
  (agent-repl-test-wr--with-pass
    (setq agent-repl-test-wr--host nil)
    (agent-repl-test-wr--eligible '("alpha")
      (agent-repl-test-wr--visible-unfocused
        (agent-repl-webview-precreate-all))
      ;; Act
      (agent-repl-test-wr--focused
        (agent-repl--webview-precreate-on-focus-change))
      ;; Assert
      (should (equal agent-repl-test-wr--mounted '("alpha"))))))

(ert-deftest agent-repl-test-wr-gaining-focus-clears-the-park ()
  "A resumed pass is no longer parked, so the next focus edge is a no-op."
  ;; Arrange
  (agent-repl-test-wr--with-pass
    (setq agent-repl-test-wr--host nil)
    (agent-repl-test-wr--eligible '("alpha")
      (agent-repl-test-wr--visible-unfocused
        (agent-repl-webview-precreate-all))
      ;; Act
      (agent-repl-test-wr--focused
        (agent-repl--webview-precreate-on-focus-change))
      ;; Assert
      (should (null agent-repl--webview-precreate-parked)))))

(ert-deftest agent-repl-test-wr-a-resumed-pass-is-recorded ()
  "The focus edge that released a park says so."
  ;; Arrange
  (agent-repl-test-wr--with-pass
    (setq agent-repl-test-wr--host nil)
    (agent-repl-test-wr--eligible '("alpha")
      (agent-repl-test-wr--visible-unfocused
        (agent-repl-webview-precreate-all))
      (let (logged)
        (agent-repl-test-wr--logging logged
          ;; Act
          (agent-repl-test-wr--focused
            (agent-repl--webview-precreate-on-focus-change)))
        ;; Assert
        (should (agent-repl-test-wr--said-p logged "precreate-resumed-on-focus"))))))

(ert-deftest agent-repl-test-wr-a-focus-edge-with-nothing-parked-does-nothing ()
  "Every focus change runs this; only a parked pass may be run."
  ;; Arrange
  (agent-repl-test-wr--with-pass
    (agent-repl-test-wr--eligible '("alpha")
      ;; Act
      (agent-repl-test-wr--focused
        (agent-repl--webview-precreate-on-focus-change))
      ;; Assert
      (should (null agent-repl-test-wr--mounted)))))

(ert-deftest agent-repl-test-wr-a-focus-edge-that-is-still-unfocused-keeps-the-park ()
  "`after-focus-change-function' also fires on LOSING focus."
  ;; Arrange
  (agent-repl-test-wr--with-pass
    (setq agent-repl-test-wr--host nil)
    (agent-repl-test-wr--eligible '("alpha")
      (agent-repl-test-wr--visible-unfocused
        (agent-repl-webview-precreate-all)
        ;; Act
        (agent-repl--webview-precreate-on-focus-change))
      ;; Assert
      (should agent-repl--webview-precreate-parked))))

(ert-deftest agent-repl-test-wr-the-focus-resume-is-registered ()
  "The release rides the same focus edge status.el repaints the tab bar on.
Asserted by CALLING the edge rather than by inspecting the advice chain:
what matters is that a real focus change reaches the resume."
  ;; Arrange
  (agent-repl-test-wr--with-pass
    (setq agent-repl-test-wr--host nil)
    (agent-repl-test-wr--eligible '("alpha")
      (agent-repl-test-wr--visible-unfocused
        (agent-repl-webview-precreate-all))
      ;; Act: the other handlers on this edge answer the frame, which has no
      ;; focus state in batch, so they no-op and leave ours the observation.
      (agent-repl-test-wr--focused
        (cl-letf (((symbol-function 'frame-focus-state) (lambda (&rest _) nil)))
          (funcall after-focus-change-function)))
      ;; Assert
      (should (null agent-repl--webview-precreate-parked)))))

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
