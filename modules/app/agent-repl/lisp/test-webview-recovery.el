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
           (agent-repl--webview-precreate-pass-open nil)
           (agent-repl--webview-precreate-pass-mounted 0)
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

;;;; ---- The drain's quit deferral ----
;;
;; The tick runs off the desktop FOCUS EDGE, which is the same instant a
;; keypress arrives, so it is one of the likeliest places for a real `C-g' to
;; land while Emacs is busy rather than waiting in a read.

(ert-deftest agent-repl-test-wr-the-drain-defers-a-quit-that-lands-in-it ()
  "A C-g arriving inside a tick is held for the command loop, not taken here."
  ;; Arrange
  (agent-repl-test-wr--with-queue
    (cl-letf (((symbol-function 'agent-repl--webview-precreate-tick)
               (lambda () (setq quit-flag t))))
      ;; Act / Assert
      (should (agent-repl-test--quit-deferred-p
                (agent-repl--webview-precreate-drain))))))

(ert-deftest agent-repl-test-wr-the-drain-mounts-despite-a-pending-quit ()
  "A quit already requested does not stop the tick it landed on."
  ;; Arrange
  (agent-repl-test-wr--with-queue
    (agent-repl-test-wr--eligible '("alpha")
      (agent-repl--webview-precreate-schedule '("alpha"))
      ;; Act
      (agent-repl-test--with-pending-quit
        (agent-repl--webview-precreate-drain))
      ;; Assert
      (should (equal agent-repl-test-wr--mounted '("alpha"))))))

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


;;;; ---- Holding: only visible-but-unfocused ----
;;
;; Creating a WKWebView instantiates a native view and macOS activates the
;; owning process when doing so would bring a window forward.  That can
;; happen from ONE state only: a frame already on screen while Emacs is not
;; the focused app.  A HIDDEN Emacs (an `open -gj' launch) creating a view
;; stays hidden, and a FOCUSED Emacs has no focus to steal -- both proceed
;; and paint.  Only visible-but-unfocused holds.  These pin that invariant,
;; the created-reason log, and the focus-edge release.
;;
;; The three macros below drive the guard's two inputs -- visibility and
;; focus -- directly, so a batch process (which has no real frames) can
;; exercise every desktop state.  `noninteractive' is bound to nil where
;; the hold itself is under test, since the hold short-circuits to nil in
;; batch and every drain test above relies on that.

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

;;;; ---- The drain honours the guard ----

(ert-deftest agent-repl-test-wr-a-visible-unfocused-drain-mounts-nothing ()
  "A visible background Emacs must not take the desktop to warm a page."
  ;; Arrange
  (agent-repl-test-wr--with-queue
    (agent-repl-test-wr--eligible '("alpha")
      (agent-repl--webview-precreate-schedule '("alpha"))
      ;; Act
      (agent-repl-test-wr--visible-unfocused
        (agent-repl--webview-precreate-drain))
      ;; Assert
      (should (null agent-repl-test-wr--mounted)))))

(ert-deftest agent-repl-test-wr-a-visible-unfocused-drain-keeps-the-queue-whole ()
  "Holding is not dropping: every owed page is still owed."
  ;; Arrange
  (agent-repl-test-wr--with-queue
    (agent-repl-test-wr--eligible '("alpha" "beta")
      (agent-repl--webview-precreate-schedule '("alpha" "beta"))
      ;; Act
      (agent-repl-test-wr--visible-unfocused
        (agent-repl--webview-precreate-drain))
      ;; Assert
      (should (equal agent-repl--webview-precreate-queue '("alpha" "beta"))))))

(ert-deftest agent-repl-test-wr-a-visible-unfocused-drain-arms-no-timer ()
  "A held drain does not spin: the focus edge is what wakes it."
  ;; Arrange
  (agent-repl-test-wr--with-queue
    (agent-repl-test-wr--eligible '("alpha")
      (agent-repl--webview-precreate-schedule '("alpha"))
      ;; Act
      (agent-repl-test-wr--visible-unfocused
        (agent-repl--webview-precreate-drain))
      ;; Assert
      (should (null agent-repl--webview-precreate-timer)))))

(ert-deftest agent-repl-test-wr-a-visible-unfocused-drain-is-recorded ()
  "A pre-creation that did not happen has to say why, and name the state."
  ;; Arrange
  (agent-repl-test-wr--with-queue
    (let ((logged nil))
      (agent-repl-test-wr--eligible '("alpha")
        (agent-repl--webview-precreate-schedule '("alpha"))
        (cl-letf (((symbol-function 'agent-repl--info)
                   (lambda (_ws fmt &rest args) (push (apply #'format fmt args) logged))))
          ;; Act
          (agent-repl-test-wr--visible-unfocused
            (agent-repl--webview-precreate-drain))))
      ;; Assert
      (should (seq-some (lambda (text)
                          (and (string-search "precreate-parked" text)
                               (string-search "reason=visible-unfocused" text)))
                        logged)))))

(ert-deftest agent-repl-test-wr-a-hidden-drain-mounts ()
  "A hidden Emacs creating a view cannot foreground it, so it paints."
  ;; Arrange
  (agent-repl-test-wr--with-queue
    (agent-repl-test-wr--eligible '("alpha")
      (agent-repl--webview-precreate-schedule '("alpha"))
      ;; Act
      (agent-repl-test-wr--hidden
        (agent-repl--webview-precreate-drain))
      ;; Assert
      (should (equal agent-repl-test-wr--mounted '("alpha"))))))

(ert-deftest agent-repl-test-wr-a-hidden-mount-is-recorded-as-hidden ()
  "A mount that happened while hidden names the reason it was safe."
  ;; Arrange
  (agent-repl-test-wr--with-queue
    (let ((logged nil))
      (agent-repl-test-wr--eligible '("alpha")
        (agent-repl--webview-precreate-schedule '("alpha"))
        (cl-letf (((symbol-function 'agent-repl--info)
                   (lambda (_ws fmt &rest args) (push (apply #'format fmt args) logged))))
          ;; Act
          (agent-repl-test-wr--hidden
            (agent-repl--webview-precreate-drain))))
      ;; Assert
      (should (seq-some (lambda (text)
                          (and (string-search "precreate-created" text)
                               (string-search "reason=hidden" text)))
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

(ert-deftest agent-repl-test-wr-a-focused-mount-is-recorded-as-focused ()
  "A mount that happened while focused names the reason it was safe."
  ;; Arrange
  (agent-repl-test-wr--with-queue
    (let ((logged nil))
      (agent-repl-test-wr--eligible '("alpha")
        (agent-repl--webview-precreate-schedule '("alpha"))
        (cl-letf (((symbol-function 'agent-repl--info)
                   (lambda (_ws fmt &rest args) (push (apply #'format fmt args) logged))))
          ;; Act
          (agent-repl-test-wr--focused
            (agent-repl--webview-precreate-drain))))
      ;; Assert
      (should (seq-some (lambda (text)
                          (and (string-search "precreate-created" text)
                               (string-search "reason=focused" text)))
                        logged)))))

(ert-deftest agent-repl-test-wr-a-batch-drain-never-holds ()
  "A batch process has no application to activate, so nothing holds."
  ;; Arrange / Act / Assert
  (should (null (agent-repl--webview-precreate-hold-p))))

(ert-deftest agent-repl-test-wr-the-editor-startup-never-holds ()
  "During the editor's startup every page is pre-created at once: a tab
opens only once its page drew, so a focus wait would open no tab."
  ;; Arrange
  (let ((noninteractive nil)
        (agent-repl-startup--phase 'running))
    (cl-letf (((symbol-function 'agent-repl--emacs-can-foreground-p) (lambda () t)))
      ;; Act / Assert
      (should (null (agent-repl--webview-precreate-hold-p))))))

(ert-deftest agent-repl-test-wr-after-the-startup-a-visible-unfocused-emacs-holds ()
  "Outside the startup the focus hold stands as it always did."
  ;; Arrange
  (let ((noninteractive nil)
        (agent-repl-startup--phase 'done))
    (cl-letf (((symbol-function 'agent-repl--emacs-can-foreground-p) (lambda () t)))
      ;; Act / Assert
      (should (agent-repl--webview-precreate-hold-p)))))

(ert-deftest agent-repl-test-wr-gaining-focus-resumes-a-parked-drain ()
  "The first look is the moment the warm page was for."
  ;; Arrange
  (agent-repl-test-wr--with-queue
    (agent-repl-test-wr--eligible '("alpha")
      (agent-repl--webview-precreate-schedule '("alpha"))
      (agent-repl-test-wr--visible-unfocused
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
      (agent-repl-test-wr--visible-unfocused
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
      (agent-repl-test-wr--visible-unfocused
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
      (agent-repl-test-wr--visible-unfocused
        (agent-repl--webview-precreate-drain))
      ;; Act: the other handlers on this edge answer the frame, which has no
      ;; focus state in batch, so they no-op and leave ours the observation.
      (agent-repl-test-wr--focused
        (cl-letf (((symbol-function 'frame-focus-state) (lambda (&rest _) nil)))
          (funcall after-focus-change-function)))
      ;; Assert
      (should (null agent-repl--webview-precreate-parked)))))


;;;; ---- The hold admits a PASS, not an item ----
;;
;; Re-asking the hold before every queue item is what stranded realtest 5's
;; cold start for twenty seconds: mounting a WKWebView hands key-window
;; status around, so an Emacs that was focused when the drain resumed reads
;; back as visible-but-unfocused a moment later and the remainder re-parks.
;; Two open workspaces painted on two separate focus edges -- one panel per
;; edge -- while the healthy shape paints both on one edge in under two
;; seconds.  The hold now gates the START of a pass; the pass then drains
;; the queue it can reach without re-asking, and the hold is asked in full
;; again for the next pass.

(defun agent-repl-test-wr--drain-until-quiet (&optional ticks)
  "Drive the drain chain until it arms no further timer.
The suites replace `run-at-time' with a token, so nothing fires on its
own; this stands in for the timer chain a live Emacs would run.  TICKS
bounds the loop so a chain that never settles fails as a hang would."
  (let ((budget (or ticks 20)))
    (agent-repl--webview-precreate-drain)
    (while (and agent-repl--webview-precreate-timer (> budget 0))
      (setq budget (1- budget))
      (agent-repl--webview-precreate-drain))
    (should (> budget 0))))

(ert-deftest agent-repl-test-wr-one-focus-edge-drains-the-whole-queue ()
  "The healthy shape: every owed panel paints on the first look, not one per look."
  ;; Arrange — parked with two workspaces owed.
  (agent-repl-test-wr--with-queue
    (agent-repl-test-wr--eligible '("alpha" "beta")
      (agent-repl--webview-precreate-schedule '("alpha" "beta"))
      (agent-repl-test-wr--visible-unfocused
        (agent-repl--webview-precreate-drain))
      ;; Act — one focus edge, and the chain it arms runs to quiet.
      (agent-repl-test-wr--focused
        (agent-repl--webview-precreate-on-focus-change)
        (agent-repl-test-wr--drain-until-quiet))
      ;; Assert
      (should (equal (reverse agent-repl-test-wr--mounted) '("alpha" "beta"))))))

(ert-deftest agent-repl-test-wr-losing-focus-mid-pass-still-mounts-the-remainder ()
  "The interrupted queue: focus gone after the first mount must not strand the rest."
  ;; Arrange — the pass opens focused, so the hold is already satisfied.
  (agent-repl-test-wr--with-queue
    (agent-repl-test-wr--eligible '("alpha" "beta")
      (agent-repl--webview-precreate-schedule '("alpha" "beta"))
      (agent-repl-test-wr--focused
        (agent-repl--webview-precreate-drain))
      ;; Act — focus is lost before the second item's tick.
      (agent-repl-test-wr--visible-unfocused
        (agent-repl--webview-precreate-drain))
      ;; Assert
      (should (equal (reverse agent-repl-test-wr--mounted) '("alpha" "beta"))))))

(ert-deftest agent-repl-test-wr-losing-focus-mid-pass-does-not-re-park ()
  "An open pass has nothing to wake, so no second focus edge is owed."
  ;; Arrange
  (agent-repl-test-wr--with-queue
    (agent-repl-test-wr--eligible '("alpha" "beta")
      (agent-repl--webview-precreate-schedule '("alpha" "beta"))
      (agent-repl-test-wr--focused
        (agent-repl--webview-precreate-drain))
      ;; Act
      (agent-repl-test-wr--visible-unfocused
        (agent-repl--webview-precreate-drain))
      ;; Assert
      (should (null agent-repl--webview-precreate-parked)))))

(ert-deftest agent-repl-test-wr-a-focus-edge-opens-exactly-one-pass ()
  "One look buys one pass, however many items that pass mounts."
  ;; Arrange
  (agent-repl-test-wr--with-queue
    (let ((opened 0))
      (agent-repl-test-wr--eligible '("alpha" "beta")
        (agent-repl--webview-precreate-schedule '("alpha" "beta"))
        (agent-repl-test-wr--visible-unfocused
          (agent-repl--webview-precreate-drain))
        (cl-letf (((symbol-function 'agent-repl--info)
                   (lambda (_ws fmt &rest args)
                     (when (string-search "precreate-pass-opened"
                                          (apply #'format fmt args))
                       (setq opened (1+ opened))))))
          ;; Act
          (agent-repl-test-wr--focused
            (agent-repl--webview-precreate-on-focus-change)
            (agent-repl-test-wr--drain-until-quiet))))
      ;; Assert
      (should (= opened 1)))))

(ert-deftest agent-repl-test-wr-the-pass-closes-when-the-queue-empties ()
  "A closed pass is what makes the hold apply in full to the next one."
  ;; Arrange
  (agent-repl-test-wr--with-queue
    (agent-repl-test-wr--eligible '("alpha")
      (agent-repl--webview-precreate-schedule '("alpha"))
      ;; Act
      (agent-repl-test-wr--focused
        (agent-repl-test-wr--drain-until-quiet))
      ;; Assert
      (should (null agent-repl--webview-precreate-pass-open)))))

(ert-deftest agent-repl-test-wr-a-later-schedule-is-held-again-after-a-pass-closed ()
  "The relaxation is per pass, never a permanent licence to mount unfocused."
  ;; Arrange — a first pass drains to completion while focused.
  (agent-repl-test-wr--with-queue
    (agent-repl-test-wr--eligible '("alpha" "beta")
      (agent-repl--webview-precreate-schedule '("alpha"))
      (agent-repl-test-wr--focused
        (agent-repl-test-wr--drain-until-quiet))
      (setq agent-repl-test-wr--mounted nil)
      ;; Act — a later workspace arrives while Emacs sits unfocused.
      (agent-repl--webview-precreate-schedule '("beta"))
      (agent-repl-test-wr--visible-unfocused
        (agent-repl--webview-precreate-drain))
      ;; Assert
      (should (null agent-repl-test-wr--mounted)))))

(ert-deftest agent-repl-test-wr-a-later-schedule-parks-again-after-a-pass-closed ()
  "The held workspace is parked, so the next focus edge is what wakes it."
  ;; Arrange
  (agent-repl-test-wr--with-queue
    (agent-repl-test-wr--eligible '("alpha" "beta")
      (agent-repl--webview-precreate-schedule '("alpha"))
      (agent-repl-test-wr--focused
        (agent-repl-test-wr--drain-until-quiet))
      ;; Act
      (agent-repl--webview-precreate-schedule '("beta"))
      (agent-repl-test-wr--visible-unfocused
        (agent-repl--webview-precreate-drain))
      ;; Assert
      (should agent-repl--webview-precreate-parked))))

;;;; ---- The pass is readable afterwards ----

(ert-deftest agent-repl-test-wr-an-opened-pass-is-recorded ()
  "A drain that started has to say so, and how much it was owed."
  ;; Arrange
  (agent-repl-test-wr--with-queue
    (let ((logged nil))
      (agent-repl-test-wr--eligible '("alpha" "beta")
        (agent-repl--webview-precreate-schedule '("alpha" "beta"))
        (cl-letf (((symbol-function 'agent-repl--info)
                   (lambda (_ws fmt &rest args) (push (apply #'format fmt args) logged))))
          ;; Act
          (agent-repl-test-wr--focused
            (agent-repl--webview-precreate-drain))))
      ;; Assert
      (should (seq-some (lambda (text)
                          (and (string-search "precreate-pass-opened" text)
                               (string-search "queued=2" text)))
                        logged)))))

(ert-deftest agent-repl-test-wr-a-completed-pass-is-recorded ()
  "A drain that finished has to say so, and how many pages it mounted."
  ;; Arrange
  (agent-repl-test-wr--with-queue
    (let ((logged nil))
      (agent-repl-test-wr--eligible '("alpha" "beta")
        (agent-repl--webview-precreate-schedule '("alpha" "beta"))
        (cl-letf (((symbol-function 'agent-repl--info)
                   (lambda (_ws fmt &rest args) (push (apply #'format fmt args) logged))))
          ;; Act
          (agent-repl-test-wr--focused
            (agent-repl-test-wr--drain-until-quiet))))
      ;; Assert
      (should (seq-some (lambda (text)
                          (and (string-search "precreate-pass-completed" text)
                               (string-search "mounted=2" text)))
                        logged)))))

(ert-deftest agent-repl-test-wr-a-resumed-drain-is-recorded ()
  "A queue that came back to life has to be readable as such, not inferred."
  ;; Arrange
  (agent-repl-test-wr--with-queue
    (let ((logged nil))
      (agent-repl-test-wr--eligible '("alpha")
        (agent-repl--webview-precreate-schedule '("alpha"))
        (agent-repl-test-wr--visible-unfocused
          (agent-repl--webview-precreate-drain))
        (cl-letf (((symbol-function 'agent-repl--info)
                   (lambda (_ws fmt &rest args) (push (apply #'format fmt args) logged))))
          ;; Act
          (agent-repl-test-wr--focused
            (agent-repl--webview-precreate-on-focus-change))))
      ;; Assert
      (should (seq-some (lambda (text)
                          (and (string-search "precreate-drained-on-focus" text)
                               (string-search "reason=focus-edge" text)))
                        logged)))))

(ert-deftest agent-repl-test-wr-an-empty-queue-tick-records-no-pass ()
  "Nothing owed is not a pass; a tick on an empty queue must stay silent."
  ;; Arrange
  (agent-repl-test-wr--with-queue
    (let ((logged nil))
      (cl-letf (((symbol-function 'agent-repl--info)
                 (lambda (_ws fmt &rest args) (push (apply #'format fmt args) logged))))
        ;; Act
        (agent-repl-test-wr--focused
          (agent-repl--webview-precreate-drain)))
      ;; Assert
      (should (null logged)))))

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

;;;; ---- The roster-update edge ----

(ert-deftest agent-repl-test-wr-roster-update-queues-newly-registered ()
  "A cold start's workspaces arrive on the roster push, not at link-up."
  ;; Arrange
  (agent-repl-test-wr--with-queue
    (agent-repl-test-wr--eligible '("alpha" "beta")
      ;; Act
      (agent-repl--webview-precreate-on-roster-update 'roster)
      ;; Assert
      (should (equal agent-repl--webview-precreate-queue '("alpha" "beta"))))))

(ert-deftest agent-repl-test-wr-roster-update-second-identical-push-queues-nothing ()
  "A repeated push owes no new work: the first already queued them."
  ;; Arrange
  (agent-repl-test-wr--with-queue
    (agent-repl-test-wr--eligible '("alpha" "beta")
      (agent-repl--webview-precreate-on-roster-update 'roster)
      ;; Act
      (let ((before (copy-sequence agent-repl--webview-precreate-queue)))
        (should (= 0 (agent-repl-webview-precreate-all)))
        ;; Assert
        (should (equal agent-repl--webview-precreate-queue before))))))

(ert-deftest agent-repl-test-wr-roster-update-skips-an-already-mounted-workspace ()
  "A workspace whose page is mounted is refused, so the push never queues it."
  ;; Arrange
  (agent-repl-test-wr--with-queue
    ;; alpha is eligible; beta is not (its page is already mounted).
    (agent-repl-test-wr--eligible '("alpha")
      (cl-letf (((symbol-function 'agent-repl--live-ws-names)
                 (lambda () '("alpha" "beta"))))
        ;; Act
        (agent-repl--webview-precreate-on-roster-update 'roster)
        ;; Assert
        (should (equal agent-repl--webview-precreate-queue '("alpha")))))))

(ert-deftest agent-repl-test-wr-roster-update-is-registered-on-the-roster-hook ()
  "The pre-creation also rides every accepted roster push."
  ;; Act / Assert
  (should (memq #'agent-repl--webview-precreate-on-roster-update
                agent-repl-roster-update-functions)))

;;; test-webview-recovery.el ends here

;;;; ---- The workspace the user stands on ----

(ert-deftest agent-repl-test-wr-the-current-workspace-is-created-first ()
  "The workspace the user stands on jumps the queue: its panels wait on it."
  ;; Arrange
  (agent-repl-test-wr--with-queue
    (agent-repl-test-wr--eligible '("alpha" "beta" "current")
      (cl-letf (((symbol-function 'agent-repl--ws-current-name) (lambda () "current"))
                ((symbol-function 'agent-repl--panels-on-view-created) #'ignore))
        (setq agent-repl--webview-precreate-queue (list "alpha" "beta" "current"))
        ;; Act
        (agent-repl--webview-precreate-tick)
        ;; Assert
        (should (equal agent-repl-test-wr--mounted '("current")))
        (should (equal agent-repl--webview-precreate-queue '("alpha" "beta")))))))

(ert-deftest agent-repl-test-wr-the-queue-drains-in-order-without-the-current-workspace ()
  "With the current workspace not queued, the queue drains in order."
  ;; Arrange
  (agent-repl-test-wr--with-queue
    (agent-repl-test-wr--eligible '("alpha" "beta")
      (cl-letf (((symbol-function 'agent-repl--ws-current-name) (lambda () "elsewhere"))
                ((symbol-function 'agent-repl--panels-on-view-created) #'ignore))
        (setq agent-repl--webview-precreate-queue (list "alpha" "beta"))
        ;; Act
        (agent-repl--webview-precreate-tick)
        ;; Assert
        (should (equal agent-repl-test-wr--mounted '("alpha")))))))

(ert-deftest agent-repl-test-wr-a-created-view-is-announced-to-the-panels ()
  "Each created view is handed to the panels, whose restore may await it."
  ;; Arrange
  (agent-repl-test-wr--with-queue
    (agent-repl-test-wr--eligible '("alpha")
      (let (announced)
        (cl-letf (((symbol-function 'agent-repl--ws-current-name) (lambda () "alpha"))
                  ((symbol-function 'agent-repl--panels-on-view-created)
                   (lambda (ws) (push ws announced))))
          (setq agent-repl--webview-precreate-queue (list "alpha"))
          ;; Act
          (agent-repl--webview-precreate-tick)
          ;; Assert
          (should (equal announced '("alpha"))))))))
