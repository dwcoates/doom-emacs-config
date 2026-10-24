;;; test-notifications.el --- Tests for agent-repl notifications -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for notifications.el -- Emacs's three parts in the daemon's desktop
;; notifications: reporting whether Emacs is focused, selecting the workspace
;; a focus gain's claim names, and selecting the workspace a clicked banner
;; names.

;;; Code:

(load (expand-file-name "test-helpers.el" (file-name-directory
                                            (or load-file-name buffer-file-name)))
      nil t)

;;;; ---- The recorder ----

(defmacro agent-repl-test-notifications--recording (&rest body)
  "Run BODY with the switch request and every log level recorded.
`switches' collects (WS . TRIGGER), `records' each (LEVEL . LINE), LEVEL
one of `debug', `info', `warn' and `error'."
  `(let ((switches nil)
         (records nil))
     (cl-letf (((symbol-function 'agent-repl-host-request-switch)
                (lambda (ws trigger) (push (cons ws trigger) switches) t))
               ((symbol-function 'agent-repl--log)
                (lambda (_scope fmt &rest args) (push (cons 'debug (apply #'format fmt args)) records)))
               ((symbol-function 'agent-repl--info)
                (lambda (_scope fmt &rest args) (push (cons 'info (apply #'format fmt args)) records)))
               ((symbol-function 'agent-repl--warn)
                (lambda (_scope fmt &rest args) (push (cons 'warn (apply #'format fmt args)) records)))
               ((symbol-function 'agent-repl--error)
                (lambda (_scope fmt &rest args) (push (cons 'error (apply #'format fmt args)) records))))
       ,@body)))

(defun agent-repl-test-notifications--recorded-p (records level pattern)
  "Non-nil when RECORDS holds a LEVEL line matching PATTERN."
  (seq-some (lambda (record) (and (eq (car record) level) (string-match-p pattern (cdr record))))
            records))

(ert-deftest agent-repl-test-notifications-recorder-records-each-level ()
  "The recorder files each log level's formatted line under its level."
  (agent-repl-test-notifications--recording
   (agent-repl--log 'scope "d %s" 1)
   (agent-repl--info 'scope "i %s" 2)
   (agent-repl--warn 'scope "w %s" 3)
   (agent-repl--error 'scope "e %s" 4)
   (should (equal records '((error . "e 4") (warn . "w 3") (info . "i 2") (debug . "d 1"))))))

(ert-deftest agent-repl-test-notifications-recorder-records-switches ()
  "The recorder records each switch request instead of making it."
  (agent-repl-test-notifications--recording
   (should (agent-repl-host-request-switch "ws" 'trigger))
   (should (equal switches '(("ws" . trigger))))))

(ert-deftest agent-repl-test-notifications-recorded-p-matches-level-and-pattern ()
  "A line matches only under its own level."
  (let ((records '((info . "claim none"))))
    (should (agent-repl-test-notifications--recorded-p records 'info "none"))
    (should-not (agent-repl-test-notifications--recorded-p records 'error "none"))))

(ert-deftest agent-repl-test-notifications-no-test-hand-rolls-a-log-capture ()
  "Every log capture in this file goes through the recorder."
  (with-temp-buffer
    (insert-file-contents (expand-file-name "test-notifications.el" agent-repl-test--module-dir))
    (should (= (how-many (concat "symbol-function '" "agent-repl--\\(info\\|warn\\|error\\|log\\)\\_>")) 4))))

;;;; ---- Tests: reporting the focus ----

(defmacro agent-repl-test-notifications--with-reporter (focused conn &rest body)
  "Run BODY with the focus reporter's state fresh, Emacs FOCUSED, and CONN live.
Every `ReportEditorFocus' call is recorded into `calls' as (CONN . REQUEST)
with its callbacks in `answers', so a test answers each one itself."
  (declare (indent 2))
  `(let ((agent-repl--focus-serial (agent-repl-serial-create))
         (agent-repl--focus-report-timer nil)
         (focused-now ,focused)
         (calls nil)
         (answers nil))
     (cl-letf (((symbol-function 'agent-repl-link-live) (lambda () ,conn))
               ((symbol-function 'agent-repl--emacs-focused-p) (lambda (&optional _ws) focused-now))
               ((symbol-function 'agent-repl-rpc-report-editor-focus)
                (lambda (c request &rest keys)
                  (push (cons c request) calls)
                  (push keys answers))))
       ,@body)))

(defun agent-repl-test-notifications--success (claim)
  "Answer a focus report's success carrying the CLAIM oneof."
  (list :arm :success :value (list :claim claim)))

(defconst agent-repl-test-notifications--no-gain
  (agent-repl-test-notifications--success '(:arm :no-gain :value nil))
  "The success a report that was no focus gain is answered with.")

(defun agent-repl-test-notifications--banner (id)
  "Answer the success of a focus gain that claimed workspace ID's banner."
  (agent-repl-test-notifications--success
   (list :arm :banner :value (list :workspace (list :id id :dir "/work/x")))))

(defun agent-repl-test-notifications--answer (keys response)
  "Answer the recorded call whose callbacks are KEYS with RESPONSE."
  (funcall (plist-get keys :on-response) response))

(ert-deftest agent-repl-test-focus-report-sends-the-current-focus ()
  "A report tells the live daemon whether Emacs is focused now."
  (agent-repl-test-notifications--with-reporter t 'conn
    (agent-repl--focus-report)
    (should (equal calls '((conn . (:focus (:arm :focused))))))))

(ert-deftest agent-repl-test-focus-report-sends-unfocused ()
  "An unfocused Emacs reports the unfocused arm."
  (agent-repl-test-notifications--with-reporter nil 'conn
    (agent-repl--focus-report)
    (should (equal calls '((conn . (:focus (:arm :unfocused))))))))

(ert-deftest agent-repl-test-focus-report-with-no-link-sends-nothing ()
  "With no link standing there is nothing to tell: the next stream states it."
  (agent-repl-test-notifications--with-reporter t nil
    (agent-repl--focus-report)
    (should-not calls)
    (should-not (agent-repl-serial-in-flight agent-repl--focus-serial))))

(ert-deftest agent-repl-test-focus-report-in-flight-owes-the-next ()
  "A change while a report is in flight sends nothing yet and is owed."
  (agent-repl-test-notifications--with-reporter t 'conn
    (agent-repl--focus-report)
    (agent-repl--focus-report)
    (should (= (length calls) 1))
    (should (agent-repl-serial-owed agent-repl--focus-serial))))

(ert-deftest agent-repl-test-focus-report-sends-the-owed-current-focus-when-answered ()
  "Once the in-flight report is answered, the owed one sends the focus held NOW."
  (agent-repl-test-notifications--with-reporter t 'conn
    (agent-repl--focus-report)
    (setq focused-now nil)
    (agent-repl--focus-report)
    (agent-repl-test-notifications--answer (car answers) agent-repl-test-notifications--no-gain)
    (should (equal (car calls) '(conn . (:focus (:arm :unfocused)))))
    (should (= (length calls) 2))))

(ert-deftest agent-repl-test-focus-report-answered-with-nothing-owed-ends ()
  "An answered report with nothing owed leaves no report in flight."
  (agent-repl-test-notifications--with-reporter t 'conn
    (agent-repl--focus-report)
    (agent-repl-test-notifications--answer (car answers) agent-repl-test-notifications--no-gain)
    (should-not (agent-repl-serial-in-flight agent-repl--focus-serial))
    (should (= (length calls) 1))))

(ert-deftest agent-repl-test-focus-report-refusal-is-recorded-at-info ()
  "A no_emacs_stream refusal is the ordinary reconnect gap, recorded at INFO."
  (agent-repl-test-notifications--with-reporter t 'conn
    (agent-repl-test-notifications--recording
     (agent-repl--focus-report)
     (agent-repl-test-notifications--answer
      (car answers) '(:arm :error :value (:cause (:arm :no-emacs-stream :value nil))))
     (should (agent-repl-test-notifications--recorded-p records 'info "focus-report refused focused=t"))
     (should-not (agent-repl-serial-in-flight agent-repl--focus-serial)))))

(ert-deftest agent-repl-test-focus-report-transport-failure-is-an-error ()
  "A transport failure is recorded at ERROR with its detail, and the report ends."
  (agent-repl-test-notifications--with-reporter t 'conn
    (agent-repl-test-notifications--recording
     (agent-repl--focus-report)
     (funcall (plist-get (car answers) :on-failure) '(:kind :connect :message "refused"))
     (should (agent-repl-test-notifications--recorded-p records 'error "focus-report failed focused=t detail=.*refused"))
     (should-not (agent-repl-serial-in-flight agent-repl--focus-serial)))))

(ert-deftest agent-repl-test-focus-report-unknown-arm-is-an-error ()
  "An answer arm the reporter does not know is recorded at ERROR."
  (agent-repl-test-notifications--with-reporter t 'conn
    (agent-repl-test-notifications--recording
     (agent-repl--focus-report)
     (agent-repl-test-notifications--answer (car answers) '(:arm :mystery :value nil))
     (should (agent-repl-test-notifications--recorded-p records 'error "focus-report unknown-arm")))))

(ert-deftest agent-repl-test-focus-changed-coalesces-into-one-timer ()
  "Two focus changes in one command loop schedule ONE report."
  (agent-repl-test-notifications--with-reporter t 'conn
    (let ((scheduled 0))
      (cl-letf (((symbol-function 'run-at-time)
                 (lambda (&rest _) (cl-incf scheduled) (timer-create))))
        (agent-repl--focus-changed)
        (agent-repl--focus-changed))
      (should (= scheduled 1)))))

(ert-deftest agent-repl-test-focus-changed-runs-the-settled-hook-once-settled ()
  "The coalesced timer runs `agent-repl-focus-settled-hook' and clears itself."
  (agent-repl-test-notifications--with-reporter t 'conn
    (let ((agent-repl-focus-settled-hook nil)
          (ran 0)
          (fire nil))
      (add-hook 'agent-repl-focus-settled-hook (lambda () (cl-incf ran)))
      (cl-letf (((symbol-function 'run-at-time)
                 (lambda (_time _repeat fn) (setq fire fn) (timer-create))))
        (agent-repl--focus-changed))
      ;; Act
      (funcall fire)
      ;; Assert
      (should (= ran 1))
      (should (null agent-repl--focus-report-timer)))))

(ert-deftest agent-repl-test-focus-report-on-link-reports ()
  "A daemon that becomes the live one is told the focus Emacs holds now."
  (agent-repl-test-notifications--with-reporter t 'conn
    (agent-repl--focus-report-on-link 'old 'new)
    (should (equal calls '((conn . (:focus (:arm :focused))))))))

(ert-deftest agent-repl-test-focus-reporter-rides-the-link-hooks ()
  "The reporter is on the link-up and promotion hooks, and on focus changes."
  (should (memq #'agent-repl--focus-report-on-link agent-repl-link-up-functions))
  (should (memq #'agent-repl--focus-report-on-link agent-repl-link-promote-functions))
  (should (advice-function-member-p #'agent-repl--focus-changed after-focus-change-function))
  (should (memq #'agent-repl--focus-report agent-repl-focus-settled-hook)))

;;;; ---- Tests: the claim a focus gain makes ----

(ert-deftest agent-repl-test-focus-gain-claim-selects-the-banners-tab ()
  "A report answered with a claimed banner requests the switch to its tab."
  (agent-repl-test-notifications--with-reporter t 'conn
    (agent-repl-test-notifications--recording
     (cl-letf (((symbol-function 'agent-repl--ws-by-ref-id)
                (lambda (id) (and (equal id "id-1") "tab-1"))))
       (agent-repl--focus-report)
       (agent-repl-test-notifications--answer (car answers) (agent-repl-test-notifications--banner "id-1")))
     (should (equal switches '(("tab-1" . notification-focus))))
     (should (agent-repl-test-notifications--recorded-p records 'info "claim banner ws=tab-1 id=id-1"))
     (should-not (agent-repl-serial-in-flight agent-repl--focus-serial)))))

(ert-deftest agent-repl-test-focus-gain-claim-is-one-call ()
  "The switch a claimed banner asks for needs no call beyond the report."
  (agent-repl-test-notifications--with-reporter t 'conn
    (agent-repl-test-notifications--recording
     (cl-letf (((symbol-function 'agent-repl--ws-by-ref-id) (lambda (_id) "tab-1")))
       (agent-repl--focus-report)
       (agent-repl-test-notifications--answer (car answers) (agent-repl-test-notifications--banner "id-1")))
     (should (= (length calls) 1))
     (should (= (length switches) 1)))))

(ert-deftest agent-repl-test-focus-gain-claim-without-a-tab-moves-nothing ()
  "A claimed banner whose workspace has no tab here moves the user nowhere."
  (agent-repl-test-notifications--recording
   (cl-letf (((symbol-function 'agent-repl--ws-by-ref-id) (lambda (_id) nil)))
     (agent-repl--focus-claim-answered t (plist-get (plist-get (agent-repl-test-notifications--banner "id-gone") :value) :claim)))
   (should-not switches)
   (should (agent-repl-test-notifications--recorded-p records 'info "claim banner-without-tab id=id-gone"))))

(ert-deftest agent-repl-test-focus-gain-with-nothing-held-moves-nothing ()
  "A gain with no banner held leaves the user where they are."
  (agent-repl-test-notifications--recording
   (agent-repl--focus-claim-answered t '(:arm :nothing-held :value nil))
   (should-not switches)
   (should (agent-repl-test-notifications--recorded-p records 'debug "focus-report ok focused=t claim=nothing-held"))))

(ert-deftest agent-repl-test-focus-report-that-is-no-gain-moves-nothing ()
  "A report that was no gain moves nothing."
  (agent-repl-test-notifications--recording
   (agent-repl--focus-claim-answered nil '(:arm :no-gain :value nil))
   (should-not switches)
   (should (agent-repl-test-notifications--recorded-p records 'debug "focus-report ok focused=nil claim=no-gain"))))

(ert-deftest agent-repl-test-focus-claim-unknown-arm-is-an-error ()
  "A claim arm Emacs does not know is recorded at ERROR and moves nothing."
  (agent-repl-test-notifications--recording
   (agent-repl--focus-claim-answered t '(:arm :mystery :value nil))
   (should-not switches)
   (should (agent-repl-test-notifications--recorded-p records 'error "claim unknown-arm focused=t arm=:mystery"))))

(ert-deftest agent-repl-test-refused-focus-report-moves-nothing ()
  "A focus the daemon refused claimed nothing, so nothing moves."
  (agent-repl-test-notifications--with-reporter t 'conn
    (agent-repl-test-notifications--recording
     (agent-repl--focus-report)
     (agent-repl-test-notifications--answer
      (car answers) '(:arm :error :value (:cause (:arm :no-emacs-stream :value nil))))
     (should-not switches))))

;;;; ---- Tests: agent-repl--notification-activate ----

(defmacro agent-repl-test-notifications--activating (&rest body)
  "Run BODY recording, with the selected frame `frame' and its focusing
recorded into `focused'."
  `(agent-repl-test-notifications--recording
    (let ((focused nil))
      (cl-letf (((symbol-function 'selected-frame) (lambda () 'frame))
                ((symbol-function 'select-frame-set-input-focus)
                 (lambda (frame) (setq focused frame))))
        ,@body))))

(ert-deftest agent-repl-test-activate-requests-the-workspace ()
  "A banner click REQUESTS the workspace's selection of the daemon.
Like every switch trigger, the frame follows the roster's `current'."
  (agent-repl-test-notifications--activating
   (agent-repl--notification-activate "ws-a")
   (should (equal switches '(("ws-a" . notification))))))

(ert-deftest agent-repl-test-activate-moves-no-frame-itself ()
  "A banner click never switches the frame locally."
  (agent-repl-test-notifications--activating
   (let (jumped)
     (cl-letf (((symbol-function 'agent-repl--ws-switch)
                (lambda (ws &rest _) (setq jumped ws))))
       (agent-repl--notification-activate "ws-a")
       (should-not jumped)))))

(ert-deftest agent-repl-test-activate-focuses-selected-frame ()
  "Activate should focus the selected frame so Emacs comes forward."
  (agent-repl-test-notifications--activating
   (agent-repl--notification-activate "ws-a")
   (should (eq focused 'frame))))

(ert-deftest agent-repl-test-activate-nil-ws-focuses-without-jump ()
  "Activate with a nil WS should focus Emacs but not attempt a jump."
  (agent-repl-test-notifications--activating
   (agent-repl--notification-activate nil)
   (should-not switches)
   (should focused)))

(ert-deftest agent-repl-test-activate-unknown-ws-warns ()
  "An activation naming no workspace is a WARNING, not a silent no-op:
the click happened and went nowhere, which is worth the record."
  (agent-repl-test-notifications--activating
   (agent-repl--notification-activate "")
   (should-not switches)
   (should (agent-repl-test-notifications--recorded-p records 'warn "activate-unknown-workspace"))))

;;;; ---- Tests: agent-repl--emacs-focused-p ----

(ert-deftest agent-repl-test-emacs-focused-p-with-no-workspace-records-centrally ()
  "With no WS, the focus observation names the CENTRAL scope explicitly.
Desktop focus is a global fact.  Passing nil down the workspace-owned
rung with nothing else naming a workspace earns
`elisp.core.log-routing-error' at ERROR -- correctly, since missing
attribution at a call site is a defect no directory can supply -- so this
call site states the central scope instead of leaving one missing."
  ;; Arrange.
  (let ((scopes nil))
    (cl-letf (((symbol-function 'agent-repl--log-verbose)
               (lambda (ws &rest _) (push ws scopes))))
      ;; Act.
      (let ((noninteractive t))
        (agent-repl--emacs-focused-p))
      ;; Assert.
      (should (equal scopes (list agent-repl--global-log-scope))))))

(ert-deftest agent-repl-test-emacs-focused-p-with-a-workspace-records-on-it ()
  "With a WS, the focus observation is attributed to that workspace."
  ;; Arrange.
  (let ((scopes nil))
    (cl-letf (((symbol-function 'agent-repl--log-verbose)
               (lambda (ws &rest _) (push ws scopes))))
      ;; Act.
      (let ((noninteractive t))
        (agent-repl--emacs-focused-p "ws1"))
      ;; Assert.
      (should (equal scopes (list "ws1"))))))

(ert-deftest agent-repl-test-emacs-focused-p-nil-in-batch ()
  "Under `noninteractive', emacs-focused-p returns nil without scanning frames."
  (cl-letf (((symbol-function 'frame-list)
             (lambda () (error "frame-list must not run under noninteractive"))))
    (let ((noninteractive t))
      (should-not (agent-repl--emacs-focused-p)))))

(ert-deftest agent-repl-test-emacs-focused-p-true-when-frame-focused ()
  "emacs-focused-p is non-nil when a frame holds OS focus."
  (cl-letf (((symbol-function 'frame-list) (lambda () (list 'f1)))
            ((symbol-function 'frame-focus-state) (lambda (_frame) t)))
    (let ((noninteractive nil))
      (should (agent-repl--emacs-focused-p)))))

(ert-deftest agent-repl-test-emacs-focused-p-false-when-no-frame-focused ()
  "emacs-focused-p is nil when every frame reports no focus."
  (cl-letf (((symbol-function 'frame-list) (lambda () (list 'f1 'f2)))
            ((symbol-function 'frame-focus-state) (lambda (_frame) nil)))
    (let ((noninteractive nil))
      (should-not (agent-repl--emacs-focused-p)))))

(ert-deftest agent-repl-test-emacs-focused-p-scans-all-frames ()
  "emacs-focused-p is non-nil when a non-selected background frame is focused."
  (cl-letf (((symbol-function 'frame-list) (lambda () (list 'f1 'f2)))
            ((symbol-function 'frame-focus-state)
             (lambda (frame) (when (eq frame 'f2) t))))
    (let ((noninteractive nil))
      (should (agent-repl--emacs-focused-p)))))

(ert-deftest agent-repl-test-emacs-focused-p-unknown-counts-as-focused ()
  "A frame whose focus is `unknown' counts as focused (conservative suppress)."
  (cl-letf (((symbol-function 'frame-list) (lambda () (list 'f1)))
            ((symbol-function 'frame-focus-state) (lambda (_frame) 'unknown)))
    (let ((noninteractive nil))
      (should (agent-repl--emacs-focused-p)))))

(provide 'test-notifications)

;;; test-notifications.el ends here
