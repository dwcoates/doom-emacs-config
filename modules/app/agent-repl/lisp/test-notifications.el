;;; test-notifications.el --- Tests for agent-repl notifications -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for notifications.el -- Emacs's two parts in the daemon's desktop
;; notifications: reporting whether Emacs is focused, and selecting the
;; workspace a clicked banner names.

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
with its callbacks in `answers', so a test answers each one itself.  Every
`ClaimLiveNotification' call is recorded the same way into `claims' and
`claim-answers'.  The daemon has accepted no focus yet."
  (declare (indent 2))
  `(let ((agent-repl--focus-report-in-flight nil)
         (agent-repl--focus-report-owed nil)
         (agent-repl--focus-report-timer nil)
         (agent-repl--focus-accepted nil)
         (focused-now ,focused)
         (calls nil)
         (answers nil)
         (claims nil)
         (claim-answers nil))
     (cl-letf (((symbol-function 'agent-repl-link-live) (lambda () ,conn))
               ((symbol-function 'agent-repl--emacs-focused-p) (lambda (&optional _ws) focused-now))
               ((symbol-function 'agent-repl-rpc-report-editor-focus)
                (lambda (c request &rest keys)
                  (push (cons c request) calls)
                  (push keys answers)))
               ((symbol-function 'agent-repl-rpc-claim-live-notification)
                (lambda (c request &rest keys)
                  (push (cons c request) claims)
                  (push keys claim-answers))))
       ,@body)))

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
    (should-not agent-repl--focus-report-in-flight)))

(ert-deftest agent-repl-test-focus-report-in-flight-owes-the-next ()
  "A change while a report is in flight sends nothing yet and is owed."
  (agent-repl-test-notifications--with-reporter t 'conn
    (agent-repl--focus-report)
    (agent-repl--focus-report)
    (should (= (length calls) 1))
    (should agent-repl--focus-report-owed)))

(ert-deftest agent-repl-test-focus-report-sends-the-owed-current-focus-when-answered ()
  "Once the in-flight report is answered, the owed one sends the focus held NOW."
  (agent-repl-test-notifications--with-reporter t 'conn
    (agent-repl--focus-report)
    (setq focused-now nil)
    (agent-repl--focus-report)
    (agent-repl-test-notifications--answer (car answers) '(:arm :success :value nil))
    (should (equal (car calls) '(conn . (:focus (:arm :unfocused)))))
    (should (= (length calls) 2))))

(ert-deftest agent-repl-test-focus-report-answered-with-nothing-owed-ends ()
  "An answered report with nothing owed leaves no report in flight."
  (agent-repl-test-notifications--with-reporter t 'conn
    (agent-repl--focus-report)
    (agent-repl-test-notifications--answer (car answers) '(:arm :success :value nil))
    (should-not agent-repl--focus-report-in-flight)
    (should (= (length calls) 1))))

(ert-deftest agent-repl-test-focus-report-refusal-is-recorded-at-info ()
  "A no_emacs_stream refusal is the ordinary reconnect gap, recorded at INFO."
  (agent-repl-test-notifications--with-reporter t 'conn
    (agent-repl-test-notifications--recording
     (agent-repl--focus-report)
     (agent-repl-test-notifications--answer
      (car answers) '(:arm :error :value (:cause (:arm :no-emacs-stream :value nil))))
     (should (agent-repl-test-notifications--recorded-p records 'info "focus-report refused focused=t"))
     (should-not agent-repl--focus-report-in-flight))))

(ert-deftest agent-repl-test-focus-report-transport-failure-is-an-error ()
  "A transport failure is recorded at ERROR with its detail, and the report ends."
  (agent-repl-test-notifications--with-reporter t 'conn
    (agent-repl-test-notifications--recording
     (agent-repl--focus-report)
     (funcall (plist-get (car answers) :on-failure) '(:kind :connect :message "refused"))
     (should (agent-repl-test-notifications--recorded-p records 'error "focus-report failed focused=t detail=.*refused"))
     (should-not agent-repl--focus-report-in-flight))))

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

(ert-deftest agent-repl-test-focus-report-on-link-reports ()
  "A daemon that becomes the live one is told the focus Emacs holds now."
  (agent-repl-test-notifications--with-reporter t 'conn
    (agent-repl--focus-report-on-link 'old 'new)
    (should (equal calls '((conn . (:focus (:arm :focused))))))))

(ert-deftest agent-repl-test-focus-reporter-rides-the-link-hooks ()
  "The reporter is on the link-up and promotion hooks, and on focus changes."
  (should (memq #'agent-repl--focus-report-on-link agent-repl-link-up-functions))
  (should (memq #'agent-repl--focus-report-on-link agent-repl-link-promote-functions))
  (should (advice-function-member-p #'agent-repl--focus-changed after-focus-change-function)))

;;;; ---- Tests: claiming the live banner on a focus gain ----

(defconst agent-repl-test-notifications--success '(:arm :success :value nil)
  "The empty success a focus report is answered with.")

(defun agent-repl-test-notifications--live (id)
  "Answer the claim response naming workspace ID's banner live."
  (list :arm :success
        :value (list :outcome (list :arm :live
                                    :value (list :workspace (list :id id :dir "/work/x"))))))

(ert-deftest agent-repl-test-focus-gain-claims-the-live-banner ()
  "An accepted focused report after an unfocused one claims, on the live link."
  (agent-repl-test-notifications--with-reporter t 'conn
    (agent-repl--focus-report)
    (agent-repl-test-notifications--answer (car answers) agent-repl-test-notifications--success)
    (should (equal claims '((conn . nil))))
    (should agent-repl--focus-accepted)))

(ert-deftest agent-repl-test-focus-staying-focused-claims-nothing ()
  "A focused report after an accepted focused one (frame to frame) is no gain."
  (agent-repl-test-notifications--with-reporter t 'conn
    (setq agent-repl--focus-accepted t)
    (agent-repl--focus-report)
    (agent-repl-test-notifications--answer (car answers) agent-repl-test-notifications--success)
    (should-not claims)))

(ert-deftest agent-repl-test-focus-loss-claims-nothing-and-rearms-the-gain ()
  "An accepted unfocused report claims nothing, and the next focused one gains."
  (agent-repl-test-notifications--with-reporter nil 'conn
    (setq agent-repl--focus-accepted t)
    (agent-repl--focus-report)
    (agent-repl-test-notifications--answer (car answers) agent-repl-test-notifications--success)
    (should-not claims)
    (should-not agent-repl--focus-accepted)
    (setq focused-now t)
    (agent-repl--focus-report)
    (agent-repl-test-notifications--answer (car answers) agent-repl-test-notifications--success)
    (should (= (length claims) 1))))

(ert-deftest agent-repl-test-refused-focus-report-claims-nothing ()
  "A focus the daemon refused is no accepted gain: nothing is claimed."
  (agent-repl-test-notifications--with-reporter t 'conn
    (agent-repl-test-notifications--recording
     (agent-repl--focus-report)
     (agent-repl-test-notifications--answer
      (car answers) '(:arm :error :value (:cause (:arm :no-emacs-stream :value nil)))))
    (should-not claims)
    (should-not agent-repl--focus-accepted)))

(ert-deftest agent-repl-test-claim-with-no-link-sends-nothing ()
  "With no link standing no daemon holds banners, and nothing is claimed."
  (agent-repl-test-notifications--with-reporter t nil
    (agent-repl-test-notifications--recording
     (agent-repl--claim-live-notification)
     (should-not claims)
     (should (agent-repl-test-notifications--recorded-p records 'debug "claim skipped reason=no-link")))))

(ert-deftest agent-repl-test-claim-live-selects-the-banners-tab ()
  "A live answer requests the switch to the tab whose ref id it names."
  (agent-repl-test-notifications--recording
   (cl-letf (((symbol-function 'agent-repl--ws-by-ref-id)
              (lambda (id) (and (equal id "id-1") "tab-1"))))
     (agent-repl--claim-live-notification-answered (agent-repl-test-notifications--live "id-1")))
   (should (equal switches '(("tab-1" . notification-focus))))
   (should (agent-repl-test-notifications--recorded-p records 'info "claim live ws=tab-1 id=id-1"))))

(ert-deftest agent-repl-test-claim-live-without-a-tab-moves-nothing ()
  "A live banner whose workspace has no tab here moves the user nowhere."
  (agent-repl-test-notifications--recording
   (cl-letf (((symbol-function 'agent-repl--ws-by-ref-id) (lambda (_id) nil)))
     (agent-repl--claim-live-notification-answered (agent-repl-test-notifications--live "id-gone")))
   (should-not switches)
   (should (agent-repl-test-notifications--recorded-p records 'info "claim live-without-tab id=id-gone"))))

(ert-deftest agent-repl-test-claim-none-moves-nothing ()
  "No live banner: the user stays where they are."
  (agent-repl-test-notifications--recording
   (agent-repl--claim-live-notification-answered
    '(:arm :success :value (:outcome (:arm :none :value nil))))
   (should-not switches)
   (should (agent-repl-test-notifications--recorded-p records 'debug "claim none"))))

(ert-deftest agent-repl-test-claim-refused-is-recorded-at-info ()
  "A claim the daemon refused (Emacs unfocused again) moves nothing, at INFO."
  (agent-repl-test-notifications--recording
   (agent-repl--claim-live-notification-answered
    '(:arm :error :value (:cause (:arm :editor-unfocused :value nil))))
   (should-not switches)
   (should (agent-repl-test-notifications--recorded-p records 'info "claim refused cause=.*editor-unfocused"))))

(ert-deftest agent-repl-test-claim-failure-is-an-error ()
  "A claim failed in transport or on the daemon is recorded at ERROR."
  (agent-repl-test-notifications--recording
   (agent-repl--claim-live-notification-failed '(:kind :connect :message "internal"))
   (should-not switches)
   (should (agent-repl-test-notifications--recorded-p records 'error "claim failed detail=.*internal"))))

(ert-deftest agent-repl-test-claim-unknown-arm-is-an-error ()
  "An answer arm the claim does not know is recorded at ERROR."
  (agent-repl-test-notifications--recording
   (agent-repl--claim-live-notification-answered '(:arm :mystery :value nil))
   (should-not switches)
   (should (agent-repl-test-notifications--recorded-p records 'error "claim unknown-arm arm=:mystery"))))

(ert-deftest agent-repl-test-claim-unknown-outcome-is-an-error ()
  "An outcome arm the claim does not know is recorded at ERROR."
  (agent-repl-test-notifications--recording
   (agent-repl--claim-live-notification-answered
    '(:arm :success :value (:outcome (:arm :mystery :value nil))))
   (should-not switches)
   (should (agent-repl-test-notifications--recorded-p records 'error "claim unknown-outcome arm=:mystery"))))

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
