;;; test-rpc.el --- ERT tests for agent-repl rpc.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Run with:
;;   AGENT_REPL_FORBID_VENDOR_CALLS=1 emacs -batch -Q -l ert \
;;     -l lisp/test-rpc.el -f ert-run-tests-batch-and-exit
;;
;; rpc.el is a seam and is tested as one: BOTH of its neighbours are stubbed.
;; The transport (`agent-repl-connect-unary' / `-unary-sync' / `-stream') is
;; replaced so nothing is spawned — connect.el's own suite owns that
;; behavior — and the codec (`agent-repl-wire-encode-*' /
;; `agent-repl-wire-decode-*') is replaced because it is written
;; concurrently in another worktree.  What is asserted here is exactly what
;; rpc.el owns: that each rpc names the right method, pairs the right
;; encoder with the right decoder, hands the decoded answer to the right
;; callback, and keeps a stream alive across an undecodable push.

;;; Code:

(load (expand-file-name "test-helpers.el" (file-name-directory
                                           (or load-file-name buffer-file-name)))
      nil t)

;; wire-common.el owns this error; it is being written concurrently, so
;; define it here when it has not landed yet.  The tests below need a
;; codec-shaped failure to raise, and the codec's failure IS this symbol.
(unless (get 'agent-repl-wire-error 'error-conditions)
  (define-error 'agent-repl-wire-error "agent-repl wire codec failure"))

;;;; ---- Harness ----

(defvar agent-repl-test-rpc--logs nil
  "List of `(LEVEL . TEXT)' entries, newest first, captured from the ladder.")

(defun agent-repl-test-rpc--record-log (level fmt args)
  "Push a `(LEVEL . TEXT)' entry built from FMT and ARGS onto the log capture."
  (push (cons level (condition-case nil (apply #'format fmt args)
                      (error (format "%S %S" fmt args))))
        agent-repl-test-rpc--logs))

(defun agent-repl-test-rpc--logs-matching (level regexp)
  "Return every captured LEVEL entry whose text matches REGEXP."
  (seq-filter (lambda (entry)
                (and (eq (car entry) level)
                     (string-match-p regexp (cdr entry))))
              agent-repl-test-rpc--logs))

(defmacro agent-repl-test-rpc--with-logs (&rest body)
  "Run BODY with the logging ladder captured instead of exercised.
`agent-repl--error' is stubbed because it signals in production, which
would abort the very branch a test is asserting."
  (declare (indent 0))
  `(let ((agent-repl-test-rpc--logs nil))
     (cl-letf (((symbol-function 'agent-repl--log)
                (lambda (_ws fmt &rest args) (agent-repl-test-rpc--record-log 'debug fmt args)))
               ((symbol-function 'agent-repl--info)
                (lambda (_ws fmt &rest args) (agent-repl-test-rpc--record-log 'info fmt args)))
               ((symbol-function 'agent-repl--warn)
                (lambda (_ws fmt &rest args) (agent-repl-test-rpc--record-log 'warn fmt args)))
               ((symbol-function 'agent-repl--error)
                (lambda (_ws fmt &rest args) (agent-repl-test-rpc--record-log 'error fmt args))))
       ,@body)))

(defconst agent-repl-test-rpc--verbs
  '(("RegisterWorkspace" agent-repl-rpc-register-workspace
     agent-repl-wire-encode-register-workspace-request
     agent-repl-wire-decode-register-workspace-response)
    ("SelectWorkspace" agent-repl-rpc-select-workspace
     agent-repl-wire-encode-select-workspace-request
     agent-repl-wire-decode-select-workspace-response)
    ("MarkWorkspaceViewed" agent-repl-rpc-mark-workspace-viewed
     agent-repl-wire-encode-mark-workspace-viewed-request
     agent-repl-wire-decode-mark-workspace-viewed-response)
    ("AdoptHostWorkspace" agent-repl-rpc-adopt-host-workspace
     agent-repl-wire-encode-adopt-host-workspace-request
     agent-repl-wire-decode-adopt-host-workspace-response)
    ("CreateWorkspace" agent-repl-rpc-create-workspace
     agent-repl-wire-encode-create-workspace-request
     agent-repl-wire-decode-create-workspace-response)
    ("RegisterRepository" agent-repl-rpc-register-repository
     agent-repl-wire-encode-register-repository-request
     agent-repl-wire-decode-register-repository-response)
    ("OpenWorkspace" agent-repl-rpc-open-workspace
     agent-repl-wire-encode-open-workspace-request
     agent-repl-wire-decode-open-workspace-response)
    ("CloseWorkspace" agent-repl-rpc-close-workspace
     agent-repl-wire-encode-close-workspace-request
     agent-repl-wire-decode-close-workspace-response)
    ("KillWorkspace" agent-repl-rpc-kill-workspace
     agent-repl-wire-encode-kill-workspace-request
     agent-repl-wire-decode-kill-workspace-response)
    ("NukeWorkspace" agent-repl-rpc-nuke-workspace
     agent-repl-wire-encode-nuke-workspace-request
     agent-repl-wire-decode-nuke-workspace-response)
    ("MergeWorkspace" agent-repl-rpc-merge-workspace
     agent-repl-wire-encode-merge-workspace-request
     agent-repl-wire-decode-merge-workspace-response)
    ("RestartWorkspace" agent-repl-rpc-restart-workspace
     agent-repl-wire-encode-restart-workspace-request
     agent-repl-wire-decode-restart-workspace-response)
    ("Interrupt" agent-repl-rpc-interrupt
     agent-repl-wire-encode-interrupt-request
     agent-repl-wire-decode-interrupt-response)
    ("SetWorkspacePriority" agent-repl-rpc-set-workspace-priority
     agent-repl-wire-encode-set-workspace-priority-request
     agent-repl-wire-decode-set-workspace-priority-response)
    ("SubmitPrompt" agent-repl-rpc-submit-prompt
     agent-repl-wire-encode-submit-prompt-request
     agent-repl-wire-decode-submit-prompt-response)
    ("SelectResponse" agent-repl-rpc-select-response
     agent-repl-wire-encode-select-response-request
     agent-repl-wire-decode-select-response-response)
    ("AdjustFeedTextScale" agent-repl-rpc-adjust-feed-text-scale
     agent-repl-wire-encode-adjust-feed-text-scale-request
     agent-repl-wire-decode-adjust-feed-text-scale-response)
    ("Deploy" agent-repl-rpc-deploy
     agent-repl-wire-encode-deploy-request
     agent-repl-wire-decode-deploy-response)
    ("UpdateShutdownSchedule" agent-repl-rpc-update-shutdown-schedule
     agent-repl-wire-encode-update-shutdown-schedule-request
     agent-repl-wire-decode-update-shutdown-schedule-response)
    ("UpdateMergeQueue" agent-repl-rpc-update-merge-queue
     agent-repl-wire-encode-update-merge-queue-request
     agent-repl-wire-decode-update-merge-queue-response)
    ("DaemonHealth" agent-repl-rpc-daemon-health
     agent-repl-wire-encode-daemon-health-request
     agent-repl-wire-decode-daemon-health-response)
    ("SessionHealth" agent-repl-rpc-session-health
     agent-repl-wire-encode-session-health-request
     agent-repl-wire-decode-session-health-response))
  "Table of `(METHOD ASYNC-FN ENCODER DECODER)' for every unary rpc Emacs calls.
This IS the surface `rpc.el' owes the rest of the elisp side; a verb
missing here or there is a broken seam.")

(defconst agent-repl-test-rpc--streams
  '(("WatchHostWorkspace" agent-repl-rpc-watch-host-workspace
     agent-repl-wire-encode-watch-host-workspace-request
     agent-repl-wire-decode-watch-host-workspace-response)
    ("WatchDaemon" agent-repl-rpc-watch-daemon
     agent-repl-wire-encode-watch-daemon-request
     agent-repl-wire-decode-watch-daemon-response)
    ("WatchWorkspaceRoster" agent-repl-rpc-watch-workspace-roster
     agent-repl-wire-encode-watch-workspace-roster-request
     agent-repl-wire-decode-watch-workspace-roster-response))
  "Table of `(METHOD FN ENCODER DECODER)' for every server-streaming rpc.")

(defun agent-repl-test-rpc--sync-name (fn)
  "Return the `-sync' variant symbol of async rpc function FN."
  (intern (concat (symbol-name fn) "-sync")))

;;;; ---- Tests: the surface ----

(ert-deftest agent-repl-test-rpc-defines-every-unary-verb ()
  "Every unary rpc Emacs calls has its async function."
  ;; Arrange
  (let ((missing nil))
    ;; Act
    (dolist (row agent-repl-test-rpc--verbs)
      (unless (fboundp (nth 1 row)) (push (nth 1 row) missing)))
    ;; Assert
    (should (null missing))))

(ert-deftest agent-repl-test-rpc-defines-every-sync-variant ()
  "Every unary rpc also has the blocking variant tests and the doctor use."
  ;; Arrange
  (let ((missing nil))
    ;; Act
    (dolist (row agent-repl-test-rpc--verbs)
      (let ((sync (agent-repl-test-rpc--sync-name (nth 1 row))))
        (unless (fboundp sync) (push sync missing))))
    ;; Assert
    (should (null missing))))

(ert-deftest agent-repl-test-rpc-defines-every-stream ()
  "Every server-streaming rpc Emacs subscribes to has its function."
  ;; Arrange
  (let ((missing nil))
    ;; Act
    (dolist (row agent-repl-test-rpc--streams)
      (unless (fboundp (nth 1 row)) (push (nth 1 row) missing)))
    ;; Assert
    (should (null missing))))

;;;; ---- Tests: method naming and codec pairing ----

(ert-deftest agent-repl-test-rpc-unary-callback-keeps-the-captured-log-workspace ()
  "A delayed unary answer logs against the scope captured at request send."
  ;; Arrange
  (let (transport-response logged-workspaces)
    (cl-letf (((symbol-function 'agent-repl--capture-log-scope)
               (lambda (_scope) "request-ws"))
              ((symbol-function 'agent-repl--log)
               (lambda (ws _fmt &rest _args) (push ws logged-workspaces)))
              ((symbol-function 'agent-repl-connect-unary)
               (lambda (_conn _method _json &rest keys)
                 (setq transport-response (plist-get keys :on-response))))
              ((symbol-function 'agent-repl-wire-encode-register-workspace-request)
               (lambda (_request) '((dir . "/workspace"))))
              ((symbol-function 'agent-repl-wire-decode-register-workspace-response)
               (lambda (_response) '(:arm :success :value nil))))
      ;; Act
      (agent-repl-rpc-register-workspace 'conn '(:dir "/workspace"))
      (funcall transport-response '((success . nil)))
      ;; Assert
      (should (equal logged-workspaces '("request-ws" "request-ws"))))))

(ert-deftest agent-repl-test-rpc-each-verb-sends-its-own-method-name ()
  "Each unary function targets the rpc it is named for, and no other."
  ;; Arrange
  (let ((observed nil))
    (agent-repl-test-rpc--with-logs
      (dolist (row agent-repl-test-rpc--verbs)
        (cl-destructuring-bind (method fn encoder decoder) row
          (cl-letf (((symbol-function 'agent-repl-connect-unary)
                     (lambda (_conn sent-method _json &rest _keys)
                       (push (cons method sent-method) observed)))
                    ((symbol-function encoder) (lambda (_request) nil))
                    ((symbol-function decoder) (lambda (_alist) nil)))
            ;; Act
            (funcall fn nil nil)))))
    ;; Assert
    (should (cl-every (lambda (pair) (equal (car pair) (cdr pair))) observed))
    (should (= (length observed) (length agent-repl-test-rpc--verbs)))))

(ert-deftest agent-repl-test-rpc-each-verb-uses-its-own-encoder ()
  "Each unary function serializes through the codec function for ITS request."
  ;; Arrange
  (let ((bodies nil))
    (agent-repl-test-rpc--with-logs
      (dolist (row agent-repl-test-rpc--verbs)
        (cl-destructuring-bind (method fn encoder decoder) row
          (cl-letf (((symbol-function 'agent-repl-connect-unary)
                     (lambda (_conn _method json &rest _keys) (push json bodies)))
                    ((symbol-function encoder)
                     (lambda (_request) (list (cons 'encodedBy method))))
                    ((symbol-function decoder) (lambda (_alist) nil)))
            ;; Act
            (funcall fn nil nil)))))
    ;; Assert
    (should (equal (nreverse bodies)
                   (mapcar (lambda (row) (format "{\"encodedBy\":\"%s\"}" (car row)))
                           agent-repl-test-rpc--verbs)))))

(ert-deftest agent-repl-test-rpc-each-verb-uses-its-own-decoder ()
  "Each unary function decodes the answer through the codec function for ITS response."
  ;; Arrange
  (let ((decoded nil))
    (agent-repl-test-rpc--with-logs
      (dolist (row agent-repl-test-rpc--verbs)
        (cl-destructuring-bind (method fn encoder decoder) row
          (cl-letf (((symbol-function 'agent-repl-connect-unary)
                     (lambda (_conn _method _json &rest keys)
                       (funcall (plist-get keys :on-response) '((success . nil)))))
                    ((symbol-function encoder) (lambda (_request) nil))
                    ((symbol-function decoder)
                     (lambda (_alist) (list :arm :success :value method))))
            ;; Act
            (funcall fn nil nil
                     :on-response (lambda (plist) (push (plist-get plist :value) decoded)))))))
    ;; Assert
    (should (equal (nreverse decoded) (mapcar #'car agent-repl-test-rpc--verbs)))))

(ert-deftest agent-repl-test-rpc-each-stream-sends-its-own-method-name ()
  "Each stream function subscribes to the rpc it is named for."
  ;; Arrange
  (let ((observed nil))
    (agent-repl-test-rpc--with-logs
      (dolist (row agent-repl-test-rpc--streams)
        (cl-destructuring-bind (method fn encoder decoder) row
          (cl-letf (((symbol-function 'agent-repl-connect-stream)
                     (lambda (_conn sent-method _json _on-push _on-close &optional _on-open)
                       (push (cons method sent-method) observed)))
                    ((symbol-function encoder) (lambda (_request) nil))
                    ((symbol-function decoder) (lambda (_alist) nil)))
            ;; Act
            (if (eq fn 'agent-repl-rpc-watch-host-workspace)
                (funcall fn nil '(:id "w1" :dir "/w") #'ignore #'ignore)
              (funcall fn nil #'ignore #'ignore))))))
    ;; Assert
    (should (cl-every (lambda (pair) (equal (car pair) (cdr pair))) observed))
    (should (= (length observed) (length agent-repl-test-rpc--streams)))))

;;;; ---- Tests: unary answer routing ----

(ert-deftest agent-repl-test-rpc-hands-the-decoded-response-to-on-response ()
  "A decoded success arm reaches ON-RESPONSE verbatim."
  ;; Arrange
  (agent-repl-test-rpc--with-logs
    (let ((answer nil))
      (cl-letf (((symbol-function 'agent-repl-connect-unary)
                 (lambda (_conn _method _json &rest keys)
                   (funcall (plist-get keys :on-response) '((success . ((workspace . nil)))))))
                ((symbol-function 'agent-repl-wire-encode-register-workspace-request)
                 (lambda (_r) nil))
                ((symbol-function 'agent-repl-wire-decode-register-workspace-response)
                 (lambda (_a) '(:arm :success :value (:workspace (:id "w1" :dir "/w"))))))
        ;; Act
        (agent-repl-rpc-register-workspace nil '(:dir "/w")
                                           :on-response (lambda (p) (setq answer p))))
      ;; Assert
      (should (equal answer '(:arm :success :value (:workspace (:id "w1" :dir "/w"))))))))

(ert-deftest agent-repl-test-rpc-hands-a-daemon-error-arm-to-on-response ()
  "A daemon-authored error ARM is an ANSWER, and never reaches ON-FAILURE."
  ;; Arrange
  (agent-repl-test-rpc--with-logs
    (let ((response nil)
          (failure nil))
      (cl-letf (((symbol-function 'agent-repl-connect-unary)
                 (lambda (_conn _method _json &rest keys)
                   (funcall (plist-get keys :on-response) '((error . nil)))))
                ((symbol-function 'agent-repl-wire-encode-close-workspace-request)
                 (lambda (_r) nil))
                ((symbol-function 'agent-repl-wire-decode-close-workspace-response)
                 (lambda (_a) '(:arm :error :value (:arm :blocked :value nil)))))
        ;; Act
        (agent-repl-rpc-close-workspace nil '(:workspace (:id "w1"))
                                        :on-response (lambda (p) (setq response p))
                                        :on-failure (lambda (d) (setq failure d))))
      ;; Assert
      (should (eq (plist-get response :arm) :error))
      (should (null failure)))))

(ert-deftest agent-repl-test-rpc-hands-a-transport-failure-to-on-failure ()
  "A transport failure reaches ON-FAILURE unchanged and never ON-RESPONSE."
  ;; Arrange
  (agent-repl-test-rpc--with-logs
    (let ((detail '(:kind :transport :code nil :status nil :message "no daemon"))
          (response nil)
          (failure nil))
      (cl-letf (((symbol-function 'agent-repl-connect-unary)
                 (lambda (_conn _method _json &rest keys)
                   (funcall (plist-get keys :on-failure) detail)))
                ((symbol-function 'agent-repl-wire-encode-daemon-health-request)
                 (lambda (_r) nil))
                ((symbol-function 'agent-repl-wire-decode-daemon-health-response)
                 (lambda (_a) nil)))
        ;; Act
        (agent-repl-rpc-daemon-health nil nil
                                      :on-response (lambda (p) (setq response p))
                                      :on-failure (lambda (d) (setq failure d))))
      ;; Assert
      (should (equal failure detail))
      (should (null response)))))

(ert-deftest agent-repl-test-rpc-logs-a-transport-failure-at-warning ()
  "A transport failure is recorded at WARNING with the method named."
  ;; Arrange
  (agent-repl-test-rpc--with-logs
    (cl-letf (((symbol-function 'agent-repl-connect-unary)
               (lambda (_conn _method _json &rest keys)
                 (funcall (plist-get keys :on-failure) '(:kind :transport :message "x"))))
              ((symbol-function 'agent-repl-wire-encode-daemon-health-request) (lambda (_r) nil))
              ((symbol-function 'agent-repl-wire-decode-daemon-health-response) (lambda (_a) nil)))
      ;; Act
      (agent-repl-rpc-daemon-health nil nil :on-failure #'ignore))
    ;; Assert
    (should (agent-repl-test-rpc--logs-matching 'warn "elisp\\.rpc\\.transport-failure"))))

(ert-deftest agent-repl-test-rpc-reports-an-undecodable-response-as-a-failure ()
  "A response the codec refuses becomes a failure, so no caller waits forever."
  ;; Arrange
  (agent-repl-test-rpc--with-logs
    (let ((failure nil)
          (response nil))
      (cl-letf (((symbol-function 'agent-repl-connect-unary)
                 (lambda (_conn _method _json &rest keys)
                   (funcall (plist-get keys :on-response) '((bogus . 1)))))
                ((symbol-function 'agent-repl-wire-encode-daemon-health-request) (lambda (_r) nil))
                ((symbol-function 'agent-repl-wire-decode-daemon-health-response)
                 (lambda (_a) (signal 'agent-repl-wire-error
                                      (list "DaemonHealthResponse" 'bogus 'unknown-field)))))
        ;; Act
        (agent-repl-rpc-daemon-health nil nil
                                      :on-response (lambda (p) (setq response p))
                                      :on-failure (lambda (d) (setq failure d))))
      ;; Assert
      (should (eq (plist-get failure :kind) :malformed))
      (should (null response)))))

(ert-deftest agent-repl-test-rpc-logs-an-undecodable-response-at-error ()
  "The undecodable-response branch reaches the ERROR rung with the raw body."
  ;; Arrange
  (agent-repl-test-rpc--with-logs
    (cl-letf (((symbol-function 'agent-repl-connect-unary)
               (lambda (_conn _method _json &rest keys)
                 (funcall (plist-get keys :on-response) '((bogus . 1)))))
              ((symbol-function 'agent-repl-wire-encode-daemon-health-request) (lambda (_r) nil))
              ((symbol-function 'agent-repl-wire-decode-daemon-health-response)
               (lambda (_a) (signal 'agent-repl-wire-error (list "R" 'bogus 'unknown-field)))))
      ;; Act
      (agent-repl-rpc-daemon-health nil nil :on-failure #'ignore))
    ;; Assert
    (should (agent-repl-test-rpc--logs-matching 'error "elisp\\.rpc\\.response-invalid"))
    (should (agent-repl-test-rpc--logs-matching 'error "bogus"))))

;;;; ---- Tests: the sync variants ----

(ert-deftest agent-repl-test-rpc-sync-returns-the-decoded-response ()
  "The blocking variant answers the decoded response plist."
  ;; Arrange
  (agent-repl-test-rpc--with-logs
    (cl-letf (((symbol-function 'agent-repl-connect-unary-sync)
               (lambda (_conn _method _json &optional _timeout) '((success . nil))))
              ((symbol-function 'agent-repl-wire-encode-daemon-health-request) (lambda (_r) nil))
              ((symbol-function 'agent-repl-wire-decode-daemon-health-response)
               (lambda (_a) '(:arm :success :value (:verdict :healthy)))))
      ;; Act / Assert
      (should (equal (agent-repl-rpc-daemon-health-sync nil nil)
                     '(:arm :success :value (:verdict :healthy)))))))

(ert-deftest agent-repl-test-rpc-sync-propagates-a-transport-signal ()
  "A transport failure surfaces in the synchronous caller's own stack."
  ;; Arrange
  (agent-repl-test-rpc--with-logs
    (cl-letf (((symbol-function 'agent-repl-connect-unary-sync)
               (lambda (_conn _method _json &optional _timeout)
                 (signal 'agent-repl-connect-error (list '(:kind :transport)))))
              ((symbol-function 'agent-repl-wire-encode-daemon-health-request) (lambda (_r) nil))
              ((symbol-function 'agent-repl-wire-decode-daemon-health-response) (lambda (_a) nil)))
      ;; Act / Assert
      (should-error (agent-repl-rpc-daemon-health-sync nil nil)
                    :type 'agent-repl-connect-error))))

(ert-deftest agent-repl-test-rpc-sync-forwards-the-caller-timeout ()
  "The blocking variant hands its TIMEOUT through to the transport."
  ;; Arrange
  (agent-repl-test-rpc--with-logs
    (let ((seen :unset))
      (cl-letf (((symbol-function 'agent-repl-connect-unary-sync)
                 (lambda (_conn _method _json &optional timeout)
                   (setq seen timeout)
                   '((success . nil))))
                ((symbol-function 'agent-repl-wire-encode-daemon-health-request) (lambda (_r) nil))
                ((symbol-function 'agent-repl-wire-decode-daemon-health-response) (lambda (_a) nil)))
        ;; Act
        (agent-repl-rpc-daemon-health-sync nil nil 3))
      ;; Assert
      (should (= seen 3)))))

;;;; ---- Tests: streams ----

(ert-deftest agent-repl-test-rpc-watch-host-workspace-echoes-the-ref-verbatim ()
  "The subscription request carries the ref exactly as it was handed over."
  ;; Arrange
  (agent-repl-test-rpc--with-logs
    (let ((ref '(:id "opaque-id" :dir "/some/where"))
          (encoded nil))
      (cl-letf (((symbol-function 'agent-repl-connect-stream)
                 (lambda (_conn _method _json _on-push _on-close &optional _on-open) nil))
                ((symbol-function 'agent-repl-wire-encode-watch-host-workspace-request)
                 (lambda (request) (setq encoded request) nil))
                ((symbol-function 'agent-repl-wire-decode-watch-host-workspace-response)
                 (lambda (_a) nil)))
        ;; Act
        (agent-repl-rpc-watch-host-workspace nil ref #'ignore #'ignore))
      ;; Assert
      (should (equal encoded (list :workspace ref)))
      (should (eq (plist-get encoded :workspace) ref)))))

(ert-deftest agent-repl-test-rpc-roster-stream-sends-an-empty-request ()
  "The roster subscription carries no request fields at all."
  ;; Arrange
  (agent-repl-test-rpc--with-logs
    (let ((requests nil))
      (cl-letf (((symbol-function 'agent-repl-connect-stream)
                 (lambda (_conn _method _json _on-push _on-close &optional _on-open) nil))
                ((symbol-function 'agent-repl-wire-encode-watch-workspace-roster-request)
                 (lambda (request) (push request requests) nil))
                ((symbol-function 'agent-repl-wire-decode-watch-workspace-roster-response)
                 (lambda (_a) nil)))
        ;; Act
        (agent-repl-rpc-watch-workspace-roster nil #'ignore #'ignore))
      ;; Assert
      (should (equal requests '(nil))))))

(ert-deftest agent-repl-test-rpc-daemon-stream-names-emacs-and-its-elisp-build ()
  "Every WatchDaemon Emacs opens names it as `emacs' with the elisp it loaded."
  ;; Arrange
  (agent-repl-test-rpc--with-logs
    (let ((requests nil))
      (cl-letf (((symbol-function 'agent-repl-connect-stream)
                 (lambda (_conn _method _json _on-push _on-close &optional _on-open) nil))
                ((symbol-function 'agent-repl-elisp-build) (lambda () "b-test"))
                ((symbol-function 'agent-repl-wire-encode-watch-daemon-request)
                 (lambda (request) (push request requests) nil))
                ((symbol-function 'agent-repl-wire-decode-watch-daemon-response)
                 (lambda (_a) nil)))
        ;; Act
        (agent-repl-rpc-watch-daemon nil #'ignore #'ignore))
      ;; Assert
      (should (equal requests
                     '((:client (:arm :emacs :value (:elisp-build "b-test")))))))))

(ert-deftest agent-repl-test-rpc-daemon-stream-sends-the-build-on-the-wire ()
  "The encoded WatchDaemon body carries `elispBuild' under the `emacs' arm."
  ;; Arrange
  (agent-repl-test-rpc--with-logs
    (let ((sent nil))
      (cl-letf (((symbol-function 'agent-repl-connect-stream)
                 (lambda (_conn _method json _on-push _on-close &optional _on-open)
                   (setq sent json) nil))
                ((symbol-function 'agent-repl-elisp-build) (lambda () "b-test")))
        ;; Act
        (agent-repl-rpc-watch-daemon nil #'ignore #'ignore))
      ;; Assert
      (should (equal sent "{\"emacs\":{\"elispBuild\":\"b-test\"}}")))))

(ert-deftest agent-repl-test-rpc-daemon-stream-without-a-build-never-opens ()
  "An Emacs with no elisp build to report fails loudly and opens nothing."
  ;; Arrange
  (agent-repl-test-rpc--with-logs
    (let ((opened nil)
          (agent-repl--elisp-module-builds nil))
      (cl-letf (((symbol-function 'agent-repl-connect-stream)
                 (lambda (&rest _args) (setq opened t) nil)))
        ;; Act / Assert
        (should-error (agent-repl-rpc-watch-daemon nil #'ignore #'ignore))
        (should-not opened)))))

(ert-deftest agent-repl-test-rpc-hands-the-decoded-push-to-on-push ()
  "Each push reaches ON-PUSH decoded, never as raw JSON."
  ;; Arrange
  (agent-repl-test-rpc--with-logs
    (let ((transport-push nil)
          (seen nil))
      (cl-letf (((symbol-function 'agent-repl-connect-stream)
                 (lambda (_conn _method _json on-push _on-close &optional _on-open)
                   (setq transport-push on-push) nil))
                ((symbol-function 'agent-repl-wire-encode-watch-daemon-request) (lambda (_r) nil))
                ((symbol-function 'agent-repl-wire-decode-watch-daemon-response)
                 (lambda (alist) (list :arm :drain-scheduled :value (alist-get 'atMs alist)))))
        (agent-repl-rpc-watch-daemon nil (lambda (p) (push p seen)) #'ignore)
        ;; Act
        (funcall transport-push '((atMs . 17))))
      ;; Assert
      (should (equal seen '((:arm :drain-scheduled :value 17)))))))

(ert-deftest agent-repl-test-rpc-drops-an-undecodable-push ()
  "A push the codec refuses is dropped; the next sound push still arrives."
  ;; Arrange
  (agent-repl-test-rpc--with-logs
    (let ((transport-push nil)
          (seen nil))
      (cl-letf (((symbol-function 'agent-repl-connect-stream)
                 (lambda (_conn _method _json on-push _on-close &optional _on-open)
                   (setq transport-push on-push) nil))
                ((symbol-function 'agent-repl-wire-encode-watch-daemon-request) (lambda (_r) nil))
                ((symbol-function 'agent-repl-wire-decode-watch-daemon-response)
                 (lambda (alist)
                   (if (alist-get 'bogus alist)
                       (signal 'agent-repl-wire-error
                               (list "WatchDaemonResponse" 'bogus 'unknown-field))
                     (list :arm :drain-cancelled :value nil)))))
        (agent-repl-rpc-watch-daemon nil (lambda (p) (push p seen)) #'ignore)
        ;; Act
        (funcall transport-push '((bogus . 1)))
        (funcall transport-push '((drainCancelled . nil))))
      ;; Assert
      (should (equal seen '((:arm :drain-cancelled :value nil)))))))

(ert-deftest agent-repl-test-rpc-logs-an-undecodable-push-at-error ()
  "A dropped push is recorded at ERROR as `elisp.rpc.push-invalid' with its JSON."
  ;; Arrange
  (agent-repl-test-rpc--with-logs
    (let ((transport-push nil))
      (cl-letf (((symbol-function 'agent-repl-connect-stream)
                 (lambda (_conn _method _json on-push _on-close &optional _on-open)
                   (setq transport-push on-push) nil))
                ((symbol-function 'agent-repl-wire-encode-watch-daemon-request) (lambda (_r) nil))
                ((symbol-function 'agent-repl-wire-decode-watch-daemon-response)
                 (lambda (_a) (signal 'agent-repl-wire-error (list "R" 'bogus 'unknown-field)))))
        (agent-repl-rpc-watch-daemon nil #'ignore #'ignore)
        ;; Act
        (funcall transport-push '((bogus . 1))))
      ;; Assert
      (should (agent-repl-test-rpc--logs-matching 'error "elisp\\.rpc\\.push-invalid"))
      (should (agent-repl-test-rpc--logs-matching 'error "bogus")))))

(ert-deftest agent-repl-test-rpc-passes-the-close-vocabulary-through ()
  "ON-CLOSE receives connect.el's vocabulary unchanged: cancelled, ended, error."
  ;; Arrange
  (agent-repl-test-rpc--with-logs
    (let ((transport-close nil)
          (seen nil))
      (cl-letf (((symbol-function 'agent-repl-connect-stream)
                 (lambda (_conn _method _json _on-push on-close &optional _on-open)
                   (setq transport-close on-close) nil))
                ((symbol-function 'agent-repl-wire-encode-watch-daemon-request) (lambda (_r) nil))
                ((symbol-function 'agent-repl-wire-decode-watch-daemon-response) (lambda (_a) nil)))
        (agent-repl-rpc-watch-daemon nil #'ignore (lambda (o) (push o seen)))
        ;; Act
        (dolist (outcome '((:cancelled) (:ended) (:error (:kind :transport))))
          (funcall transport-close outcome)))
      ;; Assert
      (should (equal (nreverse seen)
                     '((:cancelled) (:ended) (:error (:kind :transport))))))))


;;;; ---- Tests: stream acceptance pass-through ----

(ert-deftest agent-repl-test-rpc-every-stream-passes-on-open-to-the-transport ()
  "ON-OPEN carries no message, so every watcher hands it straight through."
  ;; Arrange
  (let ((accepted nil))
    (agent-repl-test-rpc--with-logs
      (dolist (row agent-repl-test-rpc--streams)
        (cl-destructuring-bind (method fn encoder decoder) row
          (let ((transport-open nil))
            (cl-letf (((symbol-function 'agent-repl-connect-stream)
                       (lambda (_conn _method _json _on-push _on-close &optional on-open)
                         (setq transport-open on-open)))
                      ((symbol-function encoder) (lambda (_request) nil))
                      ((symbol-function decoder) (lambda (_alist) nil)))
              (if (eq fn 'agent-repl-rpc-watch-host-workspace)
                  (funcall fn nil '(:id "w1" :dir "/w") #'ignore #'ignore
                           (lambda () (push method accepted)))
                (funcall fn nil #'ignore #'ignore
                         (lambda () (push method accepted))))
              ;; Act
              (should transport-open)
              (funcall transport-open))))))
    ;; Assert
    (should (equal (sort accepted #'string<)
                   (sort (mapcar #'car agent-repl-test-rpc--streams) #'string<)))))

(ert-deftest agent-repl-test-rpc-omitted-on-open-reaches-the-transport-as-nil ()
  "A caller that wants no acceptance callback must not get a wrapper for one."
  ;; Arrange
  (agent-repl-test-rpc--with-logs
    (let ((transport-open :unset))
      (cl-letf (((symbol-function 'agent-repl-connect-stream)
                 (lambda (_conn _method _json _on-push _on-close &optional on-open)
                   (setq transport-open on-open)))
                ((symbol-function 'agent-repl-wire-encode-watch-daemon-request)
                 (lambda (_r) nil))
                ((symbol-function 'agent-repl-wire-decode-watch-daemon-response)
                 (lambda (_a) nil)))
        ;; Act
        (agent-repl-rpc-watch-daemon nil #'ignore #'ignore))
      ;; Assert
      (should (null transport-open)))))

(ert-deftest agent-repl-test-rpc-logs-the-acceptance ()
  "An accepted subscription is on the record before its consumer reacts."
  ;; Arrange
  (agent-repl-test-rpc--with-logs
    (let ((transport-open nil))
      (cl-letf (((symbol-function 'agent-repl-connect-stream)
                 (lambda (_conn _method _json _on-push _on-close &optional on-open)
                   (setq transport-open on-open)))
                ((symbol-function 'agent-repl-wire-encode-watch-daemon-request)
                 (lambda (_r) nil))
                ((symbol-function 'agent-repl-wire-decode-watch-daemon-response)
                 (lambda (_a) nil)))
        (agent-repl-rpc-watch-daemon nil #'ignore #'ignore #'ignore)
        ;; Act
        (funcall transport-open))
      ;; Assert
      (should (agent-repl-test-rpc--logs-matching
               'info "elisp\\.rpc\\.stream-accepted method=\"WatchDaemon\"")))))

(provide 'test-rpc)

;;; test-rpc.el ends here
