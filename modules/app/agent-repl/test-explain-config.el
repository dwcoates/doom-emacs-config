;;; test-explain-config.el --- ERT tests for explain-config.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for the conversational, workspace-less config explainer
;; (`SPC j h c').  Two layers are covered:
;;
;;   - The DAEMON layer (session create/reuse/release, webview mount,
;;     first-turn priming vs. verbatim follow-ups).  Batch Emacs has no
;;     xwidget support, so the webview boundary wrapper
;;     (`agent-repl--frontend-make-webview-buffer') is mocked to hand
;;     back ordinary buffers.
;;
;;   - The POPUP layer (show / hide / close window placement), which is
;;     strictly per-workspace (no global visible-flag, no persp-switch
;;     re-show) and stays decoupled from other side-window UI.
;;
;; Run with:
;;   emacs -batch -Q -l ert -l test-explain-config.el -f ert-run-tests-batch-and-exit

;;; Code:

;; Special declaration for the xwidget buffer-local the ensure-webview
;; tests read; the production `(defvar xwidget-webkit-buffer-name-format)'
;; in frontend.el is file-local to that file for compilation purposes.
(defvar xwidget-webkit-buffer-name-format)

(load (expand-file-name "test-helpers.el" (file-name-directory
                                            (or load-file-name buffer-file-name)))
      nil t)

(require 'cl-lib)

;;;; ---- Helpers -----------------------------------------------------------

(defmacro agent-repl-ecfg-test--with-state (&rest body)
  "Run BODY with the explain-config module state scratch-bound.
Every global the module owns is rebound, so a test can neither see nor
leak conversation state across the suite."
  (declare (indent 0))
  `(let ((agent-repl--explain-config-session-id nil)
         (agent-repl--explain-config-webview-session-id nil)
         (agent-repl--explain-config-primed-p nil)
         (agent-repl--explain-config-replaced-window nil))
     ,@body))

(defvar agent-repl-ecfg-test--urls nil
  "URLs the mocked webview boundary was asked to mount, newest first.
Special (not lexical) so `agent-repl-test--fake-webview-factory' can
push onto it through `symbol-value'.")

;;;; ---- Preamble ------------------------------------------------------------

(ert-deftest agent-repl-ecfg-test-build-input/embeds-raw-prompt ()
  "build-input embeds the raw prompt verbatim in the rendered output."
  ;; Act
  (let ((rendered (agent-repl--explain-config-build-input
                   "what does SPC j RET do?")))
    ;; Assert
    (should (string-match-p "what does SPC j RET do\\?" rendered))))

(ert-deftest agent-repl-ecfg-test-build-input/preamble-marks-read-only ()
  "Preamble declares the entry point READ-ONLY and forbids mutation."
  ;; Act
  (let ((rendered (agent-repl--explain-config-build-input "anything")))
    ;; Assert
    (should (string-match-p "READ-ONLY" rendered))
    (should (string-match-p "MUST NOT" rendered))
    (should (string-match-p "FORBIDDEN" rendered))))

(ert-deftest agent-repl-ecfg-test-build-input/preamble-instructs-refusal-on-misuse ()
  "Preamble must instruct the model to REFUSE misuse attempts."
  ;; Act
  (let ((rendered (agent-repl--explain-config-build-input "anything")))
    ;; Assert
    (should (string-match-p "REFUSE" rendered))))

(ert-deftest agent-repl-ecfg-test-build-input/preamble-binds-whole-conversation ()
  "Preamble states the constraints bind every follow-up, not just turn one."
  ;; Act
  (let ((rendered (agent-repl--explain-config-build-input "anything")))
    ;; Assert
    (should (string-match-p "CONVERSATION" rendered))
    (should (string-match-p "follow-up" rendered))))

;;;; ---- Session defaults ------------------------------------------------------

(ert-deftest agent-repl-ecfg-test-dir/defaults-to-doom-config ()
  "Default dir resolves to `~/.config/doom' (the canonical doom config)."
  (should (equal (expand-file-name agent-repl-explain-config-dir)
                 (expand-file-name "~/.config/doom"))))

(ert-deftest agent-repl-ecfg-test-model/pins-haiku ()
  "The config-explainer pins the small/fast `haiku' model."
  (should (equal agent-repl-explain-config-model "haiku")))

(ert-deftest agent-repl-ecfg-test-permission-mode/defaults-to-bypass ()
  "Permission mode is the daemon spelling of `--dangerously-skip-permissions'."
  (should (equal agent-repl-explain-config-permission-mode "bypassPermissions")))

;;;; ---- ensure-session ---------------------------------------------------------

(ert-deftest agent-repl-ecfg-test-ensure-session/creates-in-configured-dir ()
  "A fresh session is rooted at the configured (expanded) config dir."
  ;; Arrange
  (agent-repl-ecfg-test--with-state
    (let (captured-cwd)
      (cl-letf (((symbol-function 'agent-repl--ensure-frontend-daemon) (lambda (&rest _) 'proc))
                ((symbol-function 'agent-repl--frontend-wait-ready) (lambda () t))
                ((symbol-function 'agent-repl--frontend-create-session)
                 (lambda (cwd &rest _) (setq captured-cwd cwd) "s_new")))
        ;; Act
        (agent-repl--explain-config-ensure-session)
        ;; Assert
        (should (equal captured-cwd
                       (file-name-as-directory
                        (expand-file-name agent-repl-explain-config-dir))))))))

(ert-deftest agent-repl-ecfg-test-ensure-session/pins-the-configured-model ()
  "A fresh session is created with the pinned explain-config model."
  ;; Arrange
  (agent-repl-ecfg-test--with-state
    (let (captured-model)
      (cl-letf (((symbol-function 'agent-repl--ensure-frontend-daemon) (lambda (&rest _) 'proc))
                ((symbol-function 'agent-repl--frontend-wait-ready) (lambda () t))
                ((symbol-function 'agent-repl--frontend-create-session)
                 (lambda (_cwd &optional model &rest _) (setq captured-model model) "s_new")))
        ;; Act
        (agent-repl--explain-config-ensure-session)
        ;; Assert
        (should (equal captured-model agent-repl-explain-config-model))))))

(ert-deftest agent-repl-ecfg-test-ensure-session/binds-explain-config-permission-mode ()
  "Session creation runs under the explain-config permission mode, not the
workspace frontend's."
  ;; Arrange
  (agent-repl-ecfg-test--with-state
    (let ((agent-repl-frontend-permission-mode "auto")
          captured-mode)
      (cl-letf (((symbol-function 'agent-repl--ensure-frontend-daemon) (lambda (&rest _) 'proc))
                ((symbol-function 'agent-repl--frontend-wait-ready) (lambda () t))
                ((symbol-function 'agent-repl--frontend-create-session)
                 (lambda (&rest _)
                   (setq captured-mode agent-repl-frontend-permission-mode)
                   "s_new")))
        ;; Act
        (agent-repl--explain-config-ensure-session)
        ;; Assert
        (should (equal captured-mode agent-repl-explain-config-permission-mode))))))

(ert-deftest agent-repl-ecfg-test-ensure-session/reuses-live-session ()
  "A recorded session the daemon still lists as live is reused, not recreated."
  ;; Arrange
  (agent-repl-ecfg-test--with-state
    (let ((agent-repl--explain-config-session-id "s_live")
          (created 0))
      (cl-letf (((symbol-function 'agent-repl--ensure-frontend-daemon) (lambda (&rest _) 'proc))
                ((symbol-function 'agent-repl--frontend-wait-ready) (lambda () t))
                ((symbol-function 'agent-repl--frontend-session-live-p) (lambda (_id) t))
                ((symbol-function 'agent-repl--frontend-create-session)
                 (lambda (&rest _) (cl-incf created) "s_new")))
        ;; Act
        (let ((id (agent-repl--explain-config-ensure-session)))
          ;; Assert
          (should (equal id "s_live"))
          (should (= created 0)))))))

(ert-deftest agent-repl-ecfg-test-ensure-session/recreates-dead-session ()
  "A recorded session the daemon no longer lists as live is replaced."
  ;; Arrange
  (agent-repl-ecfg-test--with-state
    (let ((agent-repl--explain-config-session-id "s_dead"))
      (cl-letf (((symbol-function 'agent-repl--ensure-frontend-daemon) (lambda (&rest _) 'proc))
                ((symbol-function 'agent-repl--frontend-wait-ready) (lambda () t))
                ((symbol-function 'agent-repl--frontend-session-live-p) (lambda (_id) nil))
                ((symbol-function 'agent-repl--frontend-create-session)
                 (lambda (&rest _) "s_new")))
        ;; Act
        (let ((id (agent-repl--explain-config-ensure-session)))
          ;; Assert
          (should (equal id "s_new"))
          (should (equal agent-repl--explain-config-session-id "s_new")))))))

(ert-deftest agent-repl-ecfg-test-ensure-session/new-session-clears-primed-flag ()
  "A new session re-arms the preamble: its first turn must carry the contract."
  ;; Arrange
  (agent-repl-ecfg-test--with-state
    (let ((agent-repl--explain-config-session-id "s_dead")
          (agent-repl--explain-config-primed-p t))
      (cl-letf (((symbol-function 'agent-repl--ensure-frontend-daemon) (lambda (&rest _) 'proc))
                ((symbol-function 'agent-repl--frontend-wait-ready) (lambda () t))
                ((symbol-function 'agent-repl--frontend-session-live-p) (lambda (_id) nil))
                ((symbol-function 'agent-repl--frontend-create-session)
                 (lambda (&rest _) "s_new")))
        ;; Act
        (agent-repl--explain-config-ensure-session)
        ;; Assert
        (should-not agent-repl--explain-config-primed-p)))))

(ert-deftest agent-repl-ecfg-test-ensure-session/errors-when-daemon-not-started ()
  "A daemon that refuses to start fails fast rather than burning the poll budget."
  ;; Arrange
  (agent-repl-ecfg-test--with-state
    (cl-letf (((symbol-function 'agent-repl--ensure-frontend-daemon) (lambda (&rest _) nil))
              ((symbol-function 'agent-repl--frontend-wait-ready)
               (lambda () (error "must not poll"))))
      ;; Act + Assert
      (should-error (agent-repl--explain-config-ensure-session) :type 'error))))

;;;; ---- release-session ----------------------------------------------------------

(ert-deftest agent-repl-ecfg-test-release-session/deletes-and-clears ()
  "Release DELETEs the daemon session and clears the binding."
  ;; Arrange
  (agent-repl-ecfg-test--with-state
    (let ((agent-repl--explain-config-session-id "s_1")
          (agent-repl--explain-config-primed-p t)
          deleted)
      (cl-letf (((symbol-function 'agent-repl--frontend-delete-session)
                 (lambda (id) (setq deleted id) t)))
        ;; Act
        (agent-repl--explain-config-release-session)
        ;; Assert
        (should (equal deleted "s_1"))
        (should-not agent-repl--explain-config-session-id)
        (should-not agent-repl--explain-config-primed-p)))))

(ert-deftest agent-repl-ecfg-test-release-session/delete-failure-still-clears-binding ()
  "A dead daemon must not abort the reset — the failure is logged, not signalled."
  ;; Arrange
  (agent-repl-ecfg-test--with-state
    (let ((agent-repl--explain-config-session-id "s_1"))
      (cl-letf (((symbol-function 'agent-repl--frontend-delete-session)
                 (lambda (_id) (error "daemon gone"))))
        ;; Act
        (agent-repl--explain-config-release-session)
        ;; Assert
        (should-not agent-repl--explain-config-session-id)))))

(ert-deftest agent-repl-ecfg-test-release-session/no-session-is-noop ()
  "Release with no bound session never touches the daemon."
  ;; Arrange
  (agent-repl-ecfg-test--with-state
    (let (deleted)
      (cl-letf (((symbol-function 'agent-repl--frontend-delete-session)
                 (lambda (_id) (setq deleted t))))
        ;; Act
        (agent-repl--explain-config-release-session)
        ;; Assert
        (should-not deleted)))))

;;;; ---- Webview ---------------------------------------------------------------------

(ert-deftest agent-repl-ecfg-test-webview-url/keeps-the-composer ()
  "The popup mounts WITHOUT `composer=0': the webapp composer is the
follow-up-question surface, since there is no Emacs input panel here."
  ;; Arrange
  (cl-letf (((symbol-function 'agent-repl--frontend-session-url)
             (lambda (id) (format "http://d/?session=%s" id))))
    ;; Act
    (let ((url (agent-repl--explain-config-webview-url "s_1")))
      ;; Assert
      (should-not (string-match-p "composer=0" url)))))

(ert-deftest agent-repl-ecfg-test-ensure-webview/mounts-at-the-session-url ()
  "A fresh webview is created against the session's webapp URL."
  ;; Arrange
  (agent-repl-ecfg-test--with-state
    (let ((agent-repl-explain-config-buffer-name " *test-ecfg-mount*")
          (agent-repl-ecfg-test--urls nil)
          (buf nil))
      (unwind-protect
          (cl-letf (((symbol-function 'agent-repl--frontend-xwidget-available-p) (lambda () t))
                    ((symbol-function 'agent-repl--frontend-session-url)
                     (lambda (id) (format "http://d/?session=%s" id)))
                    ((symbol-function 'agent-repl--frontend-make-webview-buffer)
                     (agent-repl-test--fake-webview-factory 'agent-repl-ecfg-test--urls)))
            ;; Act
            (setq buf (agent-repl--explain-config-ensure-webview "s_1"))
            ;; Assert
            (should (equal agent-repl-ecfg-test--urls '("http://d/?session=s_1"))))
        (when (buffer-live-p buf) (kill-buffer buf))))))

(ert-deftest agent-repl-ecfg-test-ensure-webview/pins-the-buffer-name ()
  "The webview buffer name is pinned so webapp title changes never rename it."
  ;; Arrange
  (agent-repl-ecfg-test--with-state
    (let ((agent-repl-explain-config-buffer-name " *test-ecfg-pin*")
          (agent-repl-ecfg-test--urls nil)
          (buf nil))
      (unwind-protect
          (cl-letf (((symbol-function 'agent-repl--frontend-xwidget-available-p) (lambda () t))
                    ((symbol-function 'agent-repl--frontend-session-url) (lambda (_id) "http://d/"))
                    ((symbol-function 'agent-repl--frontend-make-webview-buffer)
                     (agent-repl-test--fake-webview-factory 'agent-repl-ecfg-test--urls)))
            ;; Act
            (setq buf (agent-repl--explain-config-ensure-webview "s_1"))
            ;; Assert
            (should (equal (buffer-name buf) agent-repl-explain-config-buffer-name))
            (with-current-buffer buf
              (should (equal xwidget-webkit-buffer-name-format
                             agent-repl-explain-config-buffer-name))))
        (when (buffer-live-p buf) (kill-buffer buf))))))

(ert-deftest agent-repl-ecfg-test-ensure-webview/clears-the-webkit-header-line ()
  "The popup is chrome, not a browser: the `WebKit: ' header-line is dropped."
  ;; Arrange
  (agent-repl-ecfg-test--with-state
    (let ((agent-repl-explain-config-buffer-name " *test-ecfg-header*")
          (agent-repl-ecfg-test--urls nil)
          (buf nil))
      (unwind-protect
          (cl-letf (((symbol-function 'agent-repl--frontend-xwidget-available-p) (lambda () t))
                    ((symbol-function 'agent-repl--frontend-session-url) (lambda (_id) "http://d/"))
                    ((symbol-function 'agent-repl--frontend-make-webview-buffer)
                     (agent-repl-test--fake-webview-factory 'agent-repl-ecfg-test--urls)))
            ;; Act
            (setq buf (agent-repl--explain-config-ensure-webview "s_1"))
            ;; Assert
            (with-current-buffer buf
              (should-not header-line-format)))
        (when (buffer-live-p buf) (kill-buffer buf))))))

(ert-deftest agent-repl-ecfg-test-ensure-webview/reuses-webview-bound-to-same-session ()
  "A live webview already attached to the session is reused, not remounted."
  ;; Arrange
  (agent-repl-ecfg-test--with-state
    (let ((agent-repl-explain-config-buffer-name " *test-ecfg-reuse*")
          (agent-repl-ecfg-test--urls nil)
          (buf nil))
      (unwind-protect
          (cl-letf (((symbol-function 'agent-repl--frontend-xwidget-available-p) (lambda () t))
                    ((symbol-function 'agent-repl--frontend-session-url) (lambda (_id) "http://d/"))
                    ((symbol-function 'agent-repl--frontend-make-webview-buffer)
                     (agent-repl-test--fake-webview-factory 'agent-repl-ecfg-test--urls)))
            (setq buf (agent-repl--explain-config-ensure-webview "s_1"))
            ;; Act
            (let ((again (agent-repl--explain-config-ensure-webview "s_1")))
              ;; Assert
              (should (eq again buf))
              (should (= (length agent-repl-ecfg-test--urls) 1))))
        (when (buffer-live-p buf) (kill-buffer buf))))))

(ert-deftest agent-repl-ecfg-test-ensure-webview/remounts-on-session-change ()
  "A session change kills the stale webview and mounts a fresh one — an
xwidget session cannot be retargeted from outside."
  ;; Arrange
  (agent-repl-ecfg-test--with-state
    (let ((agent-repl-explain-config-buffer-name " *test-ecfg-rebind*")
          (agent-repl-ecfg-test--urls nil)
          (first nil)
          (second nil))
      (unwind-protect
          (cl-letf (((symbol-function 'agent-repl--frontend-xwidget-available-p) (lambda () t))
                    ((symbol-function 'agent-repl--frontend-session-url)
                     (lambda (id) (format "http://d/?session=%s" id)))
                    ((symbol-function 'agent-repl--frontend-make-webview-buffer)
                     (agent-repl-test--fake-webview-factory 'agent-repl-ecfg-test--urls))
                    ((symbol-function 'agent-repl--frontend-kill-webview) #'kill-buffer))
            (setq first (agent-repl--explain-config-ensure-webview "s_1"))
            ;; Act
            (setq second (agent-repl--explain-config-ensure-webview "s_2"))
            ;; Assert
            (should-not (eq second first))
            (should-not (buffer-live-p first))
            (should (equal (car agent-repl-ecfg-test--urls) "http://d/?session=s_2")))
        (when (buffer-live-p second) (kill-buffer second))))))

(ert-deftest agent-repl-ecfg-test-ensure-webview/errors-without-xwidget-support ()
  "An Emacs build without xwidget-webkit fails loudly instead of half-mounting."
  ;; Arrange
  (agent-repl-ecfg-test--with-state
    (cl-letf (((symbol-function 'agent-repl--frontend-xwidget-available-p) (lambda () nil)))
      ;; Act + Assert
      (should-error (agent-repl--explain-config-ensure-webview "s_1") :type 'user-error))))

(ert-deftest agent-repl-ecfg-test-release-webview/kills-buffer-and-clears-binding ()
  "Release kills the webview buffer and forgets which session it was bound to."
  ;; Arrange
  (agent-repl-ecfg-test--with-state
    (let* ((agent-repl-explain-config-buffer-name " *test-ecfg-release*")
           (buf (get-buffer-create agent-repl-explain-config-buffer-name))
           (agent-repl--explain-config-webview-session-id "s_1"))
      (cl-letf (((symbol-function 'agent-repl--frontend-kill-webview) #'kill-buffer))
        ;; Act
        (agent-repl--explain-config-release-webview)
        ;; Assert
        (should-not (buffer-live-p buf))
        (should-not agent-repl--explain-config-webview-session-id)))))

;;;; ---- Turns -----------------------------------------------------------------------

(ert-deftest agent-repl-ecfg-test-send/first-turn-carries-the-preamble ()
  "The first turn of a session is wrapped in the read-only contract."
  ;; Arrange
  (agent-repl-ecfg-test--with-state
    (let (sent)
      (cl-letf (((symbol-function 'agent-repl--frontend-send-message)
                 (lambda (_id text) (setq sent text) "req-1")))
        ;; Act
        (agent-repl--explain-config-send "s_1" "what is SPC o c?")
        ;; Assert
        (should (string-match-p "READ-ONLY" sent))
        (should (string-match-p "what is SPC o c\\?" sent))))))

(ert-deftest agent-repl-ecfg-test-send/follow-up-turn-is-verbatim ()
  "A follow-up turn is sent verbatim — the contract is already in context."
  ;; Arrange
  (agent-repl-ecfg-test--with-state
    (let ((agent-repl--explain-config-primed-p t)
          sent)
      (cl-letf (((symbol-function 'agent-repl--frontend-send-message)
                 (lambda (_id text) (setq sent text) "req-2")))
        ;; Act
        (agent-repl--explain-config-send "s_1" "and what about SPC o C?")
        ;; Assert
        (should (equal sent "and what about SPC o C?"))))))

(ert-deftest agent-repl-ecfg-test-send/primes-the-session-after-the-first-turn ()
  "Sending the first turn arms the primed flag so the next turn goes verbatim."
  ;; Arrange
  (agent-repl-ecfg-test--with-state
    (cl-letf (((symbol-function 'agent-repl--frontend-send-message)
               (lambda (&rest _) "req-1")))
      ;; Act
      (agent-repl--explain-config-send "s_1" "q")
      ;; Assert
      (should agent-repl--explain-config-primed-p))))

(ert-deftest agent-repl-ecfg-test-send/targets-the-given-session ()
  "The turn is injected into the session it was handed, not a rediscovered one."
  ;; Arrange
  (agent-repl-ecfg-test--with-state
    (let (target)
      (cl-letf (((symbol-function 'agent-repl--frontend-send-message)
                 (lambda (id _text) (setq target id) "req-1")))
        ;; Act
        (agent-repl--explain-config-send "s_42" "q")
        ;; Assert
        (should (equal target "s_42"))))))

;;;; ---- agent-repl-explain-config -----------------------------------------------------

(defmacro agent-repl-ecfg-test--with-stubbed-pipeline (sent-sym &rest body)
  "Run BODY with the session/webview/popup pipeline stubbed.
SENT-SYM is bound to the text handed to the send path (nil if none)."
  (declare (indent 1))
  `(agent-repl-ecfg-test--with-state
     (let ((,sent-sym nil))
       ;; Not every BODY reads the capture; `ignore' marks it as
       ;; deliberately maybe-unused so expansions compile warning-free.
       (ignore ,sent-sym)
       (cl-letf (((symbol-function 'agent-repl--explain-config-ensure-session)
                  (lambda () "s_1"))
                 ((symbol-function 'agent-repl--explain-config-ensure-webview)
                  (lambda (_id) 'fake-webview))
                 ((symbol-function 'agent-repl--explain-config-show) #'ignore)
                 ((symbol-function 'agent-repl--explain-config-send)
                  (lambda (_id text) (setq ,sent-sym text))))
         ,@body))))

(ert-deftest agent-repl-ecfg-test-command/empty-prompt-errors ()
  "explain-config signals user-error for an empty prompt."
  (agent-repl-ecfg-test--with-stubbed-pipeline sent
    ;; Act + Assert
    (should-error (agent-repl-explain-config "") :type 'user-error)
    (should-not sent)))

(ert-deftest agent-repl-ecfg-test-command/whitespace-prompt-errors ()
  "explain-config signals user-error for a whitespace-only prompt."
  (agent-repl-ecfg-test--with-stubbed-pipeline sent
    ;; Act + Assert
    (should-error (agent-repl-explain-config "   \n\t  ") :type 'user-error)
    (should-not sent)))

(ert-deftest agent-repl-ecfg-test-command/forwards-trimmed-prompt ()
  "explain-config trims surrounding whitespace before sending the turn."
  (agent-repl-ecfg-test--with-stubbed-pipeline sent
    ;; Act
    (agent-repl-explain-config "  hello  ")
    ;; Assert
    (should (equal sent "hello"))))

(ert-deftest agent-repl-ecfg-test-command/shows-the-popup ()
  "explain-config displays the webview popup for the answer."
  ;; Arrange
  (agent-repl-ecfg-test--with-state
    (let (shown)
      (cl-letf (((symbol-function 'agent-repl--explain-config-ensure-session)
                 (lambda () "s_1"))
                ((symbol-function 'agent-repl--explain-config-ensure-webview)
                 (lambda (_id) 'fake-webview))
                ((symbol-function 'agent-repl--explain-config-show)
                 (lambda () (setq shown t)))
                ((symbol-function 'agent-repl--explain-config-send) #'ignore))
        ;; Act
        (agent-repl-explain-config "q")
        ;; Assert
        (should shown)))))

(ert-deftest agent-repl-ecfg-test-command/reuses-session-for-follow-ups ()
  "A second question reuses the same session — that IS the conversation."
  ;; Arrange
  (agent-repl-ecfg-test--with-state
    (let ((sessions nil)
          (created 0))
      (cl-letf (((symbol-function 'agent-repl--ensure-frontend-daemon) (lambda (&rest _) 'proc))
                ((symbol-function 'agent-repl--frontend-wait-ready) (lambda () t))
                ((symbol-function 'agent-repl--frontend-session-live-p) (lambda (_id) t))
                ((symbol-function 'agent-repl--frontend-create-session)
                 (lambda (&rest _) (cl-incf created) "s_1"))
                ((symbol-function 'agent-repl--explain-config-ensure-webview)
                 (lambda (_id) 'fake-webview))
                ((symbol-function 'agent-repl--explain-config-show) #'ignore)
                ((symbol-function 'agent-repl--frontend-send-message)
                 (lambda (id _text) (push id sessions) "req")))
        ;; Act
        (agent-repl-explain-config "first")
        (agent-repl-explain-config "second")
        ;; Assert
        (should (equal sessions '("s_1" "s_1")))
        (should (= created 1))))))

(ert-deftest agent-repl-ecfg-test-command/second-question-is-not-re-preambled ()
  "The follow-up question rides the existing conversation, unwrapped."
  ;; Arrange
  (agent-repl-ecfg-test--with-state
    (let ((sent nil))
      (cl-letf (((symbol-function 'agent-repl--ensure-frontend-daemon) (lambda (&rest _) 'proc))
                ((symbol-function 'agent-repl--frontend-wait-ready) (lambda () t))
                ((symbol-function 'agent-repl--frontend-session-live-p) (lambda (_id) t))
                ((symbol-function 'agent-repl--frontend-create-session) (lambda (&rest _) "s_1"))
                ((symbol-function 'agent-repl--explain-config-ensure-webview)
                 (lambda (_id) 'fake-webview))
                ((symbol-function 'agent-repl--explain-config-show) #'ignore)
                ((symbol-function 'agent-repl--frontend-send-message)
                 (lambda (_id text) (push text sent) "req")))
        ;; Act
        (agent-repl-explain-config "first")
        (agent-repl-explain-config "second")
        ;; Assert
        (should (equal (car sent) "second"))))))

(ert-deftest agent-repl-ecfg-test-command/prefix-arg-resets-first ()
  "A prefix argument discards the conversation before asking."
  ;; Arrange
  (agent-repl-ecfg-test--with-stubbed-pipeline sent
    (let ((reset 0))
      (cl-letf (((symbol-function 'agent-repl-explain-config-reset)
                 (lambda () (cl-incf reset))))
        ;; Act
        (agent-repl-explain-config "q" t)
        ;; Assert
        (should (= reset 1))))))

(ert-deftest agent-repl-ecfg-test-command/no-prefix-arg-keeps-conversation ()
  "Without a prefix argument the conversation is never discarded."
  ;; Arrange
  (agent-repl-ecfg-test--with-stubbed-pipeline sent
    (let ((reset 0))
      (cl-letf (((symbol-function 'agent-repl-explain-config-reset)
                 (lambda () (cl-incf reset))))
        ;; Act
        (agent-repl-explain-config "q")
        ;; Assert
        (should (= reset 0))))))

;;;; ---- agent-repl-explain-config-reset ------------------------------------------------

(ert-deftest agent-repl-ecfg-test-reset/is-interactive ()
  "The reset command is interactively callable."
  (should (commandp #'agent-repl-explain-config-reset)))

(ert-deftest agent-repl-ecfg-test-reset/tears-down-webview-and-session ()
  "Reset hides the popup, kills the webview, and deletes the daemon session."
  ;; Arrange
  (agent-repl-ecfg-test--with-state
    (let ((hidden 0) (webview 0) (session 0))
      (cl-letf (((symbol-function 'agent-repl--explain-config-hide)
                 (lambda () (cl-incf hidden)))
                ((symbol-function 'agent-repl--explain-config-release-webview)
                 (lambda () (cl-incf webview)))
                ((symbol-function 'agent-repl--explain-config-release-session)
                 (lambda () (cl-incf session))))
        ;; Act
        (agent-repl-explain-config-reset)
        ;; Assert
        (should (= hidden 1))
        (should (= webview 1))
        (should (= session 1))))))

(ert-deftest agent-repl-ecfg-test-reset/next-question-re-primes-the-preamble ()
  "After a reset the next question opens a fresh conversation, contract and all."
  ;; Arrange
  (agent-repl-ecfg-test--with-state
    (let ((agent-repl--explain-config-session-id "s_old")
          (agent-repl--explain-config-primed-p t)
          (sent nil))
      (cl-letf (((symbol-function 'agent-repl--explain-config-hide) #'ignore)
                ((symbol-function 'agent-repl--explain-config-release-webview) #'ignore)
                ((symbol-function 'agent-repl--frontend-delete-session) (lambda (_id) t))
                ((symbol-function 'agent-repl--ensure-frontend-daemon) (lambda (&rest _) 'proc))
                ((symbol-function 'agent-repl--frontend-wait-ready) (lambda () t))
                ((symbol-function 'agent-repl--frontend-create-session) (lambda (&rest _) "s_new"))
                ((symbol-function 'agent-repl--explain-config-ensure-webview)
                 (lambda (_id) 'fake-webview))
                ((symbol-function 'agent-repl--explain-config-show) #'ignore)
                ((symbol-function 'agent-repl--frontend-send-message)
                 (lambda (_id text) (setq sent text) "req")))
        ;; Act
        (agent-repl-explain-config "fresh" t)
        ;; Assert
        (should (string-match-p "READ-ONLY" sent))
        (should (equal agent-repl--explain-config-session-id "s_new"))))))

;;;; ---- Popup: display action ---------------------------------------------------------

(ert-deftest agent-repl-ecfg-test-display-action/uses-right-side-window ()
  "Display action routes the explain-config webview to a right-side window."
  (should (member 'display-buffer-in-side-window
                  (car agent-repl--explain-config-display-action)))
  (should (eq (cdr (assq 'side agent-repl--explain-config-display-action))
              'right)))

(ert-deftest agent-repl-ecfg-test-display-action/sits-at-right-slot-0 ()
  "Explain-config window lives at slot 0 on the right side."
  (should (= (cdr (assq 'slot agent-repl--explain-config-display-action))
             0)))

(ert-deftest agent-repl-ecfg-test-display-action/configures-window-width ()
  "Display action specifies `window-width' (own width config), not `window-height'."
  (should (assq 'window-width agent-repl--explain-config-display-action))
  (should-not (assq 'window-height agent-repl--explain-config-display-action)))

(ert-deftest agent-repl-ecfg-test-width-fraction/defcustom-defaults-half-frame ()
  "Width-fraction defcustom exists and defaults to half the frame width."
  (should (boundp 'agent-repl-explain-config-width-fraction))
  (should (floatp agent-repl-explain-config-width-fraction))
  (should (= agent-repl-explain-config-width-fraction 0.5)))

(ert-deftest agent-repl-ecfg-test-window-width/computes-from-fraction ()
  "`--window-width' returns round(fraction × frame-width) for any window."
  (let ((agent-repl-explain-config-width-fraction 0.5))
    (cl-letf (((symbol-function 'window-frame) (lambda (_w) 'fake-frame))
              ((symbol-function 'frame-width) (lambda (_f) 200)))
      (should (= (agent-repl--explain-config-window-width 'fake-win) 100)))))

(ert-deftest agent-repl-ecfg-test-window-width/clamps-floor-at-1 ()
  "`--window-width' floors at 1 col even when fraction × width rounds to 0."
  (let ((agent-repl-explain-config-width-fraction 0.001))
    (cl-letf (((symbol-function 'window-frame) (lambda (_w) 'fake-frame))
              ((symbol-function 'frame-width) (lambda (_f) 40)))
      (should (>= (agent-repl--explain-config-window-width 'fake-win) 1)))))

;;;; ---- Popup: show -------------------------------------------------------------------

(ert-deftest agent-repl-ecfg-test-show/no-op-when-buffer-missing ()
  "Show is a no-op when no explain-config webview has ever been created."
  (let ((agent-repl-explain-config-buffer-name " *nonexistent-explain-config*")
        (display-called nil))
    (cl-letf (((symbol-function 'display-buffer)
               (lambda (&rest _) (setq display-called t) nil)))
      (agent-repl--explain-config-show)
      (should-not display-called))))

(ert-deftest agent-repl-ecfg-test-show/displays-existing-buffer ()
  "Show falls back to `display-buffer' with the side-window action when the
agent output window is not available to take over."
  (let* ((agent-repl-explain-config-buffer-name " *test-explain-show*")
         (buf (get-buffer-create agent-repl-explain-config-buffer-name))
         (agent-repl--explain-config-replaced-window nil)
         (display-args nil))
    (unwind-protect
        (cl-letf (((symbol-function 'get-buffer-window) (lambda (&rest _) nil))
                  ((symbol-function 'agent-repl--explain-config-current-agent-output-window)
                   (lambda () nil))
                  ((symbol-function 'display-buffer)
                   (lambda (b action) (setq display-args (list b action)) nil))
                  ((symbol-function 'agent-repl--explain-config-apply-width) #'ignore))
          (agent-repl--explain-config-show)
          (should (eq (car display-args) buf))
          (should (eq (cadr display-args)
                      agent-repl--explain-config-display-action)))
      (when (buffer-live-p buf) (kill-buffer buf)))))

(ert-deftest agent-repl-ecfg-test-show/defines-no-global-visible-flag ()
  "The popup is per-workspace: the old cross-persp `--global-visible-p'
flag must not exist at all."
  (should-not (boundp 'agent-repl--explain-config-global-visible-p)))

(ert-deftest agent-repl-ecfg-test-show/reapplies-width-when-visible ()
  "Show reapplies the configured width when a side-window already displays it."
  (let* ((agent-repl-explain-config-buffer-name " *test-explain-reapply*")
         (buf (get-buffer-create agent-repl-explain-config-buffer-name))
         (agent-repl--explain-config-replaced-window nil)
         (width-applied-with nil))
    (unwind-protect
        (cl-letf (((symbol-function 'get-buffer-window)
                   (lambda (&rest _) 'fake-win))
                  ((symbol-function 'window-live-p) (lambda (_w) t))
                  ((symbol-function 'display-buffer)
                   (lambda (&rest _) (error "should not redisplay")))
                  ((symbol-function 'agent-repl--explain-config-apply-width)
                   (lambda (w) (setq width-applied-with w))))
          (agent-repl--explain-config-show)
          (should (eq width-applied-with 'fake-win)))
      (when (buffer-live-p buf) (kill-buffer buf)))))

;;;; ---- Popup: current agent output window lookup --------------------------------

(ert-deftest agent-repl-ecfg-test-current-agent-output-window/queries-the-view-panel ()
  "The popup's takeover target is looked up via the `:view' panel kind,
not the retired `:vterm' key — `window.el' renamed the panel kind to
`:view' (resolving to `:frontend-buffer') when the vterm frontend was
removed, and explain-config.el was updated to pass the new key."
  ;; Arrange
  (let (queried-kind)
    (cl-letf (((symbol-function 'agent-repl-window--panel-window)
               (lambda (kind &rest _) (setq queried-kind kind) 'fake-win)))
      ;; Act
      (agent-repl--explain-config-current-agent-output-window)
      ;; Assert
      (should (eq queried-kind :view)))))

;;;; ---- Popup: agent output window takeover -------------------------------------------

(ert-deftest agent-repl-ecfg-test-show/takes-over-agent-output-window-when-visible ()
  "Show reuses the live agent output window via `set-window-buffer' rather
than falling back to the side-window display action."
  (let* ((agent-repl-explain-config-buffer-name " *test-explain-takeover*")
         (buf (get-buffer-create agent-repl-explain-config-buffer-name))
         (agent-repl--explain-config-replaced-window nil)
         (set-window-buffer-args nil)
         (display-buffer-called nil))
    (unwind-protect
        (cl-letf (((symbol-function 'get-buffer-window) (lambda (&rest _) nil))
                  ((symbol-function 'agent-repl--explain-config-current-agent-output-window)
                   (lambda () 'fake-output-win))
                  ((symbol-function 'window-buffer) (lambda (_w) 'prev-buf))
                  ((symbol-function 'set-window-dedicated-p) #'ignore)
                  ((symbol-function 'set-window-buffer)
                   (lambda (w b) (setq set-window-buffer-args (list w b))))
                  ((symbol-function 'display-buffer)
                   (lambda (&rest _) (setq display-buffer-called t) nil)))
          (agent-repl--explain-config-show)
          (should (equal set-window-buffer-args (list 'fake-output-win buf)))
          (should-not display-buffer-called))
      (when (buffer-live-p buf) (kill-buffer buf)))))

(ert-deftest agent-repl-ecfg-test-show/records-replaced-window-on-takeover ()
  "Show stashes the (window . prev-buffer) cell so `--hide' can restore."
  (let* ((agent-repl-explain-config-buffer-name " *test-explain-takeover-record*")
         (buf (get-buffer-create agent-repl-explain-config-buffer-name))
         (agent-repl--explain-config-replaced-window nil))
    (unwind-protect
        (cl-letf (((symbol-function 'get-buffer-window) (lambda (&rest _) nil))
                  ((symbol-function 'agent-repl--explain-config-current-agent-output-window)
                   (lambda () 'fake-output-win))
                  ((symbol-function 'window-buffer) (lambda (_w) 'prev-buf))
                  ((symbol-function 'set-window-dedicated-p) #'ignore)
                  ((symbol-function 'set-window-buffer) #'ignore))
          (agent-repl--explain-config-show)
          (should (equal agent-repl--explain-config-replaced-window
                         '(fake-output-win . prev-buf))))
      (when (buffer-live-p buf) (kill-buffer buf)))))

(ert-deftest agent-repl-ecfg-test-show/takeover-clears-window-dedication ()
  "Show clears the agent output window's dedicated flag before swapping
buffers — without this, `set-window-buffer' would error on the dedicated
window."
  (let* ((agent-repl-explain-config-buffer-name " *test-explain-takeover-dedup*")
         (buf (get-buffer-create agent-repl-explain-config-buffer-name))
         (agent-repl--explain-config-replaced-window nil)
         (dedicated-cleared-with nil))
    (unwind-protect
        (cl-letf (((symbol-function 'get-buffer-window) (lambda (&rest _) nil))
                  ((symbol-function 'agent-repl--explain-config-current-agent-output-window)
                   (lambda () 'fake-output-win))
                  ((symbol-function 'window-buffer) (lambda (_w) 'prev-buf))
                  ((symbol-function 'set-window-dedicated-p)
                   (lambda (w flag) (setq dedicated-cleared-with (list w flag))))
                  ((symbol-function 'set-window-buffer) #'ignore))
          (agent-repl--explain-config-show)
          (should (equal dedicated-cleared-with '(fake-output-win nil))))
      (when (buffer-live-p buf) (kill-buffer buf)))))

;;;; ---- Popup: hide -------------------------------------------------------------------

(ert-deftest agent-repl-ecfg-test-hide/deletes-windows-for-buffer ()
  "Hide delegates to `agent-repl-window--delete-buffer-windows' for the buffer."
  (let* ((agent-repl-explain-config-buffer-name " *test-explain-hide*")
         (buf (get-buffer-create agent-repl-explain-config-buffer-name))
         (agent-repl--explain-config-replaced-window nil)
         (delete-called-with nil))
    (unwind-protect
        (cl-letf (((symbol-function 'agent-repl-window--delete-buffer-windows)
                   (lambda (b &rest _) (setq delete-called-with b))))
          (agent-repl--explain-config-hide)
          (should (eq delete-called-with buf)))
      (when (buffer-live-p buf) (kill-buffer buf)))))

(ert-deftest agent-repl-ecfg-test-hide/no-op-when-buffer-missing ()
  "Hide is a no-op when no explain-config webview has ever been created."
  (let ((agent-repl-explain-config-buffer-name " *nonexistent-explain-hide*")
        (agent-repl--explain-config-replaced-window nil)
        (delete-called nil))
    (cl-letf (((symbol-function 'agent-repl-window--delete-buffer-windows)
               (lambda (&rest _) (setq delete-called t))))
      (agent-repl--explain-config-hide)
      (should-not delete-called))))

(ert-deftest agent-repl-ecfg-test-hide/keeps-the-conversation-alive ()
  "Hide never releases the session — the conversation survives a close."
  (agent-repl-ecfg-test--with-state
    (let ((agent-repl--explain-config-session-id "s_1")
          (deleted nil))
      (cl-letf (((symbol-function 'agent-repl-window--delete-buffer-windows) #'ignore)
                ((symbol-function 'agent-repl--frontend-delete-session)
                 (lambda (_id) (setq deleted t))))
        (agent-repl--explain-config-hide)
        (should-not deleted)
        (should (equal agent-repl--explain-config-session-id "s_1"))))))

;;;; ---- Popup: agent output window restoration on hide --------------------------------

(ert-deftest agent-repl-ecfg-test-hide/restores-prev-buffer-in-replaced-window ()
  "Hide re-displays the saved prev-buffer in the window the popup took over."
  (let* ((agent-repl-explain-config-buffer-name " *test-explain-restore*")
         (buf (get-buffer-create agent-repl-explain-config-buffer-name))
         (prev (get-buffer-create " *test-explain-restore-prev*"))
         (agent-repl--explain-config-replaced-window (cons 'fake-output-win prev))
         (set-window-buffer-args nil))
    (unwind-protect
        (cl-letf (((symbol-function 'window-live-p) (lambda (_w) t))
                  ((symbol-function 'set-window-buffer)
                   (lambda (w b) (setq set-window-buffer-args (list w b))))
                  ((symbol-function 'agent-repl-window--harden) #'ignore)
                  ((symbol-function 'agent-repl-window--delete-buffer-windows) #'ignore))
          (agent-repl--explain-config-hide)
          (should (equal set-window-buffer-args (list 'fake-output-win prev))))
      (when (buffer-live-p buf) (kill-buffer buf))
      (when (buffer-live-p prev) (kill-buffer prev)))))

(ert-deftest agent-repl-ecfg-test-hide/clears-replaced-window-after-restore ()
  "Hide clears `--replaced-window' after restoring, so a future show won't
double-restore an already-rehydrated agent output window."
  (let ((agent-repl--explain-config-replaced-window (cons 'fake-output-win 'prev-buf)))
    (cl-letf (((symbol-function 'window-live-p) (lambda (_w) nil))
              ((symbol-function 'agent-repl-window--delete-buffer-windows) #'ignore))
      (agent-repl--explain-config-hide)
      (should-not agent-repl--explain-config-replaced-window))))

(ert-deftest agent-repl-ecfg-test-hide/reapplies-agent-output-window-hardening ()
  "Hide re-applies `agent-repl-window--harden' on successful restore so the
agent output window regains its dedicate/size-fix/delete-protect recipe."
  (let* ((prev (get-buffer-create " *test-explain-reharden-prev*"))
         (agent-repl--explain-config-replaced-window (cons 'fake-output-win prev))
         (harden-called-with nil))
    (unwind-protect
        (cl-letf (((symbol-function 'window-live-p) (lambda (_w) t))
                  ((symbol-function 'set-window-buffer) #'ignore)
                  ((symbol-function 'agent-repl-window--harden)
                   (lambda (w &rest recipe) (setq harden-called-with (cons w recipe))))
                  ((symbol-function 'agent-repl-window--delete-buffer-windows) #'ignore))
          (agent-repl--explain-config-hide)
          (should (equal harden-called-with
                         '(fake-output-win :dedicate t :size-fix width :delete-protect t))))
      (when (buffer-live-p prev) (kill-buffer prev)))))

(ert-deftest agent-repl-ecfg-test-hide/skips-restore-when-window-dead ()
  "Hide is a no-op on the restore path when the saved window is no longer live."
  (let ((agent-repl--explain-config-replaced-window (cons 'dead-win 'prev-buf))
        (set-window-buffer-called nil))
    (cl-letf (((symbol-function 'window-live-p) (lambda (_w) nil))
              ((symbol-function 'set-window-buffer)
               (lambda (&rest _) (setq set-window-buffer-called t)))
              ((symbol-function 'agent-repl-window--delete-buffer-windows) #'ignore))
      (agent-repl--explain-config-hide)
      (should-not set-window-buffer-called))))

;;;; ---- agent-repl-explain-config-close -------------------------------------------------

(ert-deftest agent-repl-ecfg-test-close/is-interactive ()
  "Public close command is interactively callable (has interactive form)."
  (should (commandp #'agent-repl-explain-config-close)))

(ert-deftest agent-repl-ecfg-test-close/delegates-to-hide ()
  "Public close command invokes the private hide implementation."
  (let ((hide-called 0))
    (cl-letf (((symbol-function 'agent-repl--explain-config-hide)
               (lambda (&rest _) (cl-incf hide-called))))
      (agent-repl-explain-config-close)
      (should (= hide-called 1)))))

(ert-deftest agent-repl-ecfg-test-close/deletes-windows ()
  "End-to-end: public close deletes the popup windows."
  (let* ((agent-repl-explain-config-buffer-name " *test-explain-close-e2e*")
         (buf (get-buffer-create agent-repl-explain-config-buffer-name))
         (agent-repl--explain-config-replaced-window nil)
         (delete-called-with nil))
    (unwind-protect
        (cl-letf (((symbol-function 'agent-repl-window--delete-buffer-windows)
                   (lambda (b &rest _) (setq delete-called-with b))))
          (agent-repl-explain-config-close)
          (should (eq delete-called-with buf)))
      (when (buffer-live-p buf) (kill-buffer buf)))))

;;;; ---- Persp isolation --------------------------------------------------------------

(ert-deftest agent-repl-ecfg-test-persp-switch/no-reshow-hook-exists ()
  "The popup is per-workspace: the old persp-switch re-show hook must not
exist, so a workspace switch can never re-create the popup window."
  (should-not (fboundp 'agent-repl--explain-config-ensure-visible-on-persp-switch)))

(provide 'test-explain-config)

;;; test-explain-config.el ends here
