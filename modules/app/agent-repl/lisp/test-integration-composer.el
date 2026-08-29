;;; test-integration-composer.el --- Integration: input.el against the fake daemon -*- lexical-binding: t; -*-

;;; Commentary:

;; Scenario 12 of elisp-fanout.md §14, plus the two REQUIRED request facts
;; landing 2 and the origin increment added: every Emacs submit carries its
;; own `workspace' ref and its send site's `PromptOrigin', and pasted images
;; travel as `ImageBlock{path}'.
;;
;; The ROOT composer is HOST-NATIVE (R7): the webview runs composer-less, so
;; this is the one place a root prompt is authored, and PROMPT COMPOSITION IS
;; FREE — the metaprompt prepend and the prefix/postfix variants happen BEFORE
;; submission.  "Verbatim" means no post-submission rewriting.

;;; Code:

(require 'ert)
(require 'cl-lib)

(load (expand-file-name "test-integration-helpers.el"
                        (file-name-directory (or load-file-name buffer-file-name)))
      nil t)

;; Production names this suite depends on (input.el and prompt-queue.el, §10;
;; host.el, §7).
(declare-function agent-repl--send "input")
(declare-function agent-repl--prepare-input "input")
(declare-function agent-repl--meta-wrap "prompts")
(declare-function agent-repl-queue-deferred-prompt "prompt-queue")
(declare-function agent-repl-host-register "host")
(declare-function agent-repl-host-subscribe "host")
(declare-function agent-repl-host-unsubscribe "host")
(declare-function agent-repl-host-composer-gate "host")
(declare-function agent-repl-host-ref "host")
(declare-function agent-repl-connect-open "connect")
(declare-function agent-repl-connect-close "connect")
(declare-function agent-repl--ws-put "workspace")
(defvar agent-repl-send-posthooks)
(defvar agent-repl-host-update-functions)

;;;; ---- Fixtures ----

(defconst agent-repl-itest-composer--ws "itest-composer-ws"
  "The Doom workspace name this suite composes into.")

(defconst agent-repl-itest-composer--dir "/tmp/itest-composer-ws"
  "The workspace directory registered for the composer's workspace.")

(defun agent-repl-itest-composer--live (composer)
  "Return a HostWorkspace alist whose live session carries COMPOSER's arm."
  `((existing . ((id . ((value . "host-session-1")))
                 (live . ((generation . ((value . "gen-1")))
                          (shimAttached . t)
                          (claude . ((sessionId . "vendor-1")
                                     (configDir . "/home/itest/.claude")))
                          (backfill . ((done . ())))
                          (,composer . [])))))
    (naming . ())))

(defmacro agent-repl-itest-composer--with-composer (daemon gate ref &rest body)
  "Register, subscribe and gate the composer's workspace on DAEMON, run BODY.
GATE is the composer arm's protojson key symbol; REF is bound to the
minted WorkspaceRef.  The host push is what sets the gate — the RESOLVED
ARM IS THE GATE, so a test never sets one directly."
  (declare (indent 3) (debug (form form symbolp body)))
  `(let ((conn (agent-repl-connect-open (agent-repl-itest-daemon-address ,daemon)))
         (agent-repl-send-posthooks nil))
     (unwind-protect
         (let ((,ref nil))
           (agent-repl--ws-put agent-repl-itest-composer--ws
                               :project-dir agent-repl-itest-composer--dir)
           (agent-repl-host-register
            conn agent-repl-itest-composer--dir
            (lambda (minted) (setq ,ref minted)))
           (agent-repl-itest--wait-until (lambda () ,ref) nil
                                         "RegisterWorkspace to answer")
           (agent-repl-host-subscribe conn agent-repl-itest-composer--ws ,ref)
           (agent-repl-itest--await-subscriber ,daemon "host" (plist-get ,ref :id))
           (agent-repl-itest--push
            ,daemon "host"
            `((host . ,(agent-repl-itest-composer--live ,gate)))
            (plist-get ,ref :id))
           (agent-repl-itest--wait-until
            (lambda () (not (eq (agent-repl-host-composer-gate
                                 agent-repl-itest-composer--ws)
                                :unknown)))
            nil (format "the %s composer gate" ,gate))
           ,@body)
       (ignore-errors (agent-repl-host-unsubscribe agent-repl-itest-composer--ws))
       (agent-repl-connect-close conn))))

(defun agent-repl-itest-composer--submit-body (daemon &optional index)
  "Return DAEMON's recorded SubmitPrompt body at INDEX (default 0)."
  (nth (or index 0) (agent-repl-itest--call-bodies daemon "SubmitPrompt")))

(defun agent-repl-itest-composer--text-blocks (body)
  "Return the TextBlock texts of a recorded SubmitPrompt BODY, in order."
  (let ((blocks (agent-repl-itest--body-field body 'said 'content 'blocks)))
    (delq nil (mapcar (lambda (block)
                        (agent-repl-itest--body-field block 'text 'text))
                      blocks))))

(defun agent-repl-itest-composer--image-paths (body)
  "Return the ImageBlock paths of a recorded SubmitPrompt BODY, in order."
  (let ((blocks (agent-repl-itest--body-field body 'said 'content 'blocks)))
    (delq nil (mapcar (lambda (block)
                        (agent-repl-itest--body-field block 'image 'path 'path))
                      blocks))))

;;;; ---- The three REQUIRED request facts ----

(ert-deftest agent-repl-itest-composer-submit-carries-the-workspace-ref ()
  "Every Emacs submit carries `workspace', echoed from registration.
SubmitPromptRequest.workspace is REQUIRED (landing 2); the ref is the
daemon-minted echo token, never built from a path."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest-composer--with-composer daemon 'open ref
      ;; Act.
      (agent-repl--send agent-repl-itest-composer--ws "run the tests")
      (agent-repl-itest--await-call daemon "SubmitPrompt")
      ;; Assert.
      (let ((body (agent-repl-itest-composer--submit-body daemon)))
        (should (equal (agent-repl-itest--body-field body 'workspace 'id)
                       (plist-get ref :id)))
        (should (equal (agent-repl-itest--body-field body 'workspace 'dir)
                       (plist-get ref :dir)))))))

(ert-deftest agent-repl-itest-composer-submit-omits-the-feed ()
  "The ROOT composer leaves `feed' unset.
`feed' is optional and names a SUB-feed; per-bubble composers stay in the
webapp (R7), so a host submit must carry no feed at all."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest-composer--with-composer daemon 'open ref
      (ignore ref)
      ;; Act.
      (agent-repl--send agent-repl-itest-composer--ws "run the tests")
      (agent-repl-itest--await-call daemon "SubmitPrompt")
      ;; Assert: absence, not an empty value — PRESENCE, NEVER SENTINELS.
      (let ((body (agent-repl-itest-composer--submit-body daemon)))
        (should (null (assq 'feed body)))))))

(ert-deftest agent-repl-itest-composer-submit-carries-a-fresh-idempotency-key ()
  "Each submit mints its own idempotency key.
Two submits sharing a key would let the daemon collapse them into one."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest-composer--with-composer daemon 'open ref
      (ignore ref)
      ;; Act.
      (agent-repl--send agent-repl-itest-composer--ws "first")
      (agent-repl-itest--await-call daemon "SubmitPrompt" 1)
      (agent-repl--send agent-repl-itest-composer--ws "second")
      (agent-repl-itest--await-call daemon "SubmitPrompt" 2)
      ;; Assert.
      (let ((first (agent-repl-itest--body-field
                    (agent-repl-itest-composer--submit-body daemon 0) 'idempotencyKey))
            (second (agent-repl-itest--body-field
                     (agent-repl-itest-composer--submit-body daemon 1) 'idempotencyKey)))
        (should first)
        (should-not (equal first second))))))

(ert-deftest agent-repl-itest-composer-submit-carries-its-send-sites-origin ()
  "Each send site sends its OWN PromptOrigin, and never UNSPECIFIED.
SubmitPromptRequest.origin is REQUIRED (E1 resolved); the fake refuses
UNSPECIFIED, so a site that forgot its origin cannot pass."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest-composer--with-composer daemon 'open ref
      (ignore ref)
      ;; Act.
      (agent-repl--send agent-repl-itest-composer--ws "run the tests")
      (agent-repl-itest--await-call daemon "SubmitPrompt")
      ;; Assert.
      (let ((origin (agent-repl-itest--body-field
                     (agent-repl-itest-composer--submit-body daemon) 'origin)))
        (should (equal origin "PROMPT_ORIGIN_USER_SENT"))))))

(ert-deftest agent-repl-itest-composer-metaprompt-site-sends-its-own-origin ()
  "The metaprompt variant sends PROMPT_ORIGIN_USER_SENT_WITH_METAPROMPT.
The origins exist so the daemon can tell WHICH send site authored a
prompt; a shared origin across sites would erase that."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest-composer--with-composer daemon 'open ref
      (ignore ref)
      ;; Act.
      (agent-repl--send agent-repl-itest-composer--ws "run the tests"
                        :origin :user-sent-with-metaprompt)
      (agent-repl-itest--await-call daemon "SubmitPrompt")
      ;; Assert.
      (let ((origin (agent-repl-itest--body-field
                     (agent-repl-itest-composer--submit-body daemon) 'origin)))
        (should (equal origin "PROMPT_ORIGIN_USER_SENT_WITH_METAPROMPT"))))))

(ert-deftest agent-repl-itest-composer-deferred-drain-sends-its-own-origin ()
  "A drained deferred prompt sends PROMPT_ORIGIN_DEFERRED_PROMPT.
The drain is a distinct send site: the user's intent was authored earlier
and delivered now, and the daemon is told so."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest-composer--with-composer daemon 'open ref
      (ignore ref)
      ;; Act.
      (agent-repl--send agent-repl-itest-composer--ws "run the tests"
                        :origin :deferred-prompt)
      (agent-repl-itest--await-call daemon "SubmitPrompt")
      ;; Assert.
      (let ((origin (agent-repl-itest--body-field
                     (agent-repl-itest-composer--submit-body daemon) 'origin)))
        (should (equal origin "PROMPT_ORIGIN_DEFERRED_PROMPT"))))))

;;;; ---- Scenario 12: composition and outcomes ----

(ert-deftest agent-repl-itest-composer-text-becomes-a-user-said-text-block ()
  "The composer's text crosses as UserSaid content with one TextBlock.
UserSaid → UserContent → UserContentBlock{text} is the whole shape; the
daemon strips the sentinel-marked metaprompt spans from the DRAWN row,
never from what was submitted."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest-composer--with-composer daemon 'open ref
      (ignore ref)
      ;; Act.
      (agent-repl--send agent-repl-itest-composer--ws "run the tests")
      (agent-repl-itest--await-call daemon "SubmitPrompt")
      ;; Assert.
      (let ((texts (agent-repl-itest-composer--text-blocks
                    (agent-repl-itest-composer--submit-body daemon))))
        (should (equal 1 (length texts)))
        (should (string-match-p "run the tests" (car texts)))))))

(ert-deftest agent-repl-itest-composer-metaprompt-markers-ride-the-text ()
  "The metaprompt prepend is submitted as part of the text, markers and all.
PROMPT COMPOSITION IS FREE before submission; the sentinel markers are
how the daemon knows which span to hide when it DRAWS the row."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest-composer--with-composer daemon 'open ref
      (ignore ref)
      (let ((prepared (agent-repl--prepare-input
                       agent-repl-itest-composer--ws "run the tests"
                       :metaprompt t)))
        ;; Act.
        (agent-repl--send agent-repl-itest-composer--ws prepared
                          :origin :user-sent-with-metaprompt)
        (agent-repl-itest--await-call daemon "SubmitPrompt")
        ;; Assert: what is submitted is what was composed, unrewritten.
        (let ((texts (agent-repl-itest-composer--text-blocks
                      (agent-repl-itest-composer--submit-body daemon))))
          (should (equal (car texts) prepared)))))))

(ert-deftest agent-repl-itest-composer-attached-image-travels-as-a-path-block ()
  "A pasted image travels as `ImageBlock{path}' beside the text.
Ruled at kickoff: pasted images travel as ImageBlock{path}; the composer
keeps the attachment list per buffer and clears it on a successful send."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest-composer--with-composer daemon 'open ref
      (ignore ref)
      (let ((image (expand-file-name "pasted.png" agent-repl-itest-composer--dir)))
        ;; Act.
        (agent-repl--send agent-repl-itest-composer--ws "look at this"
                          :images (list (list :path image :media-type "image/png")))
        (agent-repl-itest--await-call daemon "SubmitPrompt")
        ;; Assert.
        (let ((body (agent-repl-itest-composer--submit-body daemon)))
          (should (equal (agent-repl-itest-composer--image-paths body) (list image))))))))

(ert-deftest agent-repl-itest-composer-attached-image-carries-its-media-type ()
  "An ImageBlock carries its `media_type' beside the location oneof.
`media_type' is a sibling of the oneof, not part of it: the location says
WHERE the bytes are, the media type says WHAT they are."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest-composer--with-composer daemon 'open ref
      (ignore ref)
      (let ((image (expand-file-name "pasted.png" agent-repl-itest-composer--dir)))
        ;; Act.
        (agent-repl--send agent-repl-itest-composer--ws "look at this"
                          :images (list (list :path image :media-type "image/png")))
        (agent-repl-itest--await-call daemon "SubmitPrompt")
        ;; Assert.
        (let* ((body (agent-repl-itest-composer--submit-body daemon))
               (blocks (agent-repl-itest--body-field body 'said 'content 'blocks))
               (image-block (seq-find (lambda (block) (assq 'image block)) blocks)))
          (should (equal (agent-repl-itest--body-field image-block 'image 'mediaType)
                         "image/png")))))))

(ert-deftest agent-repl-itest-composer-success-turn-runs-the-send-posthooks ()
  "A `turn' outcome clears the input and runs the send posthooks.
`turn' is the only outcome that means \"something to await\"."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest-composer--with-composer daemon 'open ref
      (ignore ref)
      (let ((ran nil))
        (add-hook 'agent-repl-send-posthooks (lambda (&rest _) (push t ran)))
        ;; Act.
        (agent-repl--send agent-repl-itest-composer--ws "run the tests")
        ;; Assert.
        (agent-repl-itest--wait-until (lambda () ran) nil "the send posthooks")
        (should ran)))))

(ert-deftest agent-repl-itest-composer-command-refused-is-answered-not-awaited ()
  "`command_refused' means \"answered, nothing to await\".
Both non-turn success arms mean that; the webapp draws the refusal card,
and Emacs clears the input and logs."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest--script
     daemon "SubmitPrompt"
     '((success . ((commandRefused . ((command . "/agents")))))))
    (agent-repl-itest-composer--with-composer daemon 'open ref
      (ignore ref)
      ;; Act.
      (agent-repl--send agent-repl-itest-composer--ws "/agents")
      (agent-repl-itest--await-call daemon "SubmitPrompt")
      ;; Assert.
      (agent-repl-itest--await-log daemon "elisp.input.command-answered" "info")
      (should (agent-repl-itest--logged-p daemon "elisp.input.command-answered" "info")))))

(ert-deftest agent-repl-itest-composer-command-panel-is-answered-not-awaited ()
  "`command_panel' is the other \"answered, nothing to await\" arm.
Q3 stands unruled, so Emacs IGNORES the panel payload itself and only
clears the input; the webapp draws panels from its own dev composer."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest--script
     daemon "SubmitPrompt"
     '((success . ((commandPanel . ((status . ())))))))
    (agent-repl-itest-composer--with-composer daemon 'open ref
      (ignore ref)
      ;; Act.
      (agent-repl--send agent-repl-itest-composer--ws "/status")
      (agent-repl-itest--await-call daemon "SubmitPrompt")
      ;; Assert.
      (agent-repl-itest--await-log daemon "elisp.input.command-answered" "info")
      (should (agent-repl-itest--logged-p daemon "elisp.input.command-answered" "info")))))

(ert-deftest agent-repl-itest-composer-merging-refusal-preserves-the-text ()
  "A `merging' refusal keeps the user's text: undelivered intent survives.
SubmitPromptError.merging is the daemon's own refusal arm; the composer
must not discard what the user typed."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest--script daemon "SubmitPrompt" '((error . ((merging . ())))))
    (agent-repl-itest-composer--with-composer daemon 'open ref
      (ignore ref)
      (let ((cleared nil))
        (add-hook 'agent-repl-send-posthooks (lambda (&rest _) (setq cleared t)))
        ;; Act.
        (agent-repl--send agent-repl-itest-composer--ws "run the tests")
        (agent-repl-itest--await-call daemon "SubmitPrompt")
        ;; Assert: the send did not complete, so no posthook ran.
        (should (null cleared))))))

(ert-deftest agent-repl-itest-composer-merging-gate-sends-no-rpc ()
  "The `merging' GATE closes the composer: no rpc is attempted at all.
The merge lease owns the session, and the gate is host-native, so Emacs
enforces it rather than letting the daemon refuse."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest-composer--with-composer daemon 'merging ref
      (ignore ref)
      ;; Act.
      (ignore-errors (agent-repl--send agent-repl-itest-composer--ws "run the tests"))
      ;; Assert.
      (should (null (agent-repl-itest--calls daemon "SubmitPrompt"))))))

(ert-deftest agent-repl-itest-composer-draining-gate-sends-no-rpc ()
  "The `draining' gate closes the composer: a scheduled shutdown is coming."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest-composer--with-composer daemon 'draining ref
      (ignore ref)
      ;; Act.
      (ignore-errors (agent-repl--send agent-repl-itest-composer--ws "run the tests"))
      ;; Assert.
      (should (null (agent-repl-itest--calls daemon "SubmitPrompt"))))))

(ert-deftest agent-repl-itest-composer-restarting-gate-sends-no-rpc ()
  "The `restarting' gate closes the composer during a graceful restart."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest-composer--with-composer daemon 'restarting ref
      (ignore ref)
      ;; Act.
      (ignore-errors (agent-repl--send agent-repl-itest-composer--ws "run the tests"))
      ;; Assert.
      (should (null (agent-repl-itest--calls daemon "SubmitPrompt"))))))

(ert-deftest agent-repl-itest-composer-merge-parked-gate-sends ()
  "`merge_parked' is OPEN WITH CONTEXT: the prompt is sent, not refused.
The merge gave up and wants guidance — everything submitted while parked
goes to the merge's resolution agent, never refused and never queued as
the session's own turn."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest-composer--with-composer daemon 'mergeParked ref
      ;; Act.
      (agent-repl--send agent-repl-itest-composer--ws "rebase onto master")
      (agent-repl-itest--await-call daemon "SubmitPrompt")
      ;; Assert.
      (let ((body (agent-repl-itest-composer--submit-body daemon)))
        (should (equal (agent-repl-itest--body-field body 'workspace 'id)
                       (plist-get ref :id)))))))

(ert-deftest agent-repl-itest-composer-no-session-gate-sends ()
  "A workspace with NO session simply submits: there is no precondition.
Ruled at kickoff — the daemon starts or revives the session implicitly."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (let ((conn (agent-repl-connect-open (agent-repl-itest-daemon-address daemon)))
          (ref nil))
      (unwind-protect
          (progn
            (agent-repl--ws-put agent-repl-itest-composer--ws
                                :project-dir agent-repl-itest-composer--dir)
            (agent-repl-host-register conn agent-repl-itest-composer--dir
                                      (lambda (minted) (setq ref minted)))
            (agent-repl-itest--wait-until (lambda () ref) nil
                                          "RegisterWorkspace to answer")
            (agent-repl-host-subscribe conn agent-repl-itest-composer--ws ref)
            (agent-repl-itest--await-subscriber daemon "host" (plist-get ref :id))
            (agent-repl-itest--push daemon "host"
                                    '((host . ((none . ()) (naming . ()))))
                                    (plist-get ref :id))
            (agent-repl-itest--wait-until
             (lambda () (eq (agent-repl-host-composer-gate
                             agent-repl-itest-composer--ws)
                            :no-session))
             nil "the :no-session gate")
            ;; Act.
            (agent-repl--send agent-repl-itest-composer--ws "start working")
            ;; Assert.
            (agent-repl-itest--await-call daemon "SubmitPrompt")
            (should (agent-repl-itest--calls daemon "SubmitPrompt")))
        (ignore-errors (agent-repl-host-unsubscribe agent-repl-itest-composer--ws))
        (agent-repl-connect-close conn)))))

(ert-deftest agent-repl-itest-composer-transport-failure-defers-the-prompt ()
  "A transport failure keeps the text and hands it to the outage queue.
Undelivered user intent may never be silently discarded; the queue drains
on link-up."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest-composer--with-composer daemon 'open ref
      (ignore ref)
      (let ((queued nil))
        (cl-letf (((symbol-function 'agent-repl-queue-deferred-prompt)
                   (lambda (&rest args) (push args queued))))
          ;; The daemon goes away mid-composition.
          (agent-repl-itest--stop-daemon daemon t)
          ;; Act.
          (ignore-errors (agent-repl--send agent-repl-itest-composer--ws "run the tests"))
          ;; Assert.
          (agent-repl-itest--wait-until (lambda () queued) nil
                                        "the prompt to reach the outage queue")
          (should queued))))))

(provide 'test-integration-composer)

;;; test-integration-composer.el ends here
