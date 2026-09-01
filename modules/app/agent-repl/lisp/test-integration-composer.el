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
;; NAME NOT IN THE SPEC (surfaced to the teamlead): §10 says clipboard-image.el
;; "registers the file as an attached ImageBlock{path, media_type} on the input
;; buffer" but names no function for it.  The suite calls it by this name.
(declare-function agent-repl-input-attach-image "clipboard-image")
(declare-function agent-repl-host-register "host")
(declare-function agent-repl-host-subscribe "host")
(declare-function agent-repl-host-unsubscribe "host")
(declare-function agent-repl-host-composer-gate "host")
(declare-function agent-repl-host-ref "host")
(declare-function agent-repl-host-forget "host")
(declare-function agent-repl-connect-open "connect")
(declare-function agent-repl-connect-close "connect")
(declare-function agent-repl--ws-put "workspace")
(declare-function agent-repl-input-mode "input")
(declare-function agent-repl-input-attachments "input")
(declare-function agent-repl-send-with-postfix "input")
(declare-function agent-repl-send-with-prefix "input")
(declare-function agent-repl-prompt-queue-pending "prompt-queue")
(declare-function agent-repl-prompt-queue-drain "prompt-queue")
(declare-function agent-repl--prompt-queue-on-link-up "prompt-queue")
(declare-function agent-repl-link-up-p "daemon-link")
(defvar agent-repl-send-posthooks)
(defvar agent-repl-host-update-functions)
(defvar agent-repl-send-postfix)
(defvar agent-repl-send-prefix)
;; Buffer-local var of the same name as the accessor function above.
(defvar agent-repl-input-attachments)

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
                          (,composer . ())))))
    (naming . ())))

(defconst agent-repl-itest-composer--gate-keywords
  '((open . :open) (merging . :merging) (draining . :draining)
    (restarting . :restarting) (mergeParked . :merge-parked))
  "Map a composer arm's protojson key symbol to its gate keyword.
The wire spellings a scenario passes to
`agent-repl-itest-composer--with-composer' are the same protojson keys
`agent-repl-itest-composer--live' embeds.")

(defmacro agent-repl-itest-composer--with-composer (daemon gate ref &rest body)
  "Register, subscribe and gate the composer's workspace on DAEMON, run BODY.
GATE is the composer arm's protojson key symbol; REF is bound to the
minted WorkspaceRef.  The host push is what sets the gate — the RESOLVED
ARM IS THE GATE, so a test never sets one directly.

The fixture workspace name is a single constant shared by every test in
this suite, and `agent-repl-host--by-name' is a global table this macro
does not own, so it forgets any prior entry first and then waits for the
SPECIFIC gate this push asked for (not merely \"no longer :unknown\") --
otherwise a stale non-:unknown gate left by an earlier test could satisfy
the wait before this scenario's own push has actually been delivered."
  (declare (indent 3) (debug (form form symbolp body)))
  `(let ((conn (agent-repl-connect-open (agent-repl-itest-daemon-address ,daemon)))
         (agent-repl-send-posthooks nil))
     (unwind-protect
         (let ((,ref nil))
           (ignore-errors (agent-repl-host-forget agent-repl-itest-composer--ws))
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
            (lambda () (eq (agent-repl-host-composer-gate agent-repl-itest-composer--ws)
                           (cdr (assq ,gate agent-repl-itest-composer--gate-keywords))))
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

(defun agent-repl-itest-composer--make-buffer (ws text)
  "Create a live `agent-repl-input-mode' buffer for WS containing TEXT.
Registers it as WS's `:input-buffer' and returns it; the caller kills it
and clears the registration on the way out."
  (let ((buf (generate-new-buffer (format " *agent-repl-itest-composer-%s*" ws))))
    (with-current-buffer buf
      (agent-repl-input-mode)
      (insert text))
    (agent-repl--ws-put ws :input-buffer buf)
    buf))

(defun agent-repl-itest-composer--kill-buffer (ws buf)
  "Unregister WS's `:input-buffer' and kill BUF."
  (agent-repl--ws-put ws :input-buffer nil)
  (when (buffer-live-p buf) (kill-buffer buf)))

(defun agent-repl-itest-composer--restart-on-same-dir (daemon)
  "Stop DAEMON, keeping its state dir, and start a fresh instance there.
Mirrors a real daemon restart: the same `AGENT_REPL_STATE_DIR' gets a new
`daemon.addr', which is what a link's own reconnect follows in production."
  (let ((dir (agent-repl-itest-daemon-state-dir daemon)))
    (agent-repl-itest--stop-daemon daemon t)
    (agent-repl-itest--start-daemon dir)))

(defun agent-repl-itest-composer--reattach (successor)
  "Re-register and re-subscribe the fixture workspace on SUCCESSOR.
Returns (CONN . REF).  RegisterWorkspace is idempotent by dir, so REF's id
is the one the original daemon minted -- the real reconnect path host.el
drives on link-up.  Pushes no host state; the caller decides the gate."
  (let ((conn (agent-repl-connect-open (agent-repl-itest-daemon-address successor)))
        (ref nil))
    (agent-repl-host-register conn agent-repl-itest-composer--dir
                              (lambda (minted) (setq ref minted)))
    (agent-repl-itest--wait-until (lambda () ref) nil
                                  "RegisterWorkspace on the successor to answer")
    (agent-repl-host-subscribe conn agent-repl-itest-composer--ws ref)
    (agent-repl-itest--await-subscriber successor "host" (plist-get ref :id))
    (cons conn ref)))

;;;; ---- Reading the fixture workspace's OWN log ----
;;
;; Once the fixture workspace has a registered `:project-dir', input.el and
;; host.el's WS-scoped records route to that workspace's OWN sink
;; (`agent-repl--workspace-emacs-log-target', a target file symlinked at
;; `<project-dir>/.claude/emacs/emacs.log') rather than to the daemon's
;; global JSONL `agent-repl-itest--log-records' reads.  A log assertion
;; about one of THIS suite's send sites must therefore read both.

(defun agent-repl-itest-composer--workspace-log-path ()
  "Absolute path of the fixture workspace's own log symlink."
  (expand-file-name ".claude/emacs/emacs.log" agent-repl-itest-composer--dir))

(defun agent-repl-itest-composer--workspace-log-records ()
  "Return the parsed JSONL records from the fixture workspace's own log.
A malformed final line is skipped, exactly like
`agent-repl-itest--log-records'."
  (let ((path (agent-repl-itest-composer--workspace-log-path))
        (records nil))
    (when (file-exists-p path)
      (with-temp-buffer
        (insert-file-contents path)
        (goto-char (point-min))
        (while (not (eobp))
          (let ((line (string-trim (buffer-substring (line-beginning-position)
                                                      (line-end-position)))))
            (unless (string-empty-p line)
              (condition-case nil
                  (push (json-parse-string line :object-type 'alist :array-type 'list
                                           :null-object :null :false-object :false)
                        records)
                (error nil))))
          (forward-line 1))))
    (nreverse records)))

(defun agent-repl-itest-composer--log-entries (daemon operation &optional level)
  "Return every OPERATION (at LEVEL) record for the fixture ws, either sink."
  (append (agent-repl-itest--log-entries daemon operation level)
          (seq-filter
           (lambda (record)
             (and (agent-repl-itest--operation-matches-p
                   (or (alist-get 'operation record) "") operation)
                  (or (null level) (equal (alist-get 'level record) level))))
           (agent-repl-itest-composer--workspace-log-records))))

(defun agent-repl-itest-composer--logged-p (daemon operation &optional level)
  "Return non-nil when OPERATION (at LEVEL) was logged, either sink."
  (consp (agent-repl-itest-composer--log-entries daemon operation level)))

(defun agent-repl-itest-composer--await-log (daemon operation &optional level)
  "Block until OPERATION (at LEVEL) is logged, in either sink the fixture ws uses."
  (agent-repl-itest--wait-until
   (lambda () (agent-repl-itest-composer--logged-p daemon operation level))
   nil (format "the production log to carry %s%s" operation
               (if level (format " at %s" level) ""))))

(defun agent-repl-itest-composer--log-names-arm-p (daemon operation arm-string)
  "Return non-nil when DAEMON's OPERATION log entry's context names ARM-STRING."
  (seq-some
   (lambda (entry)
     (seq-some (lambda (arg) (string-match-p (regexp-quote arm-string) arg))
               (agent-repl-itest--body-field entry 'context 'arguments)))
   (agent-repl-itest-composer--log-entries daemon operation "info")))

;;;; ---- The three REQUIRED request facts ----

(ert-deftest agent-repl-itest-composer-submit-carries-the-workspace-ref ()
  "Every Emacs submit carries `workspace', echoed from registration.
SubmitPromptRequest.workspace is REQUIRED (landing 2); the ref is the
daemon-minted echo token, never built from a path."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest-composer--with-composer daemon 'open ref
      ;; Act.
      (agent-repl--send :user-sent "run the tests" agent-repl-itest-composer--ws)
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
      (agent-repl--send :user-sent "run the tests" agent-repl-itest-composer--ws)
      (agent-repl-itest--await-call daemon "SubmitPrompt")
      ;; Assert: absence, not an empty value — PRESENCE, NEVER SENTINELS.
      (let ((body (agent-repl-itest-composer--submit-body daemon)))
        (should (null (assq 'feed body)))))))

(defconst agent-repl-itest-composer--uuid-v4-regexp
  "\\`[0-9a-f]\\{8\\}-[0-9a-f]\\{4\\}-4[0-9a-f]\\{3\\}-[89ab][0-9a-f]\\{3\\}-[0-9a-f]\\{12\\}\\'"
  "Exact shape of an RFC 4122 v4 UUID as `agent-repl--uuid' mints it.")

(ert-deftest agent-repl-itest-composer-submit-carries-a-fresh-idempotency-key ()
  "Each submit mints its own idempotency key, shaped as an RFC 4122 v4 UUID.
Two submits sharing a key would let the daemon collapse them into one; a
key of the wrong SHAPE (finding 65) would mean `agent-repl--uuid' stopped
setting the version/variant nibbles it documents."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest-composer--with-composer daemon 'open ref
      (ignore ref)
      ;; Act.
      (agent-repl--send :user-sent "first" agent-repl-itest-composer--ws)
      (agent-repl-itest--await-call daemon "SubmitPrompt" 1)
      (agent-repl--send :user-sent "second" agent-repl-itest-composer--ws)
      (agent-repl-itest--await-call daemon "SubmitPrompt" 2)
      ;; Assert.
      (let ((first (agent-repl-itest--body-field
                    (agent-repl-itest-composer--submit-body daemon 0) 'idempotencyKey))
            (second (agent-repl-itest--body-field
                     (agent-repl-itest-composer--submit-body daemon 1) 'idempotencyKey)))
        (should first)
        (should-not (equal first second))
        ;; "`(agent-repl--uuid)' (RFC 4122 v4 from `random')" -- shape, not
        ;; just inequality.
        (should (string-match-p agent-repl-itest-composer--uuid-v4-regexp first))
        (should (string-match-p agent-repl-itest-composer--uuid-v4-regexp second))))))

(ert-deftest agent-repl-itest-composer-submit-carries-its-send-sites-origin ()
  "Each send site sends its OWN PromptOrigin, and never UNSPECIFIED.
SubmitPromptRequest.origin is REQUIRED (E1 resolved); the fake refuses
UNSPECIFIED, so a site that forgot its origin cannot pass."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest-composer--with-composer daemon 'open ref
      (ignore ref)
      ;; Act.
      (agent-repl--send :user-sent "run the tests" agent-repl-itest-composer--ws)
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
      (agent-repl--send :user-sent-with-metaprompt "run the tests"
                        agent-repl-itest-composer--ws)
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
      (agent-repl--send :deferred-prompt "run the tests"
                        agent-repl-itest-composer--ws)
      (agent-repl-itest--await-call daemon "SubmitPrompt")
      ;; Assert.
      (let ((origin (agent-repl-itest--body-field
                     (agent-repl-itest-composer--submit-body daemon) 'origin)))
        (should (equal origin "PROMPT_ORIGIN_DEFERRED_PROMPT"))))))

;;;; ---- Finding 56: every remaining send site's own origin ----
;;
;; §10 "Sites: user-sent, user-sent-and-hide, user-sent-with-metaprompt,
;; user-sent-with-postfix, user-sent-with-prefix, metaprompt-read,
;; command-diff-analysis, command-explain-context, command-explain-prompt,
;; command-update-pr, command-rebase, command-create-or-update-pr,
;; deferred-prompt."  user-sent, user-sent-with-metaprompt and
;; deferred-prompt are pinned above; every other site is table-driven here,
;; one deftest per site (the suite does not already thread a dolist through
;; a single deftest, so each row stays its own test).

(defmacro agent-repl-itest-composer--deftest-origin (name origin-keyword wire-string)
  "Define a deftest NAME asserting ORIGIN-KEYWORD encodes as WIRE-STRING."
  (declare (indent 2))
  `(ert-deftest ,name ()
     ,(format "The %S send site sends %s.
Every Emacs send site has EXACTLY ONE production origin; a site that sent
the wrong value would silently mismark a stored turn's editor situation."
              origin-keyword wire-string)
     ;; Arrange.
     (agent-repl-itest--with-fake-daemon daemon
       (agent-repl-itest-composer--with-composer daemon 'open ref
         (ignore ref)
         ;; Act.
         (agent-repl--send ,origin-keyword "run the tests" agent-repl-itest-composer--ws)
         (agent-repl-itest--await-call daemon "SubmitPrompt")
         ;; Assert.
         (let ((origin (agent-repl-itest--body-field
                        (agent-repl-itest-composer--submit-body daemon) 'origin)))
           (should (equal origin ,wire-string)))))))

(agent-repl-itest-composer--deftest-origin
    agent-repl-itest-composer-user-sent-and-hide-site-sends-its-own-origin
  :user-sent-and-hide "PROMPT_ORIGIN_USER_SENT_AND_HIDE")

(agent-repl-itest-composer--deftest-origin
    agent-repl-itest-composer-user-sent-with-postfix-site-sends-its-own-origin
  :user-sent-with-postfix "PROMPT_ORIGIN_USER_SENT_WITH_POSTFIX")

(agent-repl-itest-composer--deftest-origin
    agent-repl-itest-composer-user-sent-with-prefix-site-sends-its-own-origin
  :user-sent-with-prefix "PROMPT_ORIGIN_USER_SENT_WITH_PREFIX")

(agent-repl-itest-composer--deftest-origin
    agent-repl-itest-composer-metaprompt-read-site-sends-its-own-origin
  :metaprompt-read "PROMPT_ORIGIN_METAPROMPT_READ")

(agent-repl-itest-composer--deftest-origin
    agent-repl-itest-composer-command-diff-analysis-site-sends-its-own-origin
  :command-diff-analysis "PROMPT_ORIGIN_COMMAND_DIFF_ANALYSIS")

(agent-repl-itest-composer--deftest-origin
    agent-repl-itest-composer-command-explain-context-site-sends-its-own-origin
  :command-explain-context "PROMPT_ORIGIN_COMMAND_EXPLAIN_CONTEXT")

(agent-repl-itest-composer--deftest-origin
    agent-repl-itest-composer-command-explain-prompt-site-sends-its-own-origin
  :command-explain-prompt "PROMPT_ORIGIN_COMMAND_EXPLAIN_PROMPT")

(agent-repl-itest-composer--deftest-origin
    agent-repl-itest-composer-command-update-pr-site-sends-its-own-origin
  :command-update-pr "PROMPT_ORIGIN_COMMAND_UPDATE_PR")

(agent-repl-itest-composer--deftest-origin
    agent-repl-itest-composer-command-rebase-site-sends-its-own-origin
  :command-rebase "PROMPT_ORIGIN_COMMAND_REBASE")

(agent-repl-itest-composer--deftest-origin
    agent-repl-itest-composer-command-create-or-update-pr-site-sends-its-own-origin
  :command-create-or-update-pr "PROMPT_ORIGIN_COMMAND_CREATE_OR_UPDATE_PR")

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
      (agent-repl--send :user-sent "run the tests" agent-repl-itest-composer--ws)
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
                       agent-repl-itest-composer--ws "run the tests" t)))
        ;; Act.
        (agent-repl--send :user-sent-with-metaprompt prepared
                          agent-repl-itest-composer--ws)
        (agent-repl-itest--await-call daemon "SubmitPrompt")
        ;; Assert: what is submitted is what was composed, unrewritten.
        (let ((texts (agent-repl-itest-composer--text-blocks
                      (agent-repl-itest-composer--submit-body daemon))))
          (should (equal (car texts) prepared)))))))

(ert-deftest agent-repl-itest-composer-postfix-site-sends-composed-text-and-origin ()
  "`agent-repl-send-with-postfix' submits raw+postfix under its own origin.
Scenario 12 \"prefix/postfix\": PROMPT COMPOSITION IS FREE, so what is
submitted is the raw text with `agent-repl-send-postfix' appended -- and
`agent-repl-send-with-postfix' is the ONE production site for
PROMPT_ORIGIN_USER_SENT_WITH_POSTFIX."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest-composer--with-composer daemon 'open ref
      (ignore ref)
      (let ((buf (agent-repl-itest-composer--make-buffer
                  agent-repl-itest-composer--ws "run the tests")))
        (unwind-protect
            (cl-letf (((symbol-function 'agent-repl--ws-current-name)
                       (lambda () agent-repl-itest-composer--ws))
                      ((symbol-function 'agent-repl--ws-current-log-name)
                       (lambda () agent-repl-itest-composer--ws)))
              ;; Act.
              (agent-repl-send-with-postfix)
              (agent-repl-itest--await-call daemon "SubmitPrompt")
              ;; Assert.
              (let* ((body (agent-repl-itest-composer--submit-body daemon))
                     (texts (agent-repl-itest-composer--text-blocks body)))
                (should (equal (car texts)
                               (concat "run the tests" agent-repl-send-postfix)))
                (should (equal (agent-repl-itest--body-field body 'origin)
                               "PROMPT_ORIGIN_USER_SENT_WITH_POSTFIX"))))
          (agent-repl-itest-composer--kill-buffer agent-repl-itest-composer--ws buf))))))

(ert-deftest agent-repl-itest-composer-prefix-site-sends-composed-text-and-origin ()
  "`agent-repl-send-with-prefix' submits prefix+raw under its own origin.
Scenario 12 \"prefix/postfix\": the composed text is `agent-repl-send-prefix'
prepended to the raw input, submitted verbatim, under
PROMPT_ORIGIN_USER_SENT_WITH_PREFIX -- its ONE production site."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest-composer--with-composer daemon 'open ref
      (ignore ref)
      (let ((buf (agent-repl-itest-composer--make-buffer
                  agent-repl-itest-composer--ws "run the tests")))
        (unwind-protect
            (cl-letf (((symbol-function 'agent-repl--ws-current-name)
                       (lambda () agent-repl-itest-composer--ws))
                      ((symbol-function 'agent-repl--ws-current-log-name)
                       (lambda () agent-repl-itest-composer--ws)))
              ;; Act.
              (agent-repl-send-with-prefix)
              (agent-repl-itest--await-call daemon "SubmitPrompt")
              ;; Assert.
              (let* ((body (agent-repl-itest-composer--submit-body daemon))
                     (texts (agent-repl-itest-composer--text-blocks body)))
                (should (equal (car texts)
                               (concat agent-repl-send-prefix "run the tests")))
                (should (equal (agent-repl-itest--body-field body 'origin)
                               "PROMPT_ORIGIN_USER_SENT_WITH_PREFIX"))))
          (agent-repl-itest-composer--kill-buffer agent-repl-itest-composer--ws buf))))))

(ert-deftest agent-repl-itest-composer-attached-image-travels-as-a-path-block ()
  "A pasted image travels as `ImageBlock{path}' beside the text.
Ruled at kickoff: pasted images travel as ImageBlock{path}; the composer
keeps the attachment list per buffer and clears it on a successful send."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest-composer--with-composer daemon 'open ref
      (ignore ref)
      (let ((image (expand-file-name "pasted.png" agent-repl-itest-composer--dir))
            (buf (agent-repl-itest-composer--make-buffer
                  agent-repl-itest-composer--ws "look at this")))
        (unwind-protect
            (progn
              ;; Act.
              (with-current-buffer buf (agent-repl-input-attach-image image "image/png"))
              (agent-repl--send :user-sent nil agent-repl-itest-composer--ws)
              (agent-repl-itest--await-call daemon "SubmitPrompt")
              ;; Assert.
              (let ((body (agent-repl-itest-composer--submit-body daemon)))
                (should (equal (agent-repl-itest-composer--image-paths body) (list image)))))
          (agent-repl-itest-composer--kill-buffer agent-repl-itest-composer--ws buf))))))

(ert-deftest agent-repl-itest-composer-attached-image-carries-its-media-type ()
  "An ImageBlock carries its `media_type' beside the location oneof.
`media_type' is a sibling of the oneof, not part of it: the location says
WHERE the bytes are, the media type says WHAT they are."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest-composer--with-composer daemon 'open ref
      (ignore ref)
      (let ((image (expand-file-name "pasted.png" agent-repl-itest-composer--dir))
            (buf (agent-repl-itest-composer--make-buffer
                  agent-repl-itest-composer--ws "look at this")))
        (unwind-protect
            (progn
              ;; Act.
              (with-current-buffer buf (agent-repl-input-attach-image image "image/png"))
              (agent-repl--send :user-sent nil agent-repl-itest-composer--ws)
              (agent-repl-itest--await-call daemon "SubmitPrompt")
              ;; Assert.
              (let* ((body (agent-repl-itest-composer--submit-body daemon))
                     (blocks (agent-repl-itest--body-field body 'said 'content 'blocks))
                     (image-block (seq-find (lambda (block) (assq 'image block)) blocks)))
                (should (equal (agent-repl-itest--body-field image-block 'image 'mediaType)
                               "image/png"))))
          (agent-repl-itest-composer--kill-buffer agent-repl-itest-composer--ws buf))))))

(ert-deftest agent-repl-itest-composer-attachments-clear-after-a-successful-turn-send ()
  "Attachments clear on a successful `turn' send: the NEXT send has none.
§10: the attachment list is \"cleared on a successful send\" -- a second
submit that still carried the first image would resend it twice."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest-composer--with-composer daemon 'open ref
      (ignore ref)
      (let ((image (expand-file-name "pasted.png" agent-repl-itest-composer--dir))
            (buf (agent-repl-itest-composer--make-buffer
                  agent-repl-itest-composer--ws "look at this")))
        (unwind-protect
            (progn
              ;; Act: first send, with the attachment.
              (with-current-buffer buf (agent-repl-input-attach-image image "image/png"))
              (agent-repl--send :user-sent nil agent-repl-itest-composer--ws)
              (agent-repl-itest--await-call daemon "SubmitPrompt" 1)
              (should (equal (agent-repl-itest-composer--image-paths
                              (agent-repl-itest-composer--submit-body daemon 0))
                             (list image)))
              ;; Act: second send, nothing newly attached.
              (with-current-buffer buf (insert "and this"))
              (agent-repl--send :user-sent nil agent-repl-itest-composer--ws)
              (agent-repl-itest--await-call daemon "SubmitPrompt" 2)
              ;; Assert: no image block travels a second time.
              (should (null (agent-repl-itest-composer--image-paths
                             (agent-repl-itest-composer--submit-body daemon 1)))))
          (agent-repl-itest-composer--kill-buffer agent-repl-itest-composer--ws buf))))))

(ert-deftest agent-repl-itest-composer-attachments-survive-a-merging-refusal ()
  "A `merging' refusal must not clear the attachment: it was never delivered.
§10: attachments clear ONLY on a successful send; a refused submission
that dropped the image would silently lose it."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest--script daemon "SubmitPrompt" '((error . ((merging . ())))))
    (agent-repl-itest-composer--with-composer daemon 'open ref
      (ignore ref)
      (let ((image (expand-file-name "pasted.png" agent-repl-itest-composer--dir))
            (buf (agent-repl-itest-composer--make-buffer
                  agent-repl-itest-composer--ws "look at this")))
        (unwind-protect
            (progn
              ;; Act.
              (with-current-buffer buf (agent-repl-input-attach-image image "image/png"))
              (agent-repl--send :user-sent nil agent-repl-itest-composer--ws)
              (agent-repl-itest--await-call daemon "SubmitPrompt")
              (agent-repl-itest-composer--await-log daemon "elisp.input.refused-merging" "warn")
              ;; Assert: the attachment SURVIVES the refusal.
              (should (equal (with-current-buffer buf agent-repl-input-attachments)
                             (list (list :path image :media-type "image/png")))))
          (agent-repl-itest-composer--kill-buffer agent-repl-itest-composer--ws buf))))))

(ert-deftest agent-repl-itest-composer-two-images-travel-text-first-then-images-in-order ()
  "Two attached images travel as the text block, then each image in order.
§10: blocks are \"the text block(s) plus one ... image block per image\",
in the order the person composed them -- attach order, not reverse."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest-composer--with-composer daemon 'open ref
      (ignore ref)
      (let* ((first (expand-file-name "first.png" agent-repl-itest-composer--dir))
             (second (expand-file-name "second.jpg" agent-repl-itest-composer--dir))
             (buf (agent-repl-itest-composer--make-buffer
                   agent-repl-itest-composer--ws "compare these")))
        (unwind-protect
            (progn
              ;; Act.
              (with-current-buffer buf
                (agent-repl-input-attach-image first "image/png")
                (agent-repl-input-attach-image second "image/jpeg"))
              (agent-repl--send :user-sent nil agent-repl-itest-composer--ws)
              (agent-repl-itest--await-call daemon "SubmitPrompt")
              ;; Assert: block order is text, then image, then image.
              (let* ((body (agent-repl-itest-composer--submit-body daemon))
                     (blocks (agent-repl-itest--body-field body 'said 'content 'blocks)))
                (should (equal (mapcar #'caar blocks) '(text image image)))
                (should (equal (agent-repl-itest--body-field (nth 1 blocks) 'image 'path 'path)
                               first))
                (should (equal (agent-repl-itest--body-field (nth 1 blocks) 'image 'mediaType)
                               "image/png"))
                (should (equal (agent-repl-itest--body-field (nth 2 blocks) 'image 'path 'path)
                               second))
                (should (equal (agent-repl-itest--body-field (nth 2 blocks) 'image 'mediaType)
                               "image/jpeg"))))
          (agent-repl-itest-composer--kill-buffer agent-repl-itest-composer--ws buf))))))

(ert-deftest agent-repl-itest-composer-success-turn-runs-the-send-posthooks ()
  "A `turn' outcome clears the input, pushes history, and runs the posthooks.
§10: success `:turn' -> \"clear the input, push history, run
`agent-repl-send-posthooks'\"; `turn' is the only outcome that means
\"something to await\"."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest-composer--with-composer daemon 'open ref
      (ignore ref)
      (let ((ran nil)
            (buf (agent-repl-itest-composer--make-buffer
                  agent-repl-itest-composer--ws "run the tests")))
        (unwind-protect
            (progn
              (setq agent-repl-send-posthooks
                    (list (cons "" (lambda (_ws _raw) (push t ran)))))
              ;; Act.
              (agent-repl--send :user-sent nil agent-repl-itest-composer--ws)
              ;; Assert.
              (agent-repl-itest--wait-until (lambda () ran) nil "the send posthooks")
              (should ran)
              ;; Assert: the input is EMPTY, not merely different.
              (should (equal (with-current-buffer buf (buffer-string)) ""))
              ;; Assert: history's newest entry is the sent text.
              (should (equal (car (buffer-local-value 'agent-repl--input-history buf))
                             "run the tests")))
          (agent-repl-itest-composer--kill-buffer agent-repl-itest-composer--ws buf))))))

(ert-deftest agent-repl-itest-composer-command-refused-is-answered-not-awaited ()
  "`command_refused' means \"answered, nothing to await\": input clears, logged.
Both non-turn success arms mean that; the webapp draws the refusal card,
and Emacs clears the input and logs the arm."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest--script
     daemon "SubmitPrompt"
     '((success . ((commandRefused . ((command . "/agents")))))))
    (agent-repl-itest-composer--with-composer daemon 'open ref
      (ignore ref)
      (let ((buf (agent-repl-itest-composer--make-buffer
                  agent-repl-itest-composer--ws "/agents")))
        (unwind-protect
            (progn
              ;; Act.
              (agent-repl--send :user-sent nil agent-repl-itest-composer--ws)
              (agent-repl-itest--await-call daemon "SubmitPrompt")
              ;; Assert.
              (agent-repl-itest-composer--await-log daemon "elisp.input.command-answered" "info")
              (should (agent-repl-itest-composer--logged-p daemon "elisp.input.command-answered" "info"))
              ;; Assert: the input is cleared.
              (agent-repl-itest--wait-until
               (lambda () (equal (with-current-buffer buf (buffer-string)) ""))
               nil "the composer to clear")
              (should (equal (with-current-buffer buf (buffer-string)) ""))
              ;; Assert: the log context names the answered ARM.
              (should (agent-repl-itest-composer--log-names-arm-p
                       daemon "elisp.input.command-answered" ":command-refused")))
          (agent-repl-itest-composer--kill-buffer agent-repl-itest-composer--ws buf))))))

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
      (let ((buf (agent-repl-itest-composer--make-buffer
                  agent-repl-itest-composer--ws "/status")))
        (unwind-protect
            (progn
              ;; Act.
              (agent-repl--send :user-sent nil agent-repl-itest-composer--ws)
              (agent-repl-itest--await-call daemon "SubmitPrompt")
              ;; Assert.
              (agent-repl-itest-composer--await-log daemon "elisp.input.command-answered" "info")
              (should (agent-repl-itest-composer--logged-p daemon "elisp.input.command-answered" "info"))
              ;; Assert: the input is cleared.
              (agent-repl-itest--wait-until
               (lambda () (equal (with-current-buffer buf (buffer-string)) ""))
               nil "the composer to clear")
              (should (equal (with-current-buffer buf (buffer-string)) ""))
              ;; Assert: the log context names the answered ARM.
              (should (agent-repl-itest-composer--log-names-arm-p
                       daemon "elisp.input.command-answered" ":command-panel")))
          (agent-repl-itest-composer--kill-buffer agent-repl-itest-composer--ws buf))))))

(ert-deftest agent-repl-itest-composer-merging-refusal-preserves-the-text ()
  "A `merging' refusal keeps the user's text: undelivered intent survives.
SubmitPromptError.merging is the daemon's own refusal arm; the composer
must not discard what the user typed, and must flash the exact refusal
message -- \"keep the text, `message' + a mode-line flash \\='refused:
merge in flight\\='\"."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest--script daemon "SubmitPrompt" '((error . ((merging . ())))))
    (agent-repl-itest-composer--with-composer daemon 'open ref
      (ignore ref)
      (let ((cleared nil)
            (messages nil)
            (buf (agent-repl-itest-composer--make-buffer
                  agent-repl-itest-composer--ws "run the tests")))
        (unwind-protect
            (progn
              (setq agent-repl-send-posthooks
                    (list (cons "" (lambda (_ws _raw) (setq cleared t)))))
              (cl-letf (((symbol-function 'message)
                         (lambda (fmt &rest args)
                           (push (if args (apply #'format fmt args) fmt) messages)
                           nil)))
                ;; Act.
                (agent-repl--send :user-sent nil agent-repl-itest-composer--ws)
                (agent-repl-itest--await-call daemon "SubmitPrompt")
                ;; `message' is also the quiet sink's *Messages* output, so
                ;; other lines land in MESSAGES too; wait for and find the
                ;; refusal text specifically rather than assuming position.
                (agent-repl-itest--wait-until
                 (lambda ()
                   (member "agent-repl: refused -- a merge is in flight for this workspace"
                           messages))
                 nil "the refusal message")
                ;; Assert: the send did not complete, so no posthook ran.
                (should (null cleared))
                ;; Assert: the exact refused message.
                (should (member "agent-repl: refused -- a merge is in flight for this workspace"
                               messages)))
              ;; Assert: the composer keeps the text, unchanged.
              (should (equal (with-current-buffer buf (buffer-string)) "run the tests")))
          (agent-repl-itest-composer--kill-buffer agent-repl-itest-composer--ws buf))))))

(defmacro agent-repl-itest-composer--deftest-refusing-gate (name gate-symbol expected-text)
  "Define a deftest NAME pinning GATE-SYMBOL's refusal to EXPECTED-TEXT.
Finding 59: captures the EXACT `user-error' text (never just \"no rpc\")
and asserts the composer's buffer still holds the user's words."
  (declare (indent 2))
  `(ert-deftest ,name ()
     ,(format "The `%s' gate refuses with its own fixed text and keeps the input.
`agent-repl--input-gate-refusals' names %S's message; a refusal that lost
the exact wording or erased the input would strand undelivered intent."
              gate-symbol expected-text)
     ;; Arrange.
     (agent-repl-itest--with-fake-daemon daemon
       (agent-repl-itest-composer--with-composer daemon ',gate-symbol ref
         (ignore ref)
         (let ((buf (agent-repl-itest-composer--make-buffer
                     agent-repl-itest-composer--ws "run the tests")))
           (unwind-protect
               (let ((err (should-error
                           (agent-repl--send :user-sent nil agent-repl-itest-composer--ws)
                           :type 'user-error)))
                 ;; Assert: the EXACT fixed refusal text, not just "it refused".
                 (should (equal (error-message-string err) ,expected-text))
                 ;; Assert: the composer keeps every word the user wrote.
                 (should (equal (with-current-buffer buf (buffer-string)) "run the tests"))
                 (should (null (agent-repl-itest--calls daemon "SubmitPrompt"))))
             (agent-repl-itest-composer--kill-buffer agent-repl-itest-composer--ws buf)))))))

(agent-repl-itest-composer--deftest-refusing-gate
    agent-repl-itest-composer-merging-gate-sends-no-rpc
  merging "composer closed: a merge owns this session")

(agent-repl-itest-composer--deftest-refusing-gate
    agent-repl-itest-composer-draining-gate-sends-no-rpc
  draining "composer closed: daemon draining")

(agent-repl-itest-composer--deftest-refusing-gate
    agent-repl-itest-composer-restarting-gate-sends-no-rpc
  restarting "composer closed: restarting")

(ert-deftest agent-repl-itest-composer-merge-parked-gate-sends ()
  "`merge_parked' is OPEN WITH CONTEXT: the prompt is sent, not refused.
The merge gave up and wants guidance — everything submitted while parked
goes to the merge's resolution agent, never refused and never queued as
the session's own turn."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest-composer--with-composer daemon 'mergeParked ref
      ;; Act.
      (agent-repl--send :user-sent "rebase onto master" agent-repl-itest-composer--ws)
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
            (agent-repl--send :user-sent "start working" agent-repl-itest-composer--ws)
            ;; Assert.
            (agent-repl-itest--await-call daemon "SubmitPrompt")
            (should (agent-repl-itest--calls daemon "SubmitPrompt")))
        (ignore-errors (agent-repl-host-unsubscribe agent-repl-itest-composer--ws))
        (agent-repl-connect-close conn)))))

(ert-deftest agent-repl-itest-composer-terminal-gate-sends ()
  "The `:terminal' gate simply sends: SubmitPrompt has no precondition.
§7: \"`:no-session' / `:terminal' SEND (ruled: SubmitPrompt has no
precondition; the daemon starts or revives the session implicitly)\"."
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
            (agent-repl-itest--push
             daemon "host"
             '((host . ((existing . ((id . ((value . "host-session-1")))
                                      (terminal . ((rehydratable . t)))))
                        (naming . ()))))
             (plist-get ref :id))
            (agent-repl-itest--wait-until
             (lambda () (eq (agent-repl-host-composer-gate agent-repl-itest-composer--ws)
                            :terminal))
             nil "the :terminal gate")
            ;; Act.
            (agent-repl--send :user-sent "run the tests" agent-repl-itest-composer--ws)
            ;; Assert.
            (agent-repl-itest--await-call daemon "SubmitPrompt")
            (should (agent-repl-itest--calls daemon "SubmitPrompt")))
        (ignore-errors (agent-repl-host-unsubscribe agent-repl-itest-composer--ws))
        (agent-repl-connect-close conn)))))

(ert-deftest agent-repl-itest-composer-unknown-gate-sends-and-logs-info ()
  "The `:unknown' gate (no host push yet) sends, logging INFO.
§7: \"`:unknown' (no host push yet) SEND as well, logging INFO -- the
daemon is the authority and answers with its own refusal arms\"."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    ;; A clean slate: no push in THIS test must mean :unknown, regardless of
    ;; what an earlier test in this process left in the shared host table.
    (ignore-errors (agent-repl-host-forget agent-repl-itest-composer--ws))
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
            ;; No push at all: the gate reads :unknown.
            (should (eq (agent-repl-host-composer-gate agent-repl-itest-composer--ws)
                       :unknown))
            ;; Act.
            (agent-repl--send :user-sent "run the tests" agent-repl-itest-composer--ws)
            ;; Assert.
            (agent-repl-itest--await-call daemon "SubmitPrompt")
            (should (agent-repl-itest--calls daemon "SubmitPrompt"))
            (agent-repl-itest-composer--await-log daemon "elisp.input.gate-unknown-sends" "info")
            (should (agent-repl-itest-composer--logged-p daemon "elisp.input.gate-unknown-sends" "info")))
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
      ;; The daemon goes away mid-composition.
      (agent-repl-itest--stop-daemon daemon t)
      ;; Act.
      (ignore-errors (agent-repl--send :user-sent "run the tests"
                                       agent-repl-itest-composer--ws))
      ;; Assert: the OUTAGE queue holds it -- link-up releases it, not the
      ;; roster's finish edge, so `agent-repl-queue-deferred-prompt' is the
      ;; wrong sink to watch.
      (agent-repl-itest--wait-until
       (lambda () (agent-repl-prompt-queue-pending agent-repl-itest-composer--ws :outage))
       nil "the prompt to reach the outage queue")
      (should (agent-repl-prompt-queue-pending agent-repl-itest-composer--ws :outage)))))

;;;; ---- Finding 91 (partial): the submit log names its origin ----

(ert-deftest agent-repl-itest-composer-submit-log-carries-the-origin ()
  "`elisp.input.submit' logs the send's own origin in its context.
input.el: \"elisp.input.submit ws=%s origin=%S key=%s blocks=%d\" -- a
log line that dropped the origin would make a submission untraceable to
the editor situation that caused it."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest-composer--with-composer daemon 'open ref
      (ignore ref)
      ;; Act.
      (agent-repl--send :user-sent-with-prefix "run the tests"
                        agent-repl-itest-composer--ws)
      (agent-repl-itest--await-call daemon "SubmitPrompt")
      ;; Assert.
      (agent-repl-itest-composer--await-log daemon "elisp.input.submit" "info")
      (should (agent-repl-itest-composer--log-names-arm-p
               daemon "elisp.input.submit" ":user-sent-with-prefix")))))

;;;; ---- Findings 66-68: the outage queue, end to end ----

(ert-deftest agent-repl-itest-composer-outage-drain-reuses-the-failed-attempts-idempotency-key ()
  "A prompt re-sent from the outage queue reuses the failed attempt's key.
endpoint_submit_prompt.proto: \"a retried request is not a second turn\" --
ruled: a re-drive is a RETRY, so it must carry the SAME idempotency key as
the failed attempt, letting the daemon's duplicate refusal do its job."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest-composer--with-composer daemon 'open ref
      (ignore ref)
      (let (failed-key successor adopted)
        (unwind-protect
            (progn
              ;; The daemon goes away mid-composition.
              (setq successor (agent-repl-itest-composer--restart-on-same-dir daemon))
              ;; Act: the failed attempt -- `agent-repl--send' returns its key
              ;; synchronously even though the transport fails asynchronously.
              (setq failed-key
                    (agent-repl--send :user-sent "run the tests"
                                      agent-repl-itest-composer--ws))
              (should failed-key)
              (agent-repl-itest--wait-until
               (lambda () (agent-repl-prompt-queue-pending
                           agent-repl-itest-composer--ws :outage))
               nil "the prompt to reach the outage queue")
              ;; The successor comes up and the workspace re-attaches to it,
              ;; then the link-up edge drains the outage queue.
              (setq adopted (agent-repl-itest-composer--reattach successor))
              (agent-repl-itest--push
               successor "host" `((host . ,(agent-repl-itest-composer--live 'open)))
               (plist-get (cdr adopted) :id))
              (agent-repl-itest--wait-until
               (lambda () (eq (agent-repl-host-composer-gate agent-repl-itest-composer--ws)
                              :open))
               nil "the successor's composer gate to open")
              (cl-letf (((symbol-function 'agent-repl-link-up-p) (lambda () t)))
                (agent-repl--prompt-queue-on-link-up))
              (agent-repl-itest--await-call successor "SubmitPrompt")
              ;; Assert.
              (let ((drained-key (agent-repl-itest--body-field
                                  (car (agent-repl-itest--call-bodies successor "SubmitPrompt"))
                                  'idempotencyKey)))
                (should (equal drained-key failed-key))))
          (when adopted (agent-repl-connect-close (car adopted)))
          (when successor (agent-repl-itest--stop-daemon successor t)))))))

(ert-deftest agent-repl-itest-composer-outage-drain-reaches-the-successor-end-to-end ()
  "A held outage prompt is really delivered on link-up, to the SUCCESSOR daemon.
§10: \"transport failure -> keep the text and offer it to prompt-queue.el
(drained on link-up)\" -- this exercises the real drain, not a stubbed
queue, and pins the drained request's origin and text."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest-composer--with-composer daemon 'open ref
      (ignore ref)
      (let (successor adopted)
        (unwind-protect
            (progn
              ;; The daemon goes away mid-composition.
              (setq successor (agent-repl-itest-composer--restart-on-same-dir daemon))
              ;; Act.
              (ignore-errors
                (agent-repl--send :user-sent "run the tests" agent-repl-itest-composer--ws))
              (agent-repl-itest--wait-until
               (lambda () (agent-repl-prompt-queue-pending
                           agent-repl-itest-composer--ws :outage))
               nil "the prompt to reach the outage queue")
              (setq adopted (agent-repl-itest-composer--reattach successor))
              (agent-repl-itest--push
               successor "host" `((host . ,(agent-repl-itest-composer--live 'open)))
               (plist-get (cdr adopted) :id))
              (agent-repl-itest--wait-until
               (lambda () (eq (agent-repl-host-composer-gate agent-repl-itest-composer--ws)
                              :open))
               nil "the successor's composer gate to open")
              (cl-letf (((symbol-function 'agent-repl-link-up-p) (lambda () t)))
                (agent-repl--prompt-queue-on-link-up))
              ;; Assert: it landed on the SUCCESSOR (the original daemon is
              ;; dead and cannot have received anything).
              (agent-repl-itest--await-call successor "SubmitPrompt")
              (let* ((body (car (agent-repl-itest--call-bodies successor "SubmitPrompt")))
                     (texts (agent-repl-itest-composer--text-blocks body)))
                (should (equal (agent-repl-itest--body-field body 'origin)
                               "PROMPT_ORIGIN_DEFERRED_PROMPT"))
                (should (equal (car texts) "run the tests"))))
          (when adopted (agent-repl-connect-close (car adopted)))
          (when successor (agent-repl-itest--stop-daemon successor t)))))))

(ert-deftest agent-repl-itest-composer-outage-drain-waits-for-the-gate-to-open ()
  "Link-up alone does not drain into a refusing gate; the gate must open too.
§10: \"Its liveness gate is `agent-repl-link-up-p' and the composer
gate\" -- both facts, not just link-up, must hold before a drain sends."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest-composer--with-composer daemon 'open ref
      (ignore ref)
      (let (successor adopted)
        (unwind-protect
            (progn
              ;; The daemon goes away mid-composition, with the queue outage.
              (setq successor (agent-repl-itest-composer--restart-on-same-dir daemon))
              (ignore-errors
                (agent-repl--send :user-sent "run the tests" agent-repl-itest-composer--ws))
              (agent-repl-itest--wait-until
               (lambda () (agent-repl-prompt-queue-pending
                           agent-repl-itest-composer--ws :outage))
               nil "the prompt to reach the outage queue")
              ;; The successor comes up, but its composer gate is `merging'.
              (setq adopted (agent-repl-itest-composer--reattach successor))
              (agent-repl-itest--push
               successor "host" `((host . ,(agent-repl-itest-composer--live 'merging)))
               (plist-get (cdr adopted) :id))
              (agent-repl-itest--wait-until
               (lambda () (eq (agent-repl-host-composer-gate agent-repl-itest-composer--ws)
                              :merging))
               nil "the successor's composer gate to read :merging")
              ;; Act: the link-up edge fires.
              (cl-letf (((symbol-function 'agent-repl-link-up-p) (lambda () t)))
                (agent-repl--prompt-queue-on-link-up))
              ;; Assert: the merging gate refused the drain -- nothing sent.
              (should (null (agent-repl-itest--calls successor "SubmitPrompt")))
              (should (agent-repl-prompt-queue-pending agent-repl-itest-composer--ws :outage))
              ;; Act: the gate opens.
              (agent-repl-itest--push
               successor "host" `((host . ,(agent-repl-itest-composer--live 'open)))
               (plist-get (cdr adopted) :id))
              (agent-repl-itest--wait-until
               (lambda () (eq (agent-repl-host-composer-gate agent-repl-itest-composer--ws)
                              :open))
               nil "the successor's composer gate to open")
              (cl-letf (((symbol-function 'agent-repl-link-up-p) (lambda () t)))
                (agent-repl-prompt-queue-drain agent-repl-itest-composer--ws :outage))
              ;; Assert: NOW the drain sends.
              (agent-repl-itest--await-call successor "SubmitPrompt")
              (should (agent-repl-itest--calls successor "SubmitPrompt")))
          (when adopted (agent-repl-connect-close (car adopted)))
          (when successor (agent-repl-itest--stop-daemon successor t)))))))

;;;; ---- Audit-2 additions (R-SUITE-2) ----
;;
;; Findings 32-38 of docs/overhaul/reports/elisp-suite-audit-2.md.  Kept in
;; their own section so they merge cleanly beside concurrent edits above.

(declare-function agent-repl-link-connect "daemon-link")
(declare-function agent-repl-link-teardown "daemon-link")
(declare-function agent-repl-link--cancel-reconnect "daemon-link")
(declare-function agent-repl--prompt-queue-enqueue "prompt-queue")
(declare-function agent-repl--prompt-queue-on-finish "prompt-queue")
(declare-function agent-repl--input-said "input")
(declare-function agent-repl-host-conn "host")
(defvar agent-repl-input-notice)
(defvar agent-repl--input-merge-parked-badge)
(defvar agent-repl-link-reconnect-interval-seconds)

(defun agent-repl-itest-composer--notice (ws)
  "Return WS's composer notice as its mode line actually renders it.
The notice is a BUFFER-LOCAL fact drawn through
`agent-repl--input-mode-line-spec', so the rendered segment is what the
user sees and is what an assertion about a badge must read."
  (let ((buf (agent-repl--input-buffer ws)))
    (and buf (with-current-buffer buf
               (format-mode-line agent-repl--input-mode-line-spec)))))

;; audit-2 #32
(ert-deftest agent-repl-itest-composer-no-session-refusal-keeps-everything ()
  "A `no_session' refusal keeps the text, the attachments and the posthooks.
`endpoint_submit_prompt.proto' declares nine refusal arms; input.el's `_'
branch logs `elisp.input.unknown-error-arm' at ERROR and reports
\"submission refused\".  Undelivered user intent may never be discarded,
so the refusal must leave the composer exactly as the user left it."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest--script daemon "SubmitPrompt" '((error . ((noSession . ())))))
    (agent-repl-itest-composer--with-composer daemon 'open ref
      (ignore ref)
      (let ((ran nil)
            (buf (agent-repl-itest-composer--make-buffer
                  agent-repl-itest-composer--ws "run the tests")))
        (unwind-protect
            (progn
              (with-current-buffer buf
                (agent-repl-input-attach-image "/tmp/itest-shot.png" "image/png"))
              (setq agent-repl-send-posthooks
                    (list (cons "" (lambda (_ws _raw) (setq ran t)))))
              ;; Act.
              (agent-repl--send :user-sent nil agent-repl-itest-composer--ws)
              (agent-repl-itest--await-call daemon "SubmitPrompt")
              (agent-repl-itest-composer--await-log
               daemon "elisp.input.unknown-error-arm" "error")
              ;; Assert.
              (should (equal (with-current-buffer buf (buffer-string)) "run the tests"))
              (should (agent-repl-input-attachments agent-repl-itest-composer--ws))
              (should (null ran)))
          (agent-repl-itest-composer--kill-buffer agent-repl-itest-composer--ws buf))))))

;; audit-2 #32
(ert-deftest agent-repl-itest-composer-turn-already-open-refusal-names-its-arm ()
  "A `turn_already_open' refusal is REPORTED with its own arm keyword.
The generic branch is the whole treatment for eight of the nine arms, so
the arm has to reach the record: \"submission refused (:turn-already-open)\"
is the only thing that tells a reader which refusal happened."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest--script daemon "SubmitPrompt"
                              '((error . ((turnAlreadyOpen . ())))))
    (agent-repl-itest-composer--with-composer daemon 'open ref
      (ignore ref)
      ;; Act.
      (agent-repl--send :user-sent "run the tests" agent-repl-itest-composer--ws)
      (agent-repl-itest--await-call daemon "SubmitPrompt")
      (agent-repl-itest-composer--await-log
       daemon "elisp.input.unknown-error-arm" "error")
      ;; Assert.
      (should (seq-some
               (lambda (entry)
                 (seq-some (lambda (arg) (string-match-p ":turn-already-open" arg))
                           (agent-repl-itest--body-field entry 'context 'arguments)))
               (agent-repl-itest-composer--log-entries
                daemon "elisp.input.unknown-error-arm" "error"))))))

;; audit-2 #32
(ert-deftest agent-repl-itest-composer-submit-transferring-away-routes-to-the-handover ()
  "`transferring_away' on SubmitPrompt is a HANDOVER refusal, not an unknown arm.
fanout §7 names \"every per-workspace rpc\", and verbs.el already hands
both handover arms to `agent-repl-host-handle-refusal'.  SubmitPrompt is a
per-workspace rpc, so treating the arm as an unknown refusal would report
a rollout as a failure and leave the workspace on the daemon that released
it — the ruled behavior is the one adopt walk, exactly as for a verb."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest-composer--with-composer daemon 'open ref
      (ignore ref)
      (let ((refusals nil))
        (agent-repl-itest--script
         daemon "SubmitPrompt"
         '((error . ((transferringAway . ((address . "127.0.0.1:1")))))))
        (cl-letf (((symbol-function 'agent-repl-host-handle-refusal)
                   (lambda (_ws arm) (push arm refusals))))
          ;; Act.
          (agent-repl--send :user-sent "run the tests" agent-repl-itest-composer--ws)
          (agent-repl-itest--await-call daemon "SubmitPrompt")
          ;; Assert.
          (agent-repl-itest--wait-until (lambda () refusals) nil
                                        "the handover refusal to be routed")
          (should (eq (plist-get (car refusals) :arm) :transferring-away))
          (should (equal (plist-get (plist-get (car refusals) :value) :address)
                         "127.0.0.1:1")))))))

;; audit-2 #32
(ert-deftest agent-repl-itest-composer-submit-not-yet-adopted-routes-to-the-handover ()
  "`not_yet_adopted' on SubmitPrompt routes to the handover path too.
The second handover arm, ruled the same way: the successor has not
finished adopting the workspace, which is news about a rollout rather
than a refusal to report to the user."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest-composer--with-composer daemon 'open ref
      (ignore ref)
      (let ((refusals nil))
        (agent-repl-itest--script daemon "SubmitPrompt"
                                  '((error . ((notYetAdopted . ())))))
        (cl-letf (((symbol-function 'agent-repl-host-handle-refusal)
                   (lambda (_ws arm) (push arm refusals))))
          ;; Act.
          (agent-repl--send :user-sent "run the tests" agent-repl-itest-composer--ws)
          (agent-repl-itest--await-call daemon "SubmitPrompt")
          ;; Assert.
          (agent-repl-itest--wait-until (lambda () refusals) nil
                                        "the handover refusal to be routed")
          (should (eq (plist-get (car refusals) :arm) :not-yet-adopted)))))))

;; audit-2 #33
(ert-deftest agent-repl-itest-composer-outage-drain-runs-off-the-real-link-up-hook ()
  "The outage queue drains off the REAL `agent-repl-link-up-functions' edge.
fanout §10: \"the outage queue (drained on `agent-repl-link-up-functions')\".
Every other drain test stubs `agent-repl-link-up-p' and calls
`agent-repl--prompt-queue-on-link-up' by hand, so prompt-queue.el's own
`add-hook' — and the whole reconnect path host.el drives beside it — is
unpinned.  NOTHING here drains by hand: the link's own reconnect onto the
successor is the only trigger."
  ;; Arrange: production's link hooks are LIVE (not scratch-bound), so
  ;; host.el re-registers and prompt-queue.el drains for real.
  (agent-repl-itest--with-fake-daemon primary
    (let ((agent-repl-link-reconnect-interval-seconds 0.05)
          (successor nil))
      (agent-repl--ws-put agent-repl-itest-composer--ws
                          :project-dir agent-repl-itest-composer--dir)
      (unwind-protect
          (progn
            (agent-repl-link-connect)
            (agent-repl-itest--await-subscriber primary "daemon")
            (agent-repl-itest--wait-until
             (lambda () (agent-repl-host-ref agent-repl-itest-composer--ws))
             nil "host.el's own register+subscribe on the primary")
            ;; The daemon goes away mid-composition.
            (setq successor (agent-repl-itest-composer--restart-on-same-dir primary))
            ;; Act: the send fails at the transport and is held.
            (ignore-errors
              (agent-repl--send :user-sent "run the tests"
                                agent-repl-itest-composer--ws))
            (agent-repl-itest--wait-until
             (lambda () (agent-repl-prompt-queue-pending
                         agent-repl-itest-composer--ws :outage))
             nil "the prompt to reach the outage queue")
            ;; Assert: the link's own reconnect drains it onto the successor.
            (agent-repl-itest--await-subscriber successor "daemon")
            (agent-repl-itest--await-call successor "SubmitPrompt")
            (should (equal
                     (car (agent-repl-itest-composer--text-blocks
                           (car (agent-repl-itest--call-bodies successor "SubmitPrompt"))))
                     "run the tests"))
            (should (null (agent-repl-prompt-queue-pending
                           agent-repl-itest-composer--ws :outage))))
        (ignore-errors (agent-repl-host-forget agent-repl-itest-composer--ws))
        (ignore-errors (agent-repl-link-teardown))
        (ignore-errors (agent-repl-link--cancel-reconnect))
        (when successor (agent-repl-itest--stop-daemon successor t))))))

;; audit-2 #34
(ert-deftest agent-repl-itest-composer-merge-parked-draws-its-badge ()
  "The `merge_parked' gate draws its exact badge in the composer mode line.
fanout §7: \"`:merge-parked' send, with the input mode-line badge 'merge
parked — prompts go to the resolution agent'\".  The composer is OPEN WITH
CONTEXT: the badge is the only thing that tells the user their words go to
the resolution agent rather than the session."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest-composer--with-composer daemon 'mergeParked ref
      (ignore ref)
      (let ((buf (agent-repl-itest-composer--make-buffer
                  agent-repl-itest-composer--ws "run the tests")))
        (unwind-protect
            (progn
              ;; Act.
              (agent-repl--send :user-sent nil agent-repl-itest-composer--ws)
              (agent-repl-itest--await-call daemon "SubmitPrompt")
              ;; Assert: the rendered segment carries the badge verbatim.
              (should (string-match-p
                       (regexp-quote agent-repl--input-merge-parked-badge)
                       (agent-repl-itest-composer--notice
                        agent-repl-itest-composer--ws))))
          (agent-repl-itest-composer--kill-buffer
           agent-repl-itest-composer--ws buf))))))

;; audit-2 #34
(ert-deftest agent-repl-itest-composer-open-gate-draws-no-badge ()
  "The `open' gate draws NO badge at all.
The other half of the badge's contract: a badge drawn unconditionally
would tell every user of every open composer that their prompts go to a
resolution agent that does not exist."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest-composer--with-composer daemon 'open ref
      (ignore ref)
      (let ((buf (agent-repl-itest-composer--make-buffer
                  agent-repl-itest-composer--ws "run the tests")))
        (unwind-protect
            (progn
              ;; Act.
              (agent-repl--send :user-sent nil agent-repl-itest-composer--ws)
              (agent-repl-itest--await-call daemon "SubmitPrompt")
              ;; Assert.
              (should (equal (agent-repl-itest-composer--notice
                              agent-repl-itest-composer--ws)
                             "")))
          (agent-repl-itest-composer--kill-buffer
           agent-repl-itest-composer--ws buf))))))

;; audit-2 #35
(ert-deftest agent-repl-itest-composer-merging-refusal-flashes-the-mode-line ()
  "A `merging' refusal FLASHES \"refused: merge in flight\" in the mode line.
fanout §10: \"`message' + a mode-line flash 'refused: merge in flight'\".
The echo area line is transient and the composer keeps the text, so the
flash beside the kept text is what tells the user why nothing happened."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest--script daemon "SubmitPrompt" '((error . ((merging . ())))))
    (agent-repl-itest-composer--with-composer daemon 'open ref
      (ignore ref)
      (let ((buf (agent-repl-itest-composer--make-buffer
                  agent-repl-itest-composer--ws "run the tests")))
        (unwind-protect
            (progn
              ;; Act.
              (agent-repl--send :user-sent nil agent-repl-itest-composer--ws)
              (agent-repl-itest--await-call daemon "SubmitPrompt")
              ;; Assert.
              (agent-repl-itest--wait-until
               (lambda ()
                 (string-match-p
                  (regexp-quote "refused: merge in flight")
                  (or (agent-repl-itest-composer--notice
                       agent-repl-itest-composer--ws) "")))
               nil "the refusal flash in the composer mode line")
              (should (string-match-p
                       (regexp-quote "refused: merge in flight")
                       (agent-repl-itest-composer--notice
                        agent-repl-itest-composer--ws))))
          (agent-repl-itest-composer--kill-buffer
           agent-repl-itest-composer--ws buf))))))

;; audit-2 #36
(ert-deftest agent-repl-itest-composer-send-without-a-host-ref-refuses-before-send ()
  "A submit for a workspace with NO ref is refused before send.
fanout §10: `:workspace (agent-repl-host-ref WS)' is REQUIRED, and §0
\"an incomplete request errors before send\".  The ref is the
daemon-minted echo token — there is nothing Emacs could construct from a
path — so an unregistered workspace has no submission to make."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest-composer--with-composer daemon 'open ref
      (ignore ref)
      (agent-repl--ws-put "itest-composer-never-registered"
                          :project-dir "/tmp/itest-composer-never-registered")
      ;; Act / Assert.
      (should-error
       (agent-repl--send :user-sent "x" "itest-composer-never-registered"))
      (should (null (agent-repl-itest--calls daemon "SubmitPrompt"))))))

;; audit-2 #37
(ert-deftest agent-repl-itest-composer-unspecified-origin-refuses-before-send ()
  "An UNSPECIFIED origin is refused before send, with ZERO daemon calls.
fanout §5: \"UNSPECIFIED is refused before send\".  The origin rides the
wire and the daemon persists it onto the turn's durable record, so an
unspecified one would make a stored turn untraceable forever."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest-composer--with-composer daemon 'open ref
      (ignore ref)
      ;; Act / Assert.
      (should-error
       (agent-repl--send :unspecified "run the tests" agent-repl-itest-composer--ws))
      (should (null (agent-repl-itest--calls daemon "SubmitPrompt"))))))

;; audit-2 #37
(ert-deftest agent-repl-itest-composer-unknown-origin-refuses-before-send ()
  "An origin no Emacs send site owns is refused before send.
input.el: \"The enum carries further values for other producers (the
webapp, the daemon's own merge submits); Emacs never spells those ...
naming one is refused before a request is built.\""
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest-composer--with-composer daemon 'open ref
      (ignore ref)
      ;; Act / Assert.
      (should-error
       (agent-repl--send :no-such-site "run the tests" agent-repl-itest-composer--ws))
      (should (null (agent-repl-itest--calls daemon "SubmitPrompt"))))))

;; audit-2 #38
(ert-deftest agent-repl-itest-composer-deferral-drain-mints-a-fresh-key ()
  "A DEFERRED prompt drains under a FRESH idempotency key.
Ledger (R-COMPOSER): \"the drain resends under [the failed key]
\(deferrals mint fresh)\".  A deferral never attempted anything, so it is
a new turn rather than a retry — reusing an earlier key would let the
daemon's duplicate refusal swallow a prompt the user deliberately queued
for its own turn.  Only the outage half of that ruling was pinned."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest-composer--with-composer daemon 'open ref
      (ignore ref)
      ;; A first, ordinary submission, so there is a prior key to differ from.
      (let ((first-key (agent-repl--send :user-sent "the first prompt"
                                         agent-repl-itest-composer--ws)))
        (agent-repl-itest--await-call daemon "SubmitPrompt")
        (agent-repl--prompt-queue-enqueue
         agent-repl-itest-composer--ws :deferred
         (agent-repl--input-said "the deferred prompt" nil)
         :deferred-prompt "the deferred prompt")
        (cl-letf (((symbol-function 'agent-repl-link-up-p) (lambda () t)))
          ;; Act: the roster's finish edge releases it.
          (agent-repl--prompt-queue-on-finish agent-repl-itest-composer--ws))
        (agent-repl-itest--await-call daemon "SubmitPrompt" 2)
        ;; Assert.
        (let ((drained-key (agent-repl-itest--body-field
                            (agent-repl-itest-composer--submit-body daemon 1)
                            'idempotencyKey)))
          (should (stringp drained-key))
          (should-not (equal drained-key first-key))
          (should (string-match-p
                   "\\`[0-9a-f]\\{8\\}-[0-9a-f]\\{4\\}-4[0-9a-f]\\{3\\}-[89ab][0-9a-f]\\{3\\}-[0-9a-f]\\{12\\}\\'"
                   drained-key)))))))

(provide 'test-integration-composer)

;;; test-integration-composer.el ends here
