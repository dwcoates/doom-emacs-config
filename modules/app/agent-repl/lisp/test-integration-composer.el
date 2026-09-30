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
(declare-function agent-repl--input-waiting "input")
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

(defconst agent-repl-itest-composer--dir
  (agent-repl-itest--fixture-dir "composer-ws")
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
    (restarting . :restarting))
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
           (agent-repl--ws-put agent-repl-itest-composer--ws
                               :project-dir agent-repl-itest-composer--dir)
           (ignore-errors (agent-repl-host-forget agent-repl-itest-composer--ws))
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
      (setq-local agent-repl--owning-workspace ws)
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

;;;; ---- The durable held-prompt ingress ----

(declare-function agent-repl-held-ingress-entries "held-ingress" (ws))

(defun agent-repl-itest-composer--held-entries (ws)
  "Return WS's held-prompt ingress entries as parsed alists, oldest first.
The files are read from the scenario's own `AGENT_REPL_STATE_DIR', exactly
where the daemon's ingress sweeps them."
  (mapcar (lambda (path)
            (with-temp-buffer
              (let ((coding-system-for-read 'utf-8))
                (insert-file-contents path))
              (json-parse-buffer :object-type 'alist)))
          (agent-repl-held-ingress-entries ws)))

(defun agent-repl-itest-composer--held-text (entry)
  "Return the first text block of ENTRY's said."
  (alist-get 'text (alist-get 'text (aref (alist-get 'blocks (alist-get 'content (alist-get 'said entry))) 0))))

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
              ;; THE CLEARING IS A CONSEQUENCE OF THE ANSWER, NOT OF THE
              ;; REQUEST LANDING.  `--await-call' proves only that the
              ;; daemon recorded the submit; input.el empties the
              ;; attachment list in the success callback, which runs when
              ;; the answer arrives.  A second send issued on the strength
              ;; of the recorded call alone races that callback and
              ;; sometimes re-sends the very image this test exists to
              ;; prove is gone -- so the wait is for the observable
              ;; consequence, not for the request.
              (agent-repl-itest--wait-until
               (lambda () (null (buffer-local-value 'agent-repl-input-attachments buf)))
               nil "the first send's attachments to clear")
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

(ert-deftest agent-repl-itest-composer-command-acted-is-answered-not-awaited ()
  "`command_acted' is the third \"answered, nothing to await\" arm.
The daemon recognized a session-acting command and queued the act; the
visible effect arrives on the component streams, so Emacs's whole reaction
is to clear the input and log which arm answered."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest--script
     daemon "SubmitPrompt"
     '((success . ((commandActed . ())))))
    (agent-repl-itest-composer--with-composer daemon 'open ref
      (ignore ref)
      (let ((buf (agent-repl-itest-composer--make-buffer
                  agent-repl-itest-composer--ws "/model opus")))
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
                       daemon "elisp.input.command-answered" ":command-acted")))
          (agent-repl-itest-composer--kill-buffer agent-repl-itest-composer--ws buf))))))

(ert-deftest agent-repl-itest-composer-duplicate-submission-keeps-the-text ()
  "`duplicate_submission' says the key was already accepted: nothing landed
twice and nothing is owed a resend, so the text stays and the queue is
untouched."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest--script daemon "SubmitPrompt"
                              '((error . ((duplicateSubmission . ())))))
    (agent-repl-itest-composer--with-composer daemon 'open ref
      (ignore ref)
      (let ((messages nil)
            (buf (agent-repl-itest-composer--make-buffer
                  agent-repl-itest-composer--ws "run the tests")))
        (unwind-protect
            (progn
              (cl-letf (((symbol-function 'message)
                         (lambda (fmt &rest args)
                           (push (if args (apply #'format fmt args) fmt) messages)
                           nil)))
                ;; Act.
                (agent-repl--send :user-sent nil agent-repl-itest-composer--ws)
                (agent-repl-itest--await-call daemon "SubmitPrompt")
                (agent-repl-itest--wait-until
                 (lambda ()
                   (seq-some (lambda (text) (string-match-p "already accepted" text))
                             messages))
                 nil "the duplicate-key message")
                ;; Assert: the user is told plainly.
                (should (seq-some (lambda (text) (string-match-p "already accepted" text))
                                  messages)))
              ;; Assert: the composer keeps every word.
              (should (equal (with-current-buffer buf (buffer-string)) "run the tests"))
              ;; Assert: a refusal is an ANSWER, so nothing is held for resend.
              (should-not (agent-repl-itest-composer--held-entries
                           agent-repl-itest-composer--ws)))
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
    (let ((conn (agent-repl-connect-open (agent-repl-itest-daemon-address daemon)))
          (ref nil))
      (unwind-protect
          (progn
            (agent-repl--ws-put agent-repl-itest-composer--ws
                                :project-dir agent-repl-itest-composer--dir)
            (ignore-errors (agent-repl-host-forget agent-repl-itest-composer--ws))
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

;; owner ruling 2026-09-28: held prompts survive outages and restarts
(ert-deftest agent-repl-itest-composer-transport-failure-writes-the-held-prompt-ingress ()
  "A transport failure writes the prompt to the durable held-prompt ingress.
Undelivered user intent may never be silently discarded, nor kept only in
Emacs's memory: the entry waits on disk for the daemon to ingest it."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest-composer--with-composer daemon 'open ref
      (ignore ref)
      ;; The daemon goes away mid-composition.
      (agent-repl-itest--stop-daemon daemon t)
      ;; Act.
      (ignore-errors (agent-repl--send :user-sent "run the tests"
                                       agent-repl-itest-composer--ws))
      ;; Assert.
      (agent-repl-itest--wait-until
       (lambda () (agent-repl-itest-composer--held-entries agent-repl-itest-composer--ws))
       nil "the prompt to reach the held-prompt ingress")
      (should (equal (length (agent-repl-itest-composer--held-entries
                              agent-repl-itest-composer--ws))
                     1)))))
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

;;;; ---- Findings 66-68, re-ruled 2026-09-28: the durable held-prompt ingress ----

;; owner ruling 2026-09-28
(ert-deftest agent-repl-itest-composer-held-entry-carries-the-failed-attempts-idempotency-key ()
  "The held entry carries the failed attempt's idempotency key.
endpoint_submit_prompt.proto: \"a retried request is not a second turn\" --
the daemon ingests the entry under this key, so an attempt that DID land
before the transport failed is answered as a duplicate, never run twice."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest-composer--with-composer daemon 'open ref
      (ignore ref)
      (agent-repl-itest--stop-daemon daemon t)
      ;; Act: `agent-repl--send' returns its key synchronously even though
      ;; the transport fails asynchronously.
      (let ((failed-key (agent-repl--send :user-sent "run the tests"
                                          agent-repl-itest-composer--ws)))
        (agent-repl-itest--wait-until
         (lambda () (agent-repl-itest-composer--held-entries agent-repl-itest-composer--ws))
         nil "the prompt to reach the held-prompt ingress")
        ;; Assert.
        (should failed-key)
        (should (equal (alist-get 'idempotency_key
                                  (car (agent-repl-itest-composer--held-entries
                                        agent-repl-itest-composer--ws)))
                       failed-key))))))
;; owner ruling 2026-09-28
(ert-deftest agent-repl-itest-composer-held-entry-carries-the-prompts-origin-and-words ()
  "The held entry carries the send's own origin and its words, in protojson.
The daemon resubmits exactly this, so a stored turn still traces back to
the editor situation that caused it."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest-composer--with-composer daemon 'open ref
      (ignore ref)
      (agent-repl-itest--stop-daemon daemon t)
      ;; Act.
      (ignore-errors
        (agent-repl--send :user-sent "run the tests" agent-repl-itest-composer--ws))
      (agent-repl-itest--wait-until
       (lambda () (agent-repl-itest-composer--held-entries agent-repl-itest-composer--ws))
       nil "the prompt to reach the held-prompt ingress")
      ;; Assert.
      (let ((entry (car (agent-repl-itest-composer--held-entries
                         agent-repl-itest-composer--ws))))
        (should (equal (alist-get 'origin entry) "PROMPT_ORIGIN_USER_SENT"))
        (should (equal (agent-repl-itest-composer--held-text entry) "run the tests"))
        (should (equal (alist-get 'project_dir entry)
                       (directory-file-name
                        (expand-file-name agent-repl-itest-composer--dir))))))))
;;;; ---- Audit-2 additions (R-SUITE-2) ----
;;
;; Findings 32-38 of docs/overhaul/reports/elisp-suite-audit-2.md.  Kept in
;; their own section so they merge cleanly beside concurrent edits above.

(declare-function agent-repl-link-connect "daemon-link")
(declare-function agent-repl-link-teardown "daemon-link")
(declare-function agent-repl-link--cancel-reconnect "daemon-link")
(declare-function agent-repl--input-said "input")
(declare-function agent-repl-host-conn "host")
(defvar agent-repl-input-notice)
(defvar agent-repl-link-reconnect-interval-seconds)
(defvar agent-repl-link-reconnect-max-interval-seconds)

(defun agent-repl-itest-composer--notice (ws)
  "Return WS's composer notice as its mode line actually renders it.
The notice is a BUFFER-LOCAL fact drawn through
`agent-repl--input-mode-line-spec', so the rendered segment is what the
user sees and is what an assertion about a badge must read.

The segment is produced by calling `agent-repl--input-notice-segment' --
the `:eval' body of that very spec -- rather than by `format-mode-line'.
A batch Emacs has no displayed frame, so `format-mode-line' renders the
empty string for EVERY construct, literal strings included; reading it
here would report \"no badge\" no matter what the composer set, which is
the one answer this assertion must never be able to fabricate."
  (let ((buf (agent-repl--input-buffer ws)))
    (and buf (with-current-buffer buf (agent-repl--input-notice-segment)))))

;; audit-2 #32
(ert-deftest agent-repl-itest-composer-no-session-refusal-keeps-everything ()
  "A `no_session' refusal keeps the text, the attachments and the posthooks.
`no_session' is a workspace with no session at all, which the daemon
brings up itself; input.el records it at WARN as
`elisp.input.refused-no-session' and tells the user the daemon is
starting one.  Undelivered user intent may never be discarded, so the
refusal must leave the composer exactly as the user left it.

It used to fall through input.el's `_' branch and log
`elisp.input.unknown-error-arm' at ERROR (owner's report, 2026-09-14);
the arm is HANDLED now, and the record this waits on says so."
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
               daemon "elisp.input.refused-no-session" "warn")
              ;; Assert.
              (should (equal (with-current-buffer buf (buffer-string)) "run the tests"))
              (should (agent-repl-input-attachments agent-repl-itest-composer--ws))
              (should (null ran)))
          (agent-repl-itest-composer--kill-buffer agent-repl-itest-composer--ws buf))))))

;; audit-2 #32
(ert-deftest agent-repl-itest-composer-feed-undecodable-refusal-names-its-arm ()
  "A `feed_undecodable' refusal is REPORTED with its own arm keyword.
The generic branch is the whole treatment for most declared arms, so the
arm has to reach the record: \"submission refused (:feed-undecodable)\"
is the only thing that tells a reader which refusal happened."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest--script daemon "SubmitPrompt"
                              '((error . ((feedUndecodable . ())))))
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
                 (seq-some (lambda (arg) (string-match-p ":feed-undecodable" arg))
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

;; owner ruling 2026-09-28
(ert-deftest agent-repl-itest-composer-host-push-after-ingestion-clears-the-waiting-line ()
  "The waiting line clears on the daemon's host push once the entry is gone.
The daemon removes an ingested entry and THEN re-pushes the workspace's
host state; that real push, down the real WatchHostWorkspace stream, is
the edge the composer re-counts on."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest-composer--with-composer daemon 'open ref
      (let ((buf (agent-repl-itest-composer--make-buffer
                  agent-repl-itest-composer--ws "")))
        (unwind-protect
            (progn
              (agent-repl-itest--script daemon "SubmitPrompt"
                                        '((error . ((notYetAdopted . ())))))
              (cl-letf (((symbol-function 'agent-repl-host-handle-refusal) #'ignore))
                (agent-repl--send :user-sent "run the tests" agent-repl-itest-composer--ws))
              (agent-repl-itest--wait-until
               (lambda () (agent-repl--input-waiting agent-repl-itest-composer--ws))
               nil "the waiting line to be drawn")
              ;; Act: the daemon ingests the entry, then pushes.
              (mapc #'delete-file (agent-repl-held-ingress-entries agent-repl-itest-composer--ws))
              (agent-repl-itest--push
               daemon "host" `((host . ,(agent-repl-itest-composer--live 'open)))
               (plist-get ref :id))
              ;; Assert.
              (agent-repl-itest--wait-until
               (lambda () (null (agent-repl--input-waiting agent-repl-itest-composer--ws)))
               nil "the waiting line to clear on the host push"))
          (agent-repl-itest-composer--kill-buffer agent-repl-itest-composer--ws buf))))))
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
                          :project-dir (agent-repl-itest--fixture-dir "composer-never-registered"))
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
(ert-deftest agent-repl-itest-composer-deferral-mints-a-fresh-key ()
  "A DEFERRED prompt is submitted under a FRESH idempotency key.
A deferral is a first attempt, not a retry, so it is a new turn --
reusing an earlier key would let the daemon's duplicate refusal swallow a
prompt the user deliberately deferred to its own turn."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest-composer--with-composer daemon 'open ref
      (ignore ref)
      (let ((first-key (agent-repl--send :user-sent "the first prompt"
                                         agent-repl-itest-composer--ws))
            (buf (agent-repl-itest-composer--make-buffer
                  agent-repl-itest-composer--ws "the deferred prompt")))
        (unwind-protect
            (progn
              (agent-repl-itest--await-call daemon "SubmitPrompt")
              (cl-letf (((symbol-function 'agent-repl--ws-current-name)
                         (lambda () agent-repl-itest-composer--ws)))
                ;; Act.
                (agent-repl-queue-deferred-prompt))
              (agent-repl-itest--await-call daemon "SubmitPrompt" 2)
              ;; Assert.
              (let ((deferred-key (agent-repl-itest--body-field
                                   (agent-repl-itest-composer--submit-body daemon 1)
                                   'idempotencyKey)))
                (should (stringp deferred-key))
                (should-not (equal deferred-key first-key))
                (should (string-match-p
                         "\\`[0-9a-f]\\{8\\}-[0-9a-f]\\{4\\}-4[0-9a-f]\\{3\\}-[89ab][0-9a-f]\\{3\\}-[0-9a-f]\\{12\\}\\'"
                         deferred-key))))
          (agent-repl-itest-composer--kill-buffer agent-repl-itest-composer--ws buf))))))

;;;; ---- Audit-3 additions (R-SUITE-3) ----
;;
;; Findings 43-51 of docs/overhaul/reports/elisp-suite-audit-3.md.  Kept in
;; their own section so they merge cleanly beside concurrent edits above.

(declare-function agent-repl-link-successor "daemon-link" ())
(declare-function agent-repl-update-pr "commands" ())
(declare-function agent-repl-explain "commands" ())
(declare-function agent-repl-rebase-onto-origin-master "commands" ())
(declare-function agent-repl-create-or-update-pr "commands" (&optional excluded))
(declare-function agent-repl--build-create-or-update-pr-prompt "commands" (excluded &optional ws))
(declare-function agent-repl-send-and-hide "input" ())
(declare-function agent-repl-send-with-metaprompt "input" ())
(declare-function agent-repl--fire-metaprompt-read "input" (ws))
(declare-function agent-repl--command-prefix-for "input" (ws))
(declare-function agent-repl--on-close "panels" (&optional ws))
(defvar agent-repl-link-promote-functions)
(defvar agent-repl-update-pr-prompt)
(defvar agent-repl-rebase-onto-origin-master-prompt)
(defvar agent-repl-explain-prompt-template)
(defvar agent-repl--input-history)

;;;; ---- Findings 43-45: hold/replay under a REAL handover and link ----
;;
;; RULING (frozen, not re-litigated): #43-45 are believed already
;; implemented in production.  They are pinned against the REAL path --
;; `agent-repl-host-handle-refusal' is NEVER stubbed here, because stubbing
;; it is exactly the audit-2 #32 hole this suite exists to close.

(defun agent-repl-itest-composer--announce-shutdown (daemon address)
  "Push `shutdown_announced' on DAEMON's daemon stream, naming ADDRESS.
Mirrors `agent-repl-itest-link--announce' (test-integration-link.el), kept
local here so this file's findings do not depend on another suite's
helpers."
  (agent-repl-itest--push
   daemon "daemon"
   `((shutdownAnnounced
      . ((address . ,address)
         (cause . ((selfMergeRollout . ())))
         (expectedOutageMs . "1500")
         (mintedAtMs . ,(format "%d" (truncate (* 1000 (float-time))))))))))

;; audit-3 #43
(ert-deftest agent-repl-itest-composer-transferring-away-holds-durably-under-the-same-key ()
  "A `transferring_away' refusal HOLDS the prompt durably under the SAME key.
input.el `--input-on-handover-refusal' writes it to the held-prompt
ingress under THIS attempt's key while host.el walks the handover; the
daemon that owns the workspace afterwards ingests it (owner ruling
2026-09-28 retired the in-memory promotion replay).  The audit-2 #32 tests stubbed
`agent-repl-host-handle-refusal', which is exactly the hole a production
regression could hide in; nothing here is stubbed."
  ;; Arrange: a real link to the primary; host.el's own register+subscribe.
  (agent-repl-itest--with-fake-daemon primary
    (let ((agent-repl-link-reconnect-interval-seconds 0.05)
          (agent-repl-link-reconnect-max-interval-seconds 0.2)
          (failed-key nil)
          (buf nil))
      (agent-repl--ws-put agent-repl-itest-composer--ws
                          :project-dir agent-repl-itest-composer--dir)
      (unwind-protect
          (progn
            (agent-repl-link-connect)
            (agent-repl-itest--await-subscriber primary "daemon")
            (agent-repl-itest--wait-until
             (lambda () (agent-repl-host-ref agent-repl-itest-composer--ws))
             nil "host.el's own register+subscribe on the primary")
            ;; THE REF IS THE CLIENT'S FACT, THE SUBSCRIBER IS THE DAEMON'S:
            ;; the ref is minted when RegisterWorkspace answers, strictly
            ;; before the WatchHostWorkspace subscription it then opens is
            ;; registered.  A push in that window reaches nobody.
            (agent-repl-itest--await-subscriber
             primary "host"
             (plist-get (agent-repl-host-ref agent-repl-itest-composer--ws) :id))
            (agent-repl-itest--push
             primary "host"
             `((host . ,(agent-repl-itest-composer--live 'open)))
             (plist-get (agent-repl-host-ref agent-repl-itest-composer--ws) :id))
            (agent-repl-itest--wait-until
             (lambda () (eq (agent-repl-host-composer-gate agent-repl-itest-composer--ws) :open))
             nil "the primary's composer gate to open")
            ;; Act: announce the successor and wait for the link to accept it.
            (agent-repl-itest--with-second-daemon primary successor
              (let ((successor-address (agent-repl-itest-daemon-address successor)))
                (agent-repl-itest-composer--announce-shutdown primary successor-address)
                (agent-repl-itest--await-subscriber successor "daemon")
                (agent-repl-itest--wait-until
                 #'agent-repl-link-successor nil "the successor to be ACCEPTED")
                ;; Script the primary's SubmitPrompt to refuse with the handover.
                (agent-repl-itest--script
                 primary "SubmitPrompt"
                 `((error . ((transferringAway . ((address . ,successor-address)))))))
                (setq buf (agent-repl-itest-composer--make-buffer
                          agent-repl-itest-composer--ws "run the tests"))
                ;; Act: send -- refused as a handover, not a user-facing failure.
                (setq failed-key
                      (agent-repl--send :user-sent nil agent-repl-itest-composer--ws))
                (should failed-key)
                ;; Assert: ONE durable entry under the failed attempt's key,
                ;; the text still in the buffer.
                (agent-repl-itest--wait-until
                 (lambda () (agent-repl-itest-composer--held-entries
                             agent-repl-itest-composer--ws))
                 nil "the prompt to reach the held-prompt ingress")
                (let ((entries (agent-repl-itest-composer--held-entries
                                agent-repl-itest-composer--ws)))
                  (should (equal (length entries) 1))
                  (should (equal (alist-get 'idempotency_key (car entries)) failed-key)))
                (should (equal (with-current-buffer buf (buffer-string)) "run the tests"))
                ;; Assert: the real handover walk adopted WS onto the
                ;; successor -- `agent-repl-host-handle-refusal' ran for
                ;; real and was never stubbed.
                (agent-repl-itest--await-call successor "AdoptHostWorkspace")
                (agent-repl-itest--wait-until
                 (lambda () (eq (agent-repl-host-conn agent-repl-itest-composer--ws)
                                (agent-repl-link-successor)))
                 nil "the workspace to be adopted onto the successor")
                ;; Assert: Emacs re-sent nothing itself -- the daemon that
                ;; owns the workspace ingests the durable entry.
                (should (null (agent-repl-itest--calls successor "SubmitPrompt"))))))
        (when buf (agent-repl-itest-composer--kill-buffer agent-repl-itest-composer--ws buf))
        (ignore-errors (agent-repl-host-forget agent-repl-itest-composer--ws))
        (ignore-errors (agent-repl-link-teardown))
        (ignore-errors (agent-repl-link--cancel-reconnect))))))

;; owner ruling 2026-09-28
(ert-deftest agent-repl-itest-composer-two-failed-sends-are-held-oldest-first ()
  "Two failed sends are held in the order written, each under its own key.
The daemon ingests entries in name order, so the order on disk is the
order the user's words are delivered in."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest-composer--with-composer daemon 'open ref
      (ignore ref)
      (agent-repl-itest--stop-daemon daemon t)
      ;; Act.
      (let ((first (agent-repl--send :user-sent "first held prompt"
                                     agent-repl-itest-composer--ws))
            (second (agent-repl--send :user-sent "second held prompt"
                                      agent-repl-itest-composer--ws)))
        (agent-repl-itest--wait-until
         (lambda () (= 2 (length (agent-repl-itest-composer--held-entries
                                  agent-repl-itest-composer--ws))))
         nil "both prompts to reach the held-prompt ingress")
        ;; Assert.
        (let ((entries (agent-repl-itest-composer--held-entries
                        agent-repl-itest-composer--ws)))
          (should (equal (mapcar #'agent-repl-itest-composer--held-text entries)
                         '("first held prompt" "second held prompt")))
          (should (equal (mapcar (lambda (e) (alist-get 'idempotency_key e)) entries)
                         (list first second))))))))
;; audit-3 #46
(ert-deftest agent-repl-itest-composer-image-only-submission-carries-no-text-block ()
  "An image attached to an EMPTY buffer submits as an image-only UserSaid.
user.proto: UserSaid is \"NOT a bare TextBlock\"; input.el: \"Empty TEXT
contributes no block\" -- an image-only send must not be treated as
send-empty, and must carry no TextBlock at all."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest-composer--with-composer daemon 'open ref
      (ignore ref)
      (let ((image (expand-file-name "pasted.png" agent-repl-itest-composer--dir))
            (buf (agent-repl-itest-composer--make-buffer agent-repl-itest-composer--ws "")))
        (unwind-protect
            (progn
              ;; Act.
              (with-current-buffer buf (agent-repl-input-attach-image image "image/png"))
              (agent-repl--send :user-sent nil agent-repl-itest-composer--ws)
              (agent-repl-itest--await-call daemon "SubmitPrompt")
              ;; Assert: one image block, no text block, not send-empty.
              (let* ((body (agent-repl-itest-composer--submit-body daemon))
                     (blocks (agent-repl-itest--body-field body 'said 'content 'blocks)))
                (should (equal (mapcar #'caar blocks) '(image)))
                (should (equal (agent-repl-itest-composer--image-paths body) (list image)))))
          (agent-repl-itest-composer--kill-buffer agent-repl-itest-composer--ws buf))))))

;; audit-3 #47
(ert-deftest agent-repl-itest-composer-queue-deferred-prompt-clears-the-composer-and-submits-it-deferred ()
  "`agent-repl-queue-deferred-prompt', the REAL command, empties the composer
and clears its attachments, pushes history, and submits the prompt AT ONCE
with BOTH its text and its image, asking for the deferred delivery -- the
daemon, not Emacs, holds it until the running turn ends (owner ruling,
2026-09-28: held prompts survive outages and restarts)."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest-composer--with-composer daemon 'open ref
      (ignore ref)
      (let ((image (expand-file-name "pasted.png" agent-repl-itest-composer--dir))
            (buf (agent-repl-itest-composer--make-buffer
                  agent-repl-itest-composer--ws "queue this for later")))
        (unwind-protect
            (cl-letf (((symbol-function 'agent-repl--ws-current-name)
                       (lambda () agent-repl-itest-composer--ws)))
              (with-current-buffer buf (agent-repl-input-attach-image image "image/png"))
              ;; Act.
              (agent-repl-queue-deferred-prompt)
              (agent-repl-itest--await-call daemon "SubmitPrompt")
              ;; Assert: composer emptied, attachments cleared.
              (should (equal (with-current-buffer buf (buffer-string)) ""))
              (should (null (agent-repl-input-attachments agent-repl-itest-composer--ws)))
              ;; Assert: history carries the raw text.
              (should (equal (car (buffer-local-value 'agent-repl--input-history buf))
                             "queue this for later"))
              ;; Assert: ONE submission, deferred, with BOTH text and image.
              (let ((body (agent-repl-itest-composer--submit-body daemon)))
                (should (equal (length (agent-repl-itest--calls daemon "SubmitPrompt")) 1))
                (should (equal (agent-repl-itest--body-field body 'delivery)
                               "SUBMIT_PROMPT_DELIVERY_DEFERRED"))
                (should (equal (agent-repl-itest--body-field body 'origin)
                               "PROMPT_ORIGIN_DEFERRED_PROMPT"))
                (should (equal (car (agent-repl-itest-composer--text-blocks body))
                               "queue this for later"))
                (should (equal (agent-repl-itest-composer--image-paths body) (list image)))))
          (agent-repl-itest-composer--kill-buffer agent-repl-itest-composer--ws buf))))))

;; audit-3 #47
(ert-deftest agent-repl-itest-composer-an-ordinary-send-asks-for-no-delivery ()
  "An ordinary send spells no `delivery': its absence is the ordinary one,
so only a deferral is ever held for the running turn's end unjudged."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest-composer--with-composer daemon 'open ref
      (ignore ref)
      ;; Act.
      (agent-repl--send :user-sent "run the tests" agent-repl-itest-composer--ws)
      (agent-repl-itest--await-call daemon "SubmitPrompt")
      ;; Assert.
      (should-not (agent-repl-itest--body-field
                   (agent-repl-itest-composer--submit-body daemon) 'delivery)))))

;;;; ---- Finding 48: the REAL command sites, not `agent-repl--send' by keyword ----
;;
;; The twelve origin tests above call `agent-repl--send' with the keyword
;; directly.  Each deftest below drives the actual production command
;; function instead, so the assertion is that THAT site passes its own
;; value -- every git/gh boundary and interactive reader it touches is
;; stubbed with `cl-letf', and none of them runs real git.

;; audit-3 #48
(ert-deftest agent-repl-itest-composer-update-pr-command-site-sends-its-own-text-and-origin ()
  "`agent-repl-update-pr', the REAL command entry point, sends its own text."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest-composer--with-composer daemon 'open ref
      (ignore ref)
      (cl-letf (((symbol-function 'agent-repl--ws-current-name)
                 (lambda () agent-repl-itest-composer--ws))
                ((symbol-function 'agent-repl--ws-current-log-name)
                 (lambda () agent-repl-itest-composer--ws)))
        ;; Act.
        (agent-repl-update-pr)
        (agent-repl-itest--await-call daemon "SubmitPrompt")
        ;; Assert.
        (let ((body (agent-repl-itest-composer--submit-body daemon)))
          (should (equal (car (agent-repl-itest-composer--text-blocks body))
                         agent-repl-update-pr-prompt))
          (should (equal (agent-repl-itest--body-field body 'origin)
                         "PROMPT_ORIGIN_COMMAND_UPDATE_PR")))))))

;; audit-3 #48
(ert-deftest agent-repl-itest-composer-explain-command-site-sends-its-own-text-and-origin ()
  "`agent-repl-explain', the REAL command entry point, sends its own text.
Stubs the editor-context boundary (`agent-repl--context-reference') so the
assertion is about the SEND SITE, not about magit/region detection."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest-composer--with-composer daemon 'open ref
      (ignore ref)
      (cl-letf (((symbol-function 'agent-repl--ws-current-name)
                 (lambda () agent-repl-itest-composer--ws))
                ((symbol-function 'agent-repl--ws-current-log-name)
                 (lambda () agent-repl-itest-composer--ws))
                ((symbol-function 'agent-repl--context-reference)
                 (lambda () "some/file.el:42")))
        ;; Act.
        (agent-repl-explain)
        (agent-repl-itest--await-call daemon "SubmitPrompt")
        ;; Assert.
        (let ((body (agent-repl-itest-composer--submit-body daemon)))
          (should (equal (car (agent-repl-itest-composer--text-blocks body))
                         (format agent-repl-explain-prompt-template "some/file.el:42")))
          (should (equal (agent-repl-itest--body-field body 'origin)
                         "PROMPT_ORIGIN_COMMAND_EXPLAIN_CONTEXT")))))))

;; audit-3 #48
(ert-deftest agent-repl-itest-composer-rebase-command-site-sends-its-own-text-and-origin-after-fetch ()
  "`agent-repl-rebase-onto-origin-master' sends its own text ONLY after fetch.
Stubs the `agent-repl--async-git' boundary -- NO REAL GIT -- to answer as
though `git fetch origin' succeeded."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest-composer--with-composer daemon 'open ref
      (ignore ref)
      (cl-letf (((symbol-function 'agent-repl--ws-current-name)
                 (lambda () agent-repl-itest-composer--ws))
                ((symbol-function 'agent-repl--ws-current-log-name)
                 (lambda () agent-repl-itest-composer--ws))
                ((symbol-function 'agent-repl--async-git)
                 (lambda (_label _root _args callback) (funcall callback t "stub fetch ok"))))
        ;; Act.
        (agent-repl-rebase-onto-origin-master)
        (agent-repl-itest--await-call daemon "SubmitPrompt")
        ;; Assert.
        (let ((body (agent-repl-itest-composer--submit-body daemon)))
          (should (equal (car (agent-repl-itest-composer--text-blocks body))
                         agent-repl-rebase-onto-origin-master-prompt))
          (should (equal (agent-repl-itest--body-field body 'origin)
                         "PROMPT_ORIGIN_COMMAND_REBASE")))))))

;; audit-3 #48
(ert-deftest agent-repl-itest-composer-create-or-update-pr-command-site-sends-its-own-text-and-origin ()
  "`agent-repl-create-or-update-pr', the REAL command entry point, sends its own text.
With no composer draft to prefix, the sent text is exactly the built
/create-or-update-pr prompt string."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest-composer--with-composer daemon 'open ref
      (ignore ref)
      (cl-letf (((symbol-function 'agent-repl--ws-current-name)
                 (lambda () agent-repl-itest-composer--ws))
                ((symbol-function 'agent-repl--ws-current-log-name)
                 (lambda () agent-repl-itest-composer--ws)))
        ;; Act.
        (agent-repl-create-or-update-pr)
        (agent-repl-itest--await-call daemon "SubmitPrompt")
        ;; Assert.
        (let ((body (agent-repl-itest-composer--submit-body daemon)))
          (should (equal (car (agent-repl-itest-composer--text-blocks body))
                         (agent-repl--build-create-or-update-pr-prompt
                          nil agent-repl-itest-composer--ws)))
          (should (equal (agent-repl-itest--body-field body 'origin)
                         "PROMPT_ORIGIN_COMMAND_CREATE_OR_UPDATE_PR")))))))

;; audit-3 #48
(ert-deftest agent-repl-itest-composer-send-and-hide-command-site-sends-its-own-origin ()
  "`agent-repl-send-and-hide', the REAL command entry point, sends its own origin.
Stubs the panel-closing boundary (`agent-repl--on-close') -- window layout
is not what this assertion is about."
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
                       (lambda () agent-repl-itest-composer--ws))
                      ((symbol-function 'agent-repl--on-close) (lambda (&optional _ws) nil)))
              ;; Act.
              (agent-repl-send-and-hide)
              (agent-repl-itest--await-call daemon "SubmitPrompt")
              ;; Assert.
              (let ((body (agent-repl-itest-composer--submit-body daemon)))
                (should (equal (car (agent-repl-itest-composer--text-blocks body))
                               "run the tests"))
                (should (equal (agent-repl-itest--body-field body 'origin)
                               "PROMPT_ORIGIN_USER_SENT_AND_HIDE"))))
          (agent-repl-itest-composer--kill-buffer agent-repl-itest-composer--ws buf))))))

;; audit-3 #48
(ert-deftest agent-repl-itest-composer-send-with-metaprompt-command-site-sends-its-own-origin-and-composed-text ()
  "`agent-repl-send-with-metaprompt', the REAL command entry point, sends its own origin.
The composed text carries the metaprompt read-directive prepended, exactly
as `agent-repl--prepare-input' with FORCE-METAPROMPT builds it."
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
              (let ((expected (agent-repl--prepare-input
                               agent-repl-itest-composer--ws "run the tests" t)))
                ;; Act.
                (agent-repl-send-with-metaprompt)
                (agent-repl-itest--await-call daemon "SubmitPrompt")
                ;; Assert.
                (let ((body (agent-repl-itest-composer--submit-body daemon)))
                  (should (equal (car (agent-repl-itest-composer--text-blocks body)) expected))
                  (should (equal (agent-repl-itest--body-field body 'origin)
                                 "PROMPT_ORIGIN_USER_SENT_WITH_METAPROMPT")))))
          (agent-repl-itest-composer--kill-buffer agent-repl-itest-composer--ws buf))))))

;; audit-3 #48
(ert-deftest agent-repl-itest-composer-fire-metaprompt-read-command-site-sends-its-own-origin ()
  "`agent-repl--fire-metaprompt-read', the REAL command entry point, sends its own origin.
The programmatic re-read carries the meta-wrapped read-directive as its
whole text, with no user text riding along."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest-composer--with-composer daemon 'open ref
      (ignore ref)
      (let ((expected (agent-repl--meta-wrap
                       (agent-repl--command-prefix-for agent-repl-itest-composer--ws))))
        ;; Act.
        (agent-repl--fire-metaprompt-read agent-repl-itest-composer--ws)
        (agent-repl-itest--await-call daemon "SubmitPrompt")
        ;; Assert.
        (let ((body (agent-repl-itest-composer--submit-body daemon)))
          (should (equal (car (agent-repl-itest-composer--text-blocks body)) expected))
          (should (equal (agent-repl-itest--body-field body 'origin)
                         "PROMPT_ORIGIN_METAPROMPT_READ")))))))

;; audit-3 #49
(ert-deftest agent-repl-itest-composer-merging-refusal-does-not-push-history ()
  "A `merging' refusal must NOT push history: nothing was ever sent.
fanout §10: only success `:turn' clears the input and pushes history; a
refusal that pushed anyway would let the refused text pollute history
recall as though it had been delivered."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest--script daemon "SubmitPrompt" '((error . ((merging . ())))))
    (agent-repl-itest-composer--with-composer daemon 'open ref
      (ignore ref)
      (let ((buf (agent-repl-itest-composer--make-buffer
                  agent-repl-itest-composer--ws "run the tests")))
        (unwind-protect
            (progn
              (with-current-buffer buf
                (setq agent-repl--input-history (list "sentinel-untouched")))
              ;; Act.
              (agent-repl--send :user-sent nil agent-repl-itest-composer--ws)
              (agent-repl-itest-composer--await-log daemon "elisp.input.refused-merging" "warn")
              ;; Assert: history's head is UNCHANGED.
              (should (equal (buffer-local-value 'agent-repl--input-history buf)
                             (list "sentinel-untouched"))))
          (agent-repl-itest-composer--kill-buffer agent-repl-itest-composer--ws buf))))))

;; audit-3 #50
(ert-deftest agent-repl-itest-composer-unknown-error-arm-message-names-the-arm ()
  "The generic refusal `message' names the exact refusing ARM, not just \"refused\".
input.el: `(message \"agent-repl: submission refused (%S)\" arm)' -- a
message that dropped the arm would tell the user nothing about WHICH
refusal happened.

The arm scripted here must be one the composer has NO treatment for, so
it genuinely reaches that branch.  It was `no_session', which is handled
on its own terms now (owner's report, 2026-09-14); `unknown_workspace'
takes its place and the subject of the test is unchanged."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest--script daemon "SubmitPrompt"
                              '((error . ((unknownWorkspace . ())))))
    (agent-repl-itest-composer--with-composer daemon 'open ref
      (ignore ref)
      (let ((messages nil))
        (cl-letf (((symbol-function 'message)
                   (lambda (fmt &rest args)
                     (push (if args (apply #'format fmt args) fmt) messages)
                     nil)))
          ;; Act.
          (agent-repl--send :user-sent "run the tests" agent-repl-itest-composer--ws)
          (agent-repl-itest--await-call daemon "SubmitPrompt")
          (agent-repl-itest--wait-until
           (lambda () (member "agent-repl: submission refused (:unknown-workspace)" messages))
           nil "the arm-naming refusal message")
          ;; Assert: the EXACT message, naming the arm.
          (should (member "agent-repl: submission refused (:unknown-workspace)" messages)))))))

;; audit-3 #51 (RULED, fixed in parallel -- EXPECTED RED until that lands)
(ert-deftest agent-repl-itest-composer-explicit-text-send-does-not-erase-the-unrelated-draft ()
  "A command send with EXPLICIT text must NOT erase the composer's unrelated draft.
Teamlead ruling (frozen, not re-litigated): only a send SOURCED FROM THE
COMPOSER BUFFER clears it on acceptance; `agent-repl-update-pr' sends its
own fixed text, so the user's unrelated \"my draft\" must survive the ack.
`--input-accepted' currently erases unconditionally, so this is EXPECTED
RED until the parallel fix lands."
  ;; Arrange.
  (agent-repl-itest--with-fake-daemon daemon
    (agent-repl-itest-composer--with-composer daemon 'open ref
      (ignore ref)
      (let ((buf (agent-repl-itest-composer--make-buffer
                  agent-repl-itest-composer--ws "my draft")))
        (unwind-protect
            (cl-letf (((symbol-function 'agent-repl--ws-current-name)
                       (lambda () agent-repl-itest-composer--ws))
                      ((symbol-function 'agent-repl--ws-current-log-name)
                       (lambda () agent-repl-itest-composer--ws)))
              ;; Act: a command site with its OWN explicit text.
              (agent-repl-update-pr)
              (agent-repl-itest--await-call daemon "SubmitPrompt")
              (agent-repl-itest-composer--await-log daemon "elisp.input.accepted" nil)
              ;; Assert: the unrelated draft SURVIVES the accepted send.
              (should (equal (with-current-buffer buf (buffer-string)) "my draft")))
          (agent-repl-itest-composer--kill-buffer agent-repl-itest-composer--ws buf))))))

(provide 'test-integration-composer)

;;; test-integration-composer.el ends here
