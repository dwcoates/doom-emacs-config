;;; input.el --- the host-native composer -*- lexical-binding: t; -*-

;;; Commentary:

;; THE COMPOSER IS HOST-NATIVE.  The webapp runs composer-less; this buffer
;; is where a prompt is written, decorated, gated and submitted, and
;; `agent-repl--send' is the one pipeline every send site in the module
;; goes through.
;;
;; THE PIPELINE, in order:
;;
;;   raw text        the user's own words, kept verbatim for history and
;;                   for the posthook scan
;;   prepare-input   host-side COMPOSITION (`agent-repl--prepare-input'):
;;                   the metaprompt read-directive prepended inside the
;;                   sentinel markers, the /wor source tag, the prefix and
;;                   postfix send variants.  Composition before submission
;;                   is free; "verbatim" forbids REWRITING what was
;;                   submitted, not composing it.
;;   the gate        `agent-repl-host-composer-gate' resolves the LIVE
;;                   arm's composer oneof into one keyword and this file
;;                   draws the FIXED treatment per arm (see
;;                   `agent-repl--input-gate-refusals').
;;   UserSaid        one TextBlock for the composed text plus one
;;                   ImageBlock{path, media_type} per image attached
;;                   through `clipboard-image.el'.
;;   SubmitPrompt    `(:said SAID :idempotency-key UUID :origin ORIGIN
;;                   :workspace REF)'.  All four are required; the origin
;;                   names THIS send site and the workspace ref is the
;;                   daemon-minted echo token, never built from a path.
;;
;; THE ANSWER FORKS AND NOT EVERY FORK IS A FAILURE:
;;
;;   success `turn'            a turn was minted -- clear the input, push
;;                             history, run the posthooks
;;   success `command_panel' / `command_refused' / `command_acted'
;;                             answered, nothing to await -- the webapp
;;                             draws the panel or the refusal card, so
;;                             Emacs logs it and clears the input
;;   error `merging'           refused: the text is KEPT so the user can
;;                             resubmit once the merge resolves
;;   transport failure         the daemon never answered: the text is KEPT
;;                             and the prompt is offered to
;;                             `prompt-queue.el', which drains on link-up
;;
;; PROMPT ORIGINS ARE A CLOSED VOCABULARY and every value has exactly ONE
;; production send site, so a stored turn traces back to the exact editor
;; situation that caused it.  `agent-repl--input-origins' is the list this
;; file accepts; anything else is refused before a request is built.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(declare-function agent-repl--log "agent-repl-core" (ws fmt &rest args))
(declare-function agent-repl--log-verbose "agent-repl-core" (ws fmt &rest args))
(declare-function agent-repl--info "agent-repl-core" (ws fmt &rest args))
(declare-function agent-repl--warn "agent-repl-core" (ws fmt &rest args))
(declare-function agent-repl--error "agent-repl-core" (ws fmt &rest args))
(declare-function agent-repl--fatal "agent-repl-core" (ws fmt &rest args))
(declare-function agent-repl--meta-wrap "agent-repl-core" (text))
(declare-function agent-repl--rgb-hex "agent-repl-core" (r g b))
(declare-function agent-repl--set-buffer-background "agent-repl-core" (color))
(declare-function agent-repl--ws-current-name "agent-repl-workspace" ())
(declare-function agent-repl--ws-current-log-name "agent-repl-workspace" ())
(declare-function agent-repl--ws-get "agent-repl-workspace" (ws key))
(declare-function agent-repl--history-push "agent-repl-history" (&optional text))
(declare-function agent-repl--history-reset "agent-repl-history" ())
(declare-function agent-repl--history-save "agent-repl-history" (ws))
(declare-function agent-repl--history-on-change "agent-repl-history" (&rest args))
(declare-function agent-repl--history-prev "agent-repl-history" ())
(declare-function agent-repl--history-next "agent-repl-history" ())
(declare-function agent-repl-history-search "agent-repl-history" ())
(declare-function agent-repl--on-close "agent-repl-panels" ())
(declare-function agent-repl-host-ref "agent-repl-host" (ws))
(declare-function agent-repl-host-conn "agent-repl-host" (ws))
(declare-function agent-repl-host-composer-gate "agent-repl-host" (ws))
(declare-function agent-repl-link-primary "agent-repl-daemon-link" ())
(declare-function agent-repl-rpc-submit-prompt "agent-repl-rpc" (conn request &rest keys))
(declare-function agent-repl-host-handle-refusal "agent-repl-host" (ws arm-plist))
(declare-function agent-repl-prompt-queue-offer "agent-repl-prompt-queue"
                  (ws said origin raw &optional key))
(declare-function agent-repl--kickoff-prompt-summary "agent-repl-prompt-summary" (ws raw))
(declare-function evil-insert-state "evil" (&optional arg))

;;;; ---- Metaprompt on-demand re-read -----------------------------------

;; THE METAPROMPT IS THE SESSION'S SYSTEM PROMPT, and nothing here injects
;; it.  The shim reads `metaprompt.md' out of the canonical doom checkout
;; and hands it to the SDK as a preset append, so the guidelines are
;; re-sent with every request and survive `/clear', `/compact' and resume
;; without anyone re-establishing them.  What remains here is the MANUAL
;; re-read: `agent-repl-send-with-metaprompt' and
;; `agent-repl--fire-metaprompt-read'.  Neither fires on its own.

(defvar agent-repl-metaprompt-file
  (expand-file-name "../metaprompt.md"
                    (file-name-directory (or load-file-name buffer-file-name)))
  "Absolute path to the canonical metaprompt source file in this repository.
The .md data file lives as plain text at the module root, one level above
this file's own `lisp/' directory, edited and version-controlled
alongside the code, and is read at session spawn by the shim.  It is
ALSO the path the on-demand read-directive names.  Captured at file-load
time because `load-file-name' is only bound during load.")

(defcustom agent-repl-command-prefix
  (with-temp-buffer
    (insert-file-contents agent-repl-metaprompt-file)
    (buffer-string))
  "Canonical metaprompt content, loaded from `agent-repl-metaprompt-file'.
Nil disables the on-demand re-read entirely."
  :type '(choice (const :tag "Disabled" nil) string)
  :group 'agent-repl)

(defcustom agent-repl-command-prefix-template
  "Please re-read the agent-repl metaprompt at %s and follow it for this request."
  "Format string for the on-demand metaprompt read-directive.
%s is replaced with the metaprompt path the workspace should read."
  :type 'string
  :group 'agent-repl)

(defvar agent-repl--command-prefix
  (format agent-repl-command-prefix-template agent-repl-metaprompt-file)
  "The read-directive for the canonical metaprompt path.
Pre-formatted once so the common case costs no formatting.")

(defcustom agent-repl-send-postfix "\n what do you think? do NOT code, just analyze."
  "String appended to input when sending via `agent-repl-send-with-postfix'."
  :type 'string
  :group 'agent-repl)

(defcustom agent-repl-send-prefix "just answer, dont take action: "
  "String prepended to input when sending via `agent-repl-send-with-prefix'."
  :type 'string
  :group 'agent-repl)

;;;; ---- The input buffer ------------------------------------------------

(defface agent-repl-header-line
  '((t :background "white" :foreground "black" :weight bold))
  "Face for the Agent Input header line.")

(defcustom agent-repl-input-background-shade 20
  "Base greyscale level (0-255) for the input buffer background.
Sets the red and green channels; the blue channel adds
`agent-repl-input-background-blue-boost' on top for a faint blue tint."
  :type 'integer
  :group 'agent-repl)

(defcustom agent-repl-input-background-blue-boost 6
  "Extra amount added to the blue channel of the input buffer background."
  :type 'integer
  :group 'agent-repl)

(defun agent-repl--input-background-color ()
  "Return the input buffer background as a #rrggbb hex string."
  (agent-repl--rgb-hex agent-repl-input-background-shade
                       agent-repl-input-background-shade
                       (+ agent-repl-input-background-shade
                          agent-repl-input-background-blue-boost)))

(defvar-local agent-repl-input-notice nil
  "The composer's standing notice, drawn in the input buffer's mode line.
Set to the merge-parked badge while the gate says the merge's resolution
agent owns this composer, and to the refusal flash when a submission comes
back refused.  Buffer-local to the input buffer, because the notice is
about THIS composer and no other.")

(defvar-local agent-repl-input-attachments nil
  "Images attached to this composer, oldest first.
Each element is the plist `(:path ABSOLUTE-PATH :media-type MIME)'.
`clipboard-image.el' appends to this list when the user attaches a
pasteboard image; `agent-repl--send' turns each entry into one
`ImageBlock' beside the text block and clears the list on a submission
the daemon accepted.")

(defconst agent-repl--input-mode-line-spec
  '(:eval (agent-repl--input-notice-segment))
  "The composer's mode-line segment: the standing notice, or nothing.")

(defun agent-repl--input-notice-segment ()
  "Return the input buffer's mode-line notice text, or the empty string."
  (if agent-repl-input-notice
      (concat " " agent-repl-input-notice)
    ""))

(define-derived-mode agent-repl-input-mode fundamental-mode "Agent Input"
  "Major mode for the host-native composer buffer."
  (setq-local header-line-format
              "C-c C-c: clear+save | (cmd) <up>/<down>: history | C-M-r: search history")
  (face-remap-add-relative 'header-line 'agent-repl-header-line)
  (agent-repl--set-buffer-background (agent-repl--input-background-color))
  (unless (member agent-repl--input-mode-line-spec mode-line-format)
    (setq-local mode-line-format
                (append mode-line-format (list agent-repl--input-mode-line-spec))))
  ;; Widen the fill column so wrapped prose and `fill-paragraph' reflow to
  ;; 150 columns rather than the 70-column default.
  (setq-local fill-column 150)
  ;; Soft-wrap long input lines at word boundaries.  The two underlying
  ;; variables are set directly rather than enabling `visual-line-mode':
  ;; that minor mode also remaps Evil's line motions to screen lines,
  ;; which is unwanted.
  (setq-local truncate-lines nil)
  (setq-local word-wrap t)
  ;; Intentionally unlogged: `after-change-functions' runs per keystroke, so
  ;; logging it would overwhelm the durable input/send lifecycle diagnostics.
  (add-hook 'after-change-functions #'agent-repl--history-on-change nil t))

(defun agent-repl--input-buffer (ws)
  "Return WS's live input buffer, or nil."
  (let ((buf (agent-repl--ws-get ws :input-buffer)))
    (and buf (buffer-live-p buf) buf)))

(defun agent-repl-discard-input ()
  "Save current input to history, clear the buffer, and enter insert state."
  (interactive)
  (let ((ws (agent-repl--ws-current-name))
        (input-len (buffer-size)))
    (agent-repl--log ws "elisp.input.discard ws=%s input-len=%d" ws input-len)
    (agent-repl--history-push)
    (agent-repl--history-reset)
    (agent-repl--history-save ws)
    (erase-buffer)
    (setq agent-repl-input-attachments nil)
    (when (fboundp 'evil-insert-state) (evil-insert-state))))

;;;; ---- The composer notice ---------------------------------------------

(defcustom agent-repl-input-flash-seconds 4
  "Seconds a composer refusal flash stays in the input buffer's mode line."
  :type 'number
  :group 'agent-repl)

(defun agent-repl--input-set-notice (ws text)
  "Set WS's composer mode-line notice to TEXT (nil clears it)."
  (let ((buf (agent-repl--input-buffer ws)))
    (agent-repl--log ws "elisp.input.notice ws=%s text=%S buffer=%s"
                     ws text (and buf t))
    (when buf
      (with-current-buffer buf
        (setq agent-repl-input-notice text)
        (force-mode-line-update)))))

(defun agent-repl--input-expire-flash (ws buffer text)
  "Clear BUFFER's composer notice, but only while it still shows this TEXT.

THE DWELL OUTLIVES THE FLASH THAT ARMED IT.  The timer fires seconds
later, and by then WS may have a DIFFERENT composer buffer and that
buffer a DIFFERENT notice -- the merge-parked badge, most of all, which
`agent-repl--input-check-gate\=' sets on every submit while the merge\='s
resolution agent owns the composer.  A dwell that resolved the buffer by
name and cleared whatever it found would erase a badge it never set: a
refusal flashed within `agent-repl-input-flash-seconds\=' of a parked
send would silently take the one line telling the user their words are
going somewhere else.

So the expiry is bound to what it flashed on both axes -- the BUFFER it
wrote to and the TEXT it wrote -- and clears nothing else.  WS is carried
for the log line only."
  (cond
   ((not (buffer-live-p buffer))
    (agent-repl--log ws "elisp.input.flash-expired-dead-buffer ws=%s text=%S" ws text))
   ((not (equal (buffer-local-value 'agent-repl-input-notice buffer) text))
    (agent-repl--log ws "elisp.input.flash-superseded ws=%s text=%S standing=%S"
                     ws text (buffer-local-value 'agent-repl-input-notice buffer)))
   (t
    (with-current-buffer buffer
      (setq agent-repl-input-notice nil)
      (force-mode-line-update))
    (agent-repl--log ws "elisp.input.flash-cleared ws=%s text=%S" ws text))))

(defun agent-repl--input-flash (ws text)
  "Show TEXT as WS\='s composer notice, clearing it after a short dwell.
The dwell is presentation only: nothing downstream reads the notice, so a
timer that never fires costs a stale badge and nothing else.  What it must
NOT cost is another notice: see `agent-repl--input-expire-flash\=', which
is why the buffer and the text are captured here rather than resolved when
the timer fires."
  (agent-repl--input-set-notice ws text)
  (let ((buffer (agent-repl--input-buffer ws)))
    (when buffer
      (run-at-time agent-repl-input-flash-seconds nil
                   #'agent-repl--input-expire-flash ws buffer text))))

;;;; ---- Attachments -----------------------------------------------------

(defun agent-repl-input-attach-image (path media-type)
  "Register PATH (MIME MEDIA-TYPE) as an image attached to THIS composer.
The ONE entry point that attaches an image: `clipboard-image.el' calls it
after capturing the pasteboard, and it operates on the CURRENT buffer,
which must be a composer.  Returns the resulting attachment list.

The file is NOT read here: an `ImageBlock' carries a REFERENCE, and the
daemon and the shim run on this same host, so the path is the whole
payload."
  (unless (derived-mode-p 'agent-repl-input-mode)
    (agent-repl--fatal nil "elisp.input.attach-outside-composer buffer=%s path=%s"
                       (buffer-name) path))
  (setq agent-repl-input-attachments
        (append agent-repl-input-attachments
                (list (list :path path :media-type media-type))))
  (agent-repl--info nil "elisp.input.attached path=%s media-type=%s count=%d"
                    path media-type (length agent-repl-input-attachments))
  agent-repl-input-attachments)

(defun agent-repl-input-attachments (&optional ws)
  "Return the images attached to a composer, oldest first.
With WS, reads WS's composer; without it, THIS buffer's own list -- the
send pipeline knows which workspace it is submitting for, while a caller
already inside the composer does not have to say so."
  (if (null ws)
      agent-repl-input-attachments
    (let ((buf (agent-repl--input-buffer ws)))
      (if buf
          (buffer-local-value 'agent-repl-input-attachments buf)
        (agent-repl--log ws "elisp.input.attachments-no-buffer ws=%s" ws)
        nil))))

(defun agent-repl-input-clear-attachments (ws)
  "Drop every image attached to WS's composer.
Run only after the daemon ACCEPTED the submission: an attachment cleared
on a refusal would be silently lost user intent."
  (let ((buf (agent-repl--input-buffer ws)))
    (when buf
      (with-current-buffer buf
        (agent-repl--log ws "elisp.input.attachments-cleared ws=%s count=%d"
                         ws (length agent-repl-input-attachments))
        (setq agent-repl-input-attachments nil)))))

;;;; ---- Input preparation and the metaprompt ----------------------------

(defcustom agent-repl-metaprompt-exempt-strings
  '("/clear" "/usage" "/login" "/logout")
  "Inputs that should never have the metaprompt prepended.
Compared exactly against the trimmed input.  Largely redundant with the
general slash-command rule, and retained for any future NON-slash
exemption and as an explicit record of intent."
  :type '(repeat string)
  :group 'agent-repl)

(defconst agent-repl--slash-command-name-chars "A-Za-z0-9_:-"
  "Character class of a slash-command NAME run, after the leading slash.")

(defconst agent-repl--slash-command-regexp
  (concat "\\`/[" agent-repl--slash-command-name-chars "]+\\(?:[[:space:]]\\|\\'\\)")
  "Matches an input that IS a slash-command invocation.
Anchored at the very start, so a Unix path in the middle of a sentence is
not one; the name run must be terminated by whitespace or end of input, so
`/Users/foo' (a path) does not match either.")

(defun agent-repl--slash-command-p (raw)
  "Return non-nil if RAW is a slash-command invocation.
See `agent-repl--slash-command-regexp' for exactly what counts."
  (string-match-p agent-repl--slash-command-regexp raw))

(defun agent-repl--skip-metaprompt-p (raw &optional ws)
  "Return non-nil if RAW input should never have the metaprompt prepended.
Skips any slash command, every entry of
`agent-repl-metaprompt-exempt-strings', and bare numerals, ignoring
trailing whitespace.  The slash-command clause is the load-bearing one:
the metaprompt is a harness directive meant for free-form work, and a
slash command runs a skill or built-in that owns its own behavior."
  (let* ((trimmed (string-trim-right raw))
         (slash-command-p (not (null (agent-repl--slash-command-p trimmed))))
         (exempt-p (not (null (member trimmed agent-repl-metaprompt-exempt-strings))))
         (numeral-p (not (null (string-match-p "^[0-9]+$" trimmed))))
         (result (or slash-command-p exempt-p numeral-p)))
    (agent-repl--log-verbose ws
                             "elisp.input.skip-metaprompt raw-len=%d slash-command=%s exempt=%s numeral=%s result=%s"
                             (length raw) slash-command-p exempt-p numeral-p result)
    result))

(defvar agent-repl-send-posthooks nil
  "Alist of (PATTERN . FUNCTION) posthooks run after input is ACCEPTED.
PATTERN is a string or regexp matched against the raw input (trimmed).
FUNCTION is called with (WS RAW).  They run on the minted-turn arm only:
a refused submission caused nothing to post-process.

EMPTY BY DEFAULT, deliberately.  The one posthook that used to live here
marked the tab finished after a `/clear\', and the tab\'s state is now the
ROSTER ROW\'S STATUS ARM -- the daemon pushes it, Emacs paints it, and a
client-side guess about when a turn ended is exactly the derivation the
overhaul removed.  The hook stays as the extension point it always was.")

(defun agent-repl--run-send-posthooks (ws raw)
  "Run posthooks matching RAW input for workspace WS."
  (let ((trimmed (string-trim-right raw))
        (matched-count 0))
    (dolist (hook agent-repl-send-posthooks)
      (when (string-match-p (car hook) trimmed)
        (cl-incf matched-count)
        (agent-repl--log ws "elisp.input.posthook-matched pattern=%s" (car hook))
        (funcall (cdr hook) ws raw)))
    (agent-repl--log ws "elisp.input.posthook-scan raw-len=%d hook-count=%d matched-count=%d"
                     (length raw) (length agent-repl-send-posthooks) matched-count)))

(defun agent-repl--should-prepend-metaprompt-p (raw force &optional ws)
  "Return non-nil if the read-directive should be prepended to RAW.
FORCE is the caller's explicit request for it -- nothing else prepends,
because the guidelines reach the agent as the session's system prompt
rather than as anything injected into a prompt."
  (let* ((enabled-p (and agent-repl-command-prefix t))
         (force-p (and force t))
         ;; Preserve short-circuiting: a disabled system or an unforced
         ;; send must not evaluate the exemption check.
         (skip-p (and enabled-p force-p (agent-repl--skip-metaprompt-p raw ws) t))
         (result (and enabled-p force-p (not skip-p) t)))
    (agent-repl--log-verbose ws
                             "elisp.input.should-prepend enabled=%s prompt-len=%d force=%s skip=%s result=%s"
                             enabled-p (length raw) force-p skip-p result)
    result))

(defcustom agent-repl-workspace-command-prefix "/wor"
  "String prefix that identifies workspace-related commands.
Used to detect workspace-generation and workspace-update skills so the
source workspace identity can be tagged onto the request."
  :type 'string
  :group 'agent-repl)

(defun agent-repl--workspace-command-p (raw)
  "Return non-nil if RAW is a /wor workspace-generation/update command."
  (string-prefix-p agent-repl-workspace-command-prefix (string-trim-left raw)))

(defun agent-repl--maybe-inject-source-ws (ws raw)
  "Return RAW with a source-workspace tag appended, if RAW is a /wor command.
Appends \" [source-ws:<ws-name> path:<project-dir>]\" so the
workspace-generation and workspace-update skills know both which
workspace initiated the request and the repo root.  Only the returned
string carries the tag; the caller keeps its own RAW, used for posthook
matching and history, untagged."
  (if (agent-repl--workspace-command-p raw)
      (let ((dir (agent-repl--ws-get ws :project-dir)))
        (unless dir
          (agent-repl--fatal ws "elisp.input.inject-source-ws-no-dir ws=%s" ws))
        (agent-repl--log ws "elisp.input.inject-source-ws ws=%s path=%s" ws dir)
        (concat raw (format " [source-ws:%s path:%s]" ws dir)))
    (agent-repl--log-verbose ws "elisp.input.inject-source-ws-skipped raw-len=%d" (length raw))
    raw))

(defun agent-repl--metaprompt-file-for (ws)
  "Metaprompt path the read-directive should point WS at.
`agent-repl-metaprompt-file' is fixed at load time to the checkout Emacs
loaded this file from -- the main worktree -- so a WS running in a
DIFFERENT worktree of the same repo would be told to read (and thereby be
primed to edit against) that OTHER tree rather than its own.  When WS's
own `:project-dir' carries the metaprompt at the in-repo path, return
that copy; otherwise fall back to the canonical file."
  (let* ((root (agent-repl--ws-get ws :project-dir))
         (in-ws (and root
                     (expand-file-name "modules/app/agent-repl/metaprompt.md" root)))
         (worktree-copy-p (and in-ws (file-exists-p in-ws))))
    (agent-repl--log ws "elisp.input.metaprompt-file root-present=%s worktree-copy=%s"
                     (not (null root)) (not (null worktree-copy-p)))
    (if worktree-copy-p in-ws agent-repl-metaprompt-file)))

(defun agent-repl--command-prefix-for (ws)
  "Read-directive string for WS, pointing at WS's own metaprompt copy."
  (let ((file (agent-repl--metaprompt-file-for ws)))
    (agent-repl--log ws "elisp.input.command-prefix selected=%s"
                     (if (equal file agent-repl-metaprompt-file) "canonical" "workspace"))
    (if (equal file agent-repl-metaprompt-file)
        agent-repl--command-prefix
      (format agent-repl-command-prefix-template file))))

(defun agent-repl--prepare-input (ws raw &optional force-metaprompt)
  "Compose the text WS actually submits from RAW.
PROMPT COMPOSITION IS FREE: everything this function does happens BEFORE
submission, and what is submitted is what is drawn.  The read-directive
is prepended ONLY when FORCE-METAPROMPT is non-nil, bracketed as a
harness-injected span (`agent-repl--meta-wrap') so the daemon strips it
from the DRAWN row while the agent still receives it verbatim.  A /wor
command additionally gets a source-workspace tag appended."
  (let* ((tagged (agent-repl--maybe-inject-source-ws ws raw))
         (prepend-p (agent-repl--should-prepend-metaprompt-p raw force-metaprompt ws)))
    (agent-repl--log ws "elisp.input.prepare raw-len=%d tagged-len=%d force=%s prepend=%s"
                     (length raw) (length tagged) force-metaprompt prepend-p)
    (if prepend-p
        (concat (agent-repl--meta-wrap (agent-repl--command-prefix-for ws)) "\n\n" tagged)
      tagged)))

;;;; ---- Identity --------------------------------------------------------

(defun agent-repl--uuid ()
  "Return a fresh RFC 4122 version 4 UUID string.
The submission's idempotency key: the daemon refuses duplicates by it, so
a retried request is not a second turn.  Built from `random', whose seed
Emacs takes from the system entropy source at startup -- good enough for
a key whose only job is to be different from the last one."
  (let ((bytes (make-vector 16 0)))
    (dotimes (i 16) (aset bytes i (random 256)))
    ;; Version 4 in the high nibble of byte 6, variant 10x in byte 8.
    (aset bytes 6 (logior #x40 (logand (aref bytes 6) #x0f)))
    (aset bytes 8 (logior #x80 (logand (aref bytes 8) #x3f)))
    (apply #'format
           "%02x%02x%02x%02x-%02x%02x-%02x%02x-%02x%02x-%02x%02x%02x%02x%02x%02x"
           (append bytes nil))))

;;;; ---- The gate --------------------------------------------------------

(defconst agent-repl--input-gate-refusals
  '((:merging . "composer closed: a merge owns this session")
    (:draining . "composer closed: daemon draining")
    (:restarting . "composer closed: restarting"))
  "The composer gate arms that REFUSE, and the message each one draws.
Fixed treatments per arm, never a composed sentence: the resolved arm IS
the gate.  Every other arm sends -- `:merge-parked' with the badge below,
and `:open', `:no-session', `:terminal' and `:unknown' plainly, because
SubmitPrompt has no precondition and the daemon starts or revives the
session implicitly.")

(defconst agent-repl--input-merge-parked-badge
  "merge parked -- prompts go to the resolution agent"
  "The composer badge drawn while the gate reads `:merge-parked'.
The composer is OPEN WITH CONTEXT: everything submitted while parked is
delivered to the merge's resolution agent, never refused and never queued
as the session's own turn.")

(defun agent-repl--input-check-gate (ws)
  "Apply the composer gate's fixed treatment for WS; return the gate keyword.
Signals `user-error' with the arm's own message on a refusing arm, so a
refusal aborts the pipeline before the input buffer is touched and the
user keeps every word they wrote."
  (let* ((gate (agent-repl-host-composer-gate ws))
         (refusal (cdr (assq gate agent-repl--input-gate-refusals))))
    (cond
     (refusal
      (agent-repl--warn ws "elisp.input.gate-refused ws=%s gate=%S" ws gate)
      (user-error "%s" refusal))
     ((eq gate :merge-parked)
      (agent-repl--info ws "elisp.input.gate-merge-parked ws=%s" ws)
      (agent-repl--input-set-notice ws agent-repl--input-merge-parked-badge)
      gate)
     ((eq gate :unknown)
      ;; No host push has arrived yet.  The daemon is the authority and
      ;; answers with its own refusal arms, so this SENDS rather than
      ;; guessing a state Emacs does not hold.
      (agent-repl--info ws "elisp.input.gate-unknown-sends ws=%s" ws)
      gate)
     (t
      (agent-repl--log ws "elisp.input.gate-open ws=%s gate=%S" ws gate)
      (agent-repl--input-set-notice ws nil)
      gate))))

;;;; ---- The submission --------------------------------------------------

(defconst agent-repl--input-origins
  '(:user-sent
    :user-sent-and-hide
    :user-sent-with-metaprompt
    :user-sent-with-postfix
    :user-sent-with-prefix
    :metaprompt-read
    :command-diff-analysis
    :command-explain-context
    :command-explain-prompt
    :command-update-pr
    :command-rebase
    :command-create-or-update-pr
    :deferred-prompt)
  "Every `PromptOrigin' an Emacs send site may choose.
Each value has EXACTLY ONE production send site, which is what makes a
stored turn traceable back to the editor situation that caused it.  The
enum carries further values for other producers (the webapp, the daemon's
own merge submits); Emacs never spells those, so they are absent here and
naming one is refused before a request is built.")

(defun agent-repl--input-text-block (text)
  "Return the `UserContentBlock' text arm for TEXT."
  (list :arm :text :value (list :text text)))

(defun agent-repl--input-image-block (attachment)
  "Return the `UserContentBlock' image arm for ATTACHMENT.
The image travels by REFERENCE: an `ImageBlockPath' naming a file on this
host, because the daemon and the shim read it from the same filesystem
Emacs wrote it to."
  (list :arm :image
        :value (list :location (list :arm :path
                                     :value (list :path (plist-get attachment :path)))
                     :media-type (plist-get attachment :media-type))))

(defun agent-repl--input-said (text attachments)
  "Build the `UserSaid' for TEXT plus ATTACHMENTS.
Blocks travel in the order the person composed them: the words first,
then each image in attachment order.  Empty TEXT contributes no block at
all -- an image-only submission is a legitimate thing to say, and an empty
`TextBlock' would be a sentinel for one."
  (list :content
        (list :blocks
              (append
               (unless (string-empty-p text)
                 (list (agent-repl--input-text-block text)))
               (mapcar #'agent-repl--input-image-block attachments)))))

(defun agent-repl--input-accepted (ws raw arm &optional from-buffer)
  "Finish an ACCEPTED submission for WS: keep the record, clear what was sent.
ARM names which accepted arm answered, for the log.  Applies to every
success arm: a minted turn, a resolved command panel, a
recognized-but-unsupported command and a session-acting command the
daemon acted on are all answers, and none of them leaves the sent text
owed a resend.

FROM-BUFFER says the submitted text WAS the composer\='s contents.  Only
then is the composer erased: a canned command send (`agent-repl-update-pr\='
and every other explicit-TEXT site) composed its own words, and erasing
an unrelated half-written draft the user never submitted would destroy
work the daemon was never told about (ruling on audit-3 #51).

RAW is pushed onto the input history either way -- the ring is the record
of what was sent from this workspace, and a canned prompt was sent.  The
history POSITION is only reset when the composer itself was cleared,
because a surviving draft keeps whatever navigation state it had.

ATTACHMENTS are cleared either way: the submission carried them, whatever
composed its text, so leaving them would re-send them on the next prompt."
  (let ((buf (agent-repl--input-buffer ws)))
    (when buf
      (with-current-buffer buf
        (agent-repl--history-push raw)
        (when from-buffer
          (agent-repl--history-reset)
          (erase-buffer))))
    (agent-repl--history-save ws)
    (agent-repl-input-clear-attachments ws)
    (agent-repl--log ws "elisp.input.accepted ws=%s arm=%S raw-len=%d buffer=%s from-buffer=%s"
                     ws arm (length raw) (and buf t) (and from-buffer t))))

(defun agent-repl--input-on-success (ws raw origin success &optional from-buffer)
  "Handle a `SubmitPromptSuccess' for WS.  ORIGIN and RAW name the send.
SUCCESS is the decoded outcome oneof.  FROM-BUFFER says RAW came from the
composer, which is the only thing that entitles an acceptance to erase
it."
  (pcase (plist-get success :arm)
    (:turn
     (agent-repl--info ws "elisp.input.turn-minted ws=%s origin=%S turn=%S"
                       ws origin (plist-get (plist-get success :value) :turn))
     (agent-repl--input-accepted ws raw :turn from-buffer)
     (agent-repl--run-send-posthooks ws raw))
    ((and (or :command-panel :command-refused :command-acted) arm)
     ;; Answered, nothing to await.  The webapp draws the panel or the
     ;; refusal card; Emacs's whole reaction is to stop waiting.
     (agent-repl--info ws "elisp.input.command-answered ws=%s origin=%S arm=%S value=%S"
                       ws origin arm (plist-get success :value))
     (agent-repl--input-accepted ws raw arm from-buffer))
    (arm
     (agent-repl--error ws "elisp.input.unknown-success-arm ws=%s arm=%S" ws arm))))

(defconst agent-repl--input-handover-arms '(:transferring-away :not-yet-adopted)
  "Refusal arms that are HANDOVER SIGNALS rather than user-facing failures.
The same two arms verbs.el routes, for the same reason: the fanout makes
the handover ordering enforced BY REFUSAL on EVERY per-workspace rpc, and
SubmitPrompt is one.  A lagging client self-heals from the refusal, so
telling the user their submission was refused would be reporting a fault
during the one rollout that is supposed to be invisible.")

(defun agent-repl--input-on-handover-refusal (ws said origin raw arm key)
  "Route ARM, a handover refusal of WS's submission, and HOLD the prompt.
Two acts, both required.  host.el walks the handover -- the redial, the
adopt, the webview -- because the refusal is the same fact its own pushes
carry.  The prompt itself goes to the OUTAGE queue under THIS attempt\='s
KEY: it was refused, not consumed, so re-driving it is a RETRY of this
submission and must go back out under the same key for the daemon to
recognize a duplicate.  The queue releases it on the promotion, which is
the moment the successor is the primary and the workspace is adopted.

The composer KEEPS its text: nothing here says the prompt landed, and the
user is left able to see exactly what they wrote."
  (agent-repl--info ws "elisp.input.handover-refusal ws=%s origin=%S arm=%S key=%s"
                    ws origin (plist-get arm :arm) key)
  (agent-repl-host-handle-refusal ws arm)
  (agent-repl-prompt-queue-offer ws said origin raw key))

(defconst agent-repl--input-bubble-refusal-labels
  '((:not-deliverable . "no route to that agent")
    (:agent-busy . "that agent's own turn is running"))
  "The user-facing sentence for each SubmitPromptBubbleRefused kind.
The two kinds are the same user act meeting the same answer, so they are
drawn alike -- only the sentence differs.")

(defun agent-repl--input-on-bubble-refusal (ws origin refused)
  "Draw REFUSED, the shim\='s refusal of WS\='s bubble-addressed prompt.
ORIGIN is the submission\='s own.  REFUSED is the decoded
`SubmitPromptBubbleRefused\=' (`:detail\=' `:kind\=').  An unknown kind is a
contract breach the codec already refuses, so reaching one here is
recorded at ERROR rather than drawn as a plain refusal."
  (let* ((kind (plist-get (plist-get refused :kind) :arm))
         (detail (plist-get refused :detail))
         (label (cdr (assq kind agent-repl--input-bubble-refusal-labels))))
    (if (null label)
        (progn
          (agent-repl--error ws "elisp.input.bubble-refused-unknown-kind ws=%s kind=%S" ws kind)
          (message "agent-repl: the prompt was refused for this agent (%S)" kind))
      (agent-repl--warn ws "elisp.input.bubble-refused ws=%s origin=%S kind=%S detail=%s"
                        ws origin kind detail)
      (agent-repl--input-flash ws (format "refused: %s" label))
      (message "agent-repl: refused -- %s%s" label
               (if (string-empty-p detail) "" (format " (%s)" detail))))))

(defun agent-repl--input-on-error (ws said origin raw error key)
  "Handle a `SubmitPromptError' for WS.  The composer KEEPS its text.
ERROR is the decoded error message.  Its `merging' arm means the prompt
arrived after a merge began and would be orphaned, so the user resubmits
once the merge resolves.  Its two HANDOVER arms are not failures at all
and are routed to host.el.  Its `duplicate_submission' arm says the key
was already accepted, so the earlier submission stands and nothing is
resent.  Its `bubble_refused' arm is the SHIM's refusal of a
bubble-addressed prompt, relayed by kind and drawn to the user without
being queued.  Every other arm is a refusal this composer
has no treatment for: it is recorded at ERROR naming the arm and drawn to
the user, and the text stays where it is.

SAID, RAW and KEY are the submission\='s own, carried so a handover
refusal can re-drive the very prompt that was refused."
  (let* ((reason (plist-get error :reason))
         (arm (plist-get reason :arm)))
    (pcase arm
      (:merging
       (agent-repl--warn ws "elisp.input.refused-merging ws=%s origin=%S" ws origin)
       (agent-repl--input-flash ws "refused: merge in flight")
       (message "agent-repl: refused -- a merge is in flight for this workspace"))
      (:duplicate-submission
       ;; The key was already accepted for this workspace, so the earlier
       ;; submission stands.  Nothing landed twice and nothing is owed a
       ;; resend: this is an ANSWER about identity, not a transport
       ;; failure, so the prompt is NOT queued.  The text and the
       ;; attachments stay put so the user can see what they wrote.
       (agent-repl--warn ws "elisp.input.duplicate-submission ws=%s origin=%S arm=%S key=%s"
                         ws origin arm key)
       (agent-repl--input-flash ws "already submitted")
       (message "agent-repl: this submission's key was already accepted; the earlier submission stands"))
      (:bubble-refused
       ;; The SHIM refused a bubble-addressed prompt and the daemon relayed
       ;; the refusal BY KIND.  Both kinds are the same answer to the user
       ;; ("not this agent, not now"), so both are drawn alike and neither
       ;; is queued: a re-drive would meet the same refusal.  The shim's
       ;; `detail' is echoed verbatim for the human and for the log; it is
       ;; never switched on.
       (agent-repl--input-on-bubble-refusal ws origin (plist-get reason :value)))
      ((pred (lambda (a) (memq a agent-repl--input-handover-arms)))
       (agent-repl--input-on-handover-refusal ws said origin raw reason key))
      (_
       (agent-repl--error ws "elisp.input.unknown-error-arm ws=%s arm=%S" ws arm)
       (message "agent-repl: submission refused (%S)" arm)))))

(defun agent-repl--input-on-failure (ws said origin raw detail key)
  "Handle a TRANSPORT failure for WS's submission.
The daemon never answered, so nothing is known about whether the prompt
landed.  The composer KEEPS its text and the prompt is offered to the
hold queue, which sends it for real once the link is back.

KEY is THIS attempt\='s idempotency key and rides into the queue: the
re-drive is a RETRY of this submission, not a second turn, so it goes
back out under the same key and the daemon can refuse a duplicate."
  (agent-repl--error ws "elisp.input.transport-failure ws=%s origin=%S key=%s detail=%S"
                     ws origin key detail)
  (agent-repl-prompt-queue-offer ws said origin raw key)
  (agent-repl--input-flash ws "send failed -- held until the daemon is back")
  (message "agent-repl: the daemon did not answer; the prompt is held"))

(cl-defun agent-repl--input-submit (ws said origin raw &optional key from-buffer)
  "Submit SAID to WS with ORIGIN; RAW is the user's own text for the record.
Resolves the workspace ref and the connection, mints the idempotency key,
and dispatches the answer arms.  Returns the idempotency key.

KEY re-uses an earlier attempt\='s idempotency key instead of minting a
fresh one -- the outage queue\='s re-drive passes the failed attempt\='s
key, because a re-drive is a RETRY and not a second turn.

FROM-BUFFER says RAW was read out of the composer, and rides all the way
to the acceptance because only a composer-sourced submission may erase
the composer.  A canned command, a metaprompt read and a queue re-drive
all compose their own text and leave the user\='s draft alone.

The ref is REQUIRED on every submit: it names WHICH workspace the
submission belongs to, and it is the daemon-minted echo token, never a
value Emacs constructs from a path."
  (let ((ref (agent-repl-host-ref ws))
        (conn (or (agent-repl-host-conn ws) (agent-repl-link-primary)))
        (key (or key (agent-repl--uuid))))
    (unless ref
      (agent-repl--fatal ws "elisp.input.submit-no-ref ws=%s origin=%S" ws origin))
    (unless conn
      (agent-repl--warn ws "elisp.input.submit-no-conn ws=%s origin=%S" ws origin)
      (agent-repl--input-on-failure
       ws said origin raw (list :kind :transport :message "no daemon connection") key)
      (cl-return-from agent-repl--input-submit key))
    (agent-repl--info ws "elisp.input.submit ws=%s origin=%S key=%s blocks=%d"
                      ws origin key (length (plist-get (plist-get said :content) :blocks)))
    (agent-repl-rpc-submit-prompt
     conn (list :said said :idempotency-key key :origin origin :workspace ref)
     :on-response
     (lambda (response)
       (pcase (plist-get response :arm)
         (:success (agent-repl--input-on-success
                    ws raw origin (plist-get response :value) from-buffer))
         (:error (agent-repl--input-on-error
                  ws said origin raw (plist-get response :value) key))
         (arm (agent-repl--error ws "elisp.input.unknown-response-arm ws=%s arm=%S" ws arm))))
     :on-failure
     (lambda (detail) (agent-repl--input-on-failure ws said origin raw detail key)))
    key))

;;;; ---- The send pipeline ------------------------------------------------

(defun agent-repl--read-input-buffer (ws)
  "Return the text contents of WS's input buffer, or nil."
  (let ((buf (agent-repl--input-buffer ws)))
    (if buf
        (with-current-buffer buf (buffer-string))
      (agent-repl--log-verbose ws "elisp.input.read-no-buffer ws=%s" ws)
      nil)))

(defun agent-repl--send (origin &optional prompt ws force-metaprompt)
  "Submit PROMPT (or the input buffer's contents) to workspace WS.
ORIGIN is this send site's own `PromptOrigin' keyword and is REQUIRED --
it rides the wire and the daemon persists it onto the turn's durable
record.  When PROMPT is nil the text comes from the input buffer, which
is cleared only once the daemon has ACCEPTED the submission.  When WS is
nil the current workspace is used.  FORCE-METAPROMPT prepends the
on-demand read-directive.

Returns the submission's idempotency key, or nil when nothing was sent."
  (interactive (list :user-sent))
  (unless (memq origin agent-repl--input-origins)
    (agent-repl--fatal ws "elisp.input.invalid-origin origin=%S" origin))
  (let ((ws (or ws (agent-repl--ws-current-name))))
    (unless ws
      (agent-repl--fatal nil "elisp.input.no-workspace origin=%S prompt-supplied=%s"
                         origin (not (null prompt))))
    (agent-repl--log ws "elisp.input.send ws=%s origin=%S force-metaprompt=%s from-buffer=%s"
                     ws origin force-metaprompt (null prompt))
    (agent-repl--input-check-gate ws)
    (let* ((raw (or prompt (agent-repl--read-input-buffer ws) ""))
           (attachments (agent-repl-input-attachments ws))
           (empty-p (and (string-empty-p (string-trim raw)) (null attachments))))
      (if empty-p
          (progn
            (agent-repl--log ws "elisp.input.send-empty ws=%s origin=%S -- nothing to send"
                             ws origin)
            nil)
        (let* ((text (agent-repl--prepare-input ws raw force-metaprompt))
               (said (agent-repl--input-said text attachments)))
          (agent-repl--kickoff-prompt-summary ws raw)
          ;; `(null prompt)' is the one fact that says the words came out
          ;; of the composer, and it is the only thing that entitles the
          ;; acceptance to erase it.
          (agent-repl--input-submit ws said origin raw nil (null prompt)))))))

;;;; ---- The send sites --------------------------------------------------
;;
;; ONE production site per origin, no exceptions: the vocabulary is closed
;; and durable precisely so a stored turn names the editor situation that
;; produced it.

(defun agent-repl-send ()
  "Submit the composer's contents (`PROMPT_ORIGIN_USER_SENT')."
  (interactive)
  (agent-repl--send :user-sent))

(defun agent-repl-send-and-hide ()
  "Submit the composer's contents and hide both panels."
  (interactive)
  (agent-repl--log (agent-repl--ws-current-log-name) "elisp.input.send-and-hide")
  (agent-repl--send :user-sent-and-hide)
  (agent-repl--on-close))

(defun agent-repl-send-with-metaprompt ()
  "Submit with the metaprompt read-directive prepended.
The deliberate on-demand re-read: an ordinary send carries no directive,
because the metaprompt is already the session's system prompt."
  (interactive)
  (agent-repl--log (agent-repl--ws-current-log-name) "elisp.input.send-with-metaprompt")
  (agent-repl--send :user-sent-with-metaprompt nil nil t))

(defun agent-repl--fire-metaprompt-read (ws)
  "Submit the metaprompt read-directive to WS as a standalone message.
The programmatic half of the on-demand re-read, for callers that want the
agent to go re-read the file without any user prompt riding along.
NOTHING CALLS THIS AUTOMATICALLY.  The directive is meta-wrapped so the
daemon strips it from the drawn row; RAW is empty because no user text
sits behind a harness re-read."
  (if (not agent-repl-command-prefix)
      (agent-repl--log ws "elisp.input.metaprompt-read-disabled ws=%s" ws)
    (agent-repl--log ws "elisp.input.metaprompt-read ws=%s" ws)
    (agent-repl--input-check-gate ws)
    (agent-repl--input-submit
     ws
     (agent-repl--input-said (agent-repl--meta-wrap (agent-repl--command-prefix-for ws))
                             (agent-repl-input-attachments ws))
     :metaprompt-read "")))

(defun agent-repl--append-to-input-buffer (text)
  "Append TEXT to the end of the current workspace's input buffer."
  (let* ((ws (agent-repl--ws-current-name))
         (buf (agent-repl--input-buffer ws)))
    (agent-repl--log ws "elisp.input.append len=%d buffer=%s" (length text) (and buf t))
    (if buf
        (with-current-buffer buf
          (goto-char (point-max))
          (insert text))
      (agent-repl--warn ws "elisp.input.append-no-buffer ws=%s -- text not appended" ws))))

(defun agent-repl-send-with-postfix ()
  "Append `agent-repl-send-postfix' to the composer, then submit."
  (interactive)
  (agent-repl--log (agent-repl--ws-current-log-name) "elisp.input.send-with-postfix")
  (agent-repl--append-to-input-buffer agent-repl-send-postfix)
  (agent-repl--send :user-sent-with-postfix))

(defun agent-repl--prepend-to-input-buffer (text)
  "Prepend TEXT to the start of the current workspace's input buffer."
  (let* ((ws (agent-repl--ws-current-name))
         (buf (agent-repl--input-buffer ws)))
    (agent-repl--log ws "elisp.input.prepend len=%d buffer=%s" (length text) (and buf t))
    (if buf
        (with-current-buffer buf
          (goto-char (point-min))
          (insert text))
      (agent-repl--warn ws "elisp.input.prepend-no-buffer ws=%s -- text not prepended" ws))))

(defun agent-repl-send-with-prefix ()
  "Prepend `agent-repl-send-prefix' to the composer, then submit."
  (interactive)
  (agent-repl--log (agent-repl--ws-current-log-name) "elisp.input.send-with-prefix")
  (agent-repl--prepend-to-input-buffer agent-repl-send-prefix)
  (agent-repl--send :user-sent-with-prefix))

;;;; ---- Keybindings -----------------------------------------------------

(map! :map agent-repl-input-mode-map
      :ni "RET"       #'agent-repl-send
      :ni "S-RET"     #'newline
      :ni "C-RET"     #'agent-repl-send-with-postfix
      ;; Deferred-prompt enqueue lives on `SPC j RET' in the leader map
      ;; (keybindings.el) so it is reachable from any context with one
      ;; canonical chord.  Prefix-send stays reachable on macOS via
      ;; `S-s-RET' caught by the `[remap +default/newline-above]' entry.
      [remap +default/newline-below] #'agent-repl-send-with-postfix
      [remap +default/newline-above] #'agent-repl-send-with-prefix
      :ni "C-c C-c"   #'agent-repl-discard-input
      :n  "<up>"      #'agent-repl--history-prev
      :n  "<down>"    #'agent-repl--history-next
      ;; Prompt-history search sits on `C-M-r', not the `C-r' its shell
      ;; reflex would suggest: `C-r' is vacated for the output feed's
      ;; incremental search, whose isearch reflex wants it.
      :ni "C-M-r"     #'agent-repl-history-search)

(provide 'input)

;;; input.el ends here
