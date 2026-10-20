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
;;   success `ask_answered'    the send was a STANDING ASK's typed answer
;;                             (a question's active question, or a
;;                             permission denied with it as the reason);
;;                             no turn, so it is handled like a command
;;                             answer: logged, input cleared
;;   error `ask_not_standing'  the ask settled before the typed answer
;;                             reached it: refused, the text is KEPT
;;   error `merging'           refused: the text is KEPT so the user can
;;                             resubmit once the merge resolves
;;   transport failure         the daemon never answered: the prompt is
;;                             written to the durable held-prompt ingress
;;                             (`held-ingress.el'), which the daemon
;;                             ingests into its held queue once it serves
;;
;; PROMPT ORIGINS ARE A CLOSED VOCABULARY and every value has exactly ONE
;; production send site, so a stored turn traces back to the exact editor
;; situation that caused it.  `agent-repl--input-origins' is the list this
;; file accepts; anything else is refused before a request is built.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(declare-function agent-repl-webhost-dismiss-digest "webhost" (ws))
(declare-function agent-repl--frontend-webview-buffer-name "frontend" (ws))
(declare-function evil-normal-state "evil-states" (&optional arg))
(declare-function agent-repl--log "agent-repl-core" (ws fmt &rest args))
(declare-function agent-repl--now-unix-ms "agent-repl-core" ())
(declare-function agent-repl--log-verbose "agent-repl-core" (ws fmt &rest args))
(declare-function agent-repl--info "agent-repl-core" (ws fmt &rest args))
(declare-function agent-repl--warn "agent-repl-core" (ws fmt &rest args))
(declare-function agent-repl--error "agent-repl-core" (ws fmt &rest args))
(declare-function agent-repl-refill-keymap "core" (map fill))
(declare-function agent-repl--fatal "agent-repl-core" (ws fmt &rest args))
(declare-function agent-repl--buffer-owner "agent-repl-core" (buf))
(declare-function agent-repl--with-log-context "agent-repl-core"
                  (workspace request-id function))
(declare-function agent-repl--register-timer "agent-repl-core" (key timer))
(declare-function agent-repl--cancel-timer-key "agent-repl-core" (key))
(defvar agent-repl--log-context-request-id)
(defvar agent-repl--log-context-workspace)
(defvar agent-repl--global-log-scope)
(declare-function agent-repl--meta-wrap "agent-repl-core" (text))
(declare-function agent-repl--set-buffer-background "agent-repl-core" (color))
(declare-function agent-repl--ws-current-name "agent-repl-workspace" ())
(declare-function agent-repl--ws-current-log-name "agent-repl-workspace" ())
(declare-function agent-repl--ws-get "agent-repl-workspace" (ws key))
(declare-function agent-repl--history-push "agent-repl-history" (&optional text))
(declare-function agent-repl--history-push-discard "agent-repl-history" ())
(declare-function agent-repl--history-reset "agent-repl-history" ())
(declare-function agent-repl--history-save "agent-repl-history" (ws))
(declare-function agent-repl--history-on-change "agent-repl-history" (&rest args))
(declare-function agent-repl--history-prev "agent-repl-history" ())
(declare-function agent-repl--history-next "agent-repl-history" ())
(declare-function agent-repl-history-search "agent-repl-history" ())
(declare-function agent-repl--history-unsent-head "agent-repl-history" ())
(declare-function agent-repl--ws-registered-names "agent-repl-workspace" ())
;; The history ring's own buffer-local state, declared here because the
;; refusal restore puts it back to its pre-submit value (see
;; `agent-repl--input-restore').
(defvar agent-repl--input-history)
(defvar agent-repl--history-index)
(defvar agent-repl--history-navigating)
(defvar agent-repl--history-discard-entry)
(declare-function agent-repl--on-simple-close "agent-repl-panels" (&optional ws))
(declare-function agent-repl-host-ref "agent-repl-host" (ws))
(declare-function agent-repl-verb-retry-vendor-error "verbs" (ws))
(declare-function agent-repl-host-conn-or-primary "agent-repl-host" (ws))
(declare-function agent-repl-command-completion-at-point "command-completion" ())
(declare-function agent-repl--command-completion-on-change "command-completion" (beg end len))
(declare-function agent-repl-workspace-completion-at-point "workspace-completion" ())
(declare-function agent-repl-host-composer-gate "agent-repl-host" (ws))
(declare-function agent-repl-host-standing-ask "agent-repl-host" (ws))
(declare-function agent-repl-rpc-update-question-draft "agent-repl-rpc" (conn request &rest keys))
(declare-function agent-repl-rpc-answer-permission "agent-repl-rpc" (conn request &rest keys))
(declare-function agent-repl-host-selection "agent-repl-host" (ws))
(declare-function agent-repl-link-primary "agent-repl-daemon-link" ())
(declare-function agent-repl-rpc-submit-prompt "agent-repl-rpc" (conn request &rest keys))
(declare-function agent-repl-rpc-select-feed-row "agent-repl-rpc" (conn request &rest keys))
(declare-function agent-repl-rpc-plan-rollback-sync "agent-repl-rpc" (conn request &optional timeout))
(declare-function agent-repl-rpc-roll-back "agent-repl-rpc" (conn request &rest keys))
(declare-function agent-repl-rpc-list-rollback-prompts-sync "agent-repl-rpc" (conn request &optional timeout))
(declare-function agent-repl--format-summary-relative-time "agent-repl-prompt-summary" (sent-at &optional now))
(declare-function agent-repl--image-insert-marker "clipboard-image" (path &optional ws))
(declare-function agent-repl-held-edit-active-p "held-edit" (ws))
(declare-function agent-repl-held-edit-commit "held-edit" (ws said snapshot))
(declare-function agent-repl-held-edit-cancel "held-edit" (ws))
(declare-function agent-repl-rpc-adjust-feed-text-scale "agent-repl-rpc" (conn request &rest keys))
(declare-function agent-repl-host-handle-refusal "agent-repl-host" (ws arm-plist))
(declare-function agent-repl-host-route-handover "agent-repl-host" (ws arm slug))
(declare-function agent-repl-interrupt-turn "agent-repl-verbs" (&optional ws))
(declare-function agent-repl-held-ingress-write "held-ingress" (ws said origin key &optional delivery))
(declare-function agent-repl--kickoff-prompt-summary "agent-repl-prompt-summary" (ws raw))
(declare-function evil-insert-state "evil" (&optional arg))
(declare-function agent-repl-window--panel-window "window" (kind &optional ws frame))
(declare-function evil-define-key* "evil-core" (state keymap key def &rest bindings))
(declare-function agent-repl-bind-feed-zoom-keys "core" (map))
(declare-function evil-normalize-keymaps "evil-core" (&optional state))
(declare-function evil-ex-search-forward "evil-commands" (&optional count))
(declare-function evil-ex-search-backward "evil-commands" (&optional count))
(declare-function markdown-mode "markdown-mode" ())
(declare-function agent-repl-popup-show "popup" (buffer))
(declare-function agent-repl-host-selection-markdown "agent-repl-host" (ws))

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

(defcustom agent-repl-input-background "#181817"
  "The input buffer's background, a #rrggbb hex string.
The webapp's dark `--composer-bg' (webapp/src/styles.css): the input
window is the host-native composer, so the two wear one color.  A touch
above the feed's #151514, still told apart from it (owner requests,
2026-10-17: halfway from the Claude composer's #20201f, then a touch
closer to the feed)."
  :type 'string
  :group 'agent-repl)

(defvar-local agent-repl-input-notice nil
  "The composer's standing notice, drawn in the input buffer's mode line.
Set to the refusal flash when a submission comes back refused, and
cleared when the gate opens.  Buffer-local to the input buffer, because
the notice is about THIS composer and no other.")

(defvar-local agent-repl-input-waiting nil
  "The composer's held-prompt waiting line, or nil.
\"N prompts waiting for the daemon\" while prompts this workspace's
composer could not hand to a live daemon sit in the durable ingress
(`held-ingress.el'), which is the only writer.  Drawn beside the notice
and never replaced by it: a refusal flash must not hide that prompts are
still owed.")

(defvar-local agent-repl-input-attachments nil
  "Images attached to this composer, oldest first.
Each element is the plist `(:path ABSOLUTE-PATH :media-type MIME)'.
`clipboard-image.el' appends to this list when the user attaches a
pasteboard image; `agent-repl--send' turns each entry into one
`ImageBlock' beside the text block and clears the list on a submission
the daemon accepted.")

(defvar-local agent-repl--input-flash-timer-key nil
  "Unique core timer-registry key for this composer's notice dwell.")

(defun agent-repl--input-cancel-flash-timer ()
  "Cancel this composer's pending notice dwell during buffer teardown.
This is a best-effort cleanup hook: a composer with no armed dwell has nothing
to cancel, while core's timer boundary reports whether a keyed timer existed."
  (when agent-repl--input-flash-timer-key
    (agent-repl--cancel-timer-key agent-repl--input-flash-timer-key)))

(defconst agent-repl--input-mode-line-spec
  '(:eval (agent-repl--input-notice-segment))
  "The composer's mode-line segment: the standing notice, or nothing.")

(defun agent-repl--input-notice-segment ()
  "Return the input buffer's mode-line notice text, or the empty string.
The standing notice first, then the held-prompt waiting line."
  (concat (if agent-repl-input-notice (concat " " agent-repl-input-notice) "")
          (if agent-repl-input-waiting (concat " [" agent-repl-input-waiting "]") "")))

(defun agent-repl--input-waiting (ws)
  "Return WS's composer's waiting line, or nil (also with no composer)."
  (let ((buf (agent-repl--input-buffer ws)))
    (and buf (buffer-local-value 'agent-repl-input-waiting buf))))

(defun agent-repl--input-set-waiting (ws text)
  "Set WS's composer waiting line to TEXT (nil clears it)."
  (let ((buf (agent-repl--input-buffer ws)))
    (agent-repl--log ws "elisp.input.waiting ws=%s text=%S buffer=%s" ws text (and buf t))
    (when buf
      (with-current-buffer buf
        (setq agent-repl-input-waiting text)
        (force-mode-line-update)))))

(define-derived-mode agent-repl-input-mode fundamental-mode "Agent Input"
  "Major mode for the host-native composer buffer."
  (setq-local header-line-format
              "C-c C-c: clear+save | (cmd) <up>/<down>: history | C-M-r: search history")
  (face-remap-add-relative 'header-line 'agent-repl-header-line)
  (agent-repl--set-buffer-background agent-repl-input-background)
  (unless (member agent-repl--input-mode-line-spec mode-line-format)
    (setq-local mode-line-format
                (append mode-line-format (list agent-repl--input-mode-line-spec))))
  ;; Widen the fill column so wrapped prose and `fill-paragraph' reflow to
  ;; 150 columns rather than the 70-column default.
  (setq-local fill-column 150)
  ;; Soft-wrap long input lines at word boundaries.  With
  ;; `evil-respect-visual-line-mode' set in init.el, `visual-line-mode' also
  ;; moves Evil's `j'/`k'/`0'/`$' by screen line, `gj'/`gk' by logical line.
  ;; The composer's normal-state `<up>'/`<down>' history keys still win over
  ;; Doom's motion-state screen-line `[up]'/`[down]', since evil consults
  ;; normal-state maps before motion-state ones.
  (visual-line-mode 1)
  ;; Intentionally unlogged: `after-change-functions' runs per keystroke, so
  ;; logging it would overwhelm the durable input/send lifecycle diagnostics.
  (add-hook 'after-change-functions #'agent-repl--history-on-change nil t)
  ;; A WORD STARTING WITH `/' completes against the workspace's runtime,
  ;; and a known one is marked purple (command-completion.el).
  (add-hook 'completion-at-point-functions #'agent-repl-command-completion-at-point nil t)
  (add-hook 'after-change-functions #'agent-repl--command-completion-on-change nil t)
  ;; A WORD THAT BEGINS AN OPEN WORKSPACE'S NAME completes to it
  ;; (workspace-completion.el).
  (add-hook 'completion-at-point-functions #'agent-repl-workspace-completion-at-point nil t)
  (add-hook 'kill-buffer-hook #'agent-repl--input-cancel-flash-timer nil t))

(defun agent-repl--input-buffer (ws)
  "Return WS's live input buffer, or nil."
  (let ((buf (agent-repl--ws-get ws :input-buffer)))
    (and buf (buffer-live-p buf) buf)))

(defun agent-repl--input-save-to-history (ws &optional discard)
  "Save the current composer's contents to WS's prompt history.
DISCARD non-nil saves as the discard does (`agent-repl--history-push-discard').
THE ONE SAVE a composer makes before its contents are replaced, shared by
the discard (`agent-repl-discard-input') and a rollback's refill
\(`agent-repl--input-rollback-done'): push the text (a blank or repeated
one is skipped), stop any history browsing, and persist the ring."
  (if discard (agent-repl--history-push-discard) (agent-repl--history-push))
  (agent-repl--history-reset)
  (agent-repl--history-save ws))

;;;; ---- Filling the composer from what a person said -------------------

(defun agent-repl--input-said-text (said)
  "Return the words of SAID (a decoded `UserSaid').
Its text blocks, joined in order by newlines."
  (mapconcat (lambda (block) (plist-get (plist-get block :value) :text))
             (cl-remove-if-not (lambda (block) (eq (plist-get block :arm) :text))
                               (plist-get (plist-get said :content) :blocks))
             "\n"))

(defun agent-repl--input-said-attachments (said)
  "Return SAID's image blocks as composer attachments, and what cannot be one.
The answer is the plist (:attachments LIST :dropped N).  A composer
attachment is a host PATH, `(:path :media-type)'; an image by URL, or a
block this schema does not model, cannot be put into the composer, and is
counted in N instead."
  (let ((attachments nil)
        (dropped 0))
    (dolist (block (plist-get (plist-get said :content) :blocks))
      (pcase (plist-get block :arm)
        (:text nil)
        (:image
         (let* ((image (plist-get block :value))
                (location (plist-get image :location)))
           (if (eq (plist-get location :arm) :path)
               (push (list :path (plist-get (plist-get location :value) :path)
                           :media-type (plist-get image :media-type))
                     attachments)
             (cl-incf dropped))))
        (_ (cl-incf dropped))))
    (list :attachments (nreverse attachments) :dropped dropped)))

(defun agent-repl--input-fill-said (ws buf said attachments)
  "Replace composer BUF's contents with SAID's words and ATTACHMENTS, for WS.
ATTACHMENTS are `(:path :media-type)' plists; each is attached and its
marker drawn, as a pasted image is."
  (with-current-buffer buf
    (let ((agent-repl--history-navigating t))
      (erase-buffer)
      (insert (agent-repl--input-said-text said))
      (setq agent-repl-input-attachments nil)
      (dolist (attachment attachments)
        (goto-char (point-max))
        (agent-repl-input-attach-image (plist-get attachment :path)
                                       (plist-get attachment :media-type))
        (agent-repl--image-insert-marker (plist-get attachment :path) ws))
      (goto-char (point-max)))))

(defun agent-repl-discard-input ()
  "Save current input to history, clear the buffer, and enter insert state.
When the most recent history item is one an earlier discard added and is a
substring of this input, it is replaced by this input instead of a new item
being pushed.  While the composer is editing a held prompt this is also the
edit's CANCEL: the held prompt keeps its content and the queue resumes
\(`agent-repl-held-edit-cancel')."
  (interactive)
  (let ((ws (agent-repl--ws-current-name))
        (input-len (buffer-size)))
    (agent-repl--log ws "elisp.input.discard ws=%s input-len=%d" ws input-len)
    (when (agent-repl-held-edit-active-p ws)
      (agent-repl-held-edit-cancel ws))
    (agent-repl--input-save-to-history ws t)
    (erase-buffer)
    (setq agent-repl-input-attachments nil)
    (when (fboundp 'evil-insert-state) (evil-insert-state))))

;;;; ---- Unsent input across an Emacs restart ----------------------------

;; WHAT A COMPOSER HELD WHEN EMACS QUIT IS THERE WHEN IT COMES BACK.  At
;; shutdown every workspace's non-empty composer is saved to its history
;; exactly as `agent-repl-discard-input' saves it
;; (`agent-repl--input-save-to-history' with DISCARD), which leaves that text
;; as the UNSENT head of the ring (history.el).  When the composer is born
;; again its unsent head is put back in it.  Nothing is sent either way.

(defun agent-repl--input-save-on-shutdown (ws)
  "Save WS's composer contents as the discard does, when there are any.
Returns `saved', `empty' or `no-composer'."
  (let ((buf (agent-repl--input-buffer ws)))
    (cond
     ((null buf)
      (agent-repl--log-verbose ws "elisp.input.shutdown-save ws=%s outcome=no-composer" ws)
      'no-composer)
     ((string-empty-p (string-trim (with-current-buffer buf (buffer-string))))
      (agent-repl--log ws "elisp.input.shutdown-save ws=%s outcome=empty" ws)
      'empty)
     (t
      (with-current-buffer buf
        (agent-repl--input-save-to-history ws t))
      (agent-repl--info ws "elisp.input.shutdown-save ws=%s outcome=saved input-len=%d"
                        ws (buffer-size buf))
      'saved))))

(defun agent-repl-input-save-all-on-shutdown ()
  "Save every workspace's non-empty composer to its history, for `kill-emacs-hook'.
One workspace's failure is recorded at error and does not stop the others
or the shutdown."
  (dolist (ws (agent-repl--ws-registered-names))
    (condition-case err
        (agent-repl--input-save-on-shutdown ws)
      (error
       (agent-repl--error ws "elisp.input.shutdown-save-failed ws=%s err=%S" ws err)))))

(defun agent-repl--input-restore-unsent (ws)
  "Put WS's unsent history head into its composer, never over standing text.
Run once the composer's history is restored.  Returns `restored',
`occupied', `none' or `no-composer'."
  (let ((buf (agent-repl--input-buffer ws)))
    (if (null buf)
        (progn
          (agent-repl--warn ws "elisp.input.restore-unsent ws=%s outcome=no-composer" ws)
          'no-composer)
      (with-current-buffer buf
        (let ((unsent (agent-repl--history-unsent-head)))
          (cond
           ((null unsent)
            (agent-repl--log ws "elisp.input.restore-unsent ws=%s outcome=none" ws)
            'none)
           ((> (buffer-size) 0)
            (agent-repl--info ws "elisp.input.restore-unsent ws=%s outcome=occupied standing-len=%d unsent-len=%d"
                              ws (buffer-size) (length unsent))
            'occupied)
           (t
            (let ((agent-repl--history-navigating t))
              (insert unsent))
            (agent-repl--info ws "elisp.input.restore-unsent ws=%s outcome=restored input-len=%d"
                              ws (length unsent))
            'restored)))))))

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
buffer a DIFFERENT notice -- a later refusal's own flash, most of all.  A
dwell that resolved the buffer by name and cleared whatever it found would
erase a notice it never set: the first flash's timer would cut the second
flash's dwell short.

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
      (with-current-buffer buffer
        (unless agent-repl--input-flash-timer-key
          (setq agent-repl--input-flash-timer-key
                (make-symbol (format "agent-repl-input-flash-%s" ws))))
        (agent-repl--register-timer
         agent-repl--input-flash-timer-key
         (run-at-time agent-repl-input-flash-seconds nil
                      #'agent-repl--input-expire-flash ws buffer text))))))

;;;; ---- Attachments -----------------------------------------------------

(defconst agent-repl--input-image-marker-property 'agent-repl-image-marker
  "Text property naming a span of composer text as an ATTACHMENT MARKER.
Its value is the attached image's path.  `clipboard-image.el' puts it
over the marker line it draws (and that line's newline), and
`agent-repl--read-input-buffer' drops every span carrying it -- so the
marker is DRAWN and never SENT.  The image travels as its own
`ImageBlock'; a marker riding the words would leave the agent reading a
filename it was never meant to see.

Declared here, in the file that loads first and does the stripping, so
one name serves both sides.  A TEXT PROPERTY rather than the thumbnail
overlay: the overlay exists only when a thumbnail could be drawn, and a
TTY frame gets none while still needing the marker stripped.")

(defun agent-repl-input-attach-image (path media-type)
  "Register PATH (MIME MEDIA-TYPE) as an image attached to THIS composer.
The ONE entry point that attaches an image: `clipboard-image.el' calls it
after capturing the pasteboard, and it operates on the CURRENT buffer,
which must be a composer.  Returns the resulting attachment list.

The file is NOT read here: an `ImageBlock' carries a REFERENCE, and the
daemon and the shim run on this same host, so the path is the whole
payload."
  (unless (derived-mode-p 'agent-repl-input-mode)
    (agent-repl--fatal '(:agent-repl-central "the attachment request has no composer workspace")
                       "elisp.input.attach-outside-composer buffer=%s path=%s"
                       (buffer-name) path))
  (let ((ws (agent-repl--buffer-owner (current-buffer))))
    (setq agent-repl-input-attachments
          (append agent-repl-input-attachments
                  (list (list :path path :media-type media-type))))
    (agent-repl--info ws "elisp.input.attached ws=%s path=%s media-type=%s count=%d"
                      ws path media-type (length agent-repl-input-attachments))
    agent-repl-input-attachments))

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

;;;; ---- Feed selection (reply to a response, roll back to a prompt) -------
;;
;; The feed's SELECTION, command-mode side.  In the composer, `M-s-j'/`M-s-k'
;; (normal and insert state) step through the turn boundaries: the prompts a
;; rollback can reach and the final responses, walked together in feed
;; order.  A selected final response is what the next prompt sent replies to,
;; and a selected prompt is what the rollback keys roll back to;
;; escape-twice clears it.  The DAEMON owns the selection per workspace: it knows the ordered
;; rows, so it COMPUTES where a step lands (from no selection an older step
;; starts at the newest row and a newer one at the oldest; older wraps past
;; the oldest, and newer past the newest ends the selection, returning the
;; feed to its tail, so the next newer rolls round to the oldest), pushes the result to the webapp and to
;; the host watch, and applies its own selection when a prompt is sent.
;; Emacs sends only the MOVE, and reads which KIND of row is selected off the
;; host watch (`agent-repl-host-selection') for the escape state machine.
;; A click in the webview selects too, and any landed bubble can be selected
;; that way (the `:bubble' kind); the host watch reports every change the
;; same way, whoever made it.  While anything is selected,
;; `agent-repl-input-selection-mode' stands: `/' and `?' (command mode) and
;; `C-s' (every state) open the selected bubble's markdown in a read-only
;; popup and search it there (`agent-repl-response-search-forward'), and
;; `RET' rolls back to a selected prompt, or sends the composer as a reply
;; to any other selected bubble (`agent-repl-input-selection-return').  The
;; webapp expands a bubble the moment it is selected, so the bubble searched
;; is the one drawn open.

(defconst agent-repl--input-selection-escape-warning
  "selection: press escape again to clear the selection"
  "The minibuffer WARNING shown on the FIRST command-mode escape.
No y/n confirmation -- a plain warning that a SECOND consecutive escape
clears the selection, so the user reads it before anything is cleared.")

(defconst agent-repl--input-selection-flash-prefixes
  '((:turn-boundary . "selection")
    (:clear . "selection"))
  "The composer-flash prefix each `SelectFeedRow' move speaks under.")

(defconst agent-repl--input-selection-nothing-selectable-flashes
  '((:turn-boundary . "selection: no prompt or final response to select"))
  "The composer flash when a step finds no row of its kind to select.")

(defun agent-repl--input-selection-active-p (ws)
  "Return non-nil when WS's feed has a row selected, as the host watch said."
  (memq (agent-repl-host-selection ws) '(:response :prompt :bubble)))

(defvar agent-repl-input-selection-mode-map (make-sparse-keymap)
  "Keymap of `agent-repl-input-selection-mode'.
`RET' and `C-s' in every state, `/' and `?' in normal state.")

(defun agent-repl--input-fill-selection-map (map)
  "Bind MAP's plain `RET' and `C-s', which emacs state reads.
The evil states read the auxiliary maps `agent-repl--input-selection-bind-keys'
fills, which outrank the composer's own `RET'."
  (define-key map (kbd "RET") #'agent-repl-input-selection-return)
  (define-key map (kbd "C-s") #'agent-repl-response-search-buffer))

;; Refilled on every load, BEFORE the evil binder below, so a reload drops a
;; removed binding and the evil auxiliary maps are rebuilt on top.
(agent-repl-refill-keymap agent-repl-input-selection-mode-map
                          #'agent-repl--input-fill-selection-map)

(define-minor-mode agent-repl-input-selection-mode
  "The composer's behavior while a feed row is selected.
`/' and `?' in command mode search the selected bubble's markdown instead of
the composer.  Turned on and off by `agent-repl--input-selection-changed'
alone, so it stands exactly while the daemon's selection does."
  :keymap agent-repl-input-selection-mode-map)

(defun agent-repl--input-selection-bind-keys ()
  "Bind the selection mode's keys in its Evil auxiliary maps.
`/' and `?' in normal state; `RET' and `C-s' in normal and insert state."
  (evil-define-key* 'normal agent-repl-input-selection-mode-map
    "/" #'agent-repl-response-search-forward
    "?" #'agent-repl-response-search-backward)
  (evil-define-key* '(normal insert) agent-repl-input-selection-mode-map
    (kbd "RET") #'agent-repl-input-selection-return
    (kbd "C-s") #'agent-repl-response-search-buffer))

(defun agent-repl-input-selection-return ()
  "Act on the selected bubble with `RET'.
A selected prompt a rollback can reach is rolled back to, keeping files,
exactly as `C-c C-RET' does.  Any other selected bubble (a final response
in particular) is replied to: the composer is sent, and the daemon quotes
the selected bubble ahead of it."
  (interactive)
  (let* ((ws (agent-repl--ws-current-name))
         (kind (agent-repl-host-selection ws)))
    (agent-repl--info ws "elisp.input.selection-return ws=%s kind=%S" ws kind)
    (if (eq kind :prompt)
        (agent-repl-rollback-keep-files)
      (agent-repl-send))))

(with-eval-after-load 'evil
  (agent-repl--input-selection-bind-keys))

(defun agent-repl--input-selection-changed (ws)
  "Follow WS's selection as the host watch last pushed it.
Turns `agent-repl-input-selection-mode' on or off in WS's composer, and
refills an open response view so it never shows a bubble that is no longer
the selection.  A workspace with no composer has nothing to follow."
  (let ((buf (agent-repl--input-buffer ws))
        (active (agent-repl--input-selection-active-p ws)))
    (agent-repl--log ws "elisp.input.selection-changed ws=%s active=%s composer=%s"
                     ws (and active t) (and buf t))
    (when buf
      (with-current-buffer buf
        (agent-repl-input-selection-mode (if active 1 -1))
        (when (fboundp 'evil-normalize-keymaps) (evil-normalize-keymaps))))
    (when active
      (agent-repl--response-view-refresh ws (agent-repl-host-selection-markdown ws)))))

(defun agent-repl-input-focus-for-reply (ws)
  "Select WS's composer window in insert state, to type a reply.
Called when the reader's click in the webview selected a final response
\(the host watch's `final_response_clicked').  Only a composer already on
the selected frame is focused: a gated workspace has none, and mounting
panels from a click would move the layout, so either is recorded and left
alone.  Returns the composer window when it took the keyboard, else nil."
  (let ((win (agent-repl-window--panel-window :input ws)))
    (if (not win)
        (progn
          (agent-repl--info ws "elisp.input.focus-for-reply ws=%s outcome=no-composer-window" ws)
          nil)
      (select-window win)
      (with-current-buffer (window-buffer win)
        (when (fboundp 'evil-insert-state) (evil-insert-state)))
      (agent-repl--info ws "elisp.input.focus-for-reply ws=%s outcome=focused window=%S" ws win)
      win)))

;;;; ---- The response view: the selected bubble, searched with real Evil ----

(defun agent-repl--response-view-name (ws)
  "The name of WS's response view buffer."
  (format "*agent-repl response: %s*" ws))

(defun agent-repl--response-view-fill (buffer markdown)
  "Fill BUFFER with MARKDOWN as a read-only `markdown-mode' buffer, point at top."
  (with-current-buffer buffer
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert markdown))
    (unless (derived-mode-p 'markdown-mode) (markdown-mode))
    (setq buffer-read-only t)
    (set-buffer-modified-p nil)
    (goto-char (point-min))))

(defun agent-repl--response-view-refresh (ws markdown)
  "Refill WS's open response view with MARKDOWN; nothing when none is open."
  (let ((view (get-buffer (agent-repl--response-view-name ws))))
    (when (buffer-live-p view)
      (agent-repl--log ws "elisp.input.response-view-refreshed ws=%s text-len=%d" ws (length markdown))
      (agent-repl--response-view-fill view markdown))))

(defun agent-repl--response-view-open (ws)
  "Open WS's selected bubble's markdown in the shared popup; return its window.
The text is the one the host watch's selection push delivered, so the view
shows exactly the bubble the daemon has selected.  Called only while
`agent-repl-input-selection-mode' stands, which is only while a selection
does: a missing text is a broken invariant, and signals."
  (let ((markdown (agent-repl-host-selection-markdown ws)))
    (unless (stringp markdown)
      (agent-repl--error ws "elisp.input.response-view-no-text ws=%s selection=%S"
                         ws (agent-repl-host-selection ws))
      (error "agent-repl: no selected bubble text to search in %s" ws))
    (let ((view (get-buffer-create (agent-repl--response-view-name ws))))
      (agent-repl--response-view-fill view markdown)
      (agent-repl--info ws "elisp.input.response-view-opened ws=%s text-len=%d" ws (length markdown))
      (agent-repl-popup-show view))))

(defun agent-repl--response-search (search)
  "Open the selected bubble's view, select it, and run Evil's SEARCH there."
  (let ((window (agent-repl--response-view-open (agent-repl--ws-current-name))))
    (select-window window)
    (call-interactively search)))

(defun agent-repl-response-search-forward ()
  "Search the selected bubble's markdown FORWARD with Evil's `/'.
Composer command mode, while a bubble is selected: the bubble opens
read-only in the shared popup and the search runs there, so `n', `N',
`RET' and every read-only Evil motion and yank work on it as in any buffer."
  (interactive)
  (agent-repl--response-search #'evil-ex-search-forward))

(defun agent-repl--response-view-search-key ()
  "Run what `C-s' runs in the response view, the current buffer.
The view is an ordinary buffer, so `C-s' there is the user's own buffer
search.  An unbound `C-s' is a broken invariant (Emacs binds it globally),
so it signals rather than doing nothing."
  (interactive)
  (let ((command (key-binding (kbd "C-s"))))
    (unless (commandp command)
      (let ((ws (agent-repl--ws-current-name)))
        (agent-repl--error ws "elisp.input.response-view-search-key-unbound ws=%s binding=%S"
                           ws command)
        (error "agent-repl: `C-s' runs no command in the response view")))
    (call-interactively command)))

(defun agent-repl-response-search-buffer ()
  "Search the selected bubble's markdown with what `C-s' runs.
The bubble opens read-only in the shared popup, as for `/', and the
buffer search `C-s' runs anywhere else runs there."
  (interactive)
  (agent-repl--response-search #'agent-repl--response-view-search-key))

(defun agent-repl-response-search-backward ()
  "Search the selected bubble's markdown BACKWARD with Evil's `?'.
The mirror of `agent-repl-response-search-forward'."
  (interactive)
  (agent-repl--response-search #'evil-ex-search-backward))

(defun agent-repl--input-endpoints (ws op prefix detail-name detail)
  "Return WS's (REF . CONN) for the composer verb OP, or nil after saying why.
With no daemon-minted ref, or no connection, nothing can be sent: the miss
is logged at WARN as `elisp.input.OP-no-ref' / `-no-conn' carrying DETAIL
under DETAIL-NAME, and the composer flashes under PREFIX.  THE ONE GUARD
every composer verb sends behind."
  (let ((ref (agent-repl-host-ref ws))
        (conn (agent-repl-host-conn-or-primary ws))
        (shown (concat detail-name "=%S")))
    (cond
     ((null ref)
      (agent-repl--warn ws (concat "elisp.input." op "-no-ref ws=%s " shown) ws detail)
      (agent-repl--input-flash ws (format "%s: workspace not ready" prefix))
      nil)
     ((null conn)
      (agent-repl--warn ws (concat "elisp.input." op "-no-conn ws=%s " shown) ws detail)
      (agent-repl--input-flash ws (format "%s: no daemon connection" prefix))
      nil)
     (t (cons ref conn)))))

(defun agent-repl--input-report-refusal (ws slug cause flash warn-format &rest warn-args)
  "Report a composer verb's refusal CAUSE for WS and flash FLASH.
A handover arm is routed to host.el's handover walk under
`SLUG-handover-refusal' (logged at INFO there) instead of logged as a
refusal; any other is logged at WARN with WARN-FORMAT and WARN-ARGS.  The
flash stands either way, because the verb did not happen.  THE ONE
REFUSAL REPORT every composer verb shares."
  (unless (agent-repl-host-route-handover ws cause (concat slug "-handover-refusal"))
    (apply #'agent-repl--warn ws warn-format warn-args))
  (agent-repl--input-flash ws flash))

(cl-defun agent-repl--input-call (ws &key op prefix detail-name detail rpc request
                                      on-success on-refused)
  "Send the composer verb OP for WS through RPC and report how it went.
THE ONE SHAPE every asynchronous composer verb call takes, so each answers
alike:
- no daemon-minted ref, or no connection: nothing is sent
  (`agent-repl--input-endpoints');
- otherwise RPC is called with REQUEST (a function of the ref answering
  the request plist), logged at INFO;
- a success hands its value to ON-SUCCESS;
- a refusal is reported (`agent-repl--input-report-refusal') with the
  text ON-REFUSED answers for its cause, flashed under PREFIX;
- an answer arm the composer does not know is an ERROR;
- a transport failure is logged at WARN and flashed.
Every record is the operation `elisp.input.OP...' and carries DETAIL
under DETAIL-NAME, so a log line names what was asked."
  (when-let* ((endpoints (agent-repl--input-endpoints ws op prefix detail-name detail)))
    (let ((slug (concat "elisp.input." op))
          (shown (concat detail-name "=%S")))
      (agent-repl--info ws (concat slug " ws=%s " shown) ws detail)
      (funcall
       rpc (cdr endpoints) (funcall request (car endpoints))
       :on-response
       (lambda (response)
         (pcase (plist-get response :arm)
           (:success (funcall on-success (plist-get response :value)))
           (:error
            (let ((cause (plist-get (plist-get response :value) :cause)))
              (agent-repl--input-report-refusal
               ws slug cause (format "%s: %s" prefix (funcall on-refused cause))
               (concat slug "-refused ws=%s " shown " cause=%S") ws detail cause)))
           (other
            (agent-repl--error ws (concat slug "-unknown-arm ws=%s arm=%S") ws other))))
       :on-failure
       (lambda (failure)
         (agent-repl--warn ws (concat slug "-failure ws=%s " shown " detail=%S")
                           ws detail failure)
         (agent-repl--input-flash ws (format "%s: the daemon did not answer" prefix)))))))

(defun agent-repl--input-select-feed-row (ws move)
  "Send the `SelectFeedRow' MOVE for WS and report how it went.
MOVE is (:arm ARM :value V): ARM `:turn-boundary' with V
\(:direction `:older'/`:newer'), or `:clear' with V nil.  THE ONE SEND of
every composer selection move, so each answers alike (see
`agent-repl--input-call'): the outcome is the daemon's, and its push on
the host watch is what changes Emacs's state; a step that finds nothing of
its kind says so."
  (let ((arm (plist-get move :arm)))
    (agent-repl--input-call
     ws :op "select-feed-row"
     :prefix (cdr (assq arm agent-repl--input-selection-flash-prefixes))
     :detail-name "move" :detail move
     :rpc #'agent-repl-rpc-select-feed-row
     :request (lambda (ref) (list :workspace ref :move move))
     :on-success
     (lambda (value)
       (let ((outcome (plist-get value :outcome)))
         (agent-repl--info ws "elisp.input.select-feed-row-answered ws=%s move=%S outcome=%S"
                           ws move (plist-get outcome :arm))
         ;; A step that lands on NOTHING means there is no row of its kind:
         ;; say so rather than leave the gesture silent.
         (when (eq (plist-get outcome :arm) :nothing-selectable)
           (agent-repl--input-flash
            ws (cdr (assq arm agent-repl--input-selection-nothing-selectable-flashes))))))
     :on-refused (lambda (_cause) "selection refused"))))

(defun agent-repl--input-select-step (direction)
  "Step the current workspace's selection through the turn boundaries.
DIRECTION is `:older' or `:newer'."
  (agent-repl--input-select-feed-row
   (agent-repl--ws-current-name)
   (list :arm :turn-boundary :value (list :direction direction))))

(defun agent-repl-turn-boundary-select-prev ()
  "Select the PREVIOUS (older) prompt or final response, moving up the feed.
The walk covers the prompts a rollback can reach and the final responses
together, in feed order.  From no selection it starts at the newest; past
the oldest it wraps to the newest -- the daemon owns the order and
computes the row, Emacs sends the step."
  (interactive)
  (agent-repl--input-select-step :older))

(defun agent-repl-turn-boundary-select-next ()
  "Select the NEXT (newer) prompt or final response, moving down the feed.
From no selection it starts at the oldest; past the newest it ends the
selection and the feed returns to its tail, so the next step rolls round
to the oldest."
  (interactive)
  (agent-repl--input-select-step :newer))

;;;; ---- Rollback (`C-c C-RET' / `C-c M-RET') -----------------------------
;;
;; ROLLING THE CONVERSATION BACK to just before a prompt: the selected one,
;; or the latest (which cancels it).  The DAEMON plans it (PlanRollback):
;; Emacs shows the plan as a y/n question in the minibuffer, its side effects
;; in red, and on a yes hands the plan's opaque token back (RollBack).  On
;; success the composer's contents go to its history and the composer holds
;; the rolled-back prompt again.  `C-c C-RET' keeps the files, `C-c M-RET'
;; restores the ones the agent's edit tools changed.

(defconst agent-repl--input-rollback-refusals
  '((:unknown-workspace . "the daemon does not know this workspace")
    (:workspace-ref-mismatch . "the daemon holds this workspace under another directory")
    (:transferring-away . "the workspace is moving to another daemon; press the key again")
    (:not-yet-adopted . "the daemon is still taking this workspace over; press the key again")
    (:plan-stale . "the conversation changed; press the key again")
    (:no-session . "there is no running session to roll back")
    (:prompt-not-reachable . "that prompt can no longer be rolled back to; press the key again")
    (:prompt-not-recorded . "the conversation holds no record of that prompt")
    (:first-prompt . "the first prompt can't be rolled back; /clear starts over")
    (:unseen-prompt . "a later prompt the feed never showed would be dropped")
    (:files-not-restorable . "files can't be restored to that prompt"))
  "Each PlanRollback / RollBack / ListRollbackPrompts refusal arm, in plain words.")

(defun agent-repl--input-rollback-refusal-text (cause)
  "Return the composer flash for the refusal CAUSE, `(:arm ARM :value V)'.
A vendor refusal carries the vendor's own words, which are appended."
  (let* ((arm (plist-get cause :arm))
         (said (or (cdr (assq arm agent-repl--input-rollback-refusals))
                   (format "refused (%S)" arm)))
         (vendor (plist-get (plist-get cause :value) :vendor-message)))
    (if (and vendor (not (string-empty-p vendor)))
        (format "rollback: %s: %s" said vendor)
      (format "rollback: %s" said))))

(defun agent-repl--input-rollback-plural (n one many)
  "Return \"N ONE\" when N is 1, else \"N MANY\"."
  (format "%d %s" n (if (= n 1) one many)))

(defun agent-repl--input-rollback-side-effects (plan)
  "Return PLAN's side effects as sentences.
They come in the order the confirmation lists them."
  (let* ((files (plist-get plan :files))
         (cancel (and (eq (plist-get files :arm) :restored)
                      (plist-get (plist-get files :value) :cancel-detached)))
         (queued (plist-get plan :drop-queued)))
    (delq nil
          (list (when (plist-get plan :interrupt)
                  "The running turn will be interrupted.")
                (when queued
                  (format "%s will be dropped."
                          (agent-repl--input-rollback-plural
                           (plist-get queued :prompts) "queued prompt" "queued prompts")))
                (when cancel
                  (format "%s started since then will be stopped."
                          (agent-repl--input-rollback-plural
                           (plist-get cancel :items)
                           "background agent or shell" "background agents and shells")))))))

(defun agent-repl--input-rollback-confirmation (plan)
  "Return the y/n question that states PLAN, side effects in the `error' face."
  (let* ((target (plist-get plan :target))
         (dropped (plist-get target :prompts-dropped))
         (head (format "%s \"%s\"%s."
                       (pcase (plist-get target :chosen)
                         (:selected "Roll back to before the selected prompt")
                         (:named "Roll back to before the chosen prompt")
                         (_ "Cancel the latest prompt"))
                       (plist-get target :excerpt)
                       (if (> dropped 1)
                           (format " (%d prompts are dropped)" dropped)
                         "")))
         (files (if (eq (plist-get (plist-get plan :files) :arm) :restored)
                    "Files the agent's edit tools changed are restored; shell changes and git commits are not (C-c C-RET leaves files alone)."
                  "Files are not restored (C-c M-RET restores them)."))
         (effects (mapcar (lambda (sentence) (propertize sentence 'face 'error))
                          (agent-repl--input-rollback-side-effects plan))))
    (concat (mapconcat #'identity (append (list head files) effects) " ")
            " Roll back? ")))

(defun agent-repl--input-rollback-refused (ws verb cause)
  "Log and flash WS's VERB (`plan', `roll-back' or `list') refusal CAUSE.
A handover arm is routed to host.el's handover walk (logged at INFO there)
instead of logged as a refusal; the flash stands either way, because the
rollback did not happen and the key must be pressed again."
  (agent-repl--input-report-refusal
   ws (format "elisp.input.rollback-%s" verb) cause
   (agent-repl--input-rollback-refusal-text cause)
   "elisp.input.rollback-refused ws=%s verb=%s cause=%S" ws verb cause))

(defun agent-repl--input-rollback-failed (ws verb detail)
  "Log and flash WS's VERB transport failure DETAIL."
  (agent-repl--warn ws "elisp.input.rollback-failure ws=%s verb=%s detail=%S" ws verb detail)
  (agent-repl--input-flash ws "rollback: the daemon did not answer"))

(defun agent-repl--input-rollback-done (ws success)
  "Refill WS's composer from the RollBack SUCCESS and say it is done.
The composer's contents go to its history first, as a discard saves them;
then it holds the rolled-back prompt's words and path attachments.  An
image by URL (or a block the composer cannot hold) is dropped: logged at
WARN and flashed, never silently lost."
  (let* ((said (plist-get success :prompt))
         (images (agent-repl--input-said-attachments said))
         (dropped (plist-get images :dropped))
         (restored (plist-get success :files-restored))
         (buf (agent-repl--input-buffer ws)))
    (if (null buf)
        (agent-repl--warn ws "elisp.input.rollback-no-composer ws=%s" ws)
      (with-current-buffer buf
        (agent-repl--input-save-to-history ws))
      (agent-repl--input-fill-said ws buf said (plist-get images :attachments)))
    (when (> dropped 0)
      (agent-repl--warn ws "elisp.input.rollback-attachments-dropped ws=%s count=%d" ws dropped)
      (agent-repl--input-flash
       ws (format "rollback: %s could not be attached and %s dropped"
                  (agent-repl--input-rollback-plural dropped "image" "images")
                  (if (= dropped 1) "was" "were"))))
    (agent-repl--info ws "elisp.input.rollback-done ws=%s attachments=%d dropped=%d files-restored=%S"
                      ws (length (plist-get images :attachments)) dropped
                      (and restored (plist-get restored :files)))
    (message "%s" (if restored
                      (format "rollback: done, %s restored"
                              (agent-repl--input-rollback-plural
                               (plist-get restored :files) "file" "files"))
                    "rollback: done"))))

(defun agent-repl--input-rollback-perform (ws conn ref token)
  "Send RollBack for WS on CONN with the plan TOKEN, echoed verbatim."
  (agent-repl-rpc-roll-back
   conn (list :workspace ref :token token)
   :on-response
   (lambda (response)
     (pcase (plist-get response :arm)
       (:success (agent-repl--input-rollback-done ws (plist-get response :value)))
       (:error (agent-repl--input-rollback-refused
                ws "roll-back" (plist-get (plist-get response :value) :cause)))
       (other (agent-repl--error ws "elisp.input.rollback-unknown-arm ws=%s verb=roll-back arm=%S"
                                 ws other))))
   :on-failure
   (lambda (detail) (agent-repl--input-rollback-failed ws "roll-back" detail))))

(defun agent-repl--input-rollback-endpoints (ws context)
  "Return WS's (REF . CONN) for a rollback rpc, or nil after saying why not.
CONTEXT is the request's own fields, logged when the workspace is not
ready or no daemon connection stands; the composer flashes either way.
The one guard every rollback rpc sends behind (`agent-repl--input-endpoints')."
  (agent-repl--input-endpoints ws "rollback" "rollback" "context" context))

(defun agent-repl--input-rollback (ws files &optional prompt)
  "Plan a rollback of WS's conversation, confirm it, and perform it.
FILES is `:keep-files' or `:restore-files'.  PROMPT, a FeedId plist as
ListRollbackPrompts served it, names the prompt to roll back to; nil
plans the selected prompt, else the latest.  The plan is asked for
synchronously, so the confirmation is read in the command loop of the key
that asked for it."
  (when-let* ((endpoints (agent-repl--input-rollback-endpoints
                          ws (list :files files :prompt prompt))))
    (let ((ref (car endpoints))
          (conn (cdr endpoints)))
      (agent-repl--info ws "elisp.input.rollback-plan ws=%s files=%S prompt=%S"
                        ws files (plist-get prompt :value))
      (let ((response
             (condition-case err
                 (agent-repl-rpc-plan-rollback-sync
                  conn (append (list :workspace ref :files (list :arm files :value nil))
                               (when prompt (list :prompt prompt))))
               (agent-repl-connect-error
                (agent-repl--input-rollback-failed ws "plan" (cdr err))
                nil))))
        (when response
          (pcase (plist-get response :arm)
            (:success
             (let ((outcome (plist-get (plist-get response :value) :outcome)))
               (pcase (plist-get outcome :arm)
                 (:nothing-to-roll-back
                  (agent-repl--info ws "elisp.input.rollback-nothing ws=%s" ws)
                  (agent-repl--input-flash ws "rollback: no prompt to roll back to"))
                 (:plan
                  (let ((plan (plist-get outcome :value)))
                    (if (y-or-n-p (agent-repl--input-rollback-confirmation plan))
                        (progn
                          (agent-repl--info ws "elisp.input.rollback-confirmed ws=%s" ws)
                          (agent-repl--input-rollback-perform ws conn ref (plist-get plan :token)))
                      (agent-repl--info ws "elisp.input.rollback-declined ws=%s" ws)
                      (message "rollback: cancelled"))))
                 (other
                  (agent-repl--error ws "elisp.input.rollback-unknown-outcome ws=%s outcome=%S"
                                     ws other)))))
            (:error (agent-repl--input-rollback-refused
                     ws "plan" (plist-get (plist-get response :value) :cause)))
            (other (agent-repl--error ws "elisp.input.rollback-unknown-arm ws=%s verb=plan arm=%S"
                                      ws other))))))))

;;;; ---- The stop key's second press (`C-c C-k' with nothing running) -----
;;
;; A turn stop that finds nothing running is the stop key pressed AGAIN: the
;; daemon's `nothing_running' answer carries what rolling the latest prompt
;; back offers.  With nothing after the prompt in the feed, the prompt is
;; rolled back AT ONCE, with no confirmation: nothing a reader has seen is
;; dropped, and the daemon minted the plan's token itself, refusing it as
;; stale if a bubble lands first.  With a response bubble or a card after
;; it, the reader picks which prompt to roll back to from a completion list,
;; newest first, and that rollback is confirmed as any other.

(defun agent-repl--input-rollback-offer (ws offer)
  "Act on the stop key's rollback OFFER for WS, `(:arm ARM :value V)'.
ARM `:immediate' rolls the latest prompt back with V's token, at once;
`:choose' opens the picker."
  (pcase (plist-get offer :arm)
    (:immediate
     (when-let* ((endpoints (agent-repl--input-rollback-endpoints ws (list :offer :immediate))))
       (agent-repl--info ws "elisp.input.rollback-immediate ws=%s" ws)
       (agent-repl--input-rollback-perform
        ws (cdr endpoints) (car endpoints) (plist-get (plist-get offer :value) :token))))
    (:choose
     (agent-repl--info ws "elisp.input.rollback-choose ws=%s" ws)
     (agent-repl--input-rollback-choose ws))
    (other
     (agent-repl--error ws "elisp.input.rollback-unknown-offer ws=%s offer=%S" ws other))))

(defun agent-repl--input-rollback-tokens-text (tokens)
  "Return TOKENS, a context size, as the picker's short column text.
nil (no figure stands) is a dash, never a zero."
  (cond
   ((null tokens) "-")
   ((< tokens 1000) (format "%d" tokens))
   ((< tokens 1000000) (format "%dk" (round tokens 1000.0)))
   (t (format "%.1fM" (/ tokens 1000000.0)))))

(defun agent-repl--input-rollback-candidate (index entry now)
  "Return the picker line for ENTRY, the INDEXth prompt (1 is the newest).
Columns: the index (keeping lines distinct), time since the prompt was
sent (as of NOW, a float time), the context size before it, and its
opening words."
  (let ((sent (plist-get entry :sent-at-ms)))
    (format "%2d  %-12s  %6s  %s"
            index
            (or (and sent (agent-repl--format-summary-relative-time (/ sent 1000.0) now)) "-")
            (agent-repl--input-rollback-tokens-text (plist-get entry :context-tokens))
            (plist-get entry :excerpt))))

(defun agent-repl--input-rollback-read-entry (entries)
  "Read one of ENTRIES (newest first) with `completing-read'; return it.
The list keeps the daemon's order, newest at the top, whatever sorting the
completion backend would otherwise apply."
  (let* ((now (float-time))
         (index 0)
         (lines (mapcar (lambda (entry)
                          (cons (agent-repl--input-rollback-candidate (cl-incf index) entry now)
                                entry))
                        entries))
         (table (lambda (string predicate action)
                  (if (eq action 'metadata)
                      '(metadata (display-sort-function . identity)
                                 (cycle-sort-function . identity))
                    (complete-with-action action (mapcar #'car lines) string predicate))))
         (choice (completing-read "Roll back to before (sent, context): " table nil t)))
    (cdr (assoc choice lines))))

(defun agent-repl--input-rollback-choose (ws)
  "List the prompts a rollback of WS can reach, read one, and roll back to it.
The list is asked for synchronously, so the pick is read in the command
loop of the key that asked for it; the rollback is then planned by naming
the pick and confirmed as any other."
  (when-let* ((endpoints (agent-repl--input-rollback-endpoints ws (list :offer :choose))))
    (let ((response
           (condition-case err
               (agent-repl-rpc-list-rollback-prompts-sync (cdr endpoints) (list :workspace (car endpoints)))
             (agent-repl-connect-error
              (agent-repl--input-rollback-failed ws "list" (cdr err))
              nil))))
      (when response
        (pcase (plist-get response :arm)
          (:success
           (let ((entries (plist-get (plist-get response :value) :prompts)))
             (agent-repl--info ws "elisp.input.rollback-listed ws=%s prompts=%d" ws (length entries))
             (if (null entries)
                 (agent-repl--input-flash ws "rollback: no prompt to roll back to")
               (let ((entry (agent-repl--input-rollback-read-entry entries)))
                 (agent-repl--info ws "elisp.input.rollback-chosen ws=%s prompt=%S"
                                   ws (plist-get (plist-get entry :prompt) :value))
                 (agent-repl--input-rollback ws :keep-files (plist-get entry :prompt))))))
          (:error (agent-repl--input-rollback-refused
                   ws "list" (plist-get (plist-get response :value) :cause)))
          (other (agent-repl--error ws "elisp.input.rollback-unknown-arm ws=%s verb=list arm=%S"
                                    ws other)))))))

(defun agent-repl-rollback-keep-files ()
  "Roll the conversation back to before the selected prompt, keeping files.
With no prompt selected, cancel the latest prompt.  Asks first."
  (interactive)
  (agent-repl--input-rollback (agent-repl--ws-current-name) :keep-files))

(defun agent-repl-rollback-restore-files ()
  "Roll the conversation back to before the selected prompt, restoring files.
Only files the agent's edit tools changed are restored.  With no prompt
selected, cancel the latest prompt.  Asks first."
  (interactive)
  (agent-repl--input-rollback (agent-repl--ws-current-name) :restore-files))

;; FEED TEXT ZOOM (`C-+' or `C-=' / `C--').  The daemon owns and persists a single
;; global feed text scale; Emacs only sends a DIRECTION per keypress and the
;; daemon applies a fixed small step, clamps it, and pushes the result to the
;; webapp.  These commands NEVER touch Emacs's own font or `text-scale' — they
;; only nudge the webapp feed — which is why they replace Doom's global
;; `C-+'/`C--' text-scale bindings in the composer rather than wrapping them.
;; They are PLAIN commands with no debounce: holding the key down lets Emacs
;; key auto-repeat re-invoke the command, which is exactly the continuous
;; fine-adjustment the feature wants.

(defun agent-repl--feed-text-scale-adjust (direction)
  "Send an AdjustFeedTextScale nudge in DIRECTION (`:increase' or `:decrease').
The feed text zoom is daemon-global, so the request carries no workspace
ref; the current workspace only supplies the daemon connection and the
log scope.  A missing connection is surfaced through the log rather than
silently dropped."
  (let* ((ws (agent-repl--ws-current-name))
         (conn (agent-repl-host-conn-or-primary ws)))
    (if (null conn)
        (agent-repl--warn ws "elisp.input.feed-text-scale-no-conn dir=%S" direction)
      (agent-repl--info ws "elisp.input.feed-text-scale dir=%S" direction)
      (agent-repl-rpc-adjust-feed-text-scale
       conn (list :direction direction)
       :on-response
       (lambda (response)
         (agent-repl--info ws "elisp.input.feed-text-scaled dir=%S scale=%s"
                           direction (plist-get response :scale)))
       :on-failure
       (lambda (detail)
         (agent-repl--warn ws "elisp.input.feed-text-scale-failure dir=%S detail=%S"
                           direction detail))))))

(defun agent-repl-feed-text-scale-increase ()
  "Zoom the webapp FEED text IN one small step (daemon-owned, persisted).
Bound to `C-+' and `C-=' in the composer, overriding Doom's text-scale
bindings there.  It NEVER changes Emacs's own font — only the webapp feed
text — and holding the key auto-repeats the nudge for fine adjustment."
  (interactive)
  (agent-repl--feed-text-scale-adjust :increase))

(defun agent-repl-feed-text-scale-decrease ()
  "Zoom the webapp FEED text OUT one small step (daemon-owned, persisted).
Bound to `C--' in the composer, overriding Doom's global text-scale
binding there.  It NEVER changes Emacs's own font — only the webapp feed
text — and holding the key auto-repeats the nudge for fine adjustment."
  (interactive)
  (agent-repl--feed-text-scale-adjust :decrease))

(defun agent-repl--input-dismiss-digest (ws)
  "Ask WS's page to close its standing news digest, as escape there would.
ESCAPE IN THE COMPOSER CLOSES THE DIGEST TOO (owner request, 2026-10-03):
the page closes it on its own escape, but the composer usually holds the
keyboard, so the key never reaches the page.  SO DOES SENDING A PROMPT
(owner request, 2026-10-06; `agent-repl--input-submit').  Fire-and-forget
through the page's own close control (webhost.el); a page with no digest
does nothing, and records that it had none."
  (when ws
    (agent-repl-webhost-dismiss-digest ws)))

(defun agent-repl-input-insert-escape ()
  "Insert-state escape in the composer: close a standing news digest, then
leave insert state exactly as escape always has."
  (interactive)
  (agent-repl--input-dismiss-digest (agent-repl--ws-current-name))
  (evil-normal-state))

(defun agent-repl--input-escape-default ()
  "Run escape's ORDINARY meaning in the composer.
Called only when NO feed selection is active, so this
command never STEALS escape when there is nothing to clear.  `doom/escape'
is the full Doom escape (it runs `doom-escape-hook', which is where evil's
force-normal-state lives, and falls back to `keyboard-quit'); it is used
when present and `keyboard-quit' otherwise."
  (if (fboundp 'doom/escape)
      (call-interactively 'doom/escape)
    (keyboard-quit)))

(defun agent-repl--input-selection-clear (ws)
  "Send `SelectFeedRow' CLEAR for WS.
The daemon ends the selection and pushes it: the webapp returns to the
feed bottom and the host watch tells Emacs nothing is selected."
  (agent-repl--info ws "elisp.input.selection-clear ws=%s" ws)
  (agent-repl--input-select-feed-row ws (list :arm :clear :value nil)))

(defun agent-repl-input-selection-escape ()
  "Command-mode escape for the feed selection.
WITH NO SELECTION active, escape keeps its ordinary meaning
(`agent-repl--input-escape-default') -- this command never breaks escape
when there is nothing to clear.  WITH a selection active it takes TWO
CONSECUTIVE escapes to clear:

  1st escape  -> a minibuffer WARNING that another escape will clear it.
  2nd escape  -> send CLEAR.

CONSECUTIVE is read off `last-command': after this command runs, the
command loop sets `last-command' to it, so a second escape with no other
key in between sees `last-command' equal to this command and clears; ANY
other command-mode key in between runs as a different `last-command' and
re-arms the warning instead.  That is the whole two-consecutive-escapes
state machine -- there is no separate counter to fall out of sync."
  (interactive)
  (let ((ws (agent-repl--ws-current-name)))
    (agent-repl--input-dismiss-digest ws)
    (if (not (agent-repl--input-selection-active-p ws))
        (agent-repl--input-escape-default)
      (if (eq last-command 'agent-repl-input-selection-escape)
          (agent-repl--input-selection-clear ws)
        ;; A USER'S ACT, NOT A FAULT: the first escape over a selection is
        ;; the armed half of a deliberate two-key clear, so its record is
        ;; INFO (once per keypress, and it must survive the info threshold).
        (agent-repl--info ws "elisp.input.selection-escape-armed ws=%s" ws)
        (message "%s" agent-repl--input-selection-escape-warning)))))

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
from the DRAWN row while the agent still receives it verbatim.  The
agent API finds the asking workspace from the agent's working directory,
so a prompt carries no workspace tag of its own."
  (let ((prepend-p (agent-repl--should-prepend-metaprompt-p raw force-metaprompt ws)))
    (agent-repl--log ws "elisp.input.prepare raw-len=%d force=%s prepend=%s"
                     (length raw) force-metaprompt prepend-p)
    (if prepend-p
        (concat (agent-repl--meta-wrap (agent-repl--command-prefix-for ws)) "\n\n" raw)
      raw)))

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
the gate.  Every other arm sends -- `:open', `:no-session', `:terminal'
and `:unknown' plainly, because
SubmitPrompt has no precondition and the daemon starts or revives the
session implicitly -- EXCEPT when that session is parked at its COLD
GATE.  A cold gate is a precondition SubmitPrompt does not satisfy: the
shim is up and serving, and it takes no prompt until the user answers the
gate in the panel (clear / compact / resume).  The daemon refuses such a
submission with `SubmitPromptError''s own `cold_gate' arm, which
`agent-repl--input-on-error' draws; the gate still SENDS rather than
guessing, because the daemon is the authority on that standing and
answers by name.")

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

(defun agent-repl--input-optimistic-clear (ws raw)
  "Clear WS's composer and record RAW the instant a from-buffer prompt is sent.
Owner ruling: when the user submits from the composer (hits return), Emacs
ALWAYS clears the input window and records the prompt IMMEDIATELY, whether
or not the daemon ever acks.  So this runs AT DISPATCH, before the wire
call, never in an ack callback: a missing, slow, failed or erroring ack no
longer leaves the composer full.

Nothing is lost by clearing early.  RAW is pushed onto the input history
ring here, so the user can recall it with history-prev even on a refusal
that is not queued.  And a transport failure writes the full said to the
durable held-prompt ingress (see `agent-repl--input-on-failure'), which is
independent of the composer.  Between the ring and the ingress, the erased
draft is always recoverable.

ATTACHMENTS are cleared too: the submission carried them, so leaving them
would re-send them on the next prompt.  The ring is persisted once here.

RETURNS A SNAPSHOT of everything this erased -- the composer's text and
point, its attachments, and the history ring and browse index as they
stood BEFORE the push, with the unsent-discard marker the push may clear.
`agent-repl--input-restore' puts it all back when
the daemon REFUSES the submission, so a refused prompt is never in
history and never costs the user their words.  (Regression watch: the
optimistic clear landed without a restore arm, and six composer
integration scenarios read an empty composer after a refusal.)"
  (let ((buf (agent-repl--input-buffer ws))
        (snapshot nil))
    (when buf
      (with-current-buffer buf
        (setq snapshot (list :buffer buf
                             :text (buffer-string)
                             :point (point)
                             :attachments agent-repl-input-attachments
                             :history agent-repl--input-history
                             :history-index agent-repl--history-index
                             :discard-entry agent-repl--history-discard-entry))
        (agent-repl--history-push raw)
        (agent-repl--history-reset)
        (erase-buffer)))
    (agent-repl--history-save ws)
    (agent-repl-input-clear-attachments ws)
    (agent-repl--log ws "elisp.input.optimistic-clear ws=%s raw-len=%d buffer=%s"
                     ws (length raw) (and buf t))
    snapshot))

(defun agent-repl--input-restore (ws snapshot)
  "Put WS's composer back exactly as SNAPSHOT found it, after a REFUSAL.
A refusal is the daemon saying the prompt did NOT land, so every trace of
the optimistic clear is undone: the text and point return, the
attachments return, and the history ring and browse index go back to
their pre-push values -- a refused prompt is never in history.  The ring
is re-persisted so the un-push survives a restart.

SNAPSHOT is nil for any submission that was never taken from the composer
(a canned command, a metaprompt read, a queue re-drive), and then this is
a no-op: those left the user's own draft alone and there is nothing to
restore.  A composer buffer that has since been killed is likewise
nothing to restore into."
  (let ((buf (plist-get snapshot :buffer)))
    (cond
     ((null snapshot)
      (agent-repl--log-verbose ws "elisp.input.restore-skipped ws=%s reason=no-snapshot" ws))
     ((not (buffer-live-p buf))
      (agent-repl--log ws "elisp.input.restore-skipped ws=%s reason=buffer-dead" ws))
     (t
      (with-current-buffer buf
        (let ((agent-repl--history-navigating t)
              (text (plist-get snapshot :text)))
          (erase-buffer)
          (insert text)
          (goto-char (min (plist-get snapshot :point) (point-max))))
        (setq agent-repl-input-attachments (plist-get snapshot :attachments))
        (setq agent-repl--input-history (plist-get snapshot :history))
        (setq agent-repl--history-index (plist-get snapshot :history-index))
        (setq agent-repl--history-discard-entry (plist-get snapshot :discard-entry)))
      (agent-repl--history-save ws)
      (agent-repl--log ws "elisp.input.restored ws=%s text-len=%d attachments=%d"
                       ws (length (plist-get snapshot :text))
                       (length (plist-get snapshot :attachments)))))))

(defun agent-repl--input-accepted (ws raw arm &optional from-buffer)
  "Finish an ACCEPTED submission for WS: keep the record, clear what was sent.
ARM names which accepted arm answered, for the log.  Applies to every
success arm: a minted turn, a resolved command panel, a
recognized-but-unsupported command and a session-acting command the
daemon acted on are all answers, and none of them leaves the sent text
owed a resend.

FROM-BUFFER says the submitted text WAS the composer\='s contents.  When
it is, THIS FUNCTION DOES NOT touch the composer, the history ring, or the
attachments: `agent-repl--input-optimistic-clear' already erased the
composer, pushed RAW to history, reset the position, saved the ring and
cleared the attachments at dispatch time (owner ruling: a from-buffer
submit clears and records on return regardless of the ack).  Re-doing any
of it here would double-push history or error on the already-empty
composer, so the accept path for a from-buffer submission is purely a log.

For a NON-from-buffer accept -- a canned command send
(`agent-repl-update-pr\=' and every other explicit-TEXT site), a
metaprompt read, a queue re-drive -- the composer is left alone: those
compose their own words and erasing an unrelated half-written draft the
user never submitted would destroy work the daemon was never told about
(ruling on audit-3 #51, which protected an UNRELATED draft; it never
reached the from-buffer path, which now clears optimistically instead).
RAW is still pushed onto the ring -- a canned prompt WAS sent -- but the
history POSITION is left where it was, because a surviving draft keeps its
navigation state.  ATTACHMENTS are cleared, since the submission carried
them."
  (if from-buffer
      (agent-repl--log ws "elisp.input.accepted ws=%s arm=%S raw-len=%d from-buffer=t (optimistic clear already applied at dispatch)"
                       ws arm (length raw))
    (let ((buf (agent-repl--input-buffer ws)))
      (when buf
        (with-current-buffer buf
          (agent-repl--history-push raw)))
      (agent-repl--history-save ws)
      (agent-repl-input-clear-attachments ws)
      (agent-repl--log ws "elisp.input.accepted ws=%s arm=%S raw-len=%d buffer=%s from-buffer=nil"
                       ws arm (length raw) (and buf t)))))

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
    (:ask-answered
     ;; The send answered the ask standing on the feed: no turn was
     ;; minted, so nothing is awaited and no send posthook runs.  The
     ;; card's re-push in the webapp is the visible effect.
     (agent-repl--info ws "elisp.input.ask-answered ws=%s origin=%S raw-len=%d"
                       ws origin (length raw))
     (agent-repl--input-accepted ws raw :ask-answered from-buffer))
    (arm
     (agent-repl--error ws "elisp.input.unknown-success-arm ws=%s arm=%S" ws arm))))

(defconst agent-repl--input-handover-arms '(:transferring-away :not-yet-adopted)
  "Refusal arms that are HANDOVER SIGNALS rather than user-facing failures.
The same two arms verbs.el routes, for the same reason: the fanout makes
the handover ordering enforced BY REFUSAL on EVERY per-workspace rpc, and
SubmitPrompt is one.  A lagging client self-heals from the refusal, so
telling the user their submission was refused would be reporting a fault
during the one rollout that is supposed to be invisible.")

(defun agent-repl--input-on-handover-refusal (ws said origin raw arm key &optional delivery)
  "Route ARM, a handover refusal of WS's submission, and HOLD the prompt.
Two acts, both required.  host.el walks the handover -- the redial, the
adopt, the webview -- because the refusal is the same fact its own pushes
carry.  The prompt itself is written to the durable held-prompt ingress
under THIS attempt\='s KEY: it was refused, not consumed, so re-driving
it is a RETRY of this submission and must go back out under the same key
for the daemon to recognize a duplicate.  The daemon that owns the
workspace once the handover ends ingests it.

The composer KEEPS its text: nothing here says the prompt landed, and the
user is left able to see exactly what they wrote.  DELIVERY is the
attempt's own `SubmitPromptDelivery' keyword (nil for the ordinary one),
held with it so the re-drive asks for the same delivery."
  (agent-repl--info ws "elisp.input.handover-refusal ws=%s origin=%S arm=%S key=%s delivery=%S"
                    ws origin (plist-get arm :arm) key delivery)
  (agent-repl-host-handle-refusal ws arm)
  (agent-repl--input-hold ws said origin raw key delivery))

(defconst agent-repl--input-deferred-held-arms '(:merging :cold-gate :no-session)
  "Refusal arms a DEFERRED submission is HELD through rather than refused by.
Each is a standing condition the daemon answers \='not now\=' for -- a merge
in flight, a cold gate the user answers in the panel, a session the daemon
is bringing up -- and a deferral already asked for \='later\='.  So the
prompt goes to the durable held-prompt ingress under its own key and
delivery, whose retry holds it until the condition clears, instead of
being drawn as a refusal the user must resubmit (owner ruling, 2026-09-28:
a cold gate and a merge still hold deferred prompts).")

(defun agent-repl--input-on-deferred-refusal (ws said origin raw arm key)
  "Hold WS's DEFERRED submission, which the daemon answered ARM, on disk.
ARM is one of `agent-repl--input-deferred-held-arms'.  It is an ANSWER
about a standing condition, not a fault, so it is recorded at INFO; the
prompt is written to the held-prompt ingress under KEY with its deferred
delivery, and the daemon's ingress re-drives it until the condition
clears."
  (agent-repl--info ws "elisp.input.deferred-held ws=%s origin=%S arm=%S key=%s"
                    ws origin arm key)
  (when (agent-repl--input-hold ws said origin raw key :deferred)
    (agent-repl--input-flash ws "deferred -- held until the daemon can take it")))

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

(defconst agent-repl--input-cold-gate-flash
  "cold gate: answer it in the panel (clear / compact / resume)"
  "The composer flash for `SubmitPromptError''s `cold_gate' arm.
A SESSION EXISTS -- the shim is up, which is exactly why the gate could
be raised -- and it takes no prompt until the gate is answered, which
happens in the panel and nowhere else.  So the sentence names the place
the answer is given rather than reporting a broken session.")

(defconst agent-repl--input-ask-not-standing-flash
  "the ask was already answered; nothing was sent"
  "The composer flash for `SubmitPromptError''s `ask_not_standing' arm.
The send was a standing ask's typed answer and the ask settled before it
arrived, so it reached nobody and the text stays in the composer.")

(defconst agent-repl--input-no-session-flash
  "no session; the daemon is starting it"
  "The composer flash for `SubmitPromptError''s `no_session' arm.
A workspace with NO SESSION AT ALL, which is the daemon's own to bring
up.  Nothing is owed by the user and nothing is owed by this composer, so
the sentence states the fact and the prompt is neither held nor lost.")

(defun agent-repl--input-say-refusal (ws flash &optional detail)
  "Tell the user about a refused submission of WS: FLASH in the composer,
and FLASH in the echo area followed by \=` (DETAIL)\=' when DETAIL is a
non-empty string.  DETAIL is the daemon's own account, echoed verbatim and
never switched on.  The one way a refusal arm with a fixed sentence says
itself, so no arm words its echo differently from its flash."
  (agent-repl--input-flash ws flash)
  (message "agent-repl: %s%s" flash
           (if (or (null detail) (string-empty-p detail)) "" (format " (%s)" detail))))

(defun agent-repl--input-on-error (ws said origin raw error key &optional snapshot delivery)
  "Handle a `SubmitPromptError' for WS.
EVERY arm here is the daemon saying the prompt did NOT land, so the FIRST
thing this does is hand SNAPSHOT to `agent-repl--input-restore\=': a
from-buffer submission gets its text, its point, its attachments and its
history ring back exactly as they were before the optimistic clear, and
the refused prompt is never left in history.  SNAPSHOT is nil for a
canned/metaprompt/re-drive send, which cleared nothing to begin with.
Wherever a docstring below says \"the text stays where it is\" it means
this arm adds no further erase: the restore above has already put the
composer back.
ERROR is the decoded error message.  Its `merging' arm means the prompt
arrived after a merge began and would be orphaned, so the user resubmits
once the merge resolves.  Its two HANDOVER arms are not failures at all
and are routed to host.el.  Its `duplicate_submission' arm says the key
was already accepted, so the earlier submission stands and nothing is
resent.  Its `bubble_refused' arm is the SHIM's refusal of a
bubble-addressed prompt, relayed by kind and drawn to the user without
being queued.  Its `cold_gate' arm says a session EXISTS and is parked on
the user's remediation choice, and its `no_session' arm says there is no
session at all; both are answered elsewhere -- the panel and the daemon's
own bring-up -- so both are drawn and neither is queued.  Every other arm
is a refusal this composer
has no treatment for: it is recorded at ERROR naming the arm and drawn to
the user, and the text stays where it is.

SAID, RAW and KEY are the submission\='s own, carried so a handover
refusal can re-drive the very prompt that was refused.  DELIVERY is its
`SubmitPromptDelivery' keyword: a DEFERRED submission is HELD through the
arms in `agent-repl--input-deferred-held-arms' rather than refused by them."
  (agent-repl--input-restore ws snapshot)
  (let* ((reason (plist-get error :reason))
         (arm (plist-get reason :arm)))
    (if (and (eq delivery :deferred) (memq arm agent-repl--input-deferred-held-arms))
        (agent-repl--input-on-deferred-refusal ws said origin raw arm key)
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
        (:cold-gate
         ;; The session is PARKED AT ITS COLD GATE.  It exists and is
         ;; serving; it simply takes no prompt until the user answers the
         ;; gate in the panel, so this is an ANSWER about a standing the
         ;; user resolves, not a fault and not an outage: it is recorded at
         ;; WARN, the prompt is NOT queued (a re-drive would meet the same
         ;; gate), and the text stays where it is.  The gate's own `detail'
         ;; -- the same sentence the gate card and the footer carry -- is
         ;; echoed verbatim; it is never switched on.
         (let ((detail (or (plist-get (plist-get reason :value) :detail) "")))
           (agent-repl--warn ws "elisp.input.refused-cold-gate ws=%s origin=%S arm=%S detail=%s"
                             ws origin arm detail)
           (agent-repl--input-say-refusal ws agent-repl--input-cold-gate-flash detail)))
        ((or :model-not-in-catalog :model-refused)
         ;; A `/model' ACT WAS REFUSED: the model is not in this session's
         ;; catalog, or the vendor refused the change.  The act was not
         ;; applied and the session runs on as before.  This is an ANSWER,
         ;; never an outage -- before the arm existed the refusal left the
         ;; daemon as a bare 400 and this composer HELD the prompt as if the
         ;; daemon were unreachable, so nothing ran and nothing said why.
         ;; Recorded at INFO (a refusal the user asked for, not a defect),
         ;; not queued (a re-drive meets the same refusal), and the refusal's
         ;; own sentence is shown verbatim.
         (let ((detail (or (plist-get (plist-get reason :value) :detail) "")))
           (agent-repl--info ws "elisp.input.refused-model ws=%s origin=%S arm=%S detail=%s"
                             ws origin arm detail)
           (agent-repl--input-flash ws "model change refused")
           (message "agent-repl: model change refused%s"
                    (if (string-empty-p detail) "" (format " -- %s" detail)))))
        (:ask-not-standing
         ;; The send was routed as a standing ask's TYPED ANSWER and the ask
         ;; settled first.  The text was meant as that answer, so it was not
         ;; delivered as a prompt instead; it is an answer about a standing
         ;; that moved on, recorded at INFO, not queued (a re-drive would
         ;; reach the session as a prompt the user never meant), and the
         ;; text stays so the user decides what to do with it.
         (agent-repl--info ws "elisp.input.refused-ask-not-standing ws=%s origin=%S arm=%S" ws origin arm)
         (agent-repl--input-say-refusal ws agent-repl--input-ask-not-standing-flash))
        (:no-session
         ;; The workspace has NO SESSION AT ALL, which is the daemon's own
         ;; to bring up.  Like the cold gate this is an answer rather than a
         ;; transport failure, so it is recorded at WARN and the prompt is
         ;; not queued; the text stays so the user can resubmit once the
         ;; session is up.
         (agent-repl--warn ws "elisp.input.refused-no-session ws=%s origin=%S arm=%S" ws origin arm)
         (agent-repl--input-say-refusal ws agent-repl--input-no-session-flash))
        (:bubble-refused
         ;; The SHIM refused a bubble-addressed prompt and the daemon relayed
         ;; the refusal BY KIND.  Both kinds are the same answer to the user
         ;; ("not this agent, not now"), so both are drawn alike and neither
         ;; is queued: a re-drive would meet the same refusal.  The shim's
         ;; `detail' is echoed verbatim for the human and for the log; it is
         ;; never switched on.
         (agent-repl--input-on-bubble-refusal ws origin (plist-get reason :value)))
        ((pred (lambda (a) (memq a agent-repl--input-handover-arms)))
         (agent-repl--input-on-handover-refusal ws said origin raw reason key delivery))
        (_
         (agent-repl--error ws "elisp.input.unknown-error-arm ws=%s arm=%S" ws arm)
         (message "agent-repl: submission refused (%S)" arm))))))

(defun agent-repl--input-on-failure (ws said origin raw detail key &optional delivery)
  "Handle a TRANSPORT failure for WS's submission.
The daemon never answered, so nothing is known about whether the prompt
landed.  The full SAID is written to the durable held-prompt ingress
(`agent-repl--input-hold'), which the daemon ingests once it serves --
the entry is independent of the composer, so a from-buffer submission
that was optimistically cleared at dispatch loses nothing: its words ride
the ingress AND sit in the history ring.  This path adds no erase and no
restore of the composer.

KEY is THIS attempt\='s idempotency key and rides into the entry: the
re-drive is a RETRY of this submission, not a second turn, so the daemon
answers an attempt that did land as a duplicate and delivers it once.
DELIVERY rides with it, so a deferred prompt is re-driven deferred."
  (agent-repl--error ws "elisp.input.transport-failure ws=%s origin=%S key=%s detail=%S"
                     ws origin key detail)
  (when (agent-repl--input-hold ws said origin raw key delivery)
    (agent-repl--input-flash ws "send failed -- held until the daemon is back")
    (message "agent-repl: the daemon did not answer; the prompt is held")))

(defun agent-repl--input-hold (ws said origin raw key &optional delivery)
  "Hold SAID for WS in the durable ingress under ORIGIN, KEY and DELIVERY.
DELIVERY is the attempt's `SubmitPromptDelivery' keyword, nil for the
ordinary delivery.
THE ONE PATH a prompt the daemon did not take goes down: it is written to
disk (`agent-repl-held-ingress-write'), where it survives a restart of
Emacs, the daemon and the shim, and the daemon ingests it into its own
held queue once it serves.  Emacs keeps no copy in memory.  Returns the
entry's path, or nil when it could not be written -- then the failure is
an ERROR record and a message naming RAW, which the history ring also
still holds, so the words are never lost silently."
  (condition-case err
      (agent-repl-held-ingress-write ws said origin key delivery)
    (error
     (agent-repl--error ws "elisp.input.hold-failed ws=%s origin=%S key=%s cause=%S"
                        ws origin key err)
     (message "agent-repl: the prompt could not be saved for the daemon (%s); it is in the input history: %s"
              (error-message-string err) raw)
     nil)))

(cl-defun agent-repl--input-submit (ws said origin raw &optional key from-buffer snapshot delivery)
  "Submit SAID to WS with ORIGIN; RAW is the user's own text for the record.
Resolves the workspace ref and the connection, mints the idempotency key,
and dispatches the answer arms.  Returns the idempotency key.

KEY re-uses an earlier attempt\='s idempotency key instead of minting a
fresh one, because a re-drive is a RETRY and not a second turn.

FROM-BUFFER says RAW was read out of the composer, and rides all the way
to the acceptance because only a composer-sourced submission may erase
the composer.  A canned command, a metaprompt read and a queue re-drive
all compose their own text and leave the user\='s draft alone.

DELIVERY is the `SubmitPromptDelivery' keyword the submission asks for
(`:deferred' for `SPC j RET'), nil for the ordinary delivery.  It rides the
request and every path that holds the prompt on disk.

The ref is REQUIRED on every submit: it names WHICH workspace the
submission belongs to, and it is the daemon-minted echo token, never a
value Emacs constructs from a path."
  (let ((ref (agent-repl-host-ref ws))
        (conn (agent-repl-host-conn-or-primary ws))
        (first-attempt (null key))
        (key (or key (agent-repl--uuid))))
    (agent-repl--with-log-context
     ws key
     (lambda ()
       ;; SENDING CLOSES A STANDING NEWS DIGEST (owner request, 2026-10-06):
       ;; the user has moved on to the conversation.  Only the first attempt
       ;; closes it -- a retry re-drives a send the user made earlier.
       (when first-attempt
         (agent-repl--input-dismiss-digest ws))
       (unless ref
         (agent-repl--fatal ws "elisp.input.submit-no-ref ws=%s origin=%S" ws origin))
       (unless conn
         (agent-repl--warn ws "elisp.input.submit-no-conn ws=%s origin=%S" ws origin)
         (agent-repl--input-on-failure
          ws said origin raw (list :kind :transport :message "no daemon connection") key delivery)
         (cl-return-from agent-repl--input-submit key))
       (agent-repl--info ws "elisp.input.submit ws=%s origin=%S key=%s blocks=%d selection=%S delivery=%S"
                         ws origin key (length (plist-get (plist-get said :content) :blocks))
                         (agent-repl-host-selection ws) delivery)
       (agent-repl-rpc-submit-prompt
        conn
        ;; No reply target rides the request: the daemon applies its own
        ;; selection to the prompt and ends it, pushing the result.
        (append (list :said said :idempotency-key key
                      :origin origin :workspace ref)
                (when delivery (list :delivery delivery))
                ;; THE PERSON'S SEND IS TIMED TO ITS ECHO on the page
                ;; (webapp/src/prompt-echo.ts); a retry's wait is not.
                (when first-attempt
                  (list :submitted-at-unix-ms (agent-repl--now-unix-ms))))
        :on-response
        (lambda (response)
          (agent-repl--with-log-context
           ws key
           (lambda ()
             (pcase (plist-get response :arm)
               (:success (agent-repl--input-on-success
                          ws raw origin (plist-get response :value) from-buffer))
               (:error (agent-repl--input-on-error
                        ws said origin raw (plist-get response :value) key snapshot delivery))
               (arm
                (agent-repl--error ws "elisp.input.unknown-response-arm ws=%s arm=%S"
                                   ws arm))))))
        :on-failure
        (lambda (detail)
          (agent-repl--with-log-context
           ws key
           (lambda ()
             (agent-repl--input-on-failure ws said origin raw detail key delivery)))))
       key))))

;;;; ---- The send pipeline ------------------------------------------------

(defun agent-repl--input-text-without-image-markers ()
  "Return (TEXT . STRIPPED) for the current buffer, markers removed.
TEXT is the buffer's contents with every span carrying
`agent-repl--input-image-marker-property' dropped; STRIPPED is how many
characters that cost.  The markers are DRAWN text, not composed text --
see the property's own docstring."
  (let ((pos (point-min))
        (end (point-max))
        (parts nil)
        (stripped 0))
    (while (< pos end)
      (let ((next (next-single-property-change
                   pos agent-repl--input-image-marker-property nil end)))
        (if (get-text-property pos agent-repl--input-image-marker-property)
            (setq stripped (+ stripped (- next pos)))
          (push (buffer-substring-no-properties pos next) parts))
        (setq pos next)))
    (cons (apply #'concat (nreverse parts)) stripped)))

(defun agent-repl--read-input-buffer (ws)
  "Return the text contents of WS's input buffer, or nil.
The attachment markers `clipboard-image.el' draws are NOT part of that
text: they are stripped here, which is the one place every send site and
the history push alike read the composer through."
  (let ((buf (agent-repl--input-buffer ws)))
    (if buf
        (with-current-buffer buf
          (let* ((read (agent-repl--input-text-without-image-markers))
                 (stripped (cdr read)))
            (when (> stripped 0)
              (agent-repl--log ws "elisp.input.image-markers-stripped ws=%s chars=%d"
                               ws stripped))
            (car read)))
      (agent-repl--log-verbose ws "elisp.input.read-no-buffer ws=%s" ws)
      nil)))

(defun agent-repl--send (origin &optional prompt ws force-metaprompt)
  "Submit PROMPT (or the input buffer's contents) to workspace WS.
ORIGIN is this send site's own `PromptOrigin' keyword and is REQUIRED --
it rides the wire and the daemon persists it onto the turn's durable
record.  When PROMPT is nil the text comes from the input buffer, and
that from-buffer submission is cleared OPTIMISTICALLY -- the composer is
erased and the prompt recorded the instant the user hits return, before
the wire call and regardless of whether the daemon ever acks (owner
ruling; the history ring and the held-prompt ingress are what keep the erased
draft recoverable, so nothing is lost).  When PROMPT is supplied the text
was composed by the caller (a canned command, a metaprompt, a queue
re-drive), and the user's own draft is left untouched -- those sites
record their prompt on the ack instead.  When WS is nil the current
workspace is used.  FORCE-METAPROMPT prepends the on-demand
read-directive.

While the composer is editing a held prompt, a from-buffer send COMMITS
the edit instead (`agent-repl-held-edit-commit') and returns nil.

An EMPTY from-buffer send asks the workspace's shim for its scheduled
vendor-error retry now (`agent-repl-verb-retry-vendor-error'); nothing is
sent as content.

Returns the submission's idempotency key, or nil when nothing was sent."
  (interactive (list :user-sent))
  (unless (memq origin agent-repl--input-origins)
    (agent-repl--fatal ws "elisp.input.invalid-origin origin=%S" origin))
  (let ((ws (or ws (agent-repl--ws-current-name))))
    (unless ws
      (agent-repl--fatal '(:agent-repl-central "the record reports that no workspace exists") "elisp.input.no-workspace origin=%S prompt-supplied=%s"
                         origin (not (null prompt))))
    (agent-repl--info ws "elisp.input.send ws=%s origin=%S force-metaprompt=%s from-buffer=%s"
                      ws origin force-metaprompt (null prompt))
    (agent-repl--input-check-gate ws)
    (let* ((raw (or prompt (agent-repl--read-input-buffer ws) ""))
           (attachments (agent-repl-input-attachments ws))
           (empty-p (and (string-empty-p (string-trim raw)) (null attachments))))
      (if empty-p
          (progn
            (agent-repl--info ws "elisp.input.send-empty ws=%s origin=%S -- nothing to send"
                              ws origin)
            ;; RET IN AN EMPTY COMPOSER RETRIES A FAILED TURN NOW (owner
            ;; ruling, 2026-10-20): the shim makes its scheduled retry at
            ;; once, with nothing sent as content.  With no retry scheduled
            ;; the shim answers so and nothing happens, as before.
            (when (null prompt)
              (agent-repl-verb-retry-vendor-error ws))
            nil)
        (let* ((from-buffer (null prompt))
               (text (agent-repl--prepare-input ws raw force-metaprompt))
               (said (agent-repl--input-said text attachments)))
          ;; A HELD-PROMPT EDIT TURNS THE COMPOSER'S SEND INTO ITS COMMIT:
          ;; the words replace the held prompt's content rather than going
          ;; out as a new prompt.  The composer is cleared and the words
          ;; recorded exactly as a send clears and records them.
          (if (and from-buffer (agent-repl-held-edit-active-p ws))
              (progn
                (agent-repl--info ws "elisp.input.send-commits-held-edit ws=%s origin=%S" ws origin)
                (agent-repl-held-edit-commit
                 ws said (agent-repl--input-optimistic-clear ws raw))
                nil)
            (agent-repl--kickoff-prompt-summary ws raw)
            ;; `(null prompt)' is the one fact that says the words came out
            ;; of the composer.  A from-buffer submission clears the composer
            ;; and records RAW HERE, at dispatch, so a missing or failed ack
            ;; can never leave the composer full (owner ruling).  `said' and
            ;; `attachments' were already captured above, so the clear costs
            ;; the submission nothing.
            (let ((snapshot (when from-buffer
                              (agent-repl--input-optimistic-clear ws raw))))
              (agent-repl--input-submit ws said origin raw nil from-buffer snapshot))))))))

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
  (agent-repl--on-simple-close))

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

;;;; ---- Numbered answers (a lone digit, `C-RET' on a multi-select) ------
;;
;; A STANDING CARD IS ANSWERED BY NUMBER.  While a question or permission
;; card stands on the root feed (the host view's `HostAsk'), a digit typed
;; into an EMPTY composer -- nothing before it but whitespace -- that names
;; one of the card's numbered choices is a click on that choice, sent at
;; once with no `RET'.  On a single-select question it answers the shown
;; question; on a multi-select it toggles the option's tick, and `C-RET'
;; with the composer still empty confirms the ticks; on a permission it
;; presses the button.  Anything else -- a digit after other text, a digit
;; no choice carries, no card standing -- is typed as usual.
;;
;; THE ANSWER IS THE CARD'S OWN VERB, naming the card's row:
;; `UpdateQuestionDraft' (pick, confirm) and `AnswerPermission', exactly as
;; the webapp's clicks send them.  A card that settled before the answer
;; arrived is refused as not standing and the composer says so; the answer
;; can never land on another card.

(defconst agent-repl--input-numbered-answer-prefix "answer"
  "The composer-flash prefix a numbered answer speaks under.")

(defun agent-repl--input-permission-buttons (permission)
  "Return PERMISSION's buttons in drawn order, each an `AnswerPermission' arm.
The order is `agentrepl.v1.HostAskPermission''s: allow once, always allow
when the vendor offered a standing form, deny."
  (append '(:allow-once)
          (when (plist-get permission :standing-offered) '(:allow-standing))
          '(:deny)))

(defun agent-repl--input-ask-choices (ask)
  "Return ASK's numbered choices in order; choice N is the Nth element.
Each choice is the answer plist `agent-repl--input-answer-choice' sends:
`(:verb :pick :row ROW :question-text TEXT :label LABEL)' for a question's
option, `(:verb :permission :row ROW :answer ARM)' for a permission's
button."
  (let ((row (plist-get ask :row))
        (kind (plist-get ask :kind)))
    (pcase (plist-get kind :arm)
      (:question
       (let* ((question (plist-get kind :value))
              (text (plist-get question :question-text)))
         (mapcar (lambda (option)
                   (list :verb :pick :row row :question-text text
                         :label (plist-get option :label)))
                 (plist-get (plist-get (plist-get question :selection) :value) :options))))
      (:permission
       (mapcar (lambda (answer) (list :verb :permission :row row :answer answer))
               (agent-repl--input-permission-buttons (plist-get kind :value))))
      (arm (error "agent-repl: a standing ask of kind %S has no choices" arm)))))

(defun agent-repl--input-composer-blank-p ()
  "Return non-nil when the current composer holds nothing but whitespace."
  (string-blank-p (buffer-substring-no-properties (point-min) (point-max))))

(defun agent-repl--input-numbered-choice (ws char)
  "Return the choice CHAR names on WS's standing ask, or nil.
CHAR names a choice when it is a digit 1-9, the current composer holds
nothing but whitespace, an ask stands, and the ask offers that many
choices."
  (let ((ask (agent-repl-host-standing-ask ws)))
    (when (and ask
               (characterp char) (<= ?1 char ?9)
               (agent-repl--input-composer-blank-p))
      (nth (- char ?1) (agent-repl--input-ask-choices ask)))))

(defun agent-repl--input-confirm-choice (ws)
  "Return the confirm `C-RET' sends for WS's standing ask, or nil.
Only a multi-select question is confirmed, and only from an empty
composer: with text in it `C-RET' sends that text as usual."
  (let* ((ask (agent-repl-host-standing-ask ws))
         (kind (plist-get ask :kind))
         (question (plist-get kind :value)))
    (when (and (eq (plist-get kind :arm) :question)
               (eq (plist-get (plist-get question :selection) :arm) :multi-select)
               (agent-repl--input-composer-blank-p))
      (list :verb :confirm :row (plist-get ask :row)
            :question-text (plist-get question :question-text)))))

(defun agent-repl--input-answer-refusal-text (cause)
  "Return the composer flash for a numbered answer's refusal CAUSE."
  (pcase (plist-get cause :arm)
    (:ask-not-standing "the card was already answered")
    (:unserved-value "the card no longer offers that choice")
    (:no-standing-offer "the card offers no always allow")
    (:no-session "the workspace has no session")
    (_ "answer refused")))

(defun agent-repl--input-answer-choice (ws choice)
  "Send CHOICE, a numbered answer to WS's standing card, on the card's verb.
CHOICE is a plist `agent-repl--input-ask-choices' or
`agent-repl--input-confirm-choice' built.  The card's new state arrives on
the feed; the success says nothing more."
  (let ((row (plist-get choice :row)))
    (pcase-let ((`(,op ,rpc ,request)
                 (pcase (plist-get choice :verb)
                   (:pick
                    (list "answer-question" #'agent-repl-rpc-update-question-draft
                          (lambda (ref)
                            (list :workspace ref :question row
                                  :edit (list :arm :pick
                                              :value (list :question-text (plist-get choice :question-text)
                                                           :label (plist-get choice :label)))))))
                   (:confirm
                    (list "answer-question" #'agent-repl-rpc-update-question-draft
                          (lambda (ref)
                            (list :workspace ref :question row
                                  :edit (list :arm :confirm
                                              :value (list :question-text (plist-get choice :question-text)))))))
                   (:permission
                    (list "answer-permission" #'agent-repl-rpc-answer-permission
                          (lambda (ref)
                            (list :workspace ref :permission row :answer (plist-get choice :answer)))))
                   (verb (error "agent-repl: a numbered answer of verb %S has no rpc" verb)))))
      (agent-repl--input-call
       ws :op op :prefix agent-repl--input-numbered-answer-prefix
       :detail-name "choice" :detail choice
       :rpc rpc :request request
       :on-success (lambda (_value)
                     (agent-repl--info ws "elisp.input.%s-answered ws=%s choice=%S" op ws choice))
       :on-refused #'agent-repl--input-answer-refusal-text))))

(defun agent-repl-input-digit ()
  "Answer the standing card with the typed digit's choice, or type the digit.
A digit typed into an empty composer that names one of the standing
card's numbered choices sends that choice at once; any other digit is
inserted as `self-insert-command' would."
  (interactive)
  (let* ((ws (agent-repl--ws-current-name))
         (choice (agent-repl--input-numbered-choice ws last-command-event)))
    (if choice
        (agent-repl--input-answer-choice ws choice)
      (setq this-command #'self-insert-command)
      (call-interactively #'self-insert-command))))

(defun agent-repl-input-control-return ()
  "Confirm a standing multi-select from an empty composer, else send with postfix.
`C-RET' finalizes the ticks a multi-select question was given by number;
with no multi-select standing, or with text in the composer, it is
`agent-repl-send-with-postfix'."
  (interactive)
  (let* ((ws (agent-repl--ws-current-name))
         (confirm (agent-repl--input-confirm-choice ws)))
    (if confirm
        (agent-repl--input-answer-choice ws confirm)
      (agent-repl-send-with-postfix))))

;;;; ---- Keybindings -----------------------------------------------------

(map! :map agent-repl-input-mode-map
      :ni "RET"       #'agent-repl-send
      :ni "S-RET"     #'newline
      :ni "C-RET"     #'agent-repl-input-control-return
      ;; Deferred-prompt enqueue lives on `SPC j RET' in the leader map
      ;; (keybindings.el) so it is reachable from any context with one
      ;; canonical chord.  Prefix-send stays reachable on macOS via
      ;; `S-s-RET' caught by the `[remap +default/newline-above]' entry.
      [remap +default/newline-below] #'agent-repl-input-control-return
      [remap +default/newline-above] #'agent-repl-send-with-prefix
      :ni "C-c C-c"   #'agent-repl-discard-input
      :n  "<up>"      #'agent-repl--history-prev
      :n  "<down>"    #'agent-repl--history-next
      ;; The selection's escape is COMMAND MODE ONLY (`:n' = evil normal
      ;; state), scoped to THIS composer's mode map like the history walk
      ;; above -- never global, so `<escape>' is only intercepted here.  The
      ;; escape command itself no-ops back to the ordinary escape when no
      ;; selection is active (see `agent-repl-input-selection-escape').  The
      ;; turn-boundary steps (`M-s-j'/`M-s-k') are bound below, in both states.
      :n  "<escape>"  #'agent-repl-input-selection-escape
      :i  "<escape>"  #'agent-repl-input-insert-escape
      ;; Prompt-history search sits on `C-M-r', not the `C-r' its shell
      ;; reflex would suggest: `C-r' is vacated for the output feed's
      ;; incremental search, whose isearch reflex wants it.
      :ni "C-M-r"     #'agent-repl-history-search)

;; `C-c C-k' interrupts the running turn -- the chord that used to do it
;; before the overhaul, restored against today's `Interrupt' verb.  It is
;; bound with `define-key' rather than through the `map!' form above so the
;; binding is OBSERVABLE under `emacs -Q' (where `map!' is a no-op stub) and
;; the keybinding suite can assert it, mirroring the numerals map.  A
;; `C-c'-prefixed chord is not shadowed by any evil state map, so a binding
;; on the mode map is live in both normal and insert state exactly as the
;; old `:ni' entry was.
(define-key agent-repl-input-mode-map (kbd "C-c C-k") #'agent-repl-interrupt-turn)

;; SEND AND NEWLINE IN EMACS STATE.  The `:ni' entries in the `map!' form above
;; live in evil's normal and insert auxiliary maps only, so in evil's emacs
;; state (and in a session without evil) `RET' fell through to the global
;; `newline', and the only chord that sent was `C-RET' -- which appends
;; `agent-repl-send-postfix'.  The plain mode map is what emacs state reads;
;; in normal and insert state the evil auxiliary maps still outrank it, so
;; these change nothing there.
(define-key agent-repl-input-mode-map (kbd "RET") #'agent-repl-send)
(define-key agent-repl-input-mode-map (kbd "S-RET") #'newline)
(define-key agent-repl-input-mode-map (kbd "C-<return>") #'agent-repl-input-control-return)

;; NUMBERED ANSWERS.  The digits 1-9 are bound on the plain mode map, which
;; insert and emacs state read; in normal state evil's motion map binds them
;; to `digit-argument' and outranks it, so a count prefix is untouched.  `0'
;; names no choice and stays an ordinary key.
(dolist (digit (number-sequence ?1 ?9))
  (define-key agent-repl-input-mode-map (string digit) #'agent-repl-input-digit))

;; FEED TEXT ZOOM.  Bound with `define-key' rather than through the `map!' form
;; above for the same reasons `C-c C-k' is: the binding is OBSERVABLE under
;; `emacs -Q' (where `map!' is a no-op stub) so the keybinding suite can assert
;; it, and it lives in EVERY evil state (`agent-repl-bind-feed-zoom-keys') so
;; the zoom works while typing.  These SHADOW Doom's `C-+'/`C--' bindings inside
;; the composer and the webview (frontend.el binds the same chords there),
;; which is the intended override -- the agent-repl commands send the daemon
;; RPC and never change Emacs's own font.
(agent-repl-bind-feed-zoom-keys agent-repl-input-mode-map)
(with-eval-after-load 'evil
  (agent-repl-bind-feed-zoom-keys agent-repl-input-mode-map))

;; TURN-BOUNDARY SELECTION (`M-s-j' down / `M-s-k' up): step through the
;; prompts a rollback can reach and the final responses, together, in feed
;; order.  Bound with `define-key' for the same reasons as the zoom: the
;; binding is OBSERVABLE under `emacs -Q', and it lives in BOTH evil states,
;; since no evil state map binds the chord to shadow the mode map.
(define-key agent-repl-input-mode-map (kbd "M-s-j") #'agent-repl-turn-boundary-select-next)
(define-key agent-repl-input-mode-map (kbd "M-s-k") #'agent-repl-turn-boundary-select-prev)

;; ROLLBACK (`C-c C-RET' keeps files, `C-c M-RET' restores them).  Bound with
;; `define-key' like `C-c C-k': a `C-c'-prefixed chord is not shadowed by any
;; evil state map, so it is live in both states, and the binding is
;; observable under `emacs -Q'.  `RET' and `C-c C-k' keep their meanings.
(define-key agent-repl-input-mode-map (kbd "C-c C-<return>") #'agent-repl-rollback-keep-files)
(define-key agent-repl-input-mode-map (kbd "C-c M-<return>") #'agent-repl-rollback-restore-files)

(provide 'input)

;;; input.el ends here
