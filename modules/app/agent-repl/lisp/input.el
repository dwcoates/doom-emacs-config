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

(declare-function agent-repl--log "agent-repl-core" (ws fmt &rest args))
(declare-function agent-repl--log-verbose "agent-repl-core" (ws fmt &rest args))
(declare-function agent-repl--info "agent-repl-core" (ws fmt &rest args))
(declare-function agent-repl--warn "agent-repl-core" (ws fmt &rest args))
(declare-function agent-repl--error "agent-repl-core" (ws fmt &rest args))
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
;; The history ring's own buffer-local state, declared here because the
;; refusal restore puts it back to its pre-submit value (see
;; `agent-repl--input-restore').
(defvar agent-repl--input-history)
(defvar agent-repl--history-index)
(defvar agent-repl--history-navigating)
(declare-function agent-repl--on-close "agent-repl-panels" ())
(declare-function agent-repl-host-ref "agent-repl-host" (ws))
(declare-function agent-repl-host-conn "agent-repl-host" (ws))
(declare-function agent-repl-host-composer-gate "agent-repl-host" (ws))
(declare-function agent-repl-link-primary "agent-repl-daemon-link" ())
(declare-function agent-repl-rpc-submit-prompt "agent-repl-rpc" (conn request &rest keys))
(declare-function agent-repl-rpc-select-response "agent-repl-rpc" (conn request &rest keys))
(declare-function agent-repl-held-edit-active-p "held-edit" (ws))
(declare-function agent-repl-held-edit-commit "held-edit" (ws said snapshot))
(declare-function agent-repl-held-edit-cancel "held-edit" (ws))
(declare-function agent-repl-rpc-adjust-feed-text-scale "agent-repl-rpc" (conn request &rest keys))
(declare-function agent-repl-host-handle-refusal "agent-repl-host" (ws arm-plist))
(declare-function agent-repl-interrupt-turn "agent-repl-verbs" (&optional ws))
(declare-function agent-repl-held-ingress-write "held-ingress" (ws said origin key))
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

(defvar-local agent-repl--input-response-selection nil
  "The final-response row this composer is currently REPLYING TO, or nil.
The decoded `frontend.v1.FeedId' plist `(:value STRING)' the daemon acked
from a `SelectResponse' nav, buffer-local to THIS composer because a reply
target is about this workspace's feed and no other.  Non-nil is the whole
\"a selection is active\" fact: `C-p'/`C-n' record it, escape-twice and an
accepted submit clear it, and the next submit rides it as
`reference_response_feedid'.  The daemon owns the authoritative selection
state (it also pushes it to the webapp); this is Emacs's own copy for the
escape state machine and the submit path.")

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
  (add-hook 'after-change-functions #'agent-repl--history-on-change nil t)
  (add-hook 'kill-buffer-hook #'agent-repl--input-cancel-flash-timer nil t))

(defun agent-repl--input-buffer (ws)
  "Return WS's live input buffer, or nil."
  (let ((buf (agent-repl--ws-get ws :input-buffer)))
    (and buf (buffer-live-p buf) buf)))

(defun agent-repl-discard-input ()
  "Save current input to history, clear the buffer, and enter insert state.
While the composer is editing a held prompt this is also the edit's
CANCEL: the held prompt keeps its content and the queue resumes
(`agent-repl-held-edit-cancel')."
  (interactive)
  (let ((ws (agent-repl--ws-current-name))
        (input-len (buffer-size)))
    (agent-repl--log ws "elisp.input.discard ws=%s input-len=%d" ws input-len)
    (when (agent-repl-held-edit-active-p ws)
      (agent-repl-held-edit-cancel ws))
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

;;;; ---- Response selection (reply to a past response) -------------------
;;
;; REPLY-TO-A-PAST-RESPONSE, command-mode side.  In the composer's evil
;; NORMAL state (command mode), `C-p'/`C-n' walk the ordered final-response
;; rows and escape-twice clears the walk.  The DAEMON owns the selection
;; state per workspace: it knows the ordered rows, so it COMPUTES the newly
;; selected feedid (both directions start at the most recent and wrap at each
;; end) and pushes it to the webapp; Emacs sends only the DIRECTION and keeps
;; a buffer-local copy of the acked feedid for the escape state machine and
;; the submit path.  The next submit rides that feedid as
;; `reference_response_feedid'; the daemon prepends a copy of the referenced
;; response before delivering.

(defconst agent-repl--input-response-selection-escape-warning
  "reply-to-response: press escape again to clear the selection"
  "The minibuffer WARNING shown on the FIRST command-mode escape.
No y/n confirmation -- a plain warning that a SECOND consecutive escape
clears the selection, so the user reads it before anything is cleared.")

(defconst agent-repl--input-response-selection-no-rows-flash
  "reply-to-response: no final response to select"
  "The composer flash when a nav finds no selectable final-response rows.")

(defun agent-repl--input-response-selection-value (ws)
  "Return WS's active reply-to-a-past-response FeedId plist, or nil.
Read out of WS's composer, where the async nav ack stored it; nil is the
ordinary-prompt case (no selection active)."
  (let ((buf (agent-repl--input-buffer ws)))
    (and buf (buffer-local-value 'agent-repl--input-response-selection buf))))

(defun agent-repl--input-response-selection-active-p (ws)
  "Return non-nil when WS's composer holds an active reply target."
  (not (null (agent-repl--input-response-selection-value ws))))

(defun agent-repl--input-response-selection-store (ws feedid)
  "Store FEEDID as WS's active reply target; nil drops the local selection.
Runs from the async `SelectResponse' ack and from the submit/clear paths,
so it writes the SPECIFIC composer buffer rather than whatever buffer
happens to be current when the answer arrives."
  (let ((buf (agent-repl--input-buffer ws)))
    (when buf
      (with-current-buffer buf
        (setq agent-repl--input-response-selection feedid)))))

(defun agent-repl--input-response-select (ws direction)
  "Send a `SelectResponse' nav for WS in DIRECTION and record the ack.
DIRECTION is `:prev', `:next' or `:clear'.  The daemon computes the newly
selected feedid (or NONE) and this stores it buffer-local so escape and
the submit path can read it.  A refusal or transport failure leaves the
prior selection untouched and flashes the composer -- error coverage is
never dropped just because the daemon is the authority."
  (let ((ref (agent-repl-host-ref ws))
        (conn (or (agent-repl-host-conn ws) (agent-repl-link-primary))))
    (cond
     ((null ref)
      (agent-repl--warn ws "elisp.input.response-select-no-ref ws=%s dir=%S" ws direction)
      (agent-repl--input-flash ws "reply-to-response: workspace not ready"))
     ((null conn)
      (agent-repl--warn ws "elisp.input.response-select-no-conn ws=%s dir=%S" ws direction)
      (agent-repl--input-flash ws "reply-to-response: no daemon connection"))
     (t
      (agent-repl--info ws "elisp.input.response-select ws=%s dir=%S" ws direction)
      (agent-repl-rpc-select-response
       conn (list :workspace ref :direction direction)
       :on-response
       (lambda (response)
         (pcase (plist-get response :arm)
           (:success
            (let ((selected (plist-get (plist-get response :value) :selected)))
              (agent-repl--input-response-selection-store ws selected)
              (agent-repl--info ws "elisp.input.response-selected ws=%s dir=%S selected=%s"
                                ws direction (and selected t))
              ;; A prev/next that lands on NOTHING means there are no
              ;; selectable rows: say so rather than leave the gesture silent.
              (when (and (memq direction '(:prev :next)) (null selected))
                (agent-repl--input-flash
                 ws agent-repl--input-response-selection-no-rows-flash))))
           (:error
            (agent-repl--warn ws "elisp.input.response-select-refused ws=%s dir=%S cause=%S"
                              ws direction
                              (plist-get (plist-get response :value) :cause))
            (agent-repl--input-flash ws "reply-to-response: selection refused"))
           (arm
            (agent-repl--error ws "elisp.input.response-select-unknown-arm ws=%s arm=%S"
                               ws arm))))
       :on-failure
       (lambda (detail)
         (agent-repl--warn ws "elisp.input.response-select-failure ws=%s dir=%S detail=%S"
                           ws direction detail)
         (agent-repl--input-flash ws "reply-to-response: the daemon did not answer")))))))

(defun agent-repl-response-select-prev ()
  "Select the PREVIOUS (older) final-response row to reply to.
Composer command mode only.  From no selection it starts at the most
recent final response; past the oldest it wraps to the newest -- the
daemon owns the order and computes the feedid, Emacs sends the direction."
  (interactive)
  (agent-repl--input-response-select (agent-repl--ws-current-name) :prev))

(defun agent-repl-response-select-next ()
  "Select the NEXT (newer) final-response row to reply to.
Composer command mode only.  From no selection it starts at the most
recent final response; past the newest it wraps to the oldest."
  (interactive)
  (agent-repl--input-response-select (agent-repl--ws-current-name) :next))

;; FEED TEXT ZOOM (`C-+' / `C--').  The daemon owns and persists a single
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
         (conn (or (and ws (agent-repl-host-conn ws)) (agent-repl-link-primary))))
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
Bound to `C-+' in the composer, overriding Doom's global text-scale
binding there.  It NEVER changes Emacs's own font — only the webapp feed
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

(defun agent-repl--input-escape-default ()
  "Run escape's ORDINARY meaning in the composer.
Called only when NO reply-to-a-past-response selection is active, so this
command never STEALS escape when there is nothing to clear.  `doom/escape'
is the full Doom escape (it runs `doom-escape-hook', which is where evil's
force-normal-state lives, and falls back to `keyboard-quit'); it is used
when present and `keyboard-quit' otherwise."
  (if (fboundp 'doom/escape)
      (call-interactively 'doom/escape)
    (keyboard-quit)))

(defun agent-repl--input-response-selection-clear (ws)
  "Send `SelectResponse' CLEAR for WS and drop the local selection.
The webapp returns to the feed bottom on the daemon's clear; Emacs sends
the direction and forgets its own copy AT ONCE, so the escape state
machine is settled without waiting on the async ack."
  (agent-repl--info ws "elisp.input.response-selection-clear ws=%s" ws)
  (agent-repl--input-response-select ws :clear)
  (agent-repl--input-response-selection-store ws nil))

(defun agent-repl-input-response-selection-escape ()
  "Command-mode escape for reply-to-a-past-response.
WITH NO SELECTION active, escape keeps its ordinary meaning
(`agent-repl--input-escape-default') -- this command never breaks escape
when there is nothing to clear.  WITH a selection active it takes TWO
CONSECUTIVE escapes to clear:

  1st escape  -> a minibuffer WARNING that another escape will clear it.
  2nd escape  -> send CLEAR and drop the selection.

CONSECUTIVE is read off `last-command': after this command runs, the
command loop sets `last-command' to it, so a second escape with no other
key in between sees `last-command' equal to this command and clears; ANY
other command-mode key in between runs as a different `last-command' and
re-arms the warning instead.  That is the whole two-consecutive-escapes
state machine -- there is no separate counter to fall out of sync."
  (interactive)
  (let ((ws (agent-repl--ws-current-name)))
    (if (not (agent-repl--input-response-selection-active-p ws))
        (agent-repl--input-escape-default)
      (if (eq last-command 'agent-repl-input-response-selection-escape)
          (agent-repl--input-response-selection-clear ws)
        (agent-repl--warn ws "elisp.input.response-selection-escape-armed ws=%s" ws)
        (message "%s" agent-repl--input-response-selection-escape-warning)))))

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
session implicitly -- EXCEPT when that session is parked at its COLD
GATE.  A cold gate is a precondition SubmitPrompt does not satisfy: the
shim is up and serving, and it takes no prompt until the user answers the
gate in the panel (clear / compact / resume).  The daemon refuses such a
submission with `SubmitPromptError''s own `cold_gate' arm, which
`agent-repl--input-on-error' draws; the gate still SENDS rather than
guessing, because the daemon is the authority on that standing and
answers by name.")

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
stood BEFORE the push.  `agent-repl--input-restore' puts it all back when
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
                             :history-index agent-repl--history-index))
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
        (setq agent-repl--history-index (plist-get snapshot :history-index)))
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
  ;; The daemon accepted the submission and consumed any reply-to-a-past-
  ;; response target with it, so Emacs drops its local selection: the next
  ;; prompt is an ordinary one unless the user selects again.
  (agent-repl--input-response-selection-store ws nil)
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
carry.  The prompt itself is written to the durable held-prompt ingress
under THIS attempt\='s KEY: it was refused, not consumed, so re-driving
it is a RETRY of this submission and must go back out under the same key
for the daemon to recognize a duplicate.  The daemon that owns the
workspace once the handover ends ingests it.

The composer KEEPS its text: nothing here says the prompt landed, and the
user is left able to see exactly what they wrote."
  (agent-repl--info ws "elisp.input.handover-refusal ws=%s origin=%S arm=%S key=%s"
                    ws origin (plist-get arm :arm) key)
  (agent-repl-host-handle-refusal ws arm)
  (agent-repl--input-hold ws said origin raw key))

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

(defconst agent-repl--input-no-session-flash
  "no session; the daemon is starting it"
  "The composer flash for `SubmitPromptError''s `no_session' arm.
A workspace with NO SESSION AT ALL, which is the daemon's own to bring
up.  Nothing is owed by the user and nothing is owed by this composer, so
the sentence states the fact and the prompt is neither held nor lost.")

(defun agent-repl--input-on-error (ws said origin raw error key &optional snapshot)
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
refusal can re-drive the very prompt that was refused."
  (agent-repl--input-restore ws snapshot)
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
         (agent-repl--input-flash ws agent-repl--input-cold-gate-flash)
         (message "agent-repl: %s%s" agent-repl--input-cold-gate-flash
                  (if (string-empty-p detail) "" (format " (%s)" detail)))))
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
      (:no-session
       ;; The workspace has NO SESSION AT ALL, which is the daemon's own
       ;; to bring up.  Like the cold gate this is an answer rather than a
       ;; transport failure, so it is recorded at WARN and the prompt is
       ;; not queued; the text stays so the user can resubmit once the
       ;; session is up.
       (agent-repl--warn ws "elisp.input.refused-no-session ws=%s origin=%S arm=%S" ws origin arm)
       (agent-repl--input-flash ws agent-repl--input-no-session-flash)
       (message "agent-repl: %s" agent-repl--input-no-session-flash))
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
landed.  The full SAID is written to the durable held-prompt ingress
(`agent-repl--input-hold'), which the daemon ingests once it serves --
the entry is independent of the composer, so a from-buffer submission
that was optimistically cleared at dispatch loses nothing: its words ride
the ingress AND sit in the history ring.  This path adds no erase and no
restore of the composer.

KEY is THIS attempt\='s idempotency key and rides into the entry: the
re-drive is a RETRY of this submission, not a second turn, so the daemon
answers an attempt that did land as a duplicate and delivers it once."
  (agent-repl--error ws "elisp.input.transport-failure ws=%s origin=%S key=%s detail=%S"
                     ws origin key detail)
  (when (agent-repl--input-hold ws said origin raw key)
    (agent-repl--input-flash ws "send failed -- held until the daemon is back")
    (message "agent-repl: the daemon did not answer; the prompt is held")))

(defun agent-repl--input-hold (ws said origin raw key)
  "Hold SAID for WS in the durable ingress under ORIGIN and KEY.
THE ONE PATH a prompt the daemon did not take goes down: it is written to
disk (`agent-repl-held-ingress-write'), where it survives a restart of
Emacs, the daemon and the shim, and the daemon ingests it into its own
held queue once it serves.  Emacs keeps no copy in memory.  Returns the
entry's path, or nil when it could not be written -- then the failure is
an ERROR record and a message naming RAW, which the history ring also
still holds, so the words are never lost silently."
  (condition-case err
      (agent-repl-held-ingress-write ws said origin key)
    (error
     (agent-repl--error ws "elisp.input.hold-failed ws=%s origin=%S key=%s cause=%S"
                        ws origin key err)
     (message "agent-repl: the prompt could not be saved for the daemon (%s); it is in the input history: %s"
              (error-message-string err) raw)
     nil)))

(cl-defun agent-repl--input-submit (ws said origin raw &optional key from-buffer snapshot)
  "Submit SAID to WS with ORIGIN; RAW is the user's own text for the record.
Resolves the workspace ref and the connection, mints the idempotency key,
and dispatches the answer arms.  Returns the idempotency key.

KEY re-uses an earlier attempt\='s idempotency key instead of minting a
fresh one, because a re-drive is a RETRY and not a second turn.

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
    (agent-repl--with-log-context
     ws key
     (lambda ()
       (unless ref
         (agent-repl--fatal ws "elisp.input.submit-no-ref ws=%s origin=%S" ws origin))
       (unless conn
         (agent-repl--warn ws "elisp.input.submit-no-conn ws=%s origin=%S" ws origin)
         (agent-repl--input-on-failure
          ws said origin raw (list :kind :transport :message "no daemon connection") key)
         (cl-return-from agent-repl--input-submit key))
       (agent-repl--info ws "elisp.input.submit ws=%s origin=%S key=%s blocks=%d reply-to=%s"
                         ws origin key (length (plist-get (plist-get said :content) :blocks))
                         (and (agent-repl--input-response-selection-value ws) t))
       (agent-repl-rpc-submit-prompt
        conn
        ;; A reply-to-a-past-response selection rides ALONGSIDE the composed
        ;; text: the field is present only when a selection is active, so an
        ;; ordinary prompt sends exactly what it always did.
        (let ((request (list :said said :idempotency-key key
                             :origin origin :workspace ref))
              (feedid (agent-repl--input-response-selection-value ws)))
          (if feedid
              (append request (list :reference-response-feedid feedid))
            request))
        :on-response
        (lambda (response)
          (agent-repl--with-log-context
           ws key
           (lambda ()
             (pcase (plist-get response :arm)
               (:success (agent-repl--input-on-success
                          ws raw origin (plist-get response :value) from-buffer))
               (:error (agent-repl--input-on-error
                        ws said origin raw (plist-get response :value) key snapshot))
               (arm
                (agent-repl--error ws "elisp.input.unknown-response-arm ws=%s arm=%S"
                                   ws arm))))))
        :on-failure
        (lambda (detail)
          (agent-repl--with-log-context
           ws key
           (lambda ()
             (agent-repl--input-on-failure ws said origin raw detail key)))))
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
      ;; Reply-to-a-past-response nav is COMMAND MODE ONLY (`:n' = evil
      ;; normal state), scoped to THIS composer's mode map like the history
      ;; walk above -- never global, so `C-p'/`C-n' keep their meaning
      ;; everywhere else and `<escape>' is only intercepted here.  The escape
      ;; command itself no-ops back to the ordinary escape when no selection
      ;; is active (see `agent-repl-input-response-selection-escape').
      :n  "C-p"       #'agent-repl-response-select-prev
      :n  "C-n"       #'agent-repl-response-select-next
      :n  "<escape>"  #'agent-repl-input-response-selection-escape
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

;; FEED TEXT ZOOM.  Bound with `define-key' rather than through the `map!' form
;; above for the same reasons `C-c C-k' is: the binding is OBSERVABLE under
;; `emacs -Q' (where `map!' is a no-op stub) so the keybinding suite can assert
;; it, and it lives in BOTH evil states so the zoom works while typing.  These
;; SHADOW Doom's global `C-+'/`C--' text-scale bindings inside the composer,
;; which is the intended override -- the agent-repl commands send the daemon
;; RPC and never change Emacs's own font.
(define-key agent-repl-input-mode-map (kbd "C-+") #'agent-repl-feed-text-scale-increase)
(define-key agent-repl-input-mode-map (kbd "C--") #'agent-repl-feed-text-scale-decrease)

(provide 'input)

;;; input.el ends here
