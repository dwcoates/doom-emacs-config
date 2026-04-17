;;; input.el --- input mode, send system, slash pass-through -*- lexical-binding: t; -*-

;;; Code:

;;; Metaprompt permissions prefix

(defcustom claude-repl-skip-permissions t
  "When non-nil, prepend the command prefix metaprompt to each input sent to Claude."
  :type 'boolean
  :group 'claude-repl)

(defcustom claude-repl-prefix-period 7
  "Number of prompts between metaprompt prefix injections.
The prefix is sent on the first prompt and every Nth prompt thereafter."
  :type 'integer
  :group 'claude-repl)

(defcustom claude-repl-command-prefix "DO NOT run any mutating git commands (push, reset, checkout, etc) without EXPLICIT PERMISSION from ME. Do not INSTALL or UNINSTALL anything without my EXPLICIT PERMISSION. Do not operate on any files OUTSIDE OF PROJECT with MY EXPLICIT PERMISSION. Do not take any actions unless it FOLLOWS DIRECTLY from an action EXPLICITLY REQUESTED in the following prompt. I will NEVER ask a rhetorical question -- do not infer that I want you to take action to fix the source of a bug i've just asked a question about."
  "Safety instructions embedded in the metaprompt prefix.
This text is wrapped in the `claude-repl--command-prefix' template at load time
and periodically prepended to user input (see `claude-repl-prefix-period')."
  :type 'string
  :group 'claude-repl)

(defvar claude-repl--command-prefix
  (format (concat "<<*this is a metaprompt. "
                  "I will periodically prefix my prompts with this "
                  "to remind you of our restrictions for freely making changes. "
                  "Do not be alarmed, this is merely a periodic reminder*: "
                  "%s "
                  "*metaprompt over* "
                  "(rest is actual user request that you should respond to directly)>>\n\n")
          claude-repl-command-prefix)
  "Formatted metaprompt string prepended before every input.
Active when `claude-repl-skip-permissions' is non-nil, subject to
`claude-repl-prefix-period'.  Baked in at load time from
`claude-repl-command-prefix'.")

(defcustom claude-repl-send-postfix "\n what do you think? do NOT code, just analyze."
  "String appended to input when sending via `claude-repl-send-with-postfix'."
  :type 'string
  :group 'claude-repl)

(defvar claude-repl-paste-delay 0.25
  "Seconds to wait after pasting before sending Return.
Used by `claude-repl--send-input-to-vterm' for large inputs.")

;; Instructions bar face
(defface claude-repl-header-line
  '((t :background "white" :foreground "black" :weight bold))
  "Face for the Claude Input header line.")

;;; Backspace and basic editing

(defconst claude-repl--backspace-commands
  '(evil-delete-backward-char-and-join
    evil-delete-backward-char
    delete-backward-char
    backward-delete-char-untabify)
  "Commands that should be intercepted for backspace handling in the input buffer.")

(defun claude-repl--slash-intercept-backspace ()
  "Intercept backspace in the input buffer for vterm forwarding and slash mode.
In slash mode: redirects to `claude-repl--slash-backspace' via `this-command'.
Outside slash mode: forwards backspace to vterm when the buffer is empty.
Runs as a buffer-local `pre-command-hook'."
  (when (memq this-command claude-repl--backspace-commands)
    (if claude-slash-input-mode
        (progn
          (claude-repl--log-verbose nil "slash-intercept-backspace: slash-mode branch this-command=%s" this-command)
          (setq this-command #'claude-repl--slash-backspace))
      (if (= (buffer-size) 0)
          (if-let ((vterm-buf (claude-repl--current-ws-live-vterm)))
              (progn
                (claude-repl--log-verbose nil "slash-intercept-backspace: empty-buffer-forward sending <backspace> to vterm=%s this-command=%s"
                                          (buffer-name vterm-buf) this-command)
                (with-current-buffer vterm-buf
                  (vterm-send-key "<backspace>")))
            (claude-repl--log-verbose nil "slash-intercept-backspace: empty-buffer-forward no live vterm, skipping this-command=%s" this-command))
        (claude-repl--log-verbose nil "slash-intercept-backspace: normal branch this-command=%s" this-command)))))

;; Input mode
(define-derived-mode claude-input-mode fundamental-mode "Claude Input"
  "Major mode for Claude REPL input buffer."
  (setq-local header-line-format
              "RET: send | C-RET: send+postfix | C-c C-c: clear+save (empty→C-c) | C-c C-k: interrupt | <up>/<down>: history")
  (face-remap-add-relative 'header-line 'claude-repl-header-line)
  (claude-repl--set-buffer-background 37)
  (visual-line-mode 1)
  (add-hook 'after-change-functions #'claude-repl--history-on-change nil t)
  (add-hook 'pre-command-hook #'claude-repl--slash-intercept-backspace nil t))

(defun claude-repl-discard-input ()
  "Save current input to history, clear the buffer, and enter insert state."
  (interactive)
  (claude-repl--log nil "discard-input")
  (when claude-slash-input-mode
    (claude-repl--exit-slash-mode))
  (claude-repl--history-push)
  (claude-repl--history-reset)
  (claude-repl--history-save (+workspace-current-name))
  (erase-buffer)
  (evil-insert-state))

(defun claude-repl--vterm-send-raw-ctrl-c ()
  "Write a raw ETX byte (0x03, Ctrl-C) to the current workspace's vterm process.
Returns t on success, nil if no live vterm.

Bypasses `vterm-send-key' (which routes through libvterm's key-translation
layer and can dispatch a SIGINT instead of the literal keystroke).  A raw
ETX byte matches what a native terminal sends when the user types Ctrl-C
at a Claude prompt in raw-input mode — which is what actually clears the
input line.  On failure, logs and surfaces an error (no silent fallback)."
  (if-let ((vterm-buf (claude-repl--current-ws-live-vterm)))
      (progn
        (claude-repl--log nil "send-raw-ctrl-c: sending ETX (0x03) to vterm=%s" (buffer-name vterm-buf))
        (with-current-buffer vterm-buf
          (process-send-string vterm--process "\C-c"))
        t)
    (claude-repl--slash-no-vterm-error "send-raw-ctrl-c" "\\C-c")
    nil))

(defun claude-repl-discard-or-send-interrupt ()
  "Clear Claude's prompt AND the local input buffer.
Always sends a raw Ctrl-C to Claude (clearing its current input line) and,
if the local input buffer isn't already empty, also discards its contents.
Previously this only sent Ctrl-C when the local buffer was empty, which
left users with a half-cleared state."
  (interactive)
  (claude-repl--log nil "discard-or-send-interrupt: clearing Claude prompt + local buffer (local-empty=%s)"
                    (string-blank-p (buffer-string)))
  (unless (string-blank-p (buffer-string))
    (claude-repl-discard-input))
  (claude-repl--vterm-send-raw-ctrl-c))

;;; Arrow key forwarding (insert-mode terminal navigation)

(defun claude-repl--send-vterm-key (key-name)
  "Send KEY-NAME to the Claude vterm buffer."
  (if-let ((vterm-buf (claude-repl--current-ws-live-vterm)))
      (progn
        (claude-repl--log nil "send-vterm-key: sending %s to vterm=%s" key-name (buffer-name vterm-buf))
        (with-current-buffer vterm-buf
          (vterm-send-key key-name)))
    (claude-repl--log nil "send-vterm-key: no live vterm, skipping key=%s" key-name)))

(defun claude-repl--send-up-arrow ()
  "Forward up-arrow to vterm for terminal line navigation (insert mode)."
  (interactive)
  (claude-repl--send-vterm-key "<up>"))

(defun claude-repl--send-down-arrow ()
  "Forward down-arrow to vterm for terminal line navigation (insert mode)."
  (interactive)
  (claude-repl--send-vterm-key "<down>"))

;;; Vterm history scrolling (normal-mode C-n / C-p)

(defun claude-repl--send-vterm-down ()
  "Scroll vterm history forward (next item)."
  (interactive)
  (if-let ((vterm-buf (claude-repl--current-ws-live-vterm)))
      (progn
        (claude-repl--log nil "send-vterm-down: sending <down> to vterm=%s" (buffer-name vterm-buf))
        (with-current-buffer vterm-buf
          (vterm-send-down)))
    (claude-repl--log nil "send-vterm-down: no live vterm, skipping")))

(defun claude-repl--send-vterm-up ()
  "Scroll vterm history backward (previous item)."
  (interactive)
  (if-let ((vterm-buf (claude-repl--current-ws-live-vterm)))
      (progn
        (claude-repl--log nil "send-vterm-up: sending <up> to vterm=%s" (buffer-name vterm-buf))
        (with-current-buffer vterm-buf
          (vterm-send-up)))
    (claude-repl--log nil "send-vterm-up: no live vterm, skipping")))

;; Public aliases -- used in keybindings and tests.
(defalias 'claude-repl-scroll-down #'claude-repl--send-vterm-down)
(defalias 'claude-repl-scroll-up  #'claude-repl--send-vterm-up)

;;; Vterm output scrolling

(defcustom claude-repl-scroll-lines 15
  "Number of lines to scroll per `C-S-n' / `C-S-p' keypress."
  :type 'integer
  :group 'claude-repl)

(defun claude-repl--scroll-vterm-output (scroll-fn)
  "Scroll the Claude vterm output window using SCROLL-FN.
SCROLL-FN is called with a line count (e.g. `scroll-up' or `scroll-down')."
  (claude-repl--log nil "scroll-vterm-output: fn=%s" scroll-fn)
  (claude-repl--with-vterm-buf
   (let ((vterm-win (get-buffer-window vterm-buf)))
     (when vterm-win
       (with-selected-window vterm-win
         (funcall scroll-fn claude-repl-scroll-lines))))))

(defun claude-repl-scroll-output-up ()
  "Scroll the Claude vterm output window up (toward older output)."
  (interactive)
  (claude-repl--scroll-vterm-output #'scroll-down))

(defun claude-repl-scroll-output-down ()
  "Scroll the Claude vterm output window down (toward newer output)."
  (interactive)
  (claude-repl--scroll-vterm-output #'scroll-up))

;; Wheel handlers are identical to scroll-output -- alias them.
(defalias 'claude-repl--input-wheel-up   #'claude-repl-scroll-output-up)
(defalias 'claude-repl--input-wheel-down  #'claude-repl-scroll-output-down)

;;; Single-character confirmations (y/n)

(defmacro claude-repl--define-send-char-command (char)
  "Define an interactive command `claude-repl--send-CHAR' that sends CHAR to Claude."
  (let ((fn-name (intern (format "claude-repl--send-%s" char))))
    `(defun ,fn-name ()
       ,(format "Send \"%s\" to Claude." char)
       (interactive)
       (claude-repl-send-char ,char))))

(claude-repl--define-send-char-command "y")
(claude-repl--define-send-char-command "n")

;;; Keybindings
(map! :map claude-input-mode-map
      :ni "RET"       #'claude-repl--send
      :ni "S-RET"     #'newline
      :i  "/"         #'claude-repl--slash-start
      :ni "C-RET"     #'claude-repl-send-with-postfix
      :ni "C-S-RET"   #'claude-repl-send-with-metaprompt
      [remap +default/newline-below] #'claude-repl-send-with-postfix
      [remap +default/newline-above] #'claude-repl-send-with-metaprompt
      :ni "C-c C-k"   #'claude-repl-interrupt
      :ni "C-c C-c"   #'claude-repl-discard-or-send-interrupt
      :ni "C-c y"     #'claude-repl--send-y
      :ni "C-c n"     #'claude-repl--send-n
      :ni "C-c r"     #'claude-repl-restart
      :ni "C-c q"     #'claude-repl-kill
      :ni "C-S-m"     #'claude-repl-cycle
      :ni "C-h"       #'evil-window-left
      :ni "C-j"       #'evil-window-down
      :n  "C-n"       #'claude-repl--send-vterm-down
      :n  "C-p"       #'claude-repl--send-vterm-up
      :ni "C-v"       #'claude-repl-paste-to-vterm
      :n  "<up>"        #'claude-repl--history-prev
      :n  "<down>"      #'claude-repl--history-next
      :i  "<up>"        #'claude-repl--send-up-arrow
      :i  "<down>"      #'claude-repl--send-down-arrow
      :ni "S-<up>"      #'claude-repl-scroll-output-up
      :ni "S-<down>"    #'claude-repl-scroll-output-down
      :ni "C-S-p"       #'claude-repl-scroll-output-up
      :ni "C-S-n"       #'claude-repl-scroll-output-down
      [wheel-up]        #'claude-repl--input-wheel-up
      [wheel-down]      #'claude-repl--input-wheel-down)

;; C-S-0 through C-S-9: send digit to Claude from the input buffer.
;; Named distinctly from keybindings.el's `claude-repl--send-digit-char'
;; (which binds the leader keymap) to avoid a load-order shadowing conflict.
(defun claude-repl--input-send-digit-char ()
  "Send the digit from the current key event to Claude.
Extracts the base digit from `last-command-event' (e.g. C-S-3 -> \"3\")."
  (interactive)
  (let ((digit (number-to-string (- (event-basic-type last-command-event) ?0))))
    (claude-repl--log nil "input-send-digit-char: digit=%s" digit)
    (claude-repl-send-char digit)))

(dotimes (i 10)
  (define-key claude-input-mode-map (kbd (format "C-S-%s" i))
    #'claude-repl--input-send-digit-char))

;; 0-9 in insert mode: if the buffer is empty, enter pass-through mode and
;; forward the digit to vterm; otherwise insert normally.
(defun claude-repl--insert-digit-or-passthrough ()
  "In insert mode, pass digit through to vterm if the buffer is empty.
Otherwise insert the digit normally.  The digit is determined from
`last-command-event'."
  (interactive)
  (claude-repl--log nil "insert-digit-or-passthrough: digit=%s buffer-size=%d" (string last-command-event) (buffer-size))
  (claude-repl--passthrough-start (string last-command-event)))

(dotimes (i 10)
  (evil-define-key 'insert claude-input-mode-map (kbd (number-to-string i))
    #'claude-repl--insert-digit-or-passthrough))

;;; Input preparation and metaprompt

(defvar claude-repl-metaprompt-exempt-strings
  '("/clear" "/usage" "/login" "/logout")
  "Inputs that should never have the metaprompt prepended.
Compared exactly against the trimmed input.")

(defun claude-repl--skip-metaprompt-p (raw)
  "Return non-nil if RAW input should never have the metaprompt prepended.
Matches `claude-repl-metaprompt-exempt-strings' and bare numerals,
ignoring trailing whitespace."
  (let* ((trimmed (string-trim-right raw))
         (result (or (member trimmed claude-repl-metaprompt-exempt-strings)
                     (string-match-p "^[0-9]+$" trimmed))))
    (claude-repl--log-verbose nil "skip-metaprompt-p: result=%s" result)
    result))

(defvar claude-repl-send-posthooks
  '(("^/clear$" . claude-repl--posthook-reset-prefix-counter))
  "Alist of (PATTERN . FUNCTION) posthooks run after input is sent.
PATTERN is a string or regexp matched against the raw input (trimmed).
FUNCTION is called with (WS RAW) where WS is the workspace name and RAW is the input.")

(defun claude-repl--posthook-reset-prefix-counter (ws _raw)
  "Reset the metaprompt prefix counter for workspace WS.
Resets to 1 (just past the firing point) so the next send does not
immediately re-trigger the metaprompt."
  (claude-repl--ws-put ws :prefix-counter 1))

(defun claude-repl--run-send-posthooks (ws raw)
  "Run posthooks matching RAW input for workspace WS."
  (let ((trimmed (string-trim-right raw)))
    (dolist (hook claude-repl-send-posthooks)
      (when (string-match-p (car hook) trimmed)
        (claude-repl--log ws "posthook matched pattern=%s" (car hook))
        (funcall (cdr hook) ws raw)))))

(defun claude-repl--should-prepend-metaprompt-p (raw counter &optional force)
  "Return non-nil if the metaprompt prefix should be prepended to RAW.
COUNTER is the current prefix counter.  FORCE bypasses the counter check."
  (let ((result (and claude-repl-skip-permissions
                     claude-repl-command-prefix
                     (not (claude-repl--skip-metaprompt-p raw))
                     (or force (zerop (mod counter claude-repl-prefix-period))))))
    (claude-repl--log-verbose nil "should-prepend-metaprompt-p: result=%s counter=%d force=%s" result counter force)
    result))

(defun claude-repl--prepare-input (ws raw &optional force-metaprompt)
  "Optionally prepend metaprompt prefix to RAW for workspace WS.
When FORCE-METAPROMPT is non-nil, always prepend (ignoring the counter)."
  (let ((counter (or (claude-repl--ws-get ws :prefix-counter) 0)))
    (claude-repl--log ws "prepare-input counter=%d period=%d" counter claude-repl-prefix-period)
    (if (claude-repl--should-prepend-metaprompt-p raw counter force-metaprompt)
        (concat claude-repl--command-prefix raw)
      raw)))

;;; Send pipeline

(defun claude-repl--send-input-direct (vterm-buf input)
  "Send small INPUT string directly to VTERM-BUF and refresh."
  (claude-repl--log-verbose nil "send-input-direct: len=%d" (length input))
  (with-current-buffer vterm-buf
    (vterm-send-string input)
    (vterm-send-return)
    (claude-repl--refresh-vterm)))

(defun claude-repl--run-deferred-action (buf action)
  "Execute ACTION in BUF if BUF is still alive, with `inhibit-quit' bound to t.
Called by `run-at-time' as the timer callback for `claude-repl--vterm-deferred-action'."
  (if (buffer-live-p buf)
      (let ((inhibit-quit t))
        (with-current-buffer buf
          (funcall action)))
    (claude-repl--log nil "run-deferred-action: buffer is dead, skipping action=%s" action)))

(defun claude-repl--vterm-deferred-action (buf delay action)
  "Run ACTION in BUF after DELAY seconds, if BUF is still alive.
ACTION is called with `inhibit-quit' bound to t."
  (run-at-time delay nil
               #'claude-repl--run-deferred-action buf action))

(defun claude-repl--bracketed-finalize ()
  "Send a final Return and refresh vterm after bracketed paste.
Used as the second deferred action in the bracketed paste pipeline."
  (claude-repl--log-verbose nil "bracketed-finalize: sending final return")
  (vterm-send-return)
  (claude-repl--refresh-vterm))

(defun claude-repl--bracketed-send-return (vterm-buf)
  "Send Return to VTERM-BUF and schedule a finalize step.
Used as the first deferred action in the bracketed paste pipeline."
  (claude-repl--log-verbose nil "bracketed-send-return: sending return, scheduling finalize")
  (vterm-send-return)
  (claude-repl--vterm-deferred-action
   vterm-buf 0.05
   #'claude-repl--bracketed-finalize))

(defun claude-repl--send-input-bracketed (vterm-buf input)
  "Send large INPUT string to VTERM-BUF using bracketed paste mode.
Uses `claude-repl-paste-delay' to wait before sending Return."
  (claude-repl--log-verbose nil "send-input-bracketed: len=%d" (length input))
  (with-current-buffer vterm-buf
    (vterm-send-string input t)
    (claude-repl--vterm-deferred-action
     vterm-buf claude-repl-paste-delay
     (apply-partially #'claude-repl--bracketed-send-return vterm-buf))))

(defconst claude-repl--bracketed-paste-threshold 200
  "Input length above which bracketed paste mode is used.
Inputs longer than this are sent via `claude-repl--send-input-bracketed'
to avoid terminal truncation.")

(defun claude-repl--send-input-to-vterm (vterm-buf input)
  "Send INPUT string to VTERM-BUF.
Uses paste mode for large inputs to avoid truncation."
  (let ((use-paste (> (length input) claude-repl--bracketed-paste-threshold)))
    (claude-repl--log-verbose nil "send-input-to-vterm len=%d mode=%s"
                      (length input) (if use-paste "paste" "direct"))
    (if use-paste
        (claude-repl--send-input-bracketed vterm-buf input)
      (claude-repl--send-input-direct vterm-buf input))))

(defun claude-repl--mark-ws-thinking (ws)
  "Mark workspace WS as thinking: set claude-state."
  (claude-repl--log ws "mark-ws-thinking ws=%s" ws)
  (claude-repl--ws-set-claude-state ws :thinking))

(defun claude-repl--increment-prefix-counter (ws)
  "Increment the metaprompt prefix counter for workspace WS."
  (let ((new-val (1+ (or (claude-repl--ws-get ws :prefix-counter) 0))))
    (claude-repl--log-verbose ws "increment-prefix-counter: ws=%s new-counter=%d" ws new-val)
    (claude-repl--ws-put ws :prefix-counter new-val)))

(defun claude-repl--pin-owning-workspace (vterm-buf ws)
  "Pin WS as the owning workspace on VTERM-BUF.
Ensures title-change clears the correct workspace even if the
buffer drifts between perspectives."
  (when vterm-buf
    (claude-repl--log-verbose ws "pin-owning-workspace: ws=%s" ws)
    (with-current-buffer vterm-buf
      (setq-local claude-repl--owning-workspace ws))))

(defun claude-repl--do-send (ws input raw)
  "Core send: dispatch INPUT to WS's vterm.
Increments the prefix counter, pins the owning workspace, sends INPUT,
and runs posthooks with RAW (the undecorated text).

Does NOT write `:claude-state :thinking' — that transition is the
exclusive province of the `prompt_submit' Claude Code hook (routed
through `on-prompt-submit-event').  The brief gap between RET and the
red tab reflects that the hook is the source of truth for Claude's
state."
  (let ((vterm-buf (claude-repl--ws-get ws :vterm-buffer)))
    (claude-repl--log ws "do-send ws=%s len=%d" ws (length input))
    (claude-repl--increment-prefix-counter ws)
    (claude-repl--pin-owning-workspace vterm-buf ws)
    (claude-repl--send-input-to-vterm vterm-buf input)
    (claude-repl--run-send-posthooks ws raw)))

(defun claude-repl--commit-input-buffer (ws input-buf raw &optional clear-p)
  "Record RAW input in history and optionally clear INPUT-BUF.
WS is the workspace name used for history persistence.
When CLEAR-P is non-nil, erase the input buffer after saving history."
  (claude-repl--log ws "commit-input-buffer: ws=%s clear-p=%s" ws clear-p)
  (when (and input-buf (buffer-live-p input-buf))
    (with-current-buffer input-buf
      (claude-repl--history-push raw)
      (claude-repl--history-reset)
      (when clear-p (erase-buffer))))
  (claude-repl--history-save ws))

(defun claude-repl--read-input-buffer (ws)
  "Return the text contents of the input buffer for workspace WS, or nil."
  (claude-repl--log-verbose ws "read-input-buffer: ws=%s" ws)
  (when-let ((buf (claude-repl--ws-get ws :input-buffer)))
    (when (buffer-live-p buf)
      (with-current-buffer buf (buffer-string)))))

(defun claude-repl--send (&optional prompt ws force-metaprompt)
  "Send PROMPT (or input buffer contents) to Claude in workspace WS.
When PROMPT is nil, reads from the input buffer and clears it after sending.
When WS is nil, uses the current workspace.
When FORCE-METAPROMPT is non-nil, always prepend the metaprompt prefix.
Handles input preparation, sending, history, and persistence."
  (interactive)
  (let ((ws (or ws (+workspace-current-name))))
    (unless ws (error "claude-repl--send: no active workspace"))
    (claude-repl--log ws "send: ws=%s force-metaprompt=%s from-buf=%s" ws force-metaprompt (null prompt))
    (let* ((from-buf  (null prompt))
           (input-buf (claude-repl--ws-get ws :input-buffer))
           (vterm-buf (claude-repl--ws-get ws :vterm-buffer))
           (raw       (or prompt (claude-repl--read-input-buffer ws))))
      (unless raw
        (claude-repl--log ws "send: early return -- no raw input"))
      (unless vterm-buf
        (claude-repl--log ws "send: early return -- no vterm-buf for ws=%s" ws))
      (when (and vterm-buf (not (buffer-live-p vterm-buf)))
        (claude-repl--log ws "send: early return -- vterm-buf is dead for ws=%s" ws))
      (when (and raw vterm-buf (buffer-live-p vterm-buf))
        (let ((input (claude-repl--prepare-input ws raw force-metaprompt)))
          (claude-repl--do-send ws input raw)
          (claude-repl--commit-input-buffer ws input-buf raw from-buf))))))

(defun claude-repl-send-and-hide ()
  "Send input to Claude and hide both panels."
  (interactive)
  (claude-repl--log nil "send-and-hide")
  (claude-repl--send)
  (claude-repl--on-close))

(defun claude-repl-send-with-metaprompt ()
  "Send input with the metaprompt prefix, bypassing the counter."
  (interactive)
  (claude-repl--log nil "send-with-metaprompt")
  (claude-repl--send nil nil t))

(defun claude-repl--append-to-input-buffer (text)
  "Append TEXT to the end of the current workspace's input buffer."
  (claude-repl--log nil "append-to-input-buffer: len=%d" (length text))
  (when-let ((buf (claude-repl--ws-get (+workspace-current-name) :input-buffer)))
    (with-current-buffer buf
      (goto-char (point-max))
      (insert text))))

(defun claude-repl-send-with-postfix ()
  "Append `claude-repl-send-postfix' to the input buffer, then send."
  (interactive)
  (claude-repl--log nil "send-with-postfix")
  (claude-repl--append-to-input-buffer claude-repl-send-postfix)
  (claude-repl--send))

(defun claude-repl-send-char (char)
  "Send a single character to Claude."
  (claude-repl--log nil "send-char: char=%s" char)
  (if-let ((vterm-buf (claude-repl--current-ws-live-vterm)))
      (progn
        (claude-repl--log nil "send-char: sending %s + <return> to vterm=%s" char (buffer-name vterm-buf))
        (with-current-buffer vterm-buf
          (vterm-send-string char)
          (vterm-send-return)))
    (claude-repl--log nil "send-char: no live vterm, skipping char=%s" char)))

;;; Slash-command pass-through mode
;;
;; When the user types "/" into an empty input buffer, every subsequent
;; keystroke is forwarded directly to vterm without being inserted into the
;; input buffer.  The buffer stays visually empty.  Backspace is forwarded too,
;; and deleting back past the initial "/" exits the mode.

(defvar-local claude-repl--slash-stack nil
  "Stack of characters forwarded to vterm in slash mode.
Each element is the string that was sent (the leading \"/\" is the first entry).
Popped on backspace; when empty the mode exits.")

(define-minor-mode claude-slash-input-mode
  "Minor mode that transparently forwards keystrokes to Claude vterm.
Active when the user begins input with /. The input buffer stays empty;
all characters are sent directly to vterm.

Inhibits `evil-escape' while active — Doom configures evil-escape with
the `jk' key sequence and a 150ms delay, which otherwise causes every
`j' keystroke in slash mode to flutter (held 150ms waiting for a `k'
before being forwarded to vterm)."
  :lighter " /…"
  :keymap (make-sparse-keymap)
  (if claude-slash-input-mode
      (setq-local evil-escape-inhibit t)
    (kill-local-variable 'evil-escape-inhibit)))

(defun claude-repl--slash-on-insert-state-exit ()
  "Exit slash mode when evil leaves insert state (e.g. on ESC).
Installed on `evil-insert-state-exit-hook'.  Runs in every buffer that
leaves insert state, but only acts when the buffer-local
`claude-slash-input-mode' is active — so it's effectively scoped to the
Claude input buffer."
  (when claude-slash-input-mode
    (claude-repl--log nil "slash-on-insert-state-exit: exiting slash mode (evil left insert state)")
    (claude-repl--slash-quit)))

(add-hook 'evil-insert-state-exit-hook #'claude-repl--slash-on-insert-state-exit)

(defun claude-repl--exit-slash-mode ()
  "Clear the slash stack and disable `claude-slash-input-mode'."
  (claude-repl--log nil "exit-slash-mode: stack-depth=%d" (length claude-repl--slash-stack))
  (setq claude-repl--slash-stack nil)
  (claude-slash-input-mode -1))

(defun claude-repl--slash-no-vterm-error (what payload)
  "Log + user-visible error that WHAT couldn't reach vterm (with PAYLOAD).
Per AGENTS.md \"No Silent Fallbacks\": every vterm-forward failure in slash
mode must be surfaced to the user and logged with enough state to diagnose."
  (let* ((ws (+workspace-current-name))
         (recorded (claude-repl--ws-get ws :vterm-buffer))
         (live (and (bufferp recorded) (buffer-live-p recorded))))
    (claude-repl--log ws "slash-%s: FAILED no live vterm — ws=%s recorded-vterm=%S live=%s payload=%S"
                      what ws recorded live payload)
    (message "claude-repl: cannot forward to Claude — no live vterm in workspace %s"
             (or ws "<none>"))))

(defun claude-repl--slash-vterm-send (str)
  "Send STR to the current workspace's vterm buffer.
Return t on success, nil if there is no live vterm.  Must run the send
inside the vterm buffer via `with-current-buffer' — `vterm-send-string'
reads `vterm--term' buffer-locally and silently no-ops otherwise.
On failure, logs + surfaces a user-visible error."
  (claude-repl--log-verbose nil "slash-vterm-send: str=%S" str)
  (if-let ((vterm-buf (claude-repl--current-ws-live-vterm)))
      (progn
        (with-current-buffer vterm-buf
          (vterm-send-string str))
        t)
    (claude-repl--slash-no-vterm-error "send" str)
    nil))

(defun claude-repl--slash-try-send-and-push (str)
  "Try to send STR to vterm; push onto the slash stack ONLY on success.
Returns t on success, nil on failure.  Per AGENTS.md: we must not mutate
local state (the stack) when the operation it reflects (the forward to
vterm) did not actually happen."
  (if (claude-repl--slash-vterm-send str)
      (progn
        (push str claude-repl--slash-stack)
        (claude-repl--log-verbose nil "slash-try-send-and-push: char=%S stack-depth=%d"
                                  str (length claude-repl--slash-stack))
        t)
    (claude-repl--log nil "slash-try-send-and-push: REFUSED to push char=%S — send failed"
                      str)
    nil))

(defun claude-repl--slash-abort-and-insert (char)
  "Exit slash mode and insert CHAR as a regular self-insert into the input buffer.
Used when an in-flight slash-mode forward fails: we exit the mode (so the
user is no longer trapped), drop the current key into the input buffer (so
nothing is silently discarded), and leave already-forwarded characters in
vterm untouched (no rollback — they've already been sent)."
  (claude-repl--log nil "slash-abort-and-insert: char=%S stack-depth-before-exit=%d"
                    char (length claude-repl--slash-stack))
  (claude-repl--exit-slash-mode)
  (self-insert-command 1 (string-to-char char)))

(defun claude-repl--slash-forward-char ()
  "Forward the typed character to vterm without inserting it into the buffer.
If the forward fails, exit slash mode and drop the character into the
input buffer — never silently discard user input."
  (interactive)
  (let ((char (string last-command-event)))
    (claude-repl--log-verbose nil "slash-forward-char: char=%S" char)
    (unless (claude-repl--slash-try-send-and-push char)
      (claude-repl--slash-abort-and-insert char))))

(defun claude-repl--slash-backspace ()
  "Send backspace to vterm; pop the stack; exit mode when stack is empty.
If the send fails, exit slash mode loudly — do not pop the stack past what
was actually sent."
  (interactive)
  (if-let ((vterm-buf (claude-repl--current-ws-live-vterm)))
      (progn
        (claude-repl--log-verbose nil "slash-backspace: sending <backspace> to vterm=%s" (buffer-name vterm-buf))
        (with-current-buffer vterm-buf
          (vterm-send-key "<backspace>"))
        (pop claude-repl--slash-stack)
        (let ((remaining (length claude-repl--slash-stack)))
          (claude-repl--log-verbose nil "slash-backspace: remaining-depth=%d exiting=%s"
                                    remaining (if (null claude-repl--slash-stack) "t" "nil"))
          (when (null claude-repl--slash-stack)
            (claude-repl--exit-slash-mode))))
    (claude-repl--slash-no-vterm-error "backspace" nil)
    (claude-repl--exit-slash-mode)))

(defun claude-repl--slash-command-string ()
  "Reconstruct the slash command from the slash stack.
The stack is in reverse order (most recent push first), so we reverse
and concatenate.  Tab characters are included as-is."
  (apply #'concat (reverse claude-repl--slash-stack)))

(defun claude-repl--slash-workspace-command-p ()
  "Return non-nil if the current slash stack represents a /wor command.
Used to detect workspace-generation and workspace-update skills so
the source workspace identity can be injected."
  (string-prefix-p "/wor" (claude-repl--slash-command-string)))

(defun claude-repl--slash-maybe-inject-source-ws ()
  "If the slash command starts with /wor, send the source workspace tag to vterm.
Appends \" [source-ws:<ws-name>]\" so the skill can identify which workspace
initiated the generation.  Does not push to the slash stack (this is
injected text, not user keystrokes)."
  (when (claude-repl--slash-workspace-command-p)
    (let ((ws (+workspace-current-name)))
      (claude-repl--log nil "slash-maybe-inject-source-ws: injecting source-ws=%s" ws)
      (claude-repl--slash-vterm-send (format " [source-ws:%s]" ws)))))

(defun claude-repl--slash-return ()
  "Send return to vterm and exit slash mode.
For /wor commands, injects a [source-ws:NAME] tag before return so
workspace-generation and workspace-update skills know the originating workspace.
Exits the mode regardless of send outcome — being stuck in slash mode when
vterm is gone is strictly worse than having one unforwarded RET."
  (interactive)
  (claude-repl--log nil "slash-return: exiting slash mode")
  (claude-repl--slash-maybe-inject-source-ws)
  (if-let ((vterm-buf (claude-repl--current-ws-live-vterm)))
      (progn
        (claude-repl--log nil "slash-return: sending <return> to vterm=%s" (buffer-name vterm-buf))
        (with-current-buffer vterm-buf
          (vterm-send-return)))
    (claude-repl--slash-no-vterm-error "return" nil))
  (claude-repl--exit-slash-mode))

;; Use remaps throughout so evil keymap priority is irrelevant -- remaps are
;; resolved after key->command lookup and apply across all evil states.
(defun claude-repl--slash-tab ()
  "Forward a tab character to vterm in slash mode.
If the forward fails, exit slash mode and insert TAB — see
`claude-repl--slash-forward-char'."
  (interactive)
  (claude-repl--log-verbose nil "slash-tab: forwarding tab")
  (unless (claude-repl--slash-try-send-and-push "\t")
    (claude-repl--slash-abort-and-insert "\t")))

(defun claude-repl--slash-quit ()
  "Emergency escape: exit slash mode without sending anything to vterm.
Bound to C-g so the user can always bail out of slash mode regardless of
vterm state — cheap insurance against any future silent-fallback bugs in
the slash-mode plumbing."
  (interactive)
  (claude-repl--log nil "slash-quit: user-initiated emergency exit stack-depth=%d"
                    (length claude-repl--slash-stack))
  (claude-repl--exit-slash-mode))

(defun claude-repl--passthrough-start (char)
  "Enter slash mode and forward CHAR to vterm, or fail loudly without entering.
Preconditions (empty input buffer AND live vterm for the current workspace)
are checked up front.  On failure we log, surface a user-visible message,
and fall through to normal `self-insert-command' so the character lands in
the input buffer instead of vanishing into a stuck-mode stack.
Per AGENTS.md: no silent fallback, no dropped user input."
  (cond
   ((/= (buffer-size) 0)
    (claude-repl--log nil "passthrough-start: non-empty buffer, inserting normally char=%S" char)
    (self-insert-command 1 (string-to-char char)))
   ((null (claude-repl--current-ws-live-vterm))
    (claude-repl--slash-no-vterm-error "passthrough-start" char)
    (self-insert-command 1 (string-to-char char)))
   (t
    (claude-repl--log nil "passthrough-start: entering slash mode char=%S" char)
    (claude-slash-input-mode 1)
    ;; Race guard: vterm could die between the check above and the send.
    ;; Undo mode entry + insert the char so we never end up in slash mode
    ;; with an empty stack and no way to exit via the normal paths.
    (unless (claude-repl--slash-try-send-and-push char)
      (claude-repl--log nil "passthrough-start: race — vterm died during entry, aborting")
      (claude-repl--exit-slash-mode)
      (self-insert-command 1 (string-to-char char))))))

(defun claude-repl--slash-start ()
  "Enter pass-through mode if the buffer is empty, else insert / normally."
  (interactive)
  (claude-repl--log nil "slash-start: buffer-size=%d" (buffer-size))
  (claude-repl--passthrough-start "/"))

(map! :map claude-slash-input-mode-map
      [remap self-insert-command]                #'claude-repl--slash-forward-char
      [remap indent-for-tab-command]             #'claude-repl--slash-tab
      [remap evil-delete-backward-char-and-join] #'claude-repl--slash-backspace
      [remap delete-backward-char]               #'claude-repl--slash-backspace
      [remap backward-delete-char-untabify]      #'claude-repl--slash-backspace
      [remap claude-repl--send]                   #'claude-repl--slash-return
      [remap keyboard-quit]                       #'claude-repl--slash-quit
      :ni "C-g"    #'claude-repl--slash-quit
      :ni "<up>"   #'ignore
      :ni "<down>" #'ignore)
