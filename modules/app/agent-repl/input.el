;;; input.el --- input mode, send system, slash pass-through -*- lexical-binding: t; -*-

;;; Code:

;;; Metaprompt permissions prefix

(defcustom agent-repl-skip-permissions t
  "When non-nil, prepend the command prefix metaprompt to each input sent to Claude."
  :type 'boolean
  :group 'agent-repl)

(defcustom agent-repl-prefix-period 14
  "Number of prompts between metaprompt prefix injections.
The prefix is sent on the first prompt and every Nth prompt thereafter."
  :type 'integer
  :group 'agent-repl)

(defvar agent-repl-metaprompt-file
  (expand-file-name "metaprompt.md"
                    (file-name-directory (or load-file-name buffer-file-name)))
  "Absolute path to the canonical metaprompt source file in the emacs repo.
This is the .md data file extracted out of input.el so that the
metaprompt body lives as plain text, edited and version-controlled
alongside the code.  Captured at file-load time because `load-file-name'
is only bound during load.")

(defcustom agent-repl-metaprompt-file-symlink
  "~/.config/claude/emacs/metaprompt.md"
  "User-facing symlink path embedded in the metaprompt wrapper template.
Points to `agent-repl-metaprompt-file' and is the path Claude is
instructed to read (or re-read) at each metaprompt injection so the
directive is loaded from the canonical source file on every send."
  :type 'string
  :group 'agent-repl)

(defcustom agent-repl-command-prefix
  (with-temp-buffer
    (insert-file-contents agent-repl-metaprompt-file)
    (buffer-string))
  "Canonical metaprompt content, loaded from `agent-repl-metaprompt-file'.
Not sent inline to Claude — the wrapper template at
`agent-repl-command-prefix-template' instead instructs Claude to read
`agent-repl-metaprompt-file-symlink' directly, so the body stays in one
canonical place on disk.  This variable mirrors the file's content for
tests and tooling that need to assert against the canonical metaprompt
without re-reading the file."
  :type 'string
  :group 'agent-repl)

(defcustom agent-repl-command-prefix-template
  (concat "Before taking any action, read the file at %s into context, "
          "even if you have previously done so during this session. "
          "It contains guidelines for answering the remaining messages in this session, "
          "and you must follow them. "
          "Read them into context even if you have already done so previously "
          "and they have not changed since. Everything that follows this sentence "
          "is a real user prompt, and for answering it, and for answering ALL subsequent user "
          "prompts, obey precisely the guidelines that you just read for answering, and without failure.")
  "Template instructing Claude to read the metaprompt file before acting.
Must contain a single %s placeholder, filled at load time with
`agent-repl-metaprompt-file-symlink'.  Intentionally avoids any
\"metaprompt\" terminology in the inline prefix itself — the wrapper
bookends and directive framing live inside the .md file rather than
here, so the inline prefix is a plain instruction to read the file."
  :type 'string
  :group 'agent-repl)

(defvar agent-repl--command-prefix
  (format agent-repl-command-prefix-template
          agent-repl-metaprompt-file-symlink)
  "Formatted read-directive prepended before every periodic user input.
Active when `agent-repl-skip-permissions' is non-nil, subject to
`agent-repl-prefix-period'.  A plain instruction to read the file at
`agent-repl-metaprompt-file-symlink' — the metaprompt body and its
wrapper bookends live inside that file, not here.")

;; `defcustom' and `defvar' only initialize their values on first load;
;; reloading the file (e.g. via `agent-repl-reload-config' or
;; `doom/reload') leaves the variables at their old values even when the
;; source has changed.  Force-refresh them here so editing the metaprompt
;; (either the .md file or the template) and reloading is enough — no
;; manual `M-x eval-defun' required.  `standard-value' is re-set by
;; `defcustom' on every load, so re-evaluating it picks up whatever this
;; file currently defines.
(setq agent-repl-command-prefix
      (eval (car (get 'agent-repl-command-prefix 'standard-value))))
(setq agent-repl-command-prefix-template
      (eval (car (get 'agent-repl-command-prefix-template 'standard-value))))
(setq agent-repl--command-prefix
      (format agent-repl-command-prefix-template
              agent-repl-metaprompt-file-symlink))

(defcustom agent-repl-send-postfix "\n what do you think? do NOT code, just analyze."
  "String appended to input when sending via `agent-repl-send-with-postfix'."
  :type 'string
  :group 'agent-repl)

(defcustom agent-repl-send-prefix "just answer, dont take action: "
  "String prepended to input when sending via `agent-repl-send-with-prefix'."
  :type 'string
  :group 'agent-repl)

(defcustom agent-repl-paste-delay 0.25
  "Seconds to wait after pasting before sending Return.
Used by `agent-repl--send-input-to-vterm' for large inputs."
  :type 'number
  :group 'agent-repl)

;; Instructions bar face
(defface agent-repl-header-line
  '((t :background "white" :foreground "black" :weight bold))
  "Face for the Agent Input header line.")

;;; Backspace and basic editing

(defconst agent-repl--backspace-commands
  '(evil-delete-backward-char-and-join
    evil-delete-backward-char
    delete-backward-char
    backward-delete-char-untabify)
  "Commands that should be intercepted for backspace handling in the input buffer.")

(defun agent-repl--slash-intercept-backspace ()
  "Intercept backspace in the input buffer for vterm forwarding and slash mode.
In slash mode: redirects to `agent-repl--slash-backspace' via `this-command'.
Outside slash mode: forwards backspace to vterm when the buffer is empty.
Runs as a buffer-local `pre-command-hook'."
  (when (memq this-command agent-repl--backspace-commands)
    (if agent-repl-slash-input-mode
        (progn
          (agent-repl--log-verbose (agent-repl--ws-current-name) "slash-intercept-backspace: slash-mode branch this-command=%s" this-command)
          (setq this-command #'agent-repl--slash-backspace))
      (if (= (buffer-size) 0)
          (if-let ((vterm-buf (agent-repl--current-ws-live-vterm)))
              (progn
                (agent-repl--log-verbose (agent-repl--ws-current-name) "slash-intercept-backspace: empty-buffer-forward sending <backspace> to vterm=%s this-command=%s"
                                          (buffer-name vterm-buf) this-command)
                (with-current-buffer vterm-buf
                  (vterm-send-key "<backspace>")))
            (message "[agent-repl] no live Claude session — backspace not forwarded")
            (agent-repl--log-verbose (agent-repl--ws-current-name) "slash-intercept-backspace: empty-buffer-forward no live vterm, skipping this-command=%s" this-command))
        (agent-repl--log-verbose (agent-repl--ws-current-name) "slash-intercept-backspace: normal branch this-command=%s" this-command)))))

(defcustom agent-repl-input-background-shade 37
  "Greyscale level (0-255) for the input buffer background."
  :type 'integer
  :group 'agent-repl)

;; Input mode
(define-derived-mode agent-repl-input-mode fundamental-mode "Agent Input"
  "Major mode for Agent REPL input buffer."
  (setq-local header-line-format
              (concat "C-c C-c: clear+save | C-c C-k: interrupt | (cmd) <up>/<down>: history | C-r: search history | (ins) <slash>/<digit>/<up>/<down>: direct send"))
  (face-remap-add-relative 'header-line 'agent-repl-header-line)
  (agent-repl--set-buffer-background agent-repl-input-background-shade)
  (visual-line-mode 1)
  ;; Make Evil's line-based operators (yy, dd, cc, Y, D, C) respect
  ;; `visual-line-mode' in this buffer.  These operators dispatch through
  ;; `evil-line-or-visual-line' / `evil-end-of-line-or-visual-line', both of
  ;; which read `evil-respect-visual-line-mode' at runtime, so the buffer-local
  ;; binding flips them to the screen-line variants without affecting other
  ;; buffers.  The motion keys (j/k/0/$/V/^) are bound separately in the
  ;; mode-map below, mirroring the bindings `evil-integration.el' would have
  ;; installed had the variable been set globally before Evil loaded.
  (setq-local evil-respect-visual-line-mode t)
  (add-hook 'after-change-functions #'agent-repl--history-on-change nil t)
  (add-hook 'pre-command-hook #'agent-repl--slash-intercept-backspace nil t))

(defun agent-repl-discard-input ()
  "Save current input to history, clear the buffer, and enter insert state."
  (interactive)
  (agent-repl--log (agent-repl--ws-current-name) "discard-input")
  (when agent-repl-slash-input-mode
    (agent-repl--exit-slash-mode))
  (agent-repl--history-push)
  (agent-repl--history-reset)
  (agent-repl--history-save (agent-repl--ws-current-name))
  (erase-buffer)
  (evil-insert-state))

(defun agent-repl--vterm-send-raw-ctrl-c ()
  "Write a raw ETX byte (0x03, Ctrl-C) to the current workspace's vterm process.
Returns t on success, nil if no live vterm.

Bypasses `vterm-send-key' (which routes through libvterm's key-translation
layer and can dispatch a SIGINT instead of the literal keystroke).  A raw
ETX byte matches what a native terminal sends when the user types Ctrl-C
at a Claude prompt in raw-input mode — which is what actually clears the
input line.  On failure, logs and surfaces an error (no silent fallback)."
  (if-let ((vterm-buf (agent-repl--current-ws-live-vterm)))
      (progn
        (agent-repl--log (agent-repl--ws-current-name) "send-raw-ctrl-c: sending ETX (0x03) to vterm=%s" (buffer-name vterm-buf))
        (with-current-buffer vterm-buf
          (process-send-string vterm--process "\C-c"))
        t)
    (agent-repl--slash-no-vterm-error "send-raw-ctrl-c" "\\C-c")
    nil))

(defun agent-repl-discard-or-send-interrupt ()
  "Clear Claude's prompt AND the local input buffer.
Sends a raw Ctrl-C to Claude (clearing its current input line) and,
if the local input buffer has any content (including whitespace-only),
also discards its contents.  Previously used `string-blank-p' which left
whitespace-only buffers uncleared after C-c C-c.

Exception: when Claude is `:thinking' AND the local input buffer is
non-empty, the raw Ctrl-C is suppressed — only the local buffer is
discarded (and saved to history).  This lets the user draft a message
while Claude is working and clear that draft with C-c C-c without
interrupting Claude's in-flight response.

When slash mode is active (direct-send path), the input buffer is
empty by construction but the slash-stack holds an in-flight command
that the raw Ctrl-C has just aborted on Claude's end — exit slash mode
explicitly so our record of direct sends matches Claude's now-empty
prompt line.  Without this, the next keystroke would continue
forwarding to vterm and the next slash-return posthooks would see
stale accumulated input."
  (interactive)
  (let* ((ws (agent-repl--ws-current-name))
         (local-nonempty (not (zerop (buffer-size))))
         (thinking-p (eq (agent-repl--ws-agent-state ws) :thinking))
         (skip-ctrl-c (and thinking-p local-nonempty)))
    (agent-repl--log ws "discard-or-send-interrupt: clearing Claude prompt + local buffer (local-empty=%s slash-active=%s thinking=%s skip-ctrl-c=%s)"
                      (not local-nonempty)
                      (bound-and-true-p agent-repl-slash-input-mode)
                      thinking-p
                      skip-ctrl-c)
    (cond
     (local-nonempty
      (agent-repl-discard-input))
     ((bound-and-true-p agent-repl-slash-input-mode)
      (agent-repl--exit-slash-mode)))
    (unless skip-ctrl-c
      (agent-repl--vterm-send-raw-ctrl-c))))

;;; Arrow key forwarding (insert-mode terminal navigation)

(defun agent-repl--send-vterm-key (key-name)
  "Send KEY-NAME to the Claude vterm buffer."
  (if-let ((vterm-buf (agent-repl--current-ws-live-vterm)))
      (progn
        (agent-repl--log (agent-repl--ws-current-name) "send-vterm-key: sending %s to vterm=%s" key-name (buffer-name vterm-buf))
        (with-current-buffer vterm-buf
          (vterm-send-key key-name)))
    (message "[agent-repl] no live Claude session — %s not forwarded" key-name)
    (agent-repl--log (agent-repl--ws-current-name) "send-vterm-key: no live vterm, skipping key=%s" key-name)))

(defun agent-repl--send-up-arrow ()
  "Forward up-arrow to vterm for terminal line navigation (insert mode)."
  (interactive)
  (agent-repl--send-vterm-key "<up>"))

(defun agent-repl--send-down-arrow ()
  "Forward down-arrow to vterm for terminal line navigation (insert mode)."
  (interactive)
  (agent-repl--send-vterm-key "<down>"))

;;; Vterm history scrolling (normal-mode C-n / C-p)

(defun agent-repl--send-vterm-down ()
  "Scroll vterm history forward (next item)."
  (interactive)
  (if-let ((vterm-buf (agent-repl--current-ws-live-vterm)))
      (progn
        (agent-repl--log (agent-repl--ws-current-name) "send-vterm-down: sending <down> to vterm=%s" (buffer-name vterm-buf))
        (with-current-buffer vterm-buf
          (vterm-send-down)))
    (message "[agent-repl] no live Claude session — down not forwarded")
    (agent-repl--log (agent-repl--ws-current-name) "send-vterm-down: no live vterm, skipping")))

(defun agent-repl--send-vterm-up ()
  "Scroll vterm history backward (previous item)."
  (interactive)
  (if-let ((vterm-buf (agent-repl--current-ws-live-vterm)))
      (progn
        (agent-repl--log (agent-repl--ws-current-name) "send-vterm-up: sending <up> to vterm=%s" (buffer-name vterm-buf))
        (with-current-buffer vterm-buf
          (vterm-send-up)))
    (message "[agent-repl] no live Claude session — up not forwarded")
    (agent-repl--log (agent-repl--ws-current-name) "send-vterm-up: no live vterm, skipping")))

;; Public aliases -- used in keybindings and tests.
(defalias 'agent-repl-scroll-down #'agent-repl--send-vterm-down)
(defalias 'agent-repl-scroll-up  #'agent-repl--send-vterm-up)

;;; Vterm output scrolling

(defcustom agent-repl-scroll-lines 15
  "Number of lines to scroll per `S-<up>' / `S-<down>' keypress."
  :type 'integer
  :group 'agent-repl)

(defun agent-repl--scroll-vterm-output (lines)
  "Scroll the Claude vterm output window by LINES.
Positive LINES scrolls forward (toward newer output); negative scrolls
backward (toward older output).  Adjusts `window-start' AND
`window-point' of the vterm window — does NOT select it.

Moving `window-point' into the new visible range is what makes upward
scroll go all the way to `point-min'.  Without it, redisplay snaps
`window-start' back down so the WINDOW's point (which mirrors vterm's
bottom-anchored buffer point until otherwise set) remains visible,
capping upward scroll at roughly `(buffer-point - window-height)'.
`NOFORCE' alone does not prevent this — redisplay still re-chooses
`window-start' when the recorded `window-point' is off-screen.  Mouse
wheel events delivered to the vterm window directly do not hit this
cap because `mwheel-scroll' / `scroll-down' move the selected buffer's
point along with `window-start'.

Uses `set-window-point' rather than moving vterm's buffer point so
vterm's prompt cursor stays where vterm expects it; the next vterm
output that calls `vterm-reset-cursor-point' + `set-window-point'
re-synchronizes the window with the prompt as usual.

Selecting vterm even briefly (the old `with-selected-window' approach)
fires `window-selection-change-functions', which schedules
`agent-repl--bounce-from-vterm' that bounces selection back to the
input window and disturbs redisplay.  Going through `set-window-start'
+ `set-window-point' avoids the selection-change entirely."
  (agent-repl--log (agent-repl--ws-current-name) "scroll-vterm-output: lines=%d" lines)
  (agent-repl--with-vterm-buf
   (when-let ((vterm-win (get-buffer-window vterm-buf)))
     (let ((new-start (with-current-buffer vterm-buf
                        (save-excursion
                          (goto-char (window-start vterm-win))
                          (forward-line lines)
                          (point)))))
       (set-window-start vterm-win new-start t)
       (set-window-point vterm-win new-start)))
   ;; Freeze the vterm buffer so subsequent process output from the agent
   ;; doesn't yank the scroll back to the prompt while the user is still
   ;; scrolling.  Scrolling UP (negative LINES) freezes indefinitely so
   ;; the user can read history undisturbed; scrolling DOWN arms a timed
   ;; freeze that lapses and lets auto-scroll-to-bottom resume.
   (agent-repl--vterm-freeze-bump vterm-buf (< lines 0))))

(defun agent-repl-scroll-output-up ()
  "Scroll the Claude vterm output window up (toward older output)."
  (interactive)
  (agent-repl--scroll-vterm-output (- agent-repl-scroll-lines)))

(defun agent-repl-scroll-output-down ()
  "Scroll the Claude vterm output window down (toward newer output)."
  (interactive)
  (agent-repl--scroll-vterm-output agent-repl-scroll-lines))

;; Wheel handlers are identical to scroll-output -- alias them.
(defalias 'agent-repl--input-wheel-up   #'agent-repl-scroll-output-up)
(defalias 'agent-repl--input-wheel-down  #'agent-repl-scroll-output-down)

;;; Single-character confirmations (y/n)

(defmacro agent-repl--define-send-char-command (char)
  "Define an interactive command `agent-repl--send-CHAR' that sends CHAR to Claude."
  (let ((fn-name (intern (format "agent-repl--send-%s" char))))
    `(defun ,fn-name ()
       ,(format "Send \"%s\" to Claude." char)
       (interactive)
       (agent-repl-send-char ,char))))

(agent-repl--define-send-char-command "y")
(agent-repl--define-send-char-command "n")

;;; Keybindings
(map! :map agent-repl-input-mode-map
      :ni "RET"       #'agent-repl--send
      :ni "S-RET"     #'newline
      :i  "/"         #'agent-repl--slash-start
      :ni "C-RET"     #'agent-repl-send-with-postfix
      ;; Deferred-prompt enqueue lives on `SPC j RET' in the leader map
      ;; (see `keybindings.el') so it's reachable from any context with
      ;; one canonical chord, instead of a buffer-local C-S-M-RET tower.
      ;; `C-S-RET' is intentionally NOT bound here -- the global drawer-visit
      ;; override (`agent-repl--install-drawer-visit-override') needs the
      ;; chord to reach `agent-repl-drawer-global-visit' from inside the
      ;; Claude input buffer.  Prefix-send (prepending
      ;; `agent-repl-send-prefix') stays reachable on macOS via `S-s-RET'
      ;; (Doom's `:gn' binding) caught by the `[remap
      ;; +default/newline-above]' entry below.
      [remap +default/newline-below] #'agent-repl-send-with-postfix
      [remap +default/newline-above] #'agent-repl-send-with-prefix
      :ni "C-c C-k"   #'agent-repl-interrupt
      :ni "C-c C-c"   #'agent-repl-discard-or-send-interrupt
      :ni "C-c y"     #'agent-repl--send-y
      :ni "C-c n"     #'agent-repl--send-n
      :ni "C-c r"     #'agent-repl-restart
      :ni "C-c q"     #'agent-repl-kill
      :ni "C-S-m"     #'agent-repl-cycle
      :ni "C-h"       #'evil-window-left
      :n  "C-n"       #'agent-repl--send-vterm-down
      :n  "C-p"       #'agent-repl--send-vterm-up
      :ni "C-v"       #'agent-repl-paste-to-vterm
      :n  "<up>"        #'agent-repl--history-prev
      :n  "<down>"      #'agent-repl--history-next
      :ni "C-r"         #'agent-repl-history-search
      :i  "<up>"        #'agent-repl--send-up-arrow
      :i  "<down>"      #'agent-repl--send-down-arrow
      :ni "S-<up>"      #'agent-repl-scroll-output-up
      :ni "S-<down>"    #'agent-repl-scroll-output-down
      [wheel-up]        #'agent-repl--input-wheel-up
      [wheel-down]      #'agent-repl--input-wheel-down)

;; C-S-0 through C-S-9: send digit to Claude from the input buffer.
;; Named distinctly from keybindings.el's `agent-repl--send-digit-char'
;; (which binds the leader keymap) to avoid a load-order shadowing conflict.
(defun agent-repl--input-send-digit-char ()
  "Send the digit from the current key event to Claude.
Extracts the base digit from `last-command-event' (e.g. C-S-3 -> \"3\")."
  (interactive)
  (let ((digit (number-to-string (- (event-basic-type last-command-event) ?0))))
    (agent-repl--log (agent-repl--ws-current-name) "input-send-digit-char: digit=%s" digit)
    (agent-repl-send-char digit)))

(dotimes (i 10)
  (define-key agent-repl-input-mode-map (kbd (format "C-S-%s" i))
    #'agent-repl--input-send-digit-char))

;; 0-9 in insert mode: if the buffer is empty, enter pass-through mode and
;; forward the digit to vterm; otherwise insert normally.
(defun agent-repl--insert-digit-or-passthrough ()
  "In insert mode, pass digit through to vterm if the buffer is empty.
Otherwise insert the digit normally.  The digit is determined from
`last-command-event'."
  (interactive)
  (agent-repl--log (agent-repl--ws-current-name) "insert-digit-or-passthrough: digit=%s buffer-size=%d" (string last-command-event) (buffer-size))
  (agent-repl--passthrough-start (string last-command-event)))

(dotimes (i 10)
  (evil-define-key 'insert agent-repl-input-mode-map (kbd (number-to-string i))
    #'agent-repl--insert-digit-or-passthrough))

;; Visual-line motion bindings for the Claude input buffer.
;;
;; `visual-line-mode' is always on in `agent-repl-input-mode' (the user is
;; composing free-form prose that wraps), and `evil-respect-visual-line-mode'
;; is flipped on buffer-locally in the mode body so that line-based operators
;; (yy, dd, cc, Y, D, C) operate on screen lines.  Those operators read the
;; variable at runtime, but the basic motion bindings (j/k/0/$/V/^) are
;; installed once at Evil load time by `evil-integration.el' and so they need
;; to be reinstalled here at the mode-map level.  The `g'-prefixed variants
;; give the user the inverse (logical-line) motion when they want it.
;;
;; The bindings are declared as data in `agent-repl--visual-line-bindings'
;; (an alist of `(STATE KEY COMMAND)' triples) and then applied with
;; `evil-define-key' below.  Keeping the data separate lets the tests assert
;; the intended bindings even though `evil-define-key' is a no-op stub in
;; the test harness.
(defconst agent-repl--visual-line-bindings
  '(;; Screen-line motions for normal / motion / visual state.
    (normal  "j"  evil-next-visual-line)
    (motion  "j"  evil-next-visual-line)
    (visual  "j"  evil-next-visual-line)
    (normal  "k"  evil-previous-visual-line)
    (motion  "k"  evil-previous-visual-line)
    (visual  "k"  evil-previous-visual-line)
    (normal  "0"  evil-beginning-of-visual-line)
    (motion  "0"  evil-beginning-of-visual-line)
    (visual  "0"  evil-beginning-of-visual-line)
    (normal  "^"  evil-first-non-blank-of-visual-line)
    (motion  "^"  evil-first-non-blank-of-visual-line)
    (visual  "^"  evil-first-non-blank-of-visual-line)
    (normal  "$"  evil-end-of-visual-line)
    (motion  "$"  evil-end-of-visual-line)
    (visual  "$"  evil-end-of-visual-line)
    ;; Logical-line escape hatches (g-prefixed).
    (normal  "gj" evil-next-line)
    (motion  "gj" evil-next-line)
    (visual  "gj" evil-next-line)
    (normal  "gk" evil-previous-line)
    (motion  "gk" evil-previous-line)
    (visual  "gk" evil-previous-line)
    (normal  "g0" evil-beginning-of-line)
    (motion  "g0" evil-beginning-of-line)
    (visual  "g0" evil-beginning-of-line)
    (normal  "g$" evil-end-of-line)
    (motion  "g$" evil-end-of-line)
    (visual  "g$" evil-end-of-line)
    ;; `V' selects by screen line so it composes with the rest of the family.
    (normal  "V"  evil-visual-screen-line))
  "Visual-line evil bindings installed in `agent-repl-input-mode-map'.
Each entry is `(STATE KEY-STRING COMMAND)'.  Applied via `evil-define-key'
just below.  Declared as data so tests can assert the intended binding set
even though `evil-define-key' is a no-op stub in the test harness.")

(dolist (binding agent-repl--visual-line-bindings)
  (cl-destructuring-bind (state key cmd) binding
    (evil-define-key state agent-repl-input-mode-map (kbd key) cmd)))

;;; Input preparation and metaprompt

(defcustom agent-repl-metaprompt-exempt-strings
  '("/clear" "/usage" "/login" "/logout")
  "Inputs that should never have the metaprompt prepended.
Compared exactly against the trimmed input."
  :type '(repeat string)
  :group 'agent-repl)

(defun agent-repl--skip-metaprompt-p (raw)
  "Return non-nil if RAW input should never have the metaprompt prepended.
Matches `agent-repl-metaprompt-exempt-strings' and bare numerals,
ignoring trailing whitespace."
  (let* ((trimmed (string-trim-right raw))
         (result (or (member trimmed agent-repl-metaprompt-exempt-strings)
                     (string-match-p "^[0-9]+$" trimmed))))
    (agent-repl--log-verbose (agent-repl--ws-current-name) "skip-metaprompt-p: result=%s" result)
    result))

(defvar agent-repl-send-posthooks
  '(("^/clear$" . agent-repl--posthook-reset-prefix-counter)
    ("^/clear$" . agent-repl--posthook-mark-done))
  "Alist of (PATTERN . FUNCTION) posthooks run after input is sent.
PATTERN is a string or regexp matched against the raw input (trimmed).
FUNCTION is called with (WS RAW) where WS is the workspace name and RAW is the input.")

(defun agent-repl--posthook-reset-prefix-counter (ws _raw)
  "Reset the metaprompt prefix counter for workspace WS.
Resets to 0 — the same value a freshly-initialized workspace starts at
\(see `agent-repl--initialize-agent') — so the first send after a
`/clear' re-injects the metaprompt.  A `/clear' wipes Claude's context,
including the previously-prepended guidelines, so the next prompt must
re-establish them exactly as the first prompt of a new session does.
Resetting to 1 instead would skip a full period before re-injecting,
leaving Claude without guidelines in the interim."
  (agent-repl--ws-put ws :prefix-counter 0))

(defun agent-repl--posthook-mark-done (ws _raw)
  "Mark workspace WS's agent-state as :done.
Used by the /clear posthook: clearing Claude's context ends the current
work cycle, so the tab should immediately reflect \"finished\" rather
than linger on whatever state preceded the clear."
  (agent-repl--mark-agent-done ws))

(defun agent-repl--run-send-posthooks (ws raw)
  "Run posthooks matching RAW input for workspace WS."
  (let ((trimmed (string-trim-right raw)))
    (dolist (hook agent-repl-send-posthooks)
      (when (string-match-p (car hook) trimmed)
        (agent-repl--log ws "posthook matched pattern=%s" (car hook))
        (funcall (cdr hook) ws raw)))))

(defun agent-repl--should-prepend-metaprompt-p (raw counter &optional force)
  "Return non-nil if the metaprompt prefix should be prepended to RAW.
COUNTER is the current prefix counter.  FORCE bypasses the counter check."
  (let ((result (and agent-repl-skip-permissions
                     agent-repl-command-prefix
                     (not (agent-repl--skip-metaprompt-p raw))
                     (or force (zerop (mod counter agent-repl-prefix-period))))))
    (agent-repl--log-verbose (agent-repl--ws-current-name) "should-prepend-metaprompt-p: result=%s counter=%d force=%s" result counter force)
    result))

(defun agent-repl--prepare-input (ws raw &optional force-metaprompt)
  "Optionally prepend metaprompt prefix to RAW for workspace WS.
When FORCE-METAPROMPT is non-nil, always prepend (ignoring the counter)."
  (let ((counter (or (agent-repl--ws-get ws :prefix-counter) 0)))
    (agent-repl--log ws "prepare-input counter=%d period=%d" counter agent-repl-prefix-period)
    (if (agent-repl--should-prepend-metaprompt-p raw counter force-metaprompt)
        (concat agent-repl--command-prefix "\n\n" raw)
      raw)))

;;; Send pipeline

(defun agent-repl--vterm-send-return-logged (label)
  "Send Return to the current vterm buffer, logging the attempt under LABEL.
Calls `vterm-send-return' and logs the outcome.  When `vterm--term'
is nil — meaning `vterm-send-return' would silently no-op — logs a
WARNING instead, making this common silent-failure mode visible."
  (if (bound-and-true-p vterm--term)
      (progn
        (agent-repl--log (agent-repl--ws-current-name) "%s: return delivered" label)
        (vterm-send-return))
    (agent-repl--log (agent-repl--ws-current-name) "%s: WARNING — vterm--term is %s, return NOT delivered"
                      label (if (boundp 'vterm--term) "nil" "unbound"))))

(defun agent-repl--vterm-send-return-key-logged (label)
  "Send Enter through libvterm's keyboard path, logging the attempt under LABEL.
Sends the raw CR character (`\\C-m') via `vterm-send-key', which calls
`vterm--update' — the SAME keyboard event path the arrow-key forwards
\(`agent-repl--send-vterm-key') take, so Enter and arrow navigation
share one delivery pipeline and cannot reorder around each other.

CRITICAL: the key MUST be the raw `\\C-m' character, NOT the string
\"<return>\".  vterm-module.c's `term_process_key' recognizes only a
fixed list of named keys (\"<up>\", \"<down>\", \"<tab>\",
\"<backspace>\", …) — \"<return>\" is NOT among them, and unrecognized
names longer than 4 bytes are SILENTLY DROPPED by the UTF-8 fallthrough
guard.  A `vterm-send-key \"<return>\"' call therefore no-ops while
looking exactly like a successful send.  `\\C-m' is a single byte, so
the fallthrough converts it to `vterm_keyboard_unichar' with codepoint
13, which pushes `\\r' to the PTY — the byte a terminal Enter produces.

Used for the empty-input bare-RET branch of `agent-repl--send', the
submission Return of the bracketed-paste pipeline (via
`agent-repl--bracketed-send-return'), the slash-mode submission
Return (`agent-repl--slash-return'), AND the trailing Enter of
single-char sends (`agent-repl-send-char').

When `vterm--term' is nil — meaning `vterm-send-key' would silently
no-op — logs a WARNING instead, making this common silent-failure mode
visible.

Flips `:permission' -> `:thinking' when the return is actually
delivered (a bare RET accepting a permission prompt's default option
is, like every other send, the only available answer signal) — and
deliberately NOT on the warning branch, where nothing reached Claude."
  (if (bound-and-true-p vterm--term)
      (progn
        (agent-repl--log (agent-repl--ws-current-name) "%s: return key (C-m) delivered via libvterm" label)
        (vterm-send-key "\C-m")
        (agent-repl--note-permission-answered-for-vterm))
    (agent-repl--log (agent-repl--ws-current-name) "%s: WARNING — vterm--term is %s, return-key NOT delivered"
                      label (if (boundp 'vterm--term) "nil" "unbound"))))

(defun agent-repl--send-input-direct (vterm-buf input &optional on-settle)
  "Send small INPUT string directly to VTERM-BUF and refresh.
When ON-SETTLE is non-nil, call it after sending — the send is fully
committed with no pending timers, so the callback fires immediately.
Flips `:permission' -> `:thinking' after sending — see
`agent-repl--note-permission-answered-by-send'."
  (agent-repl--log-verbose (agent-repl--ws-current-name) "send-input-direct: len=%d" (length input))
  (with-current-buffer vterm-buf
    (vterm-send-string input)
    (vterm-send-return)
    (agent-repl--refresh-vterm))
  (agent-repl--note-permission-answered-for-vterm vterm-buf)
  (when on-settle (funcall on-settle)))

(defun agent-repl--run-deferred-action (buf action)
  "Execute ACTION in BUF if BUF is still alive, with `inhibit-quit' bound to t.
Called by `run-at-time' as the timer callback for `agent-repl--vterm-deferred-action'."
  (if (buffer-live-p buf)
      (let ((inhibit-quit t))
        (with-current-buffer buf
          (funcall action)))
    (agent-repl--log (agent-repl--ws-current-name) "run-deferred-action: buffer is dead, skipping action=%s" action)))

(defun agent-repl--vterm-deferred-action (buf delay action)
  "Run ACTION in BUF after DELAY seconds, if BUF is still alive.
ACTION is called with `inhibit-quit' bound to t."
  (run-at-time delay nil
               #'agent-repl--run-deferred-action buf action))

(defun agent-repl--bracketed-finalize (&optional on-settle)
  "Send a final Return and refresh vterm after bracketed paste.
Used as the second deferred action in the bracketed paste pipeline.
When ON-SETTLE is non-nil, call it after the finalize is complete."
  (agent-repl--vterm-send-return-logged "bracketed-finalize")
  (agent-repl--refresh-vterm)
  (when on-settle (funcall on-settle)))

(defcustom agent-repl-bracketed-finalize-delay 0.05
  "Seconds between sending Return and the finalize step in bracketed paste."
  :type 'number
  :group 'agent-repl)

(defun agent-repl--bracketed-send-return (vterm-buf &optional on-settle)
  "Send Return to VTERM-BUF and schedule a finalize step.
Used as the first deferred action in the bracketed paste pipeline.
ON-SETTLE, if non-nil, is forwarded to `agent-repl--bracketed-finalize'.

Routes the SUBMISSION Return through
`agent-repl--vterm-send-return-key-logged' (`vterm-send-key \"\\C-m\"',
libvterm's keyboard path) rather than the raw `process-send-string'
path of `agent-repl--vterm-send-return-logged', so the submission
shares one delivery pipeline with every other Enter send.  NOTE: an
earlier revision passed \"<return>\" as the key name, which
vterm-module.c does not recognize and silently drops — during that
window the bracketed pipeline only submitted because the finalize
step's raw Return fired 50ms later; see
`agent-repl--vterm-send-return-key-logged' for the full trap
description.  The secondary Return in
`agent-repl--bracketed-finalize' stays on the raw best-effort path so
the pipeline's double-nudge does not become a guaranteed
double-submit."
  (agent-repl--vterm-send-return-key-logged "bracketed-send-return")
  (agent-repl--vterm-deferred-action
   vterm-buf agent-repl-bracketed-finalize-delay
   (if on-settle
       (lambda () (agent-repl--bracketed-finalize on-settle))
     #'agent-repl--bracketed-finalize)))

(defun agent-repl--send-input-bracketed (vterm-buf input &optional on-settle)
  "Send large INPUT string to VTERM-BUF using bracketed paste mode.
Uses `agent-repl-paste-delay' to wait before sending Return.
When ON-SETTLE is non-nil, it is called after the finalize step
completes — i.e. after all deferred actions have run."
  (agent-repl--log-verbose (agent-repl--ws-current-name) "send-input-bracketed: len=%d" (length input))
  (with-current-buffer vterm-buf
    (vterm-send-string input t)
    (agent-repl--vterm-deferred-action
     vterm-buf agent-repl-paste-delay
     (if on-settle
         (lambda () (agent-repl--bracketed-send-return vterm-buf on-settle))
       (apply-partially #'agent-repl--bracketed-send-return vterm-buf)))))

(defcustom agent-repl-bracketed-paste-threshold 200
  "Input length above which bracketed paste mode is used.
Inputs longer than this are sent via `agent-repl--send-input-bracketed'
to avoid terminal truncation."
  :type 'integer
  :group 'agent-repl)

(defun agent-repl--send-input-to-vterm (vterm-buf input &optional on-settle)
  "Send INPUT string to VTERM-BUF using bracketed paste mode.
Always uses bracketed paste to ensure proper separation between the
character stream and the submission Return, both delivered through
libvterm's keyboard handler via `vterm--update' — the character stream
by `vterm-send-string' and the submission Return by `vterm-send-key
\"\\C-m\"' (see `agent-repl--bracketed-send-return').  A deferred
delay sequences the Return after the paste so the two do not race.
The secondary finalize Return remains on the raw `process-send-string'
best-effort path.  When ON-SETTLE is non-nil, it is called once the
send is fully committed (after all deferred actions complete).

This is the lowest-level funnel for every string send (full sends,
predefined prompts, slash-mode pastes), so it owns
the `:permission' -> `:thinking' flip for all of them — see
`agent-repl--note-permission-answered-by-send'."
  (agent-repl--log-verbose (agent-repl--ws-current-name) "send-input-to-vterm len=%d"
                    (length input))
  (agent-repl--send-input-bracketed vterm-buf input on-settle)
  (agent-repl--note-permission-answered-for-vterm vterm-buf))

(defun agent-repl--mark-ws-thinking (ws)
  "Mark workspace WS as thinking: set agent-state."
  (agent-repl--log ws "mark-ws-thinking ws=%s" ws)
  (agent-repl--ws-set-agent-state ws :thinking))

(defun agent-repl--send-owner-ws (&optional vterm-buf)
  "Resolve the workspace name that owns VTERM-BUF, or the current workspace.
VTERM-BUF defaults to the current buffer.  Shared resolver for the
lowest-level send primitives, which receive only a vterm buffer: prefers
the buffer-local `agent-repl--owning-workspace' pin, falling back to the
current workspace when the buffer carries no owner."
  (or (agent-repl--buffer-owner (or vterm-buf (current-buffer)))
      (agent-repl--ws-current-name)))

(defun agent-repl--mark-send-thinking (ws)
  "Unconditionally mark WS `:thinking' after a non-direct, non-RET send.
Broader companion to `agent-repl--note-permission-answered-by-send':
where that helper flips only from `:permission', this one drives WS to
`:thinking' from ANY prior state.  Called by the input paths that hand
Claude something to act on WITHOUT triggering the `UserPromptSubmit'
hook that owns the `:thinking' transition for full composed sends:

- single-char sends (`agent-repl-send-char', e.g. y/n and digits),
- slash-command submission (`agent-repl--slash-return'),
- per-char slash/digit passthrough forwards (`agent-repl--slash-vterm-send').

Deliberately NOT called on the two excluded classes:

- Directly-sent full prompts (`agent-repl--send-input-to-vterm' and
  friends), whose `:thinking' is the province of the prompt_submit hook.
- A bare RET (`agent-repl--vterm-send-return-key-logged'), which keeps
  the narrower `:permission'-only flip so pressing Enter on an empty
  prompt (or accepting a default) does not spuriously force `:thinking'.

No-op when WS is nil so callers need not pre-check."
  (when ws
    (agent-repl--mark-ws-thinking ws)))

(defun agent-repl--mark-send-thinking-for-vterm (&optional vterm-buf)
  "Unconditionally mark the workspace owning VTERM-BUF `:thinking'.
Resolves the workspace via `agent-repl--send-owner-ws' (VTERM-BUF
defaults to the current buffer) and delegates to
`agent-repl--mark-send-thinking'.  The vterm-buffer-only bridge for the
lowest-level forwards that don't already know their workspace name — see
`agent-repl--mark-send-thinking' for which send paths use it."
  (when-let ((ws (agent-repl--send-owner-ws vterm-buf)))
    (agent-repl--mark-send-thinking ws)))

(defun agent-repl--note-permission-answered-by-send (ws)
  "Flip WS from `:permission' to `:thinking' after a send, if applicable.
Claude Code emits no `UserPromptSubmit' hook when the user answers a
permission prompt, so the Emacs-side keypress is the only available
signal that Claude is now working on the permitted action.

The flip lives in the LOWEST-LEVEL send primitives — the functions
that actually write input to the Claude vterm — so every send path
\(full send, bare RET, single char, slash/digit passthrough chars,
slash RET, predefined prompts, programmatic sends) inherits it
without each entry point having to remember to call it:

- `agent-repl--send-input-to-vterm' (all string sends)
- `agent-repl--slash-vterm-send' (passthrough char forwards)
- `agent-repl--vterm-send-return-key-logged' (bare-RET key path)
- `agent-repl-send-char' (single-char + return sends)
- `agent-repl--slash-return' (submission return finalizing slash mode)

Deliberately NOT flipped: navigation/edit forwards (arrow keys,
backspace, C-v paste-without-submit) and interrupts (Ctrl-C, Escape)
— those interact with the permission dialog without answering it.

Only the `:permission' state is touched — any other state is left
untouched so a normal send does not spuriously force `:thinking'."
  (when (eq (agent-repl--ws-agent-state ws) :permission)
    (agent-repl--mark-ws-thinking ws)))

(defun agent-repl--note-permission-answered-for-vterm (&optional vterm-buf)
  "Flip the workspace owning VTERM-BUF from `:permission' to `:thinking'.
VTERM-BUF defaults to the current buffer (the lowest-level return
senders run inside the vterm buffer).  Resolves the workspace from the
buffer-local `agent-repl--owning-workspace' — pinned at buffer
creation and re-pinned on every full send — falling back to the
current workspace when the buffer carries no owner.  The lowest-level
send primitives receive only a vterm buffer, not a workspace name, so
this is their bridge to `agent-repl--note-permission-answered-by-send'."
  (when-let ((ws (agent-repl--send-owner-ws vterm-buf)))
    (agent-repl--note-permission-answered-by-send ws)))

(defun agent-repl--increment-prefix-counter (ws)
  "Increment the metaprompt prefix counter for workspace WS."
  (let ((new-val (1+ (or (agent-repl--ws-get ws :prefix-counter) 0))))
    (agent-repl--log-verbose ws "increment-prefix-counter: ws=%s new-counter=%d" ws new-val)
    (agent-repl--ws-put ws :prefix-counter new-val)))

(defun agent-repl--pin-owning-workspace (vterm-buf ws)
  "Pin WS as the owning workspace on VTERM-BUF.
Ensures title-change clears the correct workspace even if the
buffer drifts between perspectives."
  (when vterm-buf
    (agent-repl--log-verbose ws "pin-owning-workspace: ws=%s" ws)
    (with-current-buffer vterm-buf
      (setq-local agent-repl--owning-workspace ws))))

(defun agent-repl--do-send (ws input raw &optional on-settle)
  "Core send: dispatch INPUT to WS's vterm.
Increments the prefix counter, pins the owning workspace, sends INPUT,
and runs posthooks with RAW (the undecorated text).
ON-SETTLE, if non-nil, is forwarded to `agent-repl--send-input-to-vterm'
and called once the send is fully committed.

Does NOT write `:agent-state :thinking' — that transition is the
exclusive province of the `prompt_submit' Claude Code hook (routed
through `on-prompt-submit-event').  The brief gap between RET and the
red tab reflects that the hook is the source of truth for Claude's
state.

Exception: a `:permission' → `:thinking' transition happens inside
`agent-repl--send-input-to-vterm' (the lowest-level send primitive
this function dispatches to).  Claude Code does not emit a
`UserPromptSubmit' hook when the user answers a permission prompt, so
the Emacs-side send is the only available signal — see
`agent-repl--note-permission-answered-by-send'.  The owning-workspace
pin below runs BEFORE the send so the primitive resolves the correct
workspace from the vterm buffer."
  (let ((vterm-buf (agent-repl--ws-get ws :vterm-buffer)))
    (agent-repl--log ws "do-send ws=%s len=%d" ws (length input))
    (agent-repl--increment-prefix-counter ws)
    (agent-repl--ws-put ws :last-prompt-time (float-time))
    (agent-repl--pin-owning-workspace vterm-buf ws)
    (agent-repl--send-input-to-vterm vterm-buf input on-settle)
    (agent-repl--run-send-posthooks ws raw)
    (agent-repl--kickoff-prompt-summary ws raw)))

(defun agent-repl--commit-input-buffer (ws input-buf raw &optional clear-p)
  "Record RAW input in history and optionally clear INPUT-BUF.
WS is the workspace name used for history persistence.
When CLEAR-P is non-nil, erase the input buffer after saving history."
  (agent-repl--log ws "commit-input-buffer: ws=%s clear-p=%s" ws clear-p)
  (when (and input-buf (buffer-live-p input-buf))
    (with-current-buffer input-buf
      (agent-repl--history-push raw)
      (agent-repl--history-reset)
      (when clear-p (erase-buffer))))
  (agent-repl--history-save ws))

(defun agent-repl--read-input-buffer (ws)
  "Return the text contents of the input buffer for workspace WS, or nil."
  (agent-repl--log-verbose ws "read-input-buffer: ws=%s" ws)
  (when-let ((buf (agent-repl--ws-get ws :input-buffer)))
    (when (buffer-live-p buf)
      (with-current-buffer buf (buffer-string)))))

(defun agent-repl--send (&optional prompt ws force-metaprompt on-settle)
  "Send PROMPT (or input buffer contents) to Claude in workspace WS.
When PROMPT is nil, reads from the input buffer and clears it after sending.
When WS is nil, uses the current workspace.
When FORCE-METAPROMPT is non-nil, always prepend the metaprompt prefix.
ON-SETTLE, if non-nil, is called once the send is fully committed
\(immediately for direct mode, after deferred actions for bracketed paste).
Handles input preparation, sending, history, and persistence."
  (interactive)
  (let ((ws (or ws (agent-repl--ws-current-name))))
    (unless ws (error "agent-repl--send: no active workspace"))
    (agent-repl--log ws "send: ws=%s force-metaprompt=%s from-buf=%s" ws force-metaprompt (null prompt))
    (let* ((from-buf  (null prompt))
           (input-buf (agent-repl--ws-get ws :input-buffer))
           (vterm-buf (agent-repl--ws-get ws :vterm-buffer))
           (raw       (or prompt (agent-repl--read-input-buffer ws)))
           ;; Empty-string is truthy in Elisp -- guard explicitly so RET on
           ;; an empty input buffer doesn't dispatch a metaprompt-only send.
           (raw-empty (or (null raw) (string-empty-p (string-trim raw)))))
      (unless vterm-buf
        (agent-repl--log ws "send: early return -- no vterm-buf for ws=%s" ws))
      (when (and vterm-buf (not (buffer-live-p vterm-buf)))
        (agent-repl--log ws "send: early return -- vterm-buf is dead for ws=%s" ws))
      (cond
       ;; Empty input on live vterm: send a bare RET to the terminal so
       ;; pressing RET always reaches Claude (useful for permission
       ;; prompts, menus, confirmations).  Skip the full send pipeline
       ;; (metaprompt prefix, counter increment, posthooks, history)
       ;; — there's no input to record.
       ((and raw-empty vterm-buf (buffer-live-p vterm-buf))
        (agent-repl--log ws "send: empty raw input -- forwarding bare RET to vterm via libvterm")
        ;; The :permission -> :thinking flip for a bare RET answering a
        ;; permission prompt happens inside the return-key primitive,
        ;; and only when the return is actually delivered.
        (with-current-buffer vterm-buf
          (agent-repl--vterm-send-return-key-logged "send-empty-bare-ret")))
       ((and (not raw-empty) vterm-buf (buffer-live-p vterm-buf))
        (let ((input (agent-repl--prepare-input ws raw force-metaprompt)))
          (agent-repl--do-send ws input raw on-settle)
          (agent-repl--commit-input-buffer ws input-buf raw from-buf)))))))

(defun agent-repl-send-and-hide ()
  "Send input to Claude and hide both panels."
  (interactive)
  (agent-repl--log (agent-repl--ws-current-name) "send-and-hide")
  (agent-repl--send)
  (agent-repl--on-close))

(defun agent-repl-send-with-metaprompt ()
  "Send input with the metaprompt prefix, bypassing the counter."
  (interactive)
  (agent-repl--log (agent-repl--ws-current-name) "send-with-metaprompt")
  (agent-repl--send nil nil t))

(defun agent-repl--append-to-input-buffer (text)
  "Append TEXT to the end of the current workspace's input buffer."
  (agent-repl--log (agent-repl--ws-current-name) "append-to-input-buffer: len=%d" (length text))
  (let ((buf (agent-repl--ws-get (agent-repl--ws-current-name) :input-buffer)))
    (if buf
        (with-current-buffer buf
          (goto-char (point-max))
          (insert text))
      (message "[agent-repl] WARNING: no input buffer for current workspace — text not appended")
      (agent-repl--log (agent-repl--ws-current-name) "append-to-input-buffer: no input buffer, text discarded"))))

(defun agent-repl-send-with-postfix ()
  "Append `agent-repl-send-postfix' to the input buffer, then send."
  (interactive)
  (agent-repl--log (agent-repl--ws-current-name) "send-with-postfix")
  (agent-repl--append-to-input-buffer agent-repl-send-postfix)
  (agent-repl--send))

(defun agent-repl--prepend-to-input-buffer (text)
  "Prepend TEXT to the start of the current workspace's input buffer."
  (agent-repl--log (agent-repl--ws-current-name) "prepend-to-input-buffer: len=%d" (length text))
  (let ((buf (agent-repl--ws-get (agent-repl--ws-current-name) :input-buffer)))
    (if buf
        (with-current-buffer buf
          (goto-char (point-min))
          (insert text))
      (message "[agent-repl] WARNING: no input buffer for current workspace — text not prepended")
      (agent-repl--log (agent-repl--ws-current-name) "prepend-to-input-buffer: no input buffer, text discarded"))))

(defun agent-repl-send-with-prefix ()
  "Prepend `agent-repl-send-prefix' to the input buffer, then send."
  (interactive)
  (agent-repl--log (agent-repl--ws-current-name) "send-with-prefix")
  (agent-repl--prepend-to-input-buffer agent-repl-send-prefix)
  (agent-repl--send))

(defun agent-repl-queue-deferred-prompt ()
  "Queue the current input buffer contents for delivery when Claude is idle.

Claude's native UI already buffers paste-while-thinking input, but
those buffered keystrokes interleave with whatever else the user
types before Claude finishes its turn — there is no guarantee the
queued text will fire as its own discrete prompt.  This command
provides that guarantee: the input buffer text is captured into
WS's `:deferred-prompts' FIFO and held until Claude reaches `:done'
\(turn finished, Stop hook resolved) or `:idle' (decayed), at which
point `agent-repl--drain-deferred-prompts' pops the head and
delivers it as a standalone prompt.  The queue is arbitrarily long;
each subsequent finished turn drains one entry.

If WS is already `:done' / `:idle' when this command runs, the
enqueue is followed by an immediate drain so a lone queued prompt
fires right away instead of sitting until the next state change.

The input buffer is cleared and the captured text is pushed onto
the input-history ring (the same as a regular send), so the user
can recall it via the history keys."
  (interactive)
  (let* ((ws (agent-repl--ws-current-name))
         (input-buf (agent-repl--ws-get ws :input-buffer))
         (raw (agent-repl--read-input-buffer ws))
         (raw-empty (or (null raw) (string-empty-p (string-trim raw)))))
    (cond
     (raw-empty
      (agent-repl--log ws "queue-deferred-prompt: ws=%s empty input — no-op" ws)
      (message "[agent-repl] no input to queue"))
     (t
      (let* ((prior (agent-repl--ws-get ws :deferred-prompts))
             (new   (append prior (list raw))))
        (agent-repl--ws-put ws :deferred-prompts new)
        (agent-repl--log ws "queue-deferred-prompt: ws=%s enqueued len=%d queue-depth=%d"
                          ws (length raw) (length new))
        (agent-repl--commit-input-buffer ws input-buf raw t)
        (message "[agent-repl] queued prompt #%d for %s (fires when :done/:idle)"
                 (length new) ws)
        (agent-repl--drain-deferred-prompts ws))))))

(defun agent-repl-send-char (char)
  "Send a single character to Claude.
The trailing Enter goes through the shared
`agent-repl--vterm-send-return-key-logged' primitive (`vterm-send-key
\"\\C-m\"', libvterm's keyboard path) so every Enter sender shares one
delivery pipeline, logging, and `vterm--term' guard.

Unconditionally marks the workspace `:thinking' after a successful
send — a single-char send (y/n, digit) is input Claude acts on but
that never fires the `UserPromptSubmit' hook, so the Emacs-side send
is the only `:thinking' signal (see `agent-repl--mark-send-thinking').
The flip lives here, at the `not-directly-sent' entry point, rather
than in the return primitive (whose inherited flip stays the narrower
`:permission'-only one, and only fires when the return is delivered)."
  (let ((ws (agent-repl--ws-current-name)))
    (agent-repl--log ws "send-char: char=%s" char)
    (if-let ((vterm-buf (agent-repl--current-ws-live-vterm)))
        (progn
          (agent-repl--log ws "send-char: sending %s + <return> to vterm=%s" char (buffer-name vterm-buf))
          (with-current-buffer vterm-buf
            (vterm-send-string char)
            (agent-repl--vterm-send-return-key-logged "send-char"))
          (agent-repl--mark-send-thinking ws))
      (message "[agent-repl] no live Claude session — '%s' not sent" char)
      (agent-repl--log ws "send-char: no live vterm, skipping char=%s" char))))

;;; Slash-command pass-through mode
;;
;; When the user types "/" into an empty input buffer, every subsequent
;; keystroke is forwarded directly to vterm without being inserted into the
;; input buffer.  The buffer stays visually empty.  Backspace is forwarded too,
;; and deleting back past the initial "/" exits the mode.

(defvar-local agent-repl--slash-stack nil
  "Stack of characters forwarded to vterm in slash mode.
Each element is the string that was sent (the leading \"/\" is the first entry).
Popped on backspace; when empty the mode exits.")

(define-minor-mode agent-repl-slash-input-mode
  "Minor mode that transparently forwards keystrokes to Claude vterm.
Active when the user begins input with /. The input buffer stays empty;
all characters are sent directly to vterm.

Inhibits `evil-escape' while active — Doom configures evil-escape with
the `jk' key sequence and a 150ms delay, which otherwise causes every
`j' keystroke in slash mode to flutter (held 150ms waiting for a `k'
before being forwarded to vterm)."
  :lighter " /…"
  :keymap (make-sparse-keymap)
  (if agent-repl-slash-input-mode
      (setq-local evil-escape-inhibit t)
    (kill-local-variable 'evil-escape-inhibit)))

(defun agent-repl--slash-on-insert-state-exit ()
  "Exit slash mode when evil leaves insert state (e.g. on ESC).
Installed on `evil-insert-state-exit-hook'.  Runs in every buffer that
leaves insert state, but only acts when the buffer-local
`agent-repl-slash-input-mode' is active — so it's effectively scoped to the
Claude input buffer."
  (when agent-repl-slash-input-mode
    (agent-repl--log (agent-repl--ws-current-name) "slash-on-insert-state-exit: exiting slash mode (evil left insert state)")
    (agent-repl--slash-quit)))

(add-hook 'evil-insert-state-exit-hook #'agent-repl--slash-on-insert-state-exit)

(defun agent-repl--exit-slash-mode ()
  "Clear the slash stack and disable `agent-repl-slash-input-mode'."
  (agent-repl--log (agent-repl--ws-current-name) "exit-slash-mode: stack-depth=%d" (length agent-repl--slash-stack))
  (setq agent-repl--slash-stack nil)
  (agent-repl-slash-input-mode -1))

(defun agent-repl--slash-no-vterm-error (what payload)
  "Log + user-visible error that WHAT couldn't reach vterm (with PAYLOAD).
Per AGENTS.md \"No Silent Fallbacks\": every vterm-forward failure in slash
mode must be surfaced to the user and logged with enough state to diagnose."
  (let* ((ws (agent-repl--ws-current-name))
         (recorded (agent-repl--ws-get ws :vterm-buffer))
         (live (and (bufferp recorded) (buffer-live-p recorded))))
    (agent-repl--log ws "slash-%s: FAILED no live vterm — ws=%s recorded-vterm=%S live=%s payload=%S"
                      what ws recorded live payload)
    (message "agent-repl: cannot forward to Claude — no live vterm in workspace %s"
             (or ws "<none>"))))

(defun agent-repl--slash-vterm-send (str)
  "Send STR to the current workspace's vterm buffer.
Return t on success, nil if there is no live vterm.  Must run the send
inside the vterm buffer via `with-current-buffer' — `vterm-send-string'
reads `vterm--term' buffer-locally and silently no-ops otherwise.
On failure, logs + surfaces a user-visible error.

Unconditionally marks the workspace `:thinking' on every successful
forward — a slash/digit forward is `not directly sent' input Claude
acts on but that never fires the `UserPromptSubmit' hook (see
`agent-repl--mark-send-thinking-for-vterm').  This also covers the
path a bare digit takes when answering a permission prompt (empty
input buffer + digit enters passthrough mode): Claude's permission
dialog commits on that digit IMMEDIATELY, with no RET ever following,
so `agent-repl--slash-return' never fires and the char forward itself
is the only place the answer can be observed."
  (agent-repl--log-verbose (agent-repl--ws-current-name) "slash-vterm-send: str=%S" str)
  (if-let ((vterm-buf (agent-repl--current-ws-live-vterm)))
      (progn
        (with-current-buffer vterm-buf
          (vterm-send-string str))
        (agent-repl--mark-send-thinking-for-vterm vterm-buf)
        t)
    (agent-repl--slash-no-vterm-error "send" str)
    nil))

(defun agent-repl--slash-try-send-and-push (str)
  "Try to send STR to vterm; push onto the slash stack ONLY on success.
Returns t on success, nil on failure.  Per AGENTS.md: we must not mutate
local state (the stack) when the operation it reflects (the forward to
vterm) did not actually happen."
  (if (agent-repl--slash-vterm-send str)
      (progn
        (push str agent-repl--slash-stack)
        (agent-repl--log-verbose (agent-repl--ws-current-name) "slash-try-send-and-push: char=%S stack-depth=%d"
                                  str (length agent-repl--slash-stack))
        t)
    (agent-repl--log (agent-repl--ws-current-name) "slash-try-send-and-push: REFUSED to push char=%S — send failed"
                      str)
    nil))

(defun agent-repl--slash-abort-and-insert (char)
  "Exit slash mode and insert CHAR as a regular self-insert into the input buffer.
Used when an in-flight slash-mode forward fails: we exit the mode (so the
user is no longer trapped), drop the current key into the input buffer (so
nothing is silently discarded), and leave already-forwarded characters in
vterm untouched (no rollback — they've already been sent)."
  (agent-repl--log (agent-repl--ws-current-name) "slash-abort-and-insert: char=%S stack-depth-before-exit=%d"
                    char (length agent-repl--slash-stack))
  (agent-repl--exit-slash-mode)
  (self-insert-command 1 (string-to-char char)))

(defun agent-repl--slash-forward-char ()
  "Forward the typed character to vterm without inserting it into the buffer.
If the forward fails, exit slash mode and drop the character into the
input buffer — never silently discard user input."
  (interactive)
  (let ((char (string last-command-event)))
    (agent-repl--log-verbose (agent-repl--ws-current-name) "slash-forward-char: char=%S" char)
    (unless (agent-repl--slash-try-send-and-push char)
      (agent-repl--slash-abort-and-insert char))))

(defun agent-repl--slash-backspace ()
  "Send backspace to vterm; pop the stack; exit mode when stack is empty.
If the send fails, exit slash mode loudly — do not pop the stack past what
was actually sent."
  (interactive)
  (if-let ((vterm-buf (agent-repl--current-ws-live-vterm)))
      (progn
        (agent-repl--log-verbose (agent-repl--ws-current-name) "slash-backspace: sending <backspace> to vterm=%s" (buffer-name vterm-buf))
        (with-current-buffer vterm-buf
          (vterm-send-key "<backspace>"))
        (pop agent-repl--slash-stack)
        (let ((remaining (length agent-repl--slash-stack)))
          (agent-repl--log-verbose (agent-repl--ws-current-name) "slash-backspace: remaining-depth=%d exiting=%s"
                                    remaining (if (null agent-repl--slash-stack) "t" "nil"))
          (when (null agent-repl--slash-stack)
            (agent-repl--exit-slash-mode))))
    (agent-repl--slash-no-vterm-error "backspace" nil)
    (agent-repl--exit-slash-mode)))

(defun agent-repl--slash-command-string ()
  "Reconstruct the slash command from the slash stack.
The stack is in reverse order (most recent push first), so we reverse
and concatenate.  Tab characters are included as-is."
  (apply #'concat (reverse agent-repl--slash-stack)))

(defcustom agent-repl-workspace-command-prefix "/wor"
  "String prefix that identifies workspace-related slash commands.
Used to detect workspace-generation and workspace-update skills."
  :type 'string
  :group 'agent-repl)

(defun agent-repl--slash-workspace-command-p ()
  "Return non-nil if the current slash stack represents a /wor command.
Used to detect workspace-generation and workspace-update skills so
the source workspace identity can be injected."
  (string-prefix-p agent-repl-workspace-command-prefix (agent-repl--slash-command-string)))

(defun agent-repl--slash-maybe-inject-source-ws ()
  "If the slash command starts with /wor, send the source workspace tag to vterm.
Appends \" [source-ws:<ws-name> path:<project-dir>]\" so the skill knows both
which workspace initiated the generation and the repo root for worktree creation.
Does not push to the slash stack (this is injected text, not user keystrokes).
Signals an error if the current workspace has no :project-dir — the skill
cannot produce a valid git_root without it."
  (when (agent-repl--slash-workspace-command-p)
    (let* ((ws (agent-repl--ws-current-name))
           (dir (or (agent-repl--ws-get ws :project-dir)
                    (error "agent-repl--slash-maybe-inject-source-ws: no :project-dir for workspace %s — cannot inject path" ws))))
      (agent-repl--log ws "slash-maybe-inject-source-ws: injecting source-ws=%s path=%s" ws dir)
      (agent-repl--slash-vterm-send (format " [source-ws:%s path:%s]" ws dir)))))

(defun agent-repl--slash-return ()
  "Send return to vterm and exit slash mode.
For /wor commands, injects a [source-ws:NAME] tag before return so
workspace-generation and workspace-update skills know the originating workspace.
Exits the mode regardless of send outcome — being stuck in slash mode when
vterm is gone is strictly worse than having one unforwarded RET.

If the input buffer is non-empty (e.g. text pasted in while in slash
mode — paste bypasses the `self-insert-command' remap and lands in the
buffer rather than being forwarded char-by-char), the buffer contents
are sent via bracketed paste to vterm BEFORE RET, so the pasted text is
concatenated with the already-forwarded direct-insert characters on
Claude's prompt line and submitted together by the trailing RET.  The
input buffer is cleared after sending.

Runs `agent-repl--run-send-posthooks' against the accumulated
slash-stack (reconstructed via `agent-repl--slash-command-string',
which already reflects backspace pops) so direct-send `/clear' fires
the same posthooks as a buffered-and-sent `/clear'.  Posthooks run
before `agent-repl--exit-slash-mode' clears the stack.

The no-pasted submission Return routes through
`agent-repl--vterm-send-return-key-logged' (`vterm-send-key \"\\C-m\"',
libvterm's keyboard path) so slash submission shares the same delivery
pipeline — and the same logging and `vterm--term' guard — as the
empty-buffer bare-RET branch and the bracketed-paste submission Return
\(`agent-repl--bracketed-send-return').  The wire bytes are equivalent
to the raw `vterm-send-return' this branch previously used; the routing
is for uniformity, not because the raw path was at fault here.

Unconditionally marks the workspace `:thinking' when the submission is
sent — a slash command is `not directly sent' input Claude acts on but
that never fires the `UserPromptSubmit' hook (see
`agent-repl--mark-send-thinking').  Runs before
`agent-repl--run-send-posthooks', so a `/clear' still ends at `:done'
\(the /clear posthook overwrites `:thinking' with `:done').  Both
branches additionally inherit the narrower `:permission'-only flip from
their send primitives (`agent-repl--vterm-send-return-key-logged' and
`agent-repl--send-input-to-vterm' respectively), which is harmless
under the unconditional flip here."
  (interactive)
  (agent-repl--log (agent-repl--ws-current-name) "slash-return: exiting slash mode")
  (agent-repl--slash-maybe-inject-source-ws)
  (let* ((ws (agent-repl--ws-current-name))
         (cmd (agent-repl--slash-command-string))
         (input-buf (agent-repl--ws-get ws :input-buffer))
         (pasted (when (and input-buf (buffer-live-p input-buf))
                   (with-current-buffer input-buf (buffer-string))))
         (has-pasted (and pasted (not (string-empty-p pasted)))))
    (if-let ((vterm-buf (agent-repl--current-ws-live-vterm)))
        (progn
          (agent-repl--log ws "slash-return: sending <return> to vterm=%s cmd=%S has-pasted=%s pasted-len=%d"
                            (buffer-name vterm-buf) cmd has-pasted (length (or pasted "")))
          (if has-pasted
              (progn
                (agent-repl--send-input-to-vterm vterm-buf pasted)
                (with-current-buffer input-buf (erase-buffer)))
            (with-current-buffer vterm-buf
              (agent-repl--vterm-send-return-key-logged "slash-return")))
          (agent-repl--mark-send-thinking ws))
      (agent-repl--slash-no-vterm-error "return" nil))
    (agent-repl--run-send-posthooks ws cmd))
  (agent-repl--exit-slash-mode))

;; Use remaps throughout so evil keymap priority is irrelevant -- remaps are
;; resolved after key->command lookup and apply across all evil states.
(defun agent-repl--slash-tab ()
  "Forward a tab character to vterm in slash mode.
If the forward fails, exit slash mode and insert TAB — see
`agent-repl--slash-forward-char'."
  (interactive)
  (agent-repl--log-verbose (agent-repl--ws-current-name) "slash-tab: forwarding tab")
  (unless (agent-repl--slash-try-send-and-push "\t")
    (agent-repl--slash-abort-and-insert "\t")))

(defun agent-repl--slash-quit ()
  "Emergency escape: exit slash mode without sending anything to vterm.
Bound to C-g so the user can always bail out of slash mode regardless of
vterm state — cheap insurance against any future silent-fallback bugs in
the slash-mode plumbing."
  (interactive)
  (agent-repl--log (agent-repl--ws-current-name) "slash-quit: user-initiated emergency exit stack-depth=%d"
                    (length agent-repl--slash-stack))
  (agent-repl--exit-slash-mode))

(defun agent-repl--passthrough-start (char)
  "Enter slash mode and forward CHAR to vterm, or fail loudly without entering.
Preconditions (empty input buffer AND live vterm for the current workspace)
are checked up front.  On failure we log, surface a user-visible message,
and fall through to normal `self-insert-command' so the character lands in
the input buffer instead of vanishing into a stuck-mode stack.
Per AGENTS.md: no silent fallback, no dropped user input."
  (cond
   ((/= (buffer-size) 0)
    (agent-repl--log (agent-repl--ws-current-name) "passthrough-start: non-empty buffer, inserting normally char=%S" char)
    (self-insert-command 1 (string-to-char char)))
   ((null (agent-repl--current-ws-live-vterm))
    (agent-repl--slash-no-vterm-error "passthrough-start" char)
    (self-insert-command 1 (string-to-char char)))
   (t
    (agent-repl--log (agent-repl--ws-current-name) "passthrough-start: entering slash mode char=%S" char)
    (agent-repl-slash-input-mode 1)
    ;; Race guard: vterm could die between the check above and the send.
    ;; Undo mode entry + insert the char so we never end up in slash mode
    ;; with an empty stack and no way to exit via the normal paths.
    (unless (agent-repl--slash-try-send-and-push char)
      (agent-repl--log (agent-repl--ws-current-name) "passthrough-start: race — vterm died during entry, aborting")
      (agent-repl--exit-slash-mode)
      (self-insert-command 1 (string-to-char char))))))

(defun agent-repl--slash-start ()
  "Enter pass-through mode if the buffer is empty, else insert / normally."
  (interactive)
  (agent-repl--log (agent-repl--ws-current-name) "slash-start: buffer-size=%d" (buffer-size))
  (agent-repl--passthrough-start "/"))

(map! :map agent-repl-slash-input-mode-map
      [remap self-insert-command]                #'agent-repl--slash-forward-char
      [remap indent-for-tab-command]             #'agent-repl--slash-tab
      [remap evil-delete-backward-char-and-join] #'agent-repl--slash-backspace
      [remap delete-backward-char]               #'agent-repl--slash-backspace
      [remap backward-delete-char-untabify]      #'agent-repl--slash-backspace
      [remap agent-repl--send]                   #'agent-repl--slash-return
      [remap keyboard-quit]                       #'agent-repl--slash-quit
      :ni "C-g"    #'agent-repl--slash-quit
      :ni "<up>"   #'ignore
      :ni "<down>" #'ignore)
