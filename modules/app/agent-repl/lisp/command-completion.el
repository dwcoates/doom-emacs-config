;;; command-completion.el --- slash-command completion in the composer -*- lexical-binding: t; -*-

;;; Commentary:

;; A WORD STARTING WITH `/' IN THE COMPOSER COMPLETES AGAINST THE WORKSPACE'S
;; RUNTIME.  The candidates are every slash command the workspace's agent
;; runtime answers -- its built-ins and every skill, custom command, plugin
;; command and MCP prompt the user added to it -- exactly as the runtime
;; itself listed them.  Nothing here knows a command by name: the closed set
;; the daemon recognizes is a different, smaller thing.
;;
;; WHERE THE LIST COMES FROM.  The shim reads the runtime's command list and
;; states it on its session stream; the daemon keeps the last statement per
;; workspace and answers it on `ListSlashCommands'.  This file asks for it
;; and keeps the answer buffer-locally in the composer.
;;
;; WHEN IT ASKS.  Each time the user types a slash that begins a word -- a
;; bare `/' the composer has not asked for at this edit -- so a skill added
;; mid-session is offered from the next slash word on.  A composer that has
;; never asked also asks for a longer slash word (one pasted whole).  The ask
;; is asynchronous: completion is served from the catalog already in hand
;; and never waits on the daemon.  Company asks this function for its prefix
;; on the bare `/', before its minimum prefix length lets it draw anything,
;; so the answer is normally in hand by the word's second character.
;;
;; THE COMPLETION IS NON-EXCLUSIVE.  `/usr/local' or any slash word that
;; names no command falls through to the next completion function.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(declare-function agent-repl--buffer-owner "agent-repl-core" (buf))
(declare-function agent-repl--log "agent-repl-core" (ws fmt &rest args))
(declare-function agent-repl--warn "agent-repl-core" (ws fmt &rest args))
(declare-function agent-repl--error "agent-repl-core" (ws fmt &rest args))
(declare-function agent-repl-host-ref "agent-repl-host" (ws))
(declare-function agent-repl-host-conn-or-primary "agent-repl-host" (ws))
(declare-function agent-repl-host-route-handover "agent-repl-host" (ws arm slug))
(declare-function agent-repl-rpc-list-slash-commands "agent-repl-rpc" (conn request &rest keys))

(defvar-local agent-repl--command-completion-catalog nil
  "The slash commands this composer completes, or nil when none are known.
A list of decoded `SessionCommandCatalogEntry' plists, in the runtime's
order, as the daemon last answered them.")

(defvar-local agent-repl--command-completion-asked-at nil
  "The edit the composer last asked for its catalog at, or nil if it never has.
A cons (START . TICK): the bare slash word's start and the buffer's
`buffer-chars-modified-tick' when it was typed.  Typing a slash again --
even at the same position, as every prompt after a send does -- is a new
TICK, and so a new ask.")

;;;; ---- The slash word at point ----

(defun agent-repl--command-completion-bounds ()
  "Return (START . END) of the slash word around point, or nil.
A slash word is a run of non-whitespace that begins with `/' at the start
of a line or after whitespace, and point sits inside it or at its end."
  (save-excursion
    (let* ((here (point))
           (start (progn (skip-chars-backward "^[:space:]") (point)))
           (end (progn (goto-char here) (skip-chars-forward "^[:space:]") (point))))
      (when (and (< start end)
                 (eq (char-after start) ?/))
        (cons start end)))))

;;;; ---- Candidates ----

(defun agent-repl--command-completion-candidates (catalog)
  "Return the completion strings CATALOG offers: each name and alias, slashed."
  (cl-loop for entry in catalog
           append (cons (concat "/" (plist-get entry :name))
                        (mapcar (lambda (alias) (concat "/" alias))
                                (plist-get entry :aliases)))))

(defun agent-repl--command-completion-entry (catalog candidate)
  "Return the CATALOG entry CANDIDATE names, as its name or an alias, or nil."
  (let ((name (string-remove-prefix "/" candidate)))
    (cl-find-if (lambda (entry)
                  (or (equal (plist-get entry :name) name)
                      (member name (plist-get entry :aliases))))
                catalog)))

(defun agent-repl--command-completion-annotation (catalog candidate)
  "Return CANDIDATE's annotation from CATALOG: its argument hint, then description.
Empty when the runtime stated neither."
  (let* ((entry (agent-repl--command-completion-entry catalog candidate))
         (parts (delq nil (list (plist-get entry :argument-hint)
                                (plist-get entry :description)))))
    (if parts (concat " " (string-join parts "  ")) "")))

(defun agent-repl--command-completion-description (catalog candidate)
  "Return CANDIDATE's description from CATALOG, or nil when it states none."
  (plist-get (agent-repl--command-completion-entry catalog candidate) :description))

;;;; ---- Asking the daemon ----

(defun agent-repl--command-completion-answered (ws buf response)
  "Install RESPONSE, a decoded `ListSlashCommandsResponse', into composer BUF.
WS names the workspace for the log.  A refusal leaves the catalog in hand
as it stood and is logged; a handover arm is walked by host.el."
  (pcase (plist-get response :arm)
    (:success
     (let ((catalog (plist-get response :value)))
       (pcase (plist-get catalog :arm)
         (:stated
          (let ((commands (plist-get (plist-get (plist-get catalog :value) :catalog) :commands)))
            (agent-repl--log ws "elisp.command-completion.answered ws=%s outcome=stated commands=%d"
                             ws (length commands))
            (when (buffer-live-p buf)
              (with-current-buffer buf
                (setq agent-repl--command-completion-catalog commands)))))
         (:unstated
          (agent-repl--log ws "elisp.command-completion.answered ws=%s outcome=unstated" ws)
          (when (buffer-live-p buf)
            (with-current-buffer buf
              (setq agent-repl--command-completion-catalog nil))))
         (other
          (agent-repl--error ws "elisp.command-completion.unknown-catalog-arm ws=%s arm=%S"
                             ws other)))))
    (:error
     (let ((cause (plist-get (plist-get response :value) :cause)))
       (unless (agent-repl-host-route-handover
                ws cause "elisp.command-completion.handover-refusal")
         (agent-repl--warn ws "elisp.command-completion.refused ws=%s cause=%S" ws cause))))
    (other
     (agent-repl--error ws "elisp.command-completion.unknown-response-arm ws=%s arm=%S"
                        ws other))))

(defun agent-repl-command-completion-refresh (ws)
  "Ask the daemon for WS's slash commands, for WS's composer (the current buffer).
Asynchronous: the answer replaces the composer's catalog when it arrives.
A workspace the daemon has not registered, or no connection, asks nothing
and says so in the log -- the next slash word asks again."
  (let ((ref (agent-repl-host-ref ws))
        (conn (agent-repl-host-conn-or-primary ws))
        (buf (current-buffer)))
    (cond
     ((null ref)
      (agent-repl--log ws "elisp.command-completion.refresh-skipped ws=%s reason=no-ref" ws))
     ((null conn)
      (agent-repl--log ws "elisp.command-completion.refresh-skipped ws=%s reason=no-connection" ws))
     (t
      (agent-repl--log ws "elisp.command-completion.refresh ws=%s" ws)
      (agent-repl-rpc-list-slash-commands
       conn (list :workspace ref)
       :on-response (lambda (response)
                      (agent-repl--command-completion-answered ws buf response))
       :on-failure (lambda (detail)
                     (agent-repl--warn ws "elisp.command-completion.transport-failure ws=%s detail=%S"
                                       ws detail)))))))

;;;; ---- completion-at-point ----

(defun agent-repl--command-completion-ask-key (bounds)
  "Return the ask key for the slash word at BOUNDS, or nil when it asks nothing.
The key of a bare `/' is (START . TICK); a longer slash word asks only in a
composer that has never asked, keyed the same way."
  (when (or (= (- (cdr bounds) (car bounds)) 1)
            (null agent-repl--command-completion-asked-at))
    (cons (car bounds) (buffer-chars-modified-tick))))

(defun agent-repl-command-completion-at-point ()
  "Complete the slash word at point against the workspace's slash commands.
The composer's `completion-at-point-functions' entry.  Asks the daemon
again when a slash has just been typed (see
`agent-repl--command-completion-ask-key'), and answers from the catalog
in hand.  Deliberately unlogged per call:
completion runs per keystroke."
  (when-let* ((bounds (agent-repl--command-completion-bounds))
              (ws (agent-repl--buffer-owner (current-buffer))))
    (let ((key (agent-repl--command-completion-ask-key bounds)))
      (when (and key (not (equal key agent-repl--command-completion-asked-at)))
        (setq agent-repl--command-completion-asked-at key)
        (agent-repl-command-completion-refresh ws)))
    (let ((catalog agent-repl--command-completion-catalog))
      (list (car bounds) (cdr bounds)
            (agent-repl--command-completion-candidates catalog)
            :exclusive 'no
            :annotation-function
            (lambda (candidate) (agent-repl--command-completion-annotation catalog candidate))
            :company-docsig
            (lambda (candidate) (agent-repl--command-completion-description catalog candidate))))))

(provide 'command-completion)
;;; command-completion.el ends here
