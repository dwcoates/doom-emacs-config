;;; held-edit.el --- editing a held prompt in the composer -*- lexical-binding: t; -*-

;;; Commentary:

;; EDITING A HELD PROMPT (owner spec, 2026-09-23; EditHeldPrompt).
;;
;; The webapp tray card's Edit button BEGINS an edit: the daemon claims the
;; held prompt, and while the claim stands it and every prompt queued after
;; it stay held.  The claim reaches Emacs as STATE on the host view --
;; `HostWorkspace.held_prompt_edit' -- and this file is its one reader:
;;
;;   A NEW EDIT (an edit id this composer has not taken) replaces the
;;   composer's contents with the held prompt's content.  Whatever the user
;;   had typed is first saved to the prompt history exactly as a send saves
;;   it (`agent-repl--history-push' / `-reset' / `-save'), so non-blank words
;;   are never lost and blank ones are not recorded.
;;
;;   WHILE THE EDIT STANDS the composer's ordinary send COMMITS instead of
;;   submitting (`agent-repl-held-edit-commit', called from
;;   `agent-repl--send'): the held prompt's content is replaced, it keeps its
;;   place in the queue and is reclassified daemon-side.  The composer's
;;   discard, `C-c C-c', CANCELS the edit (`agent-repl-held-edit-cancel',
;;   called from `agent-repl-discard-input'): the content is left unchanged
;;   and the queue resumes.
;;
;;   THE FIELD GOING ABSENT ends the edit mode, whatever ended the claim: a
;;   commit, a cancel, the prompt dropped, or this editor's host stream
;;   closing (the daemon scopes the claim to it).  The composer is never
;;   erased on that edge; nothing here decides the claim ended.
;;
;; The edit mode is shown by one mode-line segment on the composer,
;; `agent-repl-held-edit-indicator', and by nothing else.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(declare-function agent-repl--log "core" (ws fmt &rest args))
(declare-function agent-repl--info "core" (ws fmt &rest args))
(declare-function agent-repl--error "core" (ws fmt &rest args))
(declare-function agent-repl--input-buffer "input" (ws))
(declare-function agent-repl--read-input-buffer "input" (ws))
(declare-function agent-repl--input-restore "input" (ws snapshot))
(declare-function agent-repl--input-said-attachments "input" (said))
(declare-function agent-repl--input-fill-said "input" (ws buf said attachments))
(declare-function agent-repl--history-push "history" (&optional text))
(declare-function agent-repl--history-reset "history" ())
(declare-function agent-repl--history-save "history" (&optional ws))
(declare-function agent-repl-host-ref "host" (ws))
(declare-function agent-repl-rpc-edit-held-prompt "rpc" (conn request &rest keys))
(declare-function agent-repl-verbs--conn "verbs" (&optional ws))
(declare-function agent-repl-verbs--send "verbs" (rpc conn request &rest keys))
(declare-function agent-repl-verbs--refusal-arm "verbs" (value))
(defvar agent-repl-verbs--handover-arms)

(defconst agent-repl-held-edit-indicator "editing held prompt"
  "The composer's mode-line indicator while a held-prompt edit stands.")

(defvar-local agent-repl--held-edit nil
  "The held-prompt edit this composer took, or nil when none stands.
A plist `(:turn TURN :edit ID)': TURN is the decoded TurnId a commit or
cancel echoes, ID the daemon-minted edit identity that tells a new edit
from the one already taken.  Buffer-local to the composer, because the
edit is about THIS composer's contents.")

(defconst agent-repl--held-edit-mode-line-spec
  '(:eval (agent-repl--held-edit-segment))
  "The composer's held-prompt edit mode-line segment.")

(defun agent-repl--held-edit-segment ()
  "Return the mode-line indicator text while an edit stands, else \"\"."
  (if agent-repl--held-edit
      (concat " " agent-repl-held-edit-indicator)
    ""))

;;;; ---- State ----------------------------------------------------------

(defun agent-repl-held-edit-state (ws)
  "Return WS's standing held-prompt edit plist, or nil."
  (let ((buf (agent-repl--input-buffer ws)))
    (and buf (buffer-local-value 'agent-repl--held-edit buf))))

(defun agent-repl-held-edit-active-p (ws)
  "Return non-nil when WS's composer is editing a held prompt."
  (and (agent-repl-held-edit-state ws) t))

;;;; ---- Entering and leaving -------------------------------------------

(defun agent-repl--held-edit-save-draft (ws buf)
  "Save BUF's standing draft for WS to history, exactly as a send saves it.
`agent-repl--history-push' skips a blank draft, so only non-whitespace
words are recorded.  Returns non-nil when the draft was non-blank."
  (let ((raw (or (agent-repl--read-input-buffer ws) "")))
    (with-current-buffer buf
      (agent-repl--history-push raw)
      (agent-repl--history-reset))
    (agent-repl--history-save ws)
    (not (string-empty-p (string-trim raw)))))

(defun agent-repl--held-edit-enter (ws edit)
  "Take EDIT, WS's new standing held-prompt edit, into the composer.
The standing draft goes to history first; then the composer is replaced
with the held prompt's content and the edit mode is marked.  With no
composer to edit in, or content no composer can hold, the edit is
cancelled, said, and logged, rather than left claimed by nobody."
  (let* ((buf (agent-repl--input-buffer ws))
         (said (plist-get edit :said))
         (images (agent-repl--input-said-attachments said))
         ;; An attachment the composer cannot hold makes the whole content
         ;; unholdable: an edit that silently lost an image would commit
         ;; the prompt without it.
         (attachments (if (> (plist-get images :dropped) 0)
                          :unrepresentable
                        (plist-get images :attachments)))
         (turn (plist-get (plist-get edit :turn) :value)))
    (cond
     ((null buf)
      (agent-repl--info ws "elisp.held-edit.no-composer ws=%s turn=%s edit=%s" ws turn (plist-get edit :edit))
      (message "agent-repl: no composer is open to edit the held prompt in; the edit is cancelled")
      (agent-repl--held-edit-send ws (plist-get edit :turn) '(:arm :cancel :value nil) "cancel"))
     ((eq attachments :unrepresentable)
      (agent-repl--info ws "elisp.held-edit.unrepresentable ws=%s turn=%s edit=%s" ws turn (plist-get edit :edit))
      (message "agent-repl: the held prompt carries content the composer cannot hold; the edit is cancelled")
      (agent-repl--held-edit-send ws (plist-get edit :turn) '(:arm :cancel :value nil) "cancel"))
     (t
      (let ((saved (agent-repl--held-edit-save-draft ws buf)))
        (agent-repl--input-fill-said ws buf said attachments)
        (with-current-buffer buf
          (setq agent-repl--held-edit (list :turn (plist-get edit :turn) :edit (plist-get edit :edit)))
          (unless (member agent-repl--held-edit-mode-line-spec mode-line-format)
            (setq-local mode-line-format
                        (append mode-line-format (list agent-repl--held-edit-mode-line-spec))))
          (force-mode-line-update))
        (agent-repl--info ws "elisp.held-edit.began ws=%s turn=%s edit=%s draft-saved=%s attachments=%d"
                          ws turn (plist-get edit :edit) saved (length attachments)))))))

(defun agent-repl--held-edit-leave (ws buf)
  "End WS's edit mode in BUF: the daemon states no edit stands any more.
The composer's contents are left exactly as they are."
  (let ((standing (buffer-local-value 'agent-repl--held-edit buf)))
    (with-current-buffer buf
      (setq agent-repl--held-edit nil)
      (force-mode-line-update))
    (agent-repl--info ws "elisp.held-edit.ended ws=%s turn=%s edit=%s"
                      ws (plist-get (plist-get standing :turn) :value) (plist-get standing :edit))))

(defun agent-repl-held-edit-on-host-update (ws host)
  "Reconcile WS's composer with HOST's standing held-prompt edit.
Registered on `agent-repl-host-update-functions'.  A new edit id is taken;
the same id is already taken and changes nothing; an absent edit ends the
edit mode."
  (let* ((edit (plist-get host :held-prompt-edit))
         (buf (agent-repl--input-buffer ws))
         (standing (and buf (buffer-local-value 'agent-repl--held-edit buf))))
    (cond
     ((and edit (equal (plist-get standing :edit) (plist-get edit :edit)))
      (agent-repl--log ws "elisp.held-edit.unchanged ws=%s edit=%s" ws (plist-get edit :edit)))
     (edit (agent-repl--held-edit-enter ws edit))
     (standing (agent-repl--held-edit-leave ws buf))
     (t nil))))

;;;; ---- Commit and cancel -----------------------------------------------

(defun agent-repl--held-edit-on-refusal (ws step value snapshot)
  "Report the daemon's refusal VALUE of WS's edit STEP; restore SNAPSHOT.
Every arm but the two handover ones is CLAIMED here: the composer gets its
words back, and the refusal is logged and said.  A handover arm is left to
the verbs dispatcher's handover path (answering nil)."
  (let* ((arm (agent-repl-verbs--refusal-arm value))
         (keyword (plist-get arm :arm)))
    (agent-repl--input-restore ws snapshot)
    (if (memq keyword agent-repl-verbs--handover-arms)
        nil
      (agent-repl--info ws "elisp.held-edit.refused ws=%s step=%s arm=%S fields=%S"
                        ws step keyword (plist-get arm :value))
      (message "agent-repl: editing the held prompt was refused -- %s"
               (if keyword (substring (symbol-name keyword) 1) "unstated"))
      t)))

(defun agent-repl--held-edit-send (ws turn action step &optional snapshot)
  "Send WS's EditHeldPrompt ACTION on TURN; STEP names it for the log.
SNAPSHOT is the composer as a commit's clear found it, restored on a
refusal or a transport failure so the user's words are never lost."
  (agent-repl--info ws "elisp.held-edit.send ws=%s step=%s turn=%s"
                    ws step (plist-get turn :value))
  (agent-repl-verbs--send
   #'agent-repl-rpc-edit-held-prompt (agent-repl-verbs--conn ws)
   (list :workspace (agent-repl-host-ref ws) :turn turn :action action)
   :ws ws :op "edit-held-prompt"
   :on-success
   (lambda (_)
     (agent-repl--info ws "elisp.held-edit.acked ws=%s step=%s turn=%s"
                       ws step (plist-get turn :value)))
   :on-error
   (lambda (value) (agent-repl--held-edit-on-refusal ws step value snapshot))
   :on-transport-failure
   (lambda (_) (agent-repl--input-restore ws snapshot))))

(defun agent-repl-held-edit-commit (ws said snapshot)
  "Commit SAID as the new content of the held prompt WS's composer edits.
SNAPSHOT is what the send's optimistic clear erased."
  (let ((standing (agent-repl-held-edit-state ws)))
    (agent-repl--info ws "elisp.held-edit.commit ws=%s turn=%s edit=%s blocks=%d"
                      ws (plist-get (plist-get standing :turn) :value) (plist-get standing :edit)
                      (length (plist-get (plist-get said :content) :blocks)))
    (agent-repl--held-edit-send ws (plist-get standing :turn)
                                (list :arm :commit :value (list :said said))
                                "commit" snapshot)))

(defun agent-repl-held-edit-cancel (ws)
  "Cancel the held-prompt edit WS's composer stands in; content unchanged."
  (let ((standing (agent-repl-held-edit-state ws)))
    (agent-repl--info ws "elisp.held-edit.cancel ws=%s turn=%s edit=%s"
                      ws (plist-get (plist-get standing :turn) :value) (plist-get standing :edit))
    (agent-repl--held-edit-send ws (plist-get standing :turn)
                                '(:arm :cancel :value nil) "cancel")))

(add-hook 'agent-repl-host-update-functions #'agent-repl-held-edit-on-host-update)

(provide 'held-edit)

;;; held-edit.el ends here
