;;; held-ingress.el --- the durable held-prompt ingress's writer -*- lexical-binding: t; -*-

;;; Commentary:

;; A PROMPT THE DAEMON DID NOT TAKE IS WRITTEN TO DISK, NEVER KEPT IN MEMORY.
;; Owner ruling, 2026-09-28: held prompts survive outages and restarts of
;; Emacs, the daemon and the shim alike.  So when a submission gets no
;; answer (no daemon, a stuck one) or a handover refusal, the composer hands
;; the prompt HERE, and this file writes it into the daemon-owned ingress
;; at `$AGENT_REPL_STATE_DIR/held-prompts/' -- a directory that needs no
;; live daemon.  The daemon sweeps it at start and every 250ms, submits each
;; entry through SubmitPrompt's own body under the entry's idempotency key,
;; and removes the file only once the queue accepted it
;; (daemon/ARCHITECTURE.md "heldingress").  From there the prompt is an
;; ordinary daemon-held prompt: it shows in the feed's held tray and is
;; delivered by the queue's rules.
;;
;; THE KEY IS THE ATTEMPT'S OWN.  A daemon that was stuck rather than gone
;; may have accepted the submission before Emacs gave up; the entry carries
;; that attempt's key, so the daemon answers it as a duplicate and never
;; delivers it twice.
;;
;; THE WRITE IS ATOMIC: the body goes to a dot-prefixed temporary name,
;; which the daemon's glob does not match, and is renamed into place.
;;
;; THE FILE NAME IS
;;   held_<UTC %Y%m%dT%H%M%S.%N>_<workspace dir hash>_<idempotency key>.json
;; so name order is write order (the daemon ingests in name order) and the
;; composer can count a workspace's waiting prompts from the names alone.
;;
;; THE WAITING LINE.  While a workspace has entries on disk its composer's
;; mode line says "N prompts waiting for the daemon".  It is drawn from the
;; directory, never from memory, so an Emacs restart shows it again as soon
;; as the composer exists.  It is re-counted when an entry is written, when
;; the composer is created, and on every host push while it stands: the
;; daemon re-pushes a workspace's host state right AFTER it removes an
;; ingested entry, so that push is the edge the line goes away on.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(declare-function agent-repl--log "agent-repl-core" (ws fmt &rest args))
(declare-function agent-repl--info "agent-repl-core" (ws fmt &rest args))
(declare-function agent-repl--error "agent-repl-core" (ws fmt &rest args))
(declare-function agent-repl--global-state-file "agent-repl-core" (relative))
(declare-function agent-repl--ws-dir-hash-cached "agent-repl-core" (ws))
(declare-function agent-repl--ws-get "agent-repl-workspace" (ws key))
(declare-function agent-repl--input-set-waiting "agent-repl-input" (ws text))
(declare-function agent-repl--input-waiting "agent-repl-input" (ws))
(declare-function agent-repl-wire-encode-user-said "agent-repl-wire-common" (value))
(declare-function agent-repl-wire-encode-prompt-origin "agent-repl-wire-common" (value))

(defvar agent-repl-host-update-functions)

(defconst agent-repl-held-ingress-format-version 1
  "The entry format the daemon's held-prompt ingress reads.")

(defconst agent-repl-held-ingress--key-regexp "\\`[A-Za-z0-9-]+\\'"
  "What an idempotency key must look like to ride in a file name.
Every key Emacs mints is a UUID; anything else would make the name
ambiguous to the counting below, so it is refused rather than escaped.")

(defun agent-repl-held-ingress-dir ()
  "Return the held-prompt ingress directory, with a trailing slash.
Resolved on every call, never baked at load: the state root follows
`AGENT_REPL_STATE_DIR' at the moment of the write."
  (file-name-as-directory (agent-repl--global-state-file "held-prompts")))

(defun agent-repl-held-ingress--hash (ws)
  "Return WS's directory hash, or signal: an entry must name its workspace."
  (or (agent-repl--ws-dir-hash-cached ws)
      (error "agent-repl held-ingress: workspace %s has no project directory" ws)))

(defun agent-repl-held-ingress--name-regexp (hash &optional key)
  "Return the regexp matching HASH's entry names; KEY's alone when given."
  (format "\\`held_[0-9T.]+_%s_%s\\.json\\'"
          (regexp-quote hash)
          (if key (regexp-quote key) "[A-Za-z0-9-]+")))

(defun agent-repl-held-ingress-entries (ws)
  "Return WS's entry files still waiting in the ingress, oldest first."
  (let ((dir (agent-repl-held-ingress-dir))
        (hash (agent-repl--ws-dir-hash-cached ws)))
    (and hash (file-directory-p dir)
         (sort (directory-files dir t (agent-repl-held-ingress--name-regexp hash) t)
               #'string<))))

(defun agent-repl-held-ingress-waiting (ws)
  "Return how many of WS's prompts wait in the ingress for the daemon."
  (length (agent-repl-held-ingress-entries ws)))

(defun agent-repl-held-ingress--waiting-text (count)
  "Return the composer's waiting line for COUNT prompts, nil for none."
  (cond
   ((zerop count) nil)
   ((= count 1) "1 prompt waiting for the daemon")
   (t (format "%d prompts waiting for the daemon" count))))

(defun agent-repl-held-ingress-refresh (ws)
  "Re-count WS's waiting prompts and redraw its composer's waiting line.
Returns the count."
  (let* ((count (agent-repl-held-ingress-waiting ws))
         (text (agent-repl-held-ingress--waiting-text count))
         (shown (agent-repl--input-waiting ws)))
    (if (equal text shown)
        (agent-repl--log ws "elisp.held-ingress.waiting ws=%s count=%d unchanged" ws count)
      (agent-repl--info ws "elisp.held-ingress.waiting ws=%s count=%d was=%S" ws count shown)
      (agent-repl--input-set-waiting ws text))
    count))

(defun agent-repl-held-ingress--body (ws said origin key)
  "Return the JSON body of WS's entry for SAID under ORIGIN and KEY.
`json-serialize' answers UTF-8 BYTES; they are decoded to text here so
the file is written as the UTF-8 it already is, never re-encoded byte by
byte."
  (decode-coding-string
   (json-serialize
   `((version . ,agent-repl-held-ingress-format-version)
     (project_dir . ,(directory-file-name
                      (expand-file-name (agent-repl--ws-get ws :project-dir))))
     (idempotency_key . ,key)
     (origin . ,(agent-repl-wire-encode-prompt-origin origin))
     (said . ,(agent-repl-wire-encode-user-said said))
     (queued_at . ,(format-time-string "%Y-%m-%dT%H:%M:%S.%NZ" nil t))))
   'utf-8))

(defun agent-repl-held-ingress-write (ws said origin key)
  "Write SAID, submitted for WS under ORIGIN and KEY, into the ingress.
Returns the entry's path.  An entry already written under KEY for WS is
left as it is and its path returned: one attempt is one prompt, however
many failure paths report it.  A failure to write SIGNALS -- the caller
owns telling the user their words were not saved."
  (unless (and (stringp key) (string-match-p agent-repl-held-ingress--key-regexp key))
    (error "agent-repl held-ingress: key %S cannot name an entry" key))
  (let* ((dir (agent-repl-held-ingress-dir))
         (hash (agent-repl-held-ingress--hash ws))
         (existing (and (file-directory-p dir)
                        (car (directory-files
                              dir t (agent-repl-held-ingress--name-regexp hash key) t)))))
    (if existing
        (progn
          (agent-repl--info ws "elisp.held-ingress.already-written ws=%s key=%s file=%s"
                            ws key existing)
          existing)
      (let* ((name (format "held_%s_%s_%s.json"
                           (format-time-string "%Y%m%dT%H%M%S.%N" nil t) hash key))
             (path (expand-file-name name dir))
             (tmp (expand-file-name (concat "." name ".tmp") dir))
             (body (agent-repl-held-ingress--body ws said origin key)))
        (make-directory dir t)
        (let ((coding-system-for-write 'utf-8-unix))
          (with-temp-file tmp
            (insert body)))
        (rename-file tmp path)
        (agent-repl--info ws "elisp.held-ingress.written ws=%s key=%s origin=%S file=%s"
                          ws key origin path)
        (agent-repl-held-ingress-refresh ws)
        path))))

(defun agent-repl-held-ingress--on-host-update (ws _host)
  "Re-count WS's waiting prompts on a host push, while any are shown.
The daemon re-pushes a workspace's host state right after it removes an
ingested entry, so this is the edge the line clears on.

AN OPTIMIZATION: a composer showing nothing has nothing to clear, and only
this file writes entries (re-counting as it does), so such a push is
skipped without listing the directory.  Host pushes are frequent; the
listing is only paid while a line stands."
  (when (agent-repl--input-waiting ws)
    (agent-repl-held-ingress-refresh ws)))

(add-hook 'agent-repl-host-update-functions #'agent-repl-held-ingress--on-host-update)

(provide 'held-ingress)

;;; held-ingress.el ends here
