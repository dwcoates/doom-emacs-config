;;; transcripts.el --- choose which conversation a workspace resumes -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; A workspace's conversations are Claude transcript JSONL files under
;; `<config-dir>/projects/<encoded-cwd>/<uuid>.jsonl'.  Ordinarily the daemon
;; picks one for you: `resume-resolve' takes the newest eligible conversation
;; and continues it.
;;
;; THIS MODULE EXISTS BECAUSE "ELIGIBLE" CAN EXCLUDE EVERYTHING.  A conversation
;; whose newest daemon session record died by `delete session' is excluded from
;; resume permanently, and a workspace torn down and re-created used to end up
;; in exactly that state — its transcripts intact on disk, all of them
;; unreachable, a fresh empty conversation created in their place.  The teardown
;; no longer stamps that reason (see
;; `agent-repl--frontend-release-workspace-session'), but the records it already
;; wrote are durable, so the conversations it stranded need a way back.
;;
;; EXPLICIT RESUME IS THAT WAY BACK.  `resume-resolve' only chooses among
;; candidates for a CONTINUE create; an EXPLICIT create names its conversation
;; outright and is gated solely on the transcript existing and being readable
;; (the daemon's `validateResumeTarget').  So a uuid the resolver refuses is
;; still reachable by asking for it by name, which is what both commands here
;; do.
;;
;; Nothing here deletes or rewrites a transcript.  Reading is the only
;; filesystem access, and choosing a conversation to resume never destroys the
;; one being left behind.

;;; Code:

(require 'seq)

(declare-function agent-repl--ws-get "agent-repl-workspace" (ws key))
(declare-function agent-repl--ws-list-names "agent-repl-workspace" ())
(declare-function agent-repl--ws-current-name "agent-repl-workspace" ())
(declare-function agent-repl--ai-title-encode-cwd "agent-repl-ai-title" (path))
(declare-function agent-repl--ai-title-projects-dir-for-ws "agent-repl-ai-title" (ws))
(declare-function agent-repl--frontend-after-create-session "agent-repl-frontend-client"
                  (cwd model resume-mode explicit-id force-fresh on-success on-failure &optional ws))
(declare-function agent-repl--log "agent-repl-core" (ws fmt &rest args))
(declare-function agent-repl--info "agent-repl-core" (ws fmt &rest args))
(declare-function agent-repl--warn "agent-repl-core" (ws fmt &rest args))

(defcustom agent-repl-transcript-preview-chars 60
  "How much of a transcript's opening prompt the picker shows.
Long enough to tell two conversations apart, short enough that the
candidate line stays readable in the minibuffer."
  :type 'integer
  :group 'agent-repl)

;;;; ---- Discovery -------------------------------------------------------

(defun agent-repl--transcripts-dir (ws)
  "Return WS's transcript directory, or nil when it cannot be resolved.
Honors WS's per-account config dir, so an alt-account workspace reads its
own projects root rather than the default one."
  (let* ((project-dir (agent-repl--ws-get ws :project-dir))
         (encoded (and project-dir (agent-repl--ai-title-encode-cwd project-dir))))
    (when encoded
      (expand-file-name encoded (agent-repl--ai-title-projects-dir-for-ws ws)))))

(defun agent-repl--transcript-first-prompt (path)
  "Return the first user prompt recorded in transcript PATH, or nil.

Read from the FRONT rather than the tail: the opening prompt is what
distinguishes two conversations in the same workspace, while their tails
tend to converge on whatever the agent was last doing.  Reads a bounded
prefix so a hundred-megabyte transcript costs the same as a small one."
  (when (and (stringp path) (file-readable-p path))
    (ignore-errors
      (with-temp-buffer
        (insert-file-contents path nil 0 65536)
        (goto-char (point-min))
        (catch 'found
          (while (not (eobp))
            (let* ((line (buffer-substring-no-properties
                          (line-beginning-position) (line-end-position)))
                   (record (and (string-prefix-p "{" line)
                                (ignore-errors (json-parse-string line :object-type 'alist)))))
              (when record
                (let* ((message (alist-get 'message record))
                       (role (and message (alist-get 'role message)))
                       (content (and message (alist-get 'content message))))
                  (when (and (equal role "user") (stringp content)
                             (not (string-empty-p (string-trim content))))
                    (throw 'found (string-trim content))))))
            (forward-line 1))
          nil)))))

(defun agent-repl--transcript-entry (path)
  "Describe transcript PATH as a plist, or nil when it is not a transcript."
  (let ((uuid (file-name-base path)))
    (when (and (string-suffix-p ".jsonl" path) (file-readable-p path))
      (list :uuid uuid
            :path path
            :mtime (file-attribute-modification-time (file-attributes path))
            :size (file-attribute-size (file-attributes path))
            :preview (agent-repl--transcript-first-prompt path)))))

(defun agent-repl-transcripts-for-workspace (ws)
  "Return WS's known transcripts as plists, most recently modified FIRST.

The ordering is the whole point at the call sites: `most recent' is what
a restore means, and the picker's default should be the conversation the
user was last in."
  (let ((dir (agent-repl--transcripts-dir ws)))
    (when (and dir (file-directory-p dir))
      (let ((entries (delq nil (mapcar #'agent-repl--transcript-entry
                                       (directory-files dir t "\\.jsonl\\'" t)))))
        (sort entries (lambda (a b)
                        (time-less-p (plist-get b :mtime) (plist-get a :mtime))))))))

;;;; ---- Presentation ----------------------------------------------------

(defun agent-repl--transcript-candidate-label (entry)
  "Return the minibuffer label for transcript ENTRY."
  (let* ((preview (or (plist-get entry :preview) "(no recorded prompt)"))
         (clipped (if (> (length preview) agent-repl-transcript-preview-chars)
                      (concat (substring preview 0 agent-repl-transcript-preview-chars) "…")
                    preview))
         ;; Newlines in a prompt would break the candidate into rows the
         ;; completion table cannot select.
         (flat (replace-regexp-in-string "[\n\r]+" " " clipped)))
    (format "%s  %s  %s"
            (format-time-string "%Y-%m-%d %H:%M" (plist-get entry :mtime))
            (substring (plist-get entry :uuid) 0 8)
            flat)))

;;;; ---- Resuming --------------------------------------------------------

(defun agent-repl--resume-transcript (ws entry)
  "Resume transcript ENTRY in WS through an EXPLICIT create.

Explicit rather than continue: the resolver may refuse this uuid (that is
the situation this module exists for), and naming it outright bypasses
the candidate filtering while still being gated on the transcript
existing."
  (let ((cwd (agent-repl--ws-get ws :project-dir))
        (uuid (plist-get entry :uuid)))
    (unless cwd
      (error "agent-repl: workspace %s has no project-dir to resume into" ws))
    (agent-repl--info ws "transcript resume: ws=%s uuid=%s path=%s"
                      ws uuid (plist-get entry :path))
    (agent-repl--frontend-after-create-session
     cwd nil 'explicit uuid nil
     (lambda (id)
       (agent-repl--info ws "transcript resume: ws=%s uuid=%s RESUMED session=%s" ws uuid id)
       (message "[agent-repl] %s resumed %s" ws (substring uuid 0 8)))
     (lambda (detail)
       ;; Loud: a resume the user explicitly asked for that silently did
       ;; nothing would look like the workspace simply ignoring them.
       (agent-repl--warn ws "transcript resume: ws=%s uuid=%s FAILED: %S" ws uuid detail)
       (message "[agent-repl] %s could NOT resume %s: %S" ws (substring uuid 0 8) detail))
     ws)))

;;;; ---- Commands --------------------------------------------------------

;;;###autoload
(defun agent-repl-select-transcript (&optional ws)
  "Pick which recorded conversation WS should resume, and resume it.

Candidates are every transcript on disk for WS, most recent first, each
labelled with its modification time, the head of its uuid, and its
opening prompt.  The current conversation is not treated specially: it is
one candidate among the rest, because switching away from it and back is
the same operation in both directions."
  (interactive)
  (let* ((ws (or ws (agent-repl--ws-current-name)))
         (entries (agent-repl-transcripts-for-workspace ws)))
    (unless entries
      (user-error "agent-repl: no transcripts on disk for %s" ws))
    (let* ((table (mapcar (lambda (e) (cons (agent-repl--transcript-candidate-label e) e))
                          entries))
           (choice (completing-read
                    (format "Resume conversation in %s: " ws)
                    (mapcar #'car table) nil t nil nil (caar table)))
           (entry (cdr (assoc choice table))))
      (unless entry
        (error "agent-repl: no transcript matches %S" choice))
      (agent-repl--resume-transcript ws entry))))

;;;###autoload
(defun agent-repl-restore-latest-transcripts ()
  "Resume the most recent transcript in every open workspace.

For each workspace with a project-dir and at least one transcript on
disk, this resumes the newest one.  A workspace already on its newest
conversation is re-resumed rather than skipped: the daemon may have
stranded it on a fresh empty conversation whose file is newer than the
history the user actually wants, and there is no local way to tell those
apart without asking the daemon.

Reports what it did per workspace, and returns the list of workspace
names it acted on."
  (interactive)
  (let (acted skipped)
    (dolist (ws (agent-repl--ws-list-names))
      (let ((entries (and (agent-repl--ws-get ws :project-dir)
                          (agent-repl-transcripts-for-workspace ws))))
        (if (null entries)
            (push ws skipped)
          (push ws acted)
          (agent-repl--resume-transcript ws (car entries)))))
    (agent-repl--info nil "restore-latest-transcripts: resumed=%d skipped=%d"
                      (length acted) (length skipped))
    (message "[agent-repl] restoring newest transcript in %d workspace%s (%d had none)"
             (length acted) (if (= (length acted) 1) "" "s") (length skipped))
    (nreverse acted)))

(provide 'agent-repl-transcripts)
;;; transcripts.el ends here
