;;; ai-title.el --- Claude-emitted aiTitle in vterm mode-line -*- lexical-binding: t; -*-

;;; Commentary:

;; Render Claude's own `aiTitle' (the conversation title Claude writes
;; to its session jsonl under `~/.claude/projects/<encoded-cwd>/<sid>.jsonl')
;; in the vterm mode-line, immediately after the local prompt-summary
;; segment.
;;
;; This complements `prompt-summary.el' (our headless-haiku summary of
;; the user's most recent prompt): the prompt-summary tracks the
;; user's last *send*; the aiTitle tracks Claude's own running label
;; for the *conversation*.  They are deliberately separate because they
;; have different lifecycles and source-of-truth.
;;
;; Mode-line layout (left-to-right): existing segments → prompt-summary
;; → ai-title.
;;
;; Reading the jsonl on every mode-line redraw would re-stat and re-scan
;; a multi-MB file; we cache (mtime, path, title) on the workspace plist
;; and only re-scan the last `claude-repl-ai-title-scan-bytes' of the
;; file when mtime changes.  Each ai-title line is small (well under
;; 1KB) so scanning the file tail is sufficient.

;;; Code:

(require 'json)

;;;; Defcustoms

(defcustom claude-repl-ai-title-enabled t
  "Non-nil to render Claude's aiTitle in the vterm mode-line."
  :type 'boolean
  :group 'claude-repl)

(defcustom claude-repl-ai-title-projects-dir
  (expand-file-name "projects" "~/.claude")
  "Root directory under which Claude stores per-project session jsonl files."
  :type 'directory
  :group 'claude-repl)

(defcustom claude-repl-ai-title-scan-bytes 32768
  "Number of bytes to read from the end of the session jsonl when scanning
for the most recent aiTitle entry.  Each entry is short, so the tail of
the file is enough; reading the whole file on every mode-line refresh
would be wasteful for large transcripts."
  :type 'integer
  :group 'claude-repl)

;;;; Path resolution

(defun claude-repl--ai-title-encode-cwd (path)
  "Encode PATH the way Claude names its per-project subdirectory.
Each `/' and `.' in the absolute path becomes `-'.  Example:
`/Users/me/.config/x' → `-Users-me--config-x'.  Returns nil for nil/empty
input so callers can short-circuit cleanly."
  (when (and (stringp path) (not (string-empty-p path)))
    (replace-regexp-in-string "[/.]" "-" path)))

(defun claude-repl--ai-title-ws-session-id (ws)
  "Return WS's current Claude session id, or nil if not available.
Tolerates missing :active-env / instantiation struct (returns nil rather
than signalling) so the mode-line eval is safe to call before a session
has started."
  (ignore-errors
    (let ((inst (claude-repl--active-inst ws)))
      (and inst (claude-repl-instantiation-session-id inst)))))

(defun claude-repl--ai-title-jsonl-path (ws)
  "Return the absolute path to WS's session jsonl, or nil if unavailable.
Returns nil when WS has no project-dir, no session id, or the projects
root doesn't exist on disk — never raises."
  (let* ((project-dir (claude-repl--ws-get ws :project-dir))
         (session-id (claude-repl--ai-title-ws-session-id ws))
         (encoded (claude-repl--ai-title-encode-cwd project-dir)))
    (when (and project-dir session-id encoded
               (stringp session-id)
               (not (string-empty-p session-id)))
      (expand-file-name (concat session-id ".jsonl")
                        (expand-file-name encoded
                                          claude-repl-ai-title-projects-dir)))))

;;;; File reading

(defun claude-repl--ai-title-extract-from-tail (tail)
  "Return the last aiTitle string found in TAIL, or nil.
TAIL is the trailing chunk of a session jsonl (one JSON object per
line).  Walks lines bottom-up so we hit the most recent ai-title entry
first.  Skips lines that don't parse, contain no `aiTitle' field, or
have a non-string `aiTitle' value."
  (when (and (stringp tail) (not (string-empty-p tail)))
    (let ((lines (split-string tail "\n" t))
          (found nil))
      (cl-loop for line in (nreverse lines)
               while (not found)
               when (string-match-p "\"type\":\"ai-title\"" line)
               do (let ((parsed (ignore-errors
                                  (let ((json-object-type 'alist)
                                        (json-array-type 'list)
                                        (json-key-type 'string)
                                        (json-false nil)
                                        (json-null nil))
                                    (json-read-from-string line)))))
                    (let ((title (and (listp parsed)
                                      (cdr (assoc "aiTitle" parsed)))))
                      (when (and (stringp title) (not (string-empty-p title)))
                        (setq found title)))))
      found)))

(defun claude-repl--ai-title-read-from-jsonl (path)
  "Return the most recent aiTitle string from PATH, or nil.
Reads only the trailing `claude-repl-ai-title-scan-bytes' so this stays
cheap on large transcripts.  Returns nil on missing/unreadable file or
when no ai-title entry is present in the scanned tail."
  (when (and path (file-readable-p path))
    (let* ((size (or (file-attribute-size (file-attributes path)) 0))
           (cap claude-repl-ai-title-scan-bytes)
           (start (max 0 (- size cap))))
      (when (> size 0)
        (with-temp-buffer
          (insert-file-contents path nil start size)
          (claude-repl--ai-title-extract-from-tail (buffer-string)))))))

;;;; Cached lookup

(defun claude-repl--ai-title-mtime (path)
  "Return PATH's mtime as a float, or nil if PATH is missing.
Used as the cache invalidation key — when the jsonl is written to, mtime
advances and we re-scan."
  (when (and path (file-readable-p path))
    (float-time (file-attribute-modification-time (file-attributes path)))))

(defun claude-repl--ai-title-for-ws (ws)
  "Return the aiTitle for WS, using a per-workspace mtime cache.
Cache shape (stored in WS's plist under `:ai-title-cache'):
  (PATH MTIME TITLE)
If the current resolved (path, mtime) matches the cache, returns the
cached TITLE without re-reading.  Otherwise re-scans and refreshes the
cache.  Returns nil when no title is available."
  (let* ((path (claude-repl--ai-title-jsonl-path ws))
         (mtime (claude-repl--ai-title-mtime path))
         (cache (claude-repl--ws-get ws :ai-title-cache)))
    (cond
     ((null path) nil)
     ((null mtime) nil)
     ((and (consp cache)
           (equal (nth 0 cache) path)
           (equal (nth 1 cache) mtime))
      (nth 2 cache))
     (t
      (let ((title (claude-repl--ai-title-read-from-jsonl path)))
        (claude-repl--ws-put ws :ai-title-cache (list path mtime title))
        title)))))

;;;; Mode-line segment

(defun claude-repl--ai-title-segment ()
  "Return a propertized string for the mode-line's aiTitle segment.
Reads `claude-repl--owning-workspace' from the current buffer (set on
every claude-owned vterm buffer) and pulls the workspace's aiTitle.
Returns the empty string when disabled, the workspace is unknown, or no
title is yet available."
  (if (not claude-repl-ai-title-enabled)
      ""
    (let ((ws (and (boundp 'claude-repl--owning-workspace)
                   claude-repl--owning-workspace)))
      (if (not ws)
          ""
        (let ((title (claude-repl--ai-title-for-ws ws)))
          (if (or (null title) (string-empty-p title))
              ""
            (concat "  "
                    (propertize title
                                'face '(:foreground "medium purple"
                                        :weight normal)))))))))

;;;; Mode-line attachment

(defconst claude-repl--ai-title-mode-line-spec
  '(:eval (claude-repl--ai-title-segment))
  "Trailing `:eval' mode-line segment that paints the aiTitle.
Captured as a constant so the attach helper can detect (via `equal')
whether a buffer's mode-line already contains it.")

(defun claude-repl--ai-title-attach-to-mode-line (buf)
  "Append the ai-title segment to BUF's `mode-line-format' if missing.
Idempotent — does nothing when the segment is already present, the
buffer is dead, or the buffer's mode-line is not a list."
  (when (buffer-live-p buf)
    (with-current-buffer buf
      (when (and (listp mode-line-format)
                 (not (member claude-repl--ai-title-mode-line-spec
                              mode-line-format)))
        (setq-local mode-line-format
                    (append mode-line-format
                            (list claude-repl--ai-title-mode-line-spec)))
        (force-mode-line-update t)))))

(defun claude-repl-ai-title-attach-all ()
  "Attach the ai-title segment to every live workspace vterm buffer.
Run automatically when this file loads so reloading claude-repl upgrades
pre-existing vterm buffers.  Also exposed interactively for manual
recovery."
  (interactive)
  (when (and (boundp 'claude-repl--workspaces)
             (hash-table-p claude-repl--workspaces))
    (maphash
     (lambda (_ws plist)
       (let ((buf (plist-get plist :vterm-buffer)))
         (when (and buf (buffer-live-p buf))
           (claude-repl--ai-title-attach-to-mode-line buf))))
     claude-repl--workspaces)))

(claude-repl-ai-title-attach-all)

(provide 'claude-repl-ai-title)
;;; ai-title.el ends here
