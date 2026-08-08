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
;; and only re-scan the last `agent-repl-ai-title-scan-bytes' of the
;; file when mtime changes.  Each ai-title line is small (well under
;; 1KB) so scanning the file tail is sufficient.

;;; Code:

(require 'json)

;; Defined in session.el (loaded before this file).  Declared so
;; byte-compilation doesn't warn about an unknown function.
(declare-function agent-repl--compute-config-dir "session" (project-dir))

;;;; Defcustoms

(defcustom agent-repl-ai-title-enabled t
  "Non-nil to render Claude's aiTitle in the vterm mode-line."
  :type 'boolean
  :group 'agent-repl)

(defcustom agent-repl-ai-title-projects-dir
  (expand-file-name "projects" "~/.claude")
  "Root directory under which Claude stores per-project session jsonl files."
  :type 'directory
  :group 'agent-repl)

(defcustom agent-repl-ai-title-scan-bytes 32768
  "Number of bytes to read from the end of the session jsonl when scanning
for the most recent aiTitle entry.  Each entry is short, so the tail of
the file is enough; reading the whole file on every mode-line refresh
would be wasteful for large transcripts."
  :type 'integer
  :group 'agent-repl)

;;;; Path resolution

(defun agent-repl--ai-title-encode-cwd (path)
  "Encode PATH the way Claude names its per-project subdirectory.
Each `/' and `.' in the absolute path becomes `-'.  Example:
`/Users/me/.config/x' → `-Users-me--config-x'.  Returns nil for nil/empty
input so callers can short-circuit cleanly."
  (when (and (stringp path) (not (string-empty-p path)))
    (replace-regexp-in-string "[/.]" "-" path)))

(defun agent-repl--ai-title-ws-session-id (ws)
  "Return WS's current agent session id, or nil if not available.
Tolerates missing :active-env / instantiation struct (returns nil rather
than signalling) so the mode-line eval is safe to call before a session
has started."
  (ignore-errors
    (let ((inst (agent-repl--active-inst ws)))
      (and inst (agent-repl-instantiation-session-id inst)))))

(defun agent-repl--ai-title-projects-dir-for-ws (ws)
  "Return the Claude projects root for WS honoring its CLAUDE_CONFIG_DIR.
Resolves the per-account config dir via `agent-repl--compute-config-dir'
on WS's `:project-dir', so an alt-account workspace (e.g. one under the
multi-repo root, whose CLI writes transcripts to
`~/.claude-chesscom/projects/') reads `<config-dir>/projects' rather than
the default.  Falls back to `agent-repl-ai-title-projects-dir' (the
default ~/.claude/projects) when WS has no `:project-dir' or when
`agent-repl--compute-config-dir' selects the default account (returns
nil)."
  (let ((project-dir (agent-repl--ws-get ws :project-dir)))
    (if (not project-dir)
        agent-repl-ai-title-projects-dir
      (let ((config-dir (agent-repl--compute-config-dir project-dir)))
        (if config-dir
            (expand-file-name "projects" config-dir)
          agent-repl-ai-title-projects-dir)))))

(defun agent-repl--ai-title-jsonl-path (ws)
  "Return the absolute path to WS's session jsonl, or nil if unavailable.
Returns nil when WS has no project-dir, no session id, or the projects
root doesn't exist on disk — never raises.  The projects root is resolved
per-workspace via `agent-repl--ai-title-projects-dir-for-ws' so it
follows WS's CLAUDE_CONFIG_DIR."
  (let* ((project-dir (agent-repl--ws-get ws :project-dir))
         (session-id (agent-repl--ai-title-ws-session-id ws))
         (encoded (agent-repl--ai-title-encode-cwd project-dir)))
    (when (and project-dir session-id encoded
               (stringp session-id)
               (not (string-empty-p session-id)))
      (expand-file-name (concat session-id ".jsonl")
                        (expand-file-name encoded
                                          (agent-repl--ai-title-projects-dir-for-ws ws))))))

;;;; File reading

(defun agent-repl--ai-title-extract-from-tail (tail)
  "Return the last aiTitle string found in TAIL, or nil.
TAIL is the trailing chunk of a session jsonl (one JSON object per
line).  Walks lines bottom-up so we hit the most recent ai-title entry
first.  Skips lines that don't parse, contain no `aiTitle' field, or
have a non-string `aiTitle' value."
  (if (not (and (stringp tail) (not (string-empty-p tail))))
      (progn
        (agent-repl--log nil
                         "ai-title extract: skipped invalid tail type=%S empty=%s"
                         (type-of tail) (and (stringp tail) (string-empty-p tail)))
        nil)
    (let ((lines (split-string tail "\n" t))
          (found nil)
          (candidate-count 0)
          (parse-error-count 0)
          (rejected-title-count 0))
      (cl-loop for line in (nreverse lines)
               while (not found)
               when (string-match-p "\"type\":\"ai-title\"" line)
               do (progn
                    (cl-incf candidate-count)
                    (let ((parsed
                           (condition-case _err
                               (let ((json-object-type 'alist)
                                     (json-array-type 'list)
                                     (json-key-type 'string)
                                     (json-false nil)
                                     (json-null nil))
                                 (json-read-from-string line))
                             (error
                              (cl-incf parse-error-count)
                              nil))))
                      (let ((title (and (listp parsed)
                                        (cdr (assoc "aiTitle" parsed)))))
                        (if (and (stringp title) (not (string-empty-p title)))
                            (setq found title)
                          (cl-incf rejected-title-count))))))
      (agent-repl--log nil
                       "ai-title extract: scanned tail-bytes=%d lines=%d candidates=%d parse-errors=%d rejected-titles=%d title-found=%s title-length=%s"
                       (string-bytes tail) (length lines) candidate-count
                       parse-error-count rejected-title-count (not (null found))
                       (and found (length found)))
      found)))

(defun agent-repl--ai-title-read-from-jsonl (path)
  "Return the most recent aiTitle string from PATH, or nil.
Reads only the trailing `agent-repl-ai-title-scan-bytes' (via
`agent-repl--transcript-read-tail') so this stays cheap on large
transcripts.  Returns nil on missing/unreadable file or when no
ai-title entry is present in the scanned tail."
  (let* ((tail (agent-repl--transcript-read-tail
                path agent-repl-ai-title-scan-bytes))
         (title (and tail (agent-repl--ai-title-extract-from-tail tail))))
    (agent-repl--log nil
                     "ai-title read: path=%S scan-bytes=%d tail-present=%s tail-bytes=%s title-found=%s title-length=%s"
                     path agent-repl-ai-title-scan-bytes (not (null tail))
                     (and tail (string-bytes tail)) (not (null title))
                     (and title (length title)))
    title))

;;;; Cached lookup

(defun agent-repl--ai-title-mtime (path)
  "Return PATH's mtime as a float, or nil if PATH is missing.
Used as the cache invalidation key — when the jsonl is written to, mtime
advances and we re-scan."
  (when (and path (file-readable-p path))
    (float-time (file-attribute-modification-time (file-attributes path)))))

(defun agent-repl--ai-title-for-ws (ws)
  "Return the conversation title for WS, using a per-workspace mtime cache.
Delegates to `agent-repl--transcript-cached' with the `:ai-title-cache'
key and WS's backend TRANSCRIPT-TITLE-FN reader (this file's
`agent-repl--ai-title-read-from-jsonl' for the claude backend).
Returns nil when no title is available or WS's backend has no
conversation-title capability."
  (agent-repl--transcript-cached
   ws :ai-title-cache #'agent-repl-backend-transcript-title-fn))

;;;; Mode-line segment

(defun agent-repl--ai-title-segment ()
  "Return a propertized string for the mode-line's aiTitle segment.
Reads `agent-repl--owning-workspace' from the current buffer (set on
every agent-owned vterm buffer) and pulls the workspace's aiTitle.
Returns the empty string when disabled, the workspace is unknown, or no
title is yet available."
  ;; This is evaluated on every mode-line redisplay.  Its resolver and cache
  ;; helpers therefore intentionally do not log: even `--log-verbose' would
  ;; write once per redraw and bury lifecycle diagnostics.
  (if (not agent-repl-ai-title-enabled)
      ""
    (let ((ws (agent-repl--buffer-owner (current-buffer))))
      (if (not ws)
          ""
        (let ((title (agent-repl--ai-title-for-ws ws)))
          (if (or (null title) (string-empty-p title))
              ""
            (concat "  "
                    (propertize title
                                'face '(:foreground "medium purple"
                                        :weight normal)))))))))

;;;; Mode-line attachment

(defconst agent-repl--ai-title-mode-line-spec
  '(:eval (agent-repl--ai-title-segment))
  "Trailing `:eval' mode-line segment that paints the aiTitle.
Captured as a constant so the attach helper can detect (via `equal')
whether a buffer's mode-line already contains it.")

(defun agent-repl--ai-title-attach-to-mode-line (buf)
  "Append the ai-title segment to BUF's `mode-line-format' if missing.
Idempotent — does nothing when the segment is already present, the
buffer is dead, or the buffer's mode-line is not a list."
  (if (not (buffer-live-p buf))
      (agent-repl--log nil "ai-title attach: skipped dead buffer=%S" buf)
    (with-current-buffer buf
      (let* ((ws (agent-repl--buffer-owner buf))
             (format-is-list (listp mode-line-format))
             (already-attached
              (and format-is-list
                   (member agent-repl--ai-title-mode-line-spec mode-line-format))))
        (if (and format-is-list (not already-attached))
            (progn
              (setq-local mode-line-format
                          (append mode-line-format
                                  (list agent-repl--ai-title-mode-line-spec)))
              (force-mode-line-update t)
              (agent-repl--log ws
                               "ai-title attach: attached buffer=%S mode-line-length=%d"
                               (buffer-name buf) (length mode-line-format)))
          (agent-repl--log ws
                           "ai-title attach: skipped buffer=%S format-is-list=%s already-attached=%s"
                           (buffer-name buf) format-is-list (not (null already-attached))))))))

(defun agent-repl-ai-title-attach-all ()
  "Attach the ai-title segment to every live workspace vterm buffer.
Run automatically when this file loads so reloading agent-repl upgrades
pre-existing vterm buffers.  Also exposed interactively for manual
recovery."
  (interactive)
  (let ((workspaces (agent-repl--live-ws-names)))
    (agent-repl--log nil "ai-title attach-all: live-workspaces=%S count=%d"
                     workspaces (length workspaces))
    (dolist (ws workspaces)
      (let ((buf (agent-repl--ws-get ws :vterm-buffer)))
        (if (buffer-live-p buf)
            (agent-repl--ai-title-attach-to-mode-line buf)
          (agent-repl--log ws
                           "ai-title attach-all: skipped ws=%s vterm-buffer=%S live=%s"
                           ws buf (buffer-live-p buf)))))))

;; The aiTitle segment is intentionally NOT wired into the vterm
;; mode-line (see `agent-repl--workspace-mode-line'): it was dropped
;; from the status bar.  This file is still loaded because
;; `model.el' and `context.el' reuse its path/mtime resolvers
;; (`agent-repl--ai-title-jsonl-path' / `agent-repl--ai-title-mtime').
;; `agent-repl-ai-title-attach-all' remains defined for manual recovery
;; but is deliberately not run at load, so the segment never re-attaches
;; to existing buffers.

(provide 'agent-repl-ai-title)
;;; ai-title.el ends here
