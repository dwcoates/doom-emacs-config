;;; test-ai-title.el --- Tests for ai-title.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for Claude's aiTitle mode-line segment: path encoding, jsonl
;; tail scanning, mtime-keyed cache, segment formatting, and mode-line
;; attachment.

;;; Code:

(load (expand-file-name "test-helpers.el" (file-name-directory
                                            (or load-file-name buffer-file-name)))
      nil t)

;;;; ---- Tests: encode-cwd ----

(ert-deftest agent-repl-test-ai-title-encode-cwd-basic ()
  "Each `/' becomes `-'."
  (should (equal (agent-repl--ai-title-encode-cwd "/a/b/c") "-a-b-c")))

(ert-deftest agent-repl-test-ai-title-encode-cwd-dot ()
  "Each `.' becomes `-' (so `.config' renders as `-config')."
  (should (equal (agent-repl--ai-title-encode-cwd "/Users/me/.config/x")
                 "-Users-me--config-x")))

(ert-deftest agent-repl-test-ai-title-encode-cwd-nil ()
  "Nil/empty input returns nil so callers can short-circuit."
  (should-not (agent-repl--ai-title-encode-cwd nil))
  (should-not (agent-repl--ai-title-encode-cwd "")))

;;;; ---- Tests: jsonl-path resolution ----

(ert-deftest agent-repl-test-ai-title-jsonl-path-builds-from-state ()
  "Path is `<projects-root>/<encoded-cwd>/<session-id>.jsonl'."
  (agent-repl-test--with-clean-state
    (let ((agent-repl-ai-title-projects-dir "/tmp/projects-fake"))
      (agent-repl--ws-put "ws1" :project-dir "/Users/me/.config/x")
      (cl-letf (((symbol-function 'agent-repl--ai-title-ws-session-id)
                 (lambda (_ws) "sid-abc")))
        (should (equal (agent-repl--ai-title-jsonl-path "ws1")
                       "/tmp/projects-fake/-Users-me--config-x/sid-abc.jsonl"))))))

(ert-deftest agent-repl-test-ai-title-jsonl-path-nil-when-no-project-dir ()
  "Returns nil when WS has no :project-dir."
  (agent-repl-test--with-clean-state
    (cl-letf (((symbol-function 'agent-repl--ai-title-ws-session-id)
               (lambda (_ws) "sid-abc")))
      (should-not (agent-repl--ai-title-jsonl-path "ws1")))))

(ert-deftest agent-repl-test-ai-title-jsonl-path-nil-when-no-session-id ()
  "Returns nil when no session id is resolvable."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws1" :project-dir "/Users/me/x")
    (cl-letf (((symbol-function 'agent-repl--ai-title-ws-session-id)
               (lambda (_ws) nil)))
      (should-not (agent-repl--ai-title-jsonl-path "ws1")))))

(ert-deftest agent-repl-test-ai-title-jsonl-path-nil-when-empty-session-id ()
  "Returns nil when session id is an empty string."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws1" :project-dir "/Users/me/x")
    (cl-letf (((symbol-function 'agent-repl--ai-title-ws-session-id)
               (lambda (_ws) "")))
      (should-not (agent-repl--ai-title-jsonl-path "ws1")))))

;;;; ---- Tests: projects-dir CLAUDE_CONFIG_DIR resolution ----

(ert-deftest agent-repl-test-ai-title-projects-dir-under-multi-repo ()
  "projects-dir-for-ws resolves to <multi-repo-config-dir>/projects under the root."
  (agent-repl-test--with-clean-state
    (let ((process-environment (cons "MULTI_REPO_ROOT=/home/user/multi" process-environment))
          (agent-repl-multi-repo-config-dir "~/.claude-chesscom"))
      (agent-repl--ws-put "ws1" :project-dir "/home/user/multi/repoA/proj")
      (should (equal (agent-repl--ai-title-projects-dir-for-ws "ws1")
                     (expand-file-name "projects"
                                       (expand-file-name "~/.claude-chesscom")))))))

(ert-deftest agent-repl-test-ai-title-projects-dir-outside-multi-repo ()
  "projects-dir-for-ws falls back to the default projects dir outside the root."
  (agent-repl-test--with-clean-state
    (let ((process-environment (cons "MULTI_REPO_ROOT=/home/user/multi" process-environment))
          (agent-repl-default-config-dir nil)
          (agent-repl-ai-title-projects-dir "/tmp/projects-default"))
      (agent-repl--ws-put "ws1" :project-dir "/home/user/other/proj")
      (should (equal (agent-repl--ai-title-projects-dir-for-ws "ws1")
                     "/tmp/projects-default")))))

(ert-deftest agent-repl-test-ai-title-projects-dir-no-project-dir ()
  "projects-dir-for-ws falls back to the default when WS has no :project-dir."
  (agent-repl-test--with-clean-state
    (let ((agent-repl-ai-title-projects-dir "/tmp/projects-default"))
      (should (equal (agent-repl--ai-title-projects-dir-for-ws "ws1")
                     "/tmp/projects-default")))))

(ert-deftest agent-repl-test-ai-title-jsonl-path-follows-config-dir ()
  "jsonl-path builds under the resolved alt projects dir for a multi-repo workspace."
  (agent-repl-test--with-clean-state
    (let ((process-environment (cons "MULTI_REPO_ROOT=/home/user/multi" process-environment))
          (agent-repl-multi-repo-config-dir "/cc"))
      (agent-repl--ws-put "ws1" :project-dir "/home/user/multi/proj")
      (cl-letf (((symbol-function 'agent-repl--ai-title-ws-session-id)
                 (lambda (_ws) "sid-xyz")))
        (should (equal (agent-repl--ai-title-jsonl-path "ws1")
                       "/cc/projects/-home-user-multi-proj/sid-xyz.jsonl"))))))

;;;; ---- Tests: ws-session-id tolerates missing instantiation ----

(ert-deftest agent-repl-test-ai-title-ws-session-id-returns-nil-when-no-inst ()
  "Returns nil rather than signalling when WS has no instantiation —
mode-line eval must never throw."
  (agent-repl-test--with-clean-state
    ;; No :active-env / inst configured for ws-missing.
    (should-not (agent-repl--ai-title-ws-session-id "ws-missing"))))

;;;; ---- Tests: extract-from-tail ----

(ert-deftest agent-repl-test-ai-title-extract-from-tail-finds-title ()
  "Returns the aiTitle string from a tail that contains one entry."
  (should (equal (agent-repl--ai-title-extract-from-tail
                  "{\"type\":\"ai-title\",\"aiTitle\":\"Hello World\",\"sessionId\":\"s\"}")
                 "Hello World")))

(ert-deftest agent-repl-test-ai-title-extract-from-tail-returns-latest ()
  "When multiple ai-title lines are present, returns the LAST one (latest)."
  (let ((tail (concat
               "{\"type\":\"ai-title\",\"aiTitle\":\"First\",\"sessionId\":\"s\"}\n"
               "{\"type\":\"other\",\"x\":1}\n"
               "{\"type\":\"ai-title\",\"aiTitle\":\"Second\",\"sessionId\":\"s\"}\n"
               "{\"type\":\"ai-title\",\"aiTitle\":\"Third\",\"sessionId\":\"s\"}\n")))
    (should (equal (agent-repl--ai-title-extract-from-tail tail) "Third"))))

(ert-deftest agent-repl-test-ai-title-extract-from-tail-skips-non-title-lines ()
  "Skips lines that don't contain an ai-title entry."
  (let ((tail (concat
               "{\"type\":\"user\",\"message\":\"hi\"}\n"
               "{\"type\":\"assistant\",\"message\":\"hello\"}\n")))
    (should-not (agent-repl--ai-title-extract-from-tail tail))))

(ert-deftest agent-repl-test-ai-title-extract-from-tail-tolerates-malformed-lines ()
  "Malformed JSON lines are skipped without signalling."
  (let ((tail (concat
               "this is not json\n"
               "{not json either\n"
               "{\"type\":\"ai-title\",\"aiTitle\":\"Valid\",\"sessionId\":\"s\"}\n")))
    (should (equal (agent-repl--ai-title-extract-from-tail tail) "Valid"))))

(ert-deftest agent-repl-test-ai-title-extract-from-tail-empty-input ()
  "Empty / nil input returns nil."
  (should-not (agent-repl--ai-title-extract-from-tail nil))
  (should-not (agent-repl--ai-title-extract-from-tail "")))

(ert-deftest agent-repl-test-ai-title-extract-from-tail-rejects-empty-title ()
  "An ai-title entry with an empty aiTitle string is not accepted —
keeps us from rendering whitespace where the user expects a label."
  (let ((tail "{\"type\":\"ai-title\",\"aiTitle\":\"\",\"sessionId\":\"s\"}\n"))
    (should-not (agent-repl--ai-title-extract-from-tail tail))))

;;;; ---- Tests: read-from-jsonl reads file tail ----

(ert-deftest agent-repl-test-ai-title-read-from-jsonl-returns-title ()
  "End-to-end: write a small jsonl with one ai-title line, read it back."
  (let ((path (expand-file-name (format "ai-title-test-%d.jsonl" (random))
                                temporary-file-directory)))
    (unwind-protect
        (progn
          (with-temp-file path
            (insert "{\"type\":\"user\",\"message\":\"hi\"}\n")
            (insert "{\"type\":\"ai-title\",\"aiTitle\":\"My Conversation\",\"sessionId\":\"s\"}\n"))
          (should (equal (agent-repl--ai-title-read-from-jsonl path)
                         "My Conversation")))
      (when (file-exists-p path) (delete-file path)))))

(ert-deftest agent-repl-test-ai-title-read-from-jsonl-missing-file ()
  "Returns nil when the file doesn't exist."
  (should-not (agent-repl--ai-title-read-from-jsonl
               (expand-file-name (format "no-such-file-%d.jsonl" (random))
                                 temporary-file-directory))))

(ert-deftest agent-repl-test-ai-title-read-from-jsonl-nil-path ()
  "Returns nil when path is nil."
  (should-not (agent-repl--ai-title-read-from-jsonl nil)))

(ert-deftest agent-repl-test-ai-title-read-from-jsonl-reads-only-tail ()
  "Reads only the last `agent-repl-ai-title-scan-bytes' — older entries
beyond the scan window are not visible.  Pins the tail-scan behavior so
a future refactor doesn't accidentally start reading the whole file."
  (let* ((path (expand-file-name (format "ai-title-tail-%d.jsonl" (random))
                                 temporary-file-directory))
         (agent-repl-ai-title-scan-bytes 200))
    (unwind-protect
        (progn
          (with-temp-file path
            ;; Old ai-title, then a big block of filler, then a recent
            ;; non-title line.  The recent tail won't include the
            ;; ai-title, so we should get nil.
            (insert "{\"type\":\"ai-title\",\"aiTitle\":\"Old\",\"sessionId\":\"s\"}\n")
            (insert (make-string 500 ?x))
            (insert "\n")
            (insert "{\"type\":\"user\",\"message\":\"recent\"}\n"))
          (should-not (agent-repl--ai-title-read-from-jsonl path)))
      (when (file-exists-p path) (delete-file path)))))

;;;; ---- Tests: for-ws caches by mtime ----

(ert-deftest agent-repl-test-ai-title-for-ws-caches-when-mtime-unchanged ()
  "Second call hits the cache without re-reading the file."
  (agent-repl-test--with-clean-state
    (let ((path "/tmp/cache-test.jsonl")
          (mtime 12345.0)
          (read-count 0))
      (cl-letf (((symbol-function 'agent-repl--ai-title-jsonl-path)
                 (lambda (_ws) path))
                ((symbol-function 'agent-repl--ai-title-mtime)
                 (lambda (_p) mtime))
                ((symbol-function 'agent-repl--ai-title-read-from-jsonl)
                 (lambda (_p) (setq read-count (1+ read-count)) "Cached Title")))
        (should (equal (agent-repl--ai-title-for-ws "ws1") "Cached Title"))
        (should (equal (agent-repl--ai-title-for-ws "ws1") "Cached Title"))
        (should (= read-count 1))))))

(ert-deftest agent-repl-test-ai-title-for-ws-rereads-on-mtime-change ()
  "When mtime advances, the cache is invalidated and a fresh read runs."
  (agent-repl-test--with-clean-state
    (let ((path "/tmp/cache-test.jsonl")
          (mtime 12345.0)
          (read-count 0))
      (cl-letf (((symbol-function 'agent-repl--ai-title-jsonl-path)
                 (lambda (_ws) path))
                ((symbol-function 'agent-repl--ai-title-mtime)
                 (lambda (_p) mtime))
                ((symbol-function 'agent-repl--ai-title-read-from-jsonl)
                 (lambda (_p)
                   (setq read-count (1+ read-count))
                   (format "Title-%d" read-count))))
        (should (equal (agent-repl--ai-title-for-ws "ws1") "Title-1"))
        (setq mtime 99999.0)
        (should (equal (agent-repl--ai-title-for-ws "ws1") "Title-2"))
        (should (= read-count 2))))))

(ert-deftest agent-repl-test-ai-title-for-ws-nil-when-no-path ()
  "Returns nil when no path is resolvable."
  (agent-repl-test--with-clean-state
    (cl-letf (((symbol-function 'agent-repl--ai-title-jsonl-path)
               (lambda (_ws) nil)))
      (should-not (agent-repl--ai-title-for-ws "ws1")))))

(ert-deftest agent-repl-test-ai-title-for-ws-nil-when-file-missing ()
  "Returns nil when the resolved file is gone (mtime nil)."
  (agent-repl-test--with-clean-state
    (cl-letf (((symbol-function 'agent-repl--ai-title-jsonl-path)
               (lambda (_ws) "/tmp/no-such.jsonl"))
              ((symbol-function 'agent-repl--ai-title-mtime)
               (lambda (_p) nil)))
      (should-not (agent-repl--ai-title-for-ws "ws1")))))

;;;; ---- Tests: segment ----

(ert-deftest agent-repl-test-ai-title-segment-empty-without-owning-ws ()
  "Segment is empty when no owning workspace is set on the buffer."
  (agent-repl-test--with-clean-state
    (with-temp-buffer
      (should (equal (agent-repl--ai-title-segment) "")))))

(ert-deftest agent-repl-test-ai-title-segment-empty-without-title ()
  "Segment is empty when the workspace has no resolvable aiTitle."
  (agent-repl-test--with-clean-state
    (cl-letf (((symbol-function 'agent-repl--ai-title-for-ws)
               (lambda (_ws) nil)))
      (with-temp-buffer
        (setq-local agent-repl--owning-workspace "ws1")
        (should (equal (agent-repl--ai-title-segment) ""))))))

(ert-deftest agent-repl-test-ai-title-segment-renders-title ()
  "Segment contains the resolved aiTitle when one is available."
  (agent-repl-test--with-clean-state
    (cl-letf (((symbol-function 'agent-repl--ai-title-for-ws)
               (lambda (_ws) "My Conversation")))
      (with-temp-buffer
        (setq-local agent-repl--owning-workspace "ws1")
        (let ((seg (agent-repl--ai-title-segment)))
          (should (string-match-p "My Conversation" seg)))))))

(ert-deftest agent-repl-test-ai-title-segment-disabled ()
  "Segment is empty when the feature is disabled."
  (agent-repl-test--with-clean-state
    (let ((agent-repl-ai-title-enabled nil))
      (cl-letf (((symbol-function 'agent-repl--ai-title-for-ws)
                 (lambda (_ws) "Should Not Appear")))
        (with-temp-buffer
          (setq-local agent-repl--owning-workspace "ws1")
          (should (equal (agent-repl--ai-title-segment) "")))))))

;;;; ---- Tests: mode-line attachment ----

(ert-deftest agent-repl-test-ai-title-attach-to-mode-line-appends-when-missing ()
  "attach-to-mode-line appends the :eval segment when not already present."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-temp-buffer "*agent-panel-ai-title-attach-missing*"
      (setq-local mode-line-format (list "BAR"))
      (agent-repl--ai-title-attach-to-mode-line (current-buffer))
      (should (= (length mode-line-format) 2))
      (should (equal (car (last mode-line-format))
                     agent-repl--ai-title-mode-line-spec)))))

(ert-deftest agent-repl-test-ai-title-attach-to-mode-line-idempotent ()
  "attach-to-mode-line does not double-append when called twice."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-temp-buffer "*agent-panel-ai-title-attach-idempotent*"
      (setq-local mode-line-format
                  (list "BAR" agent-repl--ai-title-mode-line-spec))
      (agent-repl--ai-title-attach-to-mode-line (current-buffer))
      (agent-repl--ai-title-attach-to-mode-line (current-buffer))
      (should (= (length mode-line-format) 2)))))

(ert-deftest agent-repl-test-ai-title-attach-to-mode-line-skips-non-list ()
  "attach-to-mode-line leaves string mode-line-formats alone."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-temp-buffer "*agent-panel-ai-title-attach-string*"
      (setq-local mode-line-format "literal-string")
      (agent-repl--ai-title-attach-to-mode-line (current-buffer))
      (should (equal mode-line-format "literal-string")))))

(ert-deftest agent-repl-test-ai-title-attach-all-walks-workspaces ()
  "attach-all attaches the segment to every live workspace vterm buffer."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-temp-buffer "*agent-panel-ai-title-attach-all-1*"
      (setq-local mode-line-format (list "A"))
      (agent-repl--ws-put "ws1" :vterm-buffer (current-buffer))
      (agent-repl-test--with-temp-buffer "*agent-panel-ai-title-attach-all-2*"
        (setq-local mode-line-format (list "B"))
        (agent-repl--ws-put "ws2" :vterm-buffer (current-buffer))
        (agent-repl-ai-title-attach-all)
        (with-current-buffer "*agent-panel-ai-title-attach-all-1*"
          (should (member agent-repl--ai-title-mode-line-spec
                          mode-line-format)))
        (with-current-buffer "*agent-panel-ai-title-attach-all-2*"
          (should (member agent-repl--ai-title-mode-line-spec
                          mode-line-format)))))))

(ert-deftest agent-repl-test-ai-title-attach-all-skips-dead-buffer ()
  "attach-all tolerates dead vterm buffers without signalling."
  (agent-repl-test--with-clean-state
    (let ((dead-buf (generate-new-buffer "*agent-panel-ai-title-dead*")))
      (kill-buffer dead-buf)
      (agent-repl--ws-put "ws-dead" :vterm-buffer dead-buf)
      ;; Should not signal.
      (agent-repl-ai-title-attach-all))))

;;;; ---- Tests: workspace-mode-line does NOT wire the ai-title segment ----

(ert-deftest agent-repl-test-workspace-mode-line-omits-ai-title-segment ()
  "`agent-repl--workspace-mode-line' does NOT wire the ai-title (nor the
prompt-summary) :eval segment: both were intentionally dropped from the
status bar.  Pins the drop so a refactor doesn't silently re-add either
to newly-created vterm buffers."
  (agent-repl-test--with-clean-state
    (cl-letf (((symbol-function 'agent-repl--merge-target-name)
               (lambda (_ws) nil)))
      (let ((result (agent-repl--workspace-mode-line "ws1")))
        (should-not (member '(:eval (agent-repl--ai-title-segment)) result))
        (should-not (member '(:eval (agent-repl--prompt-summary-segment)) result))))))

(provide 'test-ai-title)
;;; test-ai-title.el ends here
