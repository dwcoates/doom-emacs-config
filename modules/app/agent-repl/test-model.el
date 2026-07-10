;;; test-model.el --- Tests for model.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for the Claude-model mode-line segment: raw-id prettifying,
;; jsonl tail scanning for the most recent main-chain assistant model,
;; mtime-keyed cache, segment formatting, and mode-line attachment.

;;; Code:

(load (expand-file-name "test-helpers.el" (file-name-directory
                                            (or load-file-name buffer-file-name)))
      nil t)

;;;; ---- Tests: prettify ----

(ert-deftest agent-repl-test-model-prettify-family-major-minor ()
  "`claude-opus-4-8' renders as `Opus 4.8'."
  (should (equal (agent-repl--model-prettify "claude-opus-4-8") "Opus 4.8")))

(ert-deftest agent-repl-test-model-prettify-sonnet ()
  "Sonnet family is recognized and capitalized."
  (should (equal (agent-repl--model-prettify "claude-sonnet-4-5") "Sonnet 4.5")))

(ert-deftest agent-repl-test-model-prettify-haiku ()
  "Haiku family is recognized and capitalized."
  (should (equal (agent-repl--model-prettify "claude-haiku-3-5") "Haiku 3.5")))

(ert-deftest agent-repl-test-model-prettify-major-only ()
  "A family with only a major version omits the dotted minor."
  (should (equal (agent-repl--model-prettify "claude-opus-4") "Opus 4")))

(ert-deftest agent-repl-test-model-prettify-family-only ()
  "A bare family id renders as the capitalized family alone."
  (should (equal (agent-repl--model-prettify "claude-opus") "Opus")))

(ert-deftest agent-repl-test-model-prettify-unknown-family-strips-prefix ()
  "Unrecognized shapes fall back to the prefix-stripped id verbatim."
  (should (equal (agent-repl--model-prettify "claude-experimental-x")
                 "experimental-x")))

(ert-deftest agent-repl-test-model-prettify-no-claude-prefix ()
  "An id lacking the `claude-' prefix is used as-is (still family-matched)."
  (should (equal (agent-repl--model-prettify "opus-4-8") "Opus 4.8")))

(ert-deftest agent-repl-test-model-prettify-nil ()
  "Nil/empty input returns nil so callers can short-circuit."
  (should-not (agent-repl--model-prettify nil))
  (should-not (agent-repl--model-prettify "")))

;;;; ---- Tests: extract-from-tail ----

(ert-deftest agent-repl-test-model-extract-from-tail-finds-model ()
  "Returns the model from a tail with one assistant entry."
  (should (equal (agent-repl--model-extract-from-tail
                  "{\"type\":\"assistant\",\"message\":{\"model\":\"claude-opus-4-8\"}}")
                 "claude-opus-4-8")))

(ert-deftest agent-repl-test-model-extract-from-tail-returns-latest ()
  "When multiple assistant lines are present, returns the LAST one (latest)."
  (let ((tail (concat
               "{\"type\":\"assistant\",\"message\":{\"model\":\"claude-opus-4-8\"}}\n"
               "{\"type\":\"user\",\"message\":{\"content\":\"hi\"}}\n"
               "{\"type\":\"assistant\",\"message\":{\"model\":\"claude-sonnet-4-5\"}}\n")))
    (should (equal (agent-repl--model-extract-from-tail tail)
                   "claude-sonnet-4-5"))))

(ert-deftest agent-repl-test-model-extract-from-tail-skips-sidechain ()
  "Sidechain assistant lines are skipped so the primary model wins."
  (let ((tail (concat
               "{\"type\":\"assistant\",\"message\":{\"model\":\"claude-opus-4-8\"}}\n"
               "{\"type\":\"assistant\",\"isSidechain\":true,\"message\":{\"model\":\"claude-haiku-3-5\"}}\n")))
    (should (equal (agent-repl--model-extract-from-tail tail)
                   "claude-opus-4-8"))))

(ert-deftest agent-repl-test-model-extract-from-tail-skips-non-assistant-lines ()
  "Skips lines that aren't assistant entries."
  (let ((tail (concat
               "{\"type\":\"user\",\"message\":{\"content\":\"hi\"}}\n"
               "{\"type\":\"ai-title\",\"aiTitle\":\"T\"}\n")))
    (should-not (agent-repl--model-extract-from-tail tail))))

(ert-deftest agent-repl-test-model-extract-from-tail-tolerates-malformed-lines ()
  "Malformed JSON lines are skipped without signalling."
  (let ((tail (concat
               "this is not json\n"
               "{not json either\n"
               "{\"type\":\"assistant\",\"message\":{\"model\":\"claude-opus-4-8\"}}\n")))
    (should (equal (agent-repl--model-extract-from-tail tail)
                   "claude-opus-4-8"))))

(ert-deftest agent-repl-test-model-extract-from-tail-empty-input ()
  "Empty / nil input returns nil."
  (should-not (agent-repl--model-extract-from-tail nil))
  (should-not (agent-repl--model-extract-from-tail "")))

(ert-deftest agent-repl-test-model-extract-from-tail-rejects-empty-model ()
  "An assistant entry with an empty model string is not accepted."
  (let ((tail "{\"type\":\"assistant\",\"message\":{\"model\":\"\"}}\n"))
    (should-not (agent-repl--model-extract-from-tail tail))))

(ert-deftest agent-repl-test-model-extract-from-tail-rejects-missing-model ()
  "An assistant entry with no `message.model' field is skipped."
  (let ((tail "{\"type\":\"assistant\",\"message\":{\"content\":\"hi\"}}\n"))
    (should-not (agent-repl--model-extract-from-tail tail))))

;;;; ---- Tests: read-from-jsonl reads file tail ----

(ert-deftest agent-repl-test-model-read-from-jsonl-returns-model ()
  "End-to-end: write a small jsonl with one assistant line, read it back."
  (let ((path (expand-file-name (format "model-test-%d.jsonl" (random))
                                temporary-file-directory)))
    (unwind-protect
        (progn
          (with-temp-file path
            (insert "{\"type\":\"user\",\"message\":{\"content\":\"hi\"}}\n")
            (insert "{\"type\":\"assistant\",\"message\":{\"model\":\"claude-opus-4-8\"}}\n"))
          (should (equal (agent-repl--model-read-from-jsonl path)
                         "claude-opus-4-8")))
      (when (file-exists-p path) (delete-file path)))))

(ert-deftest agent-repl-test-model-read-from-jsonl-missing-file ()
  "Returns nil when the file doesn't exist."
  (should-not (agent-repl--model-read-from-jsonl
               (expand-file-name (format "no-such-file-%d.jsonl" (random))
                                 temporary-file-directory))))

(ert-deftest agent-repl-test-model-read-from-jsonl-nil-path ()
  "Returns nil when path is nil."
  (should-not (agent-repl--model-read-from-jsonl nil)))

(ert-deftest agent-repl-test-model-read-from-jsonl-reads-only-tail ()
  "Reads only the last `agent-repl-model-scan-bytes' — older entries
beyond the scan window are not visible."
  (let* ((path (expand-file-name (format "model-tail-%d.jsonl" (random))
                                 temporary-file-directory))
         (agent-repl-model-scan-bytes 200))
    (unwind-protect
        (progn
          (with-temp-file path
            (insert "{\"type\":\"assistant\",\"message\":{\"model\":\"claude-opus-4-8\"}}\n")
            (insert (make-string 500 ?x))
            (insert "\n")
            (insert "{\"type\":\"user\",\"message\":{\"content\":\"recent\"}}\n"))
          (should-not (agent-repl--model-read-from-jsonl path)))
      (when (file-exists-p path) (delete-file path)))))

;;;; ---- Tests: for-ws caches by mtime ----

(ert-deftest agent-repl-test-model-for-ws-caches-when-mtime-unchanged ()
  "Second call hits the cache without re-reading the file."
  (agent-repl-test--with-clean-state
    (let ((path "/tmp/model-cache-test.jsonl")
          (mtime 12345.0)
          (read-count 0))
      (cl-letf (((symbol-function 'agent-repl--ai-title-jsonl-path)
                 (lambda (_ws) path))
                ((symbol-function 'agent-repl--ai-title-mtime)
                 (lambda (_p) mtime))
                ((symbol-function 'agent-repl--model-read-from-jsonl)
                 (lambda (_p) (setq read-count (1+ read-count)) "claude-opus-4-8")))
        (should (equal (agent-repl--model-for-ws "ws1") "claude-opus-4-8"))
        (should (equal (agent-repl--model-for-ws "ws1") "claude-opus-4-8"))
        (should (= read-count 1))))))

(ert-deftest agent-repl-test-model-for-ws-rereads-on-mtime-change ()
  "When mtime advances, the cache is invalidated and a fresh read runs."
  (agent-repl-test--with-clean-state
    (let ((path "/tmp/model-cache-test.jsonl")
          (mtime 12345.0)
          (read-count 0))
      (cl-letf (((symbol-function 'agent-repl--ai-title-jsonl-path)
                 (lambda (_ws) path))
                ((symbol-function 'agent-repl--ai-title-mtime)
                 (lambda (_p) mtime))
                ((symbol-function 'agent-repl--model-read-from-jsonl)
                 (lambda (_p)
                   (setq read-count (1+ read-count))
                   (format "claude-model-%d" read-count))))
        (should (equal (agent-repl--model-for-ws "ws1") "claude-model-1"))
        (setq mtime 99999.0)
        (should (equal (agent-repl--model-for-ws "ws1") "claude-model-2"))
        (should (= read-count 2))))))

(ert-deftest agent-repl-test-model-for-ws-nil-when-no-path ()
  "Returns nil when no path is resolvable."
  (agent-repl-test--with-clean-state
    (cl-letf (((symbol-function 'agent-repl--ai-title-jsonl-path)
               (lambda (_ws) nil)))
      (should-not (agent-repl--model-for-ws "ws1")))))

(ert-deftest agent-repl-test-model-for-ws-nil-when-file-missing ()
  "Returns nil when the resolved file is gone (mtime nil)."
  (agent-repl-test--with-clean-state
    (cl-letf (((symbol-function 'agent-repl--ai-title-jsonl-path)
               (lambda (_ws) "/tmp/no-such.jsonl"))
              ((symbol-function 'agent-repl--ai-title-mtime)
               (lambda (_p) nil)))
      (should-not (agent-repl--model-for-ws "ws1")))))

(ert-deftest agent-repl-test-model-for-ws-reads-from-config-dir-projects ()
  "model-for-ws reads the jsonl under the resolved <config-dir>/projects for a multi-repo workspace."
  (agent-repl-test--with-clean-state
    (let* ((cfg (make-temp-file "claude-cfg-" t))
           (root (make-temp-file "multi-root-" t))
           (project-dir (expand-file-name "proj" root))
           (encoded (agent-repl--ai-title-encode-cwd project-dir))
           (jsonl (expand-file-name "sid-1.jsonl"
                                    (expand-file-name encoded
                                                      (expand-file-name "projects" cfg))))
           (process-environment (cons (concat "MULTI_REPO_ROOT=" root) process-environment))
           (agent-repl-multi-repo-config-dir cfg))
      (unwind-protect
          (progn
            (agent-repl--ws-put "ws1" :project-dir project-dir)
            (agent-repl-test--seed-file
             jsonl
             "{\"type\":\"assistant\",\"message\":{\"model\":\"claude-opus-4-8\"}}\n")
            (cl-letf (((symbol-function 'agent-repl--ai-title-ws-session-id)
                       (lambda (_ws) "sid-1")))
              (should (equal (agent-repl--model-for-ws "ws1") "claude-opus-4-8"))))
        (delete-directory cfg t)
        (delete-directory root t)))))

;;;; ---- Tests: persist-value ----

(ert-deftest agent-repl-test-model-persist-value-prefers-config-dir-model ()
  "persist-value returns the config-dir session model, overriding the
generation `:model' — the workspace was generated with opus but switched
to fable mid-session, so fable is what gets persisted."
  (agent-repl-test--with-clean-state
    (cl-letf (((symbol-function 'agent-repl--model-for-ws)
               (lambda (_ws) "claude-fable-5")))
      (agent-repl--ws-put "ws1" :model "opus")
      (should (equal (agent-repl--model-persist-value "ws1") "claude-fable-5")))))

(ert-deftest agent-repl-test-model-persist-value-falls-back-to-generation-model ()
  "persist-value falls back to the generation `:model' when no config-dir
model is available yet (session has produced no assistant turn)."
  (agent-repl-test--with-clean-state
    (cl-letf (((symbol-function 'agent-repl--model-for-ws)
               (lambda (_ws) nil)))
      (agent-repl--ws-put "ws1" :model "opus")
      (should (equal (agent-repl--model-persist-value "ws1") "opus")))))

(ert-deftest agent-repl-test-model-persist-value-nil-when-no-source ()
  "persist-value returns nil when neither the config dir nor `:model' yields a model."
  (agent-repl-test--with-clean-state
    (cl-letf (((symbol-function 'agent-repl--model-for-ws)
               (lambda (_ws) nil)))
      (should-not (agent-repl--model-persist-value "ws1")))))

;;;; ---- Tests: segment ----

(ert-deftest agent-repl-test-model-segment-empty-without-owning-ws ()
  "Segment is empty when no owning workspace is set on the buffer."
  (agent-repl-test--with-clean-state
    (with-temp-buffer
      (should (equal (agent-repl--model-segment) "")))))

(ert-deftest agent-repl-test-model-segment-empty-without-model ()
  "Segment is empty when the workspace has no resolvable model."
  (agent-repl-test--with-clean-state
    (cl-letf (((symbol-function 'agent-repl--model-for-ws)
               (lambda (_ws) nil)))
      (with-temp-buffer
        (setq-local agent-repl--owning-workspace "ws1")
        (should (equal (agent-repl--model-segment) ""))))))

(ert-deftest agent-repl-test-model-segment-renders-prettified-model ()
  "Segment contains the prettified model label when one is available."
  (agent-repl-test--with-clean-state
    (cl-letf (((symbol-function 'agent-repl--model-for-ws)
               (lambda (_ws) "claude-opus-4-8")))
      (with-temp-buffer
        (setq-local agent-repl--owning-workspace "ws1")
        (let ((seg (agent-repl--model-segment)))
          (should (string-match-p "Opus 4.8" seg)))))))

(ert-deftest agent-repl-test-model-segment-disabled ()
  "Segment is empty when the feature is disabled."
  (agent-repl-test--with-clean-state
    (let ((agent-repl-model-enabled nil))
      (cl-letf (((symbol-function 'agent-repl--model-for-ws)
                 (lambda (_ws) "claude-opus-4-8")))
        (with-temp-buffer
          (setq-local agent-repl--owning-workspace "ws1")
          (should (equal (agent-repl--model-segment) "")))))))

;;;; ---- Tests: mode-line attachment ----

(ert-deftest agent-repl-test-model-attach-to-mode-line-appends-when-missing ()
  "attach-to-mode-line appends the :eval segment when not already present."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-temp-buffer "*claude-panel-model-attach-missing*"
      (setq-local mode-line-format (list "BAR"))
      (agent-repl--model-attach-to-mode-line (current-buffer))
      (should (= (length mode-line-format) 2))
      (should (equal (car (last mode-line-format))
                     agent-repl--model-mode-line-spec)))))

(ert-deftest agent-repl-test-model-attach-to-mode-line-idempotent ()
  "attach-to-mode-line does not double-append when called twice."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-temp-buffer "*claude-panel-model-attach-idempotent*"
      (setq-local mode-line-format
                  (list "BAR" agent-repl--model-mode-line-spec))
      (agent-repl--model-attach-to-mode-line (current-buffer))
      (agent-repl--model-attach-to-mode-line (current-buffer))
      (should (= (length mode-line-format) 2)))))

(ert-deftest agent-repl-test-model-attach-to-mode-line-skips-non-list ()
  "attach-to-mode-line leaves string mode-line-formats alone."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-temp-buffer "*claude-panel-model-attach-string*"
      (setq-local mode-line-format "literal-string")
      (agent-repl--model-attach-to-mode-line (current-buffer))
      (should (equal mode-line-format "literal-string")))))

(ert-deftest agent-repl-test-model-attach-all-walks-workspaces ()
  "attach-all attaches the segment to every live workspace vterm buffer."
  (agent-repl-test--with-clean-state
    (agent-repl-test--with-temp-buffer "*claude-panel-model-attach-all-1*"
      (setq-local mode-line-format (list "A"))
      (agent-repl--ws-put "ws1" :vterm-buffer (current-buffer))
      (agent-repl-test--with-temp-buffer "*claude-panel-model-attach-all-2*"
        (setq-local mode-line-format (list "B"))
        (agent-repl--ws-put "ws2" :vterm-buffer (current-buffer))
        (agent-repl-model-attach-all)
        (with-current-buffer "*claude-panel-model-attach-all-1*"
          (should (member agent-repl--model-mode-line-spec
                          mode-line-format)))
        (with-current-buffer "*claude-panel-model-attach-all-2*"
          (should (member agent-repl--model-mode-line-spec
                          mode-line-format)))))))

(ert-deftest agent-repl-test-model-attach-all-skips-dead-buffer ()
  "attach-all tolerates dead vterm buffers without signalling."
  (agent-repl-test--with-clean-state
    (let ((dead-buf (generate-new-buffer "*claude-panel-model-dead*")))
      (kill-buffer dead-buf)
      (agent-repl--ws-put "ws-dead" :vterm-buffer dead-buf)
      ;; Should not signal.
      (agent-repl-model-attach-all))))

;;;; ---- Tests: workspace-mode-line wires the model segment ----

(ert-deftest agent-repl-test-workspace-mode-line-has-model-segment ()
  "`agent-repl--workspace-mode-line' includes the model :eval segment
before the context-tokens segment.  Pins the layout so a refactor
doesn't silently drop the model from newly-created vterm buffers."
  (agent-repl-test--with-clean-state
    (cl-letf (((symbol-function 'agent-repl--merge-target-name)
               (lambda (_ws) nil)))
      (let* ((result (agent-repl--workspace-mode-line "ws1"))
             (specs (cl-remove-if-not (lambda (x) (and (consp x) (eq (car x) :eval)))
                                      result)))
        (should (member '(:eval (agent-repl--model-segment)) specs))
        ;; model must appear BEFORE the context-tokens segment in the list.
        (let ((model-pos (cl-position '(:eval (agent-repl--model-segment))
                                      result :test #'equal))
              (ctx-pos (cl-position '(:eval (agent-repl--context-segment))
                                    result :test #'equal)))
          (should (and model-pos ctx-pos (< model-pos ctx-pos))))))))

(provide 'test-model)
;;; test-model.el ends here
