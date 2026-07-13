;;; test-context.el --- Tests for context.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for the context-tokens mode-line segment: usage token summing,
;; jsonl tail scanning for the most recent main-chain assistant usage,
;; mtime-keyed cache, comma thousands formatting, segment rendering, and
;; mode-line attachment.

;;; Code:

(load (expand-file-name "test-helpers.el" (file-name-directory
                                            (or load-file-name buffer-file-name)))
      nil t)

;;;; ---- Tests: usage-total ----

(ert-deftest agent-repl-test-context-usage-total-sums-three ()
  "Sums input, cache-creation, and cache-read token counters."
  (should (= (agent-repl--context-usage-total
              '(("input_tokens" . 2)
                ("cache_creation_input_tokens" . 1277)
                ("cache_read_input_tokens" . 51900)))
             53179)))

(ert-deftest agent-repl-test-context-usage-total-missing-counter-is-zero ()
  "A missing counter contributes 0 rather than failing."
  (should (= (agent-repl--context-usage-total
              '(("input_tokens" . 100)))
             100)))

(ert-deftest agent-repl-test-context-usage-total-none-present-nil ()
  "A usage alist carrying none of the three counters yields nil."
  (should-not (agent-repl--context-usage-total
               '(("output_tokens" . 500)))))

(ert-deftest agent-repl-test-context-usage-total-non-alist-nil ()
  "A non-list usage value yields nil."
  (should-not (agent-repl--context-usage-total "nope")))

(ert-deftest agent-repl-test-context-usage-total-zero-is-valid ()
  "A present-but-zero counter is a valid total of 0, not nil."
  (should (= (agent-repl--context-usage-total
              '(("input_tokens" . 0)))
             0)))

;;;; ---- Tests: extract-from-tail ----

(ert-deftest agent-repl-test-context-extract-from-tail-finds-total ()
  "Returns the summed total from a tail with one assistant entry."
  (should (= (agent-repl--context-extract-from-tail
              "{\"type\":\"assistant\",\"message\":{\"usage\":{\"input_tokens\":10,\"cache_read_input_tokens\":90}}}")
             100)))

(ert-deftest agent-repl-test-context-extract-from-tail-returns-latest ()
  "When multiple assistant lines are present, returns the LAST one (latest)."
  (let ((tail (concat
               "{\"type\":\"assistant\",\"message\":{\"usage\":{\"input_tokens\":10}}}\n"
               "{\"type\":\"user\",\"message\":{\"content\":\"hi\"}}\n"
               "{\"type\":\"assistant\",\"message\":{\"usage\":{\"input_tokens\":42}}}\n")))
    (should (= (agent-repl--context-extract-from-tail tail) 42))))

(ert-deftest agent-repl-test-context-extract-from-tail-skips-sidechain ()
  "Sidechain assistant lines are skipped so the primary usage wins."
  (let ((tail (concat
               "{\"type\":\"assistant\",\"message\":{\"usage\":{\"input_tokens\":10}}}\n"
               "{\"type\":\"assistant\",\"isSidechain\":true,\"message\":{\"usage\":{\"input_tokens\":9999}}}\n")))
    (should (= (agent-repl--context-extract-from-tail tail) 10))))

(ert-deftest agent-repl-test-context-extract-from-tail-skips-non-assistant-lines ()
  "Skips lines that aren't assistant entries."
  (let ((tail (concat
               "{\"type\":\"user\",\"message\":{\"content\":\"hi\"}}\n"
               "{\"type\":\"ai-title\",\"aiTitle\":\"T\"}\n")))
    (should-not (agent-repl--context-extract-from-tail tail))))

(ert-deftest agent-repl-test-context-extract-from-tail-tolerates-malformed-lines ()
  "Malformed JSON lines are skipped without signalling."
  (let ((tail (concat
               "this is not json\n"
               "{not json either\n"
               "{\"type\":\"assistant\",\"message\":{\"usage\":{\"input_tokens\":7}}}\n")))
    (should (= (agent-repl--context-extract-from-tail tail) 7))))

(ert-deftest agent-repl-test-context-extract-from-tail-empty-input ()
  "Empty / nil input returns nil."
  (should-not (agent-repl--context-extract-from-tail nil))
  (should-not (agent-repl--context-extract-from-tail "")))

(ert-deftest agent-repl-test-context-extract-from-tail-skips-missing-usage ()
  "An assistant entry with no `message.usage' is skipped, falling through
to an earlier entry that has one."
  (let ((tail (concat
               "{\"type\":\"assistant\",\"message\":{\"usage\":{\"input_tokens\":5}}}\n"
               "{\"type\":\"assistant\",\"message\":{\"content\":\"hi\"}}\n")))
    (should (= (agent-repl--context-extract-from-tail tail) 5))))

;;;; ---- Tests: read-from-jsonl reads file tail ----

(ert-deftest agent-repl-test-context-read-from-jsonl-returns-total ()
  "End-to-end: write a small jsonl with one assistant line, read it back."
  (let ((path (expand-file-name (format "context-test-%d.jsonl" (random))
                                temporary-file-directory)))
    (unwind-protect
        (progn
          (with-temp-file path
            (insert "{\"type\":\"user\",\"message\":{\"content\":\"hi\"}}\n")
            (insert "{\"type\":\"assistant\",\"message\":{\"usage\":{\"input_tokens\":1,\"cache_read_input_tokens\":99}}}\n"))
          (should (= (agent-repl--context-read-from-jsonl path) 100)))
      (when (file-exists-p path) (delete-file path)))))

(ert-deftest agent-repl-test-context-read-from-jsonl-missing-file ()
  "Returns nil when the file doesn't exist."
  (should-not (agent-repl--context-read-from-jsonl
               (expand-file-name (format "no-such-file-%d.jsonl" (random))
                                 temporary-file-directory))))

(ert-deftest agent-repl-test-context-read-from-jsonl-nil-path ()
  "Returns nil when path is nil."
  (should-not (agent-repl--context-read-from-jsonl nil)))

(ert-deftest agent-repl-test-context-read-from-jsonl-reads-only-tail ()
  "Reads only the last `agent-repl-context-scan-bytes' — older entries
beyond the scan window are not visible."
  (let* ((path (expand-file-name (format "context-tail-%d.jsonl" (random))
                                 temporary-file-directory))
         (agent-repl-context-scan-bytes 200))
    (unwind-protect
        (progn
          (with-temp-file path
            (insert "{\"type\":\"assistant\",\"message\":{\"usage\":{\"input_tokens\":123}}}\n")
            (insert (make-string 500 ?x))
            (insert "\n")
            (insert "{\"type\":\"user\",\"message\":{\"content\":\"recent\"}}\n"))
          (should-not (agent-repl--context-read-from-jsonl path)))
      (when (file-exists-p path) (delete-file path)))))

;;;; ---- Tests: for-ws caches by mtime ----

(ert-deftest agent-repl-test-context-for-ws-caches-when-mtime-unchanged ()
  "Second call hits the cache without re-reading the file."
  (agent-repl-test--with-clean-state
    (let ((path "/tmp/context-cache-test.jsonl")
          (mtime 12345.0)
          (read-count 0))
      (cl-letf (((symbol-function 'agent-repl--ai-title-jsonl-path)
                 (lambda (_ws) path))
                ((symbol-function 'agent-repl--ai-title-mtime)
                 (lambda (_p) mtime))
                ((symbol-function 'agent-repl--context-read-from-jsonl)
                 (lambda (_p) (setq read-count (1+ read-count)) 100)))
        (should (= (agent-repl--context-for-ws "ws1") 100))
        (should (= (agent-repl--context-for-ws "ws1") 100))
        (should (= read-count 1))))))

(ert-deftest agent-repl-test-context-for-ws-rereads-on-mtime-change ()
  "When mtime advances, the cache is invalidated and a fresh read runs."
  (agent-repl-test--with-clean-state
    (let ((path "/tmp/context-cache-test.jsonl")
          (mtime 12345.0)
          (read-count 0))
      (cl-letf (((symbol-function 'agent-repl--ai-title-jsonl-path)
                 (lambda (_ws) path))
                ((symbol-function 'agent-repl--ai-title-mtime)
                 (lambda (_p) mtime))
                ((symbol-function 'agent-repl--context-read-from-jsonl)
                 (lambda (_p)
                   (setq read-count (1+ read-count))
                   (* read-count 100))))
        (should (= (agent-repl--context-for-ws "ws1") 100))
        (setq mtime 99999.0)
        (should (= (agent-repl--context-for-ws "ws1") 200))
        (should (= read-count 2))))))

(ert-deftest agent-repl-test-context-for-ws-nil-when-no-path ()
  "Returns nil when no path is resolvable."
  (agent-repl-test--with-clean-state
    (cl-letf (((symbol-function 'agent-repl--ai-title-jsonl-path)
               (lambda (_ws) nil)))
      (should-not (agent-repl--context-for-ws "ws1")))))

(ert-deftest agent-repl-test-context-for-ws-nil-when-file-missing ()
  "Returns nil when the resolved file is gone (mtime nil)."
  (agent-repl-test--with-clean-state
    (cl-letf (((symbol-function 'agent-repl--ai-title-jsonl-path)
               (lambda (_ws) "/tmp/no-such.jsonl"))
              ((symbol-function 'agent-repl--ai-title-mtime)
               (lambda (_p) nil)))
      (should-not (agent-repl--context-for-ws "ws1")))))

(provide 'test-context)
;;; test-context.el ends here
