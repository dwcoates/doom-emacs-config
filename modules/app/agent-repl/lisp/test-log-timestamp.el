;;; test-log-timestamp.el --- The cross-language log timestamp contract -*- lexical-binding: t; -*-

;;; Commentary:

;; Emacs's corner of the contract in proto/vocab/log-timestamp.json.
;;
;; That file is the ONE answer to how an agent-repl record writes down the
;; instant it observed.  Go asserts against it from `agent-shim/logging/go',
;; TypeScript asserts against it from `agent-shim/logging/ts', and this file is
;; the third corner — which is what makes a divergence between the three fail
;; loudly instead of quietly.
;;
;; It checks the REPRESENTATION, not the clock.  Every runtime still stamps the
;; instant it actually saw; what may never differ is how that instant is
;; written, because records that read as different clocks cannot be interleaved
;; by a human or sorted by a tool.

;;; Code:

(require 'ert)
(require 'json)
(require 'parse-time)

(let ((dir (file-name-directory (or load-file-name buffer-file-name))))
  (load (expand-file-name "test-helpers.el" dir) nil t))

(defconst agent-repl-test--timestamp-fixture
  (let* ((dir (file-name-directory (or load-file-name buffer-file-name)))
         (path (expand-file-name "proto/vocab/log-timestamp.json" dir)))
    (with-temp-buffer
      (insert-file-contents path)
      (let ((json-object-type 'alist)
            (json-array-type 'list)
            (json-key-type 'string))
        (json-read))))
  "The checked-in cross-language log timestamp contract, decoded.")

(defun agent-repl-test--timestamp-fixture (key)
  "Return the timestamp fixture's KEY value."
  (cdr (assoc key agent-repl-test--timestamp-fixture)))

(defun agent-repl-test--timestamp-pattern ()
  "Return the fixture's pattern as an Emacs regexp anchored to the whole string.
The fixture spells the pattern in the syntax Go and JavaScript both read.
Emacs needs its own escaping for the repetition braces and the anchors, so
the translation is mechanical rather than a second source of truth."
  (let ((pattern (agent-repl-test--timestamp-fixture "pattern")))
    (setq pattern (replace-regexp-in-string "{" "\\{" pattern t t))
    (setq pattern (replace-regexp-in-string "}" "\\}" pattern t t))
    (setq pattern (replace-regexp-in-string "\\`\\^" "\\`" pattern t t))
    (replace-regexp-in-string "\\$\\'" "\\'" pattern t t)))

(defun agent-repl-test--timestamp-instant (text)
  "Return TEXT as an Emacs time value, preserving its fractional seconds.
`parse-time-string' drops the fraction, and the fixture example exists in
part to pin exactly that portion of the representation."
  (let* ((fraction (if (string-match "\\.\\([0-9]+\\)" text)
                       (match-string 1 text)
                     "0"))
         (whole (encode-time
                 (parse-time-string (replace-regexp-in-string "\\.[0-9]+" "" text)))))
    (time-add whole (cons (string-to-number fraction) (expt 10 (length fraction))))))

;;;; ---- The format string agrees with the fixture -----------------------

(ert-deftest agent-repl-test-timestamp-format-matches-the-fixture ()
  "The elisp format string is the one the fixture names for Emacs.
An edit here that Go and TypeScript did not follow fails loudly rather than
in a log nobody can interleave."
  ;; Act / Assert
  (should (equal agent-repl--log-timestamp-format
                 (cdr (assoc "emacs" (agent-repl-test--timestamp-fixture "layouts"))))))

(ert-deftest agent-repl-test-timestamp-fixture-example-renders-exactly ()
  "The fixture's worked example renders exactly as the fixture records it.
The example is written for a machine whose local zone is UTC, so the
assertion forces that zone rather than depending on the developer's."
  ;; Arrange
  (let* ((example (agent-repl-test--timestamp-fixture "example"))
         (instant (agent-repl-test--timestamp-instant (cdr (assoc "instant" example)))))
    ;; Act
    (let ((rendered (format-time-string agent-repl--log-timestamp-format instant t)))
      ;; Assert
      (should (equal rendered (cdr (assoc "rendered_in_utc" example)))))))

;;;; ---- Emitted timestamps conform --------------------------------------

(ert-deftest agent-repl-test-timestamp-matches-the-fixture-pattern ()
  "An emitted timestamp matches the fixture's shape."
  ;; Act
  (let ((stamp (agent-repl--log-rfc3339-timestamp)))
    ;; Assert
    (should (string-match-p (agent-repl-test--timestamp-pattern) stamp))))

(ert-deftest agent-repl-test-timestamp-keeps-fixed-fractional-width ()
  "A whole second still carries the fixture's full fractional width.
Trailing zeros may not be trimmed, because a trimmed record sorts out of
order against its neighbors."
  ;; Arrange: an instant with no subsecond component at all.
  (let ((instant (encode-time (list 56 34 12 28 7 2026 nil nil 0))))
    ;; Act
    (let ((stamp (agent-repl--log-rfc3339-timestamp instant)))
      ;; Assert
      (should (string-match "\\.\\([0-9]+\\)" stamp))
      (should (equal (length (match-string 1 stamp))
                     (agent-repl-test--timestamp-fixture "fractional_digits"))))))

(ert-deftest agent-repl-test-timestamp-uses-the-local-zone ()
  "An emitted timestamp round-trips to the instant it was given.
A UTC wall clock written with a local offset would not survive this, which
is what makes it a real check that the fixture's local zone is honored."
  ;; Arrange
  (let ((instant (encode-time (list 56 34 12 28 7 2026 nil nil 0))))
    ;; Act
    (let ((stamp (agent-repl--log-rfc3339-timestamp instant)))
      ;; Assert
      (should (time-equal-p (encode-time (parse-time-string stamp)) instant)))))

(ert-deftest agent-repl-test-timestamp-carries-a-numeric-offset ()
  "An emitted timestamp ends in a numeric offset rather than a Z suffix."
  ;; Act
  (let ((stamp (agent-repl--log-rfc3339-timestamp)))
    ;; Assert
    (should-not (string-suffix-p "Z" stamp))))

(provide 'test-log-timestamp)
;;; test-log-timestamp.el ends here
