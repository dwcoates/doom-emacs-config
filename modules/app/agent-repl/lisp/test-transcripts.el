;;; test-transcripts.el --- Tests for transcripts.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for the transcript picker and restore: discovery under a
;; workspace's own config root, newest-first ordering, opening-prompt
;; previews, candidate labelling, and the EXPLICIT resume that reaches a
;; conversation `resume-resolve' refuses.

;;; Code:

(load (expand-file-name "test-helpers.el" (file-name-directory
                                            (or load-file-name buffer-file-name)))
      nil t)

;;;; ---- Fixtures ----

(defun agent-repl-test--write-transcript (dir uuid records &optional mtime)
  "Write RECORDS as UUID.jsonl under DIR, optionally stamping MTIME."
  (make-directory dir t)
  (let ((path (expand-file-name (concat uuid ".jsonl") dir)))
    (with-temp-file path
      (dolist (r records) (insert r "\n")))
    (when mtime (set-file-times path mtime))
    path))

(defun agent-repl-test--user-record (text)
  "A transcript line carrying TEXT as a user message."
  (json-encode `((type . "user") (message . ((role . "user") (content . ,text))))))

(defmacro agent-repl-test--transcript-ws (ws-var dir-var &rest body)
  "Run BODY with a workspace whose transcripts live in a temp projects root."
  (declare (indent 2))
  `(agent-repl-test--with-clean-state
     (let* ((root (make-temp-file "agent-repl-transcripts-" t))
            (,ws-var "tws")
            (,dir-var (expand-file-name "-p-q" (expand-file-name "projects" root))))
       (unwind-protect
           (progn
             (agent-repl--ws-put ,ws-var :project-dir "/p/q")
             (cl-letf (((symbol-function 'agent-repl--ai-title-projects-dir-for-ws)
                        (lambda (&rest _) (expand-file-name "projects" root))))
               ,@body))
         (delete-directory root t)))))

;;;; ---- Tests: discovery ----

(ert-deftest agent-repl-test-transcripts-none-when-directory-absent ()
  "A workspace with no transcript directory yields no candidates."
  ;; Arrange / Act / Assert
  (agent-repl-test--transcript-ws ws dir
    (should (null (agent-repl-transcripts-for-workspace ws)))))

(ert-deftest agent-repl-test-transcripts-finds-a-transcript ()
  "A jsonl under the workspace's projects root is a candidate."
  ;; Arrange
  (agent-repl-test--transcript-ws ws dir
    (agent-repl-test--write-transcript dir "uuid-one" (list (agent-repl-test--user-record "hello")))
    ;; Act
    (let ((entries (agent-repl-transcripts-for-workspace ws)))
      ;; Assert
      (should (equal (mapcar (lambda (e) (plist-get e :uuid)) entries) '("uuid-one"))))))

(ert-deftest agent-repl-test-transcripts-ignores-non-jsonl ()
  "Only .jsonl files are conversations."
  ;; Arrange
  (agent-repl-test--transcript-ws ws dir
    (make-directory dir t)
    (with-temp-file (expand-file-name "notes.txt" dir) (insert "not a transcript"))
    ;; Act / Assert
    (should (null (agent-repl-transcripts-for-workspace ws)))))

(ert-deftest agent-repl-test-transcripts-sorted-newest-first ()
  "Ordering is newest-first: a restore means the most recent conversation."
  ;; Arrange
  (agent-repl-test--transcript-ws ws dir
    (agent-repl-test--write-transcript dir "older" (list (agent-repl-test--user-record "a"))
                                       (encode-time 0 0 12 1 1 2020))
    (agent-repl-test--write-transcript dir "newer" (list (agent-repl-test--user-record "b"))
                                       (encode-time 0 0 12 1 1 2030))
    ;; Act
    (let ((entries (agent-repl-transcripts-for-workspace ws)))
      ;; Assert
      (should (equal (mapcar (lambda (e) (plist-get e :uuid)) entries) '("newer" "older"))))))

;;;; ---- Tests: preview ----

(ert-deftest agent-repl-test-transcript-preview-is-the-opening-prompt ()
  "The preview is the FIRST user prompt, which is what distinguishes two runs."
  ;; Arrange
  (agent-repl-test--transcript-ws ws dir
    (agent-repl-test--write-transcript
     dir "uuid-one"
     (list (agent-repl-test--user-record "first thing")
           (agent-repl-test--user-record "second thing")))
    ;; Act
    (let ((entry (car (agent-repl-transcripts-for-workspace ws))))
      ;; Assert
      (should (equal (plist-get entry :preview) "first thing")))))

(ert-deftest agent-repl-test-transcript-preview-nil-without-a-prompt ()
  "A transcript with no user prompt previews as nil rather than erroring."
  ;; Arrange
  (agent-repl-test--transcript-ws ws dir
    (agent-repl-test--write-transcript dir "uuid-one" (list "{\"type\":\"summary\"}"))
    ;; Act / Assert
    (should (null (plist-get (car (agent-repl-transcripts-for-workspace ws)) :preview)))))

(ert-deftest agent-repl-test-transcript-preview-survives-malformed-lines ()
  "A corrupt line is skipped, not fatal: the rest of the file still reads."
  ;; Arrange
  (agent-repl-test--transcript-ws ws dir
    (agent-repl-test--write-transcript
     dir "uuid-one"
     (list "{not json" (agent-repl-test--user-record "after the mess")))
    ;; Act / Assert
    (should (equal (plist-get (car (agent-repl-transcripts-for-workspace ws)) :preview)
                   "after the mess"))))

;;;; ---- Tests: candidate label ----

(ert-deftest agent-repl-test-transcript-label-flattens-newlines ()
  "A multi-line prompt must not break the candidate into unselectable rows."
  ;; Arrange
  (let ((entry (list :uuid "abcdef0123" :mtime (encode-time 0 0 12 1 1 2030)
                     :preview "line one\nline two")))
    ;; Act
    (let ((label (agent-repl--transcript-candidate-label entry)))
      ;; Assert
      (should-not (string-match-p "\n" label)))))

(ert-deftest agent-repl-test-transcript-label-names-a-promptless-transcript ()
  "A transcript with no prompt is still selectable and says so."
  ;; Arrange
  (let ((entry (list :uuid "abcdef0123" :mtime (encode-time 0 0 12 1 1 2030) :preview nil)))
    ;; Act / Assert
    (should (string-match-p "no recorded prompt"
                            (agent-repl--transcript-candidate-label entry)))))

(ert-deftest agent-repl-test-transcript-label-clips-a-long-prompt ()
  "A long prompt is clipped so the candidate line stays readable."
  ;; Arrange
  (let* ((agent-repl-transcript-preview-chars 10)
         (entry (list :uuid "abcdef0123" :mtime (encode-time 0 0 12 1 1 2030)
                      :preview "0123456789ABCDEFGHIJ")))
    ;; Act / Assert
    (should (string-match-p "0123456789…"
                            (agent-repl--transcript-candidate-label entry)))))

;;;; ---- Tests: resume ----

(ert-deftest agent-repl-test-transcript-resume-uses-explicit-mode ()
  "EXPLICIT, not continue: the resolver may refuse this uuid, which is the point."
  ;; Arrange
  (agent-repl-test--transcript-ws ws dir
    (let (captured)
      (cl-letf (((symbol-function 'agent-repl--frontend-after-create-session)
                 (lambda (cwd model resume-mode explicit-id &rest _)
                   (setq captured (list cwd model resume-mode explicit-id)))))
        ;; Act
        (agent-repl--resume-transcript ws (list :uuid "uuid-one" :path "/p/q/uuid-one.jsonl"))
        ;; Assert
        (should (eq (nth 2 captured) 'explicit))))))

(ert-deftest agent-repl-test-transcript-resume-names-the-chosen-uuid ()
  "The chosen conversation is the one requested, not whatever the daemon prefers."
  ;; Arrange
  (agent-repl-test--transcript-ws ws dir
    (let (captured)
      (cl-letf (((symbol-function 'agent-repl--frontend-after-create-session)
                 (lambda (cwd model resume-mode explicit-id &rest _)
                   (setq captured (list cwd model resume-mode explicit-id)))))
        ;; Act
        (agent-repl--resume-transcript ws (list :uuid "uuid-two" :path "/p/q/uuid-two.jsonl"))
        ;; Assert
        (should (equal (nth 3 captured) "uuid-two"))))))

(ert-deftest agent-repl-test-transcript-resume-refuses-without-a-project-dir ()
  "A workspace with nowhere to resume into fails loudly rather than silently."
  ;; Arrange
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "nodir" :project-dir nil)
    ;; Act / Assert
    (should-error (agent-repl--resume-transcript "nodir" (list :uuid "u" :path "/p"))
                  :type 'error)))

;;;; ---- Tests: restore across open workspaces ----

(ert-deftest agent-repl-test-restore-latest-picks-the-newest ()
  "Restore resumes the most recent transcript, not an arbitrary one."
  ;; Arrange
  (agent-repl-test--transcript-ws ws dir
    (agent-repl-test--write-transcript dir "older" (list (agent-repl-test--user-record "a"))
                                       (encode-time 0 0 12 1 1 2020))
    (agent-repl-test--write-transcript dir "newer" (list (agent-repl-test--user-record "b"))
                                       (encode-time 0 0 12 1 1 2030))
    (let (chosen)
      (cl-letf (((symbol-function 'agent-repl--ws-list-names) (lambda () (list ws)))
                ((symbol-function 'agent-repl--frontend-after-create-session)
                 (lambda (_cwd _model _mode explicit-id &rest _) (setq chosen explicit-id))))
        ;; Act
        (agent-repl-restore-latest-transcripts)
        ;; Assert
        (should (equal chosen "newer"))))))

(ert-deftest agent-repl-test-restore-latest-skips-a-workspace-with-none ()
  "A workspace with no transcripts is skipped rather than erroring the sweep."
  ;; Arrange
  (agent-repl-test--transcript-ws ws dir
    (let (calls)
      (cl-letf (((symbol-function 'agent-repl--ws-list-names) (lambda () (list ws)))
                ((symbol-function 'agent-repl--frontend-after-create-session)
                 (lambda (&rest _) (push t calls))))
        ;; Act
        (agent-repl-restore-latest-transcripts)
        ;; Assert
        (should (null calls))))))

(ert-deftest agent-repl-test-restore-latest-returns-the-workspaces-it-acted-on ()
  "The return value names what was restored, so a caller can report it."
  ;; Arrange
  (agent-repl-test--transcript-ws ws dir
    (agent-repl-test--write-transcript dir "only" (list (agent-repl-test--user-record "a")))
    (cl-letf (((symbol-function 'agent-repl--ws-list-names) (lambda () (list ws)))
              ((symbol-function 'agent-repl--frontend-after-create-session) (lambda (&rest _) nil)))
      ;; Act / Assert
      (should (equal (agent-repl-restore-latest-transcripts) (list ws))))))

;;; test-transcripts.el ends here
