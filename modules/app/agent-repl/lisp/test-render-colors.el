;;; test-render-colors.el --- The cross-language color contract -*- lexical-binding: t; -*-

;;; Commentary:

;; Emacs's corner of the contract in proto/vocab/render-colors.json.
;;
;; That file is the ONE table naming which of the five colors each roster
;; status arm takes.  Go asserts against it, TypeScript asserts against it,
;; and this file is the third corner — which is what makes a divergence
;; between the three fail loudly instead of quietly.
;;
;; It checks the ASSIGNMENT, not the hex: each renderer keeps its own shades
;; (a tab-bar background and a CSS dot legitimately want different ones).
;; What may never differ is which color an arm gets, which overrides the tab
;; bar declares, and which glyph each merge arm draws.
;;
;; Run with:
;;   AGENT_REPL_FORBID_VENDOR_CALLS=1 emacs -batch -Q -l ert \
;;     -l lisp/test-render-colors.el -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'json)

(let ((dir (file-name-directory (or load-file-name buffer-file-name))))
  (load (expand-file-name "test-helpers.el" dir) nil t))

(defconst agent-repl-test--color-fixture
  (let* ((dir (file-name-directory (or load-file-name buffer-file-name)))
         (path (expand-file-name "../proto/vocab/render-colors.json" dir)))
    (with-temp-buffer
      (insert-file-contents path)
      (let ((json-object-type 'alist)
            (json-array-type 'list)
            (json-key-type 'string))
        (json-read))))
  "The checked-in cross-language color assignment, decoded.")

(defconst agent-repl-test--color-fixture-text
  (let* ((dir (file-name-directory (or load-file-name buffer-file-name)))
         (path (expand-file-name "../proto/vocab/render-colors.json" dir)))
    (with-temp-buffer
      (insert-file-contents path)
      (buffer-string)))
  "The fixture's raw text, for the assertions that are about what is ABSENT.")

(defun agent-repl-test--fixture (key)
  "Return the fixture's KEY section."
  (cdr (assoc key agent-repl-test--color-fixture)))

(defun agent-repl-test--arm-keyword (wire-name)
  "Return the elisp arm keyword for the fixture's WIRE-NAME (snake_case)."
  (intern (concat ":" (replace-regexp-in-string "_" "-" wire-name))))

(defun agent-repl-test--wire-name (arm)
  "Return the fixture's snake_case spelling of the elisp ARM keyword."
  (replace-regexp-in-string "-" "_" (substring (symbol-name arm) 1)))

;;;; ---- roster_status, row for row --------------------------------------

(ert-deftest agent-repl-test-colors-every-fixture-arm-has-a-local-row ()
  "Every arm the fixture assigns a color has a row in the local table.
An arm landing in the contract with no local row would reach the palette
needing a color nobody agreed on."
  ;; Act / Assert
  (dolist (entry (agent-repl-test--fixture "roster_status"))
    (should (assq (agent-repl-test--arm-keyword (car entry))
                  agent-repl-status-color-table))))

(ert-deftest agent-repl-test-colors-every-local-row-is-a-fixture-arm ()
  "Every arm the local table colors is one the fixture declares.
A local row for an arm the contract does not have is a private state, and
a private state is exactly the drift this file exists to catch."
  ;; Act / Assert
  (dolist (row agent-repl-status-color-table)
    (should (assoc (agent-repl-test--wire-name (car row))
                   (agent-repl-test--fixture "roster_status")))))

(ert-deftest agent-repl-test-colors-every-arm-takes-the-fixture-color ()
  "Each arm takes exactly the color the fixture assigns it."
  ;; Act / Assert
  (dolist (entry (agent-repl-test--fixture "roster_status"))
    (should (equal (alist-get (agent-repl-test--arm-keyword (car entry))
                              agent-repl-status-color-table)
                   (cdr entry)))))

(ert-deftest agent-repl-test-colors-the-arm-set-is-the-protos-arm-set ()
  "The colored arms are exactly the ones the codec decodes.
The codec's list comes from `RosterRow.status' in sidebar.proto, so this
is the assertion that ties the color table to the contract's own arm set
rather than to a hand-kept copy of it."
  ;; Act / Assert
  (should (equal (sort (mapcar (lambda (row) (symbol-name (car row)))
                               agent-repl-status-color-table)
                       #'string<)
                 (sort (mapcar #'symbol-name agent-repl-wire-roster-row-status-keywords)
                       #'string<))))

;;;; ---- surface_overrides.emacs_tab_bar ---------------------------------

(ert-deftest agent-repl-test-colors-every-declared-override-is-applied ()
  "Every override the fixture declares for the tab bar is the color it paints."
  ;; Arrange
  (let ((declared (cdr (assoc "emacs_tab_bar"
                              (agent-repl-test--fixture "surface_overrides")))))
    ;; Act / Assert
    (dolist (entry declared)
      (should (equal (alist-get (agent-repl-test--arm-keyword (car entry))
                                agent-repl-status-tab-bar-color-table)
                     (cdr entry))))))

(ert-deftest agent-repl-test-colors-no-undeclared-override-exists ()
  "The tab bar overrides nothing the fixture has not declared.
An undeclared divergence is illegible from the contract, which is the
whole reason the escape hatch is a declaration."
  ;; Arrange
  (let ((declared (cdr (assoc "emacs_tab_bar"
                              (agent-repl-test--fixture "surface_overrides")))))
    ;; Act / Assert
    (dolist (row agent-repl-status-tab-bar-color-table)
      (let ((shared (alist-get (car row) agent-repl-status-color-table)))
        (unless (equal (cdr row) shared)
          (should (assoc (agent-repl-test--wire-name (car row)) declared)))))))

(ert-deftest agent-repl-test-colors-an-unoverridden-arm-keeps-the-shared-color ()
  "An arm absent from the overrides takes the shared assignment verbatim."
  ;; Act / Assert
  (should (equal (alist-get :thinking agent-repl-status-tab-bar-color-table)
                 (alist-get :thinking agent-repl-status-color-table))))

;;;; ---- merge_glyphs ----------------------------------------------------

(ert-deftest agent-repl-test-colors-every-merge-arm-has-a-glyph ()
  "Every merge arm the fixture names a glyph for draws one here.
The merge arms spend no lifecycle color, so a missing glyph would leave
them reporting nothing at all."
  ;; Act / Assert
  (dolist (entry (agent-repl-test--fixture "merge_glyphs"))
    (should (stringp (alist-get (agent-repl-test--arm-keyword (car entry))
                                agent-repl-status-merge-glyphs)))))

(ert-deftest agent-repl-test-colors-no-extra-merge-glyph-exists ()
  "Only the arms the fixture names a glyph for have one."
  ;; Act / Assert
  (dolist (row agent-repl-status-merge-glyphs)
    (should (assoc (agent-repl-test--wire-name (car row))
                   (agent-repl-test--fixture "merge_glyphs")))))

;;;; ---- The five colors -------------------------------------------------

(ert-deftest agent-repl-test-colors-the-palette-is-the-fixtures-five ()
  "The renderer draws exactly the five colors the fixture declares."
  ;; Act / Assert
  (should (equal (sort (mapcar #'car agent-repl--color-by-name) #'string<)
                 (sort (copy-sequence (agent-repl-test--fixture "colors")) #'string<))))

(ert-deftest agent-repl-test-colors-precedence-matches-the-fixture-exactly ()
  "The precedence order is the fixture's, in order — not merely its set."
  ;; Act / Assert
  (should (equal agent-repl--color-precedence
                 (agent-repl-test--fixture "precedence"))))

(ert-deftest agent-repl-test-colors-every-assigned-color-is-one-of-the-five ()
  "No arm is assigned a color outside the five (or the real answer `none')."
  ;; Act / Assert
  (dolist (row agent-repl-status-tab-bar-color-table)
    (should (member (cdr row)
                    (cons "none" (agent-repl-test--fixture "colors"))))))

;;;; ---- Teal is gone ----------------------------------------------------

(ert-deftest agent-repl-test-colors-the-fixture-carries-no-teal ()
  "Teal left the contract with hibernation."
  ;; Act / Assert
  (should-not (member "teal" (agent-repl-test--fixture "colors"))))

(ert-deftest agent-repl-test-colors-no-arm-is-painted-teal ()
  "No local assignment names teal."
  ;; Act / Assert
  (should-not (cl-find "teal" agent-repl-status-tab-bar-color-table
                       :key #'cdr :test #'equal)))

(ert-deftest agent-repl-test-colors-the-renderer-defines-no-teal ()
  "The renderer holds no teal value to paint with."
  ;; Act / Assert
  (should-not (assoc "teal" agent-repl--color-by-name)))

;;;; ---- The retired enum ------------------------------------------------

(ert-deftest agent-repl-test-colors-the-fixture-carries-no-render-state-enum ()
  "States are named by their frontend.v1 oneof arms, never by RENDER_STATE_*.
The daemon resolves every view, so the arm it sets IS the state a
renderer paints and there is no second spelling to keep aligned."
  ;; Act / Assert
  (should-not (string-match-p "RENDER_STATE_" agent-repl-test--color-fixture-text)))

;;; test-render-colors.el ends here
