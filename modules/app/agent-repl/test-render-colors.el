;;; test-render-colors.el --- The cross-language color contract -*- lexical-binding: t; -*-

;;; Commentary:

;; Emacs's corner of the contract in proto/vocab/render-colors.json.
;;
;; That file is the ONE table naming which of the five colors each
;; RenderState and each ErrorClass takes.  Go asserts against it, TypeScript
;; asserts against it, and this file is the third corner — which is what
;; makes a divergence between the three fail loudly instead of quietly.
;;
;; It checks the ASSIGNMENT, not the hex: each renderer keeps its own shades
;; (a tab-bar background and a CSS dot legitimately want different ones).
;; What may never differ is which color a state gets.
;;
;; It also finally CHECKS the two-sided contract `sidebar.el' has only ever
;; asserted in a comment.

;;; Code:

(require 'ert)
(require 'json)

(let ((dir (file-name-directory (or load-file-name buffer-file-name))))
  (load (expand-file-name "test-helpers.el" dir) nil t))

(defconst agent-repl-test--color-fixture
  (let* ((dir (file-name-directory (or load-file-name buffer-file-name)))
         (path (expand-file-name "proto/vocab/render-colors.json" dir)))
    (with-temp-buffer
      (insert-file-contents path)
      (let ((json-object-type 'alist)
            (json-array-type 'list)
            (json-key-type 'string))
        (json-read))))
  "The checked-in cross-language color assignment, decoded.")

(defun agent-repl-test--fixture (key)
  "Return the fixture's KEY section as an alist."
  (cdr (assoc key agent-repl-test--color-fixture)))

(defun agent-repl-test--state-keyword (enum-name)
  "Return the render keyword ENUM-NAME maps to, or nil."
  (cdr (assoc enum-name agent-repl--frontend-render-state-map)))

;;;; ---- Fixture coverage ------------------------------------------------

(ert-deftest agent-repl-test-colors-every-mapped-state-has-a-fixture-row ()
  "Every RenderState this frontend decodes has a color assignment.
A state Emacs accepts with no row would reach the palette needing a color
nobody agreed on."
  ;; Act / Assert
  (dolist (entry agent-repl--frontend-render-state-map)
    (should (assoc (car entry) (agent-repl-test--fixture "render_states")))))

(ert-deftest agent-repl-test-colors-every-fixture-state-is-decodable ()
  "Every colored RenderState is one this frontend can decode.
The other direction: a row for a state Emacs would reject is an
assignment nothing can honor."
  ;; Act / Assert
  (dolist (row (agent-repl-test--fixture "render_states"))
    (unless (equal (car row) "RENDER_STATE_UNSPECIFIED")
      (should (agent-repl-test--state-keyword (car row))))))

(ert-deftest agent-repl-test-colors-match-the-fixture ()
  "Emacs's per-state color NAME equals the fixture's, row for row.
This is the assertion the whole file exists for: the moment the three
renderers disagree about which color a state takes, this fails."
  ;; Act / Assert
  (dolist (row (agent-repl-test--fixture "render_states"))
    (let ((keyword (agent-repl-test--state-keyword (car row))))
      (when (and keyword (assq keyword agent-repl--state-color))
        (should (equal (alist-get keyword agent-repl--state-color) (cdr row)))))))

(ert-deftest agent-repl-test-colors-precedence-matches-the-fixture ()
  "Emacs restates the SSM's rank order without reordering it."
  ;; Act / Assert
  (should (equal agent-repl--color-precedence
                 (cdr (assoc "precedence" agent-repl-test--color-fixture)))))

(ert-deftest agent-repl-test-colors-every-named-color-has-a-value ()
  "Each of the five names resolves to a constant this renderer draws with.
A color the fixture ranks but Emacs cannot draw is an assignment nothing
can honor."
  ;; Act / Assert
  (dolist (name (cdr (assoc "colors" agent-repl-test--color-fixture)))
    (should (stringp (alist-get name agent-repl--color-by-name nil nil #'equal)))))

;;;; ---- The palette actually paints what the table says -----------------

(ert-deftest agent-repl-test-colors-every-colored-state-has-a-palette-row ()
  "Every state assigned one of the five colors HAS a tab-bar palette row.

This is the hole `interrupted\=' fell through.  It resolved, the shared
table assigned it green, and `agent-repl--tab-palette\=' had no row for
it — so `agent-repl--tab-appearance\=' fell back to
`agent-repl--tab-default\=', whose background is `unspecified\=', and the
tab painted uncolored.  A missing row is an assignment nothing honors,
and it must FAIL rather than be skipped."
  ;; Act / Assert
  (dolist (entry agent-repl--state-color)
    (let ((keyword (car entry))
          (name (cdr entry)))
      (unless (equal name "none")
        (let ((row (alist-get keyword agent-repl--tab-palette)))
          (should row)
          (should (plist-get (plist-get row :unselected) :bg)))))))

(ert-deftest agent-repl-test-colors-palette-paints-the-assigned-color ()
  "Each palette row's background IS its assigned color's constant.
Without this the shared table could agree with the fixture while the tab
bar painted something else entirely.

Deliberately UNGUARDED on the row existing: a missing row used to make
this pass silently, which is exactly how a colorless tab shipped.  The
test above proves the row is there; this proves it is right."
  ;; Act / Assert
  (dolist (entry agent-repl--state-color)
    (let* ((keyword (car entry))
           (name (cdr entry)))
      (unless (equal name "none")
        (let ((bg (plist-get (plist-get (alist-get keyword agent-repl--tab-palette)
                                        :unselected)
                             :bg)))
          (should (equal bg (alist-get name agent-repl--color-by-name nil nil #'equal))))))))

(ert-deftest agent-repl-test-colors-merge-states-spend-no-color ()
  "The merge states carry badges, never one of the five."
  ;; Act / Assert
  (dolist (keyword '(:merging :merge-queued :merge-conflict :merge-failed :merged))
    (should (equal (alist-get keyword agent-repl--state-color) "none"))))

;;;; ---- Exhaustiveness across the three Emacs tables --------------------

(ert-deftest agent-repl-test-colors-every-state-has-a-sidebar-row ()
  "Every decodable render state has a sidebar wire string.
A state absent from `agent-repl--sidebar-status-wire' is a violated
invariant, never a silent default dot."
  ;; Act / Assert
  (dolist (entry agent-repl--frontend-render-state-map)
    (should (assq (cdr entry) agent-repl--sidebar-status-wire))))

(ert-deftest agent-repl-test-colors-every-state-has-a-glyph ()
  "Every decodable render state has a workspace glyph."
  ;; Act / Assert
  (dolist (entry agent-repl--frontend-render-state-map)
    (should (assq (cdr entry) agent-repl-ws-state-icons))))

;;;; ---- One value, two surfaces -----------------------------------------
;;
;; The exhaustiveness checks above prove the rail and the tab bar share a
;; VOCABULARY.  These prove they share a SOURCE: both read
;; `agent-repl--ws-render-status', the pushed state, so there is no second
;; value for them to disagree about.  A shared vocabulary read from two
;; different places would still let a green tab sit beside a yellow dot.

(ert-deftest agent-repl-test-colors-the-dot-and-the-tab-read-one-value ()
  "The rail's dot and the tab bar's color both come from the pushed state.
Stubbing that ONE reader moves both surfaces together, which is what
makes their agreement structural rather than coincidental."
  (agent-repl-test--with-clean-state
    (agent-repl--ws-put "ws" :project-dir "/tmp/ws")
    (cl-letf (((symbol-function 'agent-repl--ws-open-p) (lambda (_ws) t))
              ((symbol-function 'agent-repl--ws-agent-open-p) (lambda (_ws) t)))
      (dolist (keyword '(:thinking :ready :idle-async :vendor-blocked :dead))
        (agent-repl--ws-put "ws" :pushed-render-state keyword)
        ;; The tab bar's key IS the pushed keyword.
        (should (eq (agent-repl--ws-bracket-state "ws") keyword))
        ;; The rail's wire string is that same keyword's row.
        (should (equal (agent-repl--sidebar-wire-status "ws")
                       (alist-get keyword agent-repl--sidebar-status-wire)))))))

(ert-deftest agent-repl-test-colors-the-dot-and-the-tab-take-the-same-color ()
  "For every state, the rail's dot and the tab bar resolve the same color.
Each renderer keeps its own shade; what may never differ is WHICH of the
five a state gets, and that is what the shared fixture pins."
  ;; Act / Assert
  (dolist (entry agent-repl--frontend-render-state-map)
    (let* ((keyword (cdr entry))
           (fixture-color (cdr (assoc (car entry) (agent-repl-test--fixture "render_states")))))
      ;; The rail has a row for it, and the tab bar assigns it the fixture's
      ;; color: one assignment, read by both.
      (should (assq keyword agent-repl--sidebar-status-wire))
      (should (equal (alist-get keyword agent-repl--state-color) fixture-color)))))

;;;; ---- Error classes ---------------------------------------------------

(ert-deftest agent-repl-test-colors-every-error-class-has-a-fixture-row ()
  "Every class Emacs decodes has a color assignment."
  ;; Act / Assert
  (dolist (entry agent-repl-failure-class-wire)
    (should (assoc (car entry) (agent-repl-test--fixture "error_classes")))))

(ert-deftest agent-repl-test-colors-internal-class-matches-its-state ()
  "The INTERNAL class takes the same color the degraded state takes.
Card color IS state color: a blue workspace explained by a card of some
other color is the drift this table prevents."
  ;; Act / Assert
  (should (equal (alist-get "ERROR_CLASS_INTERNAL"
                            (agent-repl-test--fixture "error_classes") nil nil #'equal)
                 (alist-get :degraded agent-repl--state-color))))

(ert-deftest agent-repl-test-colors-api-class-matches-its-state ()
  "The API class takes the same color the vendor-blocked state takes."
  ;; Act / Assert
  (should (equal (alist-get "ERROR_CLASS_API"
                            (agent-repl-test--fixture "error_classes") nil nil #'equal)
                 (alist-get :vendor-blocked agent-repl--state-color))))

(provide 'test-render-colors)
;;; test-render-colors.el ends here
