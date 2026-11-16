;;; completion-order.el --- the order the composer's completions are offered in -*- lexical-binding: t; -*-

;;; Commentary:

;; A COMPOSER COMPLETION OFFERS WHAT THE READER IS LIKELY TO WANT FIRST.  The
;; slash commands (command-completion.el) and the workspace names
;; (workspace-completion.el) are both offered in one order, in three tiers:
;;
;;   1. SELECTED: every candidate the reader took from a composer's completion
;;      before, the most recently taken first.  Taking one is completing it --
;;      the exit function's `finished' or `sole' -- in any composer, so a
;;      command taken in one workspace's composer is offered first in every
;;      other's too.  Held for this Emacs's life.
;;   2. MENTIONED: every candidate this composer's own workspace mentioned in
;;      its final responses, the most recently mentioned first.  The daemon
;;      reads each response as it records it and keeps the record durably,
;;      and the host stream hands Emacs the record whole, first on every
;;      subscription and then on each change (`agent-repl-host-mentions',
;;      host.el), so the order is in hand before the composer asks and
;;      survives a restart of either side.
;;   3. THE REST, in the order the completion's own table offered them.
;;
;; THE ORDER RIDES THE TABLE'S METADATA, as its `display-sort-function' and
;; `cycle-sort-function', which company and `completion-at-point' both honor,
;; so no completion UI re-sorts it (`agent-repl-completion-match-table').
;; Ordering is unlogged: it runs per keystroke.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(defvar agent-repl--completion-order-selected (make-hash-table :test 'equal)
  "When each composer completion candidate was last taken, by (KIND . NAME).
KIND is `command' or `workspace'; NAME is the candidate without a slash.
The value is a counter from `agent-repl--completion-order-clock', so two
selections in one instant are still ordered.")

(defvar agent-repl--completion-order-clock 0
  "The count of selections taken in this Emacs, the next one's stamp.")

(defun agent-repl-completion-order-note-selected (kind name)
  "Record that the reader just took NAME, a completion of KIND, in a composer."
  (cl-assert (memq kind '(command workspace)) nil "unknown completion kind: %S" kind)
  (puthash (cons kind name) (cl-incf agent-repl--completion-order-clock)
           agent-repl--completion-order-selected))

(defun agent-repl-completion-order-exit-function (kind key)
  "Return the `:exit-function' recording a taken candidate of KIND.
KEY maps the candidate string to the NAME it is recorded under.  Only a
finished completion (`finished' or `sole') is a selection; `exact' leaves
the reader still choosing."
  (lambda (candidate status)
    (when (memq status '(finished sole))
      (agent-repl-completion-order-note-selected kind (funcall key candidate)))))

(defun agent-repl-completion-order-sort (kind mentioned key candidates)
  "Return CANDIDATES of KIND in the composer's order.
MENTIONED is the composer's workspace's mentions of KIND, the most recently
mentioned first; KEY maps a candidate to its NAME.  Taken candidates come
first, the most recently taken first, then MENTIONED ones in MENTIONED's
order, then the rest in CANDIDATES's order.  CANDIDATES is not modified."
  (let ((rank (make-hash-table :test 'equal))
        (i 0))
    (dolist (name mentioned)
      (unless (gethash name rank)
        (puthash name (cl-incf i) rank)))
    (cl-flet ((tier (candidate)
                (let* ((name (funcall key candidate))
                       (taken (gethash (cons kind name) agent-repl--completion-order-selected))
                       (said (gethash name rank)))
                  (cond (taken (list 0 (- taken)))
                        (said (list 1 said))
                        (t (list 2 0))))))
      (let ((keyed (mapcar (lambda (c) (cons (tier c) c)) candidates)))
        (mapcar #'cdr
                (sort keyed
                      (lambda (a b)
                        (let ((ta (car a)) (tb (car b)))
                          (or (< (car ta) (car tb))
                              (and (= (car ta) (car tb))
                                   (< (cadr ta) (cadr tb))))))))))))

(defun agent-repl-completion-order-sorter (kind mentioned key)
  "Return a `display-sort-function' ordering candidates of KIND.
MENTIONED and KEY are as `agent-repl-completion-order-sort' takes them."
  (lambda (candidates)
    (agent-repl-completion-order-sort kind mentioned key candidates)))

(provide 'completion-order)
;;; completion-order.el ends here
