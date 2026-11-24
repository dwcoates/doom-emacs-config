;;; completion-order.el --- the order the composer's completions are offered in -*- lexical-binding: t; -*-

;;; Commentary:

;; A COMPOSER COMPLETION OFFERS WHAT THE READER IS LIKELY TO WANT FIRST.  The
;; slash commands (command-completion.el), the workspace names
;; (workspace-completion.el) and the Slack channels
;; (slack-channel-completion.el) are all offered in one order, in three
;; tiers (a channel is never mentioned, so it has only the first and last):
;;
;;   1. SELECTED: every candidate the reader took from a composer's completion
;;      before, the most recently taken first.  Taking one is completing it --
;;      the exit function's `finished' or `sole' -- in any composer, so a
;;      command taken in one workspace's composer is offered first in every
;;      other's too.  THE DAEMON OWNS THE RECORD: the exit function reports the
;;      taking (ReportComposerSelection), and the daemon keeps the record
;;      durably and pushes it whole on every host stream
;;      (`agent-repl-host-composer-selections', host.el), so it survives a
;;      restart of either side and Emacs never orders from a copy of its own.
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

;; Every log call is a log-site macro (log-site.el), needed where this
;; file is expanded; config.el loads it first.
(require 'log-site)

(require 'cl-lib)
(require 'subr-x)

(declare-function agent-repl-host-composer-selections "agent-repl-host" (kind))
(declare-function agent-repl-host-conn-or-primary "agent-repl-host" (ws))
(declare-function agent-repl-rpc-report-composer-selection "agent-repl-rpc" (conn request &rest keys))

(defun agent-repl--completion-order-answered (ws kind name response)
  "Log RESPONSE, the decoded `ReportComposerSelectionResponse' for NAME of KIND.
WS names the composer's workspace for the log.  The order itself arrives on
the host stream, never in the response."
  (pcase (plist-get response :arm)
    (:success
     (agent-repl--log ws "elisp.completion-order.recorded ws={ws} kind={kind} name={name}"
                      :ws ws :kind kind :name name))
    (:error
     (agent-repl--warn ws "elisp.completion-order.not-recorded ws={ws} kind={kind} name={name} cause={cause}"
                       :ws ws :kind kind :name name
                       :cause (prin1-to-string (plist-get (plist-get response :value) :cause))))
    (other
     (agent-repl--error ws "elisp.completion-order.unknown-response-arm ws={ws} kind={kind} name={name} arm={arm}"
                        :ws ws :kind kind :name name :arm (prin1-to-string other)))))

(defun agent-repl-completion-order-report-selected (ws kind name)
  "Report that the reader just took NAME, a completion of KIND, in WS's composer.
The daemon records it as the newest of KIND and pushes the restated record
on every host stream, which reorders every composer.  No connection reports
nothing and says so in the log."
  (cl-assert (memq kind '(command workspace channel)) nil "unknown completion kind: %S" kind)
  (if-let* ((conn (agent-repl-host-conn-or-primary ws)))
      (agent-repl-rpc-report-composer-selection
       conn (list :selection (list :arm (intern (concat ":" (symbol-name kind)))
                                   :value (list :name name)))
       :on-response (lambda (response)
                      (agent-repl--completion-order-answered ws kind name response))
       :on-failure (lambda (detail)
                     (agent-repl--warn ws "elisp.completion-order.transport-failure ws={ws} kind={kind} name={name} detail={detail}"
                                       :ws ws :kind kind :name name :detail (prin1-to-string detail))))
    (agent-repl--warn ws "elisp.completion-order.report-skipped ws={ws} kind={kind} name={name} reason=no-connection"
                      :ws ws :kind kind :name name)))

(defun agent-repl-completion-order-exit-function (ws kind key)
  "Return the `:exit-function' reporting a taken candidate of KIND.
WS is the composer's workspace, whose connection reports it.  KEY maps
the candidate string to the NAME it is reported under.  Only a finished
completion (`finished' or `sole') is a selection; `exact' leaves the
reader still choosing."
  (lambda (candidate status)
    (when (memq status '(finished sole))
      (agent-repl-completion-order-report-selected ws kind (funcall key candidate)))))

(defun agent-repl-completion-order-sort (kind mentioned key candidates)
  "Return CANDIDATES of KIND in the composer's order.
MENTIONED is the composer's workspace's mentions of KIND, the most recently
mentioned first; KEY maps a candidate to its NAME.  Taken candidates come
first, in the order of the daemon's record (the most recently taken first),
then MENTIONED ones in MENTIONED's order, then the rest in CANDIDATES's
order.  CANDIDATES is not modified."
  (cl-flet ((ranks (names)
              (let ((rank (make-hash-table :test 'equal))
                    (i 0))
                (dolist (name names rank)
                  (unless (gethash name rank)
                    (puthash name (cl-incf i) rank))))))
    (let ((taken (ranks (agent-repl-host-composer-selections kind)))
          (said (ranks mentioned)))
      (cl-flet ((tier (candidate)
                  (let* ((name (funcall key candidate))
                         (taken-rank (gethash name taken))
                         (said-rank (gethash name said)))
                    (cond (taken-rank (list 0 taken-rank))
                          (said-rank (list 1 said-rank))
                          (t (list 2 0))))))
        (let ((keyed (mapcar (lambda (c) (cons (tier c) c)) candidates)))
          (mapcar #'cdr
                  (sort keyed
                        (lambda (a b)
                          (let ((ta (car a)) (tb (car b)))
                            (or (< (car ta) (car tb))
                                (and (= (car ta) (car tb))
                                     (< (cadr ta) (cadr tb)))))))))))))

(defun agent-repl-completion-order-sorter (kind mentioned key)
  "Return a `display-sort-function' ordering candidates of KIND.
MENTIONED and KEY are as `agent-repl-completion-order-sort' takes them.  The
daemon's record of taken candidates is read each time it sorts, so a push
since the table was built is honored."
  (lambda (candidates)
    (agent-repl-completion-order-sort kind mentioned key candidates)))

(provide 'completion-order)
;;; completion-order.el ends here
