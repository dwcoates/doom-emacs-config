;;; prompt-entries.el --- a prompt's entries, marked in the composer -*- lexical-binding: t; -*-

;;; Commentary:

;; A PROMPT ANSWERS A RESPONSE'S TREE BY LABEL (`3.1.2 - my answer'), and the
;; composer marks every such entry's label while it is typed, so the person
;; sees which lines the daemon will read as entries.  What an entry IS comes
;; from the one grammar every program reads it with
;; (tree-sitter-tldr/tldr_prompt): the daemon parses a sent prompt with it
;; (daemon/internal/prose), and the composer parses its own text with it
;; here, through `treesit'.  No regular expression stands in for it.
;;
;; THE GRAMMAR IS BUILT HERE, FROM THE CHECKOUT.  Emacs loads a tree-sitter
;; grammar as a native library, which nothing else in agent-repl builds:
;; the generated parser and its scanner are compiled with `cc' into
;; agent-repl's own cache (`agent-repl-prompt-entries--library-dir'), never
;; the user's own `~/.emacs.d/tree-sitter', and rebuilt whenever their
;; content changes (the source hash beside the library).  The build is
;; asynchronous: a composer opened before it finishes is marked the moment
;; it does.  A build that fails is recorded at ERROR with the compiler's
;; output, and the composer marks nothing while the grammar is broken.
;;
;; WHAT IS MARKED.  Every heading the grammar reads whose line carries no
;; marker: a line opening on a label and a marker (`1 🔧 text') is a pasted
;; tree's root, not an entry -- the daemon's own rule (ParsePrompt).  The
;; label and its separator are drawn in `agent-repl-prompt-entry-label'
;; through an overlay, restated after every edit.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'treesit)

(declare-function agent-repl--info "core" (ws fmt &rest args))
(declare-function agent-repl--error "core" (ws fmt &rest args))
(declare-function agent-repl--async-command "core" (label dir command callback &optional input))
(declare-function agent-repl-serial-create "core" (&rest slots))
(declare-function agent-repl-serial-send "core" (serial report send))
(declare-function agent-repl--log "core" (ws fmt &rest args))
(declare-function agent-repl--warn "core" (ws fmt &rest args))
(declare-function agent-repl-host-ref "host" (ws))
(declare-function agent-repl-host-conn-or-primary "host" (ws))
(declare-function agent-repl-host-unanswered-entries "host" (ws))
(declare-function agent-repl--input-flash "input" (ws text))
(declare-function agent-repl--input-buffer "input" (ws))
(declare-function agent-repl-rpc-report-composer-entries "rpc" (conn request &rest keys))
(declare-function agent-repl-rpc-find-entry-neighbor-sync "rpc" (conn request &optional timeout))
(declare-function evil-open-below "evil-commands" (count))
(declare-function evil-open-above "evil-commands" (count))
(declare-function evil-insert-state "evil-states" (&optional arg))
(declare-function evil-define-key* "evil-core" (state keymap key def &rest bindings))
(defvar agent-repl-input-mode-map)
(defvar agent-repl-host-update-functions)
(defvar agent-repl--frontend-root)

(defface agent-repl-prompt-entry-label
  '((t :inherit font-lock-constant-face :weight bold))
  "The face of an entry's label in the composer: a line answering a label."
  :group 'agent-repl)

(defcustom agent-repl-input-entry-marking t
  "Non-nil marks a prompt's entries in the composer as you type.
Read when a composer is born, so a change applies to composers created
after it."
  :type 'boolean
  :group 'agent-repl)

(defconst agent-repl-prompt-entries--language 'tldr_prompt
  "The tree-sitter language of a prompt's entries.")

(defconst agent-repl-prompt-entries--sources '("parser.c" "scanner.c")
  "The grammar's C sources, in its `src' directory.")

(defvar agent-repl-prompt-entries--library-dir
  (expand-file-name ".cache/agent-repl/tree-sitter" "~")
  "Where the built grammar library lives, agent-repl's own.
It is put on `treesit-extra-load-path', never the user's own grammar dir.")

(defvar agent-repl-prompt-entries--compiler "cc"
  "The C compiler the grammar is built with.")

(defun agent-repl-prompt-entries--grammar-dir ()
  "The grammar's generated `src' directory in this checkout."
  (expand-file-name "tree-sitter-tldr/tldr_prompt/src" agent-repl--frontend-root))

(defun agent-repl-prompt-entries--library ()
  "The path of the built grammar library."
  (expand-file-name (format "libtree-sitter-%s%s" agent-repl-prompt-entries--language
                            (if (eq system-type 'darwin) ".dylib" ".so"))
                    agent-repl-prompt-entries--library-dir))

(defun agent-repl-prompt-entries--stamp ()
  "The file recording which sources the built library was compiled from."
  (concat (agent-repl-prompt-entries--library) ".source"))

(defun agent-repl-prompt-entries--source-hash ()
  "The content hash of the grammar's C sources, as they stand now."
  (let ((dir (agent-repl-prompt-entries--grammar-dir)))
    (secure-hash
     'sha256
     (mapconcat (lambda (name)
                  (with-temp-buffer
                    (insert-file-contents-literally (expand-file-name name dir))
                    (buffer-string)))
                agent-repl-prompt-entries--sources
                "\0"))))

(defun agent-repl-prompt-entries--fresh-p ()
  "Whether the built library stands and was compiled from today's sources."
  (and (file-exists-p (agent-repl-prompt-entries--library))
       (file-exists-p (agent-repl-prompt-entries--stamp))
       (equal (with-temp-buffer
                (insert-file-contents (agent-repl-prompt-entries--stamp))
                (string-trim (buffer-string)))
              (agent-repl-prompt-entries--source-hash))))

(defconst agent-repl-prompt-entries--build-log-scope
  '(:agent-repl-central "the prompt grammar is built once for the whole Emacs")
  "Log scope of the grammar build's records.
One library serves every composer, so its build belongs to no workspace.")

(defvar agent-repl-prompt-entries--building nil
  "Non-nil while the grammar library is being built.")

(defvar agent-repl-prompt-entries--waiting nil
  "What waits for the grammar: functions called once it loads.")

(defun agent-repl-prompt-entries--on-load-path ()
  "Put the library's directory on `treesit-extra-load-path'."
  (add-to-list 'treesit-extra-load-path agent-repl-prompt-entries--library-dir))

(defun agent-repl-prompt-entries-ensure (on-ready)
  "Call ON-READY once the prompt grammar is loadable, building it when stale.
A fresh library is put on the load path and ON-READY called at once;
otherwise ON-READY waits for the one build in flight, which this starts
when none is."
  (if (agent-repl-prompt-entries--fresh-p)
      (progn
        (agent-repl-prompt-entries--on-load-path)
        (funcall on-ready))
    (push on-ready agent-repl-prompt-entries--waiting)
    (unless agent-repl-prompt-entries--building
      (agent-repl-prompt-entries--start-build))))

(defun agent-repl-prompt-entries--start-build ()
  "Compile the grammar library asynchronously; answer the waiting when it ends.
The compiler runs through `agent-repl--async-command', the one spawn of a
program that is not git."
  (let* ((dir (agent-repl-prompt-entries--grammar-dir))
         (library (agent-repl-prompt-entries--library))
         (hash (agent-repl-prompt-entries--source-hash))
         (command (append (list agent-repl-prompt-entries--compiler
                                "-std=c11" "-fPIC" "-shared" "-O2" "-I" dir "-o" library)
                          (mapcar (lambda (name) (expand-file-name name dir))
                                  agent-repl-prompt-entries--sources))))
    (make-directory agent-repl-prompt-entries--library-dir t)
    (agent-repl--info agent-repl-prompt-entries--build-log-scope "elisp.prompt-entries.build-started library=%s source=%s" library hash)
    (setq agent-repl-prompt-entries--building t)
    (agent-repl--async-command
     "prompt-grammar" dir command
     (lambda (ok output)
       (agent-repl-prompt-entries--build-ended ok library hash output)))))

(defun agent-repl-prompt-entries--build-ended (ok library hash output)
  "Finish the build into LIBRARY from sources HASH, which succeeded when OK.
OUTPUT is the compiler's own words.  A success stamps HASH beside LIBRARY
and calls everything waiting; a failure is recorded at ERROR with OUTPUT,
and nothing waiting is called."
  (setq agent-repl-prompt-entries--building nil)
  (let ((waiting (nreverse agent-repl-prompt-entries--waiting)))
    (setq agent-repl-prompt-entries--waiting nil)
    (if (not ok)
        (agent-repl--error agent-repl-prompt-entries--build-log-scope "elisp.prompt-entries.build-failed library=%s output=%S"
                           library (string-trim output))
      (with-temp-file (agent-repl-prompt-entries--stamp) (insert hash "\n"))
      (agent-repl-prompt-entries--on-load-path)
      (agent-repl--info agent-repl-prompt-entries--build-log-scope "elisp.prompt-entries.built library=%s waiting=%d" library (length waiting))
      (mapc #'funcall waiting))))

;;;; ---- Marking the composer ----

(defvar-local agent-repl-prompt-entries--parser nil
  "This composer's parser of the prompt grammar, or nil before it loads.")

(defvar-local agent-repl-prompt-entries--edits 0
  "How many text edits this composer has had, only ever rising.
Reported with the entries, so the webapp can tell the person typed.")

(defvar-local agent-repl-prompt-entries--ws nil
  "The workspace this composer is, once marking is enabled.")

(defvar-local agent-repl-prompt-entries--serial nil
  "This composer's report serializer (`agent-repl-serial-send'), once enabled.")

(defun agent-repl-prompt-entries-enable (ws)
  "Mark the entries of WS's composer, the current buffer, from now on.
Marking starts once the grammar loads, which may be after this returns,
and so does reporting them to the daemon after each command
\(`agent-repl-prompt-entries--report').  Off when
`agent-repl-input-entry-marking' is nil."
  (if (not agent-repl-input-entry-marking)
      (agent-repl--info ws "elisp.prompt-entries.enable ws=%s outcome=off reason=agent-repl-input-entry-marking-nil" ws)
    (let ((buf (current-buffer)))
      (agent-repl-prompt-entries-ensure
       (lambda ()
         (when (buffer-live-p buf)
           (with-current-buffer buf
             (setq agent-repl-prompt-entries--parser
                   (treesit-parser-create agent-repl-prompt-entries--language)
                   agent-repl-prompt-entries--ws ws
                   agent-repl-prompt-entries--serial (agent-repl-serial-create))
             (add-hook 'after-change-functions #'agent-repl-prompt-entries--on-change nil t)
             (add-hook 'post-command-hook #'agent-repl-prompt-entries--report nil t)
             (agent-repl-prompt-entries-mark)
             (agent-repl--info ws "elisp.prompt-entries.enabled ws=%s" ws))))))))

(defun agent-repl-prompt-entries--on-change (_beg _end _len)
  "Re-mark the composer's entries after an edit.
The composer's `after-change-functions' entry; unlogged, as it runs per
keystroke.  A whole-buffer pass: an edit can move where every later entry
begins, and a composer is short.  Counts the edit, which the next report
carries."
  (cl-incf agent-repl-prompt-entries--edits)
  (agent-repl-prompt-entries-mark))

(defun agent-repl-prompt-entries--entries ()
  "Every entry of the current buffer, in order, as (LABEL BEG END).
LABEL is the label as written; BEG and END span the label and its
separator.  A heading carrying a marker is a pasted tree's root and is left
out.  THE ONE READ of the composer's entries: marking, reporting and
opening a line beside an entry all go through it."
  (let (entries)
    (dolist (capture (treesit-query-capture agent-repl-prompt-entries--parser
                                            '((heading) @heading)))
      (let* ((heading (cdr capture))
             (marker (treesit-search-subtree heading (lambda (n) (equal (treesit-node-type n) "marker")) nil nil 1))
             (label (treesit-search-subtree heading (lambda (n) (equal (treesit-node-type n) "label")) nil nil 1))
             (separator (treesit-search-subtree heading (lambda (n) (equal (treesit-node-type n) "separator")) nil nil 1)))
        (when (and label (null marker))
          (push (list (string-trim (buffer-substring-no-properties (treesit-node-start label) (treesit-node-end label)))
                      (treesit-node-start label)
                      (treesit-node-end (or separator label)))
                entries))))
    (nreverse entries)))

(defun agent-repl-prompt-entries--entry-at-point (entries)
  "The label of the entry among ENTRIES whose line point is on, or nil."
  (let ((bol (line-beginning-position)))
    (car (seq-find (lambda (entry) (= (nth 1 entry) bol)) entries))))

(defface agent-repl-prompt-entry-unanswered
  '((t :inherit warning :weight bold))
  "The face of an entry's label that answers nothing.
No response the entry targets holds its label."
  :group 'agent-repl)

(defun agent-repl-prompt-entries-mark ()
  "Restate the overlays marking the current composer's entry labels.
A label the daemon says answers nothing (`agent-repl-host-unanswered-entries')
wears `agent-repl-prompt-entry-unanswered'; every other label
`agent-repl-prompt-entry-label'."
  (when agent-repl-prompt-entries--parser
    (let ((unanswered (and agent-repl-prompt-entries--ws
                           (agent-repl-host-unanswered-entries agent-repl-prompt-entries--ws))))
      (remove-overlays (point-min) (point-max) 'agent-repl-prompt-entry t)
      (dolist (entry (agent-repl-prompt-entries--entries))
        (let ((ov (make-overlay (nth 1 entry) (nth 2 entry))))
          (overlay-put ov 'agent-repl-prompt-entry t)
          (overlay-put ov 'face (if (member (nth 0 entry) unanswered)
                                    'agent-repl-prompt-entry-unanswered
                                  'agent-repl-prompt-entry-label)))))))

(defun agent-repl-prompt-entries--host-updated (ws _host)
  "Re-mark WS's composer after a host push, which may move what answers nothing.
An `agent-repl-host-update-functions' entry."
  (let ((buf (agent-repl--input-buffer ws)))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (agent-repl-prompt-entries-mark)))))

(add-hook 'agent-repl-host-update-functions #'agent-repl-prompt-entries--host-updated)

;;;; ---- Reporting the composer's entries ----
;;
;; THE DAEMON IS TOLD WHAT THE COMPOSER HOLDS, after every command that moves
;; it: the label of every entry, and the one point is on
;; (ReportComposerEntries).  It answers with state: the node that entry
;; answers, highlighted in the feed, and the labels that answer nothing,
;; marked here.  ONE REPORT IS IN FLIGHT AT A TIME, the latest one owed once
;; it is answered (`agent-repl-serial-send'): two reports racing could land
;; in either order, and the daemon would then hold the older.

(defvar-local agent-repl-prompt-entries--reported nil
  "The (LABELS AT EDITS) last sent from this composer, or nil.")

(defun agent-repl-prompt-entries--report ()
  "Report the composer's entries to the daemon when they moved since last sent.
The composer's `post-command-hook' entry.  While a report is in flight the
new one is owed, and sent once that one is answered."
  (when (and agent-repl-prompt-entries--parser agent-repl-prompt-entries--ws)
    (let* ((entries (agent-repl-prompt-entries--entries))
           (state (list (mapcar #'car entries) (agent-repl-prompt-entries--entry-at-point entries)
                        agent-repl-prompt-entries--edits)))
      (unless (equal state agent-repl-prompt-entries--reported)
        (let ((buf (current-buffer)))
          (agent-repl-serial-send
           agent-repl-prompt-entries--serial
           (lambda () (when (buffer-live-p buf) (with-current-buffer buf (agent-repl-prompt-entries--report))))
           (lambda (done) (agent-repl-prompt-entries--send state done))))))))

(defun agent-repl-prompt-entries--send (state done)
  "Send STATE, (LABELS AT EDITS), as this composer's report; call DONE once it ends.
With no daemon-minted ref or no connection nothing is sent and nothing is
marked reported, so the next command tries again."
  (let* ((ws agent-repl-prompt-entries--ws)
         (buf (current-buffer))
         (ref (agent-repl-host-ref ws))
         (conn (agent-repl-host-conn-or-primary ws)))
    (if (not (and ref conn))
        (progn
          (agent-repl--log ws "elisp.prompt-entries.report-skipped ws=%s ref=%s conn=%s" ws (and ref t) (and conn t))
          (funcall done))
      (setq agent-repl-prompt-entries--reported state)
      (agent-repl--log ws "elisp.prompt-entries.report ws=%s labels=%S at=%S" ws (car state) (cadr state))
      (agent-repl-rpc-report-composer-entries
       conn (list :workspace ref :labels (car state)
                  :at-point (if (cadr state) (list :arm :entry :value (list :label (cadr state))) (list :arm :none))
                  :edits (nth 2 state))
       :on-response (lambda (response) (agent-repl-prompt-entries--answered ws state response done))
       :on-failure (lambda (detail) (agent-repl-prompt-entries--failed buf ws state detail done))))))

(defun agent-repl-prompt-entries--answered (ws state response done)
  "Record the daemon's RESPONSE to WS's report of STATE, then end it with DONE."
  (pcase (plist-get response :arm)
    (:success nil)
    (:error (agent-repl--info ws "elisp.prompt-entries.report-refused ws=%s labels=%S cause=%S"
                              ws (car state) (plist-get (plist-get response :value) :cause)))
    (other (agent-repl--error ws "elisp.prompt-entries.report-unknown-arm ws=%s arm=%S" ws other)))
  (funcall done))

(defun agent-repl-prompt-entries--failed (buf ws state detail done)
  "Record BUF's report of STATE for WS, which the daemon never answered.
DETAIL is the transport's failure.  Nothing is marked reported, so the next
command sends it again; DONE ends the report."
  (agent-repl--warn ws "elisp.prompt-entries.report-failure ws=%s labels=%S detail=%S" ws (car state) detail)
  (when (buffer-live-p buf)
    (with-current-buffer buf
      (setq agent-repl-prompt-entries--reported nil)))
  (funcall done))

;;;; ---- Opening a line beside an entry ----
;;
;; `o' and `O' in command mode open a line below or above, as ever; on an
;; entry line the new line is filled with the next node's label (below) or
;; the previous one's (above), in the tree that entry targets, so the next
;; entry answers the next node.  The daemon finds the node
;; (FindEntryNeighbor), for the label of the line the key was pressed on.
;; A neighbor whose entry the composer already holds that way is never
;; opened twice: point goes to the end of that entry's line, in insert mode,
;; as if the line had just been opened.

(defconst agent-repl-prompt-entries--neighbor-timeout 1.0
  "Seconds `o' and `O' wait for the daemon to name the neighbor label.")

(defun agent-repl-prompt-entries--neighbor (label way)
  "The label of LABEL's node WAY from it in its target tree, or nil.
WAY is `:next', `:previous' or `:following' (EntryNeighborDirection).
Nil, with the reason logged, when the composer is not reporting, no ref or
connection stands, the daemon refused, or it never answered."
  (let* ((ws agent-repl-prompt-entries--ws)
         (ref (and ws (agent-repl-host-ref ws)))
         (conn (and ws (agent-repl-host-conn-or-primary ws))))
    (if (not (and ref conn))
        (progn (agent-repl--log ws "elisp.prompt-entries.neighbor-skipped ws=%s label=%s way=%s" ws label way) nil)
      (condition-case err
          (let ((response (agent-repl-rpc-find-entry-neighbor-sync
                           conn (list :workspace ref :label label
                                      :direction (list :arm way))
                           agent-repl-prompt-entries--neighbor-timeout)))
            (pcase (plist-get response :arm)
              (:success
               (let ((neighbor (plist-get (plist-get response :value) :neighbor)))
                 (agent-repl--log ws "elisp.prompt-entries.neighbor ws=%s label=%s way=%s neighbor=%S"
                                  ws label way neighbor)
                 (when (eq (plist-get neighbor :arm) :found)
                   (plist-get (plist-get neighbor :value) :label))))
              (:error
               (agent-repl--info ws "elisp.prompt-entries.neighbor-refused ws=%s label=%s cause=%S"
                                 ws label (plist-get (plist-get response :value) :cause))
               nil)
              (other
               (agent-repl--error ws "elisp.prompt-entries.neighbor-unknown-arm ws=%s arm=%S" ws other)
               nil)))
        (agent-repl-connect-error
         (agent-repl--warn ws "elisp.prompt-entries.neighbor-failure ws=%s label=%s error=%S" ws label err)
         nil)))))

(defun agent-repl-prompt-entries-opening (label)
  "The words that open a blank entry answering LABEL: `LABEL - '.
Every line Emacs opens on an entry is opened with them: `o' and `O' beside
an entry, and a click on a response's branch in the webview
\(`agent-repl-input-compose-entry')."
  (concat label " - "))

(defun agent-repl-prompt-entries--label-at-point ()
  "The label of the entry point's line holds, or nil off an entry line."
  (and agent-repl-prompt-entries--parser
       (agent-repl-prompt-entries--entry-at-point (agent-repl-prompt-entries--entries))))

(defun agent-repl-prompt-entries--existing (label next)
  "The start of the nearest entry answering LABEL below (NEXT) or above point's line, or nil."
  (let* ((bol (line-beginning-position))
         (entries (agent-repl-prompt-entries--entries)))
    (nth 1 (seq-find (lambda (entry)
                       (and (equal (car entry) label)
                            (if next (> (nth 1 entry) bol) (< (nth 1 entry) bol))))
                     (if next entries (reverse entries))))))

(defun agent-repl-prompt-entries--open (next)
  "Open a line below (NEXT) or above point's line, filled with a neighbor.
On an entry line whose label has a neighbor that way, the new line opens
on `LABEL - '; otherwise it opens empty, as `evil-open-below' does.  A
neighbor already holding an entry that way is not opened again: point
moves to the end of that entry's line, in insert mode."
  (let ((label (agent-repl-prompt-entries--label-at-point)))
    (agent-repl-prompt-entries--open-on
     label (and label (agent-repl-prompt-entries--neighbor label (if next :next :previous))) next)))

(defun agent-repl-prompt-entries--open-on (label neighbor next)
  "From LABEL's line, open a line below (NEXT) or above, answering NEIGHBOR.
THE ONE OPENING of a line beside an entry, for `o', `O', `M-j' and `M-k':
the new line opens on `NEIGHBOR - ', or empty when NEIGHBOR is nil.  An
entry already answering NEIGHBOR that way is not opened again: point moves
to the end of that entry's line, in insert mode."
  (let ((existing (and neighbor (agent-repl-prompt-entries--existing neighbor next))))
    (if existing
        (progn
          (goto-char existing)
          (end-of-line)
          (evil-insert-state)
          (agent-repl--log agent-repl-prompt-entries--ws "elisp.prompt-entries.open-existing ws=%s label=%s neighbor=%s at=%d"
                           agent-repl-prompt-entries--ws label neighbor existing))
      (if next (evil-open-below 1) (evil-open-above 1))
      (when neighbor (insert (agent-repl-prompt-entries-opening neighbor))))))

(defun agent-repl-prompt-entries-open-below ()
  "Open a line below point; on an entry line, fill it with the next label.
The composer's `o' in command mode: the next node's label, in the tree the
entry targets."
  (interactive)
  (agent-repl-prompt-entries--open t))

(defun agent-repl-prompt-entries-open-above ()
  "Open a line above point; on an entry line, fill it with the previous label.
The composer's `O' in command mode: the previous node's label, in the tree
the entry targets."
  (interactive)
  (agent-repl-prompt-entries--open nil))

;;;; ---- Opening an entry's sibling ----
;;
;; `M-j' and `M-k' on an entry line in insert mode open the entry's sibling:
;; `M-j' opens a line above answering the previous one (`3.3 - ' opens
;; `3.2 - '), `M-k' a line below answering the next one (`3.4 - ').  The
;; sibling is the label's last numeral stepped by one, whether or not the
;; tree the entry targets holds it, so nothing is asked of the daemon.  A
;; first sibling has none above: the line stays as it was and the composer
;; says so.  An entry already answering the sibling that way is gone to
;; rather than opened twice, as `o' and `O' do.  Off an entry line either key
;; is what it always was.

(defun agent-repl-prompt-entries--sibling (label offset)
  "The label of LABEL's sibling OFFSET places along, or nil before the first.
Only LABEL's last numeral moves: `3.3' with -1 is `3.2', with 1 `3.4', and
`3.1' with -1 is nil.  LABEL is one the grammar read, so a label that is no
dotted run of numerals is an error."
  (unless (string-match "\\`\\(\\(?:[0-9]+\\.\\)*\\)\\([0-9]+\\)\\'" label)
    (error "prompt-entries: %S is no dotted label" label))
  (let ((parent (match-string 1 label))
        (index (+ (string-to-number (match-string 2 label)) offset)))
    (and (> index 0) (concat parent (number-to-string index)))))

(defun agent-repl-prompt-entries--open-sibling (next)
  "Open a line below (NEXT) or above point's entry, answering its sibling that way.
A first sibling has none above: nothing opens, and the composer says so."
  (let* ((ws agent-repl-prompt-entries--ws)
         (label (agent-repl-prompt-entries--label-at-point))
         (sibling (and label (agent-repl-prompt-entries--sibling label (if next 1 -1)))))
    (cond
     ((null label)
      (agent-repl--info ws "elisp.prompt-entries.sibling-off-entry ws=%s next=%s point=%d" ws next (point))
      (user-error "Point is on no entry line"))
     ((null sibling)
      (agent-repl--info ws "elisp.prompt-entries.sibling-none ws=%s label=%s" ws label)
      (agent-repl--input-flash ws (format "%s is the first sibling" label)))
     (t
      (agent-repl--info ws "elisp.prompt-entries.sibling ws=%s label=%s next=%s sibling=%s" ws label next sibling)
      (agent-repl-prompt-entries--open-on label sibling next)))))

(defun agent-repl-prompt-entries-open-sibling-above ()
  "Open a line above point's entry, answering its previous sibling.
The composer's `M-j' in insert mode on an entry line: `3.3 - ' opens
`3.2 - ' above it."
  (interactive)
  (agent-repl-prompt-entries--open-sibling nil))

(defun agent-repl-prompt-entries-open-sibling-below ()
  "Open a line below point's entry, answering its next sibling.
The composer's `M-k' in insert mode on an entry line: `3.3 - ' opens
`3.4 - ' below it."
  (interactive)
  (agent-repl-prompt-entries--open-sibling t))

;;;; ---- Stepping an entry along its tree ----
;;
;; `C-o' and `C-i' on an entry line, in command or insert mode, relabel the
;; entry to answer another node of the tree the entry targets, the entry's
;; words kept.  `C-o' steps on, to the node read after it: its first child
;; (`3.2 - ' becomes `3.2.1 - '), else its next sibling (`3.3 - '), else the
;; next sibling of its nearest ancestor that has one (`4 - ').  `C-i' steps
;; back, to its previous sibling (`3.2 - ' becomes `3.1 - '), else up to its
;; parent (`3.1 - ' becomes `3 - '), so a root steps back to the previous
;; root (`3 - ' becomes `2 - ').  The daemon finds the node
;; (FindEntryNeighbor's `following' and `previous').  Off an entry line either
;; key is what it always was; `C-i' is Doom's GUI-only `[C-i]' event, so `TAB'
;; is never taken.

(defun agent-repl-prompt-entries--relabel (from to)
  "Replace point's entry label FROM with TO, point keeping its place after it.
Point after the label stays as far from the line's end as it was; point in
or before the label moves to the new label's end."
  (let* ((beg (line-beginning-position))
         (end (+ beg (length from)))
         (from-eol (- (line-end-position) (point)))
         (after (> (point) end)))
    (unless (equal (buffer-substring-no-properties beg end) from)
      (error "prompt-entries: the entry line does not open on its label %S" from))
    (delete-region beg end)
    (goto-char beg)
    (insert to)
    (when after (goto-char (- (line-end-position) from-eol)))))

(defun agent-repl-prompt-entries--step (way none)
  "Relabel point's entry to answer its node's neighbor WAY.
WAY is `:following' or `:previous' (EntryNeighborDirection).  A node with
no neighbor that way keeps its label, and the composer flashes NONE, a
format of the label."
  (let* ((ws agent-repl-prompt-entries--ws)
         (label (agent-repl-prompt-entries--label-at-point))
         (to (and label (agent-repl-prompt-entries--neighbor label way))))
    (cond
     ((null label)
      (agent-repl--info ws "elisp.prompt-entries.step-off-entry ws=%s way=%s point=%d" ws way (point))
      (user-error "Point is on no entry line"))
     ((null to)
      (agent-repl--info ws "elisp.prompt-entries.step-none ws=%s label=%s way=%s" ws label way)
      (agent-repl--input-flash ws (format none label)))
     (t
      (agent-repl-prompt-entries--relabel label to)
      (agent-repl--info ws "elisp.prompt-entries.stepped ws=%s label=%s way=%s to=%s" ws label way to)))))

(defun agent-repl-prompt-entries-step-on ()
  "Relabel point's entry to answer the node read after its own.
The composer's `C-o' on an entry line: `3.2 - ' becomes `3.2.1 - ', or
`3.3 - ' when 3.2 has no child, or `4 - ' when 3 has no later child, its
words kept, the node found in the tree the entry targets.  The tree's
last node keeps its label, and the composer says so."
  (interactive)
  (agent-repl-prompt-entries--step :following "%s is the last node to answer"))

(defun agent-repl-prompt-entries-step-back ()
  "Relabel point's entry to answer the node before its own, `C-o' undone.
The composer's `C-i' on an entry line: `3.2 - ' becomes `3.1 - ', or
`3 - ' when 3.2 is its parent's first child, its words kept, the node
found in the tree the entry targets, as `O' finds it.  A root steps
back to the previous root (`3 - ' becomes `2 - ').  The first root keeps
its label, and the composer says so."
  (interactive)
  (agent-repl-prompt-entries--step :previous "%s is the first node to answer"))

(defun agent-repl-prompt-entries--on-entry-filter (command)
  "COMMAND on an entry line, else nil, so the key off one keeps its own binding."
  (and (agent-repl-prompt-entries--label-at-point) command))

(defun agent-repl-prompt-entries--on-entry-binding (command)
  "A binding running COMMAND on an entry line and falling through elsewhere.
THE ONE SHAPE of every entry-only key: a `menu-item' whose filter
\(`agent-repl-prompt-entries--on-entry-filter') yields nil off an entry
line, so the key lookup goes on to the next map."
  `(menu-item "" ,command :filter agent-repl-prompt-entries--on-entry-filter))

(defconst agent-repl-prompt-entries--entry-keys
  `((,(kbd "C-o") . ,(agent-repl-prompt-entries--on-entry-binding #'agent-repl-prompt-entries-step-on))
    ([C-i] . ,(agent-repl-prompt-entries--on-entry-binding #'agent-repl-prompt-entries-step-back)))
  "The composer's entry-only keys in command and insert mode, as (KEY . BINDING).
`C-o' steps on and `C-i' steps back on an entry line, and both fall through
elsewhere.  `[C-i]' is the event Doom decodes a GUI `C-i' into when a
binding for it stands, so `TAB' keeps its own meaning.")

(defconst agent-repl-prompt-entries--insert-entry-keys
  `((,(kbd "M-j") . ,(agent-repl-prompt-entries--on-entry-binding #'agent-repl-prompt-entries-open-sibling-above))
    (,(kbd "M-k") . ,(agent-repl-prompt-entries--on-entry-binding #'agent-repl-prompt-entries-open-sibling-below)))
  "The composer's entry-only keys in insert mode, as (KEY . BINDING).
`M-j' opens the previous sibling above and `M-k' the next one below on an
entry line, and both fall through elsewhere.")

(defun agent-repl-prompt-entries-bind-keys ()
  "Bind the composer's entry-aware keys.
`o' and `O' in command mode open a line on a neighbor's label; `C-o' and
`C-i' in command and insert mode step an entry line's entry
\(`agent-repl-prompt-entries--entry-keys'), and `M-j' and `M-k' in insert
mode open its sibling (`agent-repl-prompt-entries--insert-entry-keys')."
  (evil-define-key* 'normal agent-repl-input-mode-map
    "o" #'agent-repl-prompt-entries-open-below
    "O" #'agent-repl-prompt-entries-open-above)
  (pcase-dolist (`(,key . ,binding) agent-repl-prompt-entries--entry-keys)
    (evil-define-key* '(normal insert) agent-repl-input-mode-map key binding))
  (pcase-dolist (`(,key . ,binding) agent-repl-prompt-entries--insert-entry-keys)
    (evil-define-key* 'insert agent-repl-input-mode-map key binding)))

(with-eval-after-load 'evil
  (agent-repl-prompt-entries-bind-keys))

(provide 'prompt-entries)

;;; prompt-entries.el ends here
