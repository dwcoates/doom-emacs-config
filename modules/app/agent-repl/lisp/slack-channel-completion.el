;;; slack-channel-completion.el --- Slack channel completion in the composer -*- lexical-binding: t; -*-

;;; Commentary:

;; A WORD STARTING WITH `#' IN THE COMPOSER COMPLETES AGAINST THE USER'S
;; SLACK CHANNELS: every public and private channel the user is a member of
;; on their work Slack.  Typing `#gen' offers `#general'; `#back' offers
;; `#team-backend'.
;;
;; WHERE THE LIST COMES FROM.  The daemon reads it through the GNS CLI and
;; answers it on `ListSlackChannels'.  The list is the user's, not a
;; workspace's, so this Emacs keeps ONE copy for every composer.
;;
;; WHEN IT ASKS.  Each time the user types a `#' that begins a word -- a bare
;; `#' not yet asked for at this edit -- and once for a longer hash word
;; while no list is in hand.  The ask is asynchronous: completion is served
;; from the list already in hand and never waits on the daemon.  The
;; daemon's first read takes seconds, so the first `#' after a daemon starts
;; may offer nothing; the next one offers what that read found.
;;
;; A HASH WORD IS THE `#' SIGIL WORD (sigil-word.el): it may follow opening
;; brackets and quotes and drops trailing closing punctuation, so `(#gen).'
;; completes.  `##' and `#'foo' name no channel and fall through.
;;
;; MATCHING IS AT A NAME PART'S START, as the workspace names match: a
;; channel's parts split at `-', `_' and `.', and a word matches a channel
;; it begins a part of (`agent-repl-completion-match-part-starts').  `#12'
;; in prose (an issue number) therefore offers only channels a part of
;; which starts with `12', and a word matching nothing falls through.
;;
;; THE ORDER IS THE READER'S, then Slack's: a channel taken from this
;; completion before is offered first (completion-order.el), then the rest
;; in the order Slack listed them.
;;
;; EACH CHANNEL IS ANNOTATED with whether it is private and its purpose, cut
;; to `agent-repl-slack-channel-completion-annotation-width' columns; the
;; full purpose is the candidate's doc signature.
;;
;; A KNOWN CHANNEL IS PURPLE IN THE COMPOSER, as a known command is: every
;; hash word naming a channel in hand takes `agent-repl-composer-known'
;; (composer-mark.el).  An edit re-marks the lines it touched; a new list
;; re-marks every composer whole.

;;; Code:

;; Every log call is a log-site macro (log-site.el), needed where this
;; file is expanded; config.el loads it first.
(require 'log-site)

(require 'cl-lib)
(require 'subr-x)

(declare-function agent-repl--buffer-owner "agent-repl-core" (buf))
(declare-function agent-repl-annotation-column "agent-repl-core" (names gap))
(declare-function agent-repl-host-conn-or-primary "agent-repl-host" (ws))
(declare-function agent-repl-sigil-word-bounds "sigil-word" (sigil))
(declare-function agent-repl-sigil-word-table "sigil-word" (category sigil candidates &optional sort))
(declare-function agent-repl-sigil-word-ask-key "sigil-word" (bounds unanswered))
(declare-function agent-repl-sigil-word-next-known "sigil-word" (sigil known-p limit))
(declare-function agent-repl-completion-match-capf "completion-match" (beg end table &rest props))
(declare-function agent-repl-completion-match-part-starts "completion-match" (word names separators))
(declare-function agent-repl-completion-order-sorter "completion-order" (kind mentioned key))
(declare-function agent-repl-completion-order-exit-function "completion-order" (kind key))
(declare-function agent-repl-composer-mark-lines "composer-mark" (beg end tag find))
(declare-function agent-repl--input-composers "input" ())
(declare-function agent-repl-rpc-list-slack-channels "agent-repl-rpc" (conn request &rest keys))

(defcustom agent-repl-slack-channel-completion-annotation-width 40
  "Columns a channel's annotation (privacy, then purpose) may take in the popup."
  :type 'natnum
  :group 'agent-repl)

(defface agent-repl-slack-channel-completion-annotation
  '((t :inherit shadow))
  "Face of the annotation beside each channel the composer offers."
  :group 'agent-repl)

(defvar agent-repl--slack-channel-completion-channels nil
  "The channels every composer completes, or nil when none are known.
A list of decoded `SlackChannel' plists, in Slack's order, as the daemon
last answered them.  One copy for the Emacs: the channels are the user's.")

(defvar-local agent-repl--slack-channel-completion-asked-at nil
  "The edit this composer last asked for the channels at, or nil if never.
A cons (START . TICK), as `agent-repl--command-completion-asked-at' is.")

(defconst agent-repl--slack-channel-completion-separators "-_."
  "The characters that split a channel name into the parts a word may begin.")

;;;; ---- The hash word at point ----

(defun agent-repl--slack-channel-completion-bounds ()
  "Return (START . END) of the hash word around point, or nil.
The hash word is the `#' sigil word (`agent-repl-sigil-word-bounds')."
  (agent-repl-sigil-word-bounds ?#))

;;;; ---- Candidates ----

(defconst agent-repl--slack-channel-completion-category 'agent-repl-slack-channel
  "This capf's own completion category, named to no other table's.
Registered just below with the `substring' style: the candidates are
already narrowed to the channels the word begins a part of, and the style
only keeps a completion UI from narrowing them again by prefix.")

(add-to-list 'completion-category-defaults
             (list agent-repl--slack-channel-completion-category '(styles substring basic)))

(defun agent-repl--slack-channel-completion-name (candidate)
  "Return the channel name CANDIDATE completes: CANDIDATE less any leading `#'."
  (string-remove-prefix "#" candidate))

(defun agent-repl--slack-channel-completion-table (candidates &optional sort)
  "Return a completion table over CANDIDATES, `#'-prefixed channel names.
SORT, when given, is the order a completion UI offers them in.  The table
is the `#' sigil table (`agent-repl-sigil-word-table')."
  (agent-repl-sigil-word-table agent-repl--slack-channel-completion-category "#" candidates sort))

(defun agent-repl--slack-channel-completion-candidates (word channels)
  "Return the `#'-prefixed names of CHANNELS whose name WORD begins a part of."
  (mapcar (lambda (name) (concat "#" name))
          (agent-repl-completion-match-part-starts
           word (mapcar (lambda (channel) (plist-get channel :name)) channels)
           agent-repl--slack-channel-completion-separators)))

(defun agent-repl--slack-channel-completion-entry (channels candidate)
  "Return the channel of CHANNELS CANDIDATE names, or nil."
  (let ((name (agent-repl--slack-channel-completion-name candidate)))
    (cl-find-if (lambda (channel) (equal (plist-get channel :name) name)) channels)))

(defun agent-repl--slack-channel-completion-private-p (channel)
  "Return non-nil when CHANNEL is private."
  (eq (plist-get (plist-get channel :visibility) :arm) :private-channel))

(defun agent-repl--slack-channel-completion-annotation (channels candidates)
  "Return the `:annotation-function' over CANDIDATES, drawn from CHANNELS.
Each annotation is `private' for a private channel, then the channel's
purpose, cut to `agent-repl-slack-channel-completion-annotation-width'
columns, in a column two spaces past the longest candidate.  Empty for a
public channel stating no purpose."
  (let ((column (agent-repl-annotation-column candidates 2)))
    (lambda (candidate)
      (let* ((channel (agent-repl--slack-channel-completion-entry channels candidate))
             (parts (delq nil (list (when (agent-repl--slack-channel-completion-private-p channel)
                                      "private")
                                    (plist-get channel :purpose)))))
        (if parts
            (concat (make-string (max 1 (- column (string-width candidate))) ?\s)
                    (propertize (truncate-string-to-width
                                 (string-join parts "  ")
                                 agent-repl-slack-channel-completion-annotation-width nil nil "…")
                                'face 'agent-repl-slack-channel-completion-annotation))
          "")))))

(defun agent-repl--slack-channel-completion-purpose (channels candidate)
  "Return CANDIDATE's purpose from CHANNELS, or nil when it states none."
  (plist-get (agent-repl--slack-channel-completion-entry channels candidate) :purpose))

;;;; ---- Marking known channels ----

(defun agent-repl--slack-channel-completion-next-known (limit)
  "Return (START . END) of the next hash word naming a known channel before LIMIT.
Nil when none is left (`agent-repl-sigil-word-next-known').  Point is left
past the word answered."
  (when-let* ((channels agent-repl--slack-channel-completion-channels))
    (agent-repl-sigil-word-next-known
     ?# (lambda (word) (agent-repl--slack-channel-completion-entry channels word)) limit)))

(defun agent-repl--slack-channel-completion-mark (beg end)
  "Mark every known hash word on the lines BEG to END, and unmark the rest."
  (agent-repl-composer-mark-lines beg end 'agent-repl-slack-channel-completion-known
                                  #'agent-repl--slack-channel-completion-next-known))

(defun agent-repl--slack-channel-completion-on-change (beg end _len)
  "Re-mark the composer's lines BEG to END after an edit.
The composer's `after-change-functions' entry; unlogged, as it runs per
keystroke."
  (agent-repl--slack-channel-completion-mark beg end))

(defun agent-repl--slack-channel-completion-install (channels)
  "Make CHANNELS every composer's channels and re-mark every composer whole."
  (setq agent-repl--slack-channel-completion-channels channels)
  (dolist (buf (agent-repl--input-composers))
    (with-current-buffer buf
      (agent-repl--slack-channel-completion-mark (point-min) (point-max)))))

;;;; ---- Asking the daemon ----

(defun agent-repl--slack-channel-completion-answered (ws response)
  "Install RESPONSE, a decoded `ListSlackChannelsResponse'.
WS names the asking composer's workspace, for the log.  Pending leaves the
channels in hand as they stood; a failed read is warned of and leaves them
too, since the daemon's next read may succeed."
  (pcase (plist-get response :arm)
    (:success
     (let ((listing (plist-get response :value)))
       (pcase (plist-get listing :arm)
         (:listed
          (let ((channels (plist-get (plist-get listing :value) :channels)))
            (agent-repl--log ws "elisp.slack-channel-completion.answered ws={ws} outcome=listed channels={channels}"
                             :ws ws :channels (length channels))
            (agent-repl--slack-channel-completion-install channels)))
         (:pending
          (agent-repl--log ws "elisp.slack-channel-completion.answered ws={ws} outcome=pending" :ws ws))
         (other
          (agent-repl--error ws "elisp.slack-channel-completion.unknown-listing-arm ws={ws} arm={arm}"
                             :ws ws :arm (prin1-to-string other))))))
    (:error
     (let ((cause (plist-get (plist-get response :value) :cause)))
       (pcase (plist-get cause :arm)
         (:read-failed
          (agent-repl--warn ws "elisp.slack-channel-completion.read-failed ws={ws} detail={detail}"
                            :ws ws :detail (prin1-to-string (plist-get (plist-get cause :value) :detail))))
         (other
          (agent-repl--error ws "elisp.slack-channel-completion.unknown-cause-arm ws={ws} arm={arm}"
                             :ws ws :arm (prin1-to-string other))))))
    (other
     (agent-repl--error ws "elisp.slack-channel-completion.unknown-response-arm ws={ws} arm={arm}"
                        :ws ws :arm (prin1-to-string other)))))

(defun agent-repl-slack-channel-completion-refresh (ws)
  "Ask the daemon for the user's Slack channels, on behalf of WS's composer.
Asynchronous: the answer replaces every composer's channels when it
arrives.  No connection asks nothing and says so in the log -- the next
hash word asks again."
  (let ((conn (agent-repl-host-conn-or-primary ws)))
    (if (null conn)
        (agent-repl--log ws "elisp.slack-channel-completion.refresh-skipped ws={ws} reason=no-connection" :ws ws)
      (agent-repl--log ws "elisp.slack-channel-completion.refresh ws={ws}" :ws ws)
      (agent-repl-rpc-list-slack-channels
       conn nil
       :on-response (lambda (response)
                      (agent-repl--slack-channel-completion-answered ws response))
       :on-failure (lambda (detail)
                     (agent-repl--warn ws "elisp.slack-channel-completion.transport-failure ws={ws} detail={detail}"
                                       :ws ws :detail (prin1-to-string detail)))))))

;;;; ---- completion-at-point ----

(defun agent-repl--slack-channel-completion-ask-key (bounds)
  "Return the ask key for the hash word at BOUNDS, or nil when it asks nothing.
A bare `#' asks; a longer hash word asks only while no channels are in hand
\(`agent-repl-sigil-word-ask-key')."
  (agent-repl-sigil-word-ask-key bounds (null agent-repl--slack-channel-completion-channels)))

(defun agent-repl-slack-channel-completion-at-point ()
  "Complete the hash word at point against the user's Slack channels.
The composer's `completion-at-point-functions' entry.  Asks the daemon
again when a `#' has just been typed (see
`agent-repl--slack-channel-completion-ask-key'), and answers from the
channels in hand, or nil when none matches.  Deliberately unlogged per
call: completion runs per keystroke."
  (when-let* ((bounds (agent-repl--slack-channel-completion-bounds))
              (ws (agent-repl--buffer-owner (current-buffer))))
    (let ((key (agent-repl--slack-channel-completion-ask-key bounds)))
      (when (and key (not (equal key agent-repl--slack-channel-completion-asked-at)))
        (setq agent-repl--slack-channel-completion-asked-at key)
        (agent-repl-slack-channel-completion-refresh ws)))
    (let* ((channels agent-repl--slack-channel-completion-channels)
           (word (buffer-substring-no-properties (1+ (car bounds)) (cdr bounds)))
           (candidates (agent-repl--slack-channel-completion-candidates word channels)))
      (when candidates
        (agent-repl-completion-match-capf
         (car bounds) (cdr bounds)
         (agent-repl--slack-channel-completion-table
          candidates
          (agent-repl-completion-order-sorter
           'channel nil #'agent-repl--slack-channel-completion-name))
         :exit-function
         (agent-repl-completion-order-exit-function 'channel #'agent-repl--slack-channel-completion-name)
         :annotation-function
         (agent-repl--slack-channel-completion-annotation channels candidates)
         :company-docsig
         (lambda (candidate) (agent-repl--slack-channel-completion-purpose channels candidate)))))))

(provide 'slack-channel-completion)
;;; slack-channel-completion.el ends here
