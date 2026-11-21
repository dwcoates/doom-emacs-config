;;; heap-census.el --- what holds the editor's heap, measured -*- lexical-binding: t; -*-

;;; Commentary:

;; WHY IT EXISTS (todo 2026-10-25).  The live editor's heap stood at about
;; 145MB after 16 hours, and every garbage collection marks all of it: 581s
;; of collection over 8,559 collections, about 68ms each.  What holds the
;; heap was unmeasured, so no trim could be aimed.  This census measures it,
;; and the trim waits on its readings.
;;
;; THE CENSUS is one INFO record, `elisp.heap-census', stating:
;;
;;   the heap's live bytes   by kind (conses, strings, vectors, ...), from
;;                           the counts a full collection returns
;;                           (`garbage-collect'); the census's own collection
;;                           time is stated beside them
;;   the collector           collections done, seconds spent in them, and
;;                           the thresholds that pace them
;;   the process             its resident size (`process-attributes')
;;   buffers                 how many, their bytes in all, and the largest
;;                           `agent-repl-heap-census-top' by name and size
;;   agent-repl's variables  every bound `agent-repl' variable holding a hash
;;                           table, list, vector or string, the largest
;;                           `agent-repl-heap-census-top' by element count
;;
;; A variable's elements are COUNTED, never turned into bytes: Emacs offers
;; no measure of one object's size, and an estimate would read as a
;; measurement.  The counts name the holder; the live bytes by kind say
;; what it holds.
;;
;; IT RUNS WHEN THE EDITOR IS IDLE, at most once per
;; `agent-repl-heap-census-interval-seconds'.  The census forces a full
;; collection, which costs what any collection costs (about 68ms at 145MB);
;; on an idle timer no command waits on it, and once per half hour it adds
;; one collection to the editor's roughly nine a minute.  An editor never
;; idle long enough takes no census.

;;; Code:

(require 'cl-lib)
(require 'seq)

(declare-function agent-repl--info "core" (ws fmt &rest args))
(declare-function agent-repl--warn "core" (ws fmt &rest args))

(defconst agent-repl-heap-census--central
  '(:agent-repl-central "the editor's heap spans every workspace")
  "The log scope of every census record.")

(defconst agent-repl-heap-census-interval-seconds 1800
  "The fewest seconds between two censuses.")

(defconst agent-repl-heap-census-idle-seconds 30
  "Seconds the editor must be idle before a census runs.")

(defconst agent-repl-heap-census-top 10
  "How many buffers and how many variables a census names.")

(defconst agent-repl-heap-census-variable-prefix "agent-repl"
  "The prefix of the variables a census counts the elements of.")

(defvar agent-repl-heap-census--timer nil
  "The repeating idle timer that runs the census, or nil.")

(defvar agent-repl-heap-census--last nil
  "The `float-time' the last census ran at, or nil before the first.")

(defun agent-repl-heap-census--live-bytes (counts)
  "Answer the live bytes by kind in COUNTS, as `garbage-collect' returns them.
Each entry is (NAME SIZE USED [FREE]); the live bytes of a kind are SIZE
times USED.  The `heap' entry states the allocator's own totals in
kilobytes, not objects, and is left out.  The answer is an alist of
\(NAME . BYTES), largest first."
  (sort (cl-loop for (name size used) in counts
                 unless (eq name 'heap)
                 collect (cons name (* size used)))
        (lambda (a b) (> (cdr a) (cdr b)))))

(defun agent-repl-heap-census--elements (value)
  "Answer how many elements VALUE holds, or nil when it is no container.
A hash table counts its entries, a list its conses (a dotted or circular
list as far as `safe-length' walks it), a vector or a string its length."
  (cond
   ((hash-table-p value) (hash-table-count value))
   ((consp value) (safe-length value))
   ((or (vectorp value) (stringp value)) (length value))))

(defun agent-repl-heap-census--largest-variables (prefix n)
  "Answer the N bound variables named with PREFIX holding the most elements.
The answer is a list of (SYMBOL . ELEMENTS), most first; a variable whose
value is no container, or holds nothing, is left out."
  (let (found)
    (mapatoms
     (lambda (symbol)
       (when (and (boundp symbol)
                  (string-prefix-p prefix (symbol-name symbol)))
         (let ((elements (agent-repl-heap-census--elements (symbol-value symbol))))
           (when (and elements (> elements 0))
             (push (cons symbol elements) found))))))
    (seq-take (sort found (lambda (a b) (> (cdr a) (cdr b)))) n)))

(defun agent-repl-heap-census--largest-buffers (n)
  "Answer the buffer count, their bytes in all, and the N largest.
The answer is (COUNT BYTES LARGEST), LARGEST a list of (NAME . BYTES), most
first.  A buffer's bytes are its text's (`position-bytes' at its end)."
  (let ((sizes (mapcar (lambda (buffer)
                         (cons (buffer-name buffer)
                               (with-current-buffer buffer
                                 (save-restriction
                                   (widen)
                                   (1- (position-bytes (point-max)))))))
                       (buffer-list))))
    (list (length sizes)
          (apply #'+ (mapcar #'cdr sizes))
          (seq-take (sort sizes (lambda (a b) (> (cdr a) (cdr b)))) n))))

(defun agent-repl-heap-census-take ()
  "Measure what holds the heap and record it as one `elisp.heap-census' record.
A collection that does not run (`garbage-collect' answers nil while
collection is inhibited) is recorded at WARN and no census is recorded:
the heap's live bytes are the census's subject.  Answers non-nil when a
census was recorded."
  (let* ((started (float-time))
         (counts (garbage-collect))
         (gc-ms (round (* 1000 (- (float-time) started)))))
    (if (null counts)
        (progn
          (agent-repl--warn agent-repl-heap-census--central
                            "elisp.heap-census.gc-inhibited: the census's collection did not run; no census was recorded")
          nil)
      (let* ((by-kind (agent-repl-heap-census--live-bytes counts))
             (buffers (agent-repl-heap-census--largest-buffers agent-repl-heap-census-top))
             (variables (agent-repl-heap-census--largest-variables
                         agent-repl-heap-census-variable-prefix agent-repl-heap-census-top)))
        (agent-repl--info
         agent-repl-heap-census--central
         (concat "elisp.heap-census: live_bytes=%d by_kind=%S census_gc_ms=%d "
                 "gcs_done=%d gc_elapsed_s=%.1f gc_cons_threshold=%d gc_cons_percentage=%s "
                 "rss_kb=%S buffers=%d buffer_bytes=%d largest_buffers=%S largest_variables=%S")
         (apply #'+ (mapcar #'cdr by-kind)) by-kind gc-ms
         gcs-done gc-elapsed gc-cons-threshold gc-cons-percentage
         (alist-get 'rss (process-attributes (emacs-pid)))
         (nth 0 buffers) (nth 1 buffers) (nth 2 buffers) variables)
        t))))

(defun agent-repl-heap-census--on-idle ()
  "Take a census when none was taken in `agent-repl-heap-census-interval-seconds'."
  (let ((now (float-time)))
    (when (or (null agent-repl-heap-census--last)
              (>= (- now agent-repl-heap-census--last) agent-repl-heap-census-interval-seconds))
      (setq agent-repl-heap-census--last now)
      (agent-repl-heap-census-take))))

(defun agent-repl-heap-census-schedule ()
  "Arm the census on every idle stretch of `agent-repl-heap-census-idle-seconds'.
Re-arming cancels the timer an earlier call armed, so a reload never leaves
two running.  Answers the timer."
  (when (timerp agent-repl-heap-census--timer)
    (cancel-timer agent-repl-heap-census--timer))
  (setq agent-repl-heap-census--timer
        (run-with-idle-timer agent-repl-heap-census-idle-seconds t
                             #'agent-repl-heap-census--on-idle))
  (agent-repl--info agent-repl-heap-census--central
                    "elisp.heap-census.scheduled: idle_s=%d interval_s=%d top=%d"
                    agent-repl-heap-census-idle-seconds
                    agent-repl-heap-census-interval-seconds
                    agent-repl-heap-census-top)
  agent-repl-heap-census--timer)

(provide 'heap-census)
;;; heap-census.el ends here
