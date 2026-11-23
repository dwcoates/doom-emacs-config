;;; test-heap-census.el --- ERT tests for agent-repl heap-census.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Run with:
;;   agent-repld test run -ert lisp/test-heap-census.el

;;; Code:

(load (expand-file-name "test-helpers.el" (file-name-directory
                                            (or load-file-name buffer-file-name)))
      nil t)

(defvar agent-repl-test-heap-census--records nil
  "Every record written, newest first, as (LEVEL TEXT).")

(defvar agent-repl-test-heap-census--gc-counts nil
  "What the stubbed `garbage-collect' answers.")

(defvar agent-repl-test-heap-census--now 0.0
  "What the stubbed `float-time' answers.")

(defvar agent-repl-test-heap-census-fixture-table nil
  "A hash table the variable census counts.")

(defvar agent-repl-test-heap-census-fixture-list nil
  "A list the variable census counts.")

(defconst agent-repl-test-heap-census--counts
  '((conses 16 1000 10) (symbols 48 100 0) (strings 32 50 5)
    (string-bytes 1 4000) (vector-slots 8 300 20) (floats 16 2 0)
    (intervals 56 0 0) (buffers 992 3) (heap 1024 200000 1000))
  "A collection's answer: 16,000 cons bytes, 4,800 symbol bytes, 1,600
string bytes, 4,000 string-data bytes, 2,400 vector-slot bytes, 32 float
bytes and 2,976 buffer bytes; the `heap' entry is the allocator's own.")

(defmacro agent-repl-test-heap-census--with (&rest body)
  "Run BODY with a recording logger, a stubbed collection and clock."
  (declare (indent 0))
  `(let ((agent-repl-test-heap-census--records nil)
         (agent-repl-test-heap-census--gc-counts agent-repl-test-heap-census--counts)
         (agent-repl-test-heap-census--now 1000.0)
         (agent-repl-heap-census--last nil)
         (agent-repl-heap-census--timer nil))
     (cl-letf (((symbol-function 'garbage-collect)
                (lambda () agent-repl-test-heap-census--gc-counts))
               ((symbol-function 'float-time)
                (lambda (&optional _time) agent-repl-test-heap-census--now))
               ((symbol-function 'agent-repl--emit-info)
                (agent-repl-test--record-leveled-into agent-repl-test-heap-census--records 'info))
               ((symbol-function 'agent-repl--emit-warn)
                (agent-repl-test--record-leveled-into agent-repl-test-heap-census--records 'warn)))
       ,@body)))

(defun agent-repl-test-heap-census--census-records ()
  "Every census record written, newest first."
  (cl-remove-if-not (lambda (r) (and (eq (car r) 'info)
                                     (string-prefix-p "elisp.heap-census: " (cdr r))))
                    agent-repl-test-heap-census--records))

;;; The live bytes

(ert-deftest agent-repl-heap-census-test-live-bytes-are-size-times-used-largest-first ()
  ;; Act
  (let ((by-kind (agent-repl-heap-census--live-bytes agent-repl-test-heap-census--counts)))
    ;; Assert
    (should (equal (car by-kind) '(conses . 16000)))
    (should (equal (alist-get 'buffers by-kind) 2976))))

(ert-deftest agent-repl-heap-census-test-live-bytes-leave-out-the-allocators-heap-entry ()
  ;; Act
  (let ((by-kind (agent-repl-heap-census--live-bytes agent-repl-test-heap-census--counts)))
    ;; Assert
    (should-not (assq 'heap by-kind))))

;;; Element counts

(ert-deftest agent-repl-heap-census-test-elements-of-a-hash-table-are-its-entries ()
  ;; Arrange
  (let ((table (make-hash-table)))
    (puthash 'a 1 table)
    (puthash 'b 2 table)
    ;; Act / Assert
    (should (= (agent-repl-heap-census--elements table) 2))))

(ert-deftest agent-repl-heap-census-test-elements-of-a-circular-list-are-counted-without-error ()
  ;; Arrange
  (let ((ring (list 1 2 3)))
    (setcdr (last ring) ring)
    ;; Act / Assert
    (should (integerp (agent-repl-heap-census--elements ring)))))

(ert-deftest agent-repl-heap-census-test-elements-of-a-vector-and-a-string-are-their-length ()
  ;; Act / Assert
  (should (= (agent-repl-heap-census--elements (make-vector 4 nil)) 4))
  (should (= (agent-repl-heap-census--elements "abc") 3)))

(ert-deftest agent-repl-heap-census-test-a-non-container-has-no-elements ()
  ;; Act / Assert
  (should-not (agent-repl-heap-census--elements 42)))

;;; The variables and buffers named

(ert-deftest agent-repl-heap-census-test-largest-variables-are-named-most-first ()
  ;; Arrange
  (let ((agent-repl-test-heap-census-fixture-table (make-hash-table))
        (agent-repl-test-heap-census-fixture-list (make-list 3 t)))
    (dotimes (i 5) (puthash i i agent-repl-test-heap-census-fixture-table))
    ;; Act
    (let ((largest (agent-repl-heap-census--largest-variables
                    "agent-repl-test-heap-census-fixture-" 10)))
      ;; Assert
      (should (equal largest '((agent-repl-test-heap-census-fixture-table . 5)
                               (agent-repl-test-heap-census-fixture-list . 3)))))))

(ert-deftest agent-repl-heap-census-test-largest-variables-name-at-most-n ()
  ;; Arrange
  (let ((agent-repl-test-heap-census-fixture-table (make-hash-table))
        (agent-repl-test-heap-census-fixture-list (make-list 3 t)))
    (dotimes (i 5) (puthash i i agent-repl-test-heap-census-fixture-table))
    ;; Act
    (let ((largest (agent-repl-heap-census--largest-variables
                    "agent-repl-test-heap-census-fixture-" 1)))
      ;; Assert
      (should (equal largest '((agent-repl-test-heap-census-fixture-table . 5)))))))

(ert-deftest agent-repl-heap-census-test-an-empty-variable-is-not-named ()
  ;; Arrange
  (let ((agent-repl-test-heap-census-fixture-table (make-hash-table))
        (agent-repl-test-heap-census-fixture-list nil))
    ;; Act / Assert
    (should-not (agent-repl-heap-census--largest-variables
                 "agent-repl-test-heap-census-fixture-" 10))))

(ert-deftest agent-repl-heap-census-test-largest-buffers-count-every-buffer-and-name-the-largest ()
  ;; Arrange
  (let ((big (generate-new-buffer " *heap-census-big*")))
    (unwind-protect
        (progn
          (with-current-buffer big (insert (make-string 2000000 ?x)))
          ;; Act
          (pcase-let ((`(,count ,bytes ,largest) (agent-repl-heap-census--largest-buffers 1)))
            ;; Assert
            (should (= count (length (buffer-list))))
            (should (>= bytes 2000000))
            (should (equal largest (list (cons (buffer-name big) 2000000))))))
      (kill-buffer big))))

(ert-deftest agent-repl-heap-census-test-a-narrowed-buffer-counts-its-whole-text ()
  ;; Arrange
  (let ((big (generate-new-buffer " *heap-census-narrowed*")))
    (unwind-protect
        (progn
          (with-current-buffer big
            (insert (make-string 3000000 ?x))
            (narrow-to-region 1 10))
          ;; Act
          (pcase-let ((`(,_count ,_bytes ,largest) (agent-repl-heap-census--largest-buffers 1)))
            ;; Assert
            (should (equal largest (list (cons (buffer-name big) 3000000))))))
      (kill-buffer big))))

;;; The census record

(ert-deftest agent-repl-heap-census-test-take-records-the-live-bytes-and-by-kind ()
  (agent-repl-test-heap-census--with
    ;; Act
    (should (agent-repl-heap-census-take))
    ;; Assert
    (let ((text (cdr (car (agent-repl-test-heap-census--census-records)))))
      (should (string-match-p "live_bytes=31808 " text))
      (should (string-match-p "by_kind=((conses \\. 16000)" text)))))

(ert-deftest agent-repl-heap-census-test-take-records-the-collectors-pacing ()
  (agent-repl-test-heap-census--with
    ;; Arrange
    (let ((gc-cons-threshold 800000) (gc-cons-percentage 0.1))
      ;; Act
      (agent-repl-heap-census-take)
      ;; Assert
      (let ((text (cdr (car (agent-repl-test-heap-census--census-records)))))
        (should (string-match-p "gc_cons_threshold=800000 gc_cons_percentage=0.1 " text))
        (should (string-match-p (format "gcs_done=%d " gcs-done) text))))))

(ert-deftest agent-repl-heap-census-test-take-records-the-buffers-and-variables ()
  (agent-repl-test-heap-census--with
    ;; Act
    (agent-repl-heap-census-take)
    ;; Assert
    (let ((text (cdr (car (agent-repl-test-heap-census--census-records)))))
      (should (string-match-p (format "buffers=%d buffer_bytes=" (length (buffer-list))) text))
      (should (string-match-p "largest_buffers=((" text))
      (should (string-match-p "largest_variables=((agent-repl" text)))))

(ert-deftest agent-repl-heap-census-test-take-records-the-resident-size ()
  (agent-repl-test-heap-census--with
    ;; Arrange
    (cl-letf (((symbol-function 'process-attributes) (lambda (_pid) '((rss . 148000)))))
      ;; Act
      (agent-repl-heap-census-take)
      ;; Assert
      (should (string-match-p "rss_kb=148000 "
                              (cdr (car (agent-repl-test-heap-census--census-records))))))))

(ert-deftest agent-repl-heap-census-test-an-inhibited-collection-is-a-warning-and-no-census ()
  (agent-repl-test-heap-census--with
    ;; Arrange
    (setq agent-repl-test-heap-census--gc-counts nil)
    ;; Act
    (should-not (agent-repl-heap-census-take))
    ;; Assert
    (should-not (agent-repl-test-heap-census--census-records))
    (should (cl-some (lambda (r) (and (eq (car r) 'warn)
                                      (string-prefix-p "elisp.heap-census.gc-inhibited:" (cdr r))))
                     agent-repl-test-heap-census--records))))

;;; The idle cadence

(ert-deftest agent-repl-heap-census-test-the-first-idle-takes-a-census ()
  (agent-repl-test-heap-census--with
    ;; Act
    (agent-repl-heap-census--on-idle)
    ;; Assert
    (should (= (length (agent-repl-test-heap-census--census-records)) 1))))

(ert-deftest agent-repl-heap-census-test-an-idle-inside-the-interval-takes-none ()
  (agent-repl-test-heap-census--with
    ;; Arrange
    (agent-repl-heap-census--on-idle)
    (setq agent-repl-test-heap-census--now
          (+ agent-repl-test-heap-census--now agent-repl-heap-census-interval-seconds -1))
    ;; Act
    (agent-repl-heap-census--on-idle)
    ;; Assert
    (should (= (length (agent-repl-test-heap-census--census-records)) 1))))

(ert-deftest agent-repl-heap-census-test-an-idle-once-the-interval-passed-takes-another ()
  (agent-repl-test-heap-census--with
    ;; Arrange
    (agent-repl-heap-census--on-idle)
    (setq agent-repl-test-heap-census--now
          (+ agent-repl-test-heap-census--now agent-repl-heap-census-interval-seconds))
    ;; Act
    (agent-repl-heap-census--on-idle)
    ;; Assert
    (should (= (length (agent-repl-test-heap-census--census-records)) 2))))

(ert-deftest agent-repl-heap-census-test-schedule-arms-a-repeating-idle-timer ()
  (agent-repl-test-heap-census--with
    ;; Arrange
    (let (armed)
      (cl-letf (((symbol-function 'run-with-idle-timer)
                 (lambda (secs repeat fn &rest _) (push (list secs repeat fn) armed) 'timer)))
        ;; Act
        (agent-repl-heap-census-schedule)
        ;; Assert
        (should (equal armed (list (list agent-repl-heap-census-idle-seconds t
                                         #'agent-repl-heap-census--on-idle))))
        (should (cl-some (lambda (r) (string-prefix-p "elisp.heap-census.scheduled:" (cdr r)))
                         agent-repl-test-heap-census--records))))))

(ert-deftest agent-repl-heap-census-test-rescheduling-cancels-the-earlier-timer ()
  (agent-repl-test-heap-census--with
    ;; Arrange
    (let ((earlier (timer-create)) cancelled)
      (setq agent-repl-heap-census--timer earlier)
      (cl-letf (((symbol-function 'run-with-idle-timer) (lambda (&rest _) 'timer))
                ((symbol-function 'cancel-timer) (lambda (timer) (push timer cancelled))))
        ;; Act
        (agent-repl-heap-census-schedule)
        ;; Assert
        (should (equal cancelled (list earlier)))))))

(provide 'test-heap-census)
;;; test-heap-census.el ends here
