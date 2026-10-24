;;; config/splash/test-splash-config.el -*- lexical-binding: t; -*-
;;
;; Tests for modules/config/splash/config.el.
;;
;; Run with:
;;   emacs -batch -Q -l ert -l test-splash-config.el -f ert-run-tests-batch-and-exit
;;
;; Doom's dashboard module needs Doom's core to load, so the three things
;; the splash module reads from it are stood in for here: the dashboard
;; width, the banner padding, and `+doom-dashboard--center', copied from
;; modules/ui/doom-dashboard/config.el.  `+doom-dashboard-functions' starts
;; at Doom's default so the replacement is observed.

(require 'ert)
(require 'cl-lib)

(defconst test-splash--dir
  (file-name-directory (or load-file-name buffer-file-name)))

(defvar +doom-dashboard--width 80)
(defvar +doom-dashboard-banner-padding '(0 . 4))
(defvar +doom-dashboard-functions
  '(doom-dashboard-widget-banner
    doom-dashboard-widget-shortmenu
    doom-dashboard-widget-loaded
    doom-dashboard-widget-footer))

(defun +doom-dashboard--center (len s)
  (concat (make-string (ceiling (max 0 (- len (length s))) 2) ? )
          s))

(load (expand-file-name "config.el" test-splash--dir) nil t)

(defun test-splash--drawn-lines ()
  "Return the lines `+dwc/splash-widget-banner' draws, trailing newlines included."
  (with-temp-buffer
    (+dwc/splash-widget-banner)
    (split-string (buffer-string) "\n")))

(defun test-splash--face-at-text (text)
  "Return the face on the first character of TEXT in the drawn banner."
  (with-temp-buffer
    (+dwc/splash-widget-banner)
    (goto-char (point-min))
    (search-forward text)
    (get-text-property (match-beginning 0) 'face)))

(ert-deftest test-splash-dashboard-draws-our-banner-in-place-of-doom-s ()
  "The dashboard draws our banner first, then Doom's menu and load time."
  ;; Act / Assert
  (should (equal +doom-dashboard-functions
                 '(+dwc/splash-widget-banner
                   doom-dashboard-widget-shortmenu
                   doom-dashboard-widget-loaded))))

(ert-deftest test-splash-banner-draws-every-art-line ()
  "Each banner line appears, in order, at the top of the widget."
  ;; Act
  (let ((lines (mapcar #'string-trim (test-splash--drawn-lines))))
    ;; Assert
    (should (equal (cl-subseq lines 0 (length +dwc/splash-banner))
                   +dwc/splash-banner))))

(ert-deftest test-splash-banner-is-one-left-aligned-block ()
  "Every banner line starts at the same column, so the art stays intact."
  ;; Act
  (let ((lines (cl-subseq (test-splash--drawn-lines) 0 (length +dwc/splash-banner))))
    ;; Assert
    (should (= 1 (length (delete-dups
                          (mapcar (lambda (l) (- (length l) (length (string-trim-left l))))
                                  lines)))))))

(ert-deftest test-splash-banner-is-centered-in-the-dashboard ()
  "The banner block's left margin centers its widest line in the width."
  ;; Arrange
  (let ((widest (apply #'max (mapcar #'string-width +dwc/splash-banner))))
    ;; Act
    (let ((first (car (test-splash--drawn-lines))))
      ;; Assert
      (should (= (- (length first) (length (string-trim-left first)))
                 (ceiling (- +doom-dashboard--width widest) 2))))))

(ert-deftest test-splash-tagline-follows-the-banner-after-a-blank-line ()
  "The tagline sits one blank line under the art."
  ;; Act
  (let ((lines (test-splash--drawn-lines))
        (n (length +dwc/splash-banner)))
    ;; Assert
    (should (equal (list (nth n lines) (string-trim (nth (1+ n) lines)))
                   (list "" +dwc/splash-tagline)))))

(ert-deftest test-splash-banner-wears-the-dashboard-banner-face ()
  "The art wears Doom's `doom-dashboard-banner' face, so themes style it."
  ;; Act / Assert
  (should (eq (test-splash--face-at-text (car +dwc/splash-banner))
              'doom-dashboard-banner)))

(ert-deftest test-splash-tagline-wears-the-menu-description-face ()
  "The tagline wears `doom-dashboard-menu-desc', quieter than the art."
  ;; Act / Assert
  (should (eq (test-splash--face-at-text +dwc/splash-tagline)
              'doom-dashboard-menu-desc)))

(ert-deftest test-splash-banner-ends-with-the-bottom-padding ()
  "The widget ends with the padding's bottom count of newlines."
  ;; Arrange
  (let ((+doom-dashboard-banner-padding '(0 . 3)))
    ;; Act
    (let ((text (with-temp-buffer (+dwc/splash-widget-banner) (buffer-string))))
      ;; Assert
      (should (string-suffix-p (concat +dwc/splash-tagline "\n\n\n\n") text))
      (should-not (string-suffix-p "\n\n\n\n\n" text)))))

(ert-deftest test-splash-banner-leaves-the-top-padding-to-doom ()
  "The widget adds no top padding, which Doom applies when it centers."
  ;; Arrange
  (let ((+doom-dashboard-banner-padding '(5 . 0)))
    ;; Act / Assert
    (should-not (string-prefix-p "\n" (car (test-splash--drawn-lines))))))
