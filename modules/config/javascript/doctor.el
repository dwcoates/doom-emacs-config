;;; lang/javascript/doctor.el -*- lexical-binding: t; -*-

(defconst ts-doctor-eslint-executable "eslint_d"
  "The executable to use for ESLint checks.")

(defconst ts-doctor-eslint-fallback-executable "eslint"
  "Fallback executable if eslint_d isn't found.")

(defun ts-doctor--executable-found-p (exe)
  "Check if an executable EXE is available in the PATH."
  (if (executable-find exe)
      (progn
        (print! (green "Found %s executable in PATH" exe))
        t)
    (progn
      (print! (red "Missing %s executable in PATH" exe))
      nil)))

(defun ts-doctor-check-javascript-setup ()
  "Check the Javascript setup for the necessary tools."
  (print! (start "Checking Javascript environment..."))

  ;; Check for Javascript compiler
  (ts-doctor--executable-found-p "tsc")

  ;; Check for ESLint and eslint_d
  (or (ts-doctor--executable-found-p ts-doctor-eslint-executable)
      (ts-doctor--executable-found-p ts-doctor-eslint-fallback-executable))

  ;; Check for Node.js
  (ts-doctor--executable-found-p "node"))

(unless (featurep! :lang javascript)
  (warn! "The Javascript module is not enabled in your config!"))

(add-hook 'doom-doctor-hook #'ts-doctor-check-javascript-setup)
