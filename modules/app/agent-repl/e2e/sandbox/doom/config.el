;;; config.el --- sandbox profile config -*- lexical-binding: t; -*-

;;; Commentary:
;; Deliberately near-empty: the sandbox must not add behavior the host
;; profile does not have, or a test could pass here and fail there. The only
;; settings are the two the module's own templates read from the
;; environment, defaulted so nothing prompts.
;;; Code:

(setq user-full-name    (or (getenv "EMACS_FULL_NAME") "agent-repl sandbox")
      user-mail-address (or (getenv "EMACS_EMAIL") "sandbox@invalid"))

;; The e2e suite never runs real git, and a sandbox run has no network:
;; refuse to let anything block on a prompt.
(setq confirm-kill-emacs nil)
