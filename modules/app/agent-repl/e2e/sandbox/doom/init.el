;;; init.el --- MINIMAL doom profile for the agent-repl e2e sandbox -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; This is NOT the user's personal Doom profile. It enables the smallest set
;; of Doom modules that `modules/app/agent-repl` actually needs, derived by
;; reading the module's `config.el`, `packages.el`, `AGENTS.md` and every
;; `lisp/*.el` source. Each entry below carries the reason it is here; a
;; module with no reason does not belong in this file.
;;
;; Deliberately ABSENT, with the evidence:
;;
;; - `:term vterm'. The host profile enables it, and `packages.el` mentions
;;   it, but no agent-repl source calls a vterm function: `sibling-popup.el`
;;   only names the `*doom:vterm*` BUFFER, and `history.el` only compares a
;;   stored `:frontend` symbol against `'vterm`. Omitting it also spares the
;;   image vterm's native build chain (cmake, libtool, libvterm).
;;
;; - every `:lang` module but `emacs-lisp`, all of `:completion`, `:checkers`
;;   and `:tools` bar magit, and the whole personal config (`personal-cc`,
;;   `personal-org`, `personal-bindings`, `chess`, themes, treemacs). None of
;;   them is referenced by agent-repl source, and the sandbox exists to run
;;   THIS module, not the user's editor.
;;
;; - `:ui doom`, `modeline`, `doom-dashboard`. Cosmetic; the sandbox is
;;   headless.
;;
;;; Code:

(doom! :ui
       ;; `config.el' installs a notes popup rule via `set-popup-rule!', and
       ;; `sibling-popup.el' / `close-panels-on-open.el' / `popup.el' all
       ;; drive Doom's popup system. `+defaults' matches the host profile.
       (popup +defaults)
       ;; `workspace.el' drives perspectives through persp-mode, which this
       ;; module provides. `test-integration-host.el' probes
       ;; `(featurep 'persp-mode)' directly.
       workspaces

       :editor
       ;; `keybindings.el' uses `map!' with evil state selectors (`:n') and
       ;; the leader prefix. `+everywhere' matches the host profile so the
       ;; bindings resolve the same way.
       (evil +everywhere)

       :emacs
       ;; `magit.el' and `worktree.el' both sit on Emacs VC / the git
       ;; porcelain's surroundings.
       vc

       :tools
       ;; `magit.el' requires magit at call time and needs its `defvar's.
       magit

       :lang
       ;; The module IS elisp; the ERT suites run under it.
       emacs-lisp

       :app
       ;; The subject under test. At run time the entrypoint points this
       ;; directory at the read-only repo mount.
       agent-repl

       :config
       ;; `+bindings' defines the leader map that `keybindings.el' hangs
       ;; every `map! :leader' form off. Without it those forms have no
       ;; prefix to attach to.
       (default +bindings))
