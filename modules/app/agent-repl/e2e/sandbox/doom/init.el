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

;; NATIVE COMPILATION IS AVAILABLE AND USED; JIT COMPILATION IS NOT.
;;
;; The image's Emacs is a native-comp build (the Dockerfile and
;; e2e-sandbox.sh both assert `native-comp-available-p'), and Doom's packages
;; are compiled AHEAD OF TIME into the image by `doom sync --aot'. What is
;; turned off here is the JIT half: the asynchronous compilation Emacs starts
;; for elisp it loads that has no `.eln' yet.
;;
;; MEASURED, and this is why. Every scenario gets a FRESH `~/.emacs.d' staged
;; out of the image, so the `.eln' files a JIT run produces are thrown away
;; the moment the test ends -- and then produced again by the next scenario,
;; forty-five times a run, for exactly the same sources. With scenarios
;; running concurrently that dead work is not merely waste: `ps` inside the
;; container showed several `emacs -no-comp-spawn --batch ... async-comp'
;; children per scenario competing for the four CPUs the VM has, and Doom's
;; own boot then missed its measured 3.5s bound in tests that had nothing to
;; do with compilation.
;;
;; Nothing observable is given up. No assertion in the layer reads a `.eln',
;; and elisp behaves identically interpreted, byte-compiled or natively
;; compiled -- the only difference is speed, and this is the setting that
;; makes it faster here rather than slower.
;; THE BOOT BREADCRUMB. A boot that never publishes its readiness stamp
;; reports NOTHING today: the frame is graphical, so Emacs's own messages go
;; to the frame rather than to the pty, and a boot that dies or blocks before
;; `agent-repl-e2e--boot' runs cannot write the stamp -- not even the failed
;; one the boot hook's own `condition-case' writes. The pty is empty, no
;; server exists to ask, and the failure says only "no readiness stamp".
;;
;; So each stage of the boot appends a line to a file beside the stamp, and
;; the Go side prints it when the boot bound is missed. It is one
;; `write-region' per stage on a tmpfs.
(defun agent-repl-e2e--breadcrumb (stage)
  "Append STAGE and the time to the boot breadcrumb file, if one is named."
  (let ((ready (getenv "AGENT_REPL_E2E_READY")))
    (when (and ready (not (string-empty-p ready)))
      (ignore-errors
        (write-region (format "%.3f %s\n" (float-time) stage) nil
                      (concat ready ".progress") 'append 'silent)))))

(agent-repl-e2e--breadcrumb "init.el reached")

;; ---------------------------------------------------------------------------
;; THE SERVER SOCKET IS CLAIMED HERE, BEFORE ANYTHING CAN LOAD `server'.
;; ---------------------------------------------------------------------------
;;
;; THIS IS NOT A PRECAUTION, IT IS THE FIX FOR A MEASURED FAILURE. Doom's own
;; `lisp/doom-editor.el' carries
;;
;;   (use-package! server
;;     :when (display-graphic-p)
;;     :after-call doom-first-input-hook doom-first-file-hook
;;     :defer 1
;;     :config (unless (server-running-p) (server-start)))
;;
;; and this layer's frame IS graphical (the panel is an xwidget webview, so
;; `StartEmacs' runs Emacs on an Xvfb). That `:config' is a
;; `with-eval-after-load' on `server', so it fires the instant ANY code loads
;; the feature -- including `config.el''s own boot hook, whose first act is
;; `(require 'server)' -- and it fires BEFORE that hook sets `server-name'.
;; Its `:defer 1' timer can also load the feature on its own, at one second,
;; which is inside the boot the hook is still finishing.
;;
;; So the socket Doom binds is `server-socket-dir' plus the DEFAULT name
;; "server", and `server-socket-dir' is derived from XDG_RUNTIME_DIR or
;; TMPDIR -- both EMPTY in this container, leaving /tmp/emacs<uid> -- never
;; from HOME. HOME is what this layer makes unique per scenario; /tmp is
;; shared by every scenario in the container. Concurrent boots therefore raced
;; between that `server-running-p' and its `bind', and the loser died with
;;
;;   Doom failed to initialize: Cannot bind server socket: Address already in use
;;
;; naming a path in NO scenario's scratch root -- which is exactly why the
;; boot hook's occupant pre-flight and the Go side's root listing both
;; reported nothing: both were reading the scenario's own socket path, which
;; the boot had not reached yet. Measured 2026-09-05: 2 of 45 scenarios on a
;; quiet box, both dead at boot with an empty root listing and a breadcrumb
;; that stops at "boot hook entered".
;;
;; Two settings close it, and both must land before any load of `server',
;; which is why they are here in init.el and not in the boot hook:
;;
;;   * `server-name' is the scenario's own ABSOLUTE socket path, so
;;     `server--file-name' resolves to it no matter who calls `server-start'.
;;     An absolute name makes `server-socket-dir' irrelevant to the bind.
;;   * `server-socket-dir' is moved under this scenario's root anyway, so a
;;     default-named server started by anything else this profile loads --
;;     magit's `with-editor', an interactive `M-x server-start' -- still
;;     cannot reach a path another scenario shares.
;;
;; Gated on AGENT_REPL_E2E_SERVER, which only the Go layer sets: an
;; interactive `e2e-sandbox.sh shell' keeps stock Doom behavior.
(let ((socket (getenv "AGENT_REPL_E2E_SERVER")))
  (when (and socket (not (string-empty-p socket)))
    (unless (file-name-absolute-p socket)
      (error "AGENT_REPL_E2E_SERVER=%s must be absolute: a relative `server-name' resolves against `server-socket-dir', which every scenario in this container shares"
             socket))
    ;; `setq' ahead of server.el's own `defvar's, which is the ordinary way
    ;; to pin a library's variable before it loads: `defvar' leaves an
    ;; already-bound variable alone.
    (setq server-name socket
          server-socket-dir (expand-file-name
                             "server-sockets" (file-name-directory socket)))))

(setq native-comp-jit-compilation nil
      ;; The Emacs 29 spelling, kept so the profile does not silently stop
      ;; working on an older Emacs than the image's 30.2.
      native-comp-deferred-compilation nil)

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

       ;; `:app agent-repl' is DELIBERATELY ABSENT, exactly as it is in the
       ;; host profile's own init.el, and for the host's reason: Doom loads
       ;; `:config default' LAST of all modules, and its `+bindings' rebuilds
       ;; the leader's prefix maps wholesale (`SPC o' and `SPC TAB' become
       ;; fresh `:prefix-map' keymaps, `SPC w' becomes `evil-window-map').
       ;; A module loaded in the `:app' phase therefore has every one of its
       ;; `map! :leader' forms discarded before the frame is ever up.
       ;;
       ;; The module is loaded at the END of this profile's `config.el'
       ;; instead -- $DOOMDIR/config.el runs after every module's config.el,
       ;; so `keybindings.el' lands last and wins, which is precisely the
       ;; arrangement the user runs. The entrypoint still points
       ;; $DOOMDIR/modules/app/agent-repl at the read-only repo mount; only
       ;; WHEN it is loaded changes.
       ;;
       ;; `modules/app/agent-repl/packages.el' declares no packages, so its
       ;; absence from the `doom!' block costs `doom sync' nothing.

       :config
       ;; `+bindings' defines the leader map that `keybindings.el' hangs
       ;; every `map! :leader' form off. Without it those forms have no
       ;; prefix to attach to.
       (default +bindings))

;; ---------------------------------------------------------------------------
;; The e2e harness's own settings, loaded HERE and nowhere else.
;; ---------------------------------------------------------------------------
;;
;; Doom loads this file before any module's `config.el', and
;; `modules/app/agent-repl/config.el' decides AT LOAD TIME whether to register
;; cold start (`(if (and agent-repl-frontend-auto-start (not noninteractive))
;; ...)'). So the Go layer's settings -- which daemon binary, which state root,
;; which account roots, and above all `agent-repl-frontend-auto-start' nil --
;; have to be in effect BEFORE that form runs, or Emacs spawns a daemon
;; pointed at the host's own defaults before a test can say otherwise.
;;
;; Setting them with `setq' ahead of the `defcustom' in `lisp/daemon.el' is the
;; ordinary Doom pattern and it holds: `custom-declare-variable' leaves an
;; already-bound variable's value alone.
;;
;; Absent the variable, this is inert, so an interactive `$S shell' session and
;; a `doom sync' behave exactly as they did before.
(agent-repl-e2e--breadcrumb "doom! form evaluated")

(let ((settings (getenv "AGENT_REPL_E2E_SETTINGS")))
  (when (and settings (not (string-empty-p settings)))
    (unless (file-readable-p settings)
      (error "AGENT_REPL_E2E_SETTINGS=%s is not readable" settings))
    (load settings nil t)))

(agent-repl-e2e--breadcrumb "init.el finished")
