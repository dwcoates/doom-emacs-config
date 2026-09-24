# The Emacs client layer for the cross-system e2e suite

Status: DESIGN, awaiting project-lead review. No tests are written against
this document until it is reviewed.

This document amends `SPEC.md` §B "Frontends: none", which reads:

> No webapp, no WebSocket, no Emacs. Every test dials the daemon's Connect
> API directly.

USER RULING (2026-09-03): **the cross-system suite is only e2e if EMACS is
the client.** A test that dials the daemon's Connect API directly exercises
the daemon's serving surface, not the product. The product is an Emacs
frontend whose user types `SPC o o` and reads a response out of a buffer.
Everything between those two points -- the keybinding, the command, the
transport codec, the daemon, the shim, the store, the sidecar, and the
frames coming back up into buffer and window state -- is what this layer
covers.

`SPEC.md`'s existing areas are NOT replaced. They keep dialing the daemon
directly, and they stay the right shape for what they cover: the frame-level
contract of sixty-nine vendor scenarios. This layer is additive, and it
deliberately covers only what a direct Connect client CANNOT reach.

## Why an Emacs client catches things no Connect client can

Two defects already found by hand are the motivating evidence, and both are
structurally invisible to a Connect-dialing test:

- **The missing account flags.** Emacs spawns the daemon through its own
  launcher, and the launcher composes the daemon's argv. A test that starts
  `claude-repld` from a Go harness composes that argv ITSELF, so it can
  never disagree with the launcher -- and the launcher's omission of the
  per-account config-dir flags survived every direct test. Only a test where
  EMACS spawns the daemon compares the two.
- **The sentinel/kill-buffer recursion.** A real hang: closing a workspace
  re-entered a process sentinel that killed a buffer that ran the sentinel
  again. There is no frame for this. It manifests only as an Emacs process
  that stops answering, which is why this layer's heartbeat is a first-class
  mechanism and not a nicety.

## How Emacs is started: a GUI frame on Xvfb, driven by emacsclient (REVISED)

Rejected: **`emacs -batch`**. Three independent reasons, each decisive:

- Batch has no frame, no window system, no tab-bar and no redisplay, so
  window placement, panel lifecycle, tab-bar order and modeline content --
  most of what this layer exists to assert -- do not exist there.
- `config.el` gates cold start on `(not noninteractive)`, so a batch Emacs
  **never spawns the daemon at all**. The single most valuable thing this
  layer covers is unreachable in batch by the module's own design.
- `keybindings.el` compiles `map!` to a no-op when Doom is absent
  (`eval-when-compile (unless (fboundp 'map!) ...)`), so a `-Q` Emacs has no
  leader bindings regardless of mode.

Withdrawn: **a tty frame.** The first revision rejected a GUI frame under
Xvfb on the grounds that "it buys only the xwidget WebKit view, which this
layer does not cover". Both halves turned out to be wrong. The panel **is**
the WebKit view — `agent-repl-frontend-open-panel` mounts an
`xwidget-webkit` webview — so a tty frame does not cover a *reduced* panel,
it covers **none**: on one, `make-xwidget` signals

```
make-xwidget: GTK has not been initialized
```

and the proof-of-life test stops at step 3. The cost, meanwhile, is not an X
server in the sandbox: the image already carries `Xvfb` and `xauth`, and
starting one measures **21-121ms**.

Chosen: **a GUI frame on a per-test Xvfb, booting the image's REAL DOOM,
with `server-start`, driven through `emacsclient --eval`.**

- The sandbox starts `Xvfb -displayfd 1 -screen 0 1280x1024x24 -nolisten
  tcp` per test, then plain `emacs` — no `-nw` — with `DISPLAY` and
  `GDK_BACKEND=x11` in its environment. GDK_BACKEND is pinned because the
  image's Emacs is a `--with-pgtk` build: pure GTK prefers Wayland, and
  there is no compositor here.
- **The pty stays.** Emacs is still started under `script(1)`, which is
  what keeps its stdin a terminal and what collects everything it writes
  outside the frame — GTK diagnostics, an early elisp backtrace, the reason
  a boot died — into the one buffer the failure artifacts carry.
- **`-displayfd` is the synchronization primitive, and nothing sleeps.**
  Xvfb writes the display number it settled on to that descriptor *only
  once it is listening*, so one read is both "which display" and "it is
  up". It also means the **server** picks the free number, not the test, so
  two worlds in one container cannot collide the way a hard-coded `:99`
  would. The stdout carrying that number and the stderr carrying the log
  are routed to two separate files — which is why the sandbox seam grew
  `StartProcess` alongside `StartPTY`.
- A GUI frame is a real frame in every way the tty frame was: `window-list`,
  `split-window`, `tab-bar-mode`/`tab-bar-tabs`, `mode-line-format` and
  `selected-window` all behave as they do for a user, and it is additionally
  the only frame on which the product's own panel can exist.
- **No `-Q`, no `-l`, and no `--init-directory`.** The image's Doom profile
  is the init path, so the module is reached through `doom!` exactly as a
  user reaches it.
- Every interaction is
  `emacsclient --socket-name <sock> --eval <form>`, run through the
  sandbox's `Exec`. The Go side never types keys into the pty *except* by
  `execute-kbd-macro` inside a form (see the keybinding section below) —
  that was true of the tty boot and is unchanged by the GUI one, which is
  why the keybinding affordances needed no revision at all.
- The Xvfb is killed with the test, an Xvfb that died *before* cleanup is a
  loud error rather than a silence, and its log is preserved in the failure
  artifacts on every failure — unconditionally, not only when a scenario
  asked, because a frame that never appears is explained there and nowhere
  else.

### It boots real Doom, and that changed three things (REVISED)

The first revision of this document booted `emacs -nw -Q -l bootstrap.el`
and `load`ed `config.el` by hand, and flagged the Doom boot as a follow-up.
**The follow-up is taken; the `-Q` boot is withdrawn, not deferred.** The
image already bakes `doom install` + `doom sync` against a profile that
enables `:app agent-repl` and `:config (default +bindings)`, so booting it is
strictly more faithful — real `map!` bindings, the real `set-popup-rule!`
that `config.el` skips when `set-popup-rule!` is unbound (the notes-popup
rule scenario 34's family depends on), real module load order, real Doom
popup and workspace machinery.

**There is no `-Q` fallback, and no scenario needs one.** The only thing `-Q`
buys is a bare Emacs with no Doom, and bare-Emacs coverage of this module is
the module's own ERT suites (`lisp/test-*.el`), which run in batch and are
where the key-to-command mapping and every unit-level claim already live. A
scenario in THIS layer that wanted `-Q` would be asserting something about a
configuration no user runs.

Three mechanisms make the Doom boot work, and two of them live in
`sandbox/doom/`, which this layer reads but does not own:

1. **How Emacs finds Doom: `HOME`, not a flag.** This began as a version
   constraint (the image's Emacs was 28.2, and `--init-directory` landed in
   29); the image now carries 30.2, and `HOME` is still the mechanism for a
   reason that does not expire. `HOME` is the per-test scratch root and `~/.emacs.d`
   under it is *staged* from the image's `EMACSDIR`: Doom's sources are
   symlinked (they are only ever loaded), and its `.local` tree is copied in
   minus `straight/`, which is symlinked for size. That is necessary because
   the container runs `--read-only` and `/sandbox/emacs.d` is not one of its
   tmpfs mounts, so Doom's local tree cannot be written in place — anything
   Doom writes at startup has to land in the test's own scratch. Which paths
   Doom actually writes at startup is **UNVERIFIED**; a boot that hits a
   read-only path fails loudly with the pty output.

2. **When the settings take effect: before any module `config.el`.**
   `config.el` decides *at load time* whether to register cold start
   (`(if (and agent-repl-frontend-auto-start (not noninteractive)) ...)`), so
   the layer's settings — daemon binary, no-op build script, state root, the
   two account roots, and `agent-repl-frontend-auto-start` nil — must be in
   effect before that form runs, or Emacs spawns a daemon against the host's
   own defaults before a test can say otherwise. `sandbox/doom/init.el`
   therefore loads the file named by `AGENT_REPL_E2E_SETTINGS` right after
   its `doom!` form. Setting the values with `setq` ahead of the `defcustom`s
   in `lisp/daemon.el` holds: `custom-declare-variable` leaves an
   already-bound variable's value alone.

3. **How a test knows Doom finished: one readiness stamp.**
   `sandbox/doom/config.el` adds `agent-repl-e2e--boot` to Doom's own
   after-init edge (`doom-after-init-hook` when bound, `emacs-startup-hook`
   otherwise, because `DOOM_REF` is a build argument and the profile must not
   assume which exists). It enables `tab-bar-mode`, calls `server-start`, and
   writes `AGENT_REPL_E2E_READY` **last** — so the stamp's appearance means
   "Doom is up AND emacsclient will answer". The stamp is JSON and carries
   the facts the Go side then *asserts* rather than assumes: `doom`,
   `map_bang`, `popup_rule`, `agent_repl`, plus `emacs_version` and the pid.
   A profile that silently degraded fails the boot instead of making every
   keybinding and popup assertion below it vacuous. A boot that fails writes
   the SAME file with `"ok": false` and the elisp error, so the Go side
   reports the elisp error rather than timing out on a socket that is never
   coming.

The same file also carries the readback helper (`agent-repl-e2e--eval`), so
the file-based readback is unchanged from the `-Q` revision: a form in, JSON
out, nothing surviving shell quoting.

### Keybindings ARE asserted here (REVISED)

**The GUI frame changed nothing in this section, by construction.** Presses
have never travelled through the pty: `Keys`, `KeysIn` and `Leader` send an
`execute-kbd-macro` form over the *server socket*, so the keymap lookup and
the command run inside Emacs's own command loop regardless of what kind of
frame it has. Swapping `emacs -nw` for `emacs` moved the frame and left the
transport alone.

The first revision excluded them, because `map!` compiles to a no-op without
Doom (`keybindings.el`'s `eval-when-compile (unless (fboundp 'map!) ...)`).
With the Doom boot, `map!` expands for real and the leader map from
`:config (default +bindings)` is live, so **the bindings are in scope and
scenarios may assert them.**

Two affordances, and which one a scenario uses is decided by whether the
command prompts:

| affordance | what it does | use it when |
|---|---|---|
| `BindingFor(keys)` / `BindingForIn(buffer, keys)` / `LeaderBinding(keys)` | resolves a key sequence to the command it names, **without running it** | the command would prompt (`completing-read`, `y-or-n-p`), or the claim IS the mapping |
| `Keys(keys)` / `KeysIn(buffer, keys)` / `Leader(keys)` | presses the sequence through `execute-kbd-macro`: real keymap lookup, real command | the command runs to completion without prompting |

Every press enters evil NORMAL state first, because the leader is `SPC` there
and a composer buffer left in insert state would send a literal space — a
wrong pass rather than a failure. A sequence that *does* prompt blocks the
command loop and is reported as a **wedge**, which is correct: from the
outside a prompt nobody can answer is a hang.

**Which scenarios should assert a binding rather than a command.** The rule
is that a binding assertion belongs where the KEY is the user's entry point
and the module owns the key, not where the command is merely convenient to
reach:

- **Area A (cold start)** — none. Cold start has no keystroke.
- **Area B (workspace lifecycle)** — the workspace verbs are the module's own
  leader bindings and are the strongest case in the list: `SPC TAB C-n`
  (create), `SPC j x` (kill), the close and restart verbs. Assert with
  `LeaderBinding` where the verb prompts for a target and with `Leader`
  where it takes the target as an argument.
- **Area C (panel and window lifecycle)** — the panel open/close bindings,
  pressed with `Leader`; these are exactly the ones a `map!` regression
  would break silently, because the command keeps working.
- **Area D (composer)** — `RET` in the composer buffer, via
  `KeysIn(<input buffer>, "RET")` rather than by calling `agent-repl-send`.
  The composer's own map is the subject, and the proof-of-life test's step 4
  is the natural place to switch first.
- **Areas E, F, G, H, I** — command-driven as before. Their subjects are
  paint, routing, restart and refusal text; the key that reached them is not
  the claim.

The module's `lisp/test-keybindings.el` ERT suite keeps its coverage
unchanged. It asserts the mapping in isolation; this layer asserts that the
key, in a real Doom session, reaches the real command with real state behind
it.

### Commands are invoked as commands, not as internals

Each scenario calls the interactive command through `call-interactively` (or
`funcall` with its documented arguments), never the private helper beneath
it. Reaching past a command into an internal is a defect in this layer.

Interactive argument collection is satisfied the standard ERT way -- binding
`completing-read` / `read-string` / `y-or-n-p` for the duration of the one
call -- so the command still runs its own argument-collection code path.
Several commands take the target as an optional argument
(`agent-repl-close-workspace (&optional ws)`), and those are called with it
rather than through a stubbed picker wherever the picker is not the subject.

### State is read back as DATA

`emacsclient --eval` returns a printed elisp form. The rule is:

> **Never scrape human text where a variable exists.**

The variables each area reads are named per scenario below; the module
exposes the full pushed state as data, so this is nearly always possible:

| what | read from | not from |
|---|---|---|
| roster rows / arms | `agent-repl-roster--rows-by-id`, `agent-repl-roster--status-by-id` | the sidebar's drawn text |
| tab order | `agent-repl-roster--tab-order`, `agent-repl--ws-tabline-names` | the tab-bar string, `tab-bar-tabs` |
| workspace registry | `agent-repl--workspaces` | buffer names |
| host state / session ids | `agent-repl-host--by-name` | anything rendered |
| panels + layout | `window-list` + `window-buffer` + `window-parameter` | `format-mode-line` |
| daemon launch | `agent-repl--frontend-daemon-process`, `agent-repl-daemon-launch-failure`, `agent-repl-daemon-build-failure` | the `*claude-repld*` buffer text |
| attention marker | `agent-repl-status--marker-on` | the `●` glyph |
| held prompts | `agent-repl--prompt-queue` | the tray |

The sanctioned exception is a scenario whose subject IS the rendered string
-- the modeline segment's own composition
(`agent-repl-daemon-mode-line-segment`, `agent-repl-link-drain-segment`) and
the host-side refusal messages that are `user-error` text by contract. Those
read the string deliberately, and say so in a comment.

Note that `status.el`'s tab paint is SVG images sized to the tab-bar line
height, which a tty frame cannot rasterize. That is not a gap: the paint
DECISION is data (`agent-repl--color-by-name`, `agent-repl-status-color-table`,
`agent-repl-status-blink-schedule`) and is what gets asserted. Rasterization
is not this layer's claim.

Readback goes through one helper — `agent-repl-e2e--eval`, defined in
`sandbox/doom/config.el` — that reads a form from a file, wraps its
evaluation in `condition-case`, and writes `{"ok": true, "value": ...}` or
`{"ok": false, "error": "<message>"}` to a second file. Files rather than
`--eval` output on both sides, so nothing has to survive shell quoting or
elisp print escaping, and an elisp error becomes a Go failure naming the
elisp error rather than an unparseable `emacsclient` exit status.

## The heartbeat

A wedged Emacs is the failure mode this layer must fail FAST on, because a
wedged Emacs looks exactly like a slow one.

- A dedicated `emacsclient --eval '(emacs-pid)'` probe runs on its own
  goroutine at a fixed interval, against the SAME socket the scenario uses,
  so it queues behind whatever the scenario is doing and therefore measures
  the command loop's actual responsiveness.
- Bound: **`HeartbeatBound = 2s`**, a tight multiple of the observed healthy
  probe cost. It is a NAMED constant with its reason attached, per
  `SPEC.md` §B "Waits" and the module `AGENTS.md` rule that bounds are
  measured, not guessed. The number is provisional until measured against a
  healthy run of the proof-of-life test, and this document must be amended
  with the measurement before the scenario list is implemented.
- A missed heartbeat fails the running test IMMEDIATELY with the last form
  the layer sent, and dumps Emacs's `*Messages*` plus a `SIGUSR2`-triggered
  backtrace into the failure artifacts directory (`ArtifactsEnv`, reusing
  `SPEC.md`'s existing artifact plumbing). It never retries: a hang that
  clears itself is still the recursion defect.
- The heartbeat is armed for the whole life of the Emacs process, including
  teardown, because the recursion defect fired during close/kill.
- A wedge also hands over a **native backtrace of every thread**, taken from
  outside the process with gdb while the stall is still live. It is the only
  witness that survives an Emacs answering nothing at all; the section below
  is the stall that made it necessary and what it found.

### Each scenario's elisp log has its own root

`agent-repl-log-file-name` defaults to one uid-keyed file under
`temporary-file-directory`, and every scenario in this container runs as the
same uid: four parallel Emacsen appended to the one file and rotated it out
from under each other, so a failure's own records were interleaved with three
unrelated scenarios' and truncated mid-scenario. `writeSettings` aims it at
`<state root>/logs/doom-agent-repl.log`, which the failure artifacts already
copy.

### The native backtrace, and the stall that made it necessary (RESOLVED)

Every witness this layer had was a **lisp** witness: the server socket, a
nested eval, `debug-on-event`'s SIGUSR2, a plain SIGINT. All four need Emacs
to reach a QUIT check. `TestEmacsPermissionAskFiresTheAttentionMarker` stalled
in roughly **one run in sixteen** with Emacs `state=R wchan=0` answering none
of them, so nothing in the layer could say what it was doing.

`sandbox/Dockerfile` therefore carries **`gdb`**, the sandbox runs with
**`--cap-add SYS_PTRACE`** (the harness's gdb is a *sibling* of Emacs, not an
ancestor, which Yama's `ptrace_scope=1` refuses without it), and the Emacs it
builds is compiled with **`-g`**. On a wedge — before anything that waits for
Emacs to come back, and before `breakStall` — `nativeBacktrace` attaches,
unwinds every thread and files the capture as `emacs.native-backtrace.txt` in
the failure artifacts.

The first capture settled the whole question in eight frames:

```
Thread 1 (LWP 13697) "emacs":
#0  pselect () from libc.so.6
#4  xg_select (...) at xgselect.c:205
#5  wait_reading_process_output (... read_kbd=-1 ...) at process.c:5748
#6  kbd_buffer_get_event (...) at keyboard.c:4094
#9  read_char (commandflag=1, ...) at keyboard.c:3015
#11 command_loop_1 () at keyboard.c:1429
#16 recursive_edit_1 () at keyboard.c:754
#17 read_minibuf (...) at minibuf.c:905
#18 Fread_from_minibuffer (...) at minibuf.c:1394
#20 F792d6f722d6e2d70_y_or_n_p_0 () from subr-...eln
#22 F6d616769742d737461747573_magit_status_0 () from magit-status-...eln
```

It is not a C loop at all. It is `magit-status` calling **`y-or-n-p`**: the
reader enters a *recursive edit*, the command loop stops running the
scenario's forms, and — measured — the process answers no emacsclient at all
while it stands there. A prompt-aware heartbeat probe cannot see it, because
seeing it requires an answer.

**Root cause.** `magit-status` is `interactive-only`; magit names
`magit-status-setup-buffer` as the Lisp entry point precisely because
`magit-status` carries the interactive fallback:

```elisp
(let ((toplevel (magit-toplevel directory)))
  (setq directory (file-name-as-directory (expand-file-name directory)))
  (if (and toplevel (file-equal-p directory toplevel))
      (magit-status-setup-buffer directory)
    (when (y-or-n-p ...) (magit-init directory))))
```

`file-equal-p` (`lisp/files.el`, Emacs 30.2) does **not** compare device and
inode. It stats each path separately and compares the whole `file-attributes`
list — mtime, ctime and size included — with one `equal`; files.el already
documents the same fragility for Haiku's atime and special-cases only that.
So a write landing in the directory **between the two stats** makes one
identical path compare unequal to itself.

Our side supplies exactly that write: `agent-repl-add-project-workspace`
registers the workspace with the daemon and switches to it, and the
registration writes `.claude/emacs/` into the same project root magit is
opening. The recorded conversation shows it — `rev-parse --show-toplevel`
answered `exit 0` with the directory's own path, and `add-project-registered`
lands *after* `magit-status-same-window`. The prompt that stood was

```
/tmp/emacs-e2e-.../repo-other/ is a repository.  Create another in /tmp/emacs-e2e-.../repo-other/?
```

— the same directory, twice.

**Fix, at the trigger.** `agent-repl--magit-status-same-window` now calls
`magit-status-setup-buffer`, magit's own Lisp entry point, which has no prompt
in it: the question cannot be asked, whatever `file-equal-p` says on any
given schedule. A directory that genuinely is not a repository signals loudly
from magit's refresh instead, which is what this module wants. `lisp/magit.el`
carries the reasoning at the call site and `lisp/test-magit.el` pins that the
prompting entry point is never called.

The `file-equal-p` weakness itself is upstream Emacs's and is left alone; it
is recorded here with the backtrace above so a future stall of the same shape
is recognized rather than re-derived.

**Two witnesses were added alongside**, because the diagnosis needed both:

- an unanswerable prompt is now **refused loudly** by the layer's own
  settings (`y-or-n-p`/`yes-or-no-p` are advised to signal, naming the
  prompt), so this class of defect can never again present as a missed
  heartbeat. Scenarios that drive a prompting command stub the reader with
  `cl-letf`, which wins over the advice.
- the scripted git records **what it answered** — exit status and clipped
  streams — not only what was asked, since the caller's next move is a
  reaction to the answer.

## Emacs spawns the daemon -- and that is the point

`SPEC.md` §G already raised this as an open item and stated the reason
exactly:

> An e2e harness that builds its own argv is a SECOND spelling of the launch
> contract and can drift from the one that ships; the e2e daemon start should
> derive its argv from the elisp launcher's own builder
> (`agent-repl-daemon--argv` in `lisp/daemon.el`).

This layer settles that item by inverting it: nothing derives the argv,
because **Emacs performs the launch**. There is then only one spelling.

So the Go side does NOT call `harness.StartDaemon` for this layer:

- It builds the same binaries `main_test.go` already builds, reusing
  `requireNode`, `requireShimBundle`, `requireStoreBinary`,
  `requireSidecarBinary` verbatim, and it starts the
  **store** and **sidecar** itself with the existing `startStore` /
  `startSidecar` helpers. Neither is Emacs's to launch.
- The **daemon** is started by Emacs through `agent-repl-frontend-daemon-ensure`
  (the same command `config.el` arms on `emacs-startup-hook` via
  `agent-repl-daemon-schedule-ensure`). Only the launcher's own documented
  configuration is pointed into the sandbox, all as ordinary `setq` of its
  defcustoms:

  | variable | value |
  |---|---|
  | `agent-repl-daemon-command` | the `claude-repld` the Go side built |
  | `agent-repl-daemon-default-config-dir` | a scratch account root |
  | `agent-repl-daemon-multi-repo-config-dir` | a second scratch account root |
  | `agent-repl-daemon-multi-repo-root` | a scratch multi-repo tree |
  | `agent-repl-daemon-build-script` | a scripted no-op (the binaries are prebuilt) |
  | `AGENT_REPL_STATE_DIR` (env) | the scratch state root |

  The launcher composes argv and environment; the layer never does.
- The daemon **publishes its own address** into `daemon.addr` under the state
  root (there is no socket-path variable -- the transport is loopback TCP
  Connect). The Go side reads that same file to dial the daemon directly when
  a scenario needs to cross-check a frame, so both clients agree on one
  address by construction.
- The store socket is handed to the daemon the way the launcher's environment
  allows (`AGENT_REPL_STORE_SOCKET`), and the sidecar's `--config-roots` is
  told the SAME two account roots the launcher was given, resolved for
  symlinks first -- `SPEC.md` §B's "One string per config root" invariant
  applies here unchanged and is the reason `resolveConfigRoots` exists.
- The deploy's seams are exported into the Emacs process's environment so the
  daemon Emacs spawns inherits them: `AGENT_REPL_CHECKOUT` (the module root,
  whose elisp the real Emacs loads), `AGENT_REPL_DEPLOY_BUILDER` (the world's
  `harness.DeployBuilder`, `EmacsWorld.Deploy`), `AGENT_REPL_LAUNCHCTL` (a
  recording fake, `EmacsWorld.Launchctl`) and `AGENT_REPL_LAUNCH_AGENTS_DIR`.
  The store and the sidecar report their builds into the world's lock dir and
  the fake build stages copies of their binaries, so `SPEC.md` §B's "A deploy
  judges what really runs" invariant holds here unchanged.
- `AGENT_REPL_FORBID_VENDOR_CALLS=1` is in the Emacs process's environment,
  so the daemon, the shim, the store and the sidecar all inherit it.

Teardown order, registered so it runs in reverse of start: heartbeat
disarmed -> `agent-repl-frontend-daemon-stop` (Emacs ASKS the daemon to
exit; per `daemon.el`, "EMACS NEVER KILLS A DAEMON") -> `(kill-emacs)` via
emacsclient -> sidecar -> store. A hard `SIGKILL` on the Emacs pty process
and a sweep for an orphaned `claude-repld` under the scratch state root
follow unconditionally, so a wedged Emacs cannot leak a daemon into the next
test.

## The sandbox dependency (AMENDED once `sandbox/` landed)

Tests write nothing outside the sandbox and never touch the host's Emacs,
`~/.claude`, `~/.emacs.d`, or `~/.config`, and **the layer asserts that from
the inside rather than trusting the flags it was launched with.**
`assertHostIsolation` runs once, before anything starts, and fails the test
unless: `HOME` is under `/sandbox/`; `/tmp` and `HOME` are each a **tmpfs**
mount of their own, read from `/proc/self/mountinfo` rather than inferred;
`/repo-src` is a mount point; and a write probe into `/repo-src` **fails**.
The run script's `docker run` flags are what establish all four, but a test
invoked some other way could satisfy `insideSandbox()` and still reach the
host, and this layer starts a real Emacs that spawns a real daemon. `modules/app/agent-repl/e2e/sandbox/`
has since landed (`cfaea2bdb`), and **its model is inside-out from what the
first revision of this document assumed.** This section is rewritten to what
shipped; the interface the earlier revision proposed (a Go `sandbox` package
driving containers from the host) is withdrawn, not deferred.

### What shipped

`bin/e2e-sandbox.sh` has four verbs: `build`, `run`, `shell`, `preflight`.
`run` is a **one-shot `docker run --rm`** — there is no persistent container,
no container name, and no `exec` verb. Its documented usage runs the whole
test binary inside the container:

```bash
modules/app/agent-repl/e2e/sandbox/bin/e2e-sandbox.sh run --dir e2e go test -run TestEmacs .
```

### Consequence: the layer runs INSIDE, and does not drive the container

The Go test process is itself containerized, so every process this layer
starts is an ordinary local child. That removes both capabilities the project
lead flagged as missing, rather than needing them added:

- **A PTY-capable exec is not needed from the script.** The layer allocates
  its own pty in-container via `script -q -c CMD /dev/null`. `script` is a
  **guarantee of the image**, not an inference: the Dockerfile installs
  `bsdutils` (which carries `script` on bookworm) and `util-linux` by name in
  its apt list, so they are pinned to `SNAPSHOT_STAMP` like everything else
  and a reshuffle of the binary between the two packages cannot silently
  remove it. `requireSandbox` keeps an `exec.LookPath("script")` check as a
  backstop for an image built before that line existed. Relying on the
  script's
  `-t` would not work anyway: `do_run` passes `-t` only when its own stdout
  is a terminal, and under `go test` it is not.
- **A writable swept Scratch is not needed from the script.** `/tmp` is
  already `--tmpfs rw,exec,size=2g`, per-container and gone when the
  container exits. `Scratch()` is an `os.MkdirTemp("/tmp", ...)` per test,
  also removed on cleanup so `-count=N` does not accumulate.

`Available()` is therefore three-way, because the three cases have completely
different answers and a reader of a skipped run must tell them apart:

| situation | detected by | skip says |
|---|---|---|
| inside the sandbox | `AGENT_REPL_SANDBOX_SHA` set **and** `/repo-src` is a directory | nothing; it runs |
| on the host, image usable | `preflight` exits 0 | the exact `e2e-sandbox.sh run --dir e2e go test ...` command to use instead |
| on the host, image unusable | `preflight` exits non-zero | preflight's own output, **verbatim** |

Both in-container signals are required: a stray environment variable on the
host must not convince this layer it is containerized.

Quoting preflight verbatim is the sandbox README's own requirement -- "The
harness must turn a non-zero exit into a loud skip that quotes this output
verbatim -- never a silent pass, and never a fallback to an unsandboxed run."

### Emacs in the image is 30.2, built from source (RESOLVED)

An earlier revision of this document recorded the image's Emacs as bookworm's
`emacs-nox`, i.e. **28.2**, and wrote a ceiling around it ("no scenario may
assume Emacs 29+ anything"). That is withdrawn: `sandbox/Dockerfile` now
COMPILES Emacs from a pinned upstream commit, and the ceiling is gone.

- **Emacs is 30.2 at commit `636f166cfc86`** — the commit the developer's own
  Emacs is built from, pinned as `SANDBOX_EMACS_REF` exactly like every other
  identifier. Verified from inside the image: `emacs-version` is `30.2` and
  `emacs-repository-version` is `636f166cfc86aa90d63f592fd99f3fdd9ef95ebd`.
- **`--init-directory` now exists** (it landed in 29). The layer nevertheless
  keeps aiming Emacs through `HOME` plus a staged `~/.emacs.d`, and that is
  no longer a version workaround: the container is `--read-only` and
  `/sandbox/emacs.d` is not a tmpfs, so Doom's `.local` tree cannot be
  written in place wherever init points.
- **`HasEmacs()`'s floor of 27 is unchanged** and the ceiling is simply
  gone. A scenario may use 29's `treesit` or 30's own features; what it may
  not do is assume anything the image does not carry, which is now a question
  about the Dockerfile's configure line rather than about a distro package.
- **xwidgets and native compilation are both compiled in and live**
  (`(featurep 'xwidget-internal)` and `(native-comp-available-p)` are each
  `t`), because the webapp panel IS an `xwidget-webkit` webview and no distro
  Emacs is built with it. `bin/e2e-sandbox.sh build` FAILS if either is
  absent, so a degraded image cannot exist.

### The tty frame needs a real terminal type (VERIFIED in-container)

The Emacs child's `TERM` was `dumb`. That is fatal for a `-nw` frame and
Emacs says so verbatim before it loads a single line of init:

```
emacs: Terminal type "dumb" is not powerful enough to run Emacs.
It lacks the ability to position the cursor.
```

`dumb` has no `cup` capability, so it can describe a batch Emacs, which
draws nothing, and never an interactive frame. `TERM=xterm-256color` is the
replacement, and it was chosen by listing terminfo INSIDE the container
rather than assumed:

```
$ ls /lib/terminfo/x
xterm  xterm-256color  xterm-color  xterm-debian  xterm-mono
xterm-r5  xterm-r6  xterm-vt220  xterm-xfree86
$ infocmp -1 xterm-256color | head -1
#	Reconstructed via infocmp from file: /lib/terminfo/x/xterm-256color
```

The image's terminfo set is `/lib/terminfo` only (`/usr/share/terminfo` is
empty) and it carries `Eterm*`, `ansi`, `cons25*`, `cygwin`, `dumb`, `hurd`,
`linux`, `mach*`, `pcansi`, `rxvt*`, `screen*`, `tmux*`, `vt100`/`vt102`/
`vt220`/`vt52`, and the `xterm*` family above. `xterm-256color` is picked
from that set because it is also the terminal a user of this module actually
runs Emacs in, so the frame the scenarios inspect is the frame a user sees.

### UNBLOCKED: interactive Doom boots, and the stamp proves it

The previous revision recorded this layer as BLOCKED: bookworm's Emacs 28.2
could not start interactive Doom at all, because the then-pinned Doom's
`early-init.el` refuses an interactive session below 29.1 and a `-nw` frame
IS interactive. `early-init.el` aborting meant `doom!` never ran, no module
`config.el` loaded, and the readiness stamp never landed.

That is resolved, and `sandbox/bin/doom-boot-probe.sh` is the standing proof.
It boots this profile the way StartEmacs does — staged `~/.emacs.d` under a
scratch HOME, a pty from `script(1)`, `TERM=xterm-256color` — and reports the
stamp `sandbox/doom/config.el` writes:

```json
{"ok":true,"emacs_version":"30.2","doom":true,"doom_version":"2.1.0",
 "map_bang":true,"popup_rule":true,"agent_repl":true}
```

Every field `awaitDoom` asserts is true: Doom is loaded, `map!` expanded,
`set-popup-rule!` is bound, and `:app agent-repl` loaded. Two things had to be
fixed on the way there, and both are worth knowing because neither announced
itself:

- **`DOOM_REF` must be the host's Doom, not master.** Upstream master has
  moved the module library out of the doomemacs repository into a separate
  `sources/doom+` collection. On a master pin, `doom sync` SUCCEEDS and the
  `doom!` block resolves no modules at all; the only symptom is
  `"popup_rule": false` in the stamp. The pin is now v2.1.0
  (`4f4911fed96739aebbb6d1dbadf0cadbd418fb9a`), and the Dockerfile asserts
  the module directories exist so this cannot recur silently.
- **persp-mode must be loaded before `tab-bar-mode`.** Doom's
  `:ui workspaces` puts a tab-bar hook on that calls `safe-persp-name`, and
  persp-mode is deferred to an edge a headless boot never reaches, so the
  boot hook died with `(void-function safe-persp-name)`.

**Still unmeasured:** the boot bounds below. A healthy boot now exists to
measure, but no timing has been collected from a suite run, and no e2e or ERT
suite has yet been RUN inside the sandbox.

## What this layer does not cover, deliberately

- **The xwidget webapp view.** It needs a real WebKit view; the composer is
  host-native and IS covered. Anything drawn inside the webview is the
  webapp's own vitest suite's.
- **Frame-level vendor behavior.** The sixty-nine scenarios stay in the
  existing areas. This layer drives ONE ordinary prompt where it needs a
  response, and asserts Emacs's handling of it.
- **Git facts.** Scripted fake git only, per `SPEC.md`. No real git anywhere.

## Two brief items the contract does not have

Both were named in the task that commissioned this layer; neither exists,
and the layer must not invent them.

- **"Answering a permission" is not an Emacs command.** `elisp.md`:352 --
  "No permission-answering surface in Emacs (permission.el dies; the
  notification policy is the whole reaction)." There is no `permission.el`
  and no interactive permission command in `lisp/`. `:permission` exists only
  as a roster status arm (`agent-repl-roster-running-statuses`). Emacs's
  ENTIRE contribution to a permission ask is the typed
  `notification{permission_requested}` push setting the attention marker.
  Scenario 28 covers exactly that and nothing more; the ask is answered on
  the daemon side, which is where `permission_e2e_test.go` already covers it.
- **There is no interrupt command.** No `agent-repl-interrupt*` symbol
  exists. Per `elisp.md`'s workspace verbs, the interrupting act is
  `agent-repl-restart-workspace` with a prefix argument (`SPC o C-c`,
  `C-u` = force): "Forced: interrupt and bounce; the agent is NOT resumed
  afterwards." Scenario 35 drives that command.

## Scenario list

44 scenarios in 9 areas. Each names the ordinary command it drives and the
state it asserts. Areas A, C, E, F and I are the ones a Connect-dialing test
CANNOT reach at all; B, D, G and H overlap the existing areas only in that
they drive the same verbs, and assert Emacs's own state rather than frames.

### A. Cold start and the daemon launch (6) -- Emacs-only

1. **ColdStartBuildsAndSpawnsTheDaemon** -- `agent-repl-frontend-daemon-ensure`
   -- `agent-repl--frontend-daemon-process` is live, `daemon.addr` exists
   under the state root, `agent-repl-link--primary` is non-nil.
2. **LauncherArgvCarriesBothAccountRoots** -- `agent-repl-frontend-daemon-ensure`
   -- the SPAWNED process's real argv (read from the OS, not from
   `agent-repl-daemon--argv`) contains `--default-config-dir` and
   `--multi-repo-config-dir`, both `expand-file-name`d. **This is the
   account-flags regression, pinned**: the defect that reached the user's
   live logs and that `SPEC.md` §G raised as unresolved.
3. **LauncherRefusesWithoutAnAccountRoot** -- set
   `agent-repl-daemon-default-config-dir` to `""`, then ensure --
   `agent-repl-daemon-launch-failure` is set, NO daemon process was spawned,
   and `agent-repl-daemon--missing-config-flags` names the flag. Pairs with
   `daemon/integration/boot_test.go`'s `TestBootRefusesWithoutAnAccountRoot`
   from the other side.
4. **AdoptsAnAlreadyAnsweringDaemon** -- ensure, with a daemon already up and
   its address published -- no second process, `agent-repl-link--primary`
   points at the existing one. Per `elisp.md`: Emacs adopts any daemon that
   answers DaemonHealth, healthy or not, and never kills one.
5. **StateRootTravelsInTheEnvironment** -- ensure -- the spawn environment's
   `AGENT_REPL_STATE_DIR` equals `agent-repl--global-state-dir`, and the
   daemon's `logs/` directory materializes under it. One state root is the
   cross-system contract.
6. **BuildFailureSurfacesInTheModeline** -- point
   `agent-repl-daemon-build-script` at a script that exits non-zero, then
   ensure -- `agent-repl-daemon-build-failure` is set and
   `agent-repl-daemon-mode-line-segment` names the failure. Rendered-string
   assertion, deliberately: the segment's composition IS the subject.

### B. Workspace lifecycle (7)

7. **CreateWorkspaceAppearsOnTheRoster** -- `agent-repl-create-workspace` --
   `agent-repl-roster--rows-by-id` gains one row, `agent-repl-roster--tab-order`
   and `agent-repl--ws-tabline-names` both list it. The daemon minted the identity; Emacs
   only reacted.
8. **RegisterAnExistingDirectory** -- `agent-repl-add-project-workspace` --
   `agent-repl--workspaces` gains the name and `agent-repl-host--by-name`
   holds a ref whose `id` Emacs never constructed.
9. **SelectOnWorkspaceSwitch** -- `agent-repl-switch-to-project` --
   `agent-repl-host-last-selected-id` is that workspace's ref id. Select is
   one of Emacs's only two inputs to the roster.
10. **CloseWorkspaceIsAViewAct** -- `agent-repl-close-workspace` -- the tab
    is gone from `agent-repl--ws-tabline-names` and the host entry is dropped, while the
    daemon-side session is still alive (cross-checked on the Go client).
11. **CloseWithAHeldPromptDoesNotTearTheTabDown** --
    `agent-repl-queue-deferred-prompt` then `agent-repl-close-workspace` --
    the tab survives and `agent-repl--prompt-queue` still holds the prompt.
    Per `elisp.md`, the refusal manifests in the WEBAPP FOOTER, not an Emacs
    dialog, so what Emacs owes is precisely to not act; undelivered user
    intent may never be silently discarded.
12. **KillWorkspaceNeverBlocks** -- `agent-repl-kill-workspace` -- the name
    is gone from `agent-repl--ws-tabline-names`,
    session dead daemon-side, no refusal path taken even with work in flight.
13. **CloseThenKillDoesNotWedgeEmacs** -- both commands back to back on a
    workspace with a live panel -- **the heartbeat is the assertion.** This
    is the sentinel/kill-buffer recursion, pinned as a test.

### C. Panel and window lifecycle (6) -- Emacs-only

14. **PanelOpensIntoTheMainArea** -- `agent-repl-frontend-open-panel` --
    `window-list` grows and one window's buffer matches
    `agent-repl-frontend-buffer-name-format`, another
    `agent-repl-panel-buffer-name-format`.
15. **PlainCloseHidesPanelsAndLeavesTheTabAlone** -- `agent-repl-simple` --
    panel buffers are no longer displayed, `agent-repl-roster--tab-order` is
    byte-identical to before.
16. **DeprioCloseShufflesTheTab** -- `agent-repl` -- the workspace plist's
    `:saved-tab-index` is recorded and the tab order changed. The two close
    variants differing is the whole reason both commands exist.
17. **FocusInputSelectsTheComposer** -- `agent-repl-focus-input` --
    `(window-buffer (selected-window))` is the workspace's input buffer.
18. **FullscreenTogglesAndRestores** -- `agent-repl-fullscreen-and-focus`
    twice -- `agent-repl--window-fullscreen-config` becomes non-nil then nil
    and the window count returns to its starting value.
19. **OpenProgressLadderReachesRendered** -- `agent-repl-frontend-open-panel`
    -- `agent-repl--open-progress` walks `agent-repl--open-progress-stages`
    to a non-terminal end (not in `agent-repl--open-progress-terminal-phases`).
    The ladder is blessed host-native UX over Emacs-observable stages.

### D. Composer and prompt submission (7)

20. **SubmitFromComposerYieldsAResponseRow** -- insert text into the input
    buffer, then **press RET** -- the response is visible in Emacs's own
    state. **THE PROOF-OF-LIFE SCENARIO** (see below). RET, not
    `agent-repl-send`: the composer's RET is
    `map! :map agent-repl-input-mode-map :ni "RET"` in `input.el`, which the
    old `-Q` boot expanded to nothing, so pressing it is strictly more than
    the command call was.
21. **MetapromptIsComposedBeforeSubmission** --
    `agent-repl-send-with-metaprompt` -- the prompt the daemon received
    carries the sentinel-marked metaprompt span. Composition before
    submission is blessed; "verbatim" forbids only post-submission rewriting.
22. **PrefixAndPostfixSendVariants** -- `agent-repl-send-with-prefix` and
    `agent-repl-send-with-postfix` -- the submitted text has the decoration
    on the correct side.
23. **DiscardInputClearsTheComposer** -- `agent-repl-discard-input` -- the
    input buffer is empty and nothing was submitted.
24. **DeferredPromptDrainsOnTheFinishEdge** --
    `agent-repl-queue-deferred-prompt` during a running turn --
    `agent-repl--prompt-queue` holds it, then empties when the roster row
    transitions turn-running to idle. The finish edge is the trigger for all
    four Emacs-local reactions.
25. **HistoryRecallRestoresTheLastPrompt** -- `agent-repl--history-prev` --
    the input buffer holds the previously submitted text.
25a. **AttachedImageMarkerNeverRidesAsWords** --
    `agent-repl-input-attach-image` plus `agent-repl--image-insert-marker`,
    then **press RET** -- the submitted `UserSaid` carries the typed words in
    its text block and the file in an `ImageBlock{path, media_type}`, and the
    `[image attached: ...]` marker appears in NEITHER. The marker is drawn so
    the user can see the attachment; clipboard-image.el's own contract is that
    nothing about the image rides the words.

### E. Roster paint, modeline and attention (5) -- Emacs-only

26. **RosterArmsPaintTheTabInOrder** -- drive one turn --
    `agent-repl-roster--status-by-id` walks the submitting/thinking arms to a
    settled arm, and `agent-repl--color-by-name` resolves each. The roster's
    arm vocabulary is the ONE source for tab coloring.
27. **UnknownRosterArmIsRefusedLoudly** -- push a row carrying an arm the
    frozen schema does not declare -- Emacs refuses loudly rather than
    defaulting. Named directly by `elisp.md`'s replacement integration-test
    specs.
28. **PermissionAskFiresTheAttentionMarker** -- a fake-SDK permission
    scenario -- `agent-repl-status--marker-on` is set for the workspace and a
    notification was emitted, with the cadence taken from
    `agent-repl-status-blink-schedule`. Emacs answers nothing (see above).
29. **FinishEdgeFiresTheReadyReaction** -- drive a turn to completion --
    `agent-repl-roster-finish-functions` ran and the notification backend
    recorded the "Agent ready" banner for an unfocused workspace.
30. **DrainScheduleDrawsTheStandingBanner** --
    `agent-repl-daemon-shutdown-schedule` -- `agent-repl-link-drain` carries
    the reason and `at_ms`, and `agent-repl-link-drain-segment` renders it.
    Rendered-string assertion, deliberately.

### F. Find-file routing and the shared popup (4) -- Emacs-only

31. **VisitingAFileRoutesIntoItsOwningWorkspace** -- `find-file` on a path
    under a registered worktree -- the owning workspace is selected and the
    file's window is not a side window. Routing is `:around` advice on the
    display primitives, so only a real `find-file` exercises it.
32. **AnUnroutableFileRecordsARefusal** -- `find-file` on a path under no
    registered worktree -- `agent-repl--ffw-refused` records it and the file
    still opens.
33. **PendingPlacementResolvesAfterRosterReconcile** -- `find-file` on a
    not-yet-open workspace's file -- `agent-repl--ffw-pending` holds the
    placement, then drains on the post-reconcile hook.
34. **PopupOpensRightSideHalfWidth** -- `agent-repl-popup-open` with a
    `path` and a `line`, then with a directory -- the window's side is
    `right`, its width about half the frame, point is on the line, and the
    directory case yields a dired buffer. This is the ONE shared subroutine
    every open-a-file affordance must call; divergence is a defect by ruling.

### G. Interrupt and restart (3)

35. **ForcedRestartInterruptsTheTurn** -- `agent-repl-restart-workspace` with
    a prefix argument, mid-turn -- the roster arm settles `:interrupted` and
    the agent is NOT resumed. This is the interrupt path (see above).
36. **GracefulRestartHoldsPromptsMeanwhile** --
    `agent-repl-restart-workspace` without force, mid-turn -- prompts land in
    `agent-repl--prompt-queue` rather than being refused, and drain after.
37. **RestartDoesNotWedgeEmacs** -- forced restart with a live panel and
    webview binding -- heartbeat assertion, same family as 13.

### H. Reconnect, handover and rehydration (4)

38. **TabsRehydrateFromTheRosterOnConnect** -- stop and restart the daemon
    under Emacs -- `agent-repl-roster--tab-order` is rebuilt from the roster
    and `agent-repl--ws-tabline-names` matches it. The daemon is the source of which
    workspaces exist.
39. **ReRegisterIsIdempotentByDir** -- after a daemon restart --
    `agent-repl--workspaces` has the same count and the same names; no
    duplicate rows. Re-registering after a restart is the normal path, not an
    error.
40. **HandoverTransfersAtFreeness** -- a successor daemon --
    `agent-repl-link--successor` is promoted to `agent-repl-link--primary`
    and the old streams are released, driven by the `transferred` push.
41. **DaemonDownSurfacesAndReconnects** -- kill the daemon out from under
    Emacs -- `agent-repl-link--reconnect-timer` is armed,
    `agent-repl-link-no-daemon-functions` ran, and the link comes back when a
    daemon returns. Connection death is detected at the transport; no
    keepalive frames exist.

### I. Host-side refusal messages (3) -- Emacs-only

42. **SubmitWithNoDaemonIsRefusedLoudly** -- stop the daemon, then
    `agent-repl-send` -- the failure is surfaced and the composer text is
    PRESERVED, never silently dropped. Unary calls fail loudly by contract.
43. **NoWorkspacesRegisteredRefusesThePicker** -- `agent-repl-close-workspace`
    with an empty registry -- `user-error` "No agent-repl workspaces
    registered" and no wire call made. Rendered-string assertion: the
    message is the subject.
44. **NukeConfirmsBeforeDestroying** -- `agent-repl-nuke-workspace` with the
    confirmation reader answering no -- no wire call, worktree untouched.
    Nuke is the only verb that destroys data.

## The proof-of-life test

Scenario 20 is implemented first and alone, as
`TestEmacsProofOfLife` in `emacs_e2e_test.go`:

1. Sandbox up, store and sidecar started by Go, binaries built by the
   existing helpers.
2. Emacs up with a tty frame, booted through the image's Doom profile;
   `assertHostIsolation` has already passed, the readiness stamp has been
   read and its `doom` / `map_bang` / `popup_rule` / `agent_repl` facts
   asserted, and the heartbeat is armed.
3. Emacs spawns the daemon via `agent-repl-frontend-daemon-ensure`; the
   layer waits on `agent-repl-link--primary` becoming non-nil.
4. `agent-repl-add-project-workspace` on a scripted fake-git worktree.
5. `agent-repl-frontend-open-panel`; wait on the panel buffers appearing in
   `window-list`.
6. Insert a prompt into the input buffer; assert composer RET resolves to
   `agent-repl-send`, then PRESS RET.
7. Wait on Emacs's own state showing the response row, and cross-check the
   daemon's feed frame from the Go client so a green test cannot mean "Emacs
   drew something the daemon never sent".

It skips loudly without the sandbox, with the exact reason `Available()`
returned.

**Partly verified now that a container has run.** Observed for real, via
`sandbox/bin/e2e-sandbox.sh run --dir e2e go test . -run TestEmacsProofOfLife`:
the image builds; the dependency stage lands 2244 files; the store starts and
serves; `script` behaves as `StartPTY` assumes and Emacs takes a tty frame
(once `TERM` stopped being `dumb`). Observed since, via
`sandbox/bin/doom-boot-probe.sh`, which boots this profile exactly as
StartEmacs does: Doom DOES boot from a staged `~/.emacs.d` under a scratch
`HOME`; the copy set in `stageEmacsDir` is sufficient (nothing Doom writes at
startup hit a read-only path); `doom-after-init-hook` DOES exist in the
pinned `DOOM_REF`; and the server starts, so `map!`, `set-popup-rule!` and
`:app agent-repl` are all real. Still unobserved: that
`assertHostIsolation`'s four probes hold against the run script's actual
flags, and every one of `HeartbeatBound`, `emacsBootBound`, `doomBootBound`
and `doomStageBound`, which remain **PROVISIONAL** because no suite has yet
been run to completion in the sandbox.

### The bounds, measured

**Every `PROVISIONAL` marker is withdrawn.** The numbers below come from ten
healthy boots of this layer inside the sandbox — two `-count=5` runs of
`TestEmacsProofOfLife` — and a third `-count=5` that held the bounds derived
from them.

**Re-measured** over a `-count=2` of the whole layer (91 boots) after the image
began carrying Doom's native code and the scenarios became parallel; the
"today" column is that run. Two phases moved and one bound is new.

The harness measures itself. `Emacs.record` times each boot phase and
`reportPhases` logs every one on **every** run, passing or failing, so
`go test -v` always prints the numbers a future revision must re-derive its
bounds from. A bound here cannot quietly drift back into a guess.

| Bound | Observed healthy max | Value | Multiple | Why that multiple |
| --- | --- | --- | --- | --- |
| `HeartbeatBound` | 414ms | 1.25s | ~3x | The probe rides the same socket every scenario uses, so it queues behind whatever Emacs is doing: the maximum is set by the longest command a scenario runs, not by emacsclient's round trip. |
| `heartbeatInterval` | — | 250ms | — | Already *below* the observed probe latency, so the detector samples as fast as the command loop can answer. A shorter interval buys only queued probes. |
| `emacsBootBound` | 49ms | 500ms | ~10x | Larger than 3x deliberately: three times a number this small is not a bound, it is a race with the scheduler. |
| `daemonLinkBound` | 265ms | 1s | ~4x | **New.** `EnsureDaemon` was sharing `emacsBootBound` — one name over two unrelated events, so neither could be measured against its own phase. They differ by a factor of five. |
| `doomBootBound` | 1.162s → **967ms mean, 1.70s max today** | 3.5s | ~2x today | Native code is baked into the image now (`doom sync --aot`), which cut the mean from 1437ms to ~840ms serially; running two scenarios at once puts the worst back at 1.70s. NOT raised to keep a 3x ratio — loosening a product bound to accommodate the harness's own concurrency would be the harness marking its own homework. |
| `doomStageBound` | 7ms → **45ms mean, 93ms max today** | 5s | — | Down from 60s. The staged `.local/cache` carries 591 `.eln` files (58 MiB) now rather than 228 KiB, so the copy is no longer free — but there is still no slow-copy regime to bound: this exists to catch a copy that cannot finish, and it fails inside the test rather than at the suite's timeout. |
| `evalBound` | **409ms** | 1.25s | ~3x | **New.** A scenario's own `emacsclient --eval` was bounded by `HeartbeatBound` — the same conflation `daemonLinkBound` was split out of `emacsBootBound` to fix. The heartbeat probe is `(emacs-pid)` and does nothing; a scenario's form opens panels and creates an xwidget webview. It lands on the same number today by coincidence of two similar measurements, not by sharing one. |
| `xvfbReadyBound` | 121ms | 1s | ~8x | **New.** The 121ms is always the FIRST Xvfb in a fresh container, which pays once for creating `/tmp/.X11-unix`; the steady state is 21-62ms, and the multiple is taken against the slow first start. |

### Subr trampolines are built once per container, not once per scenario

**A boot bound that is missed by the harness's own waste is not a product
finding.** Redefining a *primitive* — every `advice-add` on a subr — makes
Emacs synthesize a "subr trampoline" so natively compiled callers see the
redefinition. Synthesizing one is a synchronous `call-process` of a whole
second Emacs, with the booting Emacs blocked in `read` until it returns.
`sandbox/doom/init.el` turns JIT compilation off and that does **not** cover
this: trampoline synthesis is governed by
`native-comp-enable-subr-trampolines`, and runs whether or not the JIT is
armed.

Nine primitives are advised in a scenario's Emacs — six by this module
(`status.el`, `close-panels-on-open.el`, `find-file-workspace.el`),
`define-key` by Doom's `general-auto-unbind-keys`, and `yes-or-no-p` by the
unanswerable-prompt guard the harness itself installs. Five of the nine happen
to be produced by the image's `doom sync --aot`; the other four were compiled
by **every scenario** into its own throwaway `~/.emacs.d` cache, forty-five
times a run, concurrently. Measured 2026-09-10: four scenarios died of it in
one run — `doomBootBound` missed with the breadcrumb stopping at "init.el
finished", `emacsclient` then failing with "exit status 1" against a server
that boot had not reached, and the scenario reported as a wedge. The `ps`
listing in each failure named the child: `emacs -no-comp-spawn -Q --batch -l
/tmp/emacs-int-comp-subr--trampoline-…`.

Turning trampolines **off** would not be a speedup, it would be a hole: an
Emacs without them is one where this module's own advice on
`modify-frame-parameters` and `set-window-buffer` is bypassed by every
natively compiled caller, and the layer would pass while testing something no
user runs.

So they are built **once per test binary**, before any scenario's Emacs
exists, into a container-wide directory that `EMACSNATIVELOADPATH` puts on
`native-comp-eln-load-path` — `comp--trampoline-search` consults that path
before compiling, so every boot finds all nine already on disk.

| Bound | Observed healthy max | Value | Multiple | Why that multiple |
| --- | --- | --- | --- | --- |
| `trampolinePrewarmBound` | 887ms (cold container, all nine compiled) | 20s | ~20x | The prewarm holds a parallelism slot and runs before any Emacs exists, so its measurement is always the idle one; the multiple is for a host carrying other agents' suites, and a miss here is a hard failure rather than a fallback to per-scenario compilation. |

`assertTrampolinesWerePrewarmed` runs after **every** boot and reads where
each installed trampoline was *loaded from*, so a primitive advised in future
that nobody adds to `emacsAdvisedPrimitives` fails there, named, on its first
scenario — instead of returning as a boot-bound flake on a busy machine.

### The parallelism bound, measured

Every scenario is `t.Parallel()`. The isolation that permits it is per-test by
construction, not by convention: `requireSandbox` mints a fresh
`os.MkdirTemp` scratch, so the Emacs `HOME`, its Doom tree, the state root,
both account roots, the store database, the kernel-lock directory and the fake
SDK's spool root are per-test paths; every socket carries four random bytes;
each Emacs draws on its **own** Xvfb started with `-displayfd 1`, so the X
server picks a free display under its own lock rather than one this layer
guesses; and there is no `t.Setenv` anywhere in the layer.

The machine is the constraint, so **the layer bounds itself** rather than
trusting `-parallel` to be passed correctly: `NewEmacsWorld` takes a slot
before the per-run builds and returns it after the teardown, and `StartEmacs`
refuses to run outside the gate. `AGENT_REPL_E2E_EMACS_PARALLEL` overrides the
count, and exists so the bound can be re-measured the same way it was measured
— not so a caller can turn it off.

Measured on the 4-CPU / 5.8 GiB Docker VM, one `-count=1` pass of the whole
layer at each setting:

| Slots | Wall | Peak container memory | Worst Doom boot (bound 3.5s) | Verdict |
| --- | --- | --- | --- | --- |
| 1 | 172s | 1.35 GiB | 1.50s | |
| **2** | **107s** | **1.52 GiB** | **1.58s** | **chosen** |
| 3 | 81s | 1.58 GiB | 1.78s | rejected: under sustained load (`-count=2`) three Emacsen on four CPUs pushed Doom's boot past 3.5s and failed three unrelated scenarios |
| 4 | 90s | 1.81 GiB | 2.94s | slower *and* visibly contended |

The bound is not the number that goes fastest on one lucky pass; it is the
largest number whose worst measured boot still fits the budget the product is
held to.

Those numbers were taken before the AT-SPI fix (`NO_AT_BRIDGE`), which turned
out to be the real source of the boot outliers the table blames on contention.
On the shipped layer, at two slots: a `-count=1` runs in **78s** with a
**1.14 GiB** peak, and two consecutive `-count=2` runs did **183 boots with
zero boot-bound misses**, worst 1.10s against a mean of 796ms.

### The container's memory budget

| Consumer | Before | Today |
| --- | --- | --- |
| `node_modules` on the working-copy tmpfs (`npm ci` twice per run) | 729 MiB | 0 — image-baked, linked |
| primed npm cache copied into HOME | 196 MiB | 0 — only copied on a lockfile mismatch |
| Go build cache | 331 MiB, compiled from cold every run | 479 MiB, copied from the image |
| webapp `dist` built in-container (`tsc` alone ~1 GiB peak) | ~1 GiB transient | 0 — host-staged |
| leaked Emacs / shim / shim-lock processes | grew to ~1.7 GiB over a run | 0 Emacs — waited for, escalated to, and fatal if left |
| **whole-run peak** | **3.08 GiB** | **1.22 GiB** |

Peak RSS by process, inside one container: Emacs with its GUI frame 206 MiB,
the Node shim 95 MiB, Xvfb / daemon / store / sidecar / `shim-lock` single-digit
to low-tens of MiB each.

### Teardown is ordered, and a survivor is a failure

Teardown used to ask and assume. It sent `(kill-emacs)` over `emacsclient`,
killed the pty parent, and returned. Killing the pty parent is not killing
Emacs: `script` forks its child into a session of its own — `setsid` plus
`TIOCSCTTY`, which is how the child gets a controlling terminal — so the wait
reaped the wrapper and the 206 MiB Emacs was reparented to init. The fallback
reaper found it afterwards and logged

    emacs teardown: reaping a stray this scenario left behind: pid N emacs

on scenarios that otherwise passed. That log line was the layer's only notice
that a scenario had ended owning a live Emacs — the same shape that produced
this layer's shared-server-socket failure and its 3 GiB peak.

Every step is now waited on:

1. the daemon is asked to stop through Emacs's own command, bounded by
   `teardownStopBound`, with both the transport failure and a REFUSED stop
   reported;
2. Emacs is asked to exit with `(kill-emacs)`;
3. **teardown waits for the process to leave `/proc`**, bounded by
   `emacsExitBound`. MEASURED over a full 47-scenario run instrumented at a
   30s bound: every healthy Emacs was gone on the first 50ms poll after the
   ask — 51ms, 52ms, 53ms, 54ms, the spread being poll quantization rather
   than variance. The bound is 500ms, ten times the worst of those;
4. an Emacs that misses that bound is escalated to its **process GROUP** —
   `SIGTERM`, then `SIGKILL`, each waited on for `emacsSignalBound` — because
   Emacs under a pty parent leads its own session and whatever it spawned is
   in that session with it. A group id that reads back as the test binary's
   own is refused and the bare pid is signalled instead, so a bad `/proc` read
   can never signal the suite;
5. the pty parent is killed last;
6. **teardown waits for the stopped daemon to leave `/proc` too**, bounded by
   `daemonExitBound`. Step 1's ack says the orderly exit was STARTED, not that
   it finished: `drain.ShutdownNow` announces, stands every session down,
   calls `Exit` and answers. Everything after that ack is separately bounded by
   the daemon itself — `shutdownGrace` (2s) for the in-flight requests,
   `merge.TerminalDrainBound` (2s) for a merge terminal's durable stamps, and
   `loopJoinBound` (2s) for the background loops — so the wait's bound is
   their sum, six seconds, and a daemon still standing afterwards has missed a
   bound it states itself. Without this wait there was a race the scenario
   could only lose, because the daemon's shutdown grace is held open by the
   standing streams of the very Emacs step 4 has just killed: the slower Emacs
   was to go, the more of that grace was still running when the reaper looked.
   Observed on a loaded host with Emacs's own exit escalating (1.06s against
   the 500ms bound), the reaper reported a `claude-repld` that was exiting
   exactly as designed. `go test -v` prints `emacs phase daemon-exit` for every
   scenario; on a quiet box it is one poll interval.

A stray the reaper still finds after all of that **fails the scenario that
left it**, naming the pid, its `/proc` state and wchan, and the steps teardown
had already tried. The reaping itself stays: a failing scenario must still not
leak a process into the next one.

`emacs_teardown_test.go` holds teardown's own tests — that the wait does not
return before the process is gone, that a process refusing `SIGTERM` is
escalated to its group (proved by a child of that group dying with it), that
the daemon wait does not return before the stopped daemon is gone and does not
run at all for a stop nobody made, and that a survivor is reported as a failure
and still reaped.

### What the proof-of-life test reaches today, and what blocks it

Steps 1-4 are **observed passing**, repeatedly:

- Emacs boots the image's real Doom on the Xvfb frame and the stamp's
  `doom` / `map_bang` / `popup_rule` / `agent_repl` facts hold.
- Emacs spawns the daemon through its own launcher and holds a link.
- `agent-repl-add-project-workspace` registers the scripted fake-git
  worktree, with a daemon-minted ref.
- The panel opens and **the webview is really there**: the live WKWebView
  read out of the workspace's own webview buffer answers with the daemon's
  own origin, e.g.
  `http://127.0.0.1:43945/?workspace=<id>&dir=<repo>`, serving the REAL
  webapp built from source. That is the step `make-xwidget: GTK has not
  been initialized` used to refuse.
- Composer `RET` resolves to `agent-repl-send` through the real Doom `map!`,
  is pressed, and the daemon answers `SubmitPrompt` with a turn id.

Step 5 — the roster row settling on a settled arm — was **blocked by a
production constraint, not by this layer**, and that constraint is now
fixed. The shim's session and workspace claims were `open(2)`'s `O_EXLOCK`
(`agent-shim/claude/shim/src/locks.ts`), which is macOS/BSD only; on Linux
the shim refused to start a session at all:

```
REFUSED StartSession: another shim holds this conversation's session lock
shim-session-lock: linux has no O_EXLOCK, so the shim cannot claim
  <session> exclusively; refusing to start rather than risk two shims
  writing one transcript
```

The daemon surfaced that as `OpenWorkspaceError.conversation_owned` and the
row stayed at `:none`. The refusal was deliberate and correct in itself — a
silent no-op would hand the daemon a false "free" — but it meant no real
shim could start a session inside the Linux sandbox, which is where this
whole suite runs.

Each claim is now a `shim-lock` CHILD PROCESS (`agent-shim/shim-lock`)
taking the real `flock(2)` the daemon probes, on one code path for both
platforms. `NewEmacsWorld` builds the binary and states it on the Emacs
process's environment as `AGENT_REPL_SHIM_LOCK_BIN`, beside
`AGENT_REPL_LOCK_DIR`, so the daemon Emacs starts hands it to every shim it
spawns.

## Registration

`bin/test-all.sh`'s `ALL_SUITES` roster does not contain `e2e` at all today,
and `daemon/internal/workspace/merge/suiteselect.go` maps no path to it, so
neither the cross-system suite nor this layer runs in the merge gate. That is
a pre-existing gap, not one this layer introduces, and it is called out here
because a suite outside the gate is a suite that stops being true. Wiring it
up needs a project-lead ruling on cost, since this layer starts a container.
