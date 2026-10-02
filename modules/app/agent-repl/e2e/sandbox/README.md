# e2e sandbox

A container in which the cross-system e2e suite can run a real Emacs, a real
Doom, and a real `modules/app/agent-repl` without touching anything on the
host machine.

**Nothing in this directory has been executed against a container runtime.**
`docker info` fails on the machine this was authored on (the daemon is not
running), so the image has never been built and the suite has never run
inside it. Every claim below about what the image *contains* is a claim about
what the `Dockerfile` says, not an observation. See
[What is unverified](#what-is-unverified).

## Why it exists

The e2e suite composes the four systems exactly as production wires them: a
real `claude-repld`, a real `shim-store`, a real `shim-claude-sidecar`, and
the real TypeScript shim. The Emacs-side integration suites additionally need
a real Emacs with the module loaded through Doom. Run on the host, that means:

- `~/.claude` and `~/.claude.json` — shared with the developer's own Claude
  sessions, and cross-account writes there have already cost one bare-metal
  re-login.
- `~/.emacs.d` and the Doom package/cache tree — a `doom sync` for a test
  profile would rewrite the editor the developer is working in.
- `~/.config` — this repo IS the doom config; a test that writes there edits
  the checkout.
- `~/.gitconfig` — the module shells out to git.
- `launchd` — `services.el` manages `shim-store` and `shim-claude-sidecar` as
  OS-managed services.

The sandbox makes all five container-local, so a run cannot reach the host's.

## How host-state safety is enforced

Not by convention — by construction, at four independent layers:

1. The repo is bind-mounted **read-only** at `/repo-src`. The entrypoint
   probes `[[ -w /repo-src ]]` and refuses to run if the mount turns out to
   be writable, so a mis-specified mount fails loudly instead of quietly
   writing to the checkout.

2. `HOME` is `/sandbox/home`, a path that exists only inside the image and
   that no mount ever covers. `XDG_CONFIG_HOME`, `XDG_CACHE_HOME`,
   `XDG_DATA_HOME`, `DOOMDIR`, `EMACSDIR`, `GOPATH`, `GOMODCACHE`, `GOCACHE`
   and `NPM_CONFIG_CACHE` are all pinned underneath it.

3. **No other host path is mounted at all** — the only exception is the
   artifacts directory, mounted read-write and only when the caller sets
   `AGENT_REPL_E2E_ARTIFACTS`.

4. The container runs as a non-root uid (`1000:1000`) with `--read-only` on
   the container filesystem; the writable areas are tmpfs mounts, which live
   in the runtime's memory and not on any host path the developer uses.

`--network none` is also passed, both because nothing at test time needs the
network and because it removes the one remaining way a run could reach out.

## Build

```bash
# from the repo root
modules/app/agent-repl/e2e/sandbox/bin/e2e-sandbox.sh build
```

That is the whole command: the identifiers the Dockerfile cannot resolve
from a checkout are **recorded in `pins.env`** — the base image digest, the
per-architecture Node and Go tarball checksums, the **upstream Emacs commit**,
the Doom commit SHA and the snapshot stamp — so no `--allow-unpinned` is
needed.

The gate is unchanged. `build` still **refuses to run** if an identifier is
pinned neither in `pins.env` nor in the environment, and an environment
variable of the same name always wins over the recorded value, so overriding
a pin stays deliberate:

```bash
# build against a different Doom, leaving every other pin as recorded
SANDBOX_DOOM_REF=<sha> modules/app/agent-repl/e2e/sandbox/bin/e2e-sandbox.sh build

# build against moving targets on purpose (only needed with no pins.env)
modules/app/agent-repl/e2e/sandbox/bin/e2e-sandbox.sh build --allow-unpinned
```

`--no-cache` is also accepted.

`SNAPSHOT_STAMP` must be **at or after the base image's build date**: an older
snapshot offers only older `libc6`/`perl-base` than the base image already
has, and apt then refuses the install as held broken packages. Bump the stamp
whenever the base digest is bumped.

Every build ends in a verification step, so a build cannot report success
without a working image. The image must exist, carry
`emacs`/`node`/`npm`/`go`/`git`/`rsync`/`script`/`doom`/`Xvfb` on a **login**
shell's PATH, have `doom sync` baked (the profile's `.local`), and its Emacs
must

- report version **30.2** (the host build),
- have **xwidgets** compiled in (`(featurep 'xwidget-internal)`),
- have **native compilation** actually available (`(native-comp-available-p)`,
  which is a stronger claim than "configure said yes" — it needs `libgccjit`
  and `gcc` present at run time).

Any of those missing fails the build loudly. The same three are asserted in
the Dockerfile; they are re-asserted in `verify_image` so the property
belongs to the *image*, not to one build path.

### One build at a time, and the image says which sources made it

Two structural properties, because five simultaneous builds thrashed one
buildkit cache and a build from a stale checkout re-tagged
`agent-repl-e2e-sandbox:latest` over a good image:

* **`build` takes an exclusive, host-wide lock.** It is a directory claimed
  with `mkdir` at `/tmp/agent-repl-sandbox-build.lock` (override with
  `AGENT_REPL_SANDBOX_BUILD_LOCK_DIR`), with the holder's pid recorded inside
  and a dead holder's lock taken over — the same technique as
  `bin/suite-slot.sh` and the run gate. A second build **waits**, out loud,
  and then does nothing at all if the tag already carries this checkout's
  sandbox sources. `--force` (or `--no-cache`) rebuilds anyway.

* **The image is labelled with its sources.** The git tree hash of
  `e2e/sandbox` is written on as `org.agent-repl.sandbox-tree` (a dirty
  sandbox directory gets a `dirty-<digest>` stamp instead, so it never claims
  to be the committed tree). `run` reads it back and **refuses**, naming both
  stamps, when it is not this checkout's — including when the image carries no
  stamp at all. `AGENT_REPL_SANDBOX_ALLOW_STALE=1` overrides it deliberately
  and says so.

The gate is covered by `e2e/sandbox_build_lock_test.go`, which drives the
script against a fake `docker` on `PATH`; it never needs a real runtime.

## Emacs is built FROM SOURCE, not installed from apt

**The webapp panel is an `xwidget-webkit` webview, and no distro Emacs is
built with xwidgets.** Debian's `emacs-nox` has no GUI at all, and even
`emacs-gtk` is built `--without-xwidgets`. On a distro package the panel
simply does not work, so a suite that drives it could not exist. The image
therefore compiles **the same Emacs the user runs**: `SANDBOX_EMACS_REF`
pins the upstream commit their Emacs was built from (Emacs 30.2,
*"Development version 636f166cfc86 on HEAD branch"*), gated exactly like
every other pin — an empty `EMACS_REF` fails the build.

That also settles a version floor which had made a distro package unusable
anyway: the pinned Doom refuses to start an **interactive** session below
Emacs 29.1, and a `-nw` frame is interactive, so bookworm's 28.2 aborted in
`early-init.el` with *"Detected Emacs 28.2, but interactive Doom needs
>=29.1"* — `doom!` never ran and no module `config.el` was ever loaded.

The base is Debian **trixie** because it carries the GTK 3 / WebKitGTK 4.1
and gcc-14 / libgccjit-14 this build wants. The source build happens in a
**separate stage**, so the ~1.5GB of `-dev` packages and the Emacs checkout
never reach the finished image; only the installed tree is copied in,
alongside the runtime halves of those libraries (plus `gcc`, `binutils` and
`libgccjit0`, which a native-compiled Emacs genuinely needs at *run* time —
it shells out to the assembler and linker whenever `doom sync` compiles a
`.el` that is not already in its eln cache).

### The configure line, and every departure from the host's

The host (darwin) configures with

```
--with-native-compilation=aot --with-tree-sitter --with-modules --with-gnutls
--with-xml2 --with-ns --with-xwidgets --disable-gc-mark-trace
CFLAGS='-O3 -march=native -DFD_SETSIZE=10000 -DDARWIN_UNLIMITED_SELECT'
```

Everything there is kept, except where it means nothing on linux:

- `--with-ns` → **`--with-pgtk`**. `--with-ns` is NeXTstep/Cocoa: darwin-only,
  and `configure` on linux rejects it. On linux the window system that carries
  xwidgets is GTK 3 + WebKitGTK, as either `--with-pgtk` (pure GTK, one
  drawing path for X11 and Wayland) or `--with-x-toolkit=gtk3`. pgtk is used,
  and that it carries xwidgets in this version is **verified, not assumed**:
  the build greps `configure`'s own summary for xwidgets support and then asks
  the finished binary for `xwidget-internal`. Either check failing stops the
  build — there is no quiet fallback to an Emacs without the feature.
- `-march=native` → **dropped**. A container image must not be compiled for
  whichever machine happened to build it; that flag bakes the build host's
  instruction set in and the image would then `SIGILL` on an older CPU.
- `-DDARWIN_UNLIMITED_SELECT` → **dropped**. Pure darwin: it opts into the
  macOS `select()` that is not capped at `FD_SETSIZE`. On linux the define
  names nothing.
- `-DFD_SETSIZE=10000` → **kept**. glibc's `select()` honors `FD_SETSIZE` at
  compile time, and harmless where unused.
- `-O3` → **kept**; generic and architecture-neutral.
- `-g` → **added**, the one flag the host's line does not carry. This Emacs
  exists to be diagnosed: when it stalls it answers no lisp witness at all, so
  the only account left is a native backtrace, which the harness takes with
  `gdb` and files with the failure artifacts. Without `-g` those frames carry
  bare function names and nothing about where inside a long C function the
  loop is spinning. It changes no code generation — the same `-O3` objects,
  with DWARF beside them — so nothing this Emacs *does* differs because of it;
  the cost is image size, paid once per image.
- `--with-native-compilation=aot`, `--with-tree-sitter`, `--with-modules`,
  `--with-gnutls`, `--with-xml2`, `--with-xwidgets`,
  `--disable-gc-mark-trace` → **kept verbatim**; none is platform-specific.

`--with-native-compilation=aot` natively compiles the whole of `lisp/` at
build time. **That is why this image is slow to build and large** — the cost
is paid once per image and never by a test run. Real numbers are in
*Build cost*, below.

### Giving a scenario a GUI frame

An `xwidget-webkit` webview cannot exist on a tty frame: it needs a
*graphical* frame, which needs a display, and `--network none` rules out a
remote one. The image therefore carries **`Xvfb`** (and `xauth`). A GUI
scenario runs

```bash
Xvfb :99 -screen 0 1280x1024x24 &
export DISPLAY=:99
export GDK_BACKEND=x11   # this Emacs is pgtk and would otherwise want Wayland
```

and then starts Emacs with a graphical frame instead of `-nw`.

**The Emacs layer now does exactly this.** `e2e/emacs_display_test.go`
starts one Xvfb per test with `-displayfd`, so the *server* picks a free
display and reporting it is the same edge as it being ready, and
`StartEmacs` boots plain `emacs` — no `-nw` — with `DISPLAY` and
`GDK_BACKEND=x11`. The tty-frame boot is withdrawn: the panel is an
xwidget-webkit webview, so on a tty frame it could not be created at all.

## Run

```bash
S=modules/app/agent-repl/e2e/sandbox/bin/e2e-sandbox.sh

# what the sandbox needs, and what is missing
$S preflight

# one e2e test — `e2e` is its OWN Go module (e2e/go.mod), so `--dir e2e`
# cd's the container there before running the command
$S run --dir e2e go test . -run TestTurnLifecycle -v

# the whole cross-system suite
$S run --dir e2e go test ./...

# one Emacs suite
$S run emacs -batch -Q -l ert -l lisp/test-status.el -f ert-run-tests-batch-and-exit

# the module loaded through the sandbox's Doom profile
$S run doom sync

# a unit suite of another submodule with its own go.mod, e.g. the daemon
$S run --dir daemon go test ./...

# does INTERACTIVE Doom actually come up in this image? (see below)
$S run --dir e2e/sandbox bin/doom-boot-probe.sh

# poke around
$S shell
```

The working directory inside the container is
`/work/repo/modules/app/agent-repl`, so every command above is written
relative to the module, exactly as `AGENTS.md` documents it — `--dir <path>`
(module-relative) cd's the container into a submodule first, which is
required for `e2e` (and any other submodule carrying its own `go.mod`, such
as `daemon`) since a bare `go test` run from the module root has no `go.mod`
to find there and fails with "go.mod file not found in the current directory
or any parent directory".

Logs stream straight through: stdout stays stdout, stderr stays stderr, and
nothing is captured or buffered, so a failing test's output is the caller's
output. Set `AGENT_REPL_E2E_ARTIFACTS=/some/host/dir` to have the suite's own
artifacts (`world_test.go`'s `ArtifactsEnv`) land on the host.

## What is mounted where

| container path | source | mode | why |
|---|---|---|---|
| `/repo-src` | the repo root on the host | **read-only** bind | the source under test |
| `/work` | tmpfs | rw | holds `/work/repo`, the writable working copy |
| `/work/repo` | `rsync` of an ALLOWLIST of `/repo-src/modules/app/agent-repl` (see `STAGE_ENTRIES` in `bin/entrypoint.sh`) | rw | builds and `npm ci` need to write; the read-only source cannot be written. Nothing outside the module is copied — no repo-root `go.work`, `.nvmrc` or shared config exists, and a whole-checkout copy dragged in `.claude/worktrees` (18 GB of stale agent worktrees) until the host ran out of file descriptors. Staged at the module's real relative path, so the checkout-root-relative resolutions in the Go `replace` directives and the TypeScript `../../../proto/...` imports still land. A missing entry is a loud refusal, not a late confusing failure. Because the mount is a LIVE checkout, rsync's exit 24 (source files vanished mid-copy) is tolerated with a loud per-path summary; every other nonzero exit is fatal. |
| `/sandbox/home` | tmpfs | rw | `HOME`: `~/.claude`, `~/.emacs.d` caches, git config, npm/Go caches |
| `/sandbox/doom/modules` | tmpfs | rw | the entrypoint symlinks `:app agent-repl` here at the mounted source |
| `/tmp` | tmpfs (exec) | rw | UDS paths, per-run temp dirs |
| `/artifacts` | `$AGENT_REPL_E2E_ARTIFACTS`, if set | rw bind | the only writable host path, and only on request |

## What is baked vs mounted

Baked into the image at **build** time, so no test pays for it and no run
needs the network:

- Debian **trixie** packages (`git`, `curl`, `rsync`, `ca-certificates`,
  `xz-utils`, and `bsdutils` + `util-linux` for `script(1)`) from a fixed
  `snapshot.debian.org` archive.
- **Emacs 30.2, compiled from source** at the pinned upstream commit, with
  xwidgets, native compilation (AOT), tree-sitter and modules.
- Node 22.14.0 and Go 1.24.6, from upstream release tarballs.
- Doom Emacs, cloned and checked out at an exact SHA, into `/sandbox/emacs.d`.
- The minimal Doom profile (`doom/`), and `doom install` + **`doom sync --aot`**.
  The `--aot` is not cosmetic: without it the image ships zero `.eln` files and
  every scenario's Emacs starts Doom's asynchronous JIT compiler to produce the
  same 591 of them again, into a per-test `~/.emacs.d` that is discarded
  seconds later. The profile turns JIT compilation off for the same reason.
- **The node dependency trees themselves** for `agent-shim/claude/shim` and
  `webapp`, installed by `npm ci`, plus the sha256 of each lockfile they were
  installed from — and the npm cache behind them, for the fallback below.
- The Go module cache for all seven `go.mod` modules under the module, primed
  by `go mod download`, **and the Go BUILD cache**, primed by `go build ./...`
  plus `go test -c -o /dev/null` per package. The build prime runs at
  `/work/repo/modules/app/agent-repl`, the exact path the entrypoint
  materializes the working copy at, because Go's cache key carries the
  package's absolute directory when `-trimpath` is absent.
- A container-local git identity.

Mounted / materialized at **run** time:

- the repo source (read-only) and its writable `rsync` copy — which now
  includes `webapp/dist`, the one `dist/` the allowlist admits (see
  "The webapp dist" below);
- `node_modules` **symlinked** at the image's baked trees, after checking the
  checkout's lockfile hashes to the digest the image installed from. On a
  mismatch the run says so, prints both digests, and falls back to a real
  `npm ci --offline` — the shortcut can never silently test an older
  dependency tree. The linked tree is **read-only**, so nothing a run does may
  write inside `node_modules` — see "Nothing writes inside `node_modules`"
  below. `SANDBOX_NODE_MODULES=copy` gives a writable per-run copy, `=install`
  forces the install; both are forwarded from the caller's environment;
- the Go build cache, copied out of the image into `$GOCACHE` (`go` writes to
  its cache on every invocation, and the image layer is read-only). Seeding it
  cannot be *wrong*, only useless: the cache is content-addressed, so an entry
  compiled from different bytes is simply not found;
- the Doom module symlink pointing `:app agent-repl` at the checked-out
  source, replacing the build-time snapshot of the loader files.

### Nothing writes inside `node_modules`

The baked trees are an image layer, so the `node_modules` a run sees is
read-only. That is the price of not paying ~730 MiB of tmpfs per container,
and it is a real constraint on the suites: a tool that writes inside
`node_modules` works on the host and fails only in here.

One did. Vite's default `cacheDir` is `node_modules/.vite`, and vitest creates
it before it runs anything, so all eleven `TestWebappLayer*` areas failed
inside the container while passing on the host — with a filesystem error
nowhere near the config that caused it. The fix is not `SANDBOX_NODE_MODULES=copy`
(that buys the write back at the price the bake exists to avoid); every vite
and vitest config in `webapp/` now points `cacheDir` at `webapp/.vite-cache`,
a directory that is writable in the container and on the host alike. See
`webapp/vite-cache.ts` for the whole account, `webapp/test/vite-cache.test.ts`
for the check that holds every config to it, and `wlRequireWebappWritable` in
`e2e/webapplayer_e2e_test.go` for the precondition that fails the layer loudly,
by name, if the place it writes ever stops being writable again.

### The webapp dist

The Emacs client layer's panel is a real `xwidget-webkit` webview on the
daemon's own origin, so it needs the REAL built webapp, not a stub. That build
used to happen inside the container on the first Emacs scenario of every run:
`tsc --noEmit && vite build`, about six seconds and roughly a gigabyte of
resident memory for `tsc` alone, on a tmpfs, inside a 5.8 GiB VM, immediately
before a real Emacs started.

It is host-staged now. `bin/webapp-dist.sh` holds the one staleness rule:

- `e2e-sandbox.sh run` runs `webapp-dist.sh ensure` on the **host** before the
  container starts — check, and rebuild through `bin/build-frontend.sh --force
  webapp` when stale;
- the entrypoint stages `webapp/dist` in with the working copy;
- the harness inside the container runs `webapp-dist.sh check` — the same
  script, the same rule — and **fails**, naming the command to run, rather than
  serving an older bundle to the webview.

The rule covers a superset of `bin/build-frontend.sh`'s source set: the
generated protobuf TypeScript and vocab JSON that `webapp/src` imports from
`../../proto/` are sources here too. `AGENT_REPL_SANDBOX_NO_WEBAPP_BUILD=1`
skips the host build; it does not skip the container's check, so the only thing
it can buy is a louder failure.

### The concurrency gate

One sandbox peaks at 1.22 GiB. The Docker VM has 5.79 GiB and is **shared** —
unrelated containers were holding 1.82 GiB of it on the machine this was
measured on. Two sandboxes started at once did not run slowly, they OOM-killed
each other: `npm ci` exited 137 and Emacs was SIGKILLed mid-scenario, which
then reads as a test failure with nothing wrong in it.

So `e2e-sandbox.sh run` takes a slot before it builds or starts anything, and a
second run **waits**, out loud, naming the gate and the override. The slot
count is computed rather than assumed — VM `MemTotal` from `docker info`, minus
what the running containers are actually using, minus headroom, divided by the
per-sandbox budget — and it answers **1** whenever it cannot tell, because a
gate that opens wide on a failed `docker info` would be worse than none.

Slots are directories under `/tmp/agent-repl-e2e-sandbox.slots`, claimed with
`mkdir`: the one filesystem operation that is atomic and fails if the name
exists, so two runs cannot both believe they hold the same slot. Runs from
different worktrees of this repo therefore gate against each other. A slot
whose holder died is reclaimed by reading its recorded pid from a slot that
still *exists* — nobody can be claiming it at that moment — and moving the
directory aside: a slot becomes claimable only by disappearing.

| Variable | Default | What it does |
| --- | --- | --- |
| `AGENT_REPL_SANDBOX_SLOT_DIR` | `/tmp/agent-repl-e2e-sandbox.slots` | where the slots live |
| `AGENT_REPL_SANDBOX_MEM_BUDGET_MB` | 2048 | assumed memory per sandbox (measured peak 1.22-1.58 GiB, plus a quarter) |
| `AGENT_REPL_SANDBOX_MEM_HEADROOM_MB` | 1024 | left to the VM itself |
| `AGENT_REPL_SANDBOX_MAX_SLOTS` | 4 | cap regardless of memory; past a handful the shared CPUs bind, not the RAM |
| `AGENT_REPL_SANDBOX_CPUS` | 4 | CPUs one container may use (`docker run --cpus` and `GOMAXPROCS` inside); `bin/test-e2e-emacs.sh` sets it to the core slots testrun reserved |
| `AGENT_REPL_SANDBOX_NO_GATE` | unset | run without the gate, deliberately |

`GOPROXY=off` and `npm_config_offline=true` are set in the image, so a
dependency that was *not* baked fails loudly at run time rather than silently
reaching for the network — which `--network none` would deny anyway.

## The minimal Doom profile

`doom/init.el` is **not** the user's personal profile. It enables only:
`:ui (popup +defaults)`, `:ui workspaces`, `:editor (evil +everywhere)`,
`:emacs vc`, `:tools magit`, `:lang emacs-lisp`, `:app agent-repl`,
`:config (default +bindings)`. Each carries its justification inline, as does
each notable omission.

Two omissions worth restating:

- **`:term vterm` is not enabled.** The host profile has it and the module's
  `packages.el` discusses it, but no agent-repl source calls a vterm
  function: `sibling-popup.el` only names the `*doom:vterm*` *buffer* and
  `history.el` only compares a stored `:frontend` symbol against `'vterm`.
  Omitting it also spares the image vterm's native build chain.

- **Emacs has xwidgets, and the Emacs layer now drives them.** The binary is
  built `--with-xwidgets` and the image carries `Xvfb`, and
  `e2e/emacs_display_test.go` starts one per test while `StartEmacs` takes a
  graphical frame on it. `TestEmacsProofOfLife` asserts the live WKWebView
  and reads back the daemon origin it navigated to. The entrypoint still
  starts no display of its own, deliberately: the layer that needs one owns
  its lifetime, so a non-GUI run pays nothing.
    - Readiness for the webview is asserted where it belongs — in the
      scenario, by reading the workspace's own webview buffer — rather than
      folded into the Doom readiness stamp, which correctly says only "Doom
      is up AND emacsclient answers".
    - **The shim's kernel claims work here.** They used to be
      `open(2)`'s `O_EXLOCK`, which is macOS/BSD only, so on Linux the shim
      refused to start ANY session and every sandbox scenario needing a turn
      was stuck behind it. The claim is now a `shim-lock` child process
      (`agent-shim/shim-lock`) taking a real `flock(2)` — one code path on
      both platforms. The e2e harness builds it and hands the daemon
      `AGENT_REPL_SHIM_LOCK_BIN`; it is a plain Go module with no
      dependencies beyond `agentrepl/logging`, so the image's offline module
      cache already covers it.

## `script(1)` is a guarantee, not an inference

The Emacs client layer allocates its own pty with `script -q -c CMD
/dev/null`, because Emacs needs a tty for a real frame and the run script
only passes `-t` when its own stdout is a terminal — which under `go test` it
is not. `script` lives in `bsdutils` on trixie, a Debian `required`
package, so it was already there; **`bsdutils` and `util-linux` are now in
the apt list by name anyway**, so it is pinned to `SNAPSHOT_STAMP` like every
other package and a reshuffle of the binary between those two packages cannot
silently remove it. The harness keeps a runtime `exec.LookPath("script")`
check as a backstop for an image built before that line existed.

## The Emacs client layer boots this profile

`e2e/emacs_test.go` does **not** run `emacs -Q` any more. It boots the Doom
baked here, so `map!`, `set-popup-rule!` and module load order are all real.
Three things in `doom/` exist for it, and all three are inert without the
environment variables the Go layer sets:

- **`doom/init.el`** pins `server-name` to `$AGENT_REPL_E2E_SERVER` and
  `server-socket-dir` under the same scratch root **before anything can load
  `server`**, and loads `$AGENT_REPL_E2E_SETTINGS` right after its `doom!`
  form — before any module's `config.el`, which is the last moment at which
  `agent-repl-frontend-auto-start` nil can still stop cold start from
  spawning a daemon against the host's defaults.
- **`doom/config.el`** defines the file-based readback helper
  (`agent-repl-e2e--eval`) and, on Doom's own after-init edge, enables
  `tab-bar-mode`, binds the server socket (or notices that Doom's own
  `use-package! server` already bound it, under the name init.el pinned), and
  writes the readiness stamp at
  `$AGENT_REPL_E2E_READY` **last** — so the stamp means "Doom is up AND
  emacsclient answers". A boot failure writes the same file with
  `"ok": false` and the elisp error.
- **Emacs is 30.2** (the source build above). The layer still aims Emacs at a
  per-test `~/.emacs.d` through `HOME` rather than `--init-directory`,
  staged from `/sandbox/emacs.d`: sources symlinked, the `.local` tree copied
  minus `straight/`. That staging is not a version workaround and does not go
  away on a newer Emacs — the container is `--read-only` and
  `/sandbox/emacs.d` is not a tmpfs, so Doom's local tree cannot be written
  in place wherever init is pointed.

`$S shell` and `$S run doom sync` are unaffected: none of the environment
variables above is set there.

## Preflight

`bin/preflight.sh` is what the harness calls to decide whether to run or to
skip. It exits `0` when ready, and otherwise prints an actionable message and
exits `10` (no runtime), `11` (runtime present, daemon unusable), `12` (image
not built), `13` (insufficient disk) or `14` (the runtime did not answer within
the bound; its engine is wedged).

Every call it makes into the runtime is BOUNDED, so a preflight either answers
or is reported as not answering. A wedged Docker Desktop — backend alive, socket
never answering — once held `docker info` for over nine minutes inside the Go
harness's `sync.Once` and killed the whole e2e package on its own timeout. The
per-call bound is `AGENT_REPL_SANDBOX_RUNTIME_TIMEOUT_SECONDS` (default 10).

The harness must turn a non-zero exit into a **loud skip that quotes this
output verbatim** — never a silent pass, and never a fallback to an
unsandboxed run.

On the authoring machine today, `preflight` prints exactly:

```
agent-repl e2e sandbox UNAVAILABLE: 'docker' is installed but not usable.
  'docker info' failed — the daemon or machine is not running.
  Start Docker Desktop (or dockerd), then re-run.
  Then build the image:  modules/app/agent-repl/e2e/sandbox/bin/e2e-sandbox.sh build
```

and exits `11`. Knobs: `AGENT_REPL_SANDBOX_RUNTIME`,
`AGENT_REPL_SANDBOX_IMAGE`, `AGENT_REPL_SANDBOX_MIN_FREE_GB` (default 12),
`AGENT_REPL_SANDBOX_RUNTIME_TIMEOUT_SECONDS` (default 10).

## podman

The scripts pick `docker` first, then `podman`, and honor
`AGENT_REPL_SANDBOX_RUNTIME=podman`. Every flag used —
`build -t -f --build-arg --no-cache`, `run --rm --init --network none --user
--read-only --tmpfs --mount type=bind,readonly --env --workdir`,
`image inspect`, `info` — is in podman's docker-compatible CLI, so `podman`
should be a drop-in. Three podman-specific notes:

1. On macOS podman needs its VM up (`podman machine start`); preflight's exit
   `11` says so.

2. Rootless podman maps the container's uid 1000 into the user's subuid
   range, so the `--user 1000:1000` above is not the host's uid 1000. This is
   *safer*, not less safe: the read-only bind stays read-only either way.

3. `# syntax=docker/dockerfile:1.7` at the top of the Dockerfile is a
   BuildKit frontend directive. Podman's buildah ignores it and builds with
   its own parser; nothing in this Dockerfile needs a BuildKit-only feature,
   so that is fine, but it is why the directive is a comment and not a
   requirement.

## What is verified, and what is not

Verified by building and running the image on 2026-09-04 (arm64, Docker
Desktop). Everything below was OBSERVED, not reasoned about:

- **`bin/e2e-sandbox.sh build` succeeds and `verify_image` passes.** Apt at
  `SNAPSHOT_STAMP`, the Emacs source build, both tarball checksums, the Doom
  clone, `doom install`, `doom sync`, and both cache primes all ran.
- **Emacs is the host's own build.** From inside the image:

  ```
  GNU Emacs 30.2
  emacs-version: 30.2
  repo-version: 636f166cfc86aa90d63f592fd99f3fdd9ef95ebd
  (featurep 'xwidget-internal)  => t
  (native-comp-available-p)     => t
  system-configuration-options:
    --prefix=/usr/local --with-native-compilation=aot --with-tree-sitter
    --with-modules --with-gnutls --with-xml2 --with-pgtk --with-xwidgets
    --disable-gc-mark-trace 'CFLAGS=-O3 -g -DFD_SETSIZE=10000'
  ```

  `configure`'s own summary reported "Does Emacs support Xwidgets? yes" and
  "Does Emacs have native lisp compiler? yes", and the runtime WebKitGTK is
  2.40.5 — under Emacs's `< 2.41.92` ceiling, which is the whole reason for
  the September-2023 snapshot.
- **INTERACTIVE Doom boots and publishes its readiness stamp.**
  `bin/doom-boot-probe.sh` replicates what `e2e/emacs_test.go` StartEmacs does
  — staged `~/.emacs.d` under a scratch HOME, a pty from `script(1)`,
  `TERM=xterm-256color` — and reports the stamp:

  ```json
  {"ok":true,"emacs_version":"30.2","doom":true,"doom_version":"2.1.0",
   "map_bang":true,"popup_rule":true,"agent_repl":true}
  ```

  Every field `awaitDoom` asserts is true, which is what lifts the Emacs-28.2
  block recorded in `e2e/EMACS-LAYER-SPEC.md`. The probe exits non-zero and
  dumps the pty when the stamp is missing or reports a failed boot; it is how
  both bugs above were found.
- **The offline claim holds for the entrypoint's own work.** `npm ci
  --offline` resolved both lockfiles from the baked cache under
  `--network none`.

Still unverified, stated plainly:

- **No e2e or ERT suite has been RUN inside the sandbox.** Whether
  `go test ./...` and the ERT suites pass under this Emacs on Linux is
  unknown; only the boot they depend on is established.
- **`GOPROXY=off` has not been exercised by an actual build.** The module
  cache was primed successfully, but no `go build`/`go test` has consumed it
  offline, so a gap between `go mod download`'s build list and what a test
  actually needs would still surface at run time.
- **The xwidget webview HAS been created** — observed, on the Xvfb the Emacs
  layer starts: the panel's live WKWebView answers with the daemon's own
  origin (see *Limitations*).
- **Only arm64 has been built.** The amd64 checksums in `pins.env` are
  recorded but unexercised.

Verified on the authoring machine:

- `bin/*.sh` are `shellcheck -x` clean and `bash -n` clean.
- `preflight.sh` produces the exit-`11` message quoted above against the real
  host state.
- `stage_context` assembles the build context correctly from the live
  checkout: three loader files, 97 `lisp/*.el` sources, both npm lockfile
  pairs, and all 7 `go.mod` (+ `go.sum`) manifests at their own relative
  paths.
