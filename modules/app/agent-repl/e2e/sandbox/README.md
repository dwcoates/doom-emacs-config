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

That is the whole command: the four identifiers the Dockerfile cannot resolve
from a checkout are **recorded in `pins.env`** — the base image digest, the
per-architecture Node and Go tarball checksums, the Doom commit SHA and the
snapshot stamp — so no `--allow-unpinned` is needed.

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
without a working image: the image must exist, carry
`emacs`/`node`/`npm`/`go`/`git`/`rsync`/`script`/`doom` on a **login** shell's
PATH, and have `doom sync` baked (the profile's `.local`). Any of those
missing fails the build loudly.

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
| `/work/repo` | `rsync` of `/repo-src` (minus `.git/`, `node_modules/`) | rw | builds and `npm ci` need to write; the read-only source cannot be written |
| `/sandbox/home` | tmpfs | rw | `HOME`: `~/.claude`, `~/.emacs.d` caches, git config, npm/Go caches |
| `/sandbox/doom/modules` | tmpfs | rw | the entrypoint symlinks `:app agent-repl` here at the mounted source |
| `/tmp` | tmpfs (exec) | rw | UDS paths, per-run temp dirs |
| `/artifacts` | `$AGENT_REPL_E2E_ARTIFACTS`, if set | rw bind | the only writable host path, and only on request |

## What is baked vs mounted

Baked into the image at **build** time, so no test pays for it and no run
needs the network:

- Debian packages (`emacs-nox`, `git`, `curl`, `rsync`, `ca-certificates`,
  `xz-utils`, and `bsdutils` + `util-linux` for `script(1)`) from a fixed
  `snapshot.debian.org` archive.
- Node 22.14.0 and Go 1.24.6, from upstream release tarballs.
- Doom Emacs, cloned and checked out at an exact SHA, into `/sandbox/emacs.d`.
- The minimal Doom profile (`doom/`), and `doom install` + **`doom sync`**.
- The npm package cache for both `agent-shim/claude/shim` and `webapp`,
  primed by an `npm ci` from their lockfiles in a throwaway tree.
- The Go module cache for all seven `go.mod` modules under the module,
  primed by `go mod download`.
- A container-local git identity.

Mounted / materialized at **run** time:

- the repo source (read-only) and its writable `rsync` copy;
- `npm ci --offline` in the working copy, resolving only from the baked
  cache;
- the Doom module symlink pointing `:app agent-repl` at the checked-out
  source, replacing the build-time snapshot of the loader files.

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

- **Emacs is the headless `emacs-nox` build — no GUI, no xwidget.** Every
  `(require 'xwidget)` in `frontend.el` is inside a function body, so loading
  the module never reaches one. A test that actually *drives* the xwidget
  webview cannot run in this sandbox; that is a real limitation, not an
  oversight.

## `script(1)` is a guarantee, not an inference

The Emacs client layer allocates its own pty with `script -q -c CMD
/dev/null`, because Emacs needs a tty for a real frame and the run script
only passes `-t` when its own stdout is a terminal — which under `go test` it
is not. `script` lives in `bsdutils` on bookworm, a Debian `required`
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

- **`doom/init.el`** loads `$AGENT_REPL_E2E_SETTINGS` right after its `doom!`
  form — before any module's `config.el`, which is the last moment at which
  `agent-repl-frontend-auto-start` nil can still stop cold start from
  spawning a daemon against the host's defaults.
- **`doom/config.el`** defines the file-based readback helper
  (`agent-repl-e2e--eval`) and, on Doom's own after-init edge, enables
  `tab-bar-mode`, calls `server-start`, and writes the readiness stamp at
  `$AGENT_REPL_E2E_READY` **last** — so the stamp means "Doom is up AND
  emacsclient answers". A boot failure writes the same file with
  `"ok": false` and the elisp error.
- **Emacs is 28.2** (bookworm's `emacs-nox`), so `--init-directory` (Emacs
  29) does not exist. The layer aims Emacs at a per-test `~/.emacs.d` through
  `HOME` instead, staged from `/sandbox/emacs.d`: sources symlinked, the
  `.local` tree copied minus `straight/`. That staging exists because the
  container is `--read-only` and `/sandbox/emacs.d` is not a tmpfs, so Doom's
  local tree cannot be written in place.

`$S shell` and `$S run doom sync` are unaffected: none of the environment
variables above is set there.

## Preflight

`bin/preflight.sh` is what the harness calls to decide whether to run or to
skip. It exits `0` when ready, and otherwise prints an actionable message and
exits `10` (no runtime), `11` (runtime present, daemon unusable), `12` (image
not built) or `13` (insufficient disk).

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
`AGENT_REPL_SANDBOX_IMAGE`, `AGENT_REPL_SANDBOX_MIN_FREE_GB` (default 12).

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

## What is unverified

Explicitly, so nothing here reads as a tested claim:

- **The image has never been built.** No `docker build` or `podman build` has
  run. Apt package availability at `SNAPSHOT_STAMP`, the Node/Go tarball
  URLs, the Doom clone, `doom install`, `doom sync` under this minimal
  profile, and both cache primes are all *unexecuted*.
- **No test has run inside the sandbox.** Whether the e2e suite and the ERT
  suites actually pass under `emacs-nox` on Linux is unknown.
- **The offline claim is untested.** `npm ci --offline` and `GOPROXY=off`
  succeeding purely from the baked caches is a design intent that a build
  would confirm or refute. If `go mod download` (build list only) turns out
  to miss something a `go test` needs, the run will fail loudly under
  `--network none` — which is the correct failure, and the fix is to widen
  the prime.
- **`doom install --no-config --no-env --no-fonts --no-hooks` flag names**
  are from Doom's documented CLI, not from a run of this Doom SHA.
- **Nothing about the Emacs client layer's Doom boot has been observed.**
  That Doom boots from a staged `~/.emacs.d` under a scratch `HOME`; that the
  copy set the staging uses is exactly the set Doom writes at startup (a
  read-only path Doom insists on writing would fail the boot, loudly, with
  the pty output); that `doom-after-init-hook` exists in whichever `DOOM_REF`
  a build picks (the profile falls back to `emacs-startup-hook`); and that
  `apt-get install bsdutils util-linux` resolves at `SNAPSHOT_STAMP` — all
  unexecuted.
- **Base image digest, tarball checksums and the Doom SHA cannot be resolved
  without a registry or a download** — so they are resolved once from a real
  build and recorded in `pins.env`, which `build` reads. The gate is
  unchanged: anything pinned in neither `pins.env` nor the environment is
  still a refusal to build.

Verified on the authoring machine:

- `bin/*.sh` are `shellcheck -x` clean and `bash -n` clean.
- `preflight.sh` produces the exit-`11` message quoted above against the real
  host state.
- `stage_context` assembles the build context correctly from the live
  checkout: three loader files, 97 `lisp/*.el` sources, both npm lockfile
  pairs, and all 7 `go.mod` (+ `go.sum`) manifests at their own relative
  paths.
