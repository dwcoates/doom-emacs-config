# syntax=docker/dockerfile:1.7
#
# The agent-repl cross-system e2e sandbox image.
#
# WHY THIS EXISTS: the e2e suite runs a real claude-repld, a real
# shim-store, a real shim-claude-sidecar, the real TypeScript shim, and (for
# the Emacs-side integration suites) a real Emacs + Doom. On the host that
# means writing to ~/.claude, ~/.emacs.d, the Doom cache, the user's git
# config and — for the service plane — launchd. This image carries its own
# HOME, its own Doom install and its own toolchains so a run cannot reach
# any of that. See README.md.
#
# EVERYTHING NEEDED AT TEST TIME IS BAKED HERE. A `docker run` of this image
# needs no network: apt, the Node tarball, the Go tarball, Doom's own clone,
# `doom sync`, the npm cache for the shim and the webapp, and the Go module
# cache for all seven modules are all resolved at BUILD time. The run script
# passes `--network none` for exactly that reason.
#
# ---------------------------------------------------------------------------
# Version pinning
# ---------------------------------------------------------------------------
# Pinned here, by construction:
#   * every apt package version — via a snapshot.debian.org archive fixed to
#     SNAPSHOT_STAMP, which is the only mechanism that pins a Debian suite's
#     package set without naming each `pkg=version` by hand.
#   * the Node and Go toolchains — exact upstream release tarballs.
#   * Doom — an exact git SHA (DOOM_REF).
#
# NOT pinnable from this checkout, and therefore passed in by the run script
# (which refuses to build without them unless `--allow-unpinned` is given):
#   * BASE_IMAGE's digest — resolving `debian:bookworm-slim` to a digest
#     needs a registry, i.e. a working container runtime.
#   * NODE_SHA256 / GO_SHA256 — the upstream tarball checksums; likewise not
#     knowable without fetching them.
#   * DOOM_REF — a real Doom commit SHA. Doom publishes no release tag this
#     image could name instead.
# When a checksum ARG is supplied it is VERIFIED; when it is empty the build
# says so loudly on stderr rather than silently trusting the download.
# ---------------------------------------------------------------------------

ARG BASE_IMAGE=debian:bookworm-slim
FROM ${BASE_IMAGE}

# --- pinned identifiers ----------------------------------------------------
# SNAPSHOT_STAMP MUST BE AT OR AFTER THE BASE IMAGE'S BUILD DATE. The base
# image already has the point-release versions current on the day it was
# built; a snapshot older than that offers only OLDER versions of libc6,
# perl-base and friends, and apt then refuses the install outright
# ("libc6-dev : Depends: libc6 (= 2.36-9+deb12u10) but 2.36-9+deb12u14 is to
# be installed ... held broken packages"). Bump this stamp whenever
# SANDBOX_BASE_IMAGE's digest is bumped; `docker image inspect --format
# '{{.Created}}'` gives the date it must clear.
ARG SNAPSHOT_STAMP=20260901T000000Z
ARG NODE_VERSION=22.14.0
ARG NODE_SHA256=
ARG GO_VERSION=1.24.6
ARG GO_SHA256=
ARG DOOM_REF=
ARG TARGETARCH

ENV DEBIAN_FRONTEND=noninteractive \
    LANG=C.UTF-8

# --- apt, from a fixed snapshot so the package set is reproducible ---------
#
# `emacs-nox` is the headless build on purpose: no GUI, no xwidget. Nothing
# the module loads at LOAD time needs either — `frontend.el` only ever calls
# `(require 'xwidget)` from inside a function body, so a batch/headless load
# of the module never reaches it.
#
# `bsdutils` and `util-linux` are named EXPLICITLY for `script(1)`, which is
# how the Emacs client layer allocates the pty its Emacs needs for a real tty
# frame (`e2e/emacs_sandbox_test.go`, StartPTY). Both are Debian
# `required`/`important` packages, so they are already present in
# `bookworm-slim` — naming them makes `script` a GUARANTEE of this image
# rather than an inference from the base image's package set, and pins them
# to SNAPSHOT_STAMP like everything else here. `script` lives in `bsdutils`
# on bookworm; `util-linux` is listed alongside it so a future reshuffle of
# the binary between the two packages cannot silently remove it. The harness
# keeps a runtime `exec.LookPath("script")` check as a backstop for images
# built before this line existed.
# APT TALKS TO THE SNAPSHOT OVER PLAIN http ON PURPOSE. `bookworm-slim`
# carries no `ca-certificates`, so an https:// sources.list here cannot be
# fetched at all ("The certificate is NOT trusted" / "no installation
# candidate") — and ca-certificates is itself one of the packages this very
# step installs, so it is a bootstrap cycle. Plain http breaks the cycle
# WITHOUT weakening anything that matters: apt verifies the snapshot's
# `InRelease` signature against `debian-archive-keyring`, which IS in the
# base image, and every package hash is bound to that signed index. TLS would
# add transport privacy, not integrity, and this index is public. Everything
# fetched LATER by curl (the Node and Go tarballs, Doom's clone) goes over
# https and does need certificates — which is why ca-certificates is
# installed here, before any of it runs.
RUN set -eux; \
    printf '%s\n' \
      "deb [check-valid-until=no] http://snapshot.debian.org/archive/debian/${SNAPSHOT_STAMP}/ bookworm main" \
      "deb [check-valid-until=no] http://snapshot.debian.org/archive/debian-security/${SNAPSHOT_STAMP}/ bookworm-security main" \
      > /etc/apt/sources.list; \
    rm -f /etc/apt/sources.list.d/debian.sources; \
    apt-get -o Acquire::Check-Valid-Until=false update; \
    apt-get install -y --no-install-recommends \
        bsdutils \
        ca-certificates \
        curl \
        emacs-nox \
        git \
        rsync \
        util-linux \
        xz-utils; \
    rm -rf /var/lib/apt/lists/*; \
    test -s /etc/ssl/certs/ca-certificates.crt; \
    command -v script; \
    command -v curl; \
    command -v git; \
    command -v rsync; \
    command -v emacs

# --- Node (Debian bookworm ships 18; the shim and webapp need >= 20) ------
RUN set -eux; \
    case "${TARGETARCH:-amd64}" in \
      amd64) node_arch=x64 ;; \
      arm64) node_arch=arm64 ;; \
      *) echo "sandbox: unsupported TARGETARCH=${TARGETARCH}" >&2; exit 1 ;; \
    esac; \
    tarball="node-v${NODE_VERSION}-linux-${node_arch}.tar.xz"; \
    curl -fsSLo /tmp/node.tar.xz "https://nodejs.org/dist/v${NODE_VERSION}/${tarball}"; \
    if [ -n "${NODE_SHA256}" ]; then \
      echo "${NODE_SHA256}  /tmp/node.tar.xz" | sha256sum -c -; \
    else \
      echo "sandbox: WARNING: NODE_SHA256 empty — node tarball NOT verified" >&2; \
    fi; \
    mkdir -p /opt/node; \
    tar -xJf /tmp/node.tar.xz -C /opt/node --strip-components=1; \
    rm -f /tmp/node.tar.xz
ENV PATH=/opt/node/bin:${PATH}

# --- Go (the daemon/e2e modules declare go 1.23.0; fakedaemon pins
#     toolchain go1.24.6, so the image carries the higher of the two) -------
RUN set -eux; \
    case "${TARGETARCH:-amd64}" in \
      amd64) go_arch=amd64 ;; \
      arm64) go_arch=arm64 ;; \
      *) echo "sandbox: unsupported TARGETARCH=${TARGETARCH}" >&2; exit 1 ;; \
    esac; \
    curl -fsSLo /tmp/go.tar.gz "https://go.dev/dl/go${GO_VERSION}.linux-${go_arch}.tar.gz"; \
    if [ -n "${GO_SHA256}" ]; then \
      echo "${GO_SHA256}  /tmp/go.tar.gz" | sha256sum -c -; \
    else \
      echo "sandbox: WARNING: GO_SHA256 empty — go tarball NOT verified" >&2; \
    fi; \
    tar -xzf /tmp/go.tar.gz -C /usr/local; \
    rm -f /tmp/go.tar.gz
ENV PATH=/usr/local/go/bin:${PATH} \
    GOTOOLCHAIN=local

# --- the sandbox user and its container-local HOME -------------------------
#
# Non-root, so nothing in the container can write to a bind mount owned by
# root either. HOME lives under /sandbox, which is created here and never
# bound to anything on the host.
RUN set -eux; \
    useradd --create-home --home-dir /sandbox/home --shell /bin/bash --uid 1000 sandbox; \
    mkdir -p /sandbox/home /sandbox/cache /repo-src /work; \
    chown -R sandbox:sandbox /sandbox /work
# THE PRIMED CACHES LIVE OUTSIDE HOME, at /sandbox/cache. HOME is a tmpfs at
# run time (that is what keeps a run off the host's ~), and a tmpfs mounted
# at /sandbox/home COMPLETELY HIDES whatever the image baked underneath it —
# so an npm or Go cache primed into ~/.npm or ~/go would be gone in every
# container that used it, and `npm ci --offline` / `go build` with GOPROXY=off
# would fail with no network to fall back on. Keeping the caches at
# /sandbox/cache leaves them visible from the image layer, which is read-only
# under `--read-only`: the Go module cache is complete and Go reads a
# complete cache read-only, while the entrypoint copies the (much smaller)
# npm cache into the writable HOME because npm writes to its cache even on
# an offline install.
#
# GOCACHE (the build cache) is the one that MUST be writable, so it stays
# under HOME on the tmpfs.
ENV HOME=/sandbox/home \
    XDG_CONFIG_HOME=/sandbox/home/.config \
    XDG_CACHE_HOME=/sandbox/home/.cache \
    XDG_DATA_HOME=/sandbox/home/.local/share \
    DOOMDIR=/sandbox/doom \
    EMACSDIR=/sandbox/emacs.d \
    SANDBOX_CACHE=/sandbox/cache \
    GOPATH=/sandbox/cache/go \
    GOMODCACHE=/sandbox/cache/go/pkg/mod \
    GOCACHE=/sandbox/home/.cache/go-build \
    NPM_CONFIG_CACHE=/sandbox/cache/npm \
    GIT_CONFIG_GLOBAL=/sandbox/gitconfig

USER sandbox
WORKDIR /work

# --- git identity, container-local -----------------------------------------
#
# The e2e suite never runs real git (see the repo's no-real-git rule), but
# Doom's own installer and `doom sync` both shell out to git, and git refuses
# to work without an identity. This one exists only inside the image.
# `--global` writes to GIT_CONFIG_GLOBAL (/sandbox/gitconfig), NOT to
# ~/.gitconfig: HOME is a fresh tmpfs in every container, so an identity
# written into ~ at build time would not exist at run time and git would
# refuse to work for Doom's own git calls.
RUN set -eux; \
    git config --global user.name  "agent-repl sandbox"; \
    git config --global user.email "sandbox@invalid"; \
    git config --global init.defaultBranch main; \
    git config --global advice.detachedHead false; \
    test -s /sandbox/gitconfig

# --- the MINIMAL doom profile ----------------------------------------------
#
# doom/init.el enables only what modules/app/agent-repl needs, NOT the
# user's personal config. See doom/init.el for the per-module justification.
COPY --chown=sandbox:sandbox doom/ /sandbox/doom/

# --- Doom itself, pinned to a SHA ------------------------------------------
#
# `sources/doom+/modules` is created because doom.el:189 puts it on
# `doom-module-load-path` unconditionally. A fresh clone does not contain it,
# and every `doom` invocation then printed "Setting current directory: No
# such file or directory, .../sources/doom+/modules" onto the output the
# suite reads. Creating the (empty) directory makes the path Doom already
# expects actually resolve.
RUN set -eux; \
    if [ -z "${DOOM_REF}" ]; then \
      echo "sandbox: DOOM_REF is empty; pass a Doom commit SHA (--build-arg DOOM_REF=<sha>)" >&2; \
      exit 1; \
    fi; \
    git clone --filter=blob:none https://github.com/doomemacs/doomemacs "${EMACSDIR}"; \
    git -C "${EMACSDIR}" checkout "${DOOM_REF}"; \
    mkdir -p "${EMACSDIR}/sources/doom+/modules"
ENV PATH=/sandbox/emacs.d/bin:${PATH}

# --- the module's own packages.el, for the build-time `doom sync` ----------
#
# `doom sync` must resolve `:app agent-repl`, so the module's three
# loader-path files are COPIED in at build time. At RUN time the entrypoint
# replaces this directory with the real one from the mounted repo, so tests
# always exercise the checked-out source, never this snapshot.
COPY --chown=sandbox:sandbox module-loader-files/ /sandbox/doom/modules/app/agent-repl/

# --- doom sync at IMAGE BUILD time, so no test ever pays it ----------------
#
# FLAGS: this Doom's `install` accepts only `--aot` and the `--[no-]config`,
# `--[no-]env`, `--[no-]install` pairs — `--no-fonts`/`--no-hooks` do not
# exist and make it exit 2. `--no-config` keeps it from generating a config
# over the profile COPYied in above, `--no-env` skips the envvar dump (the
# image sets its own environment), and `--no-install` leaves package
# installation to the explicit `doom sync` that follows, which is the step
# whose result the image is actually baking.
RUN set -eux; \
    doom install --no-config --no-env --no-install; \
    doom sync

# --- prime the npm caches so `npm ci` runs OFFLINE at test time ------------
#
# Only the lockfiles are copied: `npm ci` in a throwaway directory populates
# ~/.npm with every tarball the lockfile names, and the throwaway tree is
# then deleted. At run time the entrypoint's `npm ci --offline` in the
# writable working copy resolves entirely from this cache.
COPY --chown=sandbox:sandbox npm-lockfiles/shim/   /sandbox/npm-prime/shim/
COPY --chown=sandbox:sandbox npm-lockfiles/webapp/ /sandbox/npm-prime/webapp/
RUN set -eux; \
    for d in /sandbox/npm-prime/shim /sandbox/npm-prime/webapp; do \
      (cd "$d" && npm ci --ignore-scripts); \
    done; \
    rm -rf /sandbox/npm-prime

# --- prime the Go module cache so `go build`/`go test` run OFFLINE --------
#
# `go mod download` (no `all`) downloads every module in each main module's
# build list, which is what the cache needs, and — unlike `go mod download
# all` — does not try to LOAD packages, which would fail here: the local
# `replace` targets are staged as bare go.mod/go.sum manifests with no source.
COPY --chown=sandbox:sandbox go-manifests/ /sandbox/go-prime/
RUN set -eux; \
    find /sandbox/go-prime -name go.mod -printf '%h\n' | sort -u | while read -r d; do \
      (cd "$d" && go mod download) || { echo "sandbox: go mod download failed in $d" >&2; exit 1; }; \
    done; \
    rm -rf /sandbox/go-prime

# Offline from here on: every dependency above is already in the image, and
# the run script also drops the network entirely.
ENV GOFLAGS=-mod=mod \
    GOPROXY=off \
    npm_config_offline=true

# --- keep the toolchains on PATH in LOGIN shells too ----------------------
#
# Debian's /etc/profile overwrites PATH unconditionally, so a login shell
# (`e2e-sandbox.sh shell`, the image's own default CMD, anything run through
# `bash -l`) would drop /opt/node/bin, /usr/local/go/bin and Doom's bin and
# report node/go/doom as missing even though they are installed. Re-adding
# them from a profile.d drop-in makes the login and non-login environments
# agree instead of leaving it to every caller to remember `bash -c`.
#
# The same step deletes Debian's default ~/.bash_logout, which runs
# `clear_console -q` on exit. With no tty that command FAILS, and because it
# is the last thing a login shell runs it becomes the shell's — and so the
# CONTAINER's — exit status: every `bash -lc <anything>` returned 1 no matter
# what the command did, which silently turns a passing test run into a
# failure (and would mask a real one). Removing the file makes a login
# shell's status the status of the command it was given.
USER root
RUN set -eux; \
    printf '%s\n' \
      'PATH=/sandbox/emacs.d/bin:/usr/local/go/bin:/opt/node/bin:$PATH' \
      'export PATH' \
      > /etc/profile.d/10-agent-repl-sandbox.sh; \
    chmod 0644 /etc/profile.d/10-agent-repl-sandbox.sh; \
    rm -f /etc/skel/.bash_logout /sandbox/home/.bash_logout /root/.bash_logout
USER sandbox

COPY --chown=sandbox:sandbox bin/entrypoint.sh /sandbox/entrypoint.sh
ENTRYPOINT ["/sandbox/entrypoint.sh"]
CMD ["bash", "-lc", "echo 'agent-repl e2e sandbox: pass a command'"]
