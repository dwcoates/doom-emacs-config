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
ARG SNAPSHOT_STAMP=20250801T000000Z
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
RUN set -eux; \
    printf '%s\n' \
      "deb [check-valid-until=no] https://snapshot.debian.org/archive/debian/${SNAPSHOT_STAMP}/ bookworm main" \
      "deb [check-valid-until=no] https://snapshot.debian.org/archive/debian-security/${SNAPSHOT_STAMP}/ bookworm-security main" \
      > /etc/apt/sources.list; \
    rm -f /etc/apt/sources.list.d/debian.sources; \
    apt-get -o Acquire::Check-Valid-Until=false update; \
    apt-get install -y --no-install-recommends \
        ca-certificates \
        curl \
        emacs-nox \
        git \
        rsync \
        xz-utils; \
    rm -rf /var/lib/apt/lists/*

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
    mkdir -p /sandbox/home /repo-src /work; \
    chown -R sandbox:sandbox /sandbox /work
ENV HOME=/sandbox/home \
    XDG_CONFIG_HOME=/sandbox/home/.config \
    XDG_CACHE_HOME=/sandbox/home/.cache \
    XDG_DATA_HOME=/sandbox/home/.local/share \
    DOOMDIR=/sandbox/doom \
    EMACSDIR=/sandbox/emacs.d \
    GOPATH=/sandbox/home/go \
    GOMODCACHE=/sandbox/home/go/pkg/mod \
    GOCACHE=/sandbox/home/.cache/go-build \
    NPM_CONFIG_CACHE=/sandbox/home/.npm

USER sandbox
WORKDIR /work

# --- git identity, container-local -----------------------------------------
#
# The e2e suite never runs real git (see the repo's no-real-git rule), but
# Doom's own installer and `doom sync` both shell out to git, and git refuses
# to work without an identity. This one exists only inside the image.
RUN set -eux; \
    git config --global user.name  "agent-repl sandbox"; \
    git config --global user.email "sandbox@invalid"; \
    git config --global init.defaultBranch main; \
    git config --global advice.detachedHead false

# --- the MINIMAL doom profile ----------------------------------------------
#
# doom/init.el enables only what modules/app/agent-repl needs, NOT the
# user's personal config. See doom/init.el for the per-module justification.
COPY --chown=sandbox:sandbox doom/ /sandbox/doom/

# --- Doom itself, pinned to a SHA ------------------------------------------
RUN set -eux; \
    if [ -z "${DOOM_REF}" ]; then \
      echo "sandbox: DOOM_REF is empty; pass a Doom commit SHA (--build-arg DOOM_REF=<sha>)" >&2; \
      exit 1; \
    fi; \
    git clone --filter=blob:none https://github.com/doomemacs/doomemacs "${EMACSDIR}"; \
    git -C "${EMACSDIR}" checkout "${DOOM_REF}"
ENV PATH=/sandbox/emacs.d/bin:${PATH}

# --- the module's own packages.el, for the build-time `doom sync` ----------
#
# `doom sync` must resolve `:app agent-repl`, so the module's three
# loader-path files are COPIED in at build time. At RUN time the entrypoint
# replaces this directory with the real one from the mounted repo, so tests
# always exercise the checked-out source, never this snapshot.
COPY --chown=sandbox:sandbox module-loader-files/ /sandbox/doom/modules/app/agent-repl/

# --- doom sync at IMAGE BUILD time, so no test ever pays it ----------------
RUN set -eux; \
    doom install --no-config --no-env --no-fonts --no-hooks; \
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

COPY --chown=sandbox:sandbox bin/entrypoint.sh /sandbox/entrypoint.sh
ENTRYPOINT ["/sandbox/entrypoint.sh"]
CMD ["bash", "-lc", "echo 'agent-repl e2e sandbox: pass a command'"]
