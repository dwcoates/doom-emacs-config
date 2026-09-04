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
# needs no network: apt, the Emacs source build (Emacs is COMPILED here, from
# a pinned upstream commit, with xwidgets and native compilation — see the
# Emacs stage), the Node tarball, the Go tarball, Doom's own clone, `doom
# sync`, the npm cache for the shim and the webapp, and the Go module cache
# for all seven modules are all resolved at BUILD time. The run script
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
#   * Emacs — an exact upstream git SHA (EMACS_REF), built from source.
#   * Doom — an exact git SHA (DOOM_REF).
#
# NOT pinnable from this checkout, and therefore passed in by the run script
# (which refuses to build without them unless `--allow-unpinned` is given):
#   * BASE_IMAGE's digest — resolving a `debian:bookworm-*-slim` tag to a
#     digest
#     needs a registry, i.e. a working container runtime.
#   * NODE_SHA256 / GO_SHA256 — the upstream tarball checksums; likewise not
#     knowable without fetching them.
#   * EMACS_REF — the upstream Emacs commit the HOST's Emacs was built from.
#     It is a property of the developer's own build, not of this checkout.
#   * DOOM_REF — a real Doom commit SHA. Doom publishes no release tag this
#     image could name instead.
# When a checksum ARG is supplied it is VERIFIED; when it is empty the build
# says so loudly on stderr rather than silently trusting the download.
# ---------------------------------------------------------------------------

ARG BASE_IMAGE=debian:bookworm-20230904-slim

# ===========================================================================
# Stage 0: the apt snapshot, shared by every later stage
# ===========================================================================
FROM ${BASE_IMAGE} AS apt-base

# --- pinned identifiers ----------------------------------------------------
# SNAPSHOT_STAMP MUST BE AT OR AFTER THE BASE IMAGE'S BUILD DATE. The base
# image already has the point-release versions current on the day it was
# built; a snapshot older than that offers only OLDER versions of libc6,
# perl-base and friends, and apt then refuses the install outright
# ("libc6-dev : Depends: libc6 (= 2.36-9+deb12u1) but 2.36-9+deb12u4 is to
# be installed ... held broken packages"). Bump this stamp whenever
# SANDBOX_BASE_IMAGE's digest is bumped; `docker image inspect --format
# '{{.Created}}'` gives the date it must clear.
ARG SNAPSHOT_STAMP=20230910T000000Z

ENV DEBIAN_FRONTEND=noninteractive \
    LANG=C.UTF-8

# --- apt, from a fixed snapshot so the package set is reproducible ---------
#
# THE BASE IS A DATED BOOKWORM SNAPSHOT, AND WEBKITGTK IS THE ONLY REASON.
#
# Emacs 30.2's `configure` does not merely require WebKitGTK for xwidgets, it
# requires a WebKitGTK BELOW AN UPPER BOUND:
#
#     checking for webkit2gtk-4.1 >= 2.12 webkit2gtk-4.1 < 2.41.92... no
#     configure: error: xwidgets requested but WebKitGTK+ or WebKit framework
#     not found.
#
# That ceiling is upstream's deliberate refusal to build xwidgets against a
# WebKitGTK it does not support, and EVERY current Debian is above it —
# trixie ships 2.5x, and even bookworm's own security updates carried it from
# 2.40 to 2.42.1 in October 2023 and on to 2.50. Building on a current base
# therefore cannot produce an Emacs with xwidgets at all, and this image has
# no use for an Emacs without them (see the Emacs stage).
#
# SNAPSHOT_STAMP 20230910 is chosen as the NEWEST Debian package set whose
# WebKitGTK still satisfies the bound: `libwebkit2gtk-4.1-dev` is 2.40.5
# there. The base image is the matching dated `bookworm-*-slim` tag, so the
# base and the snapshot agree. Honest cost, stated rather than hidden: the
# whole package set is frozen at September 2023, so it carries no later
# security updates. That is a deliberate trade for a network-isolated test
# sandbox, and it is not something a newer stamp can fix without giving up
# xwidgets — the two constraints are directly opposed.
#
# Nothing in this image uses Debian's own `emacs` package; see the Emacs
# stage for why Emacs is built from source instead.
#
# APT TALKS TO THE SNAPSHOT OVER PLAIN http ON PURPOSE. `bookworm-slim`
# carries no `ca-certificates`, so an https:// sources.list here cannot be
# fetched at all ("The certificate is NOT trusted" / "no installation
# candidate") — and ca-certificates is itself one of the packages the stages
# below install, so it is a bootstrap cycle. Plain http breaks the cycle
# WITHOUT weakening anything that matters: apt verifies the snapshot's
# `InRelease` signature against `debian-archive-keyring`, which IS in the
# base image, and every package hash is bound to that signed index. TLS would
# add transport privacy, not integrity, and this index is public. Everything
# fetched LATER by curl or git (the Emacs clone, the Node and Go tarballs,
# Doom's clone) goes over https and does need certificates — which is why
# ca-certificates is installed before any of it runs.
RUN set -eux; \
    printf '%s\n' \
      "deb [check-valid-until=no] http://snapshot.debian.org/archive/debian/${SNAPSHOT_STAMP}/ bookworm main" \
      "deb [check-valid-until=no] http://snapshot.debian.org/archive/debian-security/${SNAPSHOT_STAMP}/ bookworm-security main" \
      > /etc/apt/sources.list; \
    rm -f /etc/apt/sources.list.d/debian.sources; \
    apt-get -o Acquire::Check-Valid-Until=false update; \
    apt-get install -y --no-install-recommends ca-certificates; \
    test -s /etc/ssl/certs/ca-certificates.crt; \
    rm -rf /var/lib/apt/lists/*

# ===========================================================================
# Stage 1: Emacs, BUILT FROM SOURCE at a pinned upstream commit
# ===========================================================================
#
# WHY FROM SOURCE, AND NOT DEBIAN'S PACKAGE. The webapp panel this module
# renders IS an `xwidget-webkit` webview. Debian ships no Emacs built with
# xwidgets — `emacs-nox` has no GUI at all and even `emacs-gtk` is built
# `--without-xwidgets` — so on any distro package the panel simply does not
# work, and a suite that drives it cannot exist. That alone settles it: the
# image builds the same Emacs the user runs.
#
# It also fixes the version floor that made a distro package unusable anyway:
# the pinned Doom (DOOM_REF) refuses to start an INTERACTIVE session below
# Emacs 29.1, and a `-nw` frame IS interactive, so bookworm's 28.2 aborted in
# `early-init.el` ("Detected Emacs 28.2, but interactive Doom needs >=29.1"),
# `doom!` never ran and no module `config.el` was ever loaded.
#
# EMACS_REF IS THE USER'S OWN BUILD: Emacs 30.2, "Development version
# 636f166cfc86 on HEAD branch". Pinning the commit — not a release tarball —
# is what makes the sandbox's Emacs the Emacs our users run, and it is gated
# like every other pin (an empty EMACS_REF fails this build loudly).
#
# The clone is the GitHub mirror rather than Savannah because the mirror
# serves partial clones: `--filter=blob:none` turns a ~700MB full history
# into a small fetch, and this build needs exactly one commit's trees.
FROM apt-base AS emacs-builder
ARG EMACS_REF=

# Build dependencies. Only this stage carries them: they are ~1.5GB of `-dev`
# packages and headers that the finished image has no use for, which is the
# whole reason Emacs is built in a separate stage and installed into the
# final image as a tree.
#
# `libwebkit2gtk-4.1-dev` + `libgtk-3-dev` are THE xwidgets dependency on
# linux, and the snapshot stamp above exists to keep the former under Emacs's
# `< 2.41.92` ceiling. `libgccjit-12-dev` is what `--with-native-compilation`
# links against (12 is this bookworm's gcc, and the libgccjit major MUST
# match the gcc Emacs then shells out to at native-compile time).
RUN set -eux; \
    apt-get -o Acquire::Check-Valid-Until=false update; \
    apt-get install -y --no-install-recommends \
        autoconf \
        automake \
        build-essential \
        git \
        libacl1-dev \
        libgccjit-12-dev \
        libgif-dev \
        libgnutls28-dev \
        libgtk-3-dev \
        libharfbuzz-dev \
        libjansson-dev \
        libjpeg-dev \
        libncurses-dev \
        libpng-dev \
        librsvg2-dev \
        libsqlite3-dev \
        libtiff-dev \
        libtree-sitter-dev \
        libwebkit2gtk-4.1-dev \
        libxml2-dev \
        libxpm-dev \
        pkg-config \
        texinfo; \
    rm -rf /var/lib/apt/lists/*

# CONFIGURE OPTIONS ARE THE HOST BUILD'S, ADAPTED TO LINUX. The host (darwin)
# configures with:
#
#   --with-native-compilation=aot --with-tree-sitter --with-modules
#   --with-gnutls --with-xml2 --with-ns --with-xwidgets
#   --disable-gc-mark-trace
#   CFLAGS='-O3 -march=native -DFD_SETSIZE=10000 -DDARWIN_UNLIMITED_SELECT'
#
# Every one of those is kept here except where it is meaningless on linux,
# and each departure is deliberate:
#
#   --with-ns  ->  --with-pgtk
#     `--with-ns` is the NeXTstep/Cocoa window system: darwin-only, and
#     `configure` on linux rejects it outright. On linux the window system
#     that CARRIES XWIDGETS is GTK 3 + WebKitGTK, in either of two flavors:
#     `--with-pgtk` (pure GTK, one drawing path for X11 and Wayland alike) or
#     `--with-x-toolkit=gtk3`. pgtk is the one used, and it is VERIFIED to
#     carry xwidgets in this version rather than assumed: `configure`'s own
#     summary is grepped below for its own verbatim line, "Does Emacs support
#     Xwidgets?   yes", and the finished binary is then asked for
#     `xwidget-internal`.
#     If either check fails the build STOPS — there is no quiet fallback to
#     an Emacs without the feature the webview needs.
#
#   -march=native  ->  DROPPED
#     A container image must not be compiled for whichever machine happened
#     to build it. `-march=native` bakes the build host's exact instruction
#     set into the binary, so the image would SIGILL on any older CPU (and
#     the whole point of the image is that it runs the same everywhere).
#
#   -DDARWIN_UNLIMITED_SELECT  ->  DROPPED
#     Pure darwin: it opts into the macOS `select()` that is not capped at
#     FD_SETSIZE. On linux the define names nothing at all.
#
#   -DFD_SETSIZE=10000  ->  KEPT
#     It still means something here: glibc's `select()` honors FD_SETSIZE at
#     compile time, and the daemon plus the shim plus the store leave this
#     Emacs with a lot of descriptors. Harmless where unused.
#
#   -O3  ->  KEPT. Generic, architecture-neutral optimization.
#
#   --with-native-compilation=aot, --with-tree-sitter, --with-modules,
#   --with-gnutls, --with-xml2, --with-xwidgets, --disable-gc-mark-trace
#     ->  ALL KEPT VERBATIM. None is platform-specific.
#
# `--with-native-compilation=aot` is why this stage is the slow one: it
# natively compiles the whole of `lisp/` at build time. That cost is paid
# HERE, once per image, and never by a test run.
#
# THE FEATURE PROBES RUN `src/emacs`, THE BUILD-TREE BINARY, not the DESTDIR
# copy. `make install DESTDIR=` stages the tree under /emacs-dist but installs
# NOTHING at the configured `--prefix`, so /emacs-dist's emacs cannot find
# /usr/local/share/emacs and dies with "cannot find the lisp directory" before
# it could answer any question about its own features. `src/emacs` runs in
# place against the source tree's lisp, and is the same binary.
RUN set -eux; \
    if [ -z "${EMACS_REF}" ]; then \
      echo "sandbox: EMACS_REF is empty; pass the upstream Emacs commit the host runs (--build-arg EMACS_REF=<sha>)" >&2; \
      exit 1; \
    fi; \
    git clone --filter=blob:none --no-checkout https://github.com/emacs-mirror/emacs /emacs-src; \
    git -C /emacs-src checkout "${EMACS_REF}"; \
    cd /emacs-src; \
    ./autogen.sh; \
    ./configure \
        --prefix=/usr/local \
        --with-native-compilation=aot \
        --with-tree-sitter \
        --with-modules \
        --with-gnutls \
        --with-xml2 \
        --with-pgtk \
        --with-xwidgets \
        --disable-gc-mark-trace \
        CFLAGS='-O3 -DFD_SETSIZE=10000' \
      | tee /tmp/configure.log; \
    grep -Ei 'xwidgets\?[[:space:]]*yes' /tmp/configure.log \
      || { echo "sandbox: configure did NOT enable xwidgets; refusing to build a degraded Emacs" >&2; \
           grep -Ei 'xwidget|webkit' /tmp/configure.log >&2 || true; exit 1; }; \
    grep -Ei 'native lisp compiler\?[[:space:]]*yes' /tmp/configure.log \
      || { echo "sandbox: configure did NOT enable native compilation" >&2; \
           grep -Ei 'native|libgccjit' /tmp/configure.log >&2 || true; exit 1; }; \
    make -j"$(nproc)"; \
    make install DESTDIR=/emacs-dist; \
    src/emacs --version | head -1; \
    src/emacs -Q --batch \
      --eval '(unless (featurep (quote xwidget-internal)) (error "built WITHOUT xwidgets"))' \
      --eval '(unless (native-comp-available-p) (error "built WITHOUT usable native compilation"))'; \
    cd /

# THE FINAL IMAGE'S SHARED-LIBRARY PACKAGES ARE DERIVED FROM THE BINARY, NOT
# HAND-LISTED. A hand-written list of `libfoo`-to-`libfoo0` runtime names is
# wrong the first time (`libtree-sitter0` does not exist; `libpng16-16t64`
# exists only on some suites) and silently rots on every base bump. Asking `ldd` which
# objects this Emacs actually loads and `dpkg -S` which package owns each one
# produces exactly the right set, and it stays right by construction.
#
# `readlink -f` first: `ldd` reports the SONAME symlink, which frequently
# belongs to the `-dev` package, while the real `.so.N.N` behind it belongs to
# the runtime package this image wants.
#
# libgccjit is added by hand on purpose — it is `dlopen`ed at native-compile
# time, so it never appears in `ldd` output, and without it
# `native-comp-available-p` is nil in the finished image.
RUN set -eux; \
    ldd /emacs-dist/usr/local/bin/emacs \
      | awk '{ for (i = 1; i <= NF; i++) if (substr($i, 1, 1) == "/") { print $i; break } }' \
      | xargs -r readlink -f \
      | sort -u \
      | xargs -r dpkg -S \
      | cut -d: -f1 \
      | tr ',' '\n' \
      | sed 's/[[:space:]]//g' \
      | sort -u > /emacs-runtime-pkgs.txt; \
    echo libgccjit0 >> /emacs-runtime-pkgs.txt; \
    sort -u -o /emacs-runtime-pkgs.txt /emacs-runtime-pkgs.txt; \
    test -s /emacs-runtime-pkgs.txt; \
    cat /emacs-runtime-pkgs.txt; \
    rm -rf /emacs-src

# ===========================================================================
# Stage 2: the sandbox image itself
# ===========================================================================
FROM apt-base

ARG NODE_VERSION=22.14.0
ARG NODE_SHA256=
ARG GO_VERSION=1.24.6
ARG GO_SHA256=
ARG DOOM_REF=
ARG TARGETARCH

# --- runtime packages ------------------------------------------------------
#
# The tools the suite shells out to, plus the RUNTIME halves of the Emacs
# build's shared-library dependencies — and those are read from the list the
# builder stage derived from the binary itself (see there for why they are
# not hand-listed). No `-dev` package appears here: the headers were only
# ever needed by the builder stage.
#
# `gcc`, `binutils` and `libgccjit0` ARE runtime dependencies, not build
# leftovers: a native-compiled Emacs shells out to the assembler and linker
# every time it compiles a `.el` that is not already in its eln cache, which
# `doom sync` does constantly. Without them native compilation fails at RUN
# time even though the binary is fine.
#
# `xvfb` and `xauth` exist so a scenario can give Emacs a REAL DISPLAY. An
# xwidget-webkit webview cannot be created on a tty frame — it needs a
# graphical frame, which needs a display — and `--network none` rules out any
# remote one. `Xvfb :99 -screen 0 1280x1024x24` plus `DISPLAY=:99` (and
# `GDK_BACKEND=x11`, because this Emacs is pgtk and would otherwise prefer
# Wayland) is what a GUI scenario runs. See README.md; the tty-frame boot the
# Emacs layer uses today is unchanged and needs none of this.
#
# `bsdutils` and `util-linux` are named EXPLICITLY for `script(1)`, which is
# how the Emacs client layer allocates the pty its Emacs needs for a real tty
# frame (`e2e/emacs_sandbox_test.go`, StartPTY). Both are Debian
# `required`/`important` packages, so they are already present in
# `bookworm-slim` — naming them makes `script` a GUARANTEE of this image rather
# than an inference from the base image's package set, and pins them to
# SNAPSHOT_STAMP like everything else here. `script` lives in `bsdutils` on
# bookworm; `util-linux` is listed alongside it so a future reshuffle of the
# binary between the two packages cannot silently remove it. The harness
# keeps a runtime `exec.LookPath("script")` check as a backstop for images
# built before this line existed.
COPY --from=emacs-builder /emacs-runtime-pkgs.txt /tmp/emacs-runtime-pkgs.txt
RUN set -eux; \
    apt-get -o Acquire::Check-Valid-Until=false update; \
    apt-get install -y --no-install-recommends \
        binutils \
        bsdutils \
        curl \
        gcc \
        git \
        rsync \
        util-linux \
        xauth \
        xvfb \
        xz-utils \
        $(tr '\n' ' ' < /tmp/emacs-runtime-pkgs.txt); \
    rm -rf /var/lib/apt/lists/* /tmp/emacs-runtime-pkgs.txt; \
    command -v script; \
    command -v curl; \
    command -v git; \
    command -v rsync; \
    command -v gcc; \
    command -v Xvfb

# --- the Emacs built in stage 1 -------------------------------------------
COPY --from=emacs-builder /emacs-dist/ /
# EVERY assertion the user asked for, made at BUILD time so a degraded image
# cannot exist: the version is the host's 30.2, xwidgets is compiled in, and
# native compilation actually WORKS here (which is a stronger claim than
# "was configured" — it needs libgccjit and gcc, installed above). The `ldd`
# check catches a shared library the runtime list above forgot, which would
# otherwise surface as a mystifying failure in the first Emacs test.
RUN set -eux; \
    ldd /usr/local/bin/emacs > /tmp/ldd.txt; \
    if grep -q 'not found' /tmp/ldd.txt; then \
      echo "sandbox: emacs is missing shared libraries:" >&2; \
      grep 'not found' /tmp/ldd.txt >&2; exit 1; \
    fi; \
    rm -f /tmp/ldd.txt; \
    emacs --version | head -1; \
    emacs_version=$(emacs -Q --batch --eval '(princ emacs-version)'); \
    case "$emacs_version" in \
      30.2*) ;; \
      *) echo "sandbox: expected Emacs 30.2 (the host build), got '$emacs_version'" >&2; exit 1 ;; \
    esac; \
    emacs -Q --batch \
      --eval '(unless (featurep (quote xwidget-internal)) (error "emacs has NO xwidgets support"))' \
      --eval '(unless (native-comp-available-p) (error "native compilation is NOT available"))' \
      --eval '(princ "emacs: xwidgets + native-comp OK\n")'

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
# DOOM_REF IS THE HOST'S OWN DOOM, for the same reason EMACS_REF is the
# host's own Emacs: the sandbox is only worth trusting if it runs what the
# user runs. It is the release tag v2.1.0, not master — see pins.env, and see
# the module-library assertion below for what master silently costs.
#
# THE CHECKOUT MUST CARRY A REAL MODULE LIBRARY, and that is ASSERTED rather
# than assumed. Upstream master has split the module trees out of this
# repository into a separate `sources/doom+` collection, so a plain clone of
# master has `modules/doom` and nothing else. Nothing fails loudly when that
# happens: `doom sync` SUCCEEDS, the `doom!` block in doom/init.el resolves
# to no modules at all, and the breakage only surfaces much later as
# `set-popup-rule!` being unbound in a booted Emacs. Checking for one module
# every profile here needs turns that into a build failure at the moment the
# ref is wrong.
#
# `sources/doom+/modules` is created only when this Doom actually names it:
# revisions that do put it on `doom-module-load-path` print "Setting current
# directory: No such file or directory, .../sources/doom+/modules" on every
# `doom` invocation, onto output the suite reads, and creating the empty
# directory makes the path Doom already expects resolve. Revisions that do
# not name it (v2.1.0 among them) get no stray directory.
RUN set -eux; \
    if [ -z "${DOOM_REF}" ]; then \
      echo "sandbox: DOOM_REF is empty; pass a Doom commit SHA (--build-arg DOOM_REF=<sha>)" >&2; \
      exit 1; \
    fi; \
    git clone --filter=blob:none https://github.com/doomemacs/doomemacs "${EMACSDIR}"; \
    git -C "${EMACSDIR}" checkout "${DOOM_REF}"; \
    for m in ui/popup editor/evil emacs/vc tools/magit lang/emacs-lisp; do \
      test -d "${EMACSDIR}/modules/$m" \
        || { echo "sandbox: DOOM_REF ${DOOM_REF} has no modules/$m; this Doom keeps its module library elsewhere (upstream master moved it to sources/doom+), so doom/init.el would resolve NO modules and set-popup-rule! would be unbound at run time" >&2; exit 1; }; \
    done; \
    if grep -q 'sources/doom+/modules' "${EMACSDIR}/lisp/doom.el"; then \
      mkdir -p "${EMACSDIR}/sources/doom+/modules"; \
    fi
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
