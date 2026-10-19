# Runtime metrics: judgement calls

Calls made while building `docs/PLAN-runtime-metrics.md` without the owner,
per the owner's instruction (2026-10-18) to log ambiguities and keep going.
Each entry says what was decided, why, and what it departs from.

## 1. No fixed ports: an ephemeral port advertised through a target file

- Plan said: fixed loopback ports, defined once.
- Decided: each runtime listens on `127.0.0.1:0` and atomically writes
  `<metrics root>/targets/<program>.json` (VictoriaMetrics `file_sd`).
- Why:
  - A daemon handover runs the incumbent and its successor at once; a fixed
    port would make the successor contend for it. The successor now
    overwrites the target file when it takes over, as it publishes
    `daemon.addr`, so there is no contention to order.
  - A test runtime can never take the live runtime's port.
- `instance` is the program name, not `host:port`, so a restart continues
  the same series.

## 2. The metrics root is opt-in by flag

- `--metrics-root` (empty is off), passed by the production launchers
  (`lisp/daemon.el`, the store and sidecar plists). Tests run without a
  listener unless they ask for one, so no test writes into the live
  targets directory.

## 3. The version is not stamped into binaries

- Plan said: stamp the version in with `-ldflags -X`.
- Conflict: `bin/build-frontend.sh` builds with `-buildvcs=false` because
  the deploy decides staleness by each binary's CONTENT HASH. Stamping a
  version would change every binary's hash on every landing and bounce every
  service for nothing.
- Decided:
  - `agentrepl_build_info{program, build}` carries the content hash only.
  - The daemon resolves its checkout's version itself and exposes
    `agentrepl_deployed_version_info{version}`:
    - the greatest version tag on HEAD (`0.129.0`), or, when HEAD carries
      none, the greatest reachable one with HEAD as semver build metadata
      (`0.129.0+0123456789ab`);
    - resolved by the `deployed_version` background loop once at start and
      again whenever a deploy gets all the way through (`deploy.Deps.Deployed`
      kicks it), so git never runs on the boot path, the deploy path or a
      scrape;
    - withdrawn, with one ERROR record per cause, when it cannot be
      resolved, because a stale version would misattribute everything after
      it.
  - A version is a property of the deployed source tree, which is what a
    regression hunt groups by; the content hashes say which programs
    actually changed between two versions.
- A first cut had `bin/build-frontend.sh` write a version file; it was
  dropped because the daemon's own deploy builds in staging and installs,
  so the daemon is the one place that knows when a version is deployed.

## 4. The shared package lives in the shared logging module

- Plan said: a new module beside `agent-shim/logging/go`.
- Decided: `agentrepl/logging/metrics`, a package inside that module.
- Why: the deploy's staleness pathspecs (`bin/lib-deploy-stamp.sh`) and the
  build's pre-flight (`bin/build-frontend.sh` `require_go_source_dirs`)
  already cover `agent-shim/logging/` for every Go runtime, and the module
  already hosts the other runtime-wide packages (`profiling`,
  `buildreport`). A new module would need every one of those tables
  extended, and a table someone forgets reads as "fully deployed".
- Consequence: the module's `go` directive moved to 1.24.0 (client_golang's
  floor), and `shim-lock` followed.

## 5. VictoriaMetrics: Homebrew, launchd, port 8428, no minimum version

- Installed by its own `program` entry (`brew install victoriametrics`) and
  run by a `launchd_service` whose program is that installed command
  (`bin/requirements.sh` learned the `command` and `files` shapes for it).
- It listens on `127.0.0.1:8428`, the one fixed port in the design: no test
  ever starts it, so nothing can collide with it, and the skill needs one
  address to query.
- The entry carries no minimum version. The installer reads the first number
  in a program's version output, and `victoria-metrics -version` begins with
  its build date (`victoria-metrics-20260925-134715-tags-v1.153.0`), so any
  minimum would always pass. A check that cannot fail is worse than none.
- Its log goes to `~/.cache/agent-repl/log/victoriametrics.log` through
  launchd's standard out and error. launchd does not rotate it; VictoriaMetrics
  logs little at its default level, but the file is unbounded.
- Verified against the real v1.153.0 binary (outside the repo, in a temporary
  directory): the `%{AGENT_REPL_METRICS_ROOT}` placeholder resolves, the file
  targets are discovered, and a scraped series is queryable. That run found
  the target files' `program` label colliding with the build info's own
  (`exported_program`), so targets now carry `instance` alone.
- Not done on the owner's machine: installing VictoriaMetrics and loading the
  service. Both are outside the repository; `bin/requirements.sh install` (or
  `/install-agent-repl`) does them.

## 6. Vendor bytes are not counted yet: the owner's call

- The plan (step 7) and the owner's ruling put network counting in the shim.
- Verified: the vendor HTTP is made by the agent SDK's bundled `claude`
  child process (`agent-shim/claude/shim/src/sdk/real-query.ts`,
  `spawnClaudeCodeProcess`), not by the shim's Node process, so the shim has
  no bytes of its own to count.
- The two ways to count them:
  - a CONNECT proxy inside the shim that the child is pointed at through
    `HTTPS_PROXY`, counting tunnel bytes in both directions without
    decrypting. Accurate, userspace only, but every session's vendor traffic
    would then depend on it: a defect in it takes every session down;
  - per-process kernel network statistics, ruled out by the mbuf incident
    (`docs/protobuf-design/drop-vendor-traffic.md`).
- Decided for now: the contract's `vendor_traffic` is optional, no shim sets
  it, and the daemon publishes no traffic series rather than a zero. The
  proxy is a change to the vendor path of every session, so it waits for the
  owner.

## 7. The profile tool keeps its own 2-second sampler

- Plan said: the process collector replaces the `ps` sampler in
  `bin/profile-agent-repl.sh`.
- Decided: both stay, each for its own question.
  - The profile session samples every 2 seconds for its 30-minute window
    and ranks individual processes, every WebKit process on the host
    included (its report tells the reader to close Safari for a clean
    reading). The metrics sample every 15 seconds, forever, and fold
    processes into agent-repl's groups (a shim and its tools by workspace,
    only Emacs's WebKit).
  - Reading the daemon's `/metrics` every 2 seconds instead would make the
    profile depend on the metrics surface being on, and a profile taken on a
    daemon started without a metrics root would have no process table, with
    no honest substitute.
- What was consolidated instead: the profile report's latency budgets now
  come from `proto/vocab/latencies.json`, and `webfootprint` reads the
  kernel through the collector's reader.

## 8. Emacs reports no latencies yet

- The contract lets Emacs call `ReportLatencies`, and the plan named Emacs
  as a reporter.
- Every latency whose end Emacs sees is today measured elsewhere: a switch
  is measured by the page from the instant Emacs hands it
  (`agent-repl-webhost-note-switch`), and Emacs's CPU and memory come from
  the daemon's process collector. So no Emacs encoder for the rpc was
  written; the first Emacs-measured latency adds one to `lisp/rpc.el`.

## 9. Emacs's own restart is not timed

- Asked (2026-10-19): metrics for restarts, the whole process and each
  workspace.
- Every restart the daemon runs or sees end is timed: deploys and their
  phases, the daemon's boot, store and sidecar restarts, each workspace's
  session start, shim replacements and handover transfers. A workspace an
  editor's opening starts is timed as `workspace_start{cause="editor_open"}`,
  which covers the workspaces an Emacs relaunch brings back.
- Emacs's own launch and a deploy's `reload_elisp` end inside Emacs, which
  reports nothing yet (call 8). Timing them needs Emacs's encoder for
  `ReportLatencies` and new kinds, a contract change left for when a restart
  investigation needs them. A deploy's webview reload is already timed: the
  reloaded page reports `page_boot`.

## 10. A moved deploy is timed by its successor

- A deploy that hands the daemon over cannot see its own end: the outgoing
  daemon's footer says nothing after the transfer, and the successor says
  `updated`. So the successor times the deploy's `handover` and `total`.
- The deploy's start instant reaches it through the one thing the two
  daemons share, the intent manifest (`deploy_started_at` beside
  `written_at`), carried there by the deploy's ask
  (`rollout.Ask.DeployStartedAt`). Both instants are this host's wall clock,
  so the durations are directly comparable; the daemon never pairs a start
  from one process with an end from another by guessing.
- A handover or restart asked without the instant is refused
  (`rollout.ErrNoDeployStart`): only a deploy asks for either.

## 11. Restart histograms carry no workspace label

- Per-workspace restarts (`workspace_start`, `shim_restart`,
  `handover_transfer`) feed one histogram per phase without a `workspace`
  label, so a regression is read across every workspace at once and a
  closed workspace leaves no histogram behind.
- Which workspace was slow is `agentrepl_workspace_restart_last_seconds`, the
  latest duration of each phase per workspace, deleted when the workspace
  leaves the registry.

## 12. Each process run is its own set of series

- Found in the metrics loop of 2026-10-19: a handover's successor booted
  once and started 17 workspaces, and the stored history showed neither. Its
  boot counter (1) continued the old daemon's identical series, and its 17
  starts read as the old daemon's 18 dropping to 17, which no query counts.
- Cause: every target was labeled only by its program, so every process of
  a program wrote one series, while every counter starts from zero with its
  process. A restart was visible only when a counter went down.
- Decided: every target also carries `run`, the process's pid and start
  instant (`agentrepl/logging/metrics.Run`). Each run starts its own series
  from zero, which is exactly what `increase_pure` reads, and a version is
  joined to the series of its own run (`on(run)`).
- Cost: a program's series are minted again at each of its restarts, mostly
  the daemon's at each deploy. VictoriaMetrics keeps an ended run's series
  only as history, and the 30-day retention bounds it.

## 13. A surface pushes its final values when it closes

- Found in the metrics loop of 2026-10-19: two handovers in one half hour
  recorded no `handover_transfer` at all, and the deploys that moved the
  daemon recorded no `decide` phase.
- Cause: the outgoing daemon times both after the handover, and by then its
  target file names the successor, so the store never scrapes it again. The
  same loss hit every exit: whatever a process recorded after its last
  scrape went with it.
- Decided: closing a metrics surface pushes everything its registry holds to
  the store's import endpoint, labeled exactly as its scrapes are (`job`,
  `instance`, `run`). The endpoint and job are one installed file,
  `metrics/store.json`, which `daemon/reposcans` pins to the plist's port and
  `scrape.yml`'s job. A push that fails is recorded through the runtime's
  canonical logger and never holds the exit.
- Not chosen: keeping the outgoing daemon scraped until it exits. Its target
  file is the successor's from the takeover, and waiting on one more scrape
  would hold the exit for up to two scrape intervals for nothing a push
  does not already deliver.

