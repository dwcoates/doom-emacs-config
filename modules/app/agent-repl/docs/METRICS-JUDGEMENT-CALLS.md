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
