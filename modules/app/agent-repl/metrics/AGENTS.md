# metrics/

The always-on metrics store: VictoriaMetrics, run by launchd
(`launchd/com.agentrepl.victoriametrics.plist`), scraping the daemon, the
store and the sidecar every 15 seconds and keeping 30 days. The plan is
`docs/PLAN-runtime-metrics.md`; the calls made building it are
`docs/METRICS-JUDGEMENT-CALLS.md`. How to query it, add a metric, and when to
offer one: the `metrics-agent-repl` skill (`skills/metrics-agent-repl/`).

- `scrape.yml` is the scrape config, copied by `bin/requirements.sh` into
  `~/.cache/agent-repl/metrics/scrape.yml`. Its only targets are the files
  each runtime advertises under `~/.cache/agent-repl/metrics/targets/`
  (`agentrepl/logging/metrics`): no runtime has a fixed port.
- `requirements.json` installs VictoriaMetrics (Homebrew) and the service.
- The store answers PromQL on `127.0.0.1:8428`. No test starts it: tests read
  a runtime's `/metrics` directly.

## Where each series comes from

- Every Go runtime (`agentrepl/logging/metrics`): the Go runtime collector
  and `agentrepl_build_info{program, build}`.
- The daemon only:
  - `agentrepl_deployed_version_info{version}` (`daemon/internal/runtimemetrics`);
  - `agentrepl_process_*{process, workspace}` and `agentrepl_host_*`
    (`daemon/internal/procmetrics`), read from the kernel at scrape time.
