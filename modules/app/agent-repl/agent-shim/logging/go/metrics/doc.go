// Package metrics is the ALWAYS-ON performance metrics surface every
// long-lived Go runtime of agent-repl (the daemon, the store, the sidecar)
// exposes through one shared implementation
// (docs/PLAN-runtime-metrics.md).
//
// METRICS ARE NOT PROFILING. A metric is a running number updated in
// nanoseconds and safe to expose always; a profile samples call stacks, costs
// CPU while it runs and exposes memory contents. The opt-in profiling
// surfaces (pprofsurface in each runtime) stay where they are and serve
// nothing from here, and this package serves `/metrics` and nothing else.
//
// THE TARGET FILE IS THE ADVERTISEMENT. A runtime listens on an ephemeral
// loopback port and, once it owns its role, atomically writes
// `<root>/targets/<program>.json` in the file_sd shape VictoriaMetrics reads.
// No port is fixed, so a test's runtime can never collide with the live one,
// and a daemon handing over to its successor never contends for a port: the
// successor overwrites the file when it takes over, exactly as it publishes
// daemon.addr. A file is never removed on exit; a target that no longer
// answers reads as down, which is the honest state of a runtime that is down.
//
// An empty root is OFF: production launchers pass it, tests that do not ask
// for metrics run without a listener.
package metrics
