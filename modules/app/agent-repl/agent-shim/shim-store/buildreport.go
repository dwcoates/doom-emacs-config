package main

import (
	"os"

	"agentrepl/logging/buildreport"
	"agentrepl/shim-store/internal/logging"
)

// buildReportDeps seams reportBuild for tests: dir resolution, self-reporting
// and writing are all injected, so a test never touches the real
// ~/.cache/agent-repl/run and never overwrites the owner's own report.
type buildReportDeps struct {
	getenv     func(string) string
	resolveDir func(func(string) string) (string, error)
	self       func() (buildreport.Report, error)
	write      func(dir, service string, r buildreport.Report) error
}

// defaultBuildReportDeps wires reportBuild to the real package for production.
func defaultBuildReportDeps() buildReportDeps {
	return buildReportDeps{
		getenv:     os.Getenv,
		resolveDir: buildreport.ResolveDir,
		self:       buildreport.Self,
		write:      buildreport.Write,
	}
}

// reportBuild writes shim-store's build report — this process's pid and the
// content hash of the binary it runs — as soon as the canonical logger
// exists and before the store starts serving, so the daemon's deploy can
// tell this process apart from a stale build.
//
// EVERY FAILURE IS LOGGED AND SWALLOWED. The store never talks to the
// daemon, so a report that never lands means only that the daemon's deploy
// will read this service as "not running the fresh build" and restart it —
// a service that cannot report its own build still has every reason to keep
// serving the one it has.
func reportBuild(log *logging.Logger, deps buildReportDeps) {
	r, err := deps.self()
	if err != nil {
		log.Log(logging.Fields{Operation: "store.buildreport.self-failed", Level: "error"},
			"shim-store could not determine its own build to report at boot service=%s: %v",
			buildreport.ServiceStore, err)
		return
	}
	dir, err := deps.resolveDir(deps.getenv)
	if err != nil {
		log.Log(logging.Fields{Operation: "store.buildreport.dir-failed", Level: "error"},
			"shim-store could not resolve its build report directory service=%s: %v",
			buildreport.ServiceStore, err)
		return
	}
	path := buildreport.Path(dir, buildreport.ServiceStore)
	if err := deps.write(dir, buildreport.ServiceStore, r); err != nil {
		log.Log(logging.Fields{Operation: "store.buildreport.write-failed", Level: "error", Path: path},
			"shim-store failed to write its build report service=%s dir=%s: %v",
			buildreport.ServiceStore, dir, err)
		return
	}
	log.Log(logging.Fields{Operation: "store.buildreport", Path: path},
		"shim-store reported its build build=%s path=%s", r.Build, path)
}
