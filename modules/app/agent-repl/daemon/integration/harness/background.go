package harness

import "errors"

// EVERY RUN OF THIS HARNESS HAPPENS AT BACKGROUND PRIORITY.
//
// A run built on this harness is the heaviest thing a test can do on the
// host: at `-parallel 8` each test owns a real claude-repld plus a fake shim,
// and the e2e suite (which runs through MainAt too) owns a whole quartet per
// test. Several of those at normal priority took the host to a load average
// of 281 while the owner's live shim fell four minutes behind and a store
// write took 163s. bin/background.sh demotes a run and every process it
// starts, and exports BackgroundPriorityEnv once it has; the Makefile targets
// route through it. A raw `go test -tags integration ./integration/...` (or a
// compiled test binary run by hand) would skip it, so WithRunRoot refuses to
// start without the marker instead of running at normal priority.

// BackgroundPriorityEnv is the marker bin/background.sh exports after it has
// put the run at background priority. Only that script sets it.
const BackgroundPriorityEnv = "AGENT_REPL_BACKGROUND_PRIORITY"

// errNotBackground is the refusal: it names the helper and the entry points
// that already route through it, so the reader knows how to rerun.
var errNotBackground = errors.New(
	"REFUSED TO START: tests run only at background priority, through bin/background.sh. " +
		"Use `make integration`/`make test` (daemon), `make test` (e2e), or wrap the command: " +
		"`<module>/bin/background.sh go test ...`. " +
		BackgroundPriorityEnv + " is unset, so this run was never demoted")

// requireBackgroundPriority refuses a run that did not come through
// bin/background.sh. getenv is os.Getenv outside its own tests.
func requireBackgroundPriority(getenv func(string) string) error {
	if getenv(BackgroundPriorityEnv) == "" {
		return errNotBackground
	}
	return nil
}
