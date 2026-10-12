package wsm

import (
	"context"
	"errors"
	"fmt"

	"agent-repld/internal/envc"
)

// EnvTestStateInMemory asks a TEST RUN's daemon to keep its state database in
// memory instead of in wsm.db. Its one accepted value is "1".
//
// WHY. A test daemon's database is thrown away with its test, and most tests
// never start a second process on the same state root. Writing it to a file
// anyway cost a WAL of ~350KB per daemon, ~1,000 daemons a suite (owner
// ruling, 2026-10-12). A test whose state must outlive the process -- a
// successor, a restart, a harness read of the file -- keeps the file; the
// integration harness decides, and refuses loudly when it decided wrong.
const EnvTestStateInMemory = "AGENT_REPL_TEST_STATE_IN_MEMORY"

// InMemoryTestRun is the proof that this process may keep its state in
// memory. Only InMemoryFromEnv makes an enabled one, so no other path can hand
// a daemon a store that forgets everything on restart.
type InMemoryTestRun struct{ enabled bool }

// Enabled reports whether the state database lives in memory.
func (m InMemoryTestRun) Enabled() bool { return m.enabled }

// InMemoryFromEnv reads EnvTestStateInMemory at boot.
//
// IT IS HONORED ONLY UNDER THE VENDOR GUARD (envc.EnvForbidVendorCalls), as
// UnsyncedFromEnv is: a live daemon that inherited the flag REFUSES TO BOOT
// rather than run on state it would lose, and a value other than "1" is
// refused too.
func InMemoryFromEnv(contracts envc.Contracts, getenv func(string) string) (InMemoryTestRun, error) {
	switch value := getenv(EnvTestStateInMemory); value {
	case "":
		return InMemoryTestRun{}, nil
	case "1":
		if !contracts.ForbidVendorCalls() {
			return InMemoryTestRun{}, fmt.Errorf("wsm: %s=1 is a test-run seam and is honored only with %s set; a live daemon keeps its state in wsm.db", EnvTestStateInMemory, envc.EnvForbidVendorCalls)
		}
		return InMemoryTestRun{enabled: true}, nil
	default:
		return InMemoryTestRun{}, fmt.Errorf("wsm: %s=%q is not a value it accepts; set it to 1 or leave it unset", EnvTestStateInMemory, value)
	}
}

// errNotProved is OpenForTestRun's refusal of a proof InMemoryFromEnv did not
// enable.
var errNotProved = errors.New("wsm: an in-memory state database needs the test-run proof (InMemoryFromEnv); this process was not given it")

// OpenForTestRun opens a fresh in-memory state database for a test-run daemon
// that proved the seam. It is OpenInMemory's database: the same schema
// template, the same tail of Open.
func OpenForTestRun(ctx context.Context, proof InMemoryTestRun, opts ...Option) (DB, error) {
	if !proof.enabled {
		return nil, errNotProved
	}
	return openInMemory(ctx, opts)
}
