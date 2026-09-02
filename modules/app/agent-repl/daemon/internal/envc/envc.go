// Package envc is the daemon's four environment contracts, typed.
//
// The contracts are read once at boot (cmd/claude-repld) and passed down as a
// value; nothing below boot reads os.Getenv for them. See
// docs/overhaul/daemon.md "Contract context (for implementers)" and the
// ARCHITECTURE.md shim-spawn contract, which reproduces the same four names on
// the shim's environment.
package envc

import (
	"os"
	"strings"
)

// The environment variable names of the four contracts. They are also the
// names the daemon sets on a spawned shim, so they are exported.
const (
	// EnvFake requests the whole stack's fake mode (no vendor process, no
	// vendor calls). The -fake flag overrides it.
	EnvFake = "AGENT_REPL_FAKE"
	// EnvForbidVendorCalls makes any vendor invocation an error rather than a
	// call. Every test sets it; VendorGuard enforces it.
	EnvForbidVendorCalls = "AGENT_REPL_FORBID_VENDOR_CALLS"
	// EnvStateDir relocates the state root. The -state-dir flag overrides it.
	EnvStateDir = "AGENT_REPL_STATE_DIR"
	// EnvOwned marks a process as launched by the daemon rather than by a
	// human at a shell.
	EnvOwned = "AGENT_REPL_OWNED"
)

// Contracts is the resolved value of the four environment contracts. It is
// immutable; the With* methods return a copy so a flag can override an
// environment value without a mutable global.
type Contracts struct {
	fake              bool
	forbidVendorCalls bool
	stateDir          string
	owned             bool
}

// Load reads the four contracts from the process environment. It never fails:
// an unrecognized value for a boolean contract is false, which is the safe
// reading for all four.
func Load() Contracts {
	return Contracts{
		fake:              truthy(os.Getenv(EnvFake)),
		forbidVendorCalls: truthy(os.Getenv(EnvForbidVendorCalls)),
		stateDir:          os.Getenv(EnvStateDir),
		owned:             truthy(os.Getenv(EnvOwned)),
	}
}

// Fake reports whether the daemon runs without a real vendor process: shims
// spawn with --fake and the classifier uses its keyword heuristic.
func (c Contracts) Fake() bool { return c.fake }

// ForbidVendorCalls reports whether any vendor invocation must be refused
// rather than attempted. VendorGuard is how a call site asks.
func (c Contracts) ForbidVendorCalls() bool { return c.forbidVendorCalls }

// StateDir is the configured state root, empty when unset (stateroot then
// applies its default).
func (c Contracts) StateDir() string { return c.stateDir }

// WithFake returns a copy whose fake contract is the flag's value; the -fake
// flag overrides the environment.
func (c Contracts) WithFake(fake bool) Contracts { c.fake = fake; return c }

// WithStateDir returns a copy whose state root is dir; the -state-dir flag
// overrides the environment. An empty dir leaves the environment value alone.
func (c Contracts) WithStateDir(dir string) Contracts {
	if dir != "" {
		c.stateDir = dir
	}
	return c
}

// truthy is the one spelling of a boolean environment contract. Anything not
// listed is false.
func truthy(v string) bool {
	switch strings.ToLower(strings.TrimSpace(v)) {
	case "1", "true", "yes", "on":
		return true
	default:
		return false
	}
}
