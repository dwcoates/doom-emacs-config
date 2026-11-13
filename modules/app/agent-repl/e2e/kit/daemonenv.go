package kit

import "strings"

// WorldDaemonEnv is a world daemon's environment: the caller's ExtraEnv,
// then this suite's own statements, appended LAST so they win (the shim's env
// is scanned front-to-back and the last assignment of a name is the effective
// one). It names no checkout: harness.StartDaemon pins its own (see
// CheckoutEnv).
func WorldDaemonEnv(extra []string, spoolRoot, lockBin string) []string {
	return append(append([]string{}, extra...),
		"AGENT_REPL_FAKE_SPOOL_ROOT="+spoolRoot,
		// The shim inherits this from the daemon and spawns it for every
		// kernel claim; without it no session starts at all.
		"AGENT_REPL_SHIM_LOCK_BIN="+lockBin)
}

// CheckoutEnv is agent-repld's checkout-root override (checkout.Env).
//
// THE CHECKOUT IS WHERE A DEPLOY INSTALLS. The daemon's own deploy judges and
// installs against it: the daemon binary lands at <checkout>/daemon/bin, and
// the fresh elisp build is hashed from <checkout>/lisp. A Go world therefore
// never names this worktree — harness.StartDaemon pins its own throwaway
// checkout (harness.BuildIdentityEnv), whose elisp is the build every harness
// WatchDaemon stream reports — or a landing's deploy would install a test
// daemon over this worktree's daemon/bin.
const CheckoutEnv = "AGENT_REPL_CHECKOUT"

// ValueOf answers the value of name in a KEY=VALUE list, or "" when absent.
func ValueOf(env []string, name string) string {
	for _, kv := range env {
		if v, ok := strings.CutPrefix(kv, name+"="); ok {
			return v
		}
	}
	return ""
}
