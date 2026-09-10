//go:build realtest

// Package realtest drives the OWNER'S ACTUAL EDITOR.
//
// Everything in here runs against the real thing: the one Emacs.app process on
// the owner's Mac, the real ~/.config/doom on master, the real ~/.claude-emacs
// state, the real store, sidecar, shim and daemon deployed from master, and the
// owner's real ~/.claude transcripts. There is NO sandbox and NO image, and no
// picture is ever taken. The single substitution is the vendor:
// AGENT_REPL_FORBID_VENDOR_CALLS=1 is exported onto the Emacs process and
// inherited by everything it spawns, so no real Claude call can occur.
//
// modules/app/agent-repl/docs/REALTEST-PLAN.md is the CONTRACT — which realtests
// exist, what each one measures, and the remediation loop they feed. This
// package is only the mechanics, and e2e/REALTEST-SPEC.md documents them.
//
// It shares NOTHING with the retired playtest layer, on purpose: that layer
// verified the module against a bare sandbox Doom profile with elisp-driven
// acts, which is the opposite of what a realtest is for.
//
// THE BUILD TAG IS THE FIRST GUARD and the environment gate is the second: no
// realtest can be reached by `go test ./...`, and even under the tag
// TestRealtest* skips unless bin/realtest.sh has set AGENT_REPL_REALTEST=1.
// bin/realtest.sh is the only supported entry point, because the preflight it
// performs — the readiness gate, the human-in-Emacs refusal, the backups — is
// not optional.
//
// Every unit test in this package runs under the same tag and needs none of
// that: they exercise the harvester, the phase reader and the source
// enumeration against fixtures in t.TempDir(), touch no real path, and start no
// process.
package realtest
