// Package reposcans holds the tests that read the agent-repl module's whole
// tree rather than one system's: the checks that keep the merge gate's suite
// selection honest against every suite's real dependencies, and the source
// scan that pins "a system's integration tests run that system alone".
//
// They live in a module of their own, outside every system's suite, so no
// system's reuse key covers the other systems' trees (testrun/internal/reuse):
// a webapp edit no longer invalidates the daemon suite. The suite always runs.
//
// It is also the home of every rule test of the daemon module (daemonrules)
// and of the helpers that read the daemon's sources for them (internal/), so
// the whole rule suite runs on a branch before its merge is admitted to the
// queue rather than only in the gate.
package reposcans
