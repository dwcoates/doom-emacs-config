// Package reposcans holds the tests that read the agent-repl module's whole
// tree rather than one system's: the checks that keep the merge gate's suite
// selection honest against every suite's real dependencies, and the source
// scan that pins "a system's integration tests run that system alone".
//
// They live in a module of their own, outside every system's suite, so no
// system's reuse key covers the other systems' trees (testrun/internal/reuse):
// a webapp edit no longer invalidates the daemon suite. The suite always runs.
package reposcans
