// Package testenv owns environment variable names shared by the test runner
// and the integration harnesses it starts.
package testenv

const (
	// Prebuild names a directory one test process fills with shared binaries.
	Prebuild = "AGENT_REPL_TEST_PREBUILD"
	// Prebuilt names the directory later test processes read shared binaries from.
	Prebuilt = "AGENT_REPL_TEST_PREBUILT"
)
