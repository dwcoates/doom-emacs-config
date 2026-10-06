package db

import (
	"os"
	"strings"
)

// EnvTestUnsyncedWrites asks a TEST RUN's store to open its database with
// SQLite's forced flushes off. Its one accepted value is "1". The daemon's
// state database answers the same variable (claude-repld's wsm package).
//
// A test run's databases are thrown away when the run ends, so the fsync per
// commit buys nothing, and a full run's hundreds of them starved the owner's
// live store of disk bandwidth (owner ruling, 2026-10-06).
const EnvTestUnsyncedWrites = "AGENT_REPL_TEST_SQLITE_UNSYNCED"

// envForbidVendorCalls is the marker every test harness sets and a live store
// never carries.
const envForbidVendorCalls = "AGENT_REPL_FORBID_VENDOR_CALLS"

// UnsyncedFromEnv reads EnvTestUnsyncedWrites.
//
// IT IS HONORED ONLY UNDER THE VENDOR GUARD (AGENT_REPL_FORBID_VENDOR_CALLS),
// so a live store that somehow inherited the flag REFUSES TO OPEN its database
// rather than run it without durability; a value other than "1" is refused
// too, so a typo never quietly means either answer.
func UnsyncedFromEnv() (bool, error) {
	switch value := os.Getenv(EnvTestUnsyncedWrites); value {
	case "":
		return false, nil
	case "1":
		if !forbidsVendorCalls(os.Getenv(envForbidVendorCalls)) {
			return false, invalidf("%s=1 is a test-run seam and is honored only with %s set; a live store keeps its database durable", EnvTestUnsyncedWrites, envForbidVendorCalls)
		}
		return true, nil
	default:
		return false, invalidf("%s=%q is not a value it accepts; set it to 1 or leave it unset", EnvTestUnsyncedWrites, value)
	}
}

// forbidsVendorCalls reads the vendor guard the way the daemon's envc does.
func forbidsVendorCalls(v string) bool {
	switch strings.ToLower(strings.TrimSpace(v)) {
	case "1", "true", "yes", "on":
		return true
	default:
		return false
	}
}
