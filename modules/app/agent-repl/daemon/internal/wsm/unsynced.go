package wsm

import (
	"fmt"

	"claude-repld/internal/envc"
)

// EnvTestUnsyncedWrites asks a TEST RUN's daemon to open its state database
// with SQLite's forced flushes off (WithUnsyncedWrites). Its one accepted
// value is "1".
//
// A test run's databases are thrown away when the run ends, so the fsync per
// commit buys nothing, and a full run's hundreds of them starved the owner's
// live store of disk bandwidth (owner ruling, 2026-10-06).
const EnvTestUnsyncedWrites = "AGENT_REPL_TEST_SQLITE_UNSYNCED"

// UnsyncedFromEnv reads EnvTestUnsyncedWrites at boot.
//
// IT IS HONORED ONLY UNDER THE VENDOR GUARD (envc.EnvForbidVendorCalls), the
// marker every test harness sets and a live daemon never carries, exactly as
// the temporary-directory guard's test seam is (tempdirs.FromEnv). A live
// daemon that somehow inherited the flag REFUSES TO BOOT rather than run its
// state database without durability; a value other than "1" is refused too,
// so a typo never quietly means "durable" or "not durable".
func UnsyncedFromEnv(contracts envc.Contracts, getenv func(string) string) (bool, error) {
	switch value := getenv(EnvTestUnsyncedWrites); value {
	case "":
		return false, nil
	case "1":
		if !contracts.ForbidVendorCalls() {
			return false, fmt.Errorf("wsm: %s=1 is a test-run seam and is honored only with %s set; a live daemon keeps its state database durable", EnvTestUnsyncedWrites, envc.EnvForbidVendorCalls)
		}
		return true, nil
	default:
		return false, fmt.Errorf("wsm: %s=%q is not a value it accepts; set it to 1 or leave it unset", EnvTestUnsyncedWrites, value)
	}
}
