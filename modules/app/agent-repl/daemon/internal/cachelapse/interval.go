package cachelapse

import (
	"fmt"
	"time"

	"agent-repld/internal/envc"
)

// EnvTestKeepaliveInterval compresses a TEST RUN's keep-alive cadence: the
// Keeper times every beat this long after the cache clock instead of
// KeepaliveInterval. Its value is a Go duration ("100ms").
//
// WHY. The daemon drives every keep-alive (owner ruling, 2026-10-26), and its
// interval is fifty-two minutes: an end-to-end suite that must watch a beat
// go out, rewind, and be waited out by a real prompt cannot wait that long,
// and nothing else can make a beat come due. The shim's own cadence lever
// (AGENT_REPL_FAKE_KEEPALIVE_INTERVAL_MS) did this until the shim stopped
// scheduling beats, and is gone with it.
//
// ONLY THE CADENCE IS COMPRESSED. StandDownGrace and RestoreLead keep their
// production values, so a test world's beats never cycle its session down:
// what the suite observes is a run of beats on one live shim.
const EnvTestKeepaliveInterval = "AGENT_REPL_TEST_KEEPALIVE_INTERVAL"

// IntervalFromEnv answers the keep-alive interval for this process:
// KeepaliveInterval, or EnvTestKeepaliveInterval's value in a test run.
//
// IT IS HONORED ONLY UNDER THE VENDOR GUARD (envc.EnvForbidVendorCalls), as
// every test-run seam is: a live daemon that inherited it REFUSES TO BOOT
// rather than beat on a cadence nobody chose for its person. A value that is
// not a positive duration is refused too, since a seam that silently did
// nothing would make the suite it was set for lie.
func IntervalFromEnv(contracts envc.Contracts, getenv func(string) string) (time.Duration, error) {
	raw := getenv(EnvTestKeepaliveInterval)
	if raw == "" {
		return KeepaliveInterval, nil
	}
	if err := contracts.RequireTestRun(EnvTestKeepaliveInterval+"="+raw, "a live daemon beats every "+KeepaliveInterval.String()); err != nil {
		return 0, fmt.Errorf("cachelapse: %w", err)
	}
	interval, err := time.ParseDuration(raw)
	if err != nil {
		return 0, fmt.Errorf("cachelapse: %s=%q is not a duration: %w", EnvTestKeepaliveInterval, raw, err)
	}
	if interval <= 0 {
		return 0, fmt.Errorf("cachelapse: %s=%q is not a positive duration", EnvTestKeepaliveInterval, raw)
	}
	return interval, nil
}
