package hostpower

import (
	"context"
	"errors"
	"fmt"
	"os/exec"
	"strings"
	"time"
)

// EnergyModeBound bounds one `pmset -g` read. Measured 2026-11-17 on a
// sixteen-core Mac at a load average of 8.7: p50 12ms, p99 53ms, max 57ms
// over 100 reads.
const EnergyModeBound = 150 * time.Millisecond

// runBounded runs TOOL with ARGS, bounded by BOUND, and answers its output.
func runBounded(bound time.Duration, tool string, args ...string) (string, error) {
	ctx, cancel := context.WithTimeout(context.Background(), bound)
	defer cancel()
	out, err := exec.CommandContext(ctx, tool, args...).CombinedOutput()
	if errors.Is(ctx.Err(), context.DeadlineExceeded) {
		return "", fmt.Errorf("hostpower: %s %s did not answer within %s", tool, strings.Join(args, " "), bound)
	}
	if err != nil {
		return "", fmt.Errorf("hostpower: %s %s: %w: %s", tool, strings.Join(args, " "), err, strings.TrimSpace(string(out)))
	}
	return string(out), nil
}

// readEnergyMode runs PMSET -g, bounded by BOUND, and answers the energy mode
// its output names (ParseEnergyMode).
func readEnergyMode(pmset string, bound time.Duration) (string, error) {
	out, err := runBounded(bound, pmset, "-g")
	if err != nil {
		return "", err
	}
	return ParseEnergyMode(out), nil
}

// readLidClosedAwake answers whether the lid is closed (IOREG's
// AppleClamshellState) while system sleep is disabled (PMSET -g's
// SleepDisabled): the machine running as persistent wifi mode leaves it under
// a closed lid. Each read is bounded by BOUND.
func readLidClosedAwake(ioreg, pmset string, bound time.Duration) (bool, error) {
	lid, err := runBounded(bound, ioreg, "-r", "-k", "AppleClamshellState", "-d", "4")
	if err != nil {
		return false, err
	}
	if !ParseLidClosed(lid) {
		return false, nil
	}
	power, err := runBounded(bound, pmset, "-g")
	if err != nil {
		return false, err
	}
	return ParseSleepDisabled(power), nil
}
