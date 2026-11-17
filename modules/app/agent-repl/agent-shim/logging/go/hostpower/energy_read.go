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

// readEnergyMode runs PMSET -g, bounded by BOUND, and answers the energy mode
// its output names (ParseEnergyMode).
func readEnergyMode(pmset string, bound time.Duration) (string, error) {
	ctx, cancel := context.WithTimeout(context.Background(), bound)
	defer cancel()
	out, err := exec.CommandContext(ctx, pmset, "-g").CombinedOutput()
	if errors.Is(ctx.Err(), context.DeadlineExceeded) {
		return "", fmt.Errorf("hostpower: %s -g did not answer within %s (measured at most 57ms)", pmset, bound)
	}
	if err != nil {
		return "", fmt.Errorf("hostpower: %s -g: %w: %s", pmset, err, strings.TrimSpace(string(out)))
	}
	return ParseEnergyMode(string(out)), nil
}
