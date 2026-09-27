package deploy

import (
	"bytes"
	"context"
	"fmt"
	"os/exec"
	"strconv"
	"strings"
	"time"

	"claude-repld/internal/rollout"
)

// layoutProbeBound bounds one `-layout-version` question. The binary answers
// from a compiled constant before it opens anything, so this is a bound on
// the process start alone.
const layoutProbeBound = 10 * time.Second

// BinaryLayout asks a daemon binary which state layout it writes, by running
// it with `-layout-version`: the answer is the binary's own, never inferred
// from the source it was built from.
func BinaryLayout(ctx context.Context, bin string) (int, error) {
	ctx, cancel := context.WithTimeout(ctx, layoutProbeBound)
	defer cancel()
	var stderr bytes.Buffer
	cmd := exec.CommandContext(ctx, bin, "-"+rollout.LayoutVersionFlagName)
	cmd.Stderr = &stderr
	out, err := cmd.Output()
	if err != nil {
		return 0, fmt.Errorf("deploy: ask %s for its state layout: %w (stderr: %q)", bin, err, strings.TrimSpace(stderr.String()))
	}
	layout, err := strconv.Atoi(strings.TrimSpace(string(out)))
	if err != nil {
		return 0, fmt.Errorf("deploy: %s answered a state layout that is not a number: %q", bin, strings.TrimSpace(string(out)))
	}
	if layout <= 0 {
		return 0, fmt.Errorf("deploy: %s answered a non-positive state layout %d", bin, layout)
	}
	return layout, nil
}
