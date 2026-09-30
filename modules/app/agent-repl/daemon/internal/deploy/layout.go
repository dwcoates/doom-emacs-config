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
	"claude-repld/internal/wsm"
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

// BinaryMigrationKind asks a daemon binary what its migration steps from a
// running layout up to its own mean for the running build, by running it with
// `-migration-kind-from`: the answer is the binary's own migration list.
func BinaryMigrationKind(ctx context.Context, bin string, from int) (wsm.MigrationKind, error) {
	ctx, cancel := context.WithTimeout(ctx, layoutProbeBound)
	defer cancel()
	var stderr bytes.Buffer
	cmd := exec.CommandContext(ctx, bin, "-"+rollout.MigrationKindFromFlagName+"="+strconv.Itoa(from))
	cmd.Stderr = &stderr
	out, err := cmd.Output()
	if err != nil {
		return 0, fmt.Errorf("deploy: ask %s what its migrations from layout %d are: %w (stderr: %q)", bin, from, err, strings.TrimSpace(stderr.String()))
	}
	return parseMigrationKind(bin, strings.TrimSpace(string(out)))
}

// parseMigrationKind reads a binary's migration-kind answer. An answer that
// names no kind is an error, never read as either one.
func parseMigrationKind(bin, answer string) (wsm.MigrationKind, error) {
	for _, kind := range []wsm.MigrationKind{wsm.MigrationAdditive, wsm.MigrationBreaking} {
		if answer == kind.String() {
			return kind, nil
		}
	}
	return 0, fmt.Errorf("deploy: %s answered a migration kind that names none: %q", bin, answer)
}
