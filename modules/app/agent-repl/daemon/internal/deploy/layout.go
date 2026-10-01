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

// askBinary runs a daemon binary with one question's argument, bounded by
// layoutProbeBound, and answers its trimmed stdout. A binary that fails is
// never read as any answer: its error carries the question and its stderr.
// Every question a deploy asks the STAGED binary goes through here.
func askBinary(ctx context.Context, bin, question string) (string, error) {
	ctx, cancel := context.WithTimeout(ctx, layoutProbeBound)
	defer cancel()
	var stderr bytes.Buffer
	cmd := exec.CommandContext(ctx, bin, question)
	cmd.Stderr = &stderr
	out, err := cmd.Output()
	if err != nil {
		return "", fmt.Errorf("deploy: ask %s %s: %w (stderr: %q)", bin, question, err, strings.TrimSpace(stderr.String()))
	}
	return strings.TrimSpace(string(out)), nil
}

// BinaryLayout asks a daemon binary which state layout it writes, by running
// it with `-layout-version`: the answer is the binary's own, never inferred
// from the source it was built from.
func BinaryLayout(ctx context.Context, bin string) (int, error) {
	answer, err := askBinary(ctx, bin, "-"+rollout.LayoutVersionFlagName)
	if err != nil {
		return 0, err
	}
	layout, err := strconv.Atoi(answer)
	if err != nil {
		return 0, fmt.Errorf("deploy: %s answered a state layout that is not a number: %q", bin, answer)
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
	answer, err := askBinary(ctx, bin, "-"+rollout.MigrationKindFromFlagName+"="+strconv.Itoa(from))
	if err != nil {
		return 0, err
	}
	return parseMigrationKind(bin, answer)
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
