// Package e2e is the cross-system end-to-end suite: a real agent-repld, a
// real shim-store, a real shim-claude-sidecar, and the real TypeScript shim
// (built from source, running its `--fake` offline scripted vendor),
// composed exactly as production wires them. The world every scenario builds
// is kit/stack's; see SPEC.md for the full design and the per-area test list.
//
// Nothing here reaches inside any of the four systems. Every assertion an
// area test makes is on what crossed a real wire: the daemon's Connect API
// (agentreplv1connect.AgentReplClient) and its watch streams, or a
// structured-log record from one of the four processes.
//
// SYNCHRONIZATION IS ALWAYS A REAL SIGNAL — a watch-stream frame, a
// structured-log record, or a bounded poll of a store read verb. There is no
// time.Sleep anywhere in this package.
package e2e

import (
	"agentrepl/e2e/kit/stack"
	"bytes"
	"fmt"
	"io/fs"
	"os"
	"path/filepath"
	"regexp"
	"testing"
)

// TestMain runs the suite over kit/stack's world (stack.RunSuite): the grep
// gate first, then every scenario, then the perf phase's final summary block
// (PERF-SPEC.md §D4), which is a no-op in the default build, where the perf
// files are not compiled at all.
func TestMain(m *testing.M) {
	// A test runs in its package's directory (runtime.Caller's file carries
	// no absolute path under -trimpath).
	here, err := os.Getwd()
	if err != nil {
		fmt.Fprintln(os.Stderr, "e2e: the working directory, which is the e2e package's:", err)
		os.Exit(1)
	}
	os.Exit(stack.RunSuite(m, here, runGrepGate, perfReportSummary))
}

// ---------------------------------------------------------------------------
// The grep gate (SPEC.md section B, "Grep gate"). Runs once, before any
// test, over every *_test.go file this package's own directory holds
// (harness files included — they hold none of the forbidden shapes either).
//
// Two forbidden shapes:
//
//  1. A direct call to the store's one write verb, WriteBatch. Of
//     storev1connect.ShimStoreClient's eight methods, WriteBatch is the ONLY
//     one that is not Open-/Watch-/Get-prefixed (confirmed by inspection of
//     proto/gen/go/store/v1/storev1connect: OpenAgentSession,
//     WatchAgentSession, ReadAgentPage, WatchBashRun, GetWorkflow,
//     GetSidecarCursors, GetLiveWork, WriteBatch) — so a literal source scan
//     for a method call naming it is exact, not a heuristic. (This doc
//     comment spells the verb without its call syntax on purpose: written
//     out as a call it would trip the gate's own self-scan below.)
//  2. A literal path write (os.MkdirAll/os.WriteFile/os.Create) whose path
//     argument's own source line mentions a vendor-transcript location this
//     suite must never hand-author: "CLAUDE_CONFIG_DIR", "/projects/", or
//     this package's own config-root/spool-root field names.
//
// This is a small regex scan run from TestMain, not a build-tag trick, so it
// fails loudly and specifically, naming the offending file:line.
// ---------------------------------------------------------------------------

var (
	writeBatchCall  = regexp.MustCompile(`\.WriteBatch\s*\(`)
	pathWriteCall   = regexp.MustCompile(`os\.(MkdirAll|WriteFile|Create)\s*\(`)
	forbiddenPathIn = []string{
		"CLAUDE_CONFIG_DIR",
		"/projects/",
		"DefaultConfigDir",
		"MultiRepoConfigDir",
		"SpoolRoot",
	}
)

func runGrepGate(dir string) error {
	entries, err := os.ReadDir(dir)
	if err != nil {
		return fmt.Errorf("read %s: %w", dir, err)
	}
	var paths []string
	for _, entry := range entries {
		name := entry.Name()
		if entry.IsDir() || filepath.Ext(name) != ".go" || !bytes.HasSuffix([]byte(name), []byte("_test.go")) {
			continue
		}
		paths = append(paths, filepath.Join(dir, name))
	}
	// The harness code that moved out of this package into kit/ (its own
	// module, so its tests are reused) is held to the same gate, every file.
	err = filepath.WalkDir(filepath.Join(dir, "kit"), func(path string, d fs.DirEntry, err error) error {
		if err != nil {
			return err
		}
		if !d.IsDir() && filepath.Ext(path) == ".go" {
			paths = append(paths, path)
		}
		return nil
	})
	if err != nil {
		return fmt.Errorf("walk %s: %w", filepath.Join(dir, "kit"), err)
	}
	var violations []string
	for _, path := range paths {
		src, err := os.ReadFile(path)
		if err != nil {
			return fmt.Errorf("read %s: %w", path, err)
		}
		for i, line := range bytes.Split(src, []byte("\n")) {
			lineNo := i + 1
			if writeBatchCall.Match(line) {
				violations = append(violations, fmt.Sprintf("%s:%d: calls a store write verb (WriteBatch) directly — drive the shim/sidecar instead", path, lineNo))
			}
			if pathWriteCall.Match(line) {
				for _, forbidden := range forbiddenPathIn {
					if bytes.Contains(line, []byte(forbidden)) {
						violations = append(violations, fmt.Sprintf("%s:%d: hand-authors a path under %q — vendor files must come from the real shim's --fake writer, never a test", path, lineNo, forbidden))
						break
					}
				}
			}
		}
	}
	if len(violations) == 0 {
		return nil
	}
	msg := "the grep gate found forbidden shapes in this package's own test files:"
	for _, v := range violations {
		msg += "\n  " + v
	}
	return fmt.Errorf("%s", msg)
}
