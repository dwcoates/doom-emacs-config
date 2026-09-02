package integration

import (
	"encoding/json"
	"os"
	"path/filepath"
	"strings"
	"testing"
)

// CRITIQUE 23 — the flags and their env vars, exercised on the REAL binary.
//
// AN EXPLICIT FLAG ALWAYS BEATS THE ENV, and a malformed window is a bootstrap
// refusal rather than a silent default. Neither can be reached through
// startSidecar: it passes --store-socket and AGENT_REPL_STORE_SOCKET naming ONE
// path, so neither spelling can be seen beating the other, and it treats a
// failed start as a test error rather than as the contract. These subjects
// launch the binary themselves.
//
// THE REFUSAL HAPPENS BEFORE THE LOG FILE EXISTS. durationSource.resolve() runs
// in main, ahead of run()'s openLogger, so the contract is exactly three things:
// exit 1, ONE bootstrap record on stderr, and NO log file — a process that
// created its log and then refused would look, to an operator reading the log
// directory, exactly like one that started.

// TestAMalformedDurationFlagRefusesToStart asserts the exit status and the
// stderr record of a window the operator spelled wrong.
func TestAMalformedDurationFlagRefusesToStart(t *testing.T) {
	// Arrange.
	ctx, cancel := testContext(t)
	defer cancel()
	tree := newVendorTree(t)
	fake := startFakeStore(t)
	logPath := filepath.Join(t.TempDir(), "never-created.log")
	args := append([]string{"--store-socket", fake.Socket}, sidecarFlags(t, tree, logPath)...)
	args = append(args, "--stale-grace", "30 seconds")

	// Act.
	proc := launchSidecar(t, args)
	exit := proc.AwaitExit(ctx)

	// Assert: exit 1, and ONE bootstrap record naming the flag it refused.
	if exit != 1 {
		t.Errorf("a malformed window exited %d, wanted 1; its stderr was:\n%s", exit, proc.Stderr())
	}
	records := bootstrapRecords(t, proc.Stderr())
	if len(records) != 1 {
		t.Fatalf("stderr carried %d bootstrap records, wanted exactly one:\n%s", len(records), proc.Stderr())
	}
	if got := records[0].Operation; got != "sidecar.bootstrap" {
		t.Errorf("the bootstrap record names operation %q, wanted sidecar.bootstrap", got)
	}
	if got := records[0].Level; got != "error" {
		t.Errorf("the bootstrap record is at level %q, wanted error", got)
	}
	detail, _ := records[0].Context["error"].(string)
	if !strings.Contains(detail, "--stale-grace") {
		t.Errorf("the bootstrap record does not name the flag it refused: %q", detail)
	}
}

// TestAMalformedDurationFlagCreatesNoLogFile asserts the other half: the refusal
// happens before the log file is opened, so none is left behind.
func TestAMalformedDurationFlagCreatesNoLogFile(t *testing.T) {
	// Arrange.
	ctx, cancel := testContext(t)
	defer cancel()
	tree := newVendorTree(t)
	fake := startFakeStore(t)
	logPath := filepath.Join(t.TempDir(), "never-created.log")
	args := append([]string{"--store-socket", fake.Socket}, sidecarFlags(t, tree, logPath)...)
	args = append(args, "--unowned-spool-window", "not-a-duration")

	// Act.
	proc := launchSidecar(t, args)
	proc.AwaitExit(ctx)

	// Assert.
	if _, err := os.Stat(logPath); err == nil {
		t.Errorf("the refused bootstrap created its log file at %s; the refusal happens before the logger exists", logPath)
	} else if !os.IsNotExist(err) {
		t.Fatalf("stat %s: %v", logPath, err)
	}
}

// TestANegativeDurationEnvValueRefusesToStart asserts the env spelling is held
// to the same rule as the flag: a negative window is refused, not defaulted.
func TestANegativeDurationEnvValueRefusesToStart(t *testing.T) {
	// Arrange.
	ctx, cancel := testContext(t)
	defer cancel()
	tree := newVendorTree(t)
	fake := startFakeStore(t)
	logPath := filepath.Join(t.TempDir(), "never-created.log")
	args := append([]string{"--store-socket", fake.Socket}, sidecarFlags(t, tree, logPath)...)

	// Act.
	proc := launchSidecar(t, args, "AGENT_REPL_STALE_SHELL_SILENCE=-5s")
	exit := proc.AwaitExit(ctx)

	// Assert.
	if exit != 1 {
		t.Errorf("a negative window from the env exited %d, wanted 1; its stderr was:\n%s", exit, proc.Stderr())
	}
	records := bootstrapRecords(t, proc.Stderr())
	if len(records) != 1 {
		t.Fatalf("stderr carried %d bootstrap records, wanted exactly one:\n%s", len(records), proc.Stderr())
	}
	detail, _ := records[0].Context["error"].(string)
	if !strings.Contains(detail, "AGENT_REPL_STALE_SHELL_SILENCE") {
		t.Errorf("the bootstrap record blames %q rather than the env var the value came from", detail)
	}
}

// TestTheStoreSocketFlagBeatsItsEnvVar asserts the precedence rule on the one
// flag where getting it wrong points the whole file plane at the wrong store:
// two stores exist, the flag names A, the env names B, and B hears NOTHING.
func TestTheStoreSocketFlagBeatsItsEnvVar(t *testing.T) {
	// Arrange.
	ctx, cancel := testContext(t)
	defer cancel()
	tree := newVendorTree(t)
	flagStore := startFakeStore(t)
	envStore := startFakeStore(t)
	logPath := filepath.Join(t.TempDir(), "sidecar.log")
	args := append([]string{"--store-socket", flagStore.Socket}, sidecarFlags(t, tree, logPath)...)

	// Act: the FLAG's store answering an rpc is the signal — no duration is
	// waited on, and the other store is asked only after that has happened.
	launchSidecar(t, args, "AGENT_REPL_STORE_SOCKET="+envStore.Socket)
	select {
	case <-flagStore.callC:
	case <-ctx.Done():
		t.Fatalf("the store named by --store-socket was never called; the env var's store saw %d call(s)", envStore.CallCount())
	}

	// Assert.
	if n := envStore.CallCount(); n != 0 {
		t.Errorf("the store named by $AGENT_REPL_STORE_SOCKET received %d call(s); an explicit flag beats the env", n)
	}
}

// TestTheStoreSocketEnvVarIsUsedWhenNoFlagIsPassed asserts the other side of the
// same rule: the env var is the flag's DEFAULT, not dead configuration.
func TestTheStoreSocketEnvVarIsUsedWhenNoFlagIsPassed(t *testing.T) {
	// Arrange.
	ctx, cancel := testContext(t)
	defer cancel()
	tree := newVendorTree(t)
	envStore := startFakeStore(t)
	logPath := filepath.Join(t.TempDir(), "sidecar.log")

	// Act.
	launchSidecar(t, sidecarFlags(t, tree, logPath), "AGENT_REPL_STORE_SOCKET="+envStore.Socket)

	// Assert: the store the env named is the one that was called.
	select {
	case got := <-envStore.callC:
		if got != "GetSidecarCursors" {
			t.Errorf("the first rpc was %q, wanted GetSidecarCursors", got)
		}
	case <-ctx.Done():
		t.Fatalf("the store named by $AGENT_REPL_STORE_SOCKET was never called")
	}
}

// bootstrapRecords parses the bootstrap records a refused start wrote to stderr.
// EVERY non-empty stderr line must be one: a bootstrap refusal's whole output is
// its record, and a stray plain-text line is a defect.
func bootstrapRecords(t *testing.T, stderr string) []logRecord {
	t.Helper()
	var out []logRecord
	for i, line := range strings.Split(stderr, "\n") {
		if strings.TrimSpace(line) == "" {
			continue
		}
		var rec logRecord
		if err := json.Unmarshal([]byte(line), &rec); err != nil {
			t.Fatalf("stderr line %d of a refused bootstrap is not JSON: %v\n%s", i+1, err, line)
		}
		out = append(out, rec)
	}
	return out
}
