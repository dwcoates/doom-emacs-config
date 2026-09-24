package integration

import (
	"bytes"
	"context"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"syscall"
	"testing"
	"time"

	"connectrpc.com/connect"

	conversationv1 "agentrepl/proto/conversation/v1"
	storev1 "agentrepl/proto/store/v1"
	"agentrepl/proto/store/v1/storev1connect"
)

// helpers_process_test.go — PROCESS-LEVEL helpers: launching the sidecar binary
// with an exact command line, and reading one already-open bash stream to its
// end.
//
// startSidecar is the suite's ordinary launcher and every ordinary subject uses
// it. It cannot serve the flags/env subjects, because it always passes BOTH a
// --store-socket flag and an AGENT_REPL_STORE_SOCKET env var naming the same
// path, so neither can be observed beating the other, and it treats a failed
// start as a fatal test error rather than as the CONTRACT — a bootstrap refusal
// is an exit code, a stderr record and no log file, all three of which are the
// thing under test.

// launchedSidecar is the sidecar binary run with an exact argv and environment.
//
// THE EXIT IS A CHANNEL, NOT A DURATION. `done` is closed by the single Wait
// goroutine, so every waiter observes the same exit and nothing polls for one.
type launchedSidecar struct {
	t      *testing.T
	cmd    *exec.Cmd
	stderr *bytes.Buffer
	done   chan struct{}
	err    error
	waited bool
}

// launchSidecar starts the binary with the argv and extra environment given.
//
// The environment is the ambient one plus the vendor-call ban plus `env`, and —
// unlike startSidecar — NO AGENT_REPL_STORE_SOCKET is added, so a subject decides
// for itself which spellings of the socket the process sees.
func launchSidecar(t *testing.T, args []string, env ...string) *launchedSidecar {
	t.Helper()
	cmd := exec.Command(sidecarBin, args...)
	cmd.Env = append(os.Environ(), "AGENT_REPL_FORBID_VENDOR_CALLS=1")
	cmd.Env = append(cmd.Env, env...)
	var stderr bytes.Buffer
	cmd.Stdout = &stderr
	cmd.Stderr = &stderr
	if err := cmd.Start(); err != nil {
		t.Fatalf("start sidecar %v: %v", args, err)
	}
	p := &launchedSidecar{t: t, cmd: cmd, stderr: &stderr, done: make(chan struct{})}
	go func() {
		p.err = cmd.Wait()
		close(p.done)
	}()
	t.Cleanup(p.Stop)
	return p
}

// requireCommaFreeConfigRoots refuses a root the --config-roots flag cannot
// carry.
//
// THE FLAG IS COMMA-SEPARATED, so a root whose own path contains a comma is
// split into two roots that both name nothing — and the sidecar then discovers
// no file at all, which reaches a subject as an opaque timeout on a wait for a
// cursor that was never going to arrive. Test roots live under t.TempDir(),
// whose path carries the TEST'S OWN NAME, so a subtest named with a comma in it
// produces exactly that. It is stated here, once, naming the root and the rule.
func requireCommaFreeConfigRoots(t *testing.T, roots ...string) {
	t.Helper()
	for _, root := range roots {
		if strings.Contains(root, ",") {
			t.Fatalf("config root %q contains a comma, which --config-roots reads as a separator: the sidecar would discover nothing. Test roots sit under t.TempDir(), whose path carries the test's name — rename the (sub)test so it has no comma.", root)
		}
	}
}

// sidecarFlags mirrors startSidecar's flag construction for the two flags every
// launch needs, so a launched sidecar reads the same trees an ordinary one does.
func sidecarFlags(t *testing.T, tree *vendorTree, logPath string) []string {
	t.Helper()
	requireCommaFreeConfigRoots(t, tree.Root, tree.SpoolRoot)
	return []string{
		"--state-dir", t.TempDir(),
		"--config-roots", tree.Root,
		"--spool-root", tree.SpoolRoot,
		"--log", logPath,
		"--poll-interval", (50 * time.Millisecond).String(),
		"--rescan-interval", (200 * time.Millisecond).String(),
		"--unowned-spool-window", (200 * time.Millisecond).String(),
	}
}

// AwaitExit blocks until the process leaves and answers its exit code. A process
// that has not exited by the suite's budget is a failure, never a retry.
func (p *launchedSidecar) AwaitExit(ctx context.Context) int {
	p.t.Helper()
	select {
	case <-p.done:
	case <-ctx.Done():
		p.t.Fatalf("the sidecar was still running at the deadline; its stderr was:\n%s", p.stderr.String())
	}
	p.waited = true
	var exit *exec.ExitError
	switch {
	case p.err == nil:
		return 0
	case asExitError(p.err, &exit):
		return exit.ExitCode()
	default:
		p.t.Fatalf("the sidecar could not be waited on: %v", p.err)
		return -1
	}
}

// Stderr answers everything the process wrote to stderr so far.
func (p *launchedSidecar) Stderr() string { return p.stderr.String() }

// Stop sends SIGTERM and waits, so no launched process outlives its subject.
func (p *launchedSidecar) Stop() {
	p.t.Helper()
	if p.waited {
		return
	}
	p.waited = true
	if p.cmd.Process != nil {
		_ = p.cmd.Process.Signal(syscall.SIGTERM)
	}
	select {
	case <-p.done:
	case <-time.After(waitBudget):
		if p.cmd.Process != nil {
			_ = p.cmd.Process.Kill()
		}
		<-p.done
		p.t.Fatalf("the launched sidecar did not exit within %s of SIGTERM", waitBudget)
	}
}

// asExitError reports whether err is an *exec.ExitError, writing it through.
func asExitError(err error, out **exec.ExitError) bool {
	if e, ok := err.(*exec.ExitError); ok {
		*out = e
		return true
	}
	return false
}

// ---------------------------------------------------------------------------
// Reading one ALREADY-OPEN bash stream to its terminal.
// ---------------------------------------------------------------------------

// drainBashRunToTerminal reads an open WatchBashRun stream — one whose first row
// has already been received — until the terminal row arrives, and answers every
// row in delivery order, first row included.
//
// IT IS THE FOLLOW PHASE'S OWN READER. awaitBashRunTerminal opens the stream and
// drains it in one call, which leaves a subject no instant at which to APPEND
// while the stream is open — so the boundary between the replay of stored rows
// and the live follow is never crossed under a subject's control. Splitting the
// open from the drain is what makes that boundary observable, and the stream's
// own delivery is still the only synchronization primitive.
func drainBashRunToTerminal(t *testing.T, run string, stream *connect.ServerStreamForClient[storev1.WatchBashRunResponse], first *storev1.StoreAgentBash) []*conversationv1.AgentBash {
	t.Helper()
	if got := first.GetRun().GetValue(); got != run {
		t.Fatalf("WatchBashRun(%s) sent a row for run %q", run, got)
	}
	out := []*conversationv1.AgentBash{first.GetFrame()}
	if isTerminalFrame(first.GetFrame()) {
		return out
	}
	for stream.Receive() {
		row := stream.Msg().GetRow()
		if got := row.GetRun().GetValue(); got != run {
			t.Fatalf("WatchBashRun(%s) sent a row for run %q", run, got)
		}
		out = append(out, row.GetFrame())
		if isTerminalFrame(row.GetFrame()) {
			return out
		}
	}
	if err := stream.Err(); err != nil {
		t.Fatalf("run %s: the stream failed after %d row(s) without a terminal: %v; its rows were %v",
			run, len(out), err, describeBashRows(out))
	}
	t.Fatalf("run %s: the stream ended after %d row(s) without a terminal; its rows were %v",
		run, len(out), describeBashRows(out))
	return nil
}

// ---------------------------------------------------------------------------
// Small store-side readers the process subjects share.
// ---------------------------------------------------------------------------

// cursorsForPath answers every cursor row the store holds for a path. It is the
// plural of cursorByPath on purpose: the rename subjects are about a file whose
// IDENTITY must survive its path changing, and "one row" is the assertion.
func cursorsForPath(ctx context.Context, t *testing.T, c storev1connect.ShimStoreClient, path string) []*storev1.CursorState {
	t.Helper()
	var out []*storev1.CursorState
	for _, cs := range allCursors(ctx, t, c) {
		if samePath(cs.GetPath(), path) {
			out = append(out, cs)
		}
	}
	return out
}

// cursorByFileID answers the store's cursor row for a file identity.
func cursorByFileID(ctx context.Context, t *testing.T, c storev1connect.ShimStoreClient, id string) *storev1.CursorState {
	t.Helper()
	for _, cs := range allCursors(ctx, t, c) {
		if cs.GetFileId() == id {
			return cs
		}
	}
	return nil
}

// awaitCursorForFileID waits until the store holds a cursor for a file identity
// at or past an offset, whatever path it is currently reachable by.
func awaitCursorForFileID(ctx context.Context, t *testing.T, c storev1connect.ShimStoreClient, id string, offset int64) *storev1.CursorState {
	t.Helper()
	tick := time.NewTicker(pollTick)
	defer tick.Stop()
	for {
		if cs := cursorByFileID(ctx, t, c, id); cs != nil && cs.GetOffset() >= offset {
			return cs
		}
		select {
		case <-ctx.Done():
			t.Fatalf("the cursor for file_id %s never reached offset %d within the deadline", id, offset)
		case <-tick.C:
		}
	}
}

// unitsInBook indexes the activity ids a book currently holds.
func unitsInBook(ctx context.Context, t *testing.T, c storev1connect.ShimStoreClient, agent string) map[string]int {
	t.Helper()
	seen := map[string]int{}
	for _, at := range bookLines(ctx, t, c, agent, 200) {
		if a := activityOf(at.GetLine()); a != nil {
			seen[a.GetActivityId().GetValue()]++
		}
	}
	return seen
}

// decoyEntry builds a STREAM-plane residue row on an upsert key, so a later
// write of that key by the sidecar changes the row's identity — which is the
// one thing an upsert may never do, and the store's own invalid_request.
func decoyEntry(upsertKey, writeID, source string) *storev1.StoreEntry {
	return &storev1.StoreEntry{
		Plane:     &storev1.Plane{Plane: &storev1.Plane_Stream{Stream: &storev1.PlaneStream{}}},
		WriteId:   writeID,
		UpsertKey: upsertKey,
		Entry: &storev1.StoreEntry_AgentUpdate{AgentUpdate: &storev1.StoreAgentUpdate{
			AgentInfo: &storev1.StoreAgentUpdate_UnservedItem{UnservedItem: &storev1.StoreUnservedItem{
				UnservedItem: &storev1.StoreUnservedItem_Unparsed{Unparsed: &storev1.StoreUnparsed{
					Source:     source,
					ParseError: "seeded by the suite to claim this upsert key as an unservable row",
					Raw:        "{}",
				}},
			}},
		}},
	}
}

// seedDecoyRow writes one entry to the real store as another producer, and
// fails loudly if the store did not accept it.
func seedDecoyRow(ctx context.Context, t *testing.T, c storev1connect.ShimStoreClient, producer string, entry *storev1.StoreEntry) {
	t.Helper()
	res, err := c.WriteBatch(ctx, connect.NewRequest(&storev1.WriteBatchRequest{
		// The decoy stands in for the shim's live write, so it states the
		// shim's class.
		WriteClass: &storev1.WriteClass{WriteClass: &storev1.WriteClass_Interactive{Interactive: &storev1.WriteClassInteractive{}}},
		Producer:   producer,
		Batch:      &storev1.EntryBatch{Entries: []*storev1.StoreEntry{entry}},
	}))
	if err != nil {
		t.Fatalf("seeding the decoy row: %v", err)
	}
	if f := res.Msg.GetFailure(); f != nil {
		t.Fatalf("the store refused the decoy row this subject rests on: %s", f.GetDetail())
	}
	if res.Msg.GetSuccess() == nil {
		t.Fatalf("the store answered neither arm for the decoy row")
	}
}

// renameFile moves a file on disk, which is the vendor's own rotation: the
// INODE is unchanged, so the cursor's identity survives while its path does not.
func renameFile(t *testing.T, from, to string) {
	t.Helper()
	mustMkdirAll(t, filepath.Dir(to))
	if err := os.Rename(from, to); err != nil {
		t.Fatalf("rename %s -> %s: %v", from, to, err)
	}
}

// describeCursors renders cursor rows for a failure message.
func describeCursors(rows []*storev1.CursorState) []string {
	out := make([]string, 0, len(rows))
	for _, cs := range rows {
		out = append(out, fmt.Sprintf("{file_id=%s path=%s offset=%d}", cs.GetFileId(), cs.GetPath(), cs.GetOffset()))
	}
	return out
}

// logsForOperation keeps the records of one operation.
func logsForOperation(recs []logRecord, operation string) []logRecord {
	var out []logRecord
	for _, r := range recs {
		if r.Operation == operation {
			out = append(out, r)
		}
	}
	return out
}

// contextStrings reads a context value the logger wrote as a list of strings.
func contextStrings(v any) []string {
	raw, ok := v.([]any)
	if !ok {
		return nil
	}
	out := make([]string, 0, len(raw))
	for _, item := range raw {
		out = append(out, fmt.Sprint(item))
	}
	return out
}
