package integration

import (
	"testing"
)

// SUBJECT — a keep-alive turn that was IN PROGRESS when the process died.
//
// NOTHING OF A KEEP-ALIVE IS STORED, and whether a record is a keep-alive's is
// read off the transcript's own links: its promptId, or the parent it names
// (convert/keepalive.go). A restarted reader resumes at the store's cursor, and
// the boot rewind moves it back to the LAST turn start — and when another
// prompt landed between the keep-alive's prompt and its reply, that turn start
// is the other prompt, so the keep-alive's prompt is never re-read. What keeps
// the answer is the PRIME: the reader hands the converter every byte before the
// first frame it delivers (tail.Primer), classified by the same rule.
//
// WITHOUT THAT, THE KEEP-ALIVE'S REPLY IS SERVED after a crash — the case
// nobody is watching, and the one the marker exists to prevent.
//
// The store is REAL so the cursor genuinely survives the restart, and a proxy in
// front of it records the entries both runs wrote.

// restartProbe is one subject's arrangement: a file, a real store behind a
// recording proxy, and the captured lines re-pointed at the subject's session.
type restartProbe struct {
	store    *realStore
	proxy    *proxyStore
	opts     sidecarOptions
	g        *growingFile
	captured capturedSession
	session  string
	cwd      string
}

func newRestartProbe(t *testing.T, cwd, session string) *restartProbe {
	t.Helper()
	store := startRealStore(t)
	proxy := startProxyStore(t, store.Socket)
	tree := newVendorTree(t)
	return &restartProbe{
		store: store, proxy: proxy, opts: defaultSidecarOptions(t, proxy.Socket, tree),
		g:        newGrowingFile(t, tree.sessionPath(cwdSlug(cwd), session)),
		captured: loadCapturedSession(t), session: session, cwd: cwd,
	}
}

// line re-points captured line i at this probe's session.
func (p *restartProbe) line(t *testing.T, i int) map[string]any {
	t.Helper()
	return retargetSession(t, decodeRecord(t, p.captured.Lines[i]), p.session, p.cwd)
}

// prompt is the captured prompt with `text`, as a turn of its own.
func (p *restartProbe) prompt(t *testing.T, text, uuid, promptID string) map[string]any {
	t.Helper()
	return asOwnPrompt(t, setUserText(t, p.line(t, 3), text), uuid, promptID)
}

// storedUnits is every unit id the two runs wrote as a page line.
func (p *restartProbe) storedUnits() map[string]bool {
	units := map[string]bool{}
	for _, line := range pageLinesOf(entriesOf(p.proxy.Batches())) {
		if a := activityOf(line); a != nil {
			units[a.GetActivityId().GetValue()] = true
		}
	}
	return units
}

// runAcrossARestart writes `before`, lets the first process store it, stops
// it, writes `after` while nothing reads, and restarts from the store's cursor.
func (p *restartProbe) runAcrossARestart(t *testing.T, before, after []map[string]any) {
	t.Helper()
	ctx, cancel := testContext(t)
	defer cancel()
	first := startSidecar(t, p.opts)
	appendRecords(t, p.g, before...)
	awaitCursorAtLeast(ctx, t, p.store.Client, p.g.Path(), p.g.Offset())
	first.Stop()
	appendRecords(t, p.g, after...)
	startSidecar(t, p.opts)
	awaitCursorAtLeast(ctx, t, p.store.Client, p.g.Path(), p.g.Offset())
}

// TestAKeepAliveTurnInProgressAtRestartStoresNothing stops the sidecar mid
// keep-alive turn, appends the rest of the turn's work, restarts, and asserts
// neither run stored anything of it.
func TestAKeepAliveTurnInProgressAtRestartStoresNothing(t *testing.T) {
	t.Parallel()
	// Arrange.
	p := newRestartProbe(t, "/work/keepalive-restart-probe", "f4f4f4f4-f4f4-4f4f-8f4f-f4f4f4f4f4f4")
	turn := chained(t,
		setUserText(t, p.line(t, 3), keepaliveMarker+"cache ping"),
		p.line(t, 7), p.line(t, 8), p.line(t, 12), p.line(t, 13))

	// Act.
	p.runAcrossARestart(t, turn[:2], turn[2:])

	// Assert.
	if entries := entriesOf(p.proxy.Batches()); len(entries) != 0 {
		t.Fatalf("a keep-alive turn spanning a restart stored %d entrie(s) (keys %v), want none",
			len(entries), upsertKeysOf(entries))
	}
}

// TestAKeepAliveReplyBehindAnInterleavedPromptStaysUnstoredAcrossARestart
// drives the case the prime exists for: a real prompt lands between the
// keep-alive's prompt and its reply, the process dies, and the reply is written
// after the restart. The boot rewind stops at the real prompt, so only the
// prefix says the reply is the keep-alive's.
func TestAKeepAliveReplyBehindAnInterleavedPromptStaysUnstoredAcrossARestart(t *testing.T) {
	t.Parallel()
	// Arrange.
	p := newRestartProbe(t, "/work/keepalive-interleave-probe", "f5f5f5f5-f5f5-4f5f-8f5f-f5f5f5f5f5f5")
	keepalive := setUserText(t, p.line(t, 3), keepaliveMarker+"cache ping")
	real := chained(t,
		p.prompt(t, "the real question", "f5f5f5f5-0000-4000-8000-000000000001", "f5f5f5f5-0000-4000-8000-0000000000aa"),
		p.line(t, 12), p.line(t, 13))
	reply := chained(t, keepalive, p.line(t, 7), p.line(t, 8))[1:]

	// Act.
	p.runAcrossARestart(t, append([]map[string]any{keepalive}, real...), reply)

	// Assert.
	units := p.storedUnits()
	if units[capturedBashCall1] {
		t.Errorf("the keep-alive's own call %s was stored after the restart; its prompt predates the rewind point", capturedBashCall1)
	}
	if !units[capturedBashCall2] {
		t.Errorf("the real prompt's call %s was not stored; stored units %v", capturedBashCall2, sortedStrings(keysOf(units)))
	}
}

// TestAKeepAliveReplyAfterATaskNotificationStaysUnstoredAcrossARestart drives
// a background task's notification landing between the keep-alive's send and
// its reply, with the process dying in between: the notification is a turn of
// its own, and the reply is still the keep-alive's.
func TestAKeepAliveReplyAfterATaskNotificationStaysUnstoredAcrossARestart(t *testing.T) {
	t.Parallel()
	// Arrange.
	p := newRestartProbe(t, "/work/keepalive-notification-probe", "f6f6f6f6-f6f6-4f6f-8f6f-f6f6f6f6f6f6")
	keepalive := setUserText(t, p.line(t, 3), keepaliveMarker+"cache ping")
	notification := withFields(t,
		p.prompt(t, "<task-notification>a background task finished</task-notification>",
			"f6f6f6f6-0000-4000-8000-000000000001", "f6f6f6f6-0000-4000-8000-0000000000aa"),
		map[string]any{"origin": map[string]any{"kind": "task-notification"}})
	before := chained(t, keepalive, notification)
	reply := chained(t, keepalive, p.line(t, 7), p.line(t, 8))[1:]

	// Act.
	p.runAcrossARestart(t, before, reply)

	// Assert.
	if units := p.storedUnits(); units[capturedBashCall1] {
		t.Errorf("the keep-alive's own call %s was stored after the restart; a notification between its prompt and its reply is a turn of its own",
			capturedBashCall1)
	}
}
