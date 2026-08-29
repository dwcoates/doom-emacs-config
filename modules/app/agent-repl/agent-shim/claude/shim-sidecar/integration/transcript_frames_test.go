package integration

import (
	"testing"

	storev1 "agentrepl/proto/store/v1"
)

// SUBJECT 1 — a real vendor transcript, ingested end to end into Agent* frames.
//
// The transcript is the one captured under modules/app/agent-repl/projects/. It
// is COPIED into a vendor-shaped tree and grown line by line, so the sidecar
// reads a file that is being written rather than one that already exists whole.

// The four units the captured transcript's two API responses produce.
//
// Each assistant block is its OWN unit (agent_activity.proto: "An assistant
// message arrives as SEVERAL block units"). A tool call's unit is identified by
// the vendor's tool_use_id; every other block's is <message.id>:<block ordinal>.
//
// THE ORDINAL IS THE BLOCK'S POSITION WITHIN THE API MESSAGE, counted in FILE
// ORDER across every transcript line sharing one `message.id`, and reset when
// the id changes. The vendor writes one block per line, so a message split over
// three lines yields ordinals 0, 1, 2 — and a tool_use block CONSUMES an
// ordinal even though it is identified by its tool_use_id instead. `usage` and
// `effort` ride ordinal 0 alone.
//
// Both captured messages are thinking-then-tool_use, so their thinking units
// are ordinal 0; TestBlockOrdinalsCountAcrossTheLinesOfOneMessage covers a
// non-zero ordinal.
const (
	capturedResponse1  = "msg_011CdwKJSPRurr1J5dkq3UTJ"
	capturedResponse2  = "msg_011CdwKKaU8iPheLaxxxiqX2"
	capturedThinking1  = capturedResponse1 + ":0"
	capturedThinking2  = capturedResponse2 + ":0"
	capturedBashCall1  = "toolu_01HhE2ReMxc7nhxD3LsRs53L"
	capturedBashCall2  = "toolu_01BkZUVG3kLG2cH5A2zWBzSx"
	capturedSpoolTask1 = "bbkqcvn8k"
)

// writeCapturedTranscript copies the captured session into tree, growing it one
// fsynced line at a time, and answers the file it wrote.
func writeCapturedTranscript(t *testing.T, tree *vendorTree, captured capturedSession) *growingFile {
	t.Helper()
	g := newGrowingFile(t, tree.sessionPath(captured.Slug, captured.Session))
	for _, line := range captured.Lines {
		g.AppendLine(line)
	}
	return g
}

// TestCapturedTranscriptMainAgentBookIsKeyedByTheFileSessionUuid asserts the R9
// identity rule: the main agent's AgentId is the transcript FILE's session uuid.
func TestCapturedTranscriptMainAgentBookIsKeyedByTheFileSessionUuid(t *testing.T) {
	// Arrange.
	ctx, cancel := testContext(t)
	defer cancel()
	store := startRealStore(t)
	tree := newVendorTree(t)
	captured := loadCapturedSession(t)

	// Act.
	startSidecar(t, defaultSidecarOptions(t, store.Socket, tree))
	writeCapturedTranscript(t, tree, captured)
	lines := awaitBookLines(ctx, t, store.Client, captured.Session, 4)

	// Assert.
	for _, at := range lines {
		if got := at.GetLine().GetPageAgentId().GetValue(); got != captured.Session {
			t.Errorf("page line names book %q, wanted the file's session uuid %q", got, captured.Session)
		}
	}
}

// TestCapturedTranscriptGivesEveryAssistantBlockItsOwnUnit asserts the four
// units the two responses produce, and that nothing collapsed them into a row
// per message.
func TestCapturedTranscriptGivesEveryAssistantBlockItsOwnUnit(t *testing.T) {
	// Arrange.
	ctx, cancel := testContext(t)
	defer cancel()
	store := startRealStore(t)
	tree := newVendorTree(t)
	captured := loadCapturedSession(t)

	// Act.
	startSidecar(t, defaultSidecarOptions(t, store.Socket, tree))
	writeCapturedTranscript(t, tree, captured)
	lines := awaitBookLines(ctx, t, store.Client, captured.Session, 4)

	// Assert.
	held := map[string]bool{}
	for _, at := range lines {
		if a := activityOf(at.GetLine()); a != nil {
			held[a.GetActivityId().GetValue()] = true
		}
	}
	for _, want := range []string{capturedThinking1, capturedBashCall1, capturedThinking2, capturedBashCall2} {
		if !held[want] {
			t.Errorf("the main agent's book holds no unit %q; it holds %v", want, sortedStrings(keysOf(held)))
		}
	}
}

// TestCapturedTranscriptOrdersItsPageNewestFirst asserts the four units come
// back in reverse file order — the store's page is newest first, ordered by
// first insert.
func TestCapturedTranscriptOrdersItsPageNewestFirst(t *testing.T) {
	// Arrange.
	ctx, cancel := testContext(t)
	defer cancel()
	store := startRealStore(t)
	tree := newVendorTree(t)
	captured := loadCapturedSession(t)

	// Act.
	startSidecar(t, defaultSidecarOptions(t, store.Socket, tree))
	writeCapturedTranscript(t, tree, captured)
	lines := awaitBookLines(ctx, t, store.Client, captured.Session, 4)

	// Assert.
	want := []string{capturedBashCall2, capturedThinking2, capturedBashCall1, capturedThinking1}
	var got []string
	interesting := map[string]bool{}
	for _, id := range want {
		interesting[id] = true
	}
	for _, at := range lines {
		a := activityOf(at.GetLine())
		if a == nil {
			continue
		}
		if id := a.GetActivityId().GetValue(); interesting[id] {
			got = append(got, id)
		}
	}
	if len(got) != len(want) {
		t.Fatalf("page held %d of the four expected units: %v", len(got), got)
	}
	for i := range want {
		if got[i] != want[i] {
			t.Fatalf("page order was %v, wanted newest-first %v", got, want)
		}
	}
}

// TestCapturedTranscriptCarriesUsageOnOneUnitPerApiResponse asserts the usage
// rule: the FIRST content block's unit carries it and every other unit of that
// response leaves it unset.
func TestCapturedTranscriptCarriesUsageOnOneUnitPerApiResponse(t *testing.T) {
	// Arrange.
	ctx, cancel := testContext(t)
	defer cancel()
	store := startRealStore(t)
	tree := newVendorTree(t)
	captured := loadCapturedSession(t)

	// Act.
	startSidecar(t, defaultSidecarOptions(t, store.Socket, tree))
	writeCapturedTranscript(t, tree, captured)
	lines := awaitBookLines(ctx, t, store.Client, captured.Session, 4)

	// Assert.
	carriers := map[string]bool{}
	for _, at := range lines {
		a := activityOf(at.GetLine())
		if a != nil && a.GetUsage() != nil {
			carriers[a.GetActivityId().GetValue()] = true
		}
	}
	for _, want := range []string{capturedThinking1, capturedThinking2} {
		if !carriers[want] {
			t.Errorf("unit %q is its response's first block and must carry usage", want)
		}
	}
	for _, never := range []string{capturedBashCall1, capturedBashCall2} {
		if carriers[never] {
			t.Errorf("unit %q is not its response's first block and must leave usage unset", never)
		}
	}
	if len(carriers) != 2 {
		t.Errorf("the transcript holds two API responses, so two units carry usage; %d did: %v",
			len(carriers), sortedStrings(keysOf(carriers)))
	}
}

// TestToolResultUpsertsItsCallRatherThanAddingARow asserts a tool RETURN
// re-emits its unit's settled state under the SAME upsert_key, so the book
// holds one line for the call and the write stream holds two entries for it.
func TestToolResultUpsertsItsCallRatherThanAddingARow(t *testing.T) {
	// Arrange.
	ctx, cancel := testContext(t)
	defer cancel()
	fake := startFakeStore(t)
	tree := newVendorTree(t)
	captured := loadCapturedSession(t)

	// Act.
	startSidecar(t, defaultSidecarOptions(t, fake.Socket, tree))
	writeCapturedTranscript(t, tree, captured)
	wantKey := "activity:" + capturedBashCall1
	fake.awaitEntry(ctx, t, "the settled state of "+capturedBashCall1, func(e *storev1.StoreEntry) bool {
		return e.GetUpsertKey() == wantKey && countUpsertKey(fake.Entries(), wantKey) >= 2
	})

	// Assert.
	entries := fake.Entries()
	if n := countUpsertKey(entries, wantKey); n < 2 {
		t.Fatalf("the call and its return produced %d entries under %q, wanted at least 2", n, wantKey)
	}
	seen := map[string]int{}
	for _, line := range linesForBook(entries, captured.Session) {
		if a := activityOf(line); a != nil {
			seen[a.GetActivityId().GetValue()]++
		}
	}
	if seen[capturedBashCall1] == 0 {
		t.Fatalf("the call's unit never reached the main agent's book")
	}
	for _, line := range linesForBook(entries, captured.Session) {
		if a := activityOf(line); a != nil && a.GetActivityId().GetValue() == capturedBashCall1 {
			if a.GetBash() == nil {
				t.Errorf("unit %q is a shell call and must carry the bash item", capturedBashCall1)
			}
		}
	}
}

// TestCapturedTranscriptLandsNothingAsUnparsed is the golden-corpus contract in
// its executable form: a real capture must convert whole.
func TestCapturedTranscriptLandsNothingAsUnparsed(t *testing.T) {
	// Arrange.
	ctx, cancel := testContext(t)
	defer cancel()
	fake := startFakeStore(t)
	tree := newVendorTree(t)
	captured := loadCapturedSession(t)

	// Act.
	startSidecar(t, defaultSidecarOptions(t, fake.Socket, tree))
	g := writeCapturedTranscript(t, tree, captured)
	awaitCursorInBatches(ctx, t, fake, g.Path(), g.Offset())

	// Assert.
	residue := unparsedOf(fake.Entries())
	if len(residue) != 0 {
		for _, r := range residue {
			t.Errorf("unparsed residue at %s:%d — %s\n%s", r.GetSource(), r.GetOffset(), r.GetParseError(), r.GetRaw())
		}
		t.Fatalf("a real capture produced %d unparsed entries; a recognizable kind reaching residue is a producer defect", len(residue))
	}
}

// TestCapturedTranscriptLandsNothingAsUnknown asserts the allow-list is EMPTY:
// context cuts and api errors have carriers now, so no discriminator may reach
// the `unknown` arm.
func TestCapturedTranscriptLandsNothingAsUnknown(t *testing.T) {
	// Arrange.
	ctx, cancel := testContext(t)
	defer cancel()
	fake := startFakeStore(t)
	tree := newVendorTree(t)
	captured := loadCapturedSession(t)

	// Act.
	startSidecar(t, defaultSidecarOptions(t, fake.Socket, tree))
	g := writeCapturedTranscript(t, tree, captured)
	awaitCursorInBatches(ctx, t, fake, g.Path(), g.Offset())

	// Assert.
	var got []string
	for _, u := range unknownsOf(fake.Entries()) {
		got = append(got, u.GetDiscriminatorField()+"="+u.GetDiscriminator())
	}
	if len(got) != 0 {
		t.Fatalf("the allowed `unknown` set is empty; the capture produced %v", sortedStrings(got))
	}
}

// TestEveryWriteCarriesTheFilePlaneEnvelope asserts the producer's envelope
// duties on every entry of a real capture.
func TestEveryWriteCarriesTheFilePlaneEnvelope(t *testing.T) {
	// Arrange.
	ctx, cancel := testContext(t)
	defer cancel()
	fake := startFakeStore(t)
	tree := newVendorTree(t)
	captured := loadCapturedSession(t)

	// Act.
	startSidecar(t, defaultSidecarOptions(t, fake.Socket, tree))
	g := writeCapturedTranscript(t, tree, captured)
	awaitCursorInBatches(ctx, t, fake, g.Path(), g.Offset())

	// Assert.
	requireProducer(t, fake.Batches())
	entries := fake.Entries()
	if len(entries) == 0 {
		t.Fatalf("the sidecar wrote no entries at all")
	}
	for _, e := range entries {
		requireFilePlane(t, e)
	}
}

// TestWriteIdsAreUniqueAcrossOneIngest asserts a deterministic write_id is also
// a DISTINCT one: two entries minted from one record differ by discriminator.
func TestWriteIdsAreUniqueAcrossOneIngest(t *testing.T) {
	// Arrange.
	ctx, cancel := testContext(t)
	defer cancel()
	fake := startFakeStore(t)
	tree := newVendorTree(t)
	captured := loadCapturedSession(t)

	// Act.
	startSidecar(t, defaultSidecarOptions(t, fake.Socket, tree))
	g := writeCapturedTranscript(t, tree, captured)
	awaitCursorInBatches(ctx, t, fake, g.Path(), g.Offset())

	// Assert.
	entries := fake.Entries()
	seen := map[string]int{}
	for _, e := range entries {
		seen[e.GetWriteId()]++
	}
	for id, n := range seen {
		if n != 1 {
			t.Errorf("write_id %q was minted %d times in one ingest", id, n)
		}
	}
}

// TestKeepAliveTurnsNeverReachAPage asserts a keep-alive-marked turn's records
// land as unserved keepalive items and never as page lines.
func TestKeepAliveTurnsNeverReachAPage(t *testing.T) {
	// Arrange.
	ctx, cancel := testContext(t)
	defer cancel()
	fake := startFakeStore(t)
	tree := newVendorTree(t)
	captured := loadCapturedSession(t)
	cwd := "/Users/dodgecoates/keepalive-probe"
	slug := cwdSlug(cwd)
	session := "11111111-1111-4111-8111-111111111111"

	prompt := setUserText(t,
		retargetSession(t, decodeRecord(t, captured.Lines[3]), session, cwd),
		keepaliveMarker+"cache ping")
	// The assistant work produced while the bit is set: the captured response's
	// thinking line and its Bash call, re-pointed at this session.
	thinking := retargetSession(t, decodeRecord(t, captured.Lines[7]), session, cwd)
	call := retargetSession(t, decodeRecord(t, captured.Lines[8]), session, cwd)

	// Act.
	startSidecar(t, defaultSidecarOptions(t, fake.Socket, tree))
	g := newGrowingFile(t, tree.sessionPath(slug, session))
	g.AppendLine(encodeRecord(t, prompt))
	g.AppendLine(encodeRecord(t, thinking))
	g.AppendLine(encodeRecord(t, call))
	awaitCursorInBatches(ctx, t, fake, g.Path(), g.Offset())

	// Assert.
	entries := fake.Entries()
	if len(keepalivesOf(entries)) == 0 {
		t.Fatalf("nothing landed on the keepalive arm; every record of a marked turn must")
	}
	for _, line := range linesForBook(entries, session) {
		if a := activityOf(line); a != nil {
			t.Errorf("unit %q reached a page line during a keep-alive turn", a.GetActivityId().GetValue())
		}
	}
}

// TestKeepAliveEndsAtTheNextOrdinaryPrompt asserts the bit is cleared by the
// next non-keepalive user prompt, so work after it is served again.
func TestKeepAliveEndsAtTheNextOrdinaryPrompt(t *testing.T) {
	// Arrange.
	ctx, cancel := testContext(t)
	defer cancel()
	fake := startFakeStore(t)
	tree := newVendorTree(t)
	captured := loadCapturedSession(t)
	cwd := "/Users/dodgecoates/keepalive-end-probe"
	slug := cwdSlug(cwd)
	session := "22222222-2222-4222-8222-222222222222"

	base := decodeRecord(t, captured.Lines[3])
	marked := setUserText(t, retargetSession(t, base, session, cwd), keepaliveMarker+"cache ping")
	ordinary := setUserText(t, retargetSession(t, base, session, cwd), "now do the real thing")
	thinking := retargetSession(t, decodeRecord(t, captured.Lines[12]), session, cwd)
	call := retargetSession(t, decodeRecord(t, captured.Lines[13]), session, cwd)

	// Act.
	startSidecar(t, defaultSidecarOptions(t, fake.Socket, tree))
	g := newGrowingFile(t, tree.sessionPath(slug, session))
	g.AppendLine(encodeRecord(t, marked))
	g.AppendLine(encodeRecord(t, ordinary))
	g.AppendLine(encodeRecord(t, thinking))
	g.AppendLine(encodeRecord(t, call))
	awaitCursorInBatches(ctx, t, fake, g.Path(), g.Offset())

	// Assert.
	held := map[string]bool{}
	for _, line := range linesForBook(fake.Entries(), session) {
		if a := activityOf(line); a != nil {
			held[a.GetActivityId().GetValue()] = true
		}
	}
	if !held[capturedBashCall2] {
		t.Fatalf("work after the keep-alive turn ended must be served; the book holds %v",
			sortedStrings(keysOf(held)))
	}
}

// TestWithheldMachineryNeverReachesAPage asserts the CLI's bookkeeping lines
// are classified at ingest into vendor_specific rather than becoming feed rows.
func TestWithheldMachineryNeverReachesAPage(t *testing.T) {
	// Arrange.
	ctx, cancel := testContext(t)
	defer cancel()
	fake := startFakeStore(t)
	tree := newVendorTree(t)
	captured := loadCapturedSession(t)

	// Act.
	startSidecar(t, defaultSidecarOptions(t, fake.Socket, tree))
	g := writeCapturedTranscript(t, tree, captured)
	awaitCursorInBatches(ctx, t, fake, g.Path(), g.Offset())

	// Assert: the capture's own machinery lines (two queue-operation records and
	// a last-prompt record) are withheld, and nothing modeled joined them.
	kinds := vendorSpecificKinds(fake.Entries())
	for _, want := range []string{"queue-operation", "last-prompt"} {
		if !containsString(kinds, want) {
			t.Errorf("machinery kind %q was not withheld; withheld kinds were %v", want, kinds)
		}
	}
	for _, line := range linesForBook(fake.Entries(), captured.Session) {
		if frameOf(line) == nil && line.GetAgentItem().GetAgentPrompt() == nil {
			t.Errorf("a page line carries neither a frame nor a prompt: %v", line)
		}
	}
}

// TestFilePlaneUserPromptIsWithheldRatherThanServed asserts R15: a user record
// with prose is never a page line, because TurnId and PromptOrigin are the
// daemon's to mint.
func TestFilePlaneUserPromptIsWithheldRatherThanServed(t *testing.T) {
	// Arrange.
	ctx, cancel := testContext(t)
	defer cancel()
	fake := startFakeStore(t)
	tree := newVendorTree(t)
	captured := loadCapturedSession(t)

	// Act.
	startSidecar(t, defaultSidecarOptions(t, fake.Socket, tree))
	g := writeCapturedTranscript(t, tree, captured)
	awaitCursorInBatches(ctx, t, fake, g.Path(), g.Offset())

	// Assert.
	entries := fake.Entries()
	if !containsString(vendorSpecificKinds(entries), vendorSpecificUserPrompt) {
		t.Errorf("the capture's user prompt was not withheld as vendor_specific{kind:%q}; kinds were %v",
			vendorSpecificUserPrompt, vendorSpecificKinds(entries))
	}
	for _, line := range pageLinesOf(entries) {
		if line.GetAgentItem().GetAgentPrompt() != nil {
			t.Errorf("the file plane minted an AgentPrompt page line, which is the shim's alone: %v", line)
		}
	}
}

// countUpsertKey counts how many entries were written under one upsert_key.
func countUpsertKey(entries []*storev1.StoreEntry, key string) int {
	n := 0
	for _, e := range entries {
		if e.GetUpsertKey() == key {
			n++
		}
	}
	return n
}

func keysOf(m map[string]bool) []string {
	out := make([]string, 0, len(m))
	for k := range m {
		out = append(out, k)
	}
	return out
}

// TestAPageLineReachesAWatcherAsItIsWritten asserts the store's standing tail
// carries the sidecar's rows live — the stream frame IS the delivery signal a
// consumer waits on, so the file plane's writes are watchable, not merely
// pollable.
func TestAPageLineReachesAWatcherAsItIsWritten(t *testing.T) {
	// Arrange.
	ctx, cancel := testContext(t)
	defer cancel()
	store := startRealStore(t)
	tree := newVendorTree(t)
	captured := loadCapturedSession(t)

	startSidecar(t, defaultSidecarOptions(t, store.Socket, tree))
	g := newGrowingFile(t, tree.sessionPath(captured.Slug, captured.Session))
	for _, line := range captured.Lines[:4] {
		g.AppendLine(line)
	}
	awaitBookLines(ctx, t, store.Client, captured.Session, 1)
	_, tail := watchBook(ctx, t, store.Client, captured.Session, 200)

	// Act: the rest of the file arrives after the watch was opened.
	for _, line := range captured.Lines[4:] {
		g.AppendLine(line)
	}

	// Assert: the last response's Bash call reaches the watcher.
	for {
		select {
		case at, ok := <-tail:
			if !ok {
				t.Fatalf("the watch stream ended before %q arrived", capturedBashCall2)
			}
			if a := activityOf(at.GetLine()); a != nil && a.GetActivityId().GetValue() == capturedBashCall2 {
				if at.GetAt().GetValue() == "" {
					t.Errorf("a streamed line carries no pointer, so a caller has no reconnect mark")
				}
				return
			}
		case <-ctx.Done():
			t.Fatalf("%q never reached the watcher within the deadline", capturedBashCall2)
		}
	}
}

// TestBlockOrdinalsCountAcrossTheLinesOfOneMessage asserts the ordinal is the
// block's position within the API MESSAGE, not within the transcript line: a
// message written over three lines yields ordinals 0, 1 and 2, and the tool_use
// block consumes one without being named by it.
func TestBlockOrdinalsCountAcrossTheLinesOfOneMessage(t *testing.T) {
	// Arrange: one API message split over three real fixture lines —
	// thinking (ordinal 0), prose (ordinal 1), a tool call (ordinal 2).
	ctx, cancel := testContext(t)
	defer cancel()
	fake := startFakeStore(t)
	tree := newVendorTree(t)
	captured := loadCapturedSession(t)
	cwd := "/Users/dodgecoates/block-ordinal-probe"
	slug := cwdSlug(cwd)
	session := "0a0a0a0a-0a0a-40a0-80a0-0a0a0a0a0a0a"
	message := capturedResponse1

	thinking := retargetSession(t, decodeRecord(t, captured.Lines[7]), session, cwd)
	prose := setMessageID(t,
		retargetSession(t, decodeRecord(t, corpusLine(t, "content-blocks/text.jsonl", 0)), session, cwd),
		message)
	call := retargetSession(t, decodeRecord(t, captured.Lines[8]), session, cwd)

	// Act.
	startSidecar(t, defaultSidecarOptions(t, fake.Socket, tree))
	g := newGrowingFile(t, tree.sessionPath(slug, session))
	g.AppendLine(encodeRecord(t, thinking))
	g.AppendLine(encodeRecord(t, prose))
	g.AppendLine(encodeRecord(t, call))
	awaitCursorInBatches(ctx, t, fake, g.Path(), g.Offset())

	// Assert.
	held := map[string]bool{}
	for _, line := range linesForBook(fake.Entries(), session) {
		if a := activityOf(line); a != nil {
			held[a.GetActivityId().GetValue()] = true
		}
	}
	for _, want := range []string{message + ":0", message + ":1", capturedBashCall1} {
		if !held[want] {
			t.Errorf("unit %q is missing; the book holds %v", want, sortedStrings(keysOf(held)))
		}
	}
	if held[message+":2"] {
		t.Errorf("the tool_use block consumed ordinal 2 but must be identified by its tool_use_id, not %q", message+":2")
	}
}

// TestUsageRidesOrdinalZeroOfAMultiLineMessage asserts the carrier is the
// message's FIRST block, wherever the vendor split the message.
func TestUsageRidesOrdinalZeroOfAMultiLineMessage(t *testing.T) {
	// Arrange.
	ctx, cancel := testContext(t)
	defer cancel()
	fake := startFakeStore(t)
	tree := newVendorTree(t)
	captured := loadCapturedSession(t)
	cwd := "/Users/dodgecoates/ordinal-usage-probe"
	slug := cwdSlug(cwd)
	session := "0b0b0b0b-0b0b-40b0-80b0-0b0b0b0b0b0b"
	message := capturedResponse1

	thinking := retargetSession(t, decodeRecord(t, captured.Lines[7]), session, cwd)
	prose := setMessageID(t,
		retargetSession(t, decodeRecord(t, corpusLine(t, "content-blocks/text.jsonl", 0)), session, cwd),
		message)
	call := retargetSession(t, decodeRecord(t, captured.Lines[8]), session, cwd)

	// Act.
	startSidecar(t, defaultSidecarOptions(t, fake.Socket, tree))
	g := newGrowingFile(t, tree.sessionPath(slug, session))
	g.AppendLine(encodeRecord(t, thinking))
	g.AppendLine(encodeRecord(t, prose))
	g.AppendLine(encodeRecord(t, call))
	awaitCursorInBatches(ctx, t, fake, g.Path(), g.Offset())

	// Assert: exactly one unit of the message carries usage, and it is ordinal 0.
	var carriers []string
	for _, line := range linesForBook(fake.Entries(), session) {
		a := activityOf(line)
		if a != nil && a.GetUsage() != nil {
			carriers = append(carriers, a.GetActivityId().GetValue())
		}
	}
	if len(carriers) != 1 {
		t.Fatalf("one API message yields ONE usage carrier; %d units carried it: %v", len(carriers), sortedStrings(carriers))
	}
	if carriers[0] != message+":0" {
		t.Errorf("usage rode unit %q, wanted the message's first block %q", carriers[0], message+":0")
	}
}

// TestNoTwoUnitsOfOneMessageShareAnActivityId asserts the ordinal actually
// discriminates: every block of one API message is a distinct unit.
func TestNoTwoUnitsOfOneMessageShareAnActivityId(t *testing.T) {
	// Arrange.
	ctx, cancel := testContext(t)
	defer cancel()
	fake := startFakeStore(t)
	tree := newVendorTree(t)
	captured := loadCapturedSession(t)
	cwd := "/Users/dodgecoates/ordinal-distinct-probe"
	slug := cwdSlug(cwd)
	session := "0c0c0c0c-0c0c-40c0-80c0-0c0c0c0c0c0c"
	message := capturedResponse1

	thinking := retargetSession(t, decodeRecord(t, captured.Lines[7]), session, cwd)
	proseA := setMessageID(t,
		retargetSession(t, decodeRecord(t, corpusLine(t, "content-blocks/text.jsonl", 0)), session, cwd),
		message)
	proseB := setMessageID(t,
		retargetSession(t, decodeRecord(t, corpusLine(t, "content-blocks/text.jsonl", 0)), session, cwd),
		message)

	// Act: two prose blocks of ONE message would collide under any per-line rule.
	startSidecar(t, defaultSidecarOptions(t, fake.Socket, tree))
	g := newGrowingFile(t, tree.sessionPath(slug, session))
	g.AppendLine(encodeRecord(t, thinking))
	g.AppendLine(encodeRecord(t, proseA))
	g.AppendLine(encodeRecord(t, proseB))
	awaitCursorInBatches(ctx, t, fake, g.Path(), g.Offset())

	// Assert.
	counts := map[string]int{}
	for _, line := range linesForBook(fake.Entries(), session) {
		if a := activityOf(line); a != nil {
			counts[a.GetActivityId().GetValue()]++
		}
	}
	for id, n := range counts {
		if n != 1 {
			t.Errorf("unit %q appears %d times; two blocks of one message collided onto one identity", id, n)
		}
	}
	for _, want := range []string{message + ":1", message + ":2"} {
		if counts[want] == 0 {
			t.Errorf("unit %q is missing; the ordinal must keep counting across the message's lines. Book: %v",
				want, sortedStrings(keysOf(toSet(counts))))
		}
	}
}

// TestOrdinalsResetWhenTheMessageIdChanges asserts the ordinal is scoped to one
// API message: the next message starts again at zero.
func TestOrdinalsResetWhenTheMessageIdChanges(t *testing.T) {
	// Arrange.
	ctx, cancel := testContext(t)
	defer cancel()
	fake := startFakeStore(t)
	tree := newVendorTree(t)
	captured := loadCapturedSession(t)
	cwd := "/Users/dodgecoates/ordinal-reset-probe"
	slug := cwdSlug(cwd)
	session := "0d0d0d0d-0d0d-40d0-80d0-0d0d0d0d0d0d"

	// Act: the captured file's two messages, each thinking-then-tool_use.
	startSidecar(t, defaultSidecarOptions(t, fake.Socket, tree))
	g := newGrowingFile(t, tree.sessionPath(slug, session))
	for _, i := range []int{7, 8, 12, 13} {
		g.AppendLine(encodeRecord(t, retargetSession(t, decodeRecord(t, captured.Lines[i]), session, cwd)))
	}
	awaitCursorInBatches(ctx, t, fake, g.Path(), g.Offset())

	// Assert: the SECOND message's thinking block is ordinal 0, not 2.
	held := map[string]bool{}
	for _, line := range linesForBook(fake.Entries(), session) {
		if a := activityOf(line); a != nil {
			held[a.GetActivityId().GetValue()] = true
		}
	}
	if !held[capturedThinking2] {
		t.Errorf("unit %q is missing; the ordinal must reset at a new message id. Book: %v",
			capturedThinking2, sortedStrings(keysOf(held)))
	}
	if held[capturedResponse2+":2"] {
		t.Errorf("the ordinal carried over from the previous message into %q", capturedResponse2+":2")
	}
}

// TestUnmodeledAttachmentsAreWithheldAsVendorSpecific asserts the ruled
// disposition of a parsed-but-unmodeled attachment: vendor_specific with kind
// "attachment/<type>", never the `unknown` arm.
func TestUnmodeledAttachmentsAreWithheldAsVendorSpecific(t *testing.T) {
	// Arrange.
	ctx, cancel := testContext(t)
	defer cancel()
	fake := startFakeStore(t)
	tree := newVendorTree(t)
	captured := loadCapturedSession(t)

	// Act.
	startSidecar(t, defaultSidecarOptions(t, fake.Socket, tree))
	g := writeCapturedTranscript(t, tree, captured)
	awaitCursorInBatches(ctx, t, fake, g.Path(), g.Offset())

	// Assert: the capture carries a deferred_tools_delta and an
	// agent_listing_delta, neither of which this contract models.
	kinds := vendorSpecificKinds(fake.Entries())
	for _, want := range []string{"attachment/deferred_tools_delta", "attachment/agent_listing_delta"} {
		if !containsString(kinds, want) {
			t.Errorf("unmodeled attachment kind %q was not withheld; withheld kinds were %v", want, kinds)
		}
	}
}
