package integration

import (
	"testing"

	storev1 "agentrepl/proto/store/v1"
)

// SUBJECT 1 — a real vendor transcript, ingested end to end into Agent* frames.
//
// The transcript is the one captured under modules/app/agent-repl/testdata/projects/. It
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

// TestCapturedTranscriptWritesItsUnitsInFileOrder asserts the four units are
// FIRST written in the order the file holds them: the store orders a page by
// first insert (newest first, its own contract, tested in shim-store's suite),
// so the order the sidecar first writes them in is the order a reader sees.
func TestCapturedTranscriptWritesItsUnitsInFileOrder(t *testing.T) {
	t.Parallel()
	// Arrange.
	ctx, cancel := testContext(t)
	defer cancel()
	store := startFakeStore(t)
	tree := newVendorTree(t)
	captured := loadCapturedSession(t)

	// Act.
	startSidecar(t, defaultSidecarOptions(t, store.Socket, tree))
	writeCapturedTranscript(t, tree, captured)
	lines := store.awaitBookUnits(ctx, t, captured.Session,
		// THE WAIT IS ON THE UNITS, NEVER ON A COUNT. The captured session
		// writes more page lines than these four, so "at least four lines"
		// is satisfied by a book that holds four of the OTHERS and none of
		// the units every assertion below reads.
		capturedThinking1, capturedBashCall1, capturedThinking2, capturedBashCall2)

	// Assert: Book answers lines in first-write order.
	want := []string{capturedThinking1, capturedBashCall1, capturedThinking2, capturedBashCall2}
	var got []string
	interesting := map[string]bool{}
	for _, id := range want {
		interesting[id] = true
	}
	for _, at := range lines {
		a := activityOf(at)
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
			t.Fatalf("first-write order was %v, wanted file order %v", got, want)
		}
	}
}

// TestCapturedTranscriptCarriesUsageOnOneUnitPerApiResponse asserts the usage
// rule: the FIRST content block's unit carries it and every other unit of that
// response leaves it unset.
func TestCapturedTranscriptCarriesUsageOnOneUnitPerApiResponse(t *testing.T) {
	t.Parallel()
	// Arrange.
	ctx, cancel := testContext(t)
	defer cancel()
	store := startFakeStore(t)
	tree := newVendorTree(t)
	captured := loadCapturedSession(t)

	// Act.
	startSidecar(t, defaultSidecarOptions(t, store.Socket, tree))
	writeCapturedTranscript(t, tree, captured)
	lines := store.awaitBookUnits(ctx, t, captured.Session,
		// THE WAIT IS ON THE UNITS, NEVER ON A COUNT. The captured session
		// writes more page lines than these four, so "at least four lines"
		// is satisfied by a book that holds four of the OTHERS and none of
		// the units every assertion below reads.
		capturedThinking1, capturedBashCall1, capturedThinking2, capturedBashCall2)

	// Assert.
	carriers := map[string]bool{}
	for _, at := range lines {
		a := activityOf(at)
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

// TestWithheldMachineryNeverReachesAPage asserts the CLI's bookkeeping lines
// are classified at ingest into vendor_specific rather than becoming feed rows.
//
// RE-AIMED for the 2026-09-13 residue ruling: the classification is unchanged
// and is now the whole of the record's fate, so the machinery kinds are read
// off the reader's own account of what it withheld.
func TestWithheldMachineryNeverReachesAPage(t *testing.T) {
	t.Parallel()
	// Arrange.
	ctx, cancel := testContext(t)
	defer cancel()
	fake := startFakeStore(t)
	tree := newVendorTree(t)
	captured := loadCapturedSession(t)
	// The withheld-record accounts are verbose, so the subject asks for them.
	opts := debugLogging(defaultSidecarOptions(t, fake.Socket, tree))

	// Act.
	startSidecar(t, opts)
	g := writeCapturedTranscript(t, tree, captured)
	awaitCursorInBatches(ctx, t, fake, g.Path(), g.Offset())

	// Assert: the capture's own machinery lines (two queue-operation records and
	// a last-prompt record) are withheld, and nothing modeled joined them.
	for _, want := range []string{"queue-operation", "last-prompt"} {
		awaitResidueWithheld(ctx, t, opts.LogPath, "vendor_specific/"+want)
		fake.awaitResidueShape(ctx, t, "vendor_specific/"+want)
	}
	for _, line := range linesForBook(fake.Entries(), captured.Session) {
		if frameOf(line) == nil && line.GetAgentItem().GetAgentPrompt() == nil {
			t.Errorf("a page line carries neither a frame nor a prompt: %v", line)
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

// TestUsageRidesOrdinalZeroOfAMultiLineMessage asserts the carrier is the
// message's FIRST block, wherever the vendor split the message.
func TestUsageRidesOrdinalZeroOfAMultiLineMessage(t *testing.T) {
	t.Parallel()
	// Arrange.
	ctx, cancel := testContext(t)
	defer cancel()
	fake := startFakeStore(t)
	tree := newVendorTree(t)
	captured := loadCapturedSession(t)
	cwd := "/work/ordinal-usage-probe"
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
	t.Parallel()
	// Arrange.
	ctx, cancel := testContext(t)
	defer cancel()
	fake := startFakeStore(t)
	tree := newVendorTree(t)
	captured := loadCapturedSession(t)
	cwd := "/work/ordinal-distinct-probe"
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

// TestUnmodeledAttachmentsAreWithheldAsVendorSpecific asserts the ruled
// disposition of a parsed-but-unmodeled attachment: vendor_specific with kind
// "attachment/<type>", never the `unknown` arm.
//
// RE-AIMED for the 2026-09-13 residue ruling: the disposition is unchanged and
// now ends at the classification, so the kinds are read off the reader's own
// account of what it withheld — where the `unknown` arm would equally show up
// had the attachment fallen through to it.
func TestUnmodeledAttachmentsAreWithheldAsVendorSpecific(t *testing.T) {
	t.Parallel()
	// Arrange.
	ctx, cancel := testContext(t)
	defer cancel()
	fake := startFakeStore(t)
	tree := newVendorTree(t)
	captured := loadCapturedSession(t)
	// The withheld-record accounts are verbose, so the subject asks for them.
	opts := debugLogging(defaultSidecarOptions(t, fake.Socket, tree))

	// Act.
	startSidecar(t, opts)
	g := writeCapturedTranscript(t, tree, captured)
	awaitCursorInBatches(ctx, t, fake, g.Path(), g.Offset())

	// Assert: the capture carries a deferred_tools_delta and an
	// agent_listing_delta, neither of which this contract models.
	for _, want := range []string{"attachment/deferred_tools_delta", "attachment/agent_listing_delta"} {
		awaitResidueWithheld(ctx, t, opts.LogPath, "vendor_specific/"+want)
		fake.awaitResidueShape(ctx, t, "vendor_specific/"+want)
	}
}
