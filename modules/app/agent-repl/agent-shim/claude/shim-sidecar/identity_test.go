package main

import (
	"os"
	"path/filepath"
	"strings"
	"testing"

	"agentrepl/shim-claude-sidecar/internal/identity"
)

// identity_test.go — WHICH BOOK A ROTATED TRANSCRIPT'S RECORDS LAND IN.
//
// A `/clear` mints a new vendor session id and a new transcript file, and the
// vendor's files carry no lineage between them. The shim keeps the main agent's
// AgentId exactly where it was (R9) and leaves the link the files lack at
// `<state>/shim/<workspace-key>/vendor-id/<new-id>.json`. These subjects are the
// reader's half: the rotated transcript is booked under the ORIGINAL id, so the
// store is never asked to move a row from one book to another.

const (
	// The three fixture ids, spelled as the uuids the vendor actually mints.
	bookOriginal  = "aaaaaaaa-aaaa-4aaa-8aaa-aaaaaaaaaaaa"
	bookRotated   = "bbbbbbbb-bbbb-4bbb-8bbb-bbbbbbbbbbbb"
	bookWorkspace = "0a1b2c3d"
)

// mintIdentity writes the shim's agent-id.json for a workspace, in
// engine/identity.ts's own field names.
func (h *harness) mintIdentity(t *testing.T, workspaceKey, originalID string) {
	t.Helper()
	dir := filepath.Join(h.state, "shim", workspaceKey)
	if err := os.MkdirAll(dir, 0o755); err != nil {
		t.Fatalf("creating %s: %v", dir, err)
	}
	h.write(t, filepath.Join(dir, "agent-id.json"), `{
  "original_vendor_session_id": "`+originalID+`",
  "workspace_key": "`+workspaceKey+`",
  "minted_at_ms": 1735689600000
}
`)
}

// linkVendorSession writes the pointer file a rotation leaves behind.
func (h *harness) linkVendorSession(t *testing.T, workspaceKey, vendorID, originalID string) {
	t.Helper()
	dir := filepath.Join(h.state, "shim", workspaceKey, "vendor-id")
	if err := os.MkdirAll(dir, 0o755); err != nil {
		t.Fatalf("creating %s: %v", dir, err)
	}
	h.write(t, filepath.Join(dir, vendorID+".json"), `{
  "vendor_session_id": "`+vendorID+`",
  "original_vendor_session_id": "`+originalID+`",
  "linked_at_ms": 1735689700000
}
`)
}

// TestARotatedTranscriptIsWatchedUnderTheOriginalsBook is the whole point: the
// file named by the NEW id writes to the book the conversation has always had.
func TestARotatedTranscriptIsWatchedUnderTheOriginalsBook(t *testing.T) {
	// Arrange: the shim minted an identity and then rotated it.
	h := newHarness(t, &fakeStore{})
	h.mintIdentity(t, bookWorkspace, bookOriginal)
	h.linkVendorSession(t, bookWorkspace, bookRotated, bookOriginal)
	path := h.transcript(t, bookRotated, assistantLine)

	// Act.
	if err := h.sc.beginCycle(); err != nil {
		t.Fatalf("beginCycle: %v", err)
	}

	// Assert.
	w, ok := h.sc.watchers[path]
	if !ok {
		t.Fatalf("the rotated transcript is not watched; the watchers were %v", h.sc.watchers)
	}
	if got := w.ctx.MainAgentID; got != bookOriginal {
		t.Errorf("the rotated transcript books to %q, want the conversation's original id %q", got, bookOriginal)
	}
	// The vendor session id is an ATTRIBUTE of the agent, never its address, so
	// it must still be the id the file is actually named by.
	if got := w.ctx.SessionID; got != bookRotated {
		t.Errorf("the watched context names vendor session %q, want the file's own id %q", got, bookRotated)
	}
}

// TestTheBookResolutionIsStatedOncePerTranscript: the resolution decides where
// everything that file ever produces lands, so it is a record, not a guess.
func TestTheBookResolutionIsStatedOncePerTranscript(t *testing.T) {
	// Arrange.
	h := newHarness(t, &fakeStore{})
	h.mintIdentity(t, bookWorkspace, bookOriginal)
	h.linkVendorSession(t, bookWorkspace, bookRotated, bookOriginal)
	h.transcript(t, bookRotated, assistantLine)

	// Act.
	if err := h.sc.beginCycle(); err != nil {
		t.Fatalf("beginCycle: %v", err)
	}

	// Assert.
	rec := h.requireOnce(t, "identity-resolve", "")
	if got := ctxString(t, rec, "vendor_session_id"); got != bookRotated {
		t.Errorf("the resolution names vendor session %q, want %q", got, bookRotated)
	}
	if got := ctxString(t, rec, "book_agent_id"); got != bookOriginal {
		t.Errorf("the resolution names book %q, want %q", got, bookOriginal)
	}
}

// TestAnUnlinkedTranscriptKeepsItsOwnBook: a conversation that never rotated has
// no link file, and the reader's answer for it must not change.
func TestAnUnlinkedTranscriptKeepsItsOwnBook(t *testing.T) {
	// Arrange.
	h := newHarness(t, &fakeStore{})
	h.mintIdentity(t, bookWorkspace, bookOriginal)
	path := h.transcript(t, bookOriginal, promptLine)

	// Act.
	if err := h.sc.beginCycle(); err != nil {
		t.Fatalf("beginCycle: %v", err)
	}

	// Assert.
	if got := h.sc.watchers[path].ctx.MainAgentID; got != bookOriginal {
		t.Errorf("an unrotated transcript books to %q, want its own id %q", got, bookOriginal)
	}
}

// TestATranscriptWithNoIdentityRecordKeepsItsOwnBook is the no-shim case — a
// tree the sidecar reads that no shim ever wrote a record for.
func TestATranscriptWithNoIdentityRecordKeepsItsOwnBook(t *testing.T) {
	// Arrange.
	h := newHarness(t, &fakeStore{})
	path := h.transcript(t, bookRotated, assistantLine)

	// Act.
	if err := h.sc.beginCycle(); err != nil {
		t.Fatalf("beginCycle: %v", err)
	}

	// Assert.
	if got := h.sc.watchers[path].ctx.MainAgentID; got != bookRotated {
		t.Errorf("an unrecorded transcript books to %q, want its own id %q", got, bookRotated)
	}
}

// TestALinkThatAppearsMidTailMovesTheWatchedFilesBook: discovery order is not
// causal order, so the book is re-resolved on every rescan.
func TestALinkThatAppearsMidTailMovesTheWatchedFilesBook(t *testing.T) {
	// Arrange: the transcript is watched before the link file exists.
	h := newHarness(t, &fakeStore{})
	path := h.transcript(t, bookRotated, assistantLine)
	if err := h.sc.beginCycle(); err != nil {
		t.Fatalf("beginCycle: %v", err)
	}
	if got := h.sc.watchers[path].ctx.MainAgentID; got != bookRotated {
		t.Fatalf("precondition: the file booked to %q before any link existed, want %q", got, bookRotated)
	}

	// Act: the shim rotates, and the next rescan sees it.
	h.mintIdentity(t, bookWorkspace, bookOriginal)
	h.linkVendorSession(t, bookWorkspace, bookRotated, bookOriginal)
	h.sc.rescan()

	// Assert.
	if got := h.sc.watchers[path].ctx.MainAgentID; got != bookOriginal {
		t.Errorf("after the link appeared the file books to %q, want %q", got, bookOriginal)
	}
	rec := h.requireOnce(t, "identity-rekey", "warn")
	if got := ctxString(t, rec, "book_agent_id"); got != bookOriginal {
		t.Errorf("the re-key names book %q, want %q", got, bookOriginal)
	}
}

// TestALinkThatAppearsBetweenTwoRescansMovesTheBookOnTheVeryNextPoll is the
// window a rescan-only re-key leaves open, and it is not a latency window: the
// reader polls several times per rescan, and records read in between are booked
// under the id the FILE is named by, committed, and never read again. So the
// link must be honored by the next READ, not by the next discovery pass.
func TestALinkThatAppearsBetweenTwoRescansMovesTheBookOnTheVeryNextPoll(t *testing.T) {
	// Arrange: the transcript is watched and read once before any link exists,
	// exactly as it is when a `/clear` outruns the shim's link file.
	store := &fakeStore{}
	h := newHarness(t, store)
	path := h.transcript(t, bookRotated, assistantLine)
	if err := h.sc.beginCycle(); err != nil {
		t.Fatalf("beginCycle: %v", err)
	}
	h.sc.pollAll()
	if got := bookOfLastWrite(t, store); got != bookRotated {
		t.Fatalf("precondition: with no link on disk the first records booked to %q, want %q", got, bookRotated)
	}

	// Act: the link appears and the transcript grows, with NO rescan between
	// the two — the poll tick is four times the rescan tick, so this is the
	// ordinary case rather than an exotic one.
	h.mintIdentity(t, bookWorkspace, bookOriginal)
	h.linkVendorSession(t, bookWorkspace, bookRotated, bookOriginal)
	h.write(t, path, assistantLine+"\n"+assistantLine+"\n")
	h.sc.pollAll()

	// Assert: the records this poll read landed in the original's book.
	if got := bookOfLastWrite(t, store); got != bookOriginal {
		t.Errorf("the records read after the link appeared landed in book %q, want %q", got, bookOriginal)
	}
	if got := h.sc.watchers[path].ctx.MainAgentID; got != bookOriginal {
		t.Errorf("the watched file still books to %q, want %q", got, bookOriginal)
	}
}

// TestAnUnchangedBookIsNotReKeyed: the re-key pass runs on every poll and every
// rescan, so a steady book must produce neither a move nor a record.
func TestAnUnchangedBookIsNotReKeyed(t *testing.T) {
	// Arrange.
	h := newHarness(t, &fakeStore{})
	h.mintIdentity(t, bookWorkspace, bookOriginal)
	h.transcript(t, bookOriginal, promptLine)
	if err := h.sc.beginCycle(); err != nil {
		t.Fatalf("beginCycle: %v", err)
	}

	// Act.
	h.sc.rescan()

	// Assert.
	h.requireNone(t, "identity-rekey", "warn")
	h.requireNone(t, "identity-remap", "warn")
}

// TestTheBookMoveUnparksTheFileTheStoreRefused is the refusal-turned-remap. A
// park is otherwise permanent, and rightly so; a book move is the one refusal
// that stops being true once the link file names the original.
func TestTheBookMoveUnparksTheFileTheStoreRefused(t *testing.T) {
	// Arrange: the rotated transcript is read before its link exists, and the
	// store refuses the batch for naming a book those rows are not in.
	store := &fakeStore{}
	h := newHarness(t, store)
	path := h.transcript(t, bookRotated, assistantLine)
	if err := h.sc.beginCycle(); err != nil {
		t.Fatalf("beginCycle: %v", err)
	}
	store.writeFail = "would move the row from book " + bookOriginal + " to " + bookRotated
	store.writeInvalidField = "batch.entries[0].upsert_key"
	h.sc.pollAll()
	if !h.sc.parked[path] {
		t.Fatalf("precondition: the refused file was not parked; parked=%v", h.sc.parked)
	}
	store.writeFail, store.writeInvalidField = "", ""

	// Act: the link appears and the next rescan re-resolves the book.
	h.mintIdentity(t, bookWorkspace, bookOriginal)
	h.linkVendorSession(t, bookWorkspace, bookRotated, bookOriginal)
	h.sc.rescan()

	// Assert: the file reads again, under the book the store already holds.
	if h.sc.parked[path] {
		t.Error("the file is still parked after its book moved; its cursor can never advance again")
	}
	if got := h.sc.watchers[path].ctx.MainAgentID; got != bookOriginal {
		t.Errorf("the un-parked file books to %q, want %q", got, bookOriginal)
	}
	h.requireOnce(t, "identity-remap", "warn")
}

// TestTheUnparkedFileRereadsTheSameBytesUnderTheNewBook: the cursor never moved
// past the refused batch, so the identical bytes are re-read — which is
// progress rather than a replay, because they now name the right book.
func TestTheUnparkedFileRereadsTheSameBytesUnderTheNewBook(t *testing.T) {
	// Arrange.
	store := &fakeStore{}
	h := newHarness(t, store)
	path := h.transcript(t, bookRotated, assistantLine)
	if err := h.sc.beginCycle(); err != nil {
		t.Fatalf("beginCycle: %v", err)
	}
	store.writeFail = "would move the row from book " + bookOriginal + " to " + bookRotated
	store.writeInvalidField = "batch.entries[0].upsert_key"
	h.sc.pollAll()
	store.writeFail, store.writeInvalidField = "", ""
	h.mintIdentity(t, bookWorkspace, bookOriginal)
	h.linkVendorSession(t, bookWorkspace, bookRotated, bookOriginal)
	h.sc.rescan()

	// Act.
	h.sc.pollAll()

	// Assert: the cursor advanced, which it could not do while parked.
	if got := h.sc.watchers[path].tailer.Offset(); got == 0 {
		t.Error("the un-parked file committed nothing; the same bytes must be re-read and accepted under the moved book")
	}
	if got := bookOfLastWrite(t, store); got != bookOriginal {
		t.Errorf("the re-read records landed in book %q, want %q", got, bookOriginal)
	}
}

// watchedWithNoBook arranges a claimed spool whose watcher holds NO book yet —
// the state a file watched before anything named its owner is in. A spool is
// only ever watched once claimed now, so the state is set on the watcher
// directly: what is under test is the re-key's reading of it, not how a
// watcher came to be without a book.
func watchedWithNoBook(t *testing.T, h *harness, task, call, spawner string) string {
	t.Helper()
	spool := h.spoolFile(t, task, "work whose book is not settled yet\n")
	h.sc.TaskSpawned(task, call, spawner, spool, false, "/workspace", "workspace-id", "session-1")
	if err := h.sc.beginCycle(); err != nil {
		t.Fatalf("beginCycle: %v", err)
	}
	w, ok := h.sc.watchers[spool]
	if !ok {
		t.Fatalf("precondition: the claimed spool %s is not watched", spool)
	}
	w.ctx.MainAgentID = ""
	return spool
}

// TestAFirstAttributionIsNotStatedAsABookMove is the realtest-5 record: a file
// watched with NO book at all is given one later. That is the first attribution
// of a file that had nowhere to move FROM, so it must not be stated as the
// rotation-driven book move a person has to act on.
func TestAFirstAttributionIsNotStatedAsABookMove(t *testing.T) {
	// Arrange: the spawner's book is a subagent's own identity — a tool_use_id
	// under the cross-plane minting rule, which is exactly what a spawn observed
	// inside a sidechain reports.
	h := newHarness(t, &fakeStore{})
	spool := watchedWithNoBook(t, h, "b1firstbook", "toolu_first_call", "toolu_spawning_agent")

	// Act.
	h.sc.rescan()

	// Assert.
	if got := h.sc.watchers[spool].ctx.MainAgentID; got != "toolu_spawning_agent" {
		t.Errorf("the newly attributed spool books to %q, want the spawner's book", got)
	}
	h.requireNone(t, "identity-rekey", "warn")
	rec := h.requireOnce(t, "identity-rekey", "info")
	if got := ctxString(t, rec, "book_agent_id"); got != "toolu_spawning_agent" {
		t.Errorf("the first attribution names book %q, want %q", got, "toolu_spawning_agent")
	}
}

// TestAFirstAttributionDoesNotBlameTheShimsIdentityFiles: the record that fired
// in realtest 5 said the shim's identity files "now name" the book, and they
// named nothing — the answer came from the spawn observation, and the id was a
// tool_use_id no identity file could ever hold. A reader sent hunting for a
// rotation that never happened is the defect, so the wording is the subject.
func TestAFirstAttributionDoesNotBlameTheShimsIdentityFiles(t *testing.T) {
	// Arrange.
	h := newHarness(t, &fakeStore{})
	watchedWithNoBook(t, h, "b1blame", "toolu_blame_call", "toolu_blame_agent")

	// Act.
	h.sc.rescan()

	// Assert.
	rec := h.requireOnce(t, "identity-rekey", "info")
	if strings.Contains(rec.Message, "identity files") {
		t.Errorf("the first attribution blames the shim's identity files, which named nothing: %q", rec.Message)
	}
	if !strings.Contains(rec.Message, string(identity.SourceUnrecorded)) {
		t.Errorf("the first attribution does not name where the answer came from: %q", rec.Message)
	}
}

// bookOfLastWrite answers the book the store's LAST batch named. The fake keeps
// every batch it was handed, refused ones included, so the last is the one the
// re-key produced.
func bookOfLastWrite(t *testing.T, store *fakeStore) string {
	t.Helper()
	if len(store.writes) == 0 {
		t.Fatal("the store was handed no batch at all")
	}
	for _, entry := range store.writes[len(store.writes)-1].GetEntries() {
		// `top_level` is the agent whose book the update belongs to, whatever
		// arm the update itself carries — a page line or, for a record the
		// converter has no frame for yet, durable residue.
		if book := entry.GetAgentUpdate().GetTopLevel().GetValue(); book != "" {
			return book
		}
	}
	t.Fatal("the last batch carried no agent update to read a book off")
	return ""
}

// TestAnUnrecordedResolutionNeverMovesAFileOffItsBook is the flip-flop the
// owner's log caught: 4da5f881 was booked out of 90a1151f at 11:06:53 with
// resolution source=unrecorded, and moved straight back at 11:06:55 with
// source=vendor_link. Two WARN book moves for a book that never changed.
//
// SourceUnrecorded is not a fact about the transcript. It is the R9 resume
// DEFAULT the resolver falls back to when no identity record names the id — the
// answer its own header says "goes stale the instant a rotation writes one" — so
// it may seed a book but never overrule one that evidence gave.
func TestAnUnrecordedResolutionNeverMovesAFileOffItsBook(t *testing.T) {
	cases := []struct {
		name string
		// remove is the identity record that stops answering for this id.
		remove string
	}{
		{name: "the link file stops answering", remove: filepath.Join("vendor-id", bookRotated+".json")},
		{name: "the whole identity record set stops answering", remove: "agent-id.json"},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange: the file is booked to the original on real evidence.
			h := newHarness(t, &fakeStore{})
			path := h.transcript(t, bookRotated, assistantLine)
			h.mintIdentity(t, bookWorkspace, bookOriginal)
			h.linkVendorSession(t, bookWorkspace, bookRotated, bookOriginal)
			if err := h.sc.beginCycle(); err != nil {
				t.Fatalf("beginCycle: %v", err)
			}
			if got := h.sc.watchers[path].ctx.MainAgentID; got != bookOriginal {
				t.Fatalf("precondition: the file books to %q, want the linked original %q", got, bookOriginal)
			}

			// Act: the record stops answering, so the next resolution is the
			// bare resume default.
			if err := os.Remove(filepath.Join(h.state, "shim", bookWorkspace, tc.remove)); err != nil {
				t.Fatalf("removing the identity record: %v", err)
			}
			h.sc.rescan()

			// Assert.
			if got := h.sc.watchers[path].ctx.MainAgentID; got != bookOriginal {
				t.Errorf("the file moved to book %q on an unrecorded resolution, want it to stay at %q", got, bookOriginal)
			}
			h.requireNone(t, "identity-rekey", "warn")
		})
	}
}
