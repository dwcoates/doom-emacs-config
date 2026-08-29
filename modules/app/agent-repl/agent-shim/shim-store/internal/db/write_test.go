package db

import (
	"errors"
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"
	storev1 "agentrepl/proto/store/v1"
	"google.golang.org/protobuf/proto"
)

// ---- the routing arms ----

func TestWriteBatchLandsAnUpdateAsAPageLineInItsBook(t *testing.T) {
	// Arrange
	d, _ := newStore(t)
	entry := pageEntry("w1", "u1", "agent-1", frameItem(activityFrame("agent-1", "act-1", prose())))

	// Act
	result := writeOK(t, d, entry)

	// Assert
	if result.Written != 1 || len(result.Lines) != 1 {
		t.Fatalf("result = %+v, want one written line", result)
	}
	if result.Lines[0].AgentID != "agent-1" {
		t.Fatalf("line book = %q, want agent-1", result.Lines[0].AgentID)
	}
	if got := scalar[string](t, d, `SELECT kind FROM entry WHERE upsert_key = 'u1'`); got != kindPageLine {
		t.Fatalf("kind = %q, want %q", got, kindPageLine)
	}
	if got := scalar[string](t, d, `SELECT book_agent_id FROM entry WHERE upsert_key = 'u1'`); got != "agent-1" {
		t.Fatalf("book = %q, want agent-1", got)
	}
}

func TestWriteBatchLandsASuccessAsBothAPageLineAndTheAgentTerminal(t *testing.T) {
	// Arrange: the feed's stop notice has no other source, and the agent's
	// terminal state has no other source either, so one write is both.
	d, _ := newStore(t)

	// Act
	result := writeOK(t, d, pageEntry("w1", "u1", "agent-1", frameItem(successFrame("agent-1"))))

	// Assert
	if len(result.Lines) != 1 {
		t.Fatalf("lines = %d, want 1", len(result.Lines))
	}
	if got := scalar[int64](t, d, `SELECT ended_at_ms FROM agent WHERE agent_id = 'agent-1'`); got != testNow {
		t.Fatalf("ended_at_ms = %d, want %d", got, testNow)
	}
	if got := scalar[int](t, d, `SELECT LENGTH(terminal) FROM agent WHERE agent_id = 'agent-1'`); got == 0 {
		t.Fatal("agent terminal was not recorded")
	}
}

func TestWriteBatchLandsAFailureAsBothAPageLineAndTheAgentTerminal(t *testing.T) {
	// Arrange
	d, _ := newStore(t)

	// Act
	result := writeOK(t, d, pageEntry("w1", "u1", "agent-1", frameItem(failureFrame("agent-1"))))

	// Assert
	if len(result.Lines) != 1 {
		t.Fatalf("lines = %d, want 1", len(result.Lines))
	}
	if got := scalar[int64](t, d, `SELECT ended_at_ms FROM agent WHERE agent_id = 'agent-1'`); got != testNow {
		t.Fatalf("ended_at_ms = %d, want %d", got, testNow)
	}
}

func TestWriteBatchLandsAPromptAsAPageLineAndNothingElse(t *testing.T) {
	// Arrange
	d, _ := newStore(t)

	// Act
	result := writeOK(t, d, pageEntry("w1", "u1", "agent-1", promptItem("agent-1")))

	// Assert
	if len(result.Lines) != 1 {
		t.Fatalf("lines = %d, want 1", len(result.Lines))
	}
	if got := scalar[int](t, d, `SELECT COUNT(*) FROM detached_work`); got != 0 {
		t.Fatalf("detached_work rows = %d, want 0", got)
	}
	if got := scalar[int](t, d, `SELECT COUNT(*) FROM agent WHERE ended_at_ms IS NOT NULL`); got != 0 {
		t.Fatalf("ended agents = %d, want 0", got)
	}
}

func TestWriteBatchCreatesTheAgentRowASubagentStartAnnounces(t *testing.T) {
	// Arrange: the spawn is a page line of the CALLER's book, and the created
	// agent's own row is what makes its book addressable before it speaks.
	d, _ := newStore(t)
	entry := pageEntry("w1", "u1", "agent-1", frameItem(activityFrame("agent-1", "act-1", subagentStart("agent-2"))))

	// Act
	writeOK(t, d, entry)

	// Assert
	if got := scalar[string](t, d, `SELECT spawned_by_agent FROM agent WHERE agent_id = 'agent-2'`); got != "agent-1" {
		t.Fatalf("spawned_by_agent = %q, want agent-1", got)
	}
	if got := scalar[int64](t, d, `SELECT started_at_ms FROM agent WHERE agent_id = 'agent-2'`); got != 42 {
		t.Fatalf("started_at_ms = %d, want the spawn's own instant 42", got)
	}
	if got := scalar[string](t, d, `SELECT isolation FROM agent WHERE agent_id = 'agent-2'`); got != "worktree" {
		t.Fatalf("isolation = %q, want worktree", got)
	}
}

func TestWriteBatchLandsADetachedSubagentAnnouncementInTheLifecycleTableOnly(t *testing.T) {
	// Arrange
	d, _ := newStore(t)
	entry := pageEntry("w1", "u1", "agent-1", frameItem(detachedFrame("agent-1", createdWork("work-1", subagentWork("agent-2")))))

	// Act
	result := writeOK(t, d, entry)

	// Assert: never a page line — the spawning call already is one.
	if len(result.Lines) != 0 {
		t.Fatalf("lines = %d, want 0", len(result.Lines))
	}
	if got := scalar[string](t, d, `SELECT kind FROM detached_work WHERE work_id = 'work-1'`); got != detachedKindSubagent {
		t.Fatalf("kind = %q, want %q", got, detachedKindSubagent)
	}
	if got := scalar[int](t, d, `SELECT COUNT(*) FROM agent WHERE agent_id = 'agent-2'`); got != 1 {
		t.Fatal("the created agent's row was not ensured")
	}
}

func TestWriteBatchRecordsEachDetachableWorkKind(t *testing.T) {
	// Arrange
	tests := []struct {
		name string
		work *conversationv1.DetachableWork
		want string
	}{
		{name: "subagent", work: subagentWork("agent-2"), want: detachedKindSubagent},
		{name: "bash", work: bashWork(), want: detachedKindBash},
		{name: "workflow", work: workflowWork(), want: detachedKindWorkflow},
		{name: "monitor", work: monitorWork(), want: detachedKindMonitor},
	}
	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			d, _ := newStore(t)
			entry := pageEntry("w1", "u1", "agent-1", frameItem(detachedFrame("agent-1", createdWork("work-1", test.work))))

			// Act
			writeOK(t, d, entry)

			// Assert
			if got := scalar[string](t, d, `SELECT kind FROM detached_work WHERE work_id = 'work-1'`); got != test.want {
				t.Fatalf("kind = %q, want %q", got, test.want)
			}
			if got := scalar[string](t, d, `SELECT cause FROM detached_work WHERE work_id = 'work-1'`); got != causeCreated {
				t.Fatalf("cause = %q, want %q", got, causeCreated)
			}
		})
	}
}

func TestWriteBatchRecordsTheDetachCauseAndItsTimeout(t *testing.T) {
	// Arrange: the figure is the CONFIGURED limit, not the work's runtime.
	d, _ := newStore(t)
	work := detachedWork("work-1", "act-1", &conversationv1.DetachedWorkDetached_TimedOut{
		TimedOut: &conversationv1.DetachedCauseTimedOut{TimeoutMs: 30_000},
	})
	entry := pageEntry("w1", "u1", "agent-1", frameItem(detachedFrame("agent-1", work)))

	// Act
	writeOK(t, d, entry)

	// Assert
	if got := scalar[string](t, d, `SELECT cause FROM detached_work WHERE work_id = 'work-1'`); got != causeTimedOut {
		t.Fatalf("cause = %q, want %q", got, causeTimedOut)
	}
	if got := scalar[int64](t, d, `SELECT timeout_ms FROM detached_work WHERE work_id = 'work-1'`); got != 30_000 {
		t.Fatalf("timeout_ms = %d, want 30000", got)
	}
	if got := scalar[string](t, d, `SELECT origin_unit FROM detached_work WHERE work_id = 'work-1'`); got != "act-1" {
		t.Fatalf("origin_unit = %q, want act-1", got)
	}
}

func TestWriteBatchRecordsTheSpoolAndItsReadability(t *testing.T) {
	// Arrange: an unreadable spool is a path shown for the record; a readable
	// one is something a surface may offer to follow, so the two are stored
	// apart rather than inferred from the path.
	d, _ := newStore(t)
	work := withOutput(createdWork("work-1", bashWork()), &conversationv1.DetachedWorkOutput{
		Path:        "/tmp/spool.log",
		Readability: &conversationv1.DetachedWorkOutput_Readable{Readable: &conversationv1.DetachedWorkOutputReadable{}},
	})
	entry := pageEntry("w1", "u1", "agent-1", frameItem(detachedFrame("agent-1", work)))

	// Act
	writeOK(t, d, entry)

	// Assert
	if got := scalar[string](t, d, `SELECT output_path FROM detached_work WHERE work_id = 'work-1'`); got != "/tmp/spool.log" {
		t.Fatalf("output_path = %q", got)
	}
	if got := scalar[bool](t, d, `SELECT output_readable FROM detached_work WHERE work_id = 'work-1'`); !got {
		t.Fatal("output_readable = false, want true")
	}
}

func TestWriteBatchLandsABashRunFrameAgainstItsRunAndNeverAsAPageLine(t *testing.T) {
	// Arrange
	d, _ := newStore(t)

	// Act
	result := writeOK(t, d, bashEntry("w1", "u1", "run-1", bashStart()))

	// Assert
	if len(result.Lines) != 0 {
		t.Fatalf("lines = %d, want 0 — the shell CALL is the page line", len(result.Lines))
	}
	if got := scalar[string](t, d, `SELECT kind FROM detached_work WHERE work_id = 'run-1'`); got != detachedKindBash {
		t.Fatalf("kind = %q, want %q", got, detachedKindBash)
	}
	if got := scalar[int](t, d, `SELECT COUNT(*) FROM detached_work WHERE work_id = 'run-1' AND ended_at_ms IS NULL`); got != 1 {
		t.Fatal("a started run was not left live")
	}
}

func TestWriteBatchEndsABashRunOnItsTerminalFrame(t *testing.T) {
	// Arrange
	d, _ := newStore(t)
	writeOK(t, d, bashEntry("w1", "u1", "run-1", bashStart()))

	// Act
	writeOK(t, d, bashEntry("w2", "u1", "run-1", bashSuccess()))

	// Assert
	if got := scalar[int64](t, d, `SELECT ended_at_ms FROM detached_work WHERE work_id = 'run-1'`); got != testNow {
		t.Fatalf("ended_at_ms = %d, want %d", got, testNow)
	}
}

func TestWriteBatchClosesDetachedWorkWhenItsOriginUnitConcludes(t *testing.T) {
	// Arrange: the run was announced as detached FROM an in-turn unit, and the
	// terminal for that unit arriving on the spawning stream is what ends it.
	d, _ := newStore(t)
	work := detachedWork("work-1", "act-1", causeRequestedArm())
	writeOK(t, d, pageEntry("w1", "u1", "agent-1", frameItem(detachedFrame("agent-1", work))))

	// Act
	writeOK(t, d, pageEntry("w2", "u2", "agent-1", frameItem(activityFrame("agent-1", "act-1", bashSuccess()))))

	// Assert
	if got := scalar[int64](t, d, `SELECT ended_at_ms FROM detached_work WHERE work_id = 'work-1'`); got != testNow {
		t.Fatalf("ended_at_ms = %d, want %d", got, testNow)
	}
}

func TestWriteBatchLeavesDetachedWorkOpenForANonTerminalOriginFrame(t *testing.T) {
	// Arrange
	d, _ := newStore(t)
	work := detachedWork("work-1", "act-1", causeRequestedArm())
	writeOK(t, d, pageEntry("w1", "u1", "agent-1", frameItem(detachedFrame("agent-1", work))))

	// Act
	writeOK(t, d, pageEntry("w2", "u2", "agent-1", frameItem(activityFrame("agent-1", "act-1", bashStart()))))

	// Assert
	if got := scalar[int](t, d, `SELECT COUNT(*) FROM detached_work WHERE work_id = 'work-1' AND ended_at_ms IS NULL`); got != 1 {
		t.Fatal("a non-terminal frame on the origin unit closed the detached row")
	}
}

func TestWriteBatchNeverDowngradesADetachedRowsKind(t *testing.T) {
	// Arrange: the bash arm classified the run; the announcement's `detached`
	// origin carries no kind at all and must not overwrite what is known.
	d, _ := newStore(t)
	writeOK(t, d, bashEntry("w1", "u1", "run-1", bashStart()))

	// Act
	work := detachedWork("run-1", "act-1", causeRequestedArm())
	writeOK(t, d, pageEntry("w2", "u2", "agent-1", frameItem(detachedFrame("agent-1", work))))

	// Assert
	if got := scalar[string](t, d, `SELECT kind FROM detached_work WHERE work_id = 'run-1'`); got != detachedKindBash {
		t.Fatalf("kind = %q, want %q", got, detachedKindBash)
	}
}

func TestWriteBatchLandsAnUnservedItemWithNoBook(t *testing.T) {
	// Arrange
	tests := []struct {
		name string
		item *storev1.StoreUnservedItem
		kind string
	}{
		{name: "keepalive", item: &storev1.StoreUnservedItem{UnservedItem: &storev1.StoreUnservedItem_Keepalive{Keepalive: promptItem("agent-1")}}, kind: kindKeepalive},
		{name: "vendor specific", item: &storev1.StoreUnservedItem{UnservedItem: &storev1.StoreUnservedItem_VendorSpecific{VendorSpecific: &storev1.StoreVendorSpecific{Kind: "hook"}}}, kind: kindVendorSpecific},
		{name: "unknown", item: &storev1.StoreUnservedItem{UnservedItem: &storev1.StoreUnservedItem_Unknown{Unknown: &storev1.StoreUnknown{Discriminator: "widget"}}}, kind: kindUnknown},
		{name: "unparsed", item: &storev1.StoreUnservedItem{UnservedItem: &storev1.StoreUnservedItem_Unparsed{Unparsed: &storev1.StoreUnparsed{Source: "t.jsonl"}}}, kind: kindUnparsed},
	}
	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			d, _ := newStore(t)

			// Act
			result := writeOK(t, d, unservedEntry("w1", "u1", test.item))

			// Assert
			if len(result.Lines) != 0 {
				t.Fatalf("lines = %d, want 0", len(result.Lines))
			}
			if got := scalar[int](t, d, `SELECT COUNT(*) FROM entry WHERE upsert_key = 'u1' AND book_agent_id IS NULL`); got != 1 {
				t.Fatal("the never-served row carries a book")
			}
			if got := scalar[string](t, d, `SELECT kind FROM entry WHERE upsert_key = 'u1'`); got != test.kind {
				t.Fatalf("kind = %q, want %q", got, test.kind)
			}
		})
	}
}

func TestWriteBatchLandsASessionUpdateWithNoBookAndNoTopLevel(t *testing.T) {
	// Arrange
	d, _ := newStore(t)

	// Act
	result := writeOK(t, d, sessionUpdateEntry("w1", "u1"))

	// Assert
	if len(result.Lines) != 0 {
		t.Fatalf("lines = %d, want 0", len(result.Lines))
	}
	if got := scalar[int](t, d, `SELECT COUNT(*) FROM entry WHERE upsert_key = 'u1' AND book_agent_id IS NULL AND top_level IS NULL AND kind = 'session_update'`); got != 1 {
		t.Fatal("the session update row is not a never-served, unattributed row")
	}
}

func TestWriteBatchStoresAWorkflowFrameDurablyAndWarnsItIsNotImplemented(t *testing.T) {
	// Arrange: nothing unconvertible is dropped — it lands whole, and the
	// warning is the only thing that says why nothing serves it.
	d, s := newStore(t)

	// Act
	result := writeOK(t, d, workflowRunEntry("w1", "u1", "run-agent-1"))

	// Assert
	if len(result.Lines) != 0 {
		t.Fatalf("lines = %d, want 0", len(result.Lines))
	}
	if got := scalar[string](t, d, `SELECT kind FROM entry WHERE upsert_key = 'u1'`); got != kindWorkflow {
		t.Fatalf("kind = %q, want %q", got, kindWorkflow)
	}
	if got := scalar[int](t, d, `SELECT COUNT(*) FROM workflow`); got != 0 {
		t.Fatalf("workflow rows = %d, want 0 — nothing is routed into that table this wave", got)
	}
	s.assertLogged(t, "warn", "workflow ingestion not implemented this wave")
}

// ---- absorption, orderings, cursor, rollback ----

func TestWriteBatchAbsorbsAReplayedWriteId(t *testing.T) {
	// Arrange: absorption IS success, and the producer retires the batch
	// either way.
	d, _ := newStore(t)
	entry := pageEntry("w1", "u1", "agent-1", frameItem(activityFrame("agent-1", "act-1", prose())))
	writeOK(t, d, entry)

	// Act
	result := writeOK(t, d, proto.Clone(entry).(*storev1.StoreEntry))

	// Assert
	if result.Absorbed != 1 || result.Written != 0 || len(result.Lines) != 0 {
		t.Fatalf("result = %+v, want one absorbed and nothing written", result)
	}
	if got := scalar[int](t, d, `SELECT COUNT(*) FROM entry`); got != 1 {
		t.Fatalf("entry rows = %d, want 1", got)
	}
}

func TestWriteBatchLeavesTheWriteOrdinalAloneForAnAbsorbedWrite(t *testing.T) {
	// Arrange: bumping it would re-deliver an unchanged line to every live
	// watcher on every retry.
	d, _ := newStore(t)
	entry := pageEntry("w1", "u1", "agent-1", frameItem(activityFrame("agent-1", "act-1", prose())))
	writeOK(t, d, entry)
	before := scalar[int64](t, d, `SELECT write_seq FROM entry WHERE upsert_key = 'u1'`)

	// Act
	writeOK(t, d, proto.Clone(entry).(*storev1.StoreEntry))

	// Assert
	if after := scalar[int64](t, d, `SELECT write_seq FROM entry WHERE upsert_key = 'u1'`); after != before {
		t.Fatalf("write_seq moved from %d to %d on an absorbed replay", before, after)
	}
}

func TestWriteBatchUpsertSupersedesTheRowWholeAndKeepsItsPosition(t *testing.T) {
	// Arrange: order is by FIRST insert, which is what makes a served pointer
	// survive the unit settling.
	d, _ := newStore(t)
	writeOK(t, d, pageEntry("w1", "u1", "agent-1", frameItem(activityFrame("agent-1", "act-1", prose()))))
	writeOK(t, d, pageEntry("w2", "u2", "agent-1", frameItem(activityFrame("agent-1", "act-2", prose()))))
	position := scalar[int64](t, d, `SELECT position FROM entry WHERE upsert_key = 'u1'`)

	// Act: the same unit settles.
	writeOK(t, d, pageEntry("w3", "u1", "agent-1", frameItem(activityFrame("agent-1", "act-1", bashSuccess()))))

	// Assert
	if got := scalar[int64](t, d, `SELECT position FROM entry WHERE upsert_key = 'u1'`); got != position {
		t.Fatalf("position moved from %d to %d on an upsert", position, got)
	}
	if got := scalar[string](t, d, `SELECT write_id FROM entry WHERE upsert_key = 'u1'`); got != "w3" {
		t.Fatalf("write_id = %q, want the superseding write", got)
	}
	if got := scalar[int](t, d, `SELECT COUNT(*) FROM entry`); got != 2 {
		t.Fatalf("entry rows = %d, want 2 — an upsert replaces, never appends", got)
	}
}

func TestWriteBatchBumpsTheWriteOrdinalOnAnUpsert(t *testing.T) {
	// Arrange: the upsert is NEW INFORMATION for a watcher even though the row
	// keeps its place, so the pin must move past it.
	d, _ := newStore(t)
	writeOK(t, d, pageEntry("w1", "u1", "agent-1", frameItem(activityFrame("agent-1", "act-1", prose()))))
	writeOK(t, d, pageEntry("w2", "u2", "agent-1", frameItem(activityFrame("agent-1", "act-2", prose()))))
	before := scalar[int64](t, d, `SELECT write_seq FROM entry WHERE upsert_key = 'u1'`)

	// Act
	writeOK(t, d, pageEntry("w3", "u1", "agent-1", frameItem(activityFrame("agent-1", "act-1", bashSuccess()))))

	// Assert
	after := scalar[int64](t, d, `SELECT write_seq FROM entry WHERE upsert_key = 'u1'`)
	if after <= before {
		t.Fatalf("write_seq = %d after an upsert, want more than %d", after, before)
	}
	if global := scalar[int64](t, d, `SELECT MAX(write_seq) FROM entry`); global != after {
		t.Fatalf("the upsert did not take the global maximum: %d vs %d", after, global)
	}
}

func TestWriteBatchAdvancesTheCursorInTheSameTransaction(t *testing.T) {
	// Arrange: the exactly-once contract IS the co-commit.
	d, _ := newStore(t)
	entry := pageEntry("w1", "u1", "agent-1", frameItem(activityFrame("agent-1", "act-1", prose())))

	// Act
	_, err := d.WriteBatch(ctx(), "sidecar", &storev1.EntryBatch{
		Entries: []*storev1.StoreEntry{entry},
		CursorAdvance: &storev1.CursorState{
			FileId: "12:34", Path: "/t/a.jsonl", Offset: 4096, Carry: []byte("half a line"),
		},
	})

	// Assert
	if err != nil {
		t.Fatalf("WriteBatch: %v", err)
	}
	if got := scalar[int64](t, d, `SELECT offset FROM cursor WHERE file_id = '12:34'`); got != 4096 {
		t.Fatalf("offset = %d, want 4096", got)
	}
	if got := scalar[int](t, d, `SELECT COUNT(*) FROM entry`); got != 1 {
		t.Fatalf("entry rows = %d, want 1", got)
	}
}

func TestWriteBatchAcceptsACursorOnlyBatch(t *testing.T) {
	// Arrange: a sidecar that read only unconvertible bytes still advances.
	d, _ := newStore(t)

	// Act
	result, err := d.WriteBatch(ctx(), "sidecar", &storev1.EntryBatch{
		CursorAdvance: &storev1.CursorState{FileId: "12:34", Path: "/t/a.jsonl", Offset: 10},
	})

	// Assert
	if err != nil {
		t.Fatalf("WriteBatch: %v", err)
	}
	if result.Written != 0 {
		t.Fatalf("written = %d, want 0", result.Written)
	}
	if got := scalar[int64](t, d, `SELECT offset FROM cursor WHERE file_id = '12:34'`); got != 10 {
		t.Fatalf("offset = %d, want 10", got)
	}
}

func TestWriteBatchCommitsNothingWhenALaterEntryIsInvalid(t *testing.T) {
	// Arrange: failure means NOTHING committed — the transaction fails whole,
	// which is what lets the producer keep the batch in memory alone.
	d, s := newStore(t)
	good := pageEntry("w1", "u1", "agent-1", frameItem(activityFrame("agent-1", "act-1", prose())))
	bad := pageEntry("w2", "u2", "", promptItem("agent-1"))

	// Act
	_, err := d.WriteBatch(ctx(), "shim", &storev1.EntryBatch{
		Entries:       []*storev1.StoreEntry{good, bad},
		CursorAdvance: &storev1.CursorState{FileId: "12:34", Path: "/t/a.jsonl", Offset: 99},
	})

	// Assert
	if !errors.Is(err, ErrInvalid) {
		t.Fatalf("error = %v, want ErrInvalid", err)
	}
	if got := scalar[int](t, d, `SELECT COUNT(*) FROM entry`); got != 0 {
		t.Fatalf("entry rows = %d, want 0", got)
	}
	if got := scalar[int](t, d, `SELECT COUNT(*) FROM cursor`); got != 0 {
		t.Fatalf("cursor rows = %d, want 0 — the cursor moved on a failed batch", got)
	}
	s.assertLogged(t, "error", "refused")
}

func TestWriteBatchLeavesTheCursorUnchangedWhenTheBatchFails(t *testing.T) {
	// Arrange: an ALREADY-ADVANCED cursor must not roll forward on a batch
	// that failed, or the sidecar skips the records it never landed.
	d, _ := newStore(t)
	if _, err := d.WriteBatch(ctx(), "sidecar", &storev1.EntryBatch{
		CursorAdvance: &storev1.CursorState{FileId: "12:34", Path: "/t/a.jsonl", Offset: 10},
	}); err != nil {
		t.Fatalf("seed: %v", err)
	}

	// Act
	_, err := d.WriteBatch(ctx(), "sidecar", &storev1.EntryBatch{
		Entries:       []*storev1.StoreEntry{pageEntry("w1", "u1", "", promptItem("agent-1"))},
		CursorAdvance: &storev1.CursorState{FileId: "12:34", Path: "/t/a.jsonl", Offset: 999},
	})

	// Assert
	if err == nil {
		t.Fatal("WriteBatch accepted an invalid entry")
	}
	if got := scalar[int64](t, d, `SELECT offset FROM cursor WHERE file_id = '12:34'`); got != 10 {
		t.Fatalf("offset = %d, want the unchanged 10", got)
	}
}

func TestWriteBatchRefusesAnEmptyProducer(t *testing.T) {
	// Arrange
	d, s := newStore(t)

	// Act
	_, err := d.WriteBatch(ctx(), "", batch(pageEntry("w1", "u1", "agent-1", promptItem("agent-1"))))

	// Assert
	if !errors.Is(err, ErrInvalid) {
		t.Fatalf("error = %v, want ErrInvalid", err)
	}
	s.assertLogged(t, "error", "producer is empty")
}

func TestWriteBatchRefusesAnUnsetBatch(t *testing.T) {
	// Arrange
	d, s := newStore(t)

	// Act
	_, err := d.WriteBatch(ctx(), "shim", nil)

	// Assert
	if !errors.Is(err, ErrInvalid) {
		t.Fatalf("error = %v, want ErrInvalid", err)
	}
	s.assertLogged(t, "error", "batch is unset")
}

func TestWriteBatchRefusesABatchThatCarriesNothing(t *testing.T) {
	// Arrange
	d, s := newStore(t)

	// Act
	_, err := d.WriteBatch(ctx(), "shim", &storev1.EntryBatch{})

	// Assert
	if !errors.Is(err, ErrInvalid) {
		t.Fatalf("error = %v, want ErrInvalid", err)
	}
	s.assertLogged(t, "error", "neither entries nor a cursor advance")
}

func TestWriteBatchRefusesACursorWithNoFileIdentity(t *testing.T) {
	// Arrange: a path-keyed cursor would restart a renamed file from zero.
	d, s := newStore(t)

	// Act
	_, err := d.WriteBatch(ctx(), "sidecar", &storev1.EntryBatch{
		CursorAdvance: &storev1.CursorState{Path: "/t/a.jsonl", Offset: 1},
	})

	// Assert
	if !errors.Is(err, ErrInvalid) {
		t.Fatalf("error = %v, want ErrInvalid", err)
	}
	s.assertLogged(t, "error", "cursor_advance.file_id is empty")
}

func TestWriteBatchRefusesACursorWithNoPath(t *testing.T) {
	// Arrange
	d, _ := newStore(t)

	// Act
	_, err := d.WriteBatch(ctx(), "sidecar", &storev1.EntryBatch{
		CursorAdvance: &storev1.CursorState{FileId: "12:34", Offset: 1},
	})

	// Assert
	if !errors.Is(err, ErrInvalid) {
		t.Fatalf("error = %v, want ErrInvalid", err)
	}
}

func TestWriteBatchRefusesANegativeCursorOffset(t *testing.T) {
	// Arrange
	d, _ := newStore(t)

	// Act
	_, err := d.WriteBatch(ctx(), "sidecar", &storev1.EntryBatch{
		CursorAdvance: &storev1.CursorState{FileId: "12:34", Path: "/t/a.jsonl", Offset: -1},
	})

	// Assert
	if !errors.Is(err, ErrInvalid) {
		t.Fatalf("error = %v, want ErrInvalid", err)
	}
}

func TestWriteBatchReportsAStorageFailureOnAClosedDatabase(t *testing.T) {
	// Arrange
	d, s := newStore(t)
	if err := d.Close(); err != nil {
		t.Fatalf("close: %v", err)
	}

	// Act
	_, err := d.WriteBatch(ctx(), "shim", batch(pageEntry("w1", "u1", "agent-1", promptItem("agent-1"))))

	// Assert
	if !errors.Is(err, ErrStorage) {
		t.Fatalf("error = %v, want ErrStorage", err)
	}
	s.assertLogged(t, "error", "refused")
}

func TestWriteBatchCorrelatesTheRefusalWithTheOffendingWrite(t *testing.T) {
	// Arrange: the identifiers ride dedicated context keys, never the message
	// text, so the integration loop can query them.
	d, s := newStore(t)

	// Act
	_, err := d.WriteBatch(ctx(), "shim", batch(pageEntry("w1", "u1", "", promptItem("agent-1"))))

	// Assert
	if err == nil {
		t.Fatal("WriteBatch accepted a page line with no book")
	}
	s.assertContext(t, "write_id", "w1")
	s.assertContext(t, "upsert_key", "u1")
	s.assertContext(t, "producer", "shim")
}

func TestWriteBatchLeavesAnEndedAgentEndedWhenAnUpdateArrivesAfterIt(t *testing.T) {
	// Arrange: only a success or a failure ends an agent, and an update
	// arriving afterwards is an out-of-order write, never a resurrection. A
	// store that re-opened the record here would report finished work as live
	// forever after, and GetLiveWork would hand the shim an obligation that
	// can never be resolved.
	d, _ := newStore(t)
	writeOK(t, d, pageEntry("w1", "u1", "agent-1", frameItem(successFrame("agent-1"))))

	// Act
	writeOK(t, d, pageEntry("w2", "u2", "agent-1", frameItem(activityFrame("agent-1", "act-1", prose()))))

	// Assert
	if got := scalar[int64](t, d, `SELECT ended_at_ms FROM agent WHERE agent_id = 'agent-1'`); got != testNow {
		t.Fatalf("ended_at_ms = %d, want the terminal's %d", got, testNow)
	}
}

func TestWriteBatchKeepsTheFirstSightInstantOfAnAgentThatSpeaksAgain(t *testing.T) {
	// Arrange: started_at_ms is when the store first heard of the agent, and a
	// later frame overwriting it would move an agent's start forward every
	// time it spoke.
	d, _ := newStore(t)
	writeOK(t, d, pageEntry("w1", "u1", "agent-1", frameItem(activityFrame("agent-1", "act-1", prose()))))
	first := scalar[int64](t, d, `SELECT started_at_ms FROM agent WHERE agent_id = 'agent-1'`)

	// Act
	writeOK(t, d, pageEntry("w2", "u2", "agent-1", frameItem(activityFrame("agent-1", "act-2", prose()))))

	// Assert
	if got := scalar[int64](t, d, `SELECT started_at_ms FROM agent WHERE agent_id = 'agent-1'`); got != first {
		t.Fatalf("started_at_ms moved from %d to %d", first, got)
	}
}

func TestWriteBatchStoresTheAgentFrameItselfAsTheTerminal(t *testing.T) {
	// Arrange: a terminal column is read back as CONVERSATION vocabulary, so
	// it holds the AgentFrame rather than the storage envelope around it — a
	// reader stripping an envelope off it would be reading the datalayer's
	// model to recover the protocol's.
	d, _ := newStore(t)

	// Act
	writeOK(t, d, pageEntry("w1", "u1", "agent-1", frameItem(successFrame("agent-1"))))

	// Assert
	blob := scalar[[]byte](t, d, `SELECT terminal FROM agent WHERE agent_id = 'agent-1'`)
	frame := &conversationv1.AgentFrame{}
	if err := proto.Unmarshal(blob, frame); err != nil {
		t.Fatalf("terminal is not an AgentFrame: %v", err)
	}
	if frame.GetAgentId().GetValue() != "agent-1" || frame.GetSuccess() == nil {
		t.Fatalf("terminal frame = %v", frame)
	}
}

func TestWriteBatchStoresTheBashFrameItselfAsTheLatestState(t *testing.T) {
	// Arrange
	d, _ := newStore(t)

	// Act
	writeOK(t, d, bashEntry("w1", "u1", "run-1", bashStart()))

	// Assert
	blob := scalar[[]byte](t, d, `SELECT latest_state FROM detached_work WHERE work_id = 'run-1'`)
	frame := &conversationv1.AgentBash{}
	if err := proto.Unmarshal(blob, frame); err != nil {
		t.Fatalf("latest_state is not an AgentBash: %v", err)
	}
	if frame.GetStart() == nil {
		t.Fatalf("latest_state frame = %v", frame)
	}
}
