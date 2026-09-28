package db

import (
	"context"
	"errors"
	"fmt"
	"path/filepath"
	"sync"
	"testing"
	"time"

	conversationv1 "agentrepl/proto/conversation/v1"
	storev1 "agentrepl/proto/store/v1"
	"agentrepl/shim-store/internal/logging"
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
	// The spawn's own instant is 42 and is deliberately NOT what lands here:
	// started_at_ms is the STORE's clock at first sight, because LiveWork
	// orders by it and the spawn frame's instant is the shim's or the vendor's.
	if got := scalar[int64](t, d, `SELECT started_at_ms FROM agent WHERE agent_id = 'agent-2'`); got != testNow {
		t.Fatalf("started_at_ms = %d, want the store's own clock %d", got, testNow)
	}
	if got := scalar[string](t, d, `SELECT isolation FROM agent WHERE agent_id = 'agent-2'`); got != "worktree" {
		t.Fatalf("isolation = %q, want worktree", got)
	}
}

func TestWriteBatchLandsADetachedSubagentAnnouncementAsAPageLine(t *testing.T) {
	// Arrange: "work left this stream" is the HANDOFF, and a book that omitted
	// it would keep claiming work that is no longer in the turn.
	d, _ := newStore(t)
	entry := pageEntry("w1", "u1", "agent-1", frameItem(detachedFrame("agent-1", createdWork("work-1", subagentWork("agent-2")))))

	// Act
	result := writeOK(t, d, entry)

	// Assert
	if len(result.Lines) != 1 {
		t.Fatalf("lines = %d, want 1", len(result.Lines))
	}
	if result.Lines[0].AgentID != "agent-1" {
		t.Fatalf("line book = %q, want the ANNOUNCING agent's book", result.Lines[0].AgentID)
	}
}

func TestWriteBatchAlsoWritesTheJoinRowADetachedAnnouncementNames(t *testing.T) {
	// Arrange: the page line is what is SERVED; the join row is what
	// GetLiveWork scans and what a terminal closes.
	d, _ := newStore(t)
	entry := pageEntry("w1", "u1", "agent-1", frameItem(detachedFrame("agent-1", createdWork("work-1", subagentWork("agent-2")))))

	// Act
	writeOK(t, d, entry)

	// Assert
	if got := scalar[string](t, d, `SELECT kind FROM detached_work WHERE work_id = 'work-1'`); got != detachedKindSubagent {
		t.Fatalf("kind = %q, want %q", got, detachedKindSubagent)
	}
	if got := scalar[int](t, d, `SELECT COUNT(*) FROM agent WHERE agent_id = 'agent-2'`); got != 1 {
		t.Fatal("the created agent's row was not ensured")
	}
}

func TestWriteBatchReAnnouncementUpsertsTheJoinRowRatherThanDuplicatingIt(t *testing.T) {
	// Arrange: the handle is the row's identity, so a producer that replays an
	// announcement under a new write_id still names one run.
	d, _ := newStore(t)
	first := pageEntry("w1", "detached:work-1", "agent-1", frameItem(detachedFrame("agent-1", createdWork("work-1", bashWork()))))
	second := pageEntry("w2", "detached:work-1", "agent-1", frameItem(detachedFrame("agent-1", createdWork("work-1", bashWork()))))
	writeOK(t, d, first)

	// Act
	writeOK(t, d, second)

	// Assert
	if got := scalar[int](t, d, `SELECT COUNT(*) FROM detached_work WHERE work_id = 'work-1'`); got != 1 {
		t.Fatalf("detached_work rows = %d, want 1", got)
	}
	if got := scalar[int](t, d, `SELECT COUNT(*) FROM entry WHERE upsert_key = 'detached:work-1'`); got != 1 {
		t.Fatalf("entry rows = %d, want 1 — the announcement supersedes its own row", got)
	}
}

func TestWriteBatchKeepsTheAnnouncementItselfOutOfTheLifecycleTable(t *testing.T) {
	// Arrange: the spool path, the readability, the cause and the timeout live
	// in the served page line and NOWHERE ELSE. Unpacking them here as well
	// would give one fact two homes that can disagree.
	d, _ := newStore(t)
	work := withOutput(createdWork("work-1", bashWork()), &conversationv1.DetachedWorkOutput{
		Path:        "/tmp/spool.log",
		Readability: &conversationv1.DetachedWorkOutput_Readable{Readable: &conversationv1.DetachedWorkOutputReadable{}},
	})
	entry := pageEntry("w1", "u1", "agent-1", frameItem(detachedFrame("agent-1", work)))

	// Act
	result := writeOK(t, d, entry)

	// Assert: the served line still carries the spool, and the join row holds
	// only join columns.
	served := result.Lines[0].Line.GetLine().GetAgentItem().GetAgentFrame().GetDetachedWork()
	if got := served.GetOutput().GetPath(); got != "/tmp/spool.log" {
		t.Fatalf("the served announcement's spool path = %q, want /tmp/spool.log", got)
	}
	columns := scalar[int](t, d, `SELECT COUNT(*) FROM pragma_table_info('detached_work')`)
	if columns != 7 {
		t.Fatalf("detached_work has %d columns, want the 7 join columns only", columns)
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
		})
	}
}

func TestWriteBatchRecordsTheOriginUnitADetachedAnnouncementNames(t *testing.T) {
	// Arrange: origin_unit is the JOIN — the one indexed lookup a terminal on
	// the spawning stream closes this row through. The detach CAUSE and its
	// timeout are conversation content and stay in the served page line.
	d, _ := newStore(t)
	work := detachedWork("work-1", "act-1", &conversationv1.DetachedWorkDetached_TimedOut{
		TimedOut: &conversationv1.DetachedCauseTimedOut{TimeoutMs: 30_000},
	})
	entry := pageEntry("w1", "u1", "agent-1", frameItem(detachedFrame("agent-1", work)))

	// Act
	result := writeOK(t, d, entry)

	// Assert
	if got := scalar[string](t, d, `SELECT origin_unit FROM detached_work WHERE work_id = 'work-1'`); got != "act-1" {
		t.Fatalf("origin_unit = %q, want act-1", got)
	}
	served := result.Lines[0].Line.GetLine().GetAgentItem().GetAgentFrame().GetDetachedWork()
	if got := served.GetDetached().GetTimedOut().GetTimeoutMs(); got != 30_000 {
		t.Fatalf("the served announcement's timeout = %d, want 30000", got)
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
		{name: "vendor specific", item: &storev1.StoreUnservedItem{UnservedItem: &storev1.StoreUnservedItem_VendorSpecific{VendorSpecific: &storev1.StoreVendorSpecific{Kind: "hook", Raw: rawRecord("hook")}}}, kind: kindVendorSpecific},
		{name: "unknown", item: &storev1.StoreUnservedItem{UnservedItem: &storev1.StoreUnservedItem_Unknown{Unknown: &storev1.StoreUnknown{Discriminator: "widget", Raw: rawRecord("widget")}}}, kind: kindUnknown},
		{name: "unparsed", item: &storev1.StoreUnservedItem{UnservedItem: &storev1.StoreUnservedItem_Unparsed{Unparsed: &storev1.StoreUnparsed{Source: "t.jsonl", Raw: "{\"broken\":"}}}, kind: kindUnparsed},
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
	_, err := d.WriteBatch(ctx(), "sidecar", WriteInteractive, &storev1.EntryBatch{
		Entries: []*storev1.StoreEntry{entry},
		CursorAdvance: &storev1.CursorState{
			FileId: "12:34", Path: "/t/a.jsonl", Offset: 4096, Carry: []byte("half a line"), Conversion: currentConversion(),
		},
	}, nil)

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
	result, err := d.WriteBatch(ctx(), "sidecar", WriteInteractive, &storev1.EntryBatch{
		CursorAdvance: &storev1.CursorState{FileId: "12:34", Path: "/t/a.jsonl", Offset: 10, Conversion: currentConversion()},
	}, nil)

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
	_, err := d.WriteBatch(ctx(), "shim", WriteInteractive, &storev1.EntryBatch{
		Entries:       []*storev1.StoreEntry{good, bad},
		CursorAdvance: &storev1.CursorState{FileId: "12:34", Path: "/t/a.jsonl", Offset: 99, Conversion: currentConversion()},
	}, nil)

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
	s.assertTracedRefusal(t, "refused")
}

func TestWriteBatchLeavesTheCursorUnchangedWhenTheBatchFails(t *testing.T) {
	// Arrange: an ALREADY-ADVANCED cursor must not roll forward on a batch
	// that failed, or the sidecar skips the records it never landed.
	d, _ := newStore(t)
	if _, err := d.WriteBatch(ctx(), "sidecar", WriteInteractive, &storev1.EntryBatch{
		CursorAdvance: &storev1.CursorState{FileId: "12:34", Path: "/t/a.jsonl", Offset: 10, Conversion: currentConversion()},
	}, nil); err != nil {
		t.Fatalf("seed: %v", err)
	}

	// Act
	_, err := d.WriteBatch(ctx(), "sidecar", WriteInteractive, &storev1.EntryBatch{
		Entries:       []*storev1.StoreEntry{pageEntry("w1", "u1", "", promptItem("agent-1"))},
		CursorAdvance: &storev1.CursorState{FileId: "12:34", Path: "/t/a.jsonl", Offset: 999, Conversion: currentConversion()},
	}, nil)

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
	_, err := d.WriteBatch(ctx(), "", WriteInteractive, batch(pageEntry("w1", "u1", "agent-1", promptItem("agent-1"))), nil)

	// Assert
	if !errors.Is(err, ErrInvalid) {
		t.Fatalf("error = %v, want ErrInvalid", err)
	}
	s.assertTracedRefusal(t, "producer is empty")
}

func TestWriteBatchRefusesAnUnsetBatch(t *testing.T) {
	// Arrange
	d, s := newStore(t)

	// Act
	_, err := d.WriteBatch(ctx(), "shim", WriteInteractive, nil, nil)

	// Assert
	if !errors.Is(err, ErrInvalid) {
		t.Fatalf("error = %v, want ErrInvalid", err)
	}
	s.assertTracedRefusal(t, "batch is unset")
}

func TestWriteBatchRefusesABatchThatCarriesNothing(t *testing.T) {
	// Arrange
	d, s := newStore(t)

	// Act
	_, err := d.WriteBatch(ctx(), "shim", WriteInteractive, &storev1.EntryBatch{}, nil)

	// Assert
	if !errors.Is(err, ErrInvalid) {
		t.Fatalf("error = %v, want ErrInvalid", err)
	}
	s.assertTracedRefusal(t, "neither entries nor a cursor advance")
}

func TestWriteBatchRefusesACursorWithNoFileIdentity(t *testing.T) {
	// Arrange: a path-keyed cursor would restart a renamed file from zero.
	d, s := newStore(t)

	// Act
	_, err := d.WriteBatch(ctx(), "sidecar", WriteInteractive, &storev1.EntryBatch{
		CursorAdvance: &storev1.CursorState{Path: "/t/a.jsonl", Offset: 1, Conversion: currentConversion()},
	}, nil)

	// Assert
	if !errors.Is(err, ErrInvalid) {
		t.Fatalf("error = %v, want ErrInvalid", err)
	}
	s.assertTracedRefusal(t, "cursor_advance.file_id is empty")
}

func TestWriteBatchRefusesACursorWithNoPath(t *testing.T) {
	// Arrange
	d, _ := newStore(t)

	// Act
	_, err := d.WriteBatch(ctx(), "sidecar", WriteInteractive, &storev1.EntryBatch{
		CursorAdvance: &storev1.CursorState{FileId: "12:34", Offset: 1, Conversion: currentConversion()},
	}, nil)

	// Assert
	if !errors.Is(err, ErrInvalid) {
		t.Fatalf("error = %v, want ErrInvalid", err)
	}
}

func TestWriteBatchRefusesANegativeCursorOffset(t *testing.T) {
	// Arrange
	d, _ := newStore(t)

	// Act
	_, err := d.WriteBatch(ctx(), "sidecar", WriteInteractive, &storev1.EntryBatch{
		CursorAdvance: &storev1.CursorState{FileId: "12:34", Path: "/t/a.jsonl", Offset: -1, Conversion: currentConversion()},
	}, nil)

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
	_, err := d.WriteBatch(ctx(), "shim", WriteInteractive, batch(pageEntry("w1", "u1", "agent-1", promptItem("agent-1"))), nil)

	// Assert
	if !errors.Is(err, ErrStorage) {
		t.Fatalf("error = %v, want ErrStorage", err)
	}
	// A STORAGE FAILURE IS THIS LAYER'S OWN: it names the statement and the
	// table, which nothing above can supply, so the normal-level record belongs
	// here and the server adds only a verbose trace.
	s.assertLogged(t, "error", "storage failure")
}

func TestWriteBatchCorrelatesTheRefusalWithTheOffendingWrite(t *testing.T) {
	// Arrange: the identifiers ride dedicated context keys, never the message
	// text, so the integration loop can query them.
	d, s := newStore(t)

	// Act
	_, err := d.WriteBatch(ctx(), "shim", WriteInteractive, batch(pageEntry("w1", "u1", "", promptItem("agent-1"))), nil)

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

// ---- the write ledger ----

func TestWriteBatchAbsorbsASupersededWriteIdWithoutRegressingTheRow(t *testing.T) {
	// Arrange: w1 then w2 settle the SAME upsert_key. Probing entry.write_id
	// would answer "never seen" for the replayed w1, because entry holds only
	// the LATEST write — and re-applying it would overwrite the newer content.
	d, _ := newStore(t)
	first := pageEntry("w1", "u1", "agent-1", frameItem(activityFrame("agent-1", "act-1", proseSaying("A"))))
	second := pageEntry("w2", "u1", "agent-1", frameItem(activityFrame("agent-1", "act-1", proseSaying("A settled"))))
	writeOK(t, d, first)
	writeOK(t, d, second)

	// Act
	result := writeOK(t, d, proto.Clone(first).(*storev1.StoreEntry))

	// Assert
	if result.Absorbed != 1 || result.Written != 0 {
		t.Fatalf("result = %+v, want the superseded replay absorbed", result)
	}
	if got := scalar[string](t, d, `SELECT write_id FROM entry WHERE upsert_key = 'u1'`); got != "w2" {
		t.Fatalf("write_id = %q, want the newer write w2 to still own the row", got)
	}
}

func TestWriteBatchLeavesTheWriteOrdinalAloneForASupersededReplay(t *testing.T) {
	// Arrange: bumping it would re-deliver the REGRESSED line to every live
	// watcher.
	d, _ := newStore(t)
	first := pageEntry("w1", "u1", "agent-1", frameItem(activityFrame("agent-1", "act-1", proseSaying("A"))))
	second := pageEntry("w2", "u1", "agent-1", frameItem(activityFrame("agent-1", "act-1", proseSaying("A settled"))))
	writeOK(t, d, first)
	writeOK(t, d, second)
	before := scalar[int64](t, d, `SELECT write_seq FROM entry WHERE upsert_key = 'u1'`)

	// Act
	writeOK(t, d, proto.Clone(first).(*storev1.StoreEntry))

	// Assert
	if after := scalar[int64](t, d, `SELECT write_seq FROM entry WHERE upsert_key = 'u1'`); after != before {
		t.Fatalf("write_seq moved from %d to %d on a superseded replay", before, after)
	}
}

func TestWriteBatchRecordsOneLedgerRowPerAppliedWrite(t *testing.T) {
	// Arrange: the ledger is the absorption index, so every APPLIED write —
	// insert and upsert alike — leaves exactly one row behind.
	d, _ := newStore(t)

	// Act
	writeOK(t, d, pageEntry("w1", "u1", "agent-1", frameItem(activityFrame("agent-1", "act-1", proseSaying("A")))))
	writeOK(t, d, pageEntry("w2", "u1", "agent-1", frameItem(activityFrame("agent-1", "act-1", proseSaying("B")))))

	// Assert
	if got := scalar[int](t, d, `SELECT COUNT(*) FROM write_ledger`); got != 2 {
		t.Fatalf("write_ledger rows = %d, want one per applied write", got)
	}
}

func TestWriteBatchLeavesNoLedgerRowForAnAbsorbedWrite(t *testing.T) {
	// Arrange
	d, _ := newStore(t)
	entry := pageEntry("w1", "u1", "agent-1", frameItem(activityFrame("agent-1", "act-1", prose())))
	writeOK(t, d, entry)

	// Act
	writeOK(t, d, proto.Clone(entry).(*storev1.StoreEntry))

	// Assert
	if got := scalar[int](t, d, `SELECT COUNT(*) FROM write_ledger WHERE write_id = 'w1'`); got != 1 {
		t.Fatalf("write_ledger rows for w1 = %d, want the single original", got)
	}
}

func TestWriteBatchLedgerRowNamesTheRowAndOrdinalItApplied(t *testing.T) {
	// Arrange
	d, _ := newStore(t)

	// Act
	writeOK(t, d, pageEntry("w1", "u1", "agent-1", frameItem(activityFrame("agent-1", "act-1", prose()))))

	// Assert
	if got := scalar[string](t, d, `SELECT upsert_key FROM write_ledger WHERE write_id = 'w1'`); got != "u1" {
		t.Fatalf("ledger upsert_key = %q, want u1", got)
	}
	if got := scalar[int64](t, d, `SELECT write_ledger.write_seq - entry.write_seq FROM write_ledger, entry WHERE write_ledger.write_id = 'w1' AND entry.upsert_key = 'u1'`); got != 0 {
		t.Fatalf("ledger write_seq differs from the row's by %d, want the same ordinal", got)
	}
}

func TestWriteBatchCommitsNoLedgerRowWhenTheBatchFails(t *testing.T) {
	// Arrange: durable or nothing includes the absorption fact — a ledger row
	// for a write whose row rolled back would make the retry a silent no-op.
	d, _ := newStore(t)
	good := pageEntry("w1", "u1", "agent-1", frameItem(activityFrame("agent-1", "act-1", prose())))
	bad := pageEntry("w2", "u2", "agent-1", frameItem(activityFrame("", "act-2", prose())))

	// Act
	if _, err := d.WriteBatch(ctx(), "producer", WriteInteractive, batch(good, bad), nil); !errors.Is(err, ErrInvalid) {
		t.Fatalf("WriteBatch error = %v, want ErrInvalid", err)
	}

	// Assert
	if got := scalar[int](t, d, `SELECT COUNT(*) FROM write_ledger`); got != 0 {
		t.Fatalf("write_ledger rows after a failed batch = %d, want 0", got)
	}
}

// ---- what a slow write_batch record blames ----

func TestWriteBatchReportsTheLockWaitInsideItsMeasuredDuration(t *testing.T) {
	// Arrange: a store that reports every batch, so the record is reachable
	// without contriving a slow one. The batch's clock starts BEFORE its
	// transaction, and every transaction here is BEGIN IMMEDIATE, so the wait
	// to begin is part of what the record calls the statement's duration —
	// which is exactly why it is also reported on its own.
	s, log := newSink(t)
	path := filepath.Join(t.TempDir(), "store.db")
	d, err := OpenWithOptions(path, log, Options{
		Now:       func() int64 { return testNow },
		SlowQuery: time.Nanosecond,
		// The bulk budget has to come down with the interactive threshold, or
		// the shipped 250ms base swallows a healthy in-process batch and the
		// record under test is never written.
		BulkBase:   time.Nanosecond,
		BulkPerRow: time.Nanosecond,
	})
	if err != nil {
		t.Fatalf("OpenWithOptions: %v", err)
	}
	t.Cleanup(func() { d.Close() }) //nolint:errcheck // best-effort test teardown

	// Act
	writeOK(t, d, pageEntry("w1", "u1", "agent-1", frameItem(activityFrame("agent-1", "act-1", prose()))))

	// Assert
	wait, duration := slowQueryTiming(t, s, StatementWriteBatch)
	if wait < 0 {
		t.Fatalf("lock_wait_ms = %v, want a non-negative wait", wait)
	}
	if wait > duration {
		t.Fatalf("lock_wait_ms = %v exceeds duration_ms = %v; the wait is a COMPONENT of the duration, not a second clock", wait, duration)
	}
}

// slowQueryTiming reads the lock wait and total duration off the one
// slow-query record naming this statement family.
func slowQueryTiming(t *testing.T, s *sink, statement string) (wait, duration float64) {
	t.Helper()
	for _, record := range s.records(t) {
		if record["operation"] != SlowQueryOperation {
			continue
		}
		context, _ := record["context"].(map[string]any)
		if context["statement"] != statement {
			continue
		}
		waitMs, ok := context["lock_wait_ms"].(float64)
		if !ok {
			t.Fatalf("the slow-query record carries no lock_wait_ms: %v", context)
		}
		durationMs, ok := context["duration_ms"].(float64)
		if !ok {
			t.Fatalf("the slow-query record carries no duration_ms: %v", context)
		}
		return waitMs, durationMs
	}
	t.Fatalf("no %s record for statement %q; log was:\n%s", SlowQueryOperation, statement, s.file.String())
	return 0, 0
}

// TestRefuseRecordsACanceledCallerAsAbandoned pins the class boundary the
// live store crossed: a GetLiveWork the caller hung up on left a
// `store.db.live-work` ERROR record behind, and nothing had gone wrong.
func TestRefuseRecordsACanceledCallerAsAbandoned(t *testing.T) {
	tests := []struct {
		name      string
		cause     error
		wantLevel string
		wantText  string
	}{
		{
			name:      "a caller that hung up is abandoned, not failed",
			cause:     context.Canceled,
			wantLevel: "info",
			wantText:  "abandoned",
		},
		{
			name:      "an rpc past its deadline is abandoned, not failed",
			cause:     context.DeadlineExceeded,
			wantLevel: "info",
			wantText:  "abandoned",
		},
		{
			name:      "a real driver failure is still the store's own error",
			cause:     errors.New("disk I/O error"),
			wantLevel: "error",
			wantText:  "refused",
		},
	}
	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			// Arrange
			s, log := newSink(t)
			d := &DB{log: log}
			err := storagef(test.cause, "scanning live agents")

			// Act
			got := d.refuse(logging.Fields{Operation: "store.db.live-work", Table: "agent"}, err)

			// Assert: the error reaches the caller unchanged whatever it was
			// recorded as — the level is a reading of the cause, never a
			// weakening of what the server is told.
			if !errors.Is(got, test.cause) {
				t.Fatalf("refuse returned %v, want it to wrap %v", got, test.cause)
			}
			s.assertLogged(t, test.wantLevel, test.wantText)
		})
	}
}

// TestAThirtyRowBatchOnAFullSizedCorpusStaysWithinItsOwnBudget is the bound the
// owner's `store.db.slow-query` warning of 2026-09-13 18:05:36 claimed was
// missed: `statement=write_batch duration_ms=1480 lock_wait_ms=0 rows=30
// threshold_ms=400`.
//
// `lock_wait_ms=0` says the batch never queued, so the claim is entirely about
// the statements INSIDE the transaction — the absorption probe, the identity
// probe, the entry upsert, the ledger insert, the shape upsert and the cursor
// advance. Every one of them is an indexed seek by construction, and this
// states it as a MEASUREMENT against a corpus larger than the one that produced
// the warning rather than as a reading of the schema.
//
// THE BOUND IS THE PRODUCTION BUDGET, not a tighter number of this test's own
// choosing: DefaultBulkBase + 30*DefaultBulkPerRow is exactly the 400ms the
// store itself would warn past, so a regression that would put a warning in the
// owner's log fails here first.
func TestAThirtyRowBatchOnAFullSizedCorpusStaysWithinItsOwnBudget(t *testing.T) {
	if raceEnabled {
		t.Skip("a wall-clock budget measures the race detector's instrumentation, not the store; see racedetector_on_test.go")
	}

	// Arrange
	d, _ := newStore(t)
	spread := seedSyntheticCorpus(t, d)
	budget := DefaultBulkBase + 30*DefaultBulkPerRow

	// Act
	started := time.Now()
	result, err := d.WriteBatch(ctx(), "test-sidecar", WriteInteractive, thirtyRowFileBatch("live", "corpus-file-0", spread+4096), nil)
	elapsed := time.Since(started)

	// Assert
	if err != nil {
		t.Fatalf("WriteBatch on a %d-row corpus: %v", syntheticCorpusRows, err)
	}
	if result.Written != 30 {
		t.Fatalf("batch wrote %d rows, want 30", result.Written)
	}
	if elapsed > budget {
		t.Fatalf("a 30-row batch on a %d-row corpus took %v, past its own %v budget", syntheticCorpusRows, elapsed, budget)
	}
}

// TestEveryStatementOfAWriteBatchSeeksRatherThanScans is the structural half of
// the 400ms budget above: not "it was fast on this box", but "no statement in
// the write transaction is allowed to walk a table".
//
// A DURATION CANNOT TELL A SEEK FROM A SCAN and a plan can. The owner's store
// reported `write_batch` at 1480ms for 30 rows with `lock_wait_ms=0` — no
// queueing, so the claim was about these statements — and the schema had just
// grown `write_ledger.source_file_id`/`source_offset` and the whole
// `residue_shapes` table. A column added without the index it is looked up by,
// or a primary key that did not land, turns one of these into a scan that is
// invisible on an idle box and ruinous on a loaded one.
func TestEveryStatementOfAWriteBatchSeeksRatherThanScans(t *testing.T) {
	for _, test := range writeBatchStatements {
		t.Run(test.name, func(t *testing.T) {
			// Arrange
			d, _ := newStore(t)

			// Act
			plan := queryPlan(t, d, test.statement, test.args...)

			// Assert
			assertNoTableScan(t, test.name, plan)
		})
	}
}

// TestEveryStatementOfAWriteBatchBuildsNoAutomaticIndex holds the write
// transaction to the same bar as live_work: no statement may answer a lookup
// by building a throwaway index from a full scan.
func TestEveryStatementOfAWriteBatchBuildsNoAutomaticIndex(t *testing.T) {
	for _, test := range writeBatchStatements {
		t.Run(test.name, func(t *testing.T) {
			// Arrange
			d, _ := newStore(t)

			// Act
			plan := queryPlan(t, d, test.statement, test.args...)

			// Assert
			assertNoAutomaticIndex(t, test.name, plan)
		})
	}
}

// writeBatchStatements is every statement of the write transaction, with
// representative arguments.
var writeBatchStatements = []struct {
	name      string
	statement string
	args      []any
}{
	{
		name:      "the write ordinal the batch orders itself by",
		statement: `SELECT COALESCE(MAX(write_seq), 0) FROM entry`,
	},
	{
		name:      "the absorption probe against the write ledger",
		statement: `SELECT 1 FROM write_ledger WHERE write_id = ?`,
		args:      []any{"corpus-write-1"},
	},
	{
		name:      "the identity probe against the entry row",
		statement: `SELECT book_agent_id, kind FROM entry WHERE upsert_key = ?`,
		args:      []any{"corpus-key-1"},
	},
	{
		name: "the entry upsert",
		statement: `INSERT INTO entry (upsert_key, write_id, write_seq, plane, kind, book_agent_id, run_id, top_level, frame, first_inserted_at_ms, last_written_at_ms)
			  VALUES (?,?,?,?,?,?,?,?,?,?,?)
			  ON CONFLICT(upsert_key) DO UPDATE SET write_id = excluded.write_id`,
		args: []any{"k", "w", 1, 2, "page_line", nil, nil, nil, []byte{0}, testNow, testNow},
	},
	{
		name:      "the ledger insert that stamps the batch's source position",
		statement: `INSERT INTO write_ledger (write_id, upsert_key, write_seq, applied_at_ms, source_file_id, source_offset) VALUES (?,?,?,?,?,?)`,
		args:      []any{"w", "k", 1, testNow, "corpus-file-0", 0},
	},
	{
		name: "the residue shape upsert",
		statement: `INSERT INTO residue_shapes (shape_hash, kind, key_structure, first_example, first_seen_ms, last_seen_ms, count)
			  VALUES (?,?,?,?,?,?,1)
			  ON CONFLICT(shape_hash) DO UPDATE SET
			    last_seen_ms = MAX(residue_shapes.last_seen_ms, excluded.last_seen_ms),
			    count = residue_shapes.count + 1`,
		args: []any{"h", "unparsed", "{a:string}", nil, testNow, testNow},
	},
	{
		name: "the cursor advance",
		statement: `INSERT INTO cursor (file_id, path, offset, carry, updated_at_ms) VALUES (?,?,?,?,?)
			  ON CONFLICT(file_id) DO UPDATE SET offset = excluded.offset`,
		args: []any{"corpus-file-0", "/p", 0, nil, testNow},
	},
	{
		name:      "the detached-work join lookup",
		statement: `SELECT work_id FROM detached_work WHERE origin_unit = ? LIMIT 1`,
		args:      []any{"act-1"},
	},
	{
		name:      "the agent terminal update",
		statement: `UPDATE agent SET ended_at_ms = ?, terminal = ? WHERE agent_id = ?`,
		args:      []any{testNow, []byte{0}, "agent-1"},
	},
	{
		name:      "the cursor's conversion bookkeeping",
		statement: upsertConversionSQL,
		args:      []any{"corpus-file-0", 2, nil},
	},
	{
		name:      "the restamp of an unchanged row",
		statement: `UPDATE entry SET write_id = ?, frame = ? WHERE position = ?`,
		args:      []any{"w", []byte{0}, 1},
	},
	{
		name:      "the retirement probe",
		statement: retireProbeSQL,
		args:      []any{"prompt:u1"},
	},
	{
		name:      "the retirement itself",
		statement: retireRowSQL,
		args:      []any{kindRetired, 2, testNow, 1},
	},
}

// ---- the class decides how a batch is committed ----

// newBoundedStore opens a store with the given bulk bounds, a hand-moved
// monotonic clock and verbose logging, so a test reads each transaction's
// timing record rather than guessing where the split fell.
func newBoundedStore(t *testing.T, clock *fakeClock, rows, bytes int, span time.Duration) (*DB, *sink) {
	t.Helper()
	s, log := newSink(t)
	d, err := OpenWithOptions(filepath.Join(t.TempDir(), "store.db"), log, Options{
		Now:            func() int64 { return testNow },
		Clock:          clock.Now,
		BulkChunkRows:  rows,
		BulkChunkBytes: bytes,
		BulkChunkTime:  span,
	})
	if err != nil {
		t.Fatalf("OpenWithOptions: %v", err)
	}
	t.Cleanup(func() { d.Close() }) //nolint:errcheck // best-effort test teardown
	return d, s
}

// entriesOf builds n distinct page lines for one book.
func entriesOf(n int) []*storev1.StoreEntry {
	out := make([]*storev1.StoreEntry, n)
	for i := range out {
		id := "e" + string(rune('a'+i))
		out[i] = pageEntry("w-"+id, "u-"+id, "agent-1", frameItem(activityFrame("agent-1", "act-"+id, prose())))
	}
	return out
}

// transactionRows reads, in order, how many entries each write_batch
// transaction took, from the per-write timing records.
func transactionRows(t *testing.T, s *sink) []int {
	t.Helper()
	var out []int
	for _, record := range s.records(t) {
		context, _ := record["context"].(map[string]any)
		if record["operation"] == WriteTimingOperation && context["statement"] == StatementWriteBatch {
			rows, _ := context["rows"].(float64)
			out = append(out, int(rows))
		}
	}
	return out
}

// TestABulkBatchIsSplitWithinItsBounds pins that the store's own bounds decide
// how much one bulk transaction takes, whatever size the producer sent, and
// that an interactive batch is never split.
func TestABulkBatchIsSplitWithinItsBounds(t *testing.T) {
	tests := []struct {
		name  string
		class WriteClass
		rows  int
		bytes int
		span  time.Duration
		// tick is how far the clock moves after each applied bulk entry.
		tick time.Duration
		want []int
	}{
		{name: "the row bound", class: WriteBulk, rows: 3, want: []int{3, 3, 3, 1}},
		{name: "the byte bound", class: WriteBulk, bytes: 1, want: []int{1, 1, 1, 1, 1, 1, 1, 1, 1, 1}},
		{name: "the time bound", class: WriteBulk, span: 10 * time.Millisecond, tick: 5 * time.Millisecond, want: []int{2, 2, 2, 2, 2}},
		{name: "an interactive batch is one transaction whatever the bounds", class: WriteInteractive, rows: 3, want: []int{10}},
	}
	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			// Arrange
			clock := &fakeClock{now: time.Unix(0, 0)}
			d, s := newBoundedStore(t, clock, test.rows, test.bytes, test.span)
			d.bulkEntryApplied = func() { clock.advance(test.tick) }

			// Act
			result, err := d.WriteBatch(ctx(), "test-producer", test.class, batch(entriesOf(10)...), nil)

			// Assert
			if err != nil {
				t.Fatalf("WriteBatch: %v", err)
			}
			if result.Written != 10 || len(result.Lines) != 10 {
				t.Fatalf("written=%d lines=%d, want 10 and 10", result.Written, len(result.Lines))
			}
			if got := transactionRows(t, s); fmt.Sprint(got) != fmt.Sprint(test.want) {
				t.Fatalf("entries per transaction = %v, want %v", got, test.want)
			}
		})
	}
}

// TestASplitBulkBatchAdvancesTheCursorOnlyInItsLastTransaction: the cursor
// advance and the shapes ride the LAST transaction, so a failure part-way can
// leave leading entries committed but never the advance past them.
func TestASplitBulkBatchAdvancesTheCursorOnlyInItsLastTransaction(t *testing.T) {
	tests := []struct {
		name  string
		query string
	}{
		{name: "the cursor advance", query: `SELECT COUNT(*) FROM cursor WHERE file_id = 'f1'`},
		{name: "the shape observation", query: `SELECT COUNT(*) FROM residue_shapes`},
	}
	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			// Arrange
			clock := &fakeClock{now: time.Unix(0, 0)}
			d, _ := newBoundedStore(t, clock, 2, 0, 0)
			var seen []int64
			d.transactionCommitted = func(WriteClass) { seen = append(seen, scalar[int64](t, d, test.query)) }
			b := batch(entriesOf(5)...)
			b.CursorAdvance = &storev1.CursorState{FileId: "f1", Path: "/t/f1.jsonl", Offset: 100, Conversion: currentConversion()}
			shapes := []*storev1.ShapeObservation{observation("h1", "unparsed", "{a:string}", "{}", 1000)}

			// Act
			if _, err := d.WriteBatch(ctx(), "test-producer", WriteBulk, b, shapes); err != nil {
				t.Fatalf("WriteBatch: %v", err)
			}

			// Assert
			if fmt.Sprint(seen) != fmt.Sprint([]int64{0, 0, 1}) {
				t.Fatalf("rows after each committed transaction = %v, want [0 0 1]", seen)
			}
		})
	}
}

// TestAnInteractiveWriteBehindALargeBulkBatchRunsNext is the owner's rule end
// to end: an interactive write that arrives while a large bulk batch is being
// written waits for the one bounded transaction in flight, then goes before
// the rest of the bulk batch.
func TestAnInteractiveWriteBehindALargeBulkBatchRunsNext(t *testing.T) {
	tests := []struct {
		name string
		want []WriteClass
	}{
		{name: "one bulk transaction, then the interactive write, then the rest",
			want: []WriteClass{WriteBulk, WriteInteractive, WriteBulk, WriteBulk, WriteBulk, WriteBulk}},
	}
	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			// Arrange
			clock := &fakeClock{now: time.Unix(0, 0)}
			d, _ := newBoundedStore(t, clock, 2, 0, 0)
			var order []WriteClass
			d.transactionCommitted = func(class WriteClass) { order = append(order, class) }
			interactiveQueued := make(chan struct{})
			d.queuedForWrite = func(class WriteClass) {
				if class == WriteInteractive {
					close(interactiveQueued)
				}
			}
			interactiveDone := make(chan error, 1)
			var once sync.Once
			d.bulkEntryApplied = func() {
				once.Do(func() {
					go func() {
						_, err := d.WriteBatch(ctx(), "test-shim", WriteInteractive,
							batch(pageEntry("w-live", "u-live", "agent-1", frameItem(activityFrame("agent-1", "act-live", prose())))), nil)
						interactiveDone <- err
					}()
					<-interactiveQueued
				})
			}

			// Act
			_, err := d.WriteBatch(ctx(), "test-sidecar", WriteBulk, batch(entriesOf(10)...), nil)

			// Assert
			if err != nil {
				t.Fatalf("bulk WriteBatch: %v", err)
			}
			if err := <-interactiveDone; err != nil {
				t.Fatalf("interactive WriteBatch: %v", err)
			}
			if fmt.Sprint(order) != fmt.Sprint(test.want) {
				t.Fatalf("transaction order = %v, want %v", order, test.want)
			}
		})
	}
}

// TestWriteBatchRefusesAnUnclassifiedWrite: a write that states no class is
// refused before anything is queued or written, and the refusal is traced.
func TestWriteBatchRefusesAnUnclassifiedWrite(t *testing.T) {
	tests := []struct {
		name  string
		class WriteClass
	}{
		{name: "the zero value", class: WriteClassUnset},
		{name: "a value no class names", class: WriteClass(7)},
	}
	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			// Arrange
			d, s := newStore(t)
			entry := pageEntry("w1", "u1", "agent-1", frameItem(activityFrame("agent-1", "act-1", prose())))

			// Act
			_, err := d.WriteBatch(ctx(), "test-producer", test.class, batch(entry), nil)

			// Assert
			if RefusalSite(err) != SiteWriteClassUnset {
				t.Fatalf("WriteBatch = %v, want a %s refusal", err, SiteWriteClassUnset)
			}
			if got := scalar[int64](t, d, `SELECT COUNT(*) FROM entry`); got != 0 {
				t.Fatalf("entry rows = %d, want 0", got)
			}
			s.assertTracedRefusal(t, "write_class is unset")
		})
	}
}

func TestWriteBatchKeepsARowsFirstTurnStamp(t *testing.T) {
	// Arrange: each case writes one unit twice, as the two planes do, and reads
	// back the turn the row is served with.
	stamp := func(entry *storev1.StoreEntry, turn string) *storev1.StoreEntry {
		if turn == "" {
			return entry
		}
		return stampedTurn(entry, turn)
	}
	tests := []struct {
		name       string
		first      string
		second     string
		wantServed string
	}{
		{name: "an unstamped write inherits the stored turn", first: "turn-a", second: "", wantServed: "turn-a"},
		{name: "a write naming another turn keeps the stored one", first: "turn-a", second: "turn-b", wantServed: "turn-a"},
		{name: "an unstamped row takes the turn a later write carries", first: "", second: "turn-b", wantServed: "turn-b"},
		{name: "a row no write stamped is served with no turn", first: "", second: "", wantServed: ""},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			d, _ := newStore(t)
			writeOK(t, d, stamp(pageEntry("w1", "u1", "agent-1", frameItem(activityFrame("agent-1", "act-1", prose()))), tt.first))

			// Act
			writeOK(t, d, stamp(pageEntry("w2", "u1", "agent-1", frameItem(activityFrame("agent-1", "act-1", bashSuccess()))), tt.second))

			// Assert
			lines, err := d.LinesSince(ctx(), "agent-1", 0)
			if err != nil {
				t.Fatalf("LinesSince: %v", err)
			}
			if len(lines) != 1 {
				t.Fatalf("lines = %d, want the one upserted row", len(lines))
			}
			if got := lines[0].Line.GetTurn().GetValue(); got != tt.wantServed {
				t.Fatalf("served turn = %q, want %q", got, tt.wantServed)
			}
		})
	}
}

func TestWriteBatchPublishesTheRowsKeptTurnToLiveWatchers(t *testing.T) {
	// Arrange: the stream plane stamped the unit; the file plane's copy cannot.
	d, _ := newStore(t)
	writeOK(t, d, stampedTurn(pageEntry("w1", "u1", "agent-1", frameItem(activityFrame("agent-1", "act-1", prose()))), "turn-a"))

	// Act
	result := writeOK(t, d, pageEntry("w2", "u1", "agent-1", frameItem(activityFrame("agent-1", "act-1", bashSuccess()))))

	// Assert
	if len(result.Lines) != 1 {
		t.Fatalf("published lines = %d, want 1", len(result.Lines))
	}
	if got := result.Lines[0].Line.GetTurn().GetValue(); got != "turn-a" {
		t.Fatalf("published turn = %q, want the row's first stamp %q", got, "turn-a")
	}
}

// ---- the conversion version ----

func TestAFilePlaneEntryWithoutAConversionVersionIsRefused(t *testing.T) {
	// Arrange
	d, _ := newStore(t)
	entry := filePageEntry("w1", "u1", "agent-1", promptItem("agent-1"), 1)
	entry.ConversionVersion = nil

	// Act
	_, err := d.WriteBatch(ctx(), "test-sidecar", WriteBulk, batch(entry), nil)

	// Assert
	if got := RefusalSite(err); got != SiteConversionVersionPlane {
		t.Fatalf("site = %q (error: %v), want %q", got, err, SiteConversionVersionPlane)
	}
}

func TestAFilePlaneEntryAtConversionVersionZeroIsRefused(t *testing.T) {
	// Arrange
	d, _ := newStore(t)
	entry := filePageEntry("w1", "u1", "agent-1", promptItem("agent-1"), 0)

	// Act
	_, err := d.WriteBatch(ctx(), "test-sidecar", WriteBulk, batch(entry), nil)

	// Assert
	if got := RefusalField(err); got != "entries[0].conversion_version" {
		t.Fatalf("field = %q (error: %v), want entries[0].conversion_version", got, err)
	}
}

func TestAStreamPlaneEntryCarryingAConversionVersionIsRefused(t *testing.T) {
	// Arrange
	d, _ := newStore(t)
	entry := pageEntry("w1", "u1", "agent-1", promptItem("agent-1"))
	entry.ConversionVersion = fileVersion()

	// Act
	_, err := d.WriteBatch(ctx(), "test-shim", WriteInteractive, batch(entry), nil)

	// Assert
	if got := RefusalSite(err); got != SiteConversionVersionPlane {
		t.Fatalf("site = %q (error: %v), want %q", got, err, SiteConversionVersionPlane)
	}
}

// ---- the restamp: a re-derivation that changes nothing costs readers nothing ----

func TestAnUnchangedRowReReadUnderANewVersionIsRestampedNotWritten(t *testing.T) {
	// Arrange
	d, _ := newStore(t)
	healBatch(t, d, 100, 1, []*storev1.StoreEntry{filePageEntry("w-v1", "prompt:u1", "agent-1", promptItem("agent-1"), 1)})

	// Act
	result := healBatch(t, d, 100, 2, []*storev1.StoreEntry{filePageEntry("w-v2", "prompt:u1", "agent-1", promptItem("agent-1"), 2)})

	// Assert
	if result.Restamped != 1 || result.Written != 0 || len(result.Lines) != 0 {
		t.Fatalf("restamped=%d written=%d lines=%d, want one restamp and nothing published", result.Restamped, result.Written, len(result.Lines))
	}
}

func TestARestampLeavesTheRowsWriteSeq(t *testing.T) {
	// Arrange: the write_seq is what a watch replays by.
	d, _ := newStore(t)
	healBatch(t, d, 100, 1, []*storev1.StoreEntry{filePageEntry("w-v1", "prompt:u1", "agent-1", promptItem("agent-1"), 1)})
	before := scalar[int64](t, d, `SELECT write_seq FROM entry WHERE upsert_key = 'prompt:u1'`)

	// Act
	healBatch(t, d, 100, 2, []*storev1.StoreEntry{filePageEntry("w-v2", "prompt:u1", "agent-1", promptItem("agent-1"), 2)})

	// Assert
	if after := scalar[int64](t, d, `SELECT write_seq FROM entry WHERE upsert_key = 'prompt:u1'`); after != before {
		t.Fatalf("write_seq %d -> %d, want it unchanged", before, after)
	}
}

func TestARestampRecordsTheNewConversionVersionOnTheRow(t *testing.T) {
	// Arrange
	d, _ := newStore(t)
	healBatch(t, d, 100, 1, []*storev1.StoreEntry{filePageEntry("w-v1", "prompt:u1", "agent-1", promptItem("agent-1"), 1)})

	// Act
	healBatch(t, d, 100, 2, []*storev1.StoreEntry{filePageEntry("w-v2", "prompt:u1", "agent-1", promptItem("agent-1"), 2)})

	// Assert
	stored := &storev1.StoreEntry{}
	if err := proto.Unmarshal(scalar[[]byte](t, d, `SELECT frame FROM entry WHERE upsert_key = 'prompt:u1'`), stored); err != nil {
		t.Fatalf("decoding the row: %v", err)
	}
	if stored.GetConversionVersion() != 2 {
		t.Fatalf("stored conversion_version = %d, want 2", stored.GetConversionVersion())
	}
}

func TestARowWhoseContentChangedUnderANewVersionIsWritten(t *testing.T) {
	// Arrange
	d, _ := newStore(t)
	healBatch(t, d, 100, 1, []*storev1.StoreEntry{filePageEntry("w-v1", "activity:a1", "agent-1", frameItem(activityFrame("agent-1", "a1", proseSaying("old"))), 1)})

	// Act
	result := healBatch(t, d, 100, 2, []*storev1.StoreEntry{filePageEntry("w-v2", "activity:a1", "agent-1", frameItem(activityFrame("agent-1", "a1", proseSaying("new"))), 2)})

	// Assert
	if result.Written != 1 || result.Restamped != 0 || len(result.Lines) != 1 {
		t.Fatalf("written=%d restamped=%d lines=%d, want the re-derived row written and published", result.Written, result.Restamped, len(result.Lines))
	}
}

func TestAStreamPlaneWriteOfUnchangedContentIsStillWritten(t *testing.T) {
	// Arrange: whether live readers have heard a stream write before is not
	// the store's call.
	d, _ := newStore(t)
	writeOK(t, d, pageEntry("w1", "prompt:u1", "agent-1", promptItem("agent-1")))

	// Act
	result := writeOK(t, d, pageEntry("w2", "prompt:u1", "agent-1", promptItem("agent-1")))

	// Assert
	if result.Written != 1 || result.Restamped != 0 {
		t.Fatalf("written=%d restamped=%d, want the stream write applied", result.Written, result.Restamped)
	}
}

// ---- the cursor's conversion ----

func TestACursorAdvanceWithoutAConversionIsRefused(t *testing.T) {
	// Arrange
	d, _ := newStore(t)

	// Act
	_, err := d.WriteBatch(ctx(), "test-sidecar", WriteBulk, &storev1.EntryBatch{
		CursorAdvance: &storev1.CursorState{FileId: "12:34", Path: "/t/a.jsonl", Offset: 10},
	}, nil)

	// Assert
	if got := RefusalSite(err); got != SiteCursorConversionUnset {
		t.Fatalf("site = %q (error: %v), want %q", got, err, SiteCursorConversionUnset)
	}
}

func TestACursorConversionAtVersionZeroIsRefused(t *testing.T) {
	// Arrange
	d, _ := newStore(t)

	// Act
	_, err := d.WriteBatch(ctx(), "test-sidecar", WriteBulk, &storev1.EntryBatch{CursorAdvance: cursorAt(10, 0)}, nil)

	// Assert
	if got := RefusalField(err); got != "cursor_advance.conversion.version" {
		t.Fatalf("field = %q (error: %v), want cursor_advance.conversion.version", got, err)
	}
}

func TestACursorConversionWithNoStateIsRefused(t *testing.T) {
	// Arrange
	d, _ := newStore(t)
	cursor := cursorAt(10, 2)
	cursor.Conversion.State = nil

	// Act
	_, err := d.WriteBatch(ctx(), "test-sidecar", WriteBulk, &storev1.EntryBatch{CursorAdvance: cursor}, nil)

	// Assert
	if got := RefusalField(err); got != "cursor_advance.conversion.state" {
		t.Fatalf("field = %q (error: %v), want cursor_advance.conversion.state", got, err)
	}
}

func TestAHealWhoseThroughIsNotPastTheOffsetIsRefused(t *testing.T) {
	// Arrange: a re-read that has reached where the old conversion stopped is
	// `current`, never a heal through its own position.
	d, _ := newStore(t)
	cursor := cursorAt(500, 2)
	cursor.Conversion.State = &storev1.CursorConversion_Healing{Healing: &storev1.CursorConversionHealing{Through: 500}}

	// Act
	_, err := d.WriteBatch(ctx(), "test-sidecar", WriteBulk, &storev1.EntryBatch{CursorAdvance: cursor}, nil)

	// Assert
	if got := RefusalField(err); got != "cursor_advance.conversion.healing.through" {
		t.Fatalf("field = %q (error: %v), want cursor_advance.conversion.healing.through", got, err)
	}
}
