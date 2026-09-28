package main

// heal_test.go — a conversion change heals the rows it made wrong: a
// transcript whose rows an older conversion produced is re-read from its start,
// on a budget of its own, retiring every row a record no longer converts to.

import (
	"os"
	"slices"
	"strings"
	"testing"
	"time"

	storev1 "agentrepl/proto/store/v1"
	"agentrepl/shim-claude-sidecar/internal/convert"
	"agentrepl/shim-claude-sidecar/internal/tail"
)

const (
	// healNotificationLine is a task notification the old conversion minted a
	// `prompt:heal-n1` row for; the current one never does.
	healNotificationLine = `{"type":"user","uuid":"heal-n1","isSidechain":false,"entrypoint":"cli","origin":{"kind":"task-notification"},` +
		`"timestamp":"2026-08-29T12:00:00.000Z","message":{"role":"user","content":"<task-notification>\n<task-id>bsh1</task-id>\n<status>completed</status>\n</task-notification>"}}`
	// healTypedLine is a prompt a person typed: it converts to `prompt:heal-p1`
	// under every conversion.
	healTypedLine = `{"type":"user","uuid":"heal-p1","isSidechain":false,"entrypoint":"cli",` +
		`"timestamp":"2026-08-29T12:00:01.000Z","message":{"role":"user","content":[{"type":"text","text":"fix the build"}]}}`
)

// preVersioning is the cursor a sidecar that predates conversion versions left
// for a file it had read to `offset`: no conversion at all.
func preVersioning(t *testing.T, path string, offset int64) *storev1.CursorState {
	t.Helper()
	return &storev1.CursorState{FileId: identityOf(t, path), Path: path, Offset: offset}
}

// fileSizeOf answers a fixture's size, the offset an old reader had read it to.
func fileSizeOf(t *testing.T, path string) int64 {
	t.Helper()
	info, err := os.Stat(path)
	if err != nil {
		t.Fatalf("stat %s: %v", path, err)
	}
	return info.Size()
}

// healingTranscript writes an active session's transcript whose stored cursor
// predates conversion versions, and begins the cycle over it.
func healingTranscript(t *testing.T, h *harness, session string, lines ...string) string {
	t.Helper()
	path := h.transcript(t, session, lines...)
	h.store.cursors = append(h.store.cursors, preVersioning(t, path, fileSizeOf(t, path)))
	if err := h.sc.beginCycle(); err != nil {
		t.Fatalf("beginCycle: %v", err)
	}
	return path
}

// retiredKeysWritten is every upsert key the writes asked the store to retire.
func retiredKeysWritten(store *fakeStore) []string {
	var keys []string
	for _, batch := range store.writes {
		for _, retirement := range batch.GetRetirements() {
			keys = append(keys, retirement.GetUpsertKey())
		}
	}
	return keys
}

// manyTurns is enough lines that one transcript cannot be read in one batch.
func manyTurns() []string {
	var lines []string
	for len(lines) <= tail.MaxBatchFrames {
		lines = append(lines, promptLine, assistantLine)
	}
	return lines
}

func TestHealOwed(t *testing.T) {
	current := convert.ConversionVersion
	healing := func(version uint32, through int64) *storev1.CursorConversion {
		return &storev1.CursorConversion{Version: version, State: &storev1.CursorConversion_Healing{
			Healing: &storev1.CursorConversionHealing{Through: through},
		}}
	}
	tests := []struct {
		name        string
		kind        tail.Kind
		cursor      *storev1.CursorState
		wantThrough int64
		wantResumed bool
		wantNone    bool
	}{
		{
			name:     "a file never read owes nothing",
			kind:     tail.KindSessionTranscript,
			cursor:   nil,
			wantNone: true,
		},
		{
			name:     "a spool is never re-derived",
			kind:     tail.KindShellSpool,
			cursor:   &storev1.CursorState{Offset: 500},
			wantNone: true,
		},
		{
			name:     "a cursor read under the current conversion owes nothing",
			kind:     tail.KindSessionTranscript,
			cursor:   &storev1.CursorState{Offset: 500, Conversion: currentConversion()},
			wantNone: true,
		},
		{
			name:     "a cursor from a newer conversion owes nothing",
			kind:     tail.KindSessionTranscript,
			cursor:   &storev1.CursorState{Offset: 500, Conversion: &storev1.CursorConversion{Version: current + 1}},
			wantNone: true,
		},
		{
			name:     "a pre-versioning cursor at offset zero owes nothing",
			kind:     tail.KindSessionTranscript,
			cursor:   &storev1.CursorState{Offset: 0},
			wantNone: true,
		},
		{
			name:        "a pre-versioning transcript is re-read through its offset",
			kind:        tail.KindSessionTranscript,
			cursor:      &storev1.CursorState{Offset: 500},
			wantThrough: 500,
		},
		{
			name:        "a pre-versioning subagent transcript is re-read through its offset",
			kind:        tail.KindAgentTranscript,
			cursor:      &storev1.CursorState{Offset: 500},
			wantThrough: 500,
		},
		{
			name:        "an older conversion's heal cut short is re-read through its own through",
			kind:        tail.KindSessionTranscript,
			cursor:      &storev1.CursorState{Offset: 200, Conversion: healing(current-1, 900)},
			wantThrough: 900,
		},
		{
			name:        "a heal under the current conversion is resumed",
			kind:        tail.KindSessionTranscript,
			cursor:      &storev1.CursorState{Offset: 200, Conversion: healing(current, 900)},
			wantThrough: 900,
			wantResumed: true,
		},
		{
			name:     "a current heal already past its through owes nothing",
			kind:     tail.KindSessionTranscript,
			cursor:   &storev1.CursorState{Offset: 900, Conversion: healing(current, 900)},
			wantNone: true,
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act.
			got := healOwed(tt.kind, tt.cursor)

			// Assert.
			if tt.wantNone {
				if got != nil {
					t.Fatalf("healOwed = %+v, want none", got)
				}
				return
			}
			if got == nil || got.through != tt.wantThrough || got.resumed != tt.wantResumed {
				t.Fatalf("healOwed = %+v, want through=%d resumed=%t", got, tt.wantThrough, tt.wantResumed)
			}
		})
	}
}

func TestAPreVersioningTranscriptIsReReadFromItsStart(t *testing.T) {
	// Arrange.
	h := newHarness(t, &fakeStore{})

	// Act.
	path := healingTranscript(t, h, "sess-heal", promptLine, assistantLine, promptLine, assistantLine)

	// Assert.
	if got := h.sc.watchers[path].tailer.Offset(); got != 0 {
		t.Fatalf("offset = %d, want 0: the whole file predates the current conversion", got)
	}
}

func TestAPreVersioningTranscriptStatesItsReDerivation(t *testing.T) {
	// Arrange.
	h := newHarness(t, &fakeStore{})

	// Act.
	path := healingTranscript(t, h, "sess-heal", promptLine, assistantLine)

	// Assert.
	rec := h.requireOnce(t, "conversion-heal", "info")
	if got := ctxString(t, rec, "path"); got != path {
		t.Fatalf("the heal record names path %q, want %q", got, path)
	}
}

func TestAHealRetiresTheNotificationsPrompt(t *testing.T) {
	// Arrange.
	h := newHarness(t, &fakeStore{})
	healingTranscript(t, h, "sess-heal", healNotificationLine, healTypedLine)

	// Act.
	h.sc.healStep()

	// Assert.
	if keys := retiredKeysWritten(h.store); !slices.Contains(keys, convert.PromptKey("heal-n1")) {
		t.Fatalf("retired %v, want the notification's prompt row retired", keys)
	}
}

func TestAHealRetiresAtTheCurrentConversion(t *testing.T) {
	// Arrange.
	h := newHarness(t, &fakeStore{})
	healingTranscript(t, h, "sess-heal", healNotificationLine)

	// Act.
	h.sc.healStep()

	// Assert.
	for _, batch := range h.store.writes {
		for _, retirement := range batch.GetRetirements() {
			if retirement.GetConversionVersion() != convert.ConversionVersion {
				t.Fatalf("retirement %q names version %d, want %d", retirement.GetUpsertKey(), retirement.GetConversionVersion(), convert.ConversionVersion)
			}
		}
	}
}

func TestAHealNeverRetiresAPromptItStillConvertsTo(t *testing.T) {
	// Arrange.
	h := newHarness(t, &fakeStore{})
	healingTranscript(t, h, "sess-heal", healNotificationLine, healTypedLine)

	// Act.
	h.sc.healStep()

	// Assert.
	if keys := retiredKeysWritten(h.store); slices.Contains(keys, convert.PromptKey("heal-p1")) {
		t.Fatalf("retired %v, want the typed prompt's row left standing", keys)
	}
}

func TestALiveBatchNamesNoRetirement(t *testing.T) {
	// Arrange: a file read for the first time produced no row to retire.
	h := newHarness(t, &fakeStore{})
	h.transcript(t, "sess-live", healNotificationLine, healTypedLine)
	if err := h.sc.beginCycle(); err != nil {
		t.Fatalf("beginCycle: %v", err)
	}

	// Act.
	h.tick()

	// Assert.
	if keys := retiredKeysWritten(h.store); len(keys) != 0 {
		t.Fatalf("a first read retired %v, want nothing", keys)
	}
}

func TestABatchShortOfWhereTheOldConversionStoppedStatesHealing(t *testing.T) {
	// Arrange.
	h := newHarness(t, &fakeStore{})
	path := healingTranscript(t, h, "sess-heal", manyTurns()...)

	// Act.
	h.sc.healStep()

	// Assert.
	first := h.store.writes[0].GetCursorAdvance().GetConversion()
	if first.GetHealing().GetThrough() != fileSizeOf(t, path) {
		t.Fatalf("the first batch states %v, want healing through %d", first, fileSizeOf(t, path))
	}
}

func TestTheBatchReachingWhereTheOldConversionStoppedStatesCurrent(t *testing.T) {
	// Arrange.
	h := newHarness(t, &fakeStore{})
	healingTranscript(t, h, "sess-heal", manyTurns()...)

	// Act.
	h.sc.healStep()

	// Assert.
	last := h.store.writes[len(h.store.writes)-1].GetCursorAdvance().GetConversion()
	if last.GetCurrent() == nil || last.GetVersion() != convert.ConversionVersion {
		t.Fatalf("the last batch states %v, want current at version %d", last, convert.ConversionVersion)
	}
}

func TestAHealIsReadInSeveralBoundedBatches(t *testing.T) {
	// Arrange.
	h := newHarness(t, &fakeStore{})
	healingTranscript(t, h, "sess-heal", manyTurns()...)

	// Act.
	h.sc.healStep()

	// Assert.
	if len(h.store.writes) < 2 {
		t.Fatalf("the heal wrote %d batch(es), want the file read in bounded batches", len(h.store.writes))
	}
}

func TestAFinishedHealReturnsTheFileToOrdinaryReading(t *testing.T) {
	// Arrange.
	h := newHarness(t, &fakeStore{})
	path := healingTranscript(t, h, "sess-heal", promptLine, assistantLine)

	// Act.
	h.sc.healStep()

	// Assert.
	if h.sc.watchers[path].heal != nil {
		t.Fatal("the heal is still in progress after reading to where the older conversion stopped")
	}
}

func TestAHealedTranscriptOwesNothingOnTheNextBoot(t *testing.T) {
	// Arrange.
	h := newHarness(t, &fakeStore{})
	healingTranscript(t, h, "sess-heal", manyTurns()...)
	h.sc.healStep()
	stored := h.store.writes[len(h.store.writes)-1].GetCursorAdvance()

	// Act.
	got := healOwed(tail.KindSessionTranscript, stored)

	// Assert: a second run re-derives nothing.
	if got != nil {
		t.Fatalf("the committed cursor %v still owes %+v", stored, got)
	}
}

func TestAHealCutShortResumesAtItsCommittedPosition(t *testing.T) {
	// Arrange: a restart found the heal stored mid-file.
	h := newHarness(t, &fakeStore{})
	turn := int64(len(promptLine + "\n" + assistantLine + "\n"))
	path := h.transcript(t, "sess-heal", promptLine, assistantLine, promptLine, assistantLine, promptLine, assistantLine)
	h.store.cursors = []*storev1.CursorState{{
		FileId: identityOf(t, path), Path: path, Offset: 2 * turn,
		Conversion: &storev1.CursorConversion{Version: convert.ConversionVersion, State: &storev1.CursorConversion_Healing{
			Healing: &storev1.CursorConversionHealing{Through: 3 * turn},
		}},
	}}

	// Act.
	if err := h.sc.beginCycle(); err != nil {
		t.Fatalf("beginCycle: %v", err)
	}

	// Assert: the stored position, walked back to its in-progress turn like
	// any restart's, and never the file's start.
	w := h.sc.watchers[path]
	if w.tailer.Offset() != turn || w.heal == nil || !w.heal.resumed {
		t.Fatalf("offset=%d heal=%+v, want the heal resumed at %d", w.tailer.Offset(), w.heal, turn)
	}
}

func TestAHealOfAFileNowShorterThanItsThroughEndsAtTheFilesEnd(t *testing.T) {
	// Arrange: the older conversion had read further than the file now reaches.
	h := newHarness(t, &fakeStore{})
	path := h.transcript(t, "sess-heal", promptLine, assistantLine)
	size := fileSizeOf(t, path)
	h.store.cursors = []*storev1.CursorState{preVersioning(t, path, size+1000)}
	if err := h.sc.beginCycle(); err != nil {
		t.Fatalf("beginCycle: %v", err)
	}

	// Act.
	h.sc.healStep()

	// Assert.
	last := h.store.writes[len(h.store.writes)-1].GetCursorAdvance()
	if last.GetOffset() != size || last.GetConversion().GetCurrent() == nil || h.sc.watchers[path].heal != nil {
		t.Fatalf("last advance %v, heal %+v; want current at the file's end %d and the heal over", last, h.sc.watchers[path].heal, size)
	}
}

func TestAnUncommittedShortHealEndIsRecordedAtError(t *testing.T) {
	// Arrange: the file was truncated to nothing since the older conversion
	// read it, and the store cannot take the write that ends the heal.
	h := newHarness(t, &fakeStore{writeFail: "disk full"})
	path := h.transcript(t, "sess-heal", promptLine, assistantLine)
	h.store.cursors = []*storev1.CursorState{preVersioning(t, path, 1000)}
	if err := os.Truncate(path, 0); err != nil {
		t.Fatalf("truncate: %v", err)
	}
	if err := h.sc.beginCycle(); err != nil {
		t.Fatalf("beginCycle: %v", err)
	}

	// Act.
	h.sc.healStep()

	// Assert.
	var found bool
	for _, rec := range h.opsAt(t, "store-write", "error") {
		found = found || strings.Contains(rec.Message, "re-derivation that stopped short was not committed")
	}
	if !found {
		t.Fatalf("no store-write error record for the uncommitted heal end; records: %v", h.opsAt(t, "store-write", ""))
	}
}

func TestADormantTranscriptOwingAHealIsWatched(t *testing.T) {
	// Arrange: no active workspace owns the file.
	h := newHarness(t, &fakeStore{})
	h.closedWorkspace(t, "sess-closed")
	path := h.inactiveTranscript(t, "sess-closed", healNotificationLine)
	h.store.cursors = []*storev1.CursorState{preVersioning(t, path, fileSizeOf(t, path))}

	// Act.
	if err := h.sc.beginCycle(); err != nil {
		t.Fatalf("beginCycle: %v", err)
	}

	// Assert.
	if w, watched := h.sc.watchers[path]; !watched || w.heal == nil {
		t.Fatal("a dormant transcript owing a re-derivation was not admitted for it")
	}
}

func TestAHealReadsAnActiveConversationsFileFirst(t *testing.T) {
	// Arrange: the dormant file sorts first by path; the active one must still
	// lead.
	h := newHarness(t, &fakeStore{})
	h.closedWorkspace(t, "aa-dormant")
	dormant := h.inactiveTranscript(t, "aa-dormant", healNotificationLine)
	active := h.transcript(t, "zz-active", healNotificationLine)
	h.store.cursors = []*storev1.CursorState{
		preVersioning(t, dormant, fileSizeOf(t, dormant)),
		preVersioning(t, active, fileSizeOf(t, active)),
	}
	if err := h.sc.beginCycle(); err != nil {
		t.Fatalf("beginCycle: %v", err)
	}

	// Act.
	order := h.sc.healOrder()

	// Assert.
	if len(order) != 2 || order[0] != active {
		t.Fatalf("heal order = %v, want the active conversation's %s first", order, active)
	}
}

func TestAHealStopsWhenItsBudgetIsSpent(t *testing.T) {
	// Arrange: every reading of the clock moves it a whole poll interval, so
	// the budget is spent by the first batch.
	h := newHarness(t, &fakeStore{})
	healingTranscript(t, h, "sess-heal", manyTurns()...)
	h.sc.now = func() time.Time {
		h.advance(time.Second)
		return h.clock
	}

	// Act.
	h.sc.healStep()

	// Assert: one batch, and no more, on this tick.
	if len(h.store.writes) != 1 {
		t.Fatalf("the heal wrote %d batch(es) on a spent budget, want exactly the one it always reads", len(h.store.writes))
	}
}
