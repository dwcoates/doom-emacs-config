package tail

// The BOOT REWIND: a restarted reader resumes at the first record of the
// in-progress turn rather than at the byte the store's cursor names, so a
// converter's in-memory joins re-warm. These tests own the scan's mechanics;
// what a turn start IS lives in IsUserPromptRecord below.

import (
	"io"
	"path/filepath"
	"strings"
	"testing"

	storev1 "agentrepl/proto/store/v1"
	"agentrepl/shim-claude-sidecar/internal/logging"
)

// rewindTailer lays down content and restores a cursor at offset.
func rewindTailer(t *testing.T, content string, offset int64) (*Tailer, *[]string) {
	t.Helper()
	path := filepath.Join(t.TempDir(), "session.jsonl")
	writeFile(t, path, content)
	var logs []string
	log := logging.New(sliceWriter{lines: &logs}, io.Discard).With(logging.Context{Component: "test"})
	tr := New(path, JSONLCodec{}, &holdStub{hold: func([]Frame) (int64, bool) { return 0, false }}, &Context{}, log)
	tr.Restore(&storev1.CursorState{FileId: "1:2", Path: path, Offset: offset, Carry: []byte("partial")})
	return tr, &logs
}

const (
	promptLine    = `{"type":"user","message":{"role":"user","content":"do the thing"}}`
	assistantLine = `{"type":"assistant","message":{"role":"assistant","content":[{"type":"text","text":"ok"}]}}`
	resultLine    = `{"type":"user","message":{"role":"user","content":[{"type":"tool_result","tool_use_id":"t1","content":"done"}]}}`
)

func TestRewindMovesToTheLastTurnStart(t *testing.T) {
	// Arrange: two turns, the second still in progress at the cursor.
	first := promptLine + "\n" + assistantLine + "\n"
	content := first + promptLine + "\n" + assistantLine + "\n"
	tr, _ := rewindTailer(t, content, int64(len(content)))

	// Act.
	rewound := tr.RewindToTurnStart(DefaultRewindWindow, IsUserPromptRecord)

	// Assert.
	if !rewound {
		t.Fatal("a transcript with a turn start in the window was not rewound")
	}
	if tr.offset != int64(len(first)) {
		t.Fatalf("rewound offset = %d, want %d (the in-progress turn's first record)", tr.offset, int64(len(first)))
	}
}

func TestRewindDropsTheRestoredCarry(t *testing.T) {
	// Arrange.
	content := promptLine + "\n" + assistantLine + "\n"
	tr, _ := rewindTailer(t, content, int64(len(content)))

	// Act.
	tr.RewindToTurnStart(DefaultRewindWindow, IsUserPromptRecord)

	// Assert: every carried byte belongs to a line the rewound position re-reads
	// whole, so keeping it would double those bytes.
	if len(tr.carry) != 0 {
		t.Fatalf("carry = %q, want it dropped with the rewind", tr.carry)
	}
}

func TestRewindIsANoOpAtOffsetZero(t *testing.T) {
	// Arrange: the fresh-store answer.
	tr, _ := rewindTailer(t, promptLine+"\n", 0)

	// Act.
	rewound := tr.RewindToTurnStart(DefaultRewindWindow, IsUserPromptRecord)

	// Assert.
	if rewound || tr.offset != 0 {
		t.Fatalf("a file read from its start was rewound to %d", tr.offset)
	}
}

func TestRewindLeavesTheCursorWhenNoTurnStartIsInWindow(t *testing.T) {
	// Arrange: a window holding only assistant records.
	content := assistantLine + "\n" + assistantLine + "\n"
	tr, _ := rewindTailer(t, content, int64(len(content)))

	// Act.
	rewound := tr.RewindToTurnStart(DefaultRewindWindow, IsUserPromptRecord)

	// Assert: reading from an arbitrary older position is worse than not
	// rewinding, so the store's cursor stands.
	if rewound || tr.offset != int64(len(content)) {
		t.Fatalf("offset = %d, want the store's cursor %d", tr.offset, int64(len(content)))
	}
}

func TestRewindSkipsAFragmentAtTheWindowEdge(t *testing.T) {
	// Arrange: a window that begins mid-line, so the leading bytes are a
	// fragment rather than a record.
	content := promptLine + "\n" + assistantLine + "\n"
	tr, _ := rewindTailer(t, content, int64(len(content)))

	// Act: a window small enough to cut the first prompt line in half.
	rewound := tr.RewindToTurnStart(int64(len(assistantLine)+10), IsUserPromptRecord)

	// Assert: the fragment is not mistaken for a turn start.
	if rewound {
		t.Fatalf("a partial line at the window edge was read as a record, rewinding to %d", tr.offset)
	}
}

func TestRewindIsLoggedAtNormalVerbosity(t *testing.T) {
	// Arrange.
	content := promptLine + "\n" + assistantLine + "\n"
	tr, logs := rewindTailer(t, content, int64(len(content)))

	// Act.
	tr.RewindToTurnStart(DefaultRewindWindow, IsUserPromptRecord)

	// Assert.
	if !strings.Contains(strings.Join(*logs, "\n"), "rewound the restored cursor") {
		t.Fatalf("the rewind was silent; got %v", *logs)
	}
}

func TestRewindRejectsAMissingPredicate(t *testing.T) {
	// Arrange.
	tr, _ := rewindTailer(t, promptLine+"\n", 10)
	defer func() {
		// Assert.
		if recover() == nil {
			t.Fatal("a rewind with no turn-start predicate was accepted")
		}
	}()

	// Act.
	tr.RewindToTurnStart(DefaultRewindWindow, nil)
}

func TestIsUserPromptRecordClassifies(t *testing.T) {
	tests := []struct {
		name string
		line string
		want bool
	}{
		{name: "prose prompt as a string", line: promptLine, want: true},
		{name: "prose prompt as a text block", line: `{"type":"user","message":{"content":[{"type":"text","text":"hi"}]}}`, want: true},
		{name: "tool result carrier", line: resultLine, want: false},
		{name: "assistant record", line: assistantLine, want: false},
		{name: "harness bookkeeping", line: `{"type":"user","isMeta":true,"message":{"content":"noise"}}`, want: false},
		{name: "empty string content", line: `{"type":"user","message":{"content":""}}`, want: false},
		{name: "no message", line: `{"type":"user"}`, want: false},
		{name: "system record", line: `{"type":"system","subtype":"compact_boundary"}`, want: false},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			frames, _ := JSONLCodec{}.Decode([]byte(tc.line+"\n"), 0)
			if len(frames) != 1 || frames[0].Obj == nil {
				t.Fatalf("fixture %q did not decode to one record", tc.line)
			}

			// Act.
			got := IsUserPromptRecord(frames[0].Obj)

			// Assert.
			if got != tc.want {
				t.Fatalf("IsUserPromptRecord = %t, want %t", got, tc.want)
			}
		})
	}
}
