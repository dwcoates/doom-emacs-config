package untoldnotice

import (
	"context"
	"errors"
	"reflect"
	"strings"
	"testing"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
)

// fakeHolds answers every Supersede with retired (or err) and records the
// turns it was asked to retire.
type fakeHolds struct {
	retired bool
	err     error
	asked   []ids.TurnID
}

func (h *fakeHolds) Supersede(_ context.Context, _ ids.WorkspaceID, turn ids.TurnID) (bool, error) {
	h.asked = append(h.asked, turn)
	return h.retired, h.err
}

const op = "daemon.test"

var prev = Notice{Turn: "turn-1", Sentences: []string{"one moved.", "two landed."}}

func TestCarriedAnswersWhatTheNextNoticeSaysAgain(t *testing.T) {
	tests := []struct {
		name    string
		holds   *fakeHolds
		prev    Notice
		found   bool
		want    []string
		asked   []ids.TurnID
		level   string
		message string
	}{
		{name: "no notice is recorded", holds: &fakeHolds{}, found: false},
		{name: "the notice was never submitted", holds: &fakeHolds{}, prev: Notice{Sentences: prev.Sentences}, found: true,
			want: prev.Sentences, level: "info", message: "the subscriber's last notice was never submitted; the next one says its changes"},
		{name: "the notice was superseded before it was told", holds: &fakeHolds{retired: true}, prev: prev, found: true,
			want: prev.Sentences, asked: []ids.TurnID{"turn-1"}, level: "info", message: "the subscriber's untold notice was superseded; the next one says its changes first"},
		{name: "the notice was told", holds: &fakeHolds{retired: false}, prev: prev, found: true,
			asked: []ids.TurnID{"turn-1"}, level: "debug", message: "the subscriber's last notice was told; the next one says only the new change"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			log := dlog.NewTestLogger()

			// Act
			got := Carried(context.Background(), tt.holds, log, op, "ws-alice", tt.prev, tt.found, dlog.Context{"repo": "/r"})

			// Assert
			if !reflect.DeepEqual(got, tt.want) || !reflect.DeepEqual(tt.holds.asked, tt.asked) {
				t.Fatalf("Carried = %v asking %v, want %v asking %v", got, tt.holds.asked, tt.want, tt.asked)
			}
			if tt.message == "" {
				return
			}
			record, ok := dlog.FindRecord(log.Records(), dlog.RecordQuery{Level: tt.level, Operation: op, Message: tt.message})
			if !ok || record.Context["subscriber"] != "ws-alice" || record.Context["repo"] != "/r" {
				t.Fatalf("no %s record %q naming the subscriber and the subject: %+v", tt.level, tt.message, log.Records())
			}
		})
	}
}

func TestCarriedRecordsANoticeThatCannotBeSupersededAndCarriesNothing(t *testing.T) {
	// Arrange
	log := dlog.NewTestLogger()
	holds := &fakeHolds{err: errors.New("the store is gone")}

	// Act
	got := Carried(context.Background(), holds, log, op, "ws-alice", prev, true, dlog.Context{})

	// Assert
	if got != nil {
		t.Fatalf("Carried = %v, want nothing carried from a notice that stands", got)
	}
	record, ok := dlog.FindRecord(log.Records(), dlog.RecordQuery{Level: "error", Operation: op,
		Message: "could not supersede the subscriber's untold notice; it stands and is told, and the next one says only the new change"})
	if !ok || record.Context["cause"] != "the store is gone" || record.Context["superseded_turn"] != "turn-1" {
		t.Fatalf("no error record naming the notice and the cause: %+v", log.Records())
	}
}

func TestSentencesStatesOneChangeAsItsSentence(t *testing.T) {
	// Arrange, Act, Assert.
	if got := Sentences([]string{"one landed."}); got != "one landed." {
		t.Fatalf("Sentences = %q, want the one sentence", got)
	}
}

func TestSentencesCountsAndListsSeveralChangesOldestFirst(t *testing.T) {
	// Arrange, Act.
	got := Sentences([]string{"one moved.", "one moved again.", "one landed."})

	// Assert.
	want := "It changed 3 times since the last notice you were told, oldest first:\n- one moved.\n- one moved again.\n- one landed."
	if got != want {
		t.Fatalf("Sentences = %q, want %q", got, want)
	}
}

func TestSentencesFailsHardOnNoChange(t *testing.T) {
	// Arrange.
	defer func() {
		if r := recover(); r == nil || !strings.Contains(r.(string), "no change") {
			t.Fatalf("Sentences of no change recovered %v, want a panic naming the defect", r)
		}
	}()

	// Act.
	Sentences(nil)
}
