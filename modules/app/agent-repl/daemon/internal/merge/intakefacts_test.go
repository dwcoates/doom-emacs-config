package merge

import (
	"context"
	"errors"
	"fmt"
	"sort"
	"strings"
	"testing"
	"time"

	"agent-repld/internal/dlog"
)

// rec is a log record of OP and MSG at LEVEL, its raw line naming them.
func rec(level, op, msg string) LogRecord {
	return LogRecord{Level: level, Operation: op, Message: msg, Raw: fmt.Sprintf(`{"level":%q,"operation":%q,"message":%q}`, level, op, msg)}
}

func TestParseLogLinesKeepsEachRecordVerbatim(t *testing.T) {
	tests := []struct {
		name    string
		text    string
		want    []LogRecord
		wantErr bool
	}{
		{name: "records, with the reader's own account between them", text: "{\"level\":\"warn\",\"operation\":\"a\",\"message\":\"m\"}\nlogs: sink unreadable\n\n",
			want: []LogRecord{{Level: "warn", Operation: "a", Message: "m", Raw: `{"level":"warn","operation":"a","message":"m"}`}}},
		{name: "nothing logged", text: ""},
		{name: "a record that does not decode", text: "{\"level\":", wantErr: true},
		{name: "a record marked transient is left out", text: "{\"level\":\"warn\",\"operation\":\"daemon.ghclient.get\",\"message\":\"GitHub could not be reached\",\"context\":{\"transient\":true}}\n"},
		{name: "a record marked not transient is kept",
			text: "{\"level\":\"warn\",\"operation\":\"a\",\"message\":\"m\",\"context\":{\"transient\":false}}\n",
			want: []LogRecord{{Level: "warn", Operation: "a", Message: "m", Raw: `{"level":"warn","operation":"a","message":"m","context":{"transient":false}}`}}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			got, err := ParseLogLines(tt.text)

			// Assert
			if (err != nil) != tt.wantErr || fmt.Sprint(got) != fmt.Sprint(tt.want) {
				t.Fatalf("ParseLogLines = (%+v, %v), want %+v (error %v)", got, err, tt.want, tt.wantErr)
			}
		})
	}
}

func TestSortFindingsMarksEachGroupNewOrSeenBefore(t *testing.T) {
	// Arrange
	window := []LogRecord{
		rec("warn", "daemon.reap", "slow pass"),
		rec("error", "daemon.merge", "boom"),
		rec("warn", "daemon.reap", "slow pass"),
		rec("warn", "daemon.shim", "tail lag"),
		rec("error", "daemon.reap", "slow pass"),
	}
	before := []LogRecord{rec("warn", "daemon.shim", "tail lag"), rec("warn", "daemon.shim", "tail lag")}

	// Act
	got := SortFindings(window, before)

	// Assert: NEW first, the most severe and frequent first among them; the
	// group's level is its most severe record's.
	want := []FindingGroup{
		{Operation: "daemon.reap", Message: "slow pass", Level: "error", Count: 3, Sample: rec("warn", "daemon.reap", "slow pass").Raw},
		{Operation: "daemon.merge", Message: "boom", Level: "error", Count: 1, Sample: rec("error", "daemon.merge", "boom").Raw},
		{Operation: "daemon.shim", Message: "tail lag", Level: "warn", Count: 1, Before: 2, Sample: rec("warn", "daemon.shim", "tail lag").Raw},
	}
	if fmt.Sprint(got) != fmt.Sprint(want) {
		t.Fatalf("SortFindings = %+v\nwant %+v", got, want)
	}
}

func TestTheIntakesBriefSortsTheWindowAgainstTheSpanBeforeTheDeploy(t *testing.T) {
	// Arrange
	h := checked(t)
	owe(t, h)
	var spans [][2]time.Time
	h.logRecords = func(since, until time.Time) ([]LogRecord, error) {
		spans = append(spans, [2]time.Time{since, until})
		if until.Equal(deployStarted) {
			return []LogRecord{rec("warn", "daemon.shim", "tail lag")}, nil
		}
		return []LogRecord{rec("warn", "daemon.shim", "tail lag"), rec("error", "daemon.merge", "boom")}, nil
	}
	q, ok, err := h.o.checkedQueueOf(context.Background(), mediationKey(h))
	if err != nil || !ok {
		t.Fatalf("checkedQueueOf = (%v, %v)", ok, err)
	}

	// Act
	_, _, err = h.o.intakeBrief(context.Background(), q, *owedCheck(t, h))

	// Assert
	if err != nil || len(spans) != 2 {
		t.Fatalf("intakeBrief = %v after reading %v, want both spans read", err, spans)
	}
	window, before := spans[0], spans[1]
	if !window[0].Equal(deployStarted) || !before[1].Equal(deployStarted) || before[1].Sub(before[0]) != window[1].Sub(window[0]) {
		t.Fatalf("read window %v and before %v, want equal spans either side of the deploy", window, before)
	}
}

func TestTheIntakesBriefStatesTheDeployAndNothingTheWindowHolds(t *testing.T) {
	// Arrange: a window holding a warning and an error.
	h := checked(t)
	owe(t, h)
	h.logRecords = func(time.Time, time.Time) ([]LogRecord, error) {
		return []LogRecord{rec("warn", "daemon.shim", "tail lag"), rec("error", "daemon.merge", "boom")}, nil
	}
	q, _, _ := h.o.checkedQueueOf(context.Background(), mediationKey(h))

	// Act
	_, window, err := h.o.intakeBrief(context.Background(), q, *owedCheck(t, h))

	// Assert: the brief's values name the deploy and where to look, and none
	// of them carries a record; the window the store keeps still holds both
	// groups.
	if err != nil {
		t.Fatalf("intakeBrief = %v", err)
	}
	values := h.briefValues[len(h.briefValues)-1]
	var names []string
	for name, value := range values {
		names = append(names, name)
		if strings.Contains(value, "boom") || strings.Contains(value, "tail lag") {
			t.Fatalf("the brief's %s carries a record of the window: %q", name, value)
		}
	}
	sort.Strings(names)
	if got := strings.Join(names, ","); got != "deploy_started,ending,gate_log,repo_dir,version,window_started" {
		t.Fatalf("the brief was given %s", got)
	}
	if values["window_started"] != deployStarted.UTC().Format(time.RFC3339) || len(window.Groups) != 2 {
		t.Fatalf("window_started %q and %d stored groups; want the deploy's start and both groups kept", values["window_started"], len(window.Groups))
	}
}

func TestAWindowThatCannotBeReadKeepsTheQueueHeld(t *testing.T) {
	// Arrange
	h := checked(t)
	owe(t, h)
	h.logRecords = func(time.Time, time.Time) ([]LogRecord, error) { return nil, errors.New("logs.sh exited 2") }

	// Act
	h.o.Mediate(context.Background())

	// Assert
	if owedCheck(t, h) == nil || len(h.queue.submissions) != 0 {
		t.Fatalf("submitted %d; want no brief and the check still owed", len(h.queue.submissions))
	}
	if _, ok := dlog.FindRecord(h.logs.Records(), dlog.RecordQuery{Level: "error", Operation: opMediation, Message: "the intake's brief could not be composed; the queue stays held"}); !ok {
		t.Fatalf("records = %v, want the failed brief at ERROR", h.logs.Records())
	}
}
