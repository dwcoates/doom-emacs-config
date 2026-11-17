package merge

import (
	"context"
	"errors"
	"fmt"
	"strings"
	"testing"
	"time"

	"agent-repld/internal/dlog"
	"agent-repld/internal/wsm"
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

func TestRenderFindingsQuotesEachGroupsSample(t *testing.T) {
	tests := []struct {
		name   string
		groups []FindingGroup
		want   []string
	}{
		{name: "no findings says so", want: []string{"No system logged a warning or an error in the window."}},
		{name: "a new and a seen group", groups: []FindingGroup{
			{Operation: "a", Message: "m", Level: "error", Count: 2, Sample: "{raw-a}", Class: wsm.WindowNew},
			{Operation: "b", Message: "n", Level: "warn", Count: 1, Before: 3, Sample: "{raw-b}", Class: wsm.WindowSeenBefore},
		}, want: []string{"- NEW: 2 ERROR of a: m\n  {raw-a}", "- SEEN BEFORE, owned by nobody (3 in the span before): 1 WARN of b: n\n  {raw-b}"}},
		{name: "an owned group names its owner and what it was told", groups: []FindingGroup{
			{Operation: "c", Message: "o", Level: "warn", Count: 1, Before: 1, Sample: "{raw-c}", Class: wsm.WindowOwned, Owner: "deploy-remediation-1", Findings: "the sink warning"},
		}, want: []string{"- OWNED by deploy-remediation-1, still at work on it: 1 WARN of c: o", "  Its owners were told: the sink warning"}},
		{name: "a recurred group names whose fix it outlived", groups: []FindingGroup{
			{Operation: "d", Message: "p", Level: "warn", Count: 4, Before: 2, Sample: "{raw-d}", Class: wsm.WindowRecurred, Owner: "deploy-remediation-1", Findings: "first\nsecond"},
		}, want: []string{"- RECURRED after deploy-remediation-1's fix deployed: 4 WARN of d: p", "  Its owners were told: first / second"}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			got := RenderFindings(tt.groups)

			// Assert
			for _, w := range tt.want {
				if !strings.Contains(got, w) {
					t.Fatalf("RenderFindings = %q, want it to hold %q", got, w)
				}
			}
		})
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
	values := h.briefValues[len(h.briefValues)-1]
	if !strings.Contains(values["facts"], "- NEW: 1 ERROR of daemon.merge: boom") || !strings.Contains(values["facts"], "- SEEN BEFORE, owned by nobody (1 in the span before): 1 WARN of daemon.shim") {
		t.Fatalf("facts = %q, want the merge error NEW and the shim warning SEEN BEFORE", values["facts"])
	}
	if values["bound"] != fmt.Sprint(theBound) || !strings.Contains(values["chain"], "first deploy") {
		t.Fatalf("bound %q, chain %q; want the repository's bound and a first deploy", values["bound"], values["chain"])
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

func TestTheChainSoFarNamesWhatEachRemediationLanded(t *testing.T) {
	// Arrange: one remediation dispatched and landed, its fix deployed.
	h := checked(t)
	ctx := context.Background()
	owe(t, h)
	withIntake(t, h, deployStarted)
	if _, err := h.db.DispatchMediation(ctx, mediationKey(h), deployStarted, deployStarted, "ws-fixer", aFixerTurn); err != nil {
		t.Fatal(err)
	}
	h.db.mu.Lock()
	h.db.workspaces["ws-fixer"] = wsm.Workspace{ID: "ws-fixer", Name: "deploy-remediation-x", Repo: theRepository}
	h.db.mu.Unlock()
	if _, err := h.db.BeginRemediationFinish(ctx, wsm.RemediationFinish{Workspace: "ws-fixer", Repo: mediationKey(h), StartedAt: deployStarted}); err != nil {
		t.Fatal(err)
	}
	if _, err := h.db.AdvanceRemediationFinish(ctx, "ws-fixer", wsm.RemediationGating, wsm.RemediationLanding, deployStarted); err != nil {
		t.Fatal(err)
	}
	if _, err := h.db.RecordRemediationLanded(ctx, "ws-fixer", "abc123", []string{"daemon/x.go"}, deployStarted); err != nil {
		t.Fatal(err)
	}
	oweAt(t, h, deployStarted.Add(time.Minute))
	q, _, _ := h.o.checkedQueueOf(ctx, mediationKey(h))

	// Act
	got, err := h.o.chainSoFar(ctx, q, *owedCheck(t, h))

	// Assert
	for _, want := range []string{"deployed 1 remediations in a row", "last remediation workspace is deploy-remediation-x", "deploy-remediation-x landed abc123, changing daemon/x.go."} {
		if err != nil || !strings.Contains(got, want) {
			t.Fatalf("chainSoFar = (%q, %v), want it to hold %q", got, err, want)
		}
	}
}

func TestTheChainSoFarWhoseRemediationNameCannotBeReadIsAnError(t *testing.T) {
	// Arrange
	h := checked(t)
	ctx := context.Background()
	owe(t, h)
	withIntake(t, h, deployStarted)
	if _, err := h.db.DispatchMediation(ctx, mediationKey(h), deployStarted, deployStarted, "ws-fixer", aFixerTurn); err != nil {
		t.Fatal(err)
	}
	q, _, _ := h.o.checkedQueueOf(ctx, mediationKey(h))
	m := *owedCheck(t, h)
	h.db.mu.Lock()
	h.db.workspaceErr = errors.New("db gone")
	h.db.mu.Unlock()

	// Act
	got, err := h.o.chainSoFar(ctx, q, m)

	// Assert
	if err == nil || got != "" {
		t.Fatalf("chainSoFar = (%q, %v), want an error and no account", got, err)
	}
	if rec, ok := h.recordFor("error", opMediation); !ok || rec.Context["workspace"] != "ws-fixer" {
		t.Fatalf("ERROR record = %+v (%v), want the unread name recorded", rec, ok)
	}
}
