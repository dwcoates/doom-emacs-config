package merge

import (
	"context"
	"errors"
	"fmt"
	"os"
	"path/filepath"
	"sort"
	"strings"
	"testing"
	"time"

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
	if got := strings.Join(names, ","); got != "deploy_started,digest,ending,gate_log,repo_dir,version,window_started" {
		t.Fatalf("the brief was given %s", got)
	}
	if values["window_started"] != deployStarted.UTC().Format(time.RFC3339) || len(window.Groups) != 2 {
		t.Fatalf("window_started %q and %d stored groups; want the deploy's start and both groups kept", values["window_started"], len(window.Groups))
	}
}

func TestAWindowThatCannotBeReadEscalatesTheCheck(t *testing.T) {
	// Arrange
	h := checked(t)
	owe(t, h)
	h.logRecords = func(time.Time, time.Time) ([]LogRecord, error) { return nil, errors.New("logs.sh exited 2") }

	// Act
	h.o.Mediate(context.Background())

	// Assert
	assertBriefFailureEscalated(t, h, "the intake's brief could not be composed", "logs.sh exited 2")
}

func TestDigestSitesGroupsTheWindowByLoggingSite(t *testing.T) {
	// Arrange: one site logging about two workspaces, another at two levels.
	window := []LogRecord{
		rec("warn", "daemon.shim", "tail lag on ws-a"),
		rec("error", "daemon.merge", "boom"),
		rec("warn", "daemon.shim", "tail lag on ws-b"),
		rec("warn", "daemon.merge", "slow"),
		rec("warn", "daemon.shim", "tail lag on ws-c"),
	}

	// Act
	got := DigestSites(window)

	// Assert: errors first, then by count; a site's differing messages are
	// one line, and its sample is its first record.
	var lines []string
	for _, site := range got {
		lines = append(lines, fmt.Sprintf("%s %s %d", site.Level, site.Operation, site.Count))
	}
	if want := "error daemon.merge 1|warn daemon.shim 3|warn daemon.merge 1"; strings.Join(lines, "|") != want {
		t.Fatalf("DigestSites = %v, want %s", lines, want)
	}
	if got[1].Sample != window[0].Raw {
		t.Fatalf("the shim site's sample is %q, want its first record %q", got[1].Sample, window[0].Raw)
	}
}

func TestRenderDigestStatesEachSitesCountAndSample(t *testing.T) {
	cases := []struct {
		name  string
		sites []DigestSite
		left  []LeftOut
		want  []string
	}{
		{name: "a kind that was left out is stated with its count and why",
			left: []LeftOut{{Kind: wsm.IgnoredRecordKind{Operation: "daemon.ghclient.post", MessageContains: "Parent comment not found", Why: "GitHub's own"}, Count: 3}},
			want: []string{"    3  daemon.ghclient.post  \"Parent comment not found\"  GitHub's own\n"}},
		{name: "each site is one table line and one sample",
			sites: []DigestSite{{Level: "error", Operation: "daemon.merge", Count: 2, Sample: `{"operation":"daemon.merge"}`}},
			want:  []string{"    2  error  daemon.merge\n", "\n{\"operation\":\"daemon.merge\"}\n"}},
		{name: "a window with no site still states its span",
			want: []string{"Warnings and errors from 2026-11-19T17:00:00Z through 2026-11-19T18:00:00Z"}},
	}
	since := time.Date(2026, 11, 19, 17, 0, 0, 0, time.UTC)
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Act
			got := RenderDigest(tc.sites, tc.left, since, since.Add(time.Hour))

			// Assert
			for _, want := range tc.want {
				if !strings.Contains(got, want) {
					t.Fatalf("RenderDigest = %q, want it to hold %q", got, want)
				}
			}
		})
	}
}

func TestTheIntakesBriefNamesADigestItWroteOfTheWindow(t *testing.T) {
	// Arrange
	h := checked(t)
	owe(t, h)
	h.logRecords = func(time.Time, time.Time) ([]LogRecord, error) {
		return []LogRecord{rec("error", "daemon.merge", "boom")}, nil
	}
	q, _, _ := h.o.checkedQueueOf(context.Background(), mediationKey(h))

	// Act
	_, _, err := h.o.intakeBrief(context.Background(), q, *owedCheck(t, h))

	// Assert
	if err != nil {
		t.Fatalf("intakeBrief = %v", err)
	}
	path := h.briefValues[len(h.briefValues)-1]["digest"]
	if path != DigestPath(LogsDir(h.stateDir), deployStarted) {
		t.Fatalf("the brief names digest %q, want the check's own", path)
	}
	written, readErr := os.ReadFile(path)
	if readErr != nil || !strings.Contains(string(written), "    1  error  daemon.merge\n") {
		t.Fatalf("the digest reads (%q, %v), want the merge site's line", written, readErr)
	}
}

func TestADigestThatCannotBeWrittenEscalatesTheCheck(t *testing.T) {
	// Arrange: the logs directory's place is taken by a file.
	h := checked(t)
	owe(t, h)
	if err := os.MkdirAll(filepath.Dir(LogsDir(h.stateDir)), 0o755); err != nil {
		t.Fatal(err)
	}
	if err := os.WriteFile(LogsDir(h.stateDir), []byte("in the way"), 0o644); err != nil {
		t.Fatal(err)
	}
	h.logRecords = func(time.Time, time.Time) ([]LogRecord, error) {
		return []LogRecord{rec("error", "daemon.merge", "boom")}, nil
	}

	// Act
	h.o.Mediate(context.Background())

	// Assert
	assertBriefFailureEscalated(t, h, "the intake's brief could not be composed", "")
}

func TestAQuietWindowReleasesTheCheckWithoutTheMediator(t *testing.T) {
	cases := []struct {
		name string
		// window is what every system logged since the last check.
		window []LogRecord
		// wantReleased is whether the check is released with no mediator run.
		wantReleased bool
	}{
		{name: "a window with no warning and no error releases the check", wantReleased: true},
		{name: "a window holding one warning is the mediator's", window: []LogRecord{rec("warn", "daemon.shim", "tail lag")}},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			h := checked(t)
			owe(t, h)
			h.logRecords = func(time.Time, time.Time) ([]LogRecord, error) { return tc.window, nil }

			// Act
			h.o.Mediate(context.Background())

			// Assert
			released := owedCheck(t, h) == nil
			quiet := len(h.recordsFor("info", opMediation, "no system logged a warning or an error in the check's window; the deploy is released and the mediator is not run")) == 1
			if released != tc.wantReleased || quiet != tc.wantReleased {
				t.Fatalf("released %v and recorded quiet %v, want both %v", released, quiet, tc.wantReleased)
			}
			if tc.wantReleased && len(h.queue.submissions) != 0 {
				t.Fatalf("submitted %d briefs, want the mediator never told", len(h.queue.submissions))
			}
		})
	}
}

func TestAQuietReleaseRecordsWhereTheCheckReadTo(t *testing.T) {
	// Arrange
	h := checked(t)
	owe(t, h)
	h.logRecords = func(time.Time, time.Time) ([]LogRecord, error) { return nil, nil }

	// Act
	h.o.Mediate(context.Background())

	// Assert: the next check's window starts where this one read to.
	through, found, err := h.db.CheckedThrough(context.Background(), mediationKey(h))
	if err != nil || !found || through.Before(deployStarted) {
		t.Fatalf("CheckedThrough = (%v, %v, %v), want this check's read recorded", through, found, err)
	}
}

func TestACheckDigestIsOneOfTheQueuesLogs(t *testing.T) {
	// Arrange
	path := DigestPath("/state/merge-logs", time.Unix(1795121928, 0))

	// Act
	isLog := IsLog(filepath.Base(path))

	// Assert: the retention pass prunes it with the queue's other logs.
	if !isLog || filepath.Base(path) != "deploy_check-1795121928.log" {
		t.Fatalf("the digest is %s (a queue log: %v), want deploy_check-1795121928.log pruned with the rest", path, isLog)
	}
}

func TestLeaveOutDropsTheRecordsOfEveryKnownKind(t *testing.T) {
	kinds := []wsm.IgnoredRecordKind{
		{Operation: "daemon.ghclient.post", MessageContains: "Parent comment not found", Why: "GitHub's own"},
		{Operation: "elisp.spell", Why: "user-driven"},
	}
	cases := []struct {
		name     string
		record   LogRecord
		wantKept bool
	}{
		{name: "a record of a kind's operation holding its text is left out", record: rec("error", "daemon.ghclient.post", "404 Parent comment not found")},
		{name: "a record of that operation without the text is kept", record: rec("error", "daemon.ghclient.post", "500 server error"), wantKept: true},
		{name: "every record of an operation named with no text is left out", record: rec("error", "elisp.spell", "anything at all")},
		{name: "a record of another operation holding the text is kept", record: rec("error", "daemon.merge", "Parent comment not found"), wantKept: true},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Act
			kept, left := leaveOut([]LogRecord{tc.record}, kinds)

			// Assert
			if (len(kept) == 1) != tc.wantKept || (len(left) == 1) == tc.wantKept {
				t.Fatalf("leaveOut kept %d and left out %v, want kept %v", len(kept), left, tc.wantKept)
			}
		})
	}
}

func TestLeaveOutCountsWhatEachKindDropped(t *testing.T) {
	// Arrange
	kinds := []wsm.IgnoredRecordKind{{Operation: "elisp.spell", Why: "user-driven"}, {Operation: "never.logged", Why: "unused"}}
	records := []LogRecord{rec("error", "elisp.spell", "a"), rec("error", "elisp.spell", "b"), rec("warn", "daemon.merge", "c")}

	// Act
	kept, left := leaveOut(records, kinds)

	// Assert: one entry, for the kind that matched, with its count.
	if len(kept) != 1 || len(left) != 1 || left[0].Kind.Operation != "elisp.spell" || left[0].Count != 2 {
		t.Fatalf("leaveOut kept %d and left out %+v, want one kept and two spell records left out", len(kept), left)
	}
}

func TestAWindowHoldingOnlyIgnoredKindsReleasesTheCheckWithoutTheMediator(t *testing.T) {
	// Arrange: the window's only record is of a kind the checks leave out.
	h := checked(t)
	owe(t, h)
	if err := h.db.IgnoreRecordKind(context.Background(), wsm.IgnoredRecordKind{Operation: "daemon.shim", Why: "passes on its own", AddedAt: deployStarted}); err != nil {
		t.Fatal(err)
	}
	h.logRecords = func(time.Time, time.Time) ([]LogRecord, error) {
		return []LogRecord{rec("warn", "daemon.shim", "tail lag")}, nil
	}

	// Act
	h.o.Mediate(context.Background())

	// Assert
	if owedCheck(t, h) != nil || len(h.queue.submissions) != 0 {
		t.Fatalf("submitted %d briefs with the check still owed %v; want it released and the mediator never told", len(h.queue.submissions), owedCheck(t, h) != nil)
	}
}

func TestOnlyADeployMediatorChangesTheIgnoredRecords(t *testing.T) {
	kind := wsm.IgnoredRecordKind{Operation: "daemon.shim", MessageContains: "tail lag", Why: "passes on its own"}
	cases := []struct {
		name string
		// mediator is whether the asking workspace is the queue's mediator.
		mediator bool
		// change runs one change as workspace ws-fixer.
		change   func(h *harness) error
		wantKept int
	}{
		{name: "the mediator records a kind", mediator: true, wantKept: 1,
			change: func(h *harness) error { return h.o.IgnoreRecords(context.Background(), "ws-fixer", kind) }},
		{name: "the mediator withdraws a kind", mediator: true,
			change: func(h *harness) error {
				if err := h.o.IgnoreRecords(context.Background(), "ws-fixer", kind); err != nil {
					return err
				}
				return h.o.UnignoreRecords(context.Background(), "ws-fixer", kind.Operation, kind.MessageContains)
			}},
		{name: "a workspace that is no mediator is refused, and nothing is recorded",
			change: func(h *harness) error { return h.o.IgnoreRecords(context.Background(), "ws-fixer", kind) }},
		{name: "a workspace that is no mediator cannot withdraw one",
			change: func(h *harness) error {
				return h.o.UnignoreRecords(context.Background(), "ws-fixer", kind.Operation, kind.MessageContains)
			}},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			h := checked(t)
			if tc.mediator {
				owe(t, h)
				withMediator(t, h)
			}
			seeded, err := h.db.IgnoredRecordKinds(context.Background())
			if err != nil {
				t.Fatal(err)
			}

			// Act
			err = tc.change(h)

			// Assert
			var refused *RefusalError
			if tc.mediator == errors.As(err, &refused) || (tc.mediator && err != nil) {
				t.Fatalf("the change = %v, want refused %v", err, !tc.mediator)
			}
			if !tc.mediator && refused.Arm != ArmNotMediator {
				t.Fatalf("refused as %s, want %s", refused.Arm, ArmNotMediator)
			}
			kinds, readErr := h.db.IgnoredRecordKinds(context.Background())
			if readErr != nil || len(kinds)-len(seeded) != tc.wantKept {
				t.Fatalf("kinds went from %d to %d (%v), want %d more", len(seeded), len(kinds), readErr, tc.wantKept)
			}
		})
	}
}
