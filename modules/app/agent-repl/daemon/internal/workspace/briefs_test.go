package workspace

import (
	"context"
	"errors"
	"strings"
	"testing"

	"agent-repld/internal/dlog"
	"agent-repld/internal/health"
	"agent-repld/internal/prompts"
	"agent-repld/internal/sourcescan"
)

// composeBriefName is the brief the composeBrief tests arrange.
const composeBriefName = "compose-brief-test"

func TestComposeBrief(t *testing.T) {
	cases := []struct {
		name string
		// brief is the arranged brief; nil leaves it missing.
		brief *prompts.Prompt
		// wantText is the composed text on success; empty means a failure.
		wantText string
		// wantErr is a fragment the failure's error must carry.
		wantErr string
		// wantRecord is the message of the ERROR record a failure leaves.
		wantRecord string
		// wantKind is the fault a failure raises.
		wantKind string
	}{
		{
			name:     "a brief that loads and splices answers the spliced text",
			brief:    &prompts.Prompt{Name: composeBriefName, Body: "hello {{who}}", Placeholders: []string{"who"}},
			wantText: "hello world",
		},
		{
			name:       "a missing brief is an error naming it as unreadable",
			wantErr:    "the " + composeBriefName + " brief is unreadable",
			wantRecord: "could not read a daemon brief",
			wantKind:   health.KindPromptsDirMissing,
		},
		{
			name:       "a brief whose placeholders do not match is an error naming it as unsplicable",
			brief:      &prompts.Prompt{Name: composeBriefName, Body: "hello {{whom}}", Placeholders: []string{"whom"}},
			wantErr:    "the " + composeBriefName + " brief will not splice",
			wantRecord: "could not splice a daemon brief",
			wantKind:   health.KindBriefUnsplicable,
		},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			f := newFixture(t)
			if tc.brief != nil {
				f.briefs[composeBriefName] = *tc.brief
			}
			v := f.verbs.(*verbs)
			log := f.log.Global()

			// Act.
			text, err := v.composeBrief(context.Background(), log, "daemon.workspace.compose_test", composeBriefName, map[string]string{"who": "world"})

			// Assert.
			if tc.wantErr == "" {
				if err != nil {
					t.Fatalf("composeBrief: %v", err)
				}
				if text != tc.wantText {
					t.Fatalf("text = %q, want %q", text, tc.wantText)
				}
				if len(f.health.opened) != 0 {
					t.Fatalf("opened faults = %+v, want none for a brief that composed", f.health.opened)
				}
				return
			}
			if err == nil || !strings.Contains(err.Error(), tc.wantErr) {
				t.Fatalf("err = %v, want one containing %q", err, tc.wantErr)
			}
			if text != "" {
				t.Fatalf("text = %q, want none beside an error", text)
			}
			record, ok := dlog.FindRecord(f.log.logger.Records(), dlog.RecordQuery{Level: dlog.LevelError, Operation: "daemon.workspace.compose_test"})
			if !ok || record.Message != tc.wantRecord {
				t.Fatalf("records = %+v, want an ERROR %q record under the caller's operation", f.log.logger.Records(), tc.wantRecord)
			}
			if record.Context["brief"] != composeBriefName {
				t.Fatalf("record brief = %v, want %q", record.Context["brief"], composeBriefName)
			}
			if len(f.health.opened) != 1 || f.health.opened[0].Kind != tc.wantKind || f.health.opened[0].Workspace != nil {
				t.Fatalf("opened faults = %+v, want one daemon-scoped %s fault", f.health.opened, tc.wantKind)
			}
		})
	}
}

// TestComposeBriefClosesAStandingPromptsFault pins that a brief that composes
// again is the health probe: the condition the fault records no longer holds.
func TestComposeBriefClosesAStandingPromptsFault(t *testing.T) {
	// Arrange: a fault stands from an earlier failed read.
	f := newFixture(t)
	v := f.verbs.(*verbs)
	log := f.log.Global()
	if _, err := v.composeBrief(context.Background(), log, "daemon.workspace.compose_test", composeBriefName, nil); err == nil {
		t.Fatal("composeBrief of a missing brief = success, want an error")
	}
	f.briefs[composeBriefName] = prompts.Prompt{Name: composeBriefName, Body: "hello"}

	// Act.
	if _, err := v.composeBrief(context.Background(), log, "daemon.workspace.compose_test", composeBriefName, nil); err != nil {
		t.Fatalf("composeBrief after the brief returned: %v", err)
	}

	// Assert.
	if len(f.health.closed) != 1 {
		t.Fatalf("closed faults = %v, want exactly the standing prompts-directory fault", f.health.closed)
	}
}

// unsplicable is a brief whose placeholder the values handed it do not cover.
func unsplicable(name string) prompts.Prompt {
	return prompts.Prompt{Name: name, Body: "hello {{whom}}", Placeholders: []string{"whom"}}
}

// TestASpliceFailureNamesItsBrief pins the fault a splice raises to the brief
// it is about, which no prompts-directory reader would mistake for the
// directory being gone.
func TestASpliceFailureNamesItsBrief(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.briefs[composeBriefName] = unsplicable(composeBriefName)
	v := f.verbs.(*verbs)

	// Act.
	_, err := v.composeBrief(context.Background(), f.log.Global(), "daemon.workspace.compose_test", composeBriefName, map[string]string{"who": "world"})

	// Assert.
	if err == nil {
		t.Fatal("composeBrief = success, want the splice's failure")
	}
	if got := f.health.opened; len(got) != 1 || got[0].Evidence["brief"] != composeBriefName || !strings.Contains(got[0].Evidence["cause"], "no value for placeholder whom") {
		t.Fatalf("opened faults = %+v, want one naming the brief and the splice's cause", got)
	}
}

func TestASpliceFailureOpensOneFaultPerBrief(t *testing.T) {
	cases := []struct {
		name   string
		second string
		want   int
	}{
		{name: "the same brief failing again opens no second fault", second: composeBriefName, want: 1},
		{name: "another brief failing opens its own", second: "another-brief", want: 2},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			f := newFixture(t)
			f.briefs[composeBriefName] = unsplicable(composeBriefName)
			f.briefs["another-brief"] = unsplicable("another-brief")
			v := f.verbs.(*verbs)
			log := f.log.Global()
			_, _ = v.composeBrief(context.Background(), log, "daemon.workspace.compose_test", composeBriefName, nil)

			// Act.
			_, _ = v.composeBrief(context.Background(), log, "daemon.workspace.compose_test", tc.second, nil)

			// Assert.
			if len(f.health.opened) != tc.want {
				t.Fatalf("opened faults = %+v, want %d", f.health.opened, tc.want)
			}
		})
	}
}

func TestComposeBriefClosesOnlyItsOwnSpliceFault(t *testing.T) {
	cases := []struct {
		name     string
		composed string
		want     int
	}{
		{name: "the brief composing closes its fault", composed: composeBriefName, want: 1},
		{name: "another brief composing leaves it standing", composed: "another-brief", want: 0},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange: a fault stands from an earlier failed splice.
			f := newFixture(t)
			f.briefs[composeBriefName] = unsplicable(composeBriefName)
			v := f.verbs.(*verbs)
			log := f.log.Global()
			_, _ = v.composeBrief(context.Background(), log, "daemon.workspace.compose_test", composeBriefName, nil)
			f.briefs[composeBriefName] = prompts.Prompt{Name: composeBriefName, Body: "hello"}
			f.briefs["another-brief"] = prompts.Prompt{Name: "another-brief", Body: "hello"}

			// Act.
			if _, err := v.composeBrief(context.Background(), log, "daemon.workspace.compose_test", tc.composed, nil); err != nil {
				t.Fatalf("composeBrief: %v", err)
			}

			// Assert.
			if len(f.health.closed) != tc.want {
				t.Fatalf("closed faults = %v, want %d", f.health.closed, tc.want)
			}
		})
	}
}

func TestASpliceFaultThatCannotBeRecordedIsLogged(t *testing.T) {
	cases := []struct {
		name    string
		fail    func(h *fakeHealth)
		message string
	}{
		{name: "the standing faults cannot be read", fail: func(h *fakeHealth) { h.listErr = errors.New("the state database is locked") },
			message: "could not check whether the daemon fault already stands; it is recorded anyway"},
		{name: "the fault cannot be opened", fail: func(h *fakeHealth) { h.openErr = errors.New("the state database is locked") },
			message: "could not record the daemon fault"},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			f := newFixture(t)
			f.briefs[composeBriefName] = unsplicable(composeBriefName)
			tc.fail(f.health)
			v := f.verbs.(*verbs)

			// Act.
			_, err := v.composeBrief(context.Background(), f.log.Global(), "daemon.workspace.compose_test", composeBriefName, nil)

			// Assert.
			if err == nil {
				t.Fatal("composeBrief = success, want the splice's failure")
			}
			record, ok := dlog.FindRecord(f.log.logger.Records(), dlog.RecordQuery{Level: dlog.LevelError, Operation: opBriefFault})
			if !ok || record.Message != tc.message || record.Context["brief"] != composeBriefName || record.Context["cause"] == nil {
				t.Fatalf("records = %+v, want an ERROR %q naming the brief and the cause", f.log.logger.Records(), tc.message)
			}
		})
	}
}

// TestEveryPromptsFaultIsRaisedThroughComposeBrief pins that the brief sites
// share composeBrief's shape: a hand-rolled load-splice-raise in any other file
// would drift from it (another log message, a fault left open), so raising the
// prompts fault anywhere but the helper fails here.
func TestEveryPromptsFaultIsRaisedThroughComposeBrief(t *testing.T) {
	// Arrange / Act.
	var raisers []string
	for _, file := range sourcescan.Production(t) {
		if file.Name != "briefs.go" && strings.Contains(string(file.Source), "v.raisePromptsFault(") {
			raisers = append(raisers, file.Name)
		}
	}

	// Assert.
	if len(raisers) != 0 {
		t.Fatalf("files raising the prompts fault outside composeBrief = %v, want none", raisers)
	}
}
