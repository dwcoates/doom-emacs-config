package workspace

import (
	"context"
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
		},
		{
			name:       "a brief whose placeholders do not match is an error naming it as unsplicable",
			brief:      &prompts.Prompt{Name: composeBriefName, Body: "hello {{whom}}", Placeholders: []string{"whom"}},
			wantErr:    "the " + composeBriefName + " brief will not splice",
			wantRecord: "could not splice a daemon brief",
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
			if len(f.health.opened) != 1 || f.health.opened[0].Kind != health.KindPromptsDirMissing {
				t.Fatalf("opened faults = %+v, want one prompts_dir_missing fault", f.health.opened)
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
