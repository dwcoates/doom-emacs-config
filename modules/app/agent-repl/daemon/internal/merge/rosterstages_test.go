package merge

import (
	"strings"
	"testing"
	"time"

	frontendv1 "agentrepl/proto/frontend/v1"

	"agent-repld/internal/wsm"
)

// at is the instant MS milliseconds into the epoch.
func at(ms int64) time.Time { return time.UnixMilli(ms) }

// endedAt is a pointer to the instant MS milliseconds into the epoch.
func endedAt(ms int64) *time.Time {
	t := at(ms)
	return &t
}

// stageShape is one drawn stage reduced to what a test compares: its label,
// round, state arm and the start and end it carries.
type stageShape struct {
	label   string
	round   uint32
	state   string
	started int64
	ended   int64
}

func shapeOf(st *frontendv1.RosterMergeQueueStage) stageShape {
	out := stageShape{label: st.GetLabel(), round: st.GetRound()}
	switch s := st.GetState().(type) {
	case *frontendv1.RosterMergeQueueStage_Live:
		out.state, out.started = "live", s.Live.GetStartedAtMs()
	case *frontendv1.RosterMergeQueueStage_Succeeded:
		out.state, out.started, out.ended = "succeeded", s.Succeeded.GetStartedAtMs(), s.Succeeded.GetEndedAtMs()
	case *frontendv1.RosterMergeQueueStage_Failed:
		out.state, out.started, out.ended = "failed", s.Failed.GetStartedAtMs(), s.Failed.GetEndedAtMs()
	}
	return out
}

func TestStagesOf(t *testing.T) {
	tests := []struct {
		name      string
		intervals []wsm.TabInterval
		want      []stageShape
	}{
		{
			name:      "a live round is a live stage",
			intervals: []wsm.TabInterval{{Round: 1, Kind: TabQueue, StartedAt: at(10)}},
			want:      []stageShape{{label: "queue", round: 1, state: "live", started: 10}},
		},
		{
			name:      "a succeeded round is a succeeded stage",
			intervals: []wsm.TabInterval{{Round: 1, Kind: TabQueue, StartedAt: at(10), EndedAt: endedAt(20), Outcome: "succeeded"}},
			want:      []stageShape{{label: "queue", round: 1, state: "succeeded", started: 10, ended: 20}},
		},
		{
			name: "a failed, restarted or abandoned round is a failed stage",
			intervals: []wsm.TabInterval{
				{Round: 1, Kind: TabCommitting, StartedAt: at(10), EndedAt: endedAt(20), Outcome: "restarted"},
				{Round: 2, Kind: TabCommitting, StartedAt: at(30), EndedAt: endedAt(40), Outcome: "failed"},
				{Round: 1, Kind: TabDeploy, StartedAt: at(50), EndedAt: endedAt(60), Outcome: "abandoned"},
			},
			want: []stageShape{
				{label: "committing", round: 1, state: "failed", started: 10, ended: 20},
				{label: "committing", round: 2, state: "failed", started: 30, ended: 40},
				{label: "deploy", round: 1, state: "failed", started: 50, ended: 60},
			},
		},
		{
			name: "a conflict resolution is part of one rebasing stage, live while it runs",
			intervals: []wsm.TabInterval{
				{Round: 1, Kind: TabRebasing, StartedAt: at(10)},
				{Round: 1, Kind: TabConflicts, StartedAt: at(15)},
			},
			want: []stageShape{{label: "rebasing", round: 1, state: "live", started: 10}},
		},
		{
			name: "every tests and fixing round is one tests stage, settled as its latest round",
			intervals: []wsm.TabInterval{
				{Round: 1, Kind: TabTests, StartedAt: at(10), EndedAt: endedAt(20), Outcome: "failed"},
				{Round: 1, Kind: TabFixes, StartedAt: at(20), EndedAt: endedAt(30), Outcome: "succeeded"},
				{Round: 2, Kind: TabTests, StartedAt: at(30), EndedAt: endedAt(40), Outcome: "succeeded"},
			},
			want: []stageShape{{label: "tests", round: 1, state: "succeeded", started: 10, ended: 40}},
		},
		{
			name: "stages are drawn in the order each began, whatever the ledger's order",
			intervals: []wsm.TabInterval{
				{Round: 1, Kind: TabCommitting, StartedAt: at(30)},
				{Round: 1, Kind: TabQueue, StartedAt: at(10), EndedAt: endedAt(20), Outcome: "succeeded"},
				{Round: 1, Kind: TabPick, StartedAt: at(20), EndedAt: endedAt(30), Outcome: "succeeded"},
			},
			want: []stageShape{
				{label: "queue", round: 1, state: "succeeded", started: 10, ended: 20},
				{label: "resolving", round: 1, state: "succeeded", started: 20, ended: 30},
				{label: "committing", round: 1, state: "live", started: 30},
			},
		},
		{
			name: "every deploy stage is drawn in its own words",
			intervals: []wsm.TabInterval{
				{Round: 1, Kind: TabPreDeploy, StartedAt: at(10), EndedAt: endedAt(20), Outcome: "succeeded"},
				{Round: 1, Kind: TabPostDeploy, StartedAt: at(30)},
			},
			want: []stageShape{
				{label: stageWords(TabPreDeploy), round: 1, state: "succeeded", started: 10, ended: 20},
				{label: stageWords(TabPostDeploy), round: 1, state: "live", started: 30},
			},
		},
		{
			name: "no intervals draw no stage",
			want: []stageShape{},
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Act
			got, err := stagesOf(tc.intervals)

			// Assert
			if err != nil {
				t.Fatalf("stagesOf: %v", err)
			}
			shapes := make([]stageShape, 0, len(got))
			for _, st := range got {
				shapes = append(shapes, shapeOf(st))
			}
			if len(shapes) != len(tc.want) {
				t.Fatalf("stages = %+v, want %+v", shapes, tc.want)
			}
			for i := range shapes {
				if shapes[i] != tc.want[i] {
					t.Fatalf("stage %d = %+v, want %+v", i, shapes[i], tc.want[i])
				}
			}
		})
	}
}

func TestStagesOfRefusesWhatTheLedgerHasNoBusinessHolding(t *testing.T) {
	tests := []struct {
		name      string
		intervals []wsm.TabInterval
		wantErr   string
	}{
		{
			name:      "an unknown kind",
			intervals: []wsm.TabInterval{{Round: 1, Kind: "rollout", StartedAt: at(10)}},
			wantErr:   `kind "rollout"`,
		},
		{
			name:      "an unknown outcome",
			intervals: []wsm.TabInterval{{Round: 1, Kind: TabQueue, StartedAt: at(10), EndedAt: endedAt(20), Outcome: "vanished"}},
			wantErr:   `outcome "vanished"`,
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Act
			got, err := stagesOf(tc.intervals)

			// Assert
			if err == nil || !strings.Contains(err.Error(), tc.wantErr) {
				t.Fatalf("stagesOf = %v, %v, want an error naming %s", got, err, tc.wantErr)
			}
		})
	}
}
