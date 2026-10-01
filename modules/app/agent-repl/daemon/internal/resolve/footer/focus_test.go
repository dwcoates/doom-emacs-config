package footer

import (
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"
)

// focusRead is the published focus as a reader sees it: the panel's name and
// the generation, with an unset focus reading "none" at generation 0.
type focusRead struct {
	panel      string
	generation uint64
}

// focusOfView reads the focus off a published view.
func focusOfView(view *frontendv1.FooterView) focusRead {
	f := view.GetFocus()
	if f == nil {
		return focusRead{panel: "none"}
	}
	switch f.GetPanel().(type) {
	case *frontendv1.FooterExpandedFocus_Agents:
		return focusRead{panel: "agents", generation: f.GetGeneration()}
	case *frontendv1.FooterExpandedFocus_Shells:
		return focusRead{panel: "shells", generation: f.GetGeneration()}
	case *frontendv1.FooterExpandedFocus_Monitors:
		return focusRead{panel: "monitors", generation: f.GetGeneration()}
	case *frontendv1.FooterExpandedFocus_MergeTests:
		return focusRead{panel: "merge_tests", generation: f.GetGeneration()}
	default:
		return focusRead{panel: "unset arm", generation: f.GetGeneration()}
	}
}

// TestALaunchFocusesTheHighestPriorityLiveKind is the owner's rule: each
// launch of detached work focuses AGENTS, then SHELLS, then MONITORS, whichever
// is the highest kind live once it has started, under a new generation.
func TestALaunchFocusesTheHighestPriorityLiveKind(t *testing.T) {
	tests := []struct {
		name    string
		arrange []LiveWorkSet
		act     LiveWorkSet
		want    focusRead
	}{
		{
			name: "a subagent launch focuses agents",
			act:  liveSet([]string{"agent-1"}, nil, nil),
			want: focusRead{"agents", 1},
		},
		{
			name: "a shell launch with no subagent live focuses shells",
			act:  liveSet(nil, []string{"shell-1"}, nil),
			want: focusRead{"shells", 1},
		},
		{
			name: "a monitor launch with neither live focuses monitors",
			act:  liveSet(nil, nil, []string{"monitor-1"}),
			want: focusRead{"monitors", 1},
		},
		{
			name:    "a shell launch while a subagent is live re-focuses agents",
			arrange: []LiveWorkSet{liveSet([]string{"agent-1"}, nil, nil)},
			act:     liveSet([]string{"agent-1"}, []string{"shell-1"}, nil),
			want:    focusRead{"agents", 2},
		},
		{
			name:    "a subagent launch while a shell runs focuses agents",
			arrange: []LiveWorkSet{liveSet(nil, []string{"shell-1"}, nil)},
			act:     liveSet([]string{"agent-1"}, []string{"shell-1"}, nil),
			want:    focusRead{"agents", 2},
		},
		{
			name:    "a monitor launch while a shell runs focuses shells",
			arrange: []LiveWorkSet{liveSet(nil, []string{"shell-1"}, nil)},
			act:     liveSet(nil, []string{"shell-1"}, []string{"monitor-1"}),
			want:    focusRead{"shells", 2},
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			connected(h)
			for _, set := range tc.arrange {
				announceMain(h, set)
				h.r.OnLiveWorkChanged(testWS, set)
			}
			announceMain(h, tc.act)

			// Act
			h.r.OnLiveWorkChanged(testWS, tc.act)

			// Assert
			if got := focusOfView(h.view(t)); got != tc.want {
				t.Fatalf("focus = %+v, want %+v", got, tc.want)
			}
		})
	}
}

// TestALaunchAnnouncedBeforeTheSetStillFocuses is the ordinary delivery order:
// the watcher hands the footer the announcement BEFORE the set that lists it,
// so the row already stands when the set arrives. The launch is read off the
// set against the previous set, not off the rows the set had to open.
func TestALaunchAnnouncedBeforeTheSetStillFocuses(t *testing.T) {
	// Arrange: the announcement opens the row.
	h := newHarness(t)
	connected(h)
	h.r.OnDetachedWork(testWS, mainAgent, createdShell("shell-1", "npm test"))

	// Act
	h.r.OnLiveWorkChanged(testWS, liveSet(nil, []string{"shell-1"}, nil))

	// Assert
	if got, want := focusOfView(h.view(t)), (focusRead{"shells", 1}); got != want {
		t.Fatalf("focus = %+v, want %+v", got, want)
	}
}

// TestAChangeThatLaunchesNothingKeepsTheStandingFocus is the edge half of the
// rule: between launches the standing focus is left exactly as it is, so the
// user's own chip clicks stand. Each case arranges one launch (shells, 1) and
// then delivers something that is NOT a launch.
func TestAChangeThatLaunchesNothingKeepsTheStandingFocus(t *testing.T) {
	launched := liveSet(nil, []string{"shell-1"}, nil)
	tests := []struct {
		name string
		act  func(h *harness)
	}{
		{
			name: "a re-take of the same set",
			act:  func(h *harness) { h.r.OnLiveWorkChanged(testWS, launched) },
		},
		{
			name: "an item ending",
			act:  func(h *harness) { h.r.OnLiveWorkChanged(testWS, LiveWorkSet{}) },
		},
		{
			name: "an adopted shell",
			act: func(h *harness) {
				h.r.OnDetachedWork(testWS, nil, createdShell("shell-2", "make"))
				h.r.OnLiveWorkChanged(testWS, liveSet(nil, []string{"shell-1", "shell-2"}, nil))
			},
		},
		{
			name: "an adopted subagent listed by its created agent",
			act: func(h *harness) {
				h.r.OnDetachedWork(testWS, nil, detachedSubagentWork("work-2", "agent-2", "Explore"))
				h.r.OnLiveWorkChanged(testWS, liveSet([]string{"agent-2"}, []string{"shell-1"}, nil))
			},
		},
		{
			name: "a replayed run whose terminal already landed",
			act: func(h *harness) {
				h.r.OnDetachedWork(testWS, mainAgent, detachedSubagentWork("agent-2", "agent-2", "Explore"))
				h.r.OnSubagent(testWS, workID("agent-2"), subagentSettled(false))
				h.r.OnLiveWorkChanged(testWS, liveSet([]string{"agent-2"}, []string{"shell-1"}, nil))
			},
		},
		{
			name: "a scheduled job listing",
			act: func(h *harness) {
				h.r.OnActivity(testWS, mainAgent, cronListed(
					&conversationv1.AgentCronJob{JobId: "j1", Cron: "*/5 * * * *", HumanSchedule: "every 5 min"},
				))
			},
		},
		{
			name: "a task change",
			act: func(h *harness) {
				h.r.OnActivity(testWS, mainAgent, taskAct("task-1", pendingTask(stated("write the tests"))))
			},
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			connected(h)
			announceMain(h, launched)
			h.r.OnLiveWorkChanged(testWS, launched)

			// Act
			tc.act(h)

			// Assert
			if got, want := focusOfView(h.view(t)), (focusRead{"shells", 1}); got != want {
				t.Fatalf("focus = %+v, want %+v: nothing launched, so nothing may be minted", got, want)
			}
		})
	}
}

// TestAnAdoptedItemDoesNotMaskALaterLaunch holds the adoption mark to the one
// set change it exists for: once the set carries the adopted item, a real
// launch that follows mints as any launch does.
func TestAnAdoptedItemDoesNotMaskALaterLaunch(t *testing.T) {
	// Arrange: an adopted shell, taken up by the set.
	h := newHarness(t)
	connected(h)
	h.r.OnDetachedWork(testWS, nil, createdShell("shell-1", "make"))
	h.r.OnLiveWorkChanged(testWS, liveSet(nil, []string{"shell-1"}, nil))
	announceMain(h, liveSet(nil, nil, []string{"monitor-1"}))

	// Act: a monitor starts.
	h.r.OnLiveWorkChanged(testWS, liveSet(nil, []string{"shell-1"}, []string{"monitor-1"}))

	// Assert
	if got, want := focusOfView(h.view(t)), (focusRead{"shells", 1}); got != want {
		t.Fatalf("focus = %+v, want %+v", got, want)
	}
}

// TestAMintedFocusIsRecordedOnce — an invisible action is a logging defect.
// The record names the panel, the generation and the item that launched.
func TestAMintedFocusIsRecordedOnce(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)
	announceMain(h, liveSet(nil, []string{"shell-1"}, nil))

	// Act
	h.r.OnLiveWorkChanged(testWS, liveSet(nil, []string{"shell-1"}, nil))
	h.r.OnLiveWorkChanged(testWS, liveSet(nil, []string{"shell-1"}, nil))

	// Assert
	if got := countOf(h.log.Records(), "info", "daemon.footer.focus_minted"); got != 1 {
		t.Fatalf("focus_minted info records = %d, want exactly one for one launch", got)
	}
	rec := lastRecord(t, h, "daemon.footer.focus_minted")
	if rec.Context["panel"] != "shells" || rec.Context["generation"] != uint64(1) || rec.Context["work_id"] != "shell:shell-1" {
		t.Fatalf("record context = %+v, want panel shells, generation 1, work_id shell:shell-1", rec.Context)
	}
}

// testingFacts is a merge testing in round n with one suite waiting.
func testingFacts(n int) MergeFacts {
	return MergeFacts{State: "merging", Step: StepTesting, TestsRound: n, Tests: []*frontendv1.FooterMergeTestRow{mergeTestRow("daemon", waitingRow())}}
}

func TestAMergeThatBeginsTestingFocusesTheMergeTestsPanel(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)
	h.r.SetMerge(testWS, MergeFacts{State: "merging", Step: StepRebasing, Total: 1})

	// Act
	h.r.SetMerge(testWS, testingFacts(1))

	// Assert
	if got := focusOfView(h.view(t)); got != (focusRead{panel: "merge_tests", generation: 1}) {
		t.Fatalf("focus = %+v, want the merge tests panel at generation 1", got)
	}
}

func TestEachNewTestingRoundMintsANewGeneration(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)
	h.r.SetMerge(testWS, testingFacts(1))
	h.r.SetMerge(testWS, MergeFacts{State: "merging", Step: StepFixing, Attempt: 1, MaxAttempts: 3, TestsRound: 1})

	// Act
	h.r.SetMerge(testWS, testingFacts(2))

	// Assert
	if got := focusOfView(h.view(t)); got != (focusRead{panel: "merge_tests", generation: 2}) {
		t.Fatalf("focus = %+v, want the merge tests panel at generation 2", got)
	}
}

func TestARepublishOfTheSameTestingRoundMintsNothing(t *testing.T) {
	// Arrange: a suite finishing republishes the round's facts.
	h := newHarness(t)
	connected(h)
	h.r.SetMerge(testWS, testingFacts(1))

	// Act
	h.r.SetMerge(testWS, testingFacts(1))

	// Assert
	if got := focusOfView(h.view(t)); got.generation != 1 {
		t.Fatalf("focus = %+v, want generation 1 still: the round did not change", got)
	}
}

func TestTheMergeTestsFocusIsRecorded(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)

	// Act
	h.r.SetMerge(testWS, testingFacts(1))

	// Assert
	records := recordsOf(h.log.Records(), "daemon.footer.focus_minted")
	if len(records) != 1 || records[0].Context["panel"] != "merge_tests" {
		t.Fatalf("focus_minted records = %+v, want one naming the merge tests panel", records)
	}
}
