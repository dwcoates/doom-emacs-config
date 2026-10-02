package feed

import (
	"fmt"
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"claude-repld/internal/contextcut"
	"claude-repld/internal/ids"
)

// THE DRAWN DIVIDER BOUNDS EXACTLY WHEN ITS CUT DOES: the feed reads the bound
// off drawn rows (boundsDelivery) and the session watcher off the page's cuts
// (contextcut.Bounds), and the two must be one rule. Each case is one arm.
func TestADividerBoundsDeliveryExactlyWhenItsCutBounds(t *testing.T) {
	cases := []struct {
		name string
		cut  *conversationv1.ContextCut
	}{
		{name: "cleared", cut: clearedCut()},
		{name: "compacted", cut: compactedCut("what survived")},
		{name: "compaction failed", cut: &conversationv1.ContextCut{Cut: &conversationv1.ContextCut_CompactionFailed{
			CompactionFailed: &conversationv1.ContextCompactionFailed{Error: "refused"},
		}}},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			h := newHarness(t)

			// Act.
			h.cutAt("entry-cut", tc.cut)

			// Assert.
			row := h.only(rootFeed())
			if got, want := boundsDelivery(row), contextcut.Bounds(tc.cut); got != want {
				t.Fatalf("boundsDelivery = %v, contextcut.Bounds = %v; the two rules disagree", got, want)
			}
		})
	}
}

// THE /clear TWO-STAGE DIVIDER. A /clear is reflected the instant the daemon
// accepts it — the red bar and the cleared feed appear before the shim is asked
// — and the shim's later ContextCut confirms that SAME row with its subtext. A
// terminal below the bar (the "response cut short" bubble) is never drawn, and a
// clear that fails recovers the feed rather than leaving a phantom bar.

// clearedContextCut is the cut a /clear produces on the wire.
func clearedContextCut() *conversationv1.ContextCut {
	return &conversationv1.ContextCut{
		Cut: &conversationv1.ContextCut_Cleared{Cleared: &conversationv1.ContextCleared{}},
	}
}

func TestAClearReceivedDrawsItsDividerBeforeAnyShimRoundTrip(t *testing.T) {
	// Arrange: a conversation stands.
	h := newHarness(t)
	h.deliverPrompt("turn-1", "hello")

	// Act: the daemon accepts a /clear — no cut has come back from the shim.
	h.resolver.OnClearReceived(testWorkspace, ids.TurnID("turn-2"))

	// Assert: the red bar is already drawn, with no subtext yet, and it bounds
	// delivery so the feed is cleared.
	sep := h.separationRow().GetSeparation()
	if sep.GetCleared() == nil {
		t.Fatalf("kind = %T, want cleared", sep.GetKind())
	}
	if sep.GetLabel().GetText() != "" {
		t.Fatalf("label = %q, want empty until the shim confirms", sep.GetLabel().GetText())
	}
}

func TestAClearsSubtextArrivesOnlyWhenTheShimConfirms(t *testing.T) {
	// Arrange: the optimistic bar is up and the clear turn is running.
	h := newHarness(t)
	h.resolver.OnClearReceived(testWorkspace, ids.TurnID("turn-2"))
	h.resolver.OnTurnOpened(testWorkspace, ids.TurnID("turn-2"))

	// Act: the shim's ContextCut confirms the clear.
	h.cutAt("entry-clear", clearedContextCut())

	// Assert: the same bar now carries the "context cleared" subtext.
	sep := h.separationRow().GetSeparation()
	if sep.GetLabel().GetText() != "context cleared" {
		t.Fatalf("label = %q, want the confirmed subtext", sep.GetLabel().GetText())
	}
}

func TestTheOptimisticDividerAndTheConfirmedCutAreOneRow(t *testing.T) {
	// Arrange: the optimistic bar is up and the clear turn is running.
	h := newHarness(t)
	h.resolver.OnClearReceived(testWorkspace, ids.TurnID("turn-2"))
	before := h.separationRow().GetId().GetValue()
	h.resolver.OnTurnOpened(testWorkspace, ids.TurnID("turn-2"))

	// Act: the shim confirms the clear.
	h.cutAt("entry-clear", clearedContextCut())

	// Assert: exactly one divider, and its id is the one the receipt drew.
	if got := len(h.separationRows()); got != 1 {
		t.Fatalf("separation rows = %d, want the optimistic and confirmed to be one row", got)
	}
	if after := h.separationRow().GetId().GetValue(); after != before {
		t.Fatalf("confirmed row id = %q, want the optimistic row's %q", after, before)
	}
}

func TestTheSecondPlaneDeliveryOfAClearStaysOneRow(t *testing.T) {
	// Arrange: a confirmed clear.
	h := newHarness(t)
	h.resolver.OnClearReceived(testWorkspace, ids.TurnID("turn-2"))
	h.resolver.OnTurnOpened(testWorkspace, ids.TurnID("turn-2"))
	h.cutAt("entry-clear", clearedContextCut())
	// The turn ends, so no turn is in flight when the other plane re-delivers.
	h.terminal("turn-2", interruptedByUser(), nil)

	// Act: the file plane delivers the SAME cut at the SAME store pointer.
	h.cutAt("entry-clear", clearedContextCut())

	// Assert: still one divider — the pointer maps back to the one turn-keyed row.
	if got := len(h.separationRows()); got != 1 {
		t.Fatalf("separation rows = %d, want one however many planes deliver the clear", got)
	}
}

func TestAClearAbortedBeforeTheShimRetiresItsDivider(t *testing.T) {
	// Arrange: the optimistic bar is up.
	h := newHarness(t)
	h.deliverPrompt("turn-1", "hello")
	h.resolver.OnClearReceived(testWorkspace, ids.TurnID("turn-2"))

	// Act: the shim refused the turn before it ever ran.
	h.resolver.OnContextCutAborted(testWorkspace, ids.TurnID("turn-2"))

	// Assert: the phantom bar is gone and the feed recovers.
	if got := len(h.separationRows()); got != 0 {
		t.Fatalf("separation rows = %d, want the phantom bar retired", got)
	}
}

// ONE ROW KIND FOR EVERY SEPARATION — context cuts and worktree moves alike —
// because one renderer subroutine draws them all and an arm selects only its
// accent and its label. A separation belongs to NO TURN.

// separationRow finds the divider on the root feed.
func (h *harness) separationRow() *frontendv1.FeedRow {
	h.t.Helper()
	for _, row := range h.rows(rootFeed()) {
		if row.GetSeparation() != nil {
			return row
		}
	}
	h.t.Fatal("no separation divider on the root feed")
	return nil
}

// cut sends one context cut, at the store position AT.
//
// EVERY CUT NEEDS A POSITION because the position is the cut's identity: the
// two producing planes deliver the same entry, and the divider keys on the
// entry rather than on a count of arrivals. A caller that wants to model the
// SECOND delivery of the same cut passes the same AT again.
func (h *harness) cutAt(at string, cut *conversationv1.ContextCut) {
	h.t.Helper()
	h.resolver.OnContextCut(testWorkspace, mainAgent(), cut,
		&conversationv1.HistoryPointer{Value: at}, nil, nil)
}

// cut sends one context cut at a position of its own.
func (h *harness) cut(cut *conversationv1.ContextCut) {
	h.t.Helper()
	h.cutSeq++
	h.cutAt(fmt.Sprintf("entry-%d", h.cutSeq), cut)
}

func TestAClearedContextDrawsItsDividerWithNoTokenFigure(t *testing.T) {
	// Arrange, Act.
	h := newHarness(t)
	h.cut(&conversationv1.ContextCut{
		Cut: &conversationv1.ContextCut_Cleared{Cleared: &conversationv1.ContextCleared{}},
	})

	// Assert: the vendor's reset record carries no delta, so none is invented.
	separation := h.separationRow().GetSeparation()
	if separation.GetCleared() == nil {
		t.Fatalf("kind = %T, want cleared", separation.GetKind())
	}
	if separation.GetLabel().GetText() != "context cleared" {
		t.Fatalf("label = %q", separation.GetLabel().GetText())
	}
	if separation.GetTokens() != nil {
		t.Fatalf("tokens = %+v, want unset for a clear", separation.GetTokens())
	}
}

func TestACompactionDrawsItsSummaryFoldedWithBothFormattedSides(t *testing.T) {
	// Arrange, Act.
	h := newHarness(t)
	h.cut(&conversationv1.ContextCut{
		Cut: &conversationv1.ContextCut_Compacted{Compacted: &conversationv1.ContextCompacted{
			Summary: &conversationv1.AgentResponseProse{Markdown: "we were fixing the flaky test"},
			Tokens:  &conversationv1.ContextTokenDelta{TokensBefore: 180_000, TokensAfter: 12_000},
			Trigger: &conversationv1.ContextCompacted_Requested{
				Requested: &conversationv1.ContextCompactionRequested{},
			},
			DurationMs: 42_000,
		}},
	})

	// Assert: the client renders both sides verbatim and does no arithmetic.
	separation := h.separationRow().GetSeparation()
	compacted := separation.GetCompacted()
	if compacted.GetSummary().GetMarkdown() != "we were fixing the flaky test" {
		t.Fatalf("summary = %q", compacted.GetSummary().GetMarkdown())
	}
	if separation.GetTokens().GetBeforeText() != "180k" || separation.GetTokens().GetAfterText() != "12k" {
		t.Fatalf("tokens = %+v, want the formatted 180k → 12k", separation.GetTokens())
	}
}

func TestAnAutomaticCompactionIsNotDrawnLikeARequestedOne(t *testing.T) {
	tests := []struct {
		name    string
		trigger any
		want    string
	}{
		{
			name:    "the user or daemon asked for it",
			trigger: &conversationv1.ContextCompactionRequested{},
			want:    "context compacted on request · took 42 s",
		},
		{
			name:    "it happened to them while they watched",
			trigger: &conversationv1.ContextCompactionAutomatic{},
			want:    "context compacted automatically · took 42 s",
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			h := newHarness(t)
			compacted := &conversationv1.ContextCompacted{
				Summary:    &conversationv1.AgentResponseProse{Markdown: "s"},
				Tokens:     &conversationv1.ContextTokenDelta{TokensBefore: 100, TokensAfter: 10},
				DurationMs: 42_000,
			}
			switch tr := tc.trigger.(type) {
			case *conversationv1.ContextCompactionRequested:
				compacted.Trigger = &conversationv1.ContextCompacted_Requested{Requested: tr}
			case *conversationv1.ContextCompactionAutomatic:
				compacted.Trigger = &conversationv1.ContextCompacted_Automatic{Automatic: tr}
			}

			// Act.
			h.cut(&conversationv1.ContextCut{
				Cut: &conversationv1.ContextCut_Compacted{Compacted: compacted},
			})

			// Assert.
			got := h.separationRow().GetSeparation().GetLabel().GetText()
			if got != tc.want {
				t.Fatalf("label = %q, want %q", got, tc.want)
			}
		})
	}
}

// failedCompaction drives one failed compaction through the resolver.
func failedCompaction(h *harness, reason string) {
	h.cut(&conversationv1.ContextCut{
		Cut: &conversationv1.ContextCut_CompactionFailed{
			CompactionFailed: &conversationv1.ContextCompactionFailed{Error: reason},
		},
	})
}

func TestAFailedCompactionDrawsTheCompactionFailedDivider(t *testing.T) {
	// Arrange, Act: NOTHING WAS CUT (landing 8: the divider says so).
	h := newHarness(t)
	h.deliverPrompt("turn-1", "compact please")
	failedCompaction(h, "the summarizer refused")

	// Assert.
	got := h.separationRow().GetSeparation().GetCompactionFailed()
	if got.GetError() != "the summarizer refused" {
		t.Fatalf("error = %q, want the producer's account verbatim", got.GetError())
	}
}

func TestAFailedCompactionsDividerIsLabelled(t *testing.T) {
	// Arrange, Act.
	h := newHarness(t)
	h.deliverPrompt("turn-1", "compact please")
	failedCompaction(h, "the summarizer refused")

	// Assert.
	if got := h.separationRow().GetSeparation().GetLabel().GetText(); got != "compaction failed" {
		t.Fatalf("label = %q, want the composed label", got)
	}
}

func TestAFailedCompactionsDividerCarriesNoTokens(t *testing.T) {
	// Arrange, Act: no size changed, so no figure is drawn.
	h := newHarness(t)
	h.deliverPrompt("turn-1", "compact please")
	failedCompaction(h, "the summarizer refused")

	// Assert.
	if got := h.separationRow().GetSeparation().Tokens; got != nil {
		t.Fatalf("tokens = %+v, want UNSET", got)
	}
}

func TestAFailedCompactionWarns(t *testing.T) {
	// Arrange, Act.
	h := newHarness(t)
	h.deliverPrompt("turn-1", "compact please")
	failedCompaction(h, "the summarizer refused")

	// Assert.
	if !h.hasRecord("warn", "daemon.feed.compaction_failed") {
		t.Fatalf("records = %+v, want a WARN daemon.feed.compaction_failed", h.records())
	}
}

func TestAFailedCompactionAlsoRidesTheTurnsEvidence(t *testing.T) {
	// Arrange: a turn in flight that then fails of something else.
	h := newHarness(t)
	h.deliverPrompt("turn-1", "compact please")
	failedCompaction(h, "the summarizer refused")

	// Act.
	h.terminal("turn-1", nil, &conversationv1.AgentFailure{
		Failure: &conversationv1.AgentFailure_PromptTooLong{PromptTooLong: &conversationv1.AgentPromptTooLong{}},
	})

	// Assert.
	headline := h.terminalRow("turn-1").GetErrored().GetHeadline().GetText()
	if !contains(headline, "the summarizer refused") {
		t.Fatalf("headline = %q, want the compaction failure as evidence", headline)
	}
}

func TestAContextCutWithNoArmDrawsNothingAndWarns(t *testing.T) {
	// Arrange, Act.
	h := newHarness(t)
	h.cut(&conversationv1.ContextCut{})

	// Assert.
	if rows := h.rows(rootFeed()); len(rows) != 0 {
		t.Fatalf("rows = %d, want none", len(rows))
	}
	if !h.hasRecord("warn", "daemon.feed.context_cut_unset") {
		t.Fatalf("records = %+v, want a WARN daemon.feed.context_cut_unset", h.records())
	}
}

func TestASeparationBelongsToNoTurn(t *testing.T) {
	// Arrange: a turn is in flight, so a row that CAN be stamped would be.
	h := newHarness(t)
	h.deliverPrompt("turn-1", "hello")

	// Act.
	h.cut(&conversationv1.ContextCut{
		Cut: &conversationv1.ContextCut_Cleared{Cleared: &conversationv1.ContextCleared{}},
	})

	// Assert.
	if got := h.separationRow().GetTurn(); got != nil {
		t.Fatalf("turn = %+v, want unset on a separation", got)
	}
}

func TestEnteringAWorktreeDrawsADividerAndNotAToolCard(t *testing.T) {
	// Arrange, Act.
	h := newHarness(t)
	branch := "DWC/fix-flaky"
	h.send(&conversationv1.AgentActivity{
		ActivityId: &conversationv1.AgentActivityId{Value: "unit-1"},
		Item: &conversationv1.AgentActivity_Worktree{Worktree: &conversationv1.AgentWorktree{
			State: &conversationv1.AgentWorktree_Success{Success: &conversationv1.AgentWorktreeSuccess{
				Act: &conversationv1.AgentWorktreeSuccess_Entered{Entered: &conversationv1.AgentWorktreeEntered{
					Path:   "/tmp/wt",
					Branch: &branch,
				}},
			}},
		}},
	})

	// Assert: the path is drawn AND a jump target; no context changed, so no
	// token figure rides it.
	separation := h.separationRow().GetSeparation()
	entered := separation.GetWorktreeEntered()
	if entered.GetPath().GetText() != "/tmp/wt" {
		t.Fatalf("path = %q", entered.GetPath().GetText())
	}
	if entered.GetBranch().GetText() != branch {
		t.Fatalf("branch = %q", entered.GetBranch().GetText())
	}
	if separation.GetTokens() != nil {
		t.Fatalf("tokens = %+v, want unset on a worktree arm", separation.GetTokens())
	}
}

func TestAWorktreeEnteredWithNoBranchNamedCarriesNone(t *testing.T) {
	// Arrange, Act.
	h := newHarness(t)
	h.send(&conversationv1.AgentActivity{
		ActivityId: &conversationv1.AgentActivityId{Value: "unit-1"},
		Item: &conversationv1.AgentActivity_Worktree{Worktree: &conversationv1.AgentWorktree{
			State: &conversationv1.AgentWorktree_Success{Success: &conversationv1.AgentWorktreeSuccess{
				Act: &conversationv1.AgentWorktreeSuccess_Entered{
					Entered: &conversationv1.AgentWorktreeEntered{Path: "/tmp/wt"},
				},
			}},
		}},
	})

	// Assert.
	if h.separationRow().GetSeparation().GetWorktreeEntered().GetBranch() != nil {
		t.Fatal("a branch the vendor did not name was drawn")
	}
}

func TestAKeptTreeIsSomewhereToGo(t *testing.T) {
	// Arrange, Act.
	h := newHarness(t)
	h.send(&conversationv1.AgentActivity{
		ActivityId: &conversationv1.AgentActivityId{Value: "unit-1"},
		Item: &conversationv1.AgentActivity_Worktree{Worktree: &conversationv1.AgentWorktree{
			State: &conversationv1.AgentWorktree_Success{Success: &conversationv1.AgentWorktreeSuccess{
				Act: &conversationv1.AgentWorktreeSuccess_Exited{Exited: &conversationv1.AgentWorktreeExited{
					Outcome: &conversationv1.AgentWorktreeExited_Kept{Kept: &conversationv1.AgentWorktreeKept{}},
					Path:    "/tmp/wt",
				}},
			}},
		}},
	})

	// Assert.
	left := h.separationRow().GetSeparation().GetWorktreeLeft()
	if left.GetKept().GetPath().GetText() != "/tmp/wt" {
		t.Fatalf("kept path = %q", left.GetKept().GetPath().GetText())
	}
}

func TestARemovedTreeDrawsTheDiscardLineLoudWhenAnythingWasDiscarded(t *testing.T) {
	// Arrange, Act.
	h := newHarness(t)
	files, commits := uint32(3), uint32(2)
	h.send(&conversationv1.AgentActivity{
		ActivityId: &conversationv1.AgentActivityId{Value: "unit-1"},
		Item: &conversationv1.AgentActivity_Worktree{Worktree: &conversationv1.AgentWorktree{
			State: &conversationv1.AgentWorktree_Success{Success: &conversationv1.AgentWorktreeSuccess{
				Act: &conversationv1.AgentWorktreeSuccess_Exited{Exited: &conversationv1.AgentWorktreeExited{
					Outcome: &conversationv1.AgentWorktreeExited_Removed{
						Removed: &conversationv1.AgentWorktreeRemoved{
							DiscardedFiles: &files, DiscardedCommits: &commits,
						},
					},
				}},
			}},
		}},
	})

	// Assert.
	got := h.separationRow().GetSeparation().GetWorktreeLeft().GetRemoved().GetDiscarded().GetText()
	if got != "3 files, 2 commits discarded" {
		t.Fatalf("discard line = %q", got)
	}
}

func TestASingleDiscardedFileAndCommitAreNamedInTheSingular(t *testing.T) {
	// Arrange, Act: the fake's `!worktree-remove` discards exactly one commit,
	// and a headless sandbox run photographed the line reading "1 commits
	// discarded".
	h := newHarness(t)
	files, commits := uint32(1), uint32(1)
	h.send(&conversationv1.AgentActivity{
		ActivityId: &conversationv1.AgentActivityId{Value: "unit-1"},
		Item: &conversationv1.AgentActivity_Worktree{Worktree: &conversationv1.AgentWorktree{
			State: &conversationv1.AgentWorktree_Success{Success: &conversationv1.AgentWorktreeSuccess{
				Act: &conversationv1.AgentWorktreeSuccess_Exited{Exited: &conversationv1.AgentWorktreeExited{
					Outcome: &conversationv1.AgentWorktreeExited_Removed{
						Removed: &conversationv1.AgentWorktreeRemoved{
							DiscardedFiles: &files, DiscardedCommits: &commits,
						},
					},
				}},
			}},
		}},
	})

	// Assert.
	got := h.separationRow().GetSeparation().GetWorktreeLeft().GetRemoved().GetDiscarded().GetText()
	if got != "1 file, 1 commit discarded" {
		t.Fatalf("discard line = %q", got)
	}
}

func TestARemovedTreeWithNoStatedDiscardDrawsTheLabelAlone(t *testing.T) {
	// Arrange, Act: an UNSET figure is not zero.
	h := newHarness(t)
	h.send(&conversationv1.AgentActivity{
		ActivityId: &conversationv1.AgentActivityId{Value: "unit-1"},
		Item: &conversationv1.AgentActivity_Worktree{Worktree: &conversationv1.AgentWorktree{
			State: &conversationv1.AgentWorktree_Success{Success: &conversationv1.AgentWorktreeSuccess{
				Act: &conversationv1.AgentWorktreeSuccess_Exited{Exited: &conversationv1.AgentWorktreeExited{
					Outcome: &conversationv1.AgentWorktreeExited_Removed{
						Removed: &conversationv1.AgentWorktreeRemoved{},
					},
				}},
			}},
		}},
	})

	// Assert.
	if got := h.separationRow().GetSeparation().GetWorktreeLeft().GetRemoved().GetDiscarded(); got != nil {
		t.Fatalf("discard line = %+v, want unset", got)
	}
}

func TestAFailedWorktreeCallIsAToolFailureAndNotADivider(t *testing.T) {
	// Arrange, Act.
	h := newHarness(t)
	h.send(&conversationv1.AgentActivity{
		ActivityId: &conversationv1.AgentActivityId{Value: "unit-1"},
		Item: &conversationv1.AgentActivity_Worktree{Worktree: &conversationv1.AgentWorktree{
			State: &conversationv1.AgentWorktree_Failure{
				Failure: &conversationv1.AgentWorktreeFailure{},
			},
		}},
	})

	// Assert: no divider is drawn for a move that did not happen.
	for _, row := range h.rows(rootFeed()) {
		if row.GetSeparation() != nil {
			t.Fatal("a failed worktree call drew a divider")
		}
	}
}

func TestEachWorktreeActIsItsOwnDividerAndNeverCoalesced(t *testing.T) {
	// Arrange: an enter and an exit, far apart, as two units.
	h := newHarness(t)
	for _, act := range []struct {
		unit string
		item *conversationv1.AgentWorktreeSuccess
	}{
		{unit: "unit-1", item: &conversationv1.AgentWorktreeSuccess{
			Act: &conversationv1.AgentWorktreeSuccess_Entered{
				Entered: &conversationv1.AgentWorktreeEntered{Path: "/tmp/wt"},
			},
		}},
		{unit: "unit-2", item: &conversationv1.AgentWorktreeSuccess{
			Act: &conversationv1.AgentWorktreeSuccess_Exited{Exited: &conversationv1.AgentWorktreeExited{
				Outcome: &conversationv1.AgentWorktreeExited_Kept{Kept: &conversationv1.AgentWorktreeKept{}},
				Path:    "/tmp/wt",
			}},
		}},
	} {
		// Act.
		h.send(&conversationv1.AgentActivity{
			ActivityId: &conversationv1.AgentActivityId{Value: act.unit},
			Item: &conversationv1.AgentActivity_Worktree{Worktree: &conversationv1.AgentWorktree{
				State: &conversationv1.AgentWorktree_Success{Success: act.item},
			}},
		})
	}

	// Assert: two dividers — everything between them happened inside the tree.
	dividers := 0
	for _, row := range h.rows(rootFeed()) {
		if row.GetSeparation() != nil {
			dividers++
		}
	}
	if dividers != 2 {
		t.Fatalf("dividers = %d, want 2 (unlike plan mode, worktree acts never coalesce)", dividers)
	}
}

// separationRows answers every divider on the root feed, for the tests whose
// subject is HOW MANY there are.
func (h *harness) separationRows() []*frontendv1.FeedRow {
	h.t.Helper()
	var out []*frontendv1.FeedRow
	for _, row := range h.rows(rootFeed()) {
		if row.GetSeparation() != nil {
			out = append(out, row)
		}
	}
	return out
}

// compactedCut is the cut a `!compact <summary>` scenario produces.
func compactedCut(summary string) *conversationv1.ContextCut {
	return &conversationv1.ContextCut{
		Cut: &conversationv1.ContextCut_Compacted{Compacted: &conversationv1.ContextCompacted{
			Summary: &conversationv1.AgentResponseProse{Markdown: summary},
			Tokens:  &conversationv1.ContextTokenDelta{TokensBefore: 180000, TokensAfter: 12000},
		}},
	}
}

// ONE CUT IS ONE DIVIDER, however many planes deliver it. The shim's stream
// plane and the sidecar's file plane write the SAME store entry, and every
// write of an entry is delivered on the agent's tail — so the daemon sees one
// compaction twice, and a per-arrival counter drew it twice.
func TestOneCutDeliveredTwiceDrawsOneDivider(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.cutAt("entry-7", compactedCut("the summary"))

	// Act: the second plane's delivery of the SAME store entry.
	h.cutAt("entry-7", compactedCut("the summary"))

	// Assert
	if got := len(h.separationRows()); got != 1 {
		t.Fatalf("separation rows = %d, want 1: one cut is one divider however many planes deliver it", got)
	}
}

// The second delivery UPSERTS the first's row, so a plane that carries less
// than the other cannot leave the reader with a divider that says nothing.
func TestASecondDeliveryOfACutRedrawsTheSameRow(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.cutAt("entry-7", compactedCut("the summary"))
	first := h.separationRow().GetId().GetValue()

	// Act
	h.cutAt("entry-7", compactedCut("the summary"))

	// Assert
	if got := h.separationRow().GetId().GetValue(); got != first {
		t.Fatalf("the second delivery's row id = %q, want the first's %q", got, first)
	}
}

// TWO DISTINCT CUTS ARE TWO DIVIDERS. The identity is the entry's position, so
// cuts that are identical in content but at different positions stay apart —
// which is what a `/clear` after a `/clear` looks like.
func TestTwoCutsAtDifferentPositionsDrawTwoDividers(t *testing.T) {
	// Arrange
	h := newHarness(t)
	cleared := &conversationv1.ContextCut{
		Cut: &conversationv1.ContextCut_Cleared{Cleared: &conversationv1.ContextCleared{}},
	}

	// Act
	h.cutAt("entry-7", cleared)
	h.cutAt("entry-9", cleared)

	// Assert
	if got := len(h.separationRows()); got != 2 {
		t.Fatalf("separation rows = %d, want 2: two cuts at two positions are two dividers", got)
	}
}

// A CUT WITH NO POSITION STILL DRAWS, and says so. A producer that states no
// position is a fault to see rather than a divider to drop, and the duplicate
// it may leave is strictly better than a missing one.
func TestACutWithNoPositionStillDrawsAndIsReported(t *testing.T) {
	// Arrange
	h := newHarness(t)

	// Act
	h.resolver.OnContextCut(testWorkspace, mainAgent(), compactedCut("the summary"), nil, nil, nil)

	// Assert
	if got := len(h.separationRows()); got != 1 {
		t.Fatalf("separation rows = %d, want the divider drawn anyway", got)
	}
	reported := false
	for _, record := range h.records() {
		if record.Operation == "daemon.feed.context_cut_unpositioned" {
			reported = true
		}
	}
	if !reported {
		t.Fatal("an unpositioned cut drew its divider silently; the fault must be recorded")
	}
}

// A LATE FILE-PLANE /clear CUT DOES NOT RE-KEY ONTO A LATER PROMPT. The file
// plane forwards the /clear envelope late — after the user has sent the next
// prompt — and it is delivered on the SAME store pointer as the stream plane's
// cut. It must upsert the ONE divider keyed on the clear turn, never draw a
// second bar below the new prompt nor suppress that prompt's own turn.
func TestALateFilePlaneClearDoesNotRekeyOntoALaterPrompt(t *testing.T) {
	// Arrange: a /clear turn ran to completion; the stream cut landed at a
	// pointer; then a normal prompt was delivered and is now the turn in flight.
	h := newHarness(t)
	h.deliverPrompt("turn-clear", "/clear")
	h.cutAt("entry-clear", clearedContextCut())
	h.terminal("turn-clear", interruptedByUser(), nil)
	h.deliverPrompt("turn-hello", "hello")

	// Act: the file plane's late copy of the SAME cut arrives while turn-hello
	// is in flight.
	h.cutAt("entry-clear", clearedContextCut())

	// Assert: still one divider, keyed on the clear turn — no second bar below
	// the new prompt.
	rows := h.separationRows()
	if len(rows) != 1 {
		t.Fatalf("separation rows = %d, want one — the late file-plane cut upserts the clear turn's divider", len(rows))
	}
	if got := rows[0].GetId().GetValue(); got != h.clearDividerRowID("turn-clear") {
		t.Fatalf("divider row = %q, want it keyed on the clear turn, not the later prompt", got)
	}
	// The later prompt's turn is untouched: it was not marked cleared, so its own
	// terminal will still draw.
	if h.resolver.state(testWorkspace).clearConfirmed[ids.TurnID("turn-hello")] {
		t.Fatal("the late /clear cut marked the later prompt's turn cleared; its terminal would be wrongly suppressed")
	}
	if got := len(h.userPromptRows()); got != 1 {
		t.Fatalf("user-prompt rows = %d, want only the normal 'hello' (the /clear draws none)", got)
	}
}
