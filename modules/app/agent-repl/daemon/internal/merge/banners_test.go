package merge

import (
	"context"
	"errors"
	"reflect"
	"strings"
	"sync"
	"testing"

	"agent-repld/internal/desktopnotify"
	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/sourcescan"
	"agent-repld/internal/wsm"
)

// raisedBanner is one desktop banner a merge raised: "queued" with its
// label, or its resolution's end with its summary.
type raisedBanner struct {
	ws   ids.WorkspaceID
	kind string
	text string
}

// fakeBanners records every banner the merge queue raised, in order.
type fakeBanners struct {
	mu     sync.Mutex
	raised []raisedBanner
}

func (b *fakeBanners) Queued(ws ids.WorkspaceID, label string) {
	b.mu.Lock()
	defer b.mu.Unlock()
	b.raised = append(b.raised, raisedBanner{ws: ws, kind: "queued", text: label})
}

func (b *fakeBanners) Resolved(ws ids.WorkspaceID, end desktopnotify.MergeEnd, summary string) {
	b.mu.Lock()
	defer b.mu.Unlock()
	b.raised = append(b.raised, raisedBanner{ws: ws, kind: end.String(), text: summary})
}

// seen answers every banner raised so far.
func (b *fakeBanners) seen() []raisedBanner {
	b.mu.Lock()
	defer b.mu.Unlock()
	return append([]raisedBanner(nil), b.raised...)
}

// kinds answers each raised banner's workspace and kind, without its text.
func (b *fakeBanners) kinds() []raisedBanner {
	var out []raisedBanner
	for _, r := range b.seen() {
		out = append(out, raisedBanner{ws: r.ws, kind: r.kind})
	}
	return out
}

// TestEveryEndOfARunningMergeRaisesOnlyItsQueuedAndResolvedBanners pins that
// a merge raises exactly two banners, whichever way it ends: one as it is put
// in line, and one as it leaves the queue for good.
func TestEveryEndOfARunningMergeRaisesOnlyItsQueuedAndResolvedBanners(t *testing.T) {
	for _, tc := range releaseCases() {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange and Act.
			h := ended(t, tc.phase, tc.res)

			// Assert.
			end := "abandoned"
			if tc.phase == phaseLanded {
				end = "landed"
			}
			want := []raisedBanner{{ws: theWorkspace, kind: "queued"}, {ws: theWorkspace, kind: end}}
			if got := h.banners.kinds(); !reflect.DeepEqual(got, want) {
				t.Fatalf("banners = %+v, want %+v", got, want)
			}
		})
	}
}

func TestAQueuedMergesBannerCarriesWhatItLands(t *testing.T) {
	// Arrange
	h := newHarness(t)

	// Act
	enqueue(t, h)

	// Assert
	got := h.banners.seen()
	if len(got) != 1 || got[0].kind != "queued" || got[0].ws != theWorkspace || got[0].text == "" || got[0].text == string(theWorkspace) {
		t.Fatalf("banners = %+v, want one queued banner carrying the merge's label", got)
	}
}

func TestAFailedMergeRaisesTheFailedBannerWithItsSummary(t *testing.T) {
	// Arrange: a merge whose before-action fails.
	h := newHarness(t)
	h.emacsRepo()
	h.configureActions([]string{"before"}, nil)
	h.briefs["before"] = nil
	h.turnCloses = []wsm.TurnClose{wsm.CloseFailed}
	enqueue(t, h)

	// Act
	_ = h.admit(context.Background())

	// Assert
	got := h.banners.seen()
	if len(got) != 2 || got[1].kind != "failed" || got[1].ws != theWorkspace || got[1].text == "" {
		t.Fatalf("banners = %+v, want the queued banner then the failed one with its summary", got)
	}
}

func TestAnAgentsRequestRaisesNoBannerBeforeItsTurnEnds(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.inFlight = "turn-asking"
	if err := h.request(t, ownBranch, RequestedByAgent); err != nil {
		t.Fatalf("Enqueue: %v", err)
	}
	before := h.banners.seen()

	// Act
	h.runWait(t)

	// Assert
	if len(before) != 0 {
		t.Fatalf("banners before the requesting turn ended = %+v, want none", before)
	}
	if got := h.banners.kinds(); !reflect.DeepEqual(got, []raisedBanner{{ws: theWorkspace, kind: "queued"}}) {
		t.Fatalf("banners once the turn ended = %+v, want the one queued banner", got)
	}
}

func TestARequestDroppedBeforeItWasInLineRaisesNoBanner(t *testing.T) {
	// Arrange: the requesting turn is still running.
	h := newHarness(t)
	h.inFlight = "turn-asking"
	h.awaitGate = make(chan struct{})
	if err := h.request(t, ownBranch, RequestedByAgent); err != nil {
		t.Fatalf("Enqueue: %v", err)
	}

	// Act
	if err := h.o.Evict(context.Background(), theWorkspace); err != nil {
		t.Fatalf("Evict: %v", err)
	}

	// Assert
	if got := h.banners.seen(); len(got) != 0 {
		t.Fatalf("banners = %+v, want none for a merge never put in line", got)
	}
}

func TestADetachedMergeRaisesItsBannersOnTheMergeWorkspace(t *testing.T) {
	// Arrange: a detached request handed off and in line.
	h := newHarness(t)
	h.emacsRepo()
	h.landsCleanly("abc123def4567")
	h.git.changed = []string{"modules/app/agent-repl/daemon/x.go"}
	h.gatePasses("daemon")
	if err := requestDetached(t, h, RequestedByUser); err != nil {
		t.Fatalf("Enqueue: %v", err)
	}

	// Act
	if err := h.admit(context.Background()); err != nil {
		t.Fatalf("the merge failed: %v", err)
	}

	// Assert
	want := []raisedBanner{{ws: theMergeWorkspace, kind: "queued"}, {ws: theMergeWorkspace, kind: "landed"}}
	if got := h.banners.kinds(); !reflect.DeepEqual(got, want) {
		t.Fatalf("banners = %+v, want %+v", got, want)
	}
}

func TestAForkThatFailsRaisesTheRequestersAbandonedBanner(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.detached.forkErr = errors.New("the worktree could not be made")

	// Act
	if err := requestDetached(t, h, RequestedByUser); err != nil {
		t.Fatalf("Enqueue: %v", err)
	}

	// Assert
	got := h.banners.seen()
	if len(got) != 1 || got[0].ws != theWorkspace || got[0].kind != "abandoned" || !strings.Contains(got[0].text, "the worktree could not be made") {
		t.Fatalf("banners = %+v, want the requester's abandoned banner naming the fork's failure", got)
	}
}

func TestAMergeTheRestartCouldNotResumeRaisesTheFailedBanner(t *testing.T) {
	// Arrange: an interrupted merge whose geometry was forgotten.
	h := newHarness(t)
	enqueue(t, h)
	interruptMerge(t, h)
	h.db.mu.Lock()
	delete(h.db.jobs, theWorkspace)
	h.db.mu.Unlock()

	// Act
	if err := h.o.Recover(context.Background()); err != nil {
		t.Fatalf("Recover failed: %v", err)
	}

	// Assert
	want := []raisedBanner{{ws: theWorkspace, kind: "queued"}, {ws: theWorkspace, kind: "failed"}}
	if got := h.banners.kinds(); !reflect.DeepEqual(got, want) {
		t.Fatalf("banners = %+v, want %+v", got, want)
	}
}

func TestAWaitingMergeTheRestartCouldNotRequeueRaisesTheAbandonedBanner(t *testing.T) {
	// Arrange: a merge queued before the shutdown, whose geometry is gone.
	h := newHarness(t)
	enqueue(t, h)
	forgetJob(h, theWorkspace)
	fresh, err := newOrchestrator(h.deps())
	if err != nil {
		t.Fatalf("building the successor: %v", err)
	}

	// Act
	if err := fresh.Recover(context.Background()); err != nil {
		t.Fatalf("Recover failed: %v", err)
	}

	// Assert
	want := []raisedBanner{{ws: theWorkspace, kind: "queued"}, {ws: theWorkspace, kind: "abandoned"}}
	if got := h.banners.kinds(); !reflect.DeepEqual(got, want) {
		t.Fatalf("banners = %+v, want %+v", got, want)
	}
}

func TestQueueRequestWithNoRecordedRequestIsAnError(t *testing.T) {
	// Arrange: nothing was requested.
	h := newHarness(t)

	// Act
	err := h.o.queueRequest(context.Background(), theWorkspace, h.repoKey())

	// Assert
	if err == nil || !strings.Contains(err.Error(), "no recorded request") {
		t.Fatalf("queueRequest = %v, want the missing request named", err)
	}
	if _, ok := h.recordFor(dlog.LevelError, "daemon.merge.request"); !ok {
		t.Fatal("the missing request was not recorded at ERROR")
	}
	if got := h.banners.seen(); len(got) != 0 {
		t.Fatalf("banners = %+v, want none", got)
	}
}

func TestDetachedOutcomeKindMergeEnd(t *testing.T) {
	cases := []struct {
		kind DetachedOutcomeKind
		want desktopnotify.MergeEnd
	}{
		{kind: DetachedLanded, want: desktopnotify.MergeLanded},
		{kind: DetachedFailed, want: desktopnotify.MergeFailed},
		{kind: DetachedAbandoned, want: desktopnotify.MergeAbandoned},
	}
	for _, tc := range cases {
		t.Run(tc.kind.String(), func(t *testing.T) {
			// Act
			got := tc.kind.mergeEnd()

			// Assert
			if got != tc.want {
				t.Fatalf("mergeEnd() = %s, want %s", got, tc.want)
			}
		})
	}
}

func TestDetachedOutcomeKindMergeEndPanicsOnAnUnknownKind(t *testing.T) {
	// Assert
	defer func() {
		if recover() == nil {
			t.Fatal("an unknown outcome kind was given a banner end")
		}
	}()

	// Act
	DetachedOutcomeKind(9).mergeEnd()
}

// TestEveryResolutionIsAnnouncedThroughEnded pins the one funnel: a merge's
// resolved banner is raised only through raiseResolved, by ended (and by the
// fork that never made a merge workspace), and a handback skips ended only for
// a merge never put in line, so no terminal can hand back without announcing
// its end, and every end's banner is worded the same way.
func TestEveryResolutionIsAnnouncedThroughEnded(t *testing.T) {
	// Arrange / Act: the package's own production sources.
	resolved, raised, handBacks := 0, 0, 0
	for _, file := range sourcescan.Production(t) {
		resolved += strings.Count(string(file.Source), "Banners.Resolved(")
		raised += strings.Count(string(file.Source), "o.raiseResolved(")
		handBacks += strings.Count(string(file.Source), "o.handBack(ctx")
	}

	// Assert
	if resolved != 1 {
		t.Errorf("Banners.Resolved is called at %d sites, want 1: raiseResolved", resolved)
	}
	if raised != 2 {
		t.Errorf("raiseResolved is called at %d sites, want 2: ended and the failed fork", raised)
	}
	if handBacks != 2 {
		t.Errorf("handBack is called at %d sites, want 2: ended and a merge never put in line", handBacks)
	}
}
