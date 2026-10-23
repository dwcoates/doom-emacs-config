package desktopnotify

import (
	"errors"
	"testing"

	"agent-repld/internal/ids"
)

// postSettled posts one banner for ws while Emacs is unfocused and waits for
// its program to return.
func postSettled(f notifierFixture, ws ids.WorkspaceID) {
	f.notifier.Post(ws, "turn_ended", titledBy)
	f.notifier.wg.Wait()
}

// heldFixture is a notifier whose banners are handed to the platform (macOS),
// with Emacs's stream attached unfocused so banners post.
func heldFixture(t *testing.T) notifierFixture {
	t.Helper()
	f := newNotifierFixture(t, named)
	f.backend.outcome = Handed
	f.focus.Attach(false)
	return f
}

// claimTarget is the target a claim of ws answers.
func claimTarget(ws ids.WorkspaceID) Target {
	return Target{ID: ws, Dir: named.workspace.Dir}
}

// claimRecorded asserts the claim's one INFO record and its context.
func claimRecorded(t *testing.T, f notifierFixture, held int, want Claim) {
	t.Helper()
	r, ok := hasRecord(f.log, "info", "a focus gain claimed the newest held banner")
	if !ok || r.Context["held"] != held || r.Context["claimed"] != want.Held || r.Context["workspace"] != string(want.Target.ID) {
		t.Fatalf("record = %+v (found=%v), want held=%d and the claim's outcome recorded", r, ok, held)
	}
}

func TestClaimAnswersTheNewestHeldBanner(t *testing.T) {
	cases := []struct {
		name  string
		posts []ids.WorkspaceID
		held  int
		want  Claim
	}{
		{name: "the newest of two banners", posts: []ids.WorkspaceID{"ws1", "ws2"}, held: 2,
			want: Claim{Held: true, Target: claimTarget("ws2")}},
		{name: "a workspace's later banner replaces its earlier one", posts: []ids.WorkspaceID{"ws1", "ws2", "ws1"}, held: 2,
			want: Claim{Held: true, Target: claimTarget("ws1")}},
		{name: "none when nothing is held", posts: nil, held: 0, want: Claim{}},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			f := heldFixture(t)
			for _, ws := range tc.posts {
				postSettled(f, ws)
			}

			// Act
			got := f.notifier.Claim()

			// Assert
			if got != tc.want {
				t.Fatalf("Claim = %+v, want %+v", got, tc.want)
			}
			claimRecorded(t, f, tc.held, tc.want)
		})
	}
}

func TestClaimLetsGoOfEveryBannerHeld(t *testing.T) {
	// Arrange
	f := heldFixture(t)
	postSettled(f, "ws1")
	postSettled(f, "ws2")
	f.notifier.Claim()

	// Act
	got := f.notifier.Claim()

	// Assert
	if got.Held {
		t.Fatalf("second Claim = %+v, want none: the first consumed every banner", got)
	}
}

func TestClaimRunsNoProgram(t *testing.T) {
	// Arrange
	f := heldFixture(t)
	postSettled(f, "ws1")
	posted := len(f.backend.banners())

	// Act
	f.notifier.Claim()

	// Assert: the claim is memory alone; the backend ran only the post.
	if got := len(f.backend.banners()); got != posted {
		t.Fatalf("the backend ran %d programs after the claim, want none", got-posted)
	}
}

func TestClaimAnswersAReadBackBannerStillShowing(t *testing.T) {
	// Arrange
	f := newNotifierFixture(t, named)
	f.focus.Attach(false)
	f.backend.outcome = Unclicked
	f.backend.block = make(chan struct{})
	f.backend.madeSignal = make(chan struct{})
	f.notifier.Post("ws1", "turn_ended", titledBy)
	<-f.backend.madeSignal

	// Act
	got := f.notifier.Claim()

	// Assert
	close(f.backend.block)
	f.notifier.wg.Wait()
	if got != (Claim{Held: true, Target: claimTarget("ws1")}) {
		t.Fatalf("Claim = %+v, want the showing banner's ws1", got)
	}
}

func TestADismissedReadBackBannerIsNoLongerHeld(t *testing.T) {
	// Arrange
	f := newNotifierFixture(t, named)
	f.focus.Attach(false)
	f.backend.outcome = Unclicked
	postSettled(f, "ws1")

	// Act
	got := f.notifier.Claim()

	// Assert
	if got.Held {
		t.Fatalf("Claim = %+v, want none: the banner left the desktop", got)
	}
}

func TestAHandedBannerStaysHeldAfterItsProgramExits(t *testing.T) {
	// Arrange
	f := heldFixture(t)

	// Act
	postSettled(f, "ws1")

	// Assert
	if got := f.notifier.Claim(); got != (Claim{Held: true, Target: claimTarget("ws1")}) {
		t.Fatalf("Claim = %+v, want ws1: the platform holds a handed banner", got)
	}
	r, ok := hasRecord(f.log, "debug", "settled the held banner")
	if !ok || r.Context["handed"] != true || r.Context["still_held"] != true {
		t.Fatalf("record = %+v (found=%v), want the handed banner recorded still held", r, ok)
	}
}

func TestAFailedBannerIsNeverHeld(t *testing.T) {
	// Arrange
	f := heldFixture(t)
	f.backend.err = errors.New("exit status 3: notifications are not authorized")
	postSettled(f, "ws1")

	// Act
	got := f.notifier.Claim()

	// Assert
	if got.Held {
		t.Fatalf("Claim = %+v, want none: no banner was made", got)
	}
}

func TestForgetLetsGoOfEveryHeldBanner(t *testing.T) {
	// Arrange
	f := heldFixture(t)
	postSettled(f, "ws1")
	postSettled(f, "ws2")

	// Act
	f.notifier.Forget("the user selected a workspace")

	// Assert
	if got := f.notifier.Claim(); got.Held {
		t.Fatalf("Claim = %+v, want none after Forget", got)
	}
	r, ok := hasRecord(f.log, "debug", "let go of every held banner")
	if !ok || r.Context["reason"] != "the user selected a workspace" || r.Context["held"] != 2 {
		t.Fatalf("record = %+v (found=%v), want the reason and count recorded", r, ok)
	}
}

func TestAClickedBannerLetsGoOfEveryHeldBannerBeforeAskingEmacs(t *testing.T) {
	// Arrange
	f := newNotifierFixture(t, named)
	f.focus.Attach(false)
	f.backend.outcome = Unclicked
	f.backend.block = make(chan struct{})
	f.backend.madeSignal = make(chan struct{})
	f.notifier.Post("ws1", "turn_ended", titledBy)
	<-f.backend.madeSignal
	f.backend.mu.Lock()
	f.backend.outcome = Clicked
	f.backend.mu.Unlock()

	// Act
	close(f.backend.block)
	f.notifier.wg.Wait()

	// Assert
	if got := f.notifier.Claim(); got.Held {
		t.Fatalf("Claim = %+v, want none: the click chose the workspace", got)
	}
	if got := f.clicks.clicked(); len(got) != 1 || got[0] != "ws1" {
		t.Fatalf("clicks = %v, want [ws1]", got)
	}
	if _, ok := hasRecord(f.log, "debug", "let go of every held banner"); !ok {
		t.Fatal("the click let go of nothing on the record")
	}
}
