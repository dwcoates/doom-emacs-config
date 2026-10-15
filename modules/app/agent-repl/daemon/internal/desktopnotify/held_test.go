package desktopnotify

import (
	"context"
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

// focusEmacs reports Emacs focused, as the focus gain a claim follows.
func focusEmacs(t *testing.T, f notifierFixture) {
	t.Helper()
	if err := f.focus.Report(true); err != nil {
		t.Fatalf("Report: %v", err)
	}
}

// claimTarget is the target a live claim of ws answers.
func claimTarget(ws ids.WorkspaceID) Target {
	return Target{ID: ws, Dir: named.workspace.Dir}
}

func TestClaimAnswersTheNewestHandedBannerThePlatformStillHolds(t *testing.T) {
	cases := []struct {
		name      string
		delivered map[ids.WorkspaceID]bool
		want      Claim
	}{
		{name: "the newest of two delivered banners", delivered: map[ids.WorkspaceID]bool{"ws1": true, "ws2": true},
			want: Claim{Live: true, Target: claimTarget("ws2")}},
		{name: "an older banner when the newest is no longer listed", delivered: map[ids.WorkspaceID]bool{"ws1": true},
			want: Claim{Live: true, Target: claimTarget("ws1")}},
		{name: "none when the platform lists neither", delivered: map[ids.WorkspaceID]bool{"other": true},
			want: Claim{}},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			f := heldFixture(t)
			postSettled(f, "ws1")
			postSettled(f, "ws2")
			f.backend.delivered = tc.delivered
			focusEmacs(t, f)

			// Act
			got, err := f.notifier.Claim(context.Background())

			// Assert
			if err != nil || got != tc.want {
				t.Fatalf("Claim = (%+v, %v), want (%+v, nil)", got, err, tc.want)
			}
			r, ok := hasRecord(f.log, "info", "answered a focus claim")
			if !ok || r.Context["held"] != 2 || r.Context["live"] != tc.want.Live || r.Context["workspace"] != string(tc.want.Target.ID) {
				t.Fatalf("record = %+v (found=%v), want the claim's outcome recorded", r, ok)
			}
		})
	}
}

func TestClaimWithNothingHeldAnswersNoneAndAsksNothing(t *testing.T) {
	// Arrange
	f := heldFixture(t)
	focusEmacs(t, f)

	// Act
	got, err := f.notifier.Claim(context.Background())

	// Assert
	if err != nil || got.Live {
		t.Fatalf("Claim = (%+v, %v), want none", got, err)
	}
	if asks := f.backend.deliveredAsks(); asks != 0 {
		t.Fatalf("the platform was asked %d times, want none", asks)
	}
}

func TestClaimLetsGoOfEveryBannerItRead(t *testing.T) {
	// Arrange
	f := heldFixture(t)
	postSettled(f, "ws1")
	postSettled(f, "ws2")
	f.backend.delivered = map[ids.WorkspaceID]bool{"ws1": true, "ws2": true}
	focusEmacs(t, f)
	if _, err := f.notifier.Claim(context.Background()); err != nil {
		t.Fatalf("first Claim: %v", err)
	}

	// Act
	got, err := f.notifier.Claim(context.Background())

	// Assert
	if err != nil || got.Live {
		t.Fatalf("second Claim = (%+v, %v), want none: the first consumed every banner", got, err)
	}
}

func TestClaimWhileEmacsIsUnfocusedIsRefusedAndConsumesNothing(t *testing.T) {
	// Arrange
	f := heldFixture(t)
	postSettled(f, "ws1")
	f.backend.delivered = map[ids.WorkspaceID]bool{"ws1": true}

	// Act
	_, err := f.notifier.Claim(context.Background())

	// Assert
	if !errors.Is(err, ErrEditorUnfocused) {
		t.Fatalf("Claim = %v, want ErrEditorUnfocused", err)
	}
	if _, ok := hasRecord(f.log, "info", "a claim arrived while Emacs reads as unfocused; it was refused"); !ok {
		t.Fatal("the refused claim left no record")
	}
	focusEmacs(t, f)
	if got, err := f.notifier.Claim(context.Background()); err != nil || got != (Claim{Live: true, Target: claimTarget("ws1")}) {
		t.Fatalf("the focused Claim = (%+v, %v), want ws1: the refused one consumed nothing", got, err)
	}
}

func TestClaimWithNoEmacsStreamIsRefused(t *testing.T) {
	// Arrange
	f := newNotifierFixture(t, named)

	// Act
	_, err := f.notifier.Claim(context.Background())

	// Assert
	if !errors.Is(err, ErrEditorUnfocused) {
		t.Fatalf("Claim = %v, want ErrEditorUnfocused", err)
	}
}

func TestClaimSurfacesAFailedDeliveredQueryAndConsumesNothing(t *testing.T) {
	// Arrange
	f := heldFixture(t)
	postSettled(f, "ws1")
	cause := errors.New("exit status 1: list refused")
	f.backend.deliveredErr = cause
	focusEmacs(t, f)

	// Act
	got, err := f.notifier.Claim(context.Background())

	// Assert
	if !errors.Is(err, cause) || got.Live {
		t.Fatalf("Claim = (%+v, %v), want the program's failure", got, err)
	}
	if _, ok := hasRecord(f.log, "info", "answered a focus claim"); ok {
		t.Fatal("a failed claim was recorded as answered")
	}
	f.backend.deliveredErr = nil
	f.backend.delivered = map[ids.WorkspaceID]bool{"ws1": true}
	if got, err := f.notifier.Claim(context.Background()); err != nil || !got.Live {
		t.Fatalf("the retried Claim = (%+v, %v), want ws1: the failed one consumed nothing", got, err)
	}
}

func TestClaimAnswersAReadBackBannerStillShowingWithoutAskingThePlatform(t *testing.T) {
	// Arrange
	f := newNotifierFixture(t, named)
	f.focus.Attach(false)
	f.backend.outcome = Unclicked
	f.backend.block = make(chan struct{})
	f.backend.madeSignal = make(chan struct{})
	f.notifier.Post("ws1", "turn_ended", titledBy)
	<-f.backend.madeSignal
	focusEmacs(t, f)

	// Act
	got, err := f.notifier.Claim(context.Background())

	// Assert
	close(f.backend.block)
	f.notifier.wg.Wait()
	if err != nil || got != (Claim{Live: true, Target: claimTarget("ws1")}) {
		t.Fatalf("Claim = (%+v, %v), want the showing banner's ws1", got, err)
	}
	if asks := f.backend.deliveredAsks(); asks != 0 {
		t.Fatalf("the platform was asked %d times, want none for a read-back banner", asks)
	}
}

func TestADismissedReadBackBannerIsNoLongerHeld(t *testing.T) {
	// Arrange
	f := newNotifierFixture(t, named)
	f.focus.Attach(false)
	f.backend.outcome = Unclicked
	postSettled(f, "ws1")
	focusEmacs(t, f)

	// Act
	got, err := f.notifier.Claim(context.Background())

	// Assert
	if err != nil || got.Live {
		t.Fatalf("Claim = (%+v, %v), want none: the banner left the desktop", got, err)
	}
}

func TestAFailedBannerIsNeverHeld(t *testing.T) {
	// Arrange
	f := heldFixture(t)
	f.backend.err = errors.New("exit status 3: notifications are not authorized")
	postSettled(f, "ws1")
	focusEmacs(t, f)

	// Act
	got, err := f.notifier.Claim(context.Background())

	// Assert
	if err != nil || got.Live {
		t.Fatalf("Claim = (%+v, %v), want none: no banner was made", got, err)
	}
}

func TestAWorkspacesLaterBannerReplacesItsEarlierOne(t *testing.T) {
	// Arrange
	f := heldFixture(t)
	postSettled(f, "ws1")
	postSettled(f, "ws2")
	postSettled(f, "ws1")
	f.backend.delivered = map[ids.WorkspaceID]bool{"ws1": true, "ws2": true}
	focusEmacs(t, f)

	// Act
	got, err := f.notifier.Claim(context.Background())

	// Assert
	if err != nil || got != (Claim{Live: true, Target: claimTarget("ws1")}) {
		t.Fatalf("Claim = (%+v, %v), want ws1, whose later banner is the newest", got, err)
	}
	r, _ := hasRecord(f.log, "info", "answered a focus claim")
	if r.Context["held"] != 2 {
		t.Fatalf("held %v banners, want 2: one per workspace", r.Context["held"])
	}
}

func TestClaimSparesABannerMadeWhileItAskedThePlatform(t *testing.T) {
	// Arrange
	f := heldFixture(t)
	postSettled(f, "ws1")
	f.backend.delivered = map[ids.WorkspaceID]bool{"ws1": true, "ws2": true}
	f.backend.onDelivered = func() {
		f.backend.onDelivered = nil
		f.notifier.hold(claimTarget("ws2"), f.log)
	}
	focusEmacs(t, f)

	// Act
	first, err := f.notifier.Claim(context.Background())

	// Assert
	if err != nil || first != (Claim{Live: true, Target: claimTarget("ws1")}) {
		t.Fatalf("Claim = (%+v, %v), want ws1, the one banner it read", first, err)
	}
	second, err := f.notifier.Claim(context.Background())
	if err != nil || second != (Claim{Live: true, Target: claimTarget("ws2")}) {
		t.Fatalf("the next Claim = (%+v, %v), want ws2: a banner made meanwhile is spared", second, err)
	}
}

func TestForgetLetsGoOfEveryHeldBanner(t *testing.T) {
	// Arrange
	f := heldFixture(t)
	postSettled(f, "ws1")
	postSettled(f, "ws2")
	f.backend.delivered = map[ids.WorkspaceID]bool{"ws1": true, "ws2": true}
	focusEmacs(t, f)

	// Act
	f.notifier.Forget("the user selected a workspace")

	// Assert
	if got, err := f.notifier.Claim(context.Background()); err != nil || got.Live {
		t.Fatalf("Claim = (%+v, %v), want none after Forget", got, err)
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
	focusEmacs(t, f)
	if got, err := f.notifier.Claim(context.Background()); err != nil || got.Live {
		t.Fatalf("Claim = (%+v, %v), want none: the click chose the workspace", got, err)
	}
	if got := f.clicks.clicked(); len(got) != 1 || got[0] != "ws1" {
		t.Fatalf("clicks = %v, want [ws1]", got)
	}
	if _, ok := hasRecord(f.log, "debug", "let go of every held banner"); !ok {
		t.Fatal("the click let go of nothing on the record")
	}
}
