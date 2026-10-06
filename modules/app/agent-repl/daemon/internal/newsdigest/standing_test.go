package newsdigest

import (
	"context"
	"errors"
	"testing"
	"time"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	frontendv1 "agentrepl/proto/frontend/v1"
	"google.golang.org/protobuf/proto"
)

// dismissReq is a DismissNewsDigest request naming id.
func dismissReq(id string) *agentreplv1.DismissNewsDigestRequest {
	return &agentreplv1.DismissNewsDigestRequest{Id: &frontendv1.NewsDigestId{Value: id}}
}

func TestRepublishPublishesTheStandingDigest(t *testing.T) {
	// Arrange
	w := newWorld(t)
	w.store.state.LatestID = "d1"
	w.store.state.Standing = encodedOverlay(t, "d1", 1)
	d := w.digester()

	// Act
	err := d.Republish(context.Background())

	// Assert
	if err != nil {
		t.Fatalf("Republish: %v", err)
	}
	if got := latestStanding(t, d).GetShown().GetId().GetValue(); got != "d1" {
		t.Fatalf("shown id = %q, want d1", got)
	}
}

func TestRepublishPublishesNoneWhenNoDigestStands(t *testing.T) {
	// Arrange
	w := newWorld(t)
	d := w.digester()

	// Act
	err := d.Republish(context.Background())

	// Assert
	if err != nil || latestStanding(t, d).GetNone() == nil {
		t.Fatalf("Republish = %v, standing %v, want none", err, latestStanding(t, d))
	}
}

func TestRepublishReportsAnUnreadableStore(t *testing.T) {
	// Arrange
	w := newWorld(t)
	w.store.stateErr = errScripted
	d := w.digester()

	// Act
	err := d.Republish(context.Background())

	// Assert
	if !errors.Is(err, errScripted) {
		t.Fatalf("Republish = %v, want the store's failure", err)
	}
	if len(records(w.log, "error", opStanding)) != 1 {
		t.Fatalf("records = %v, want one ERROR", w.log.Records())
	}
	if _, ok := d.Topic().Latest(); ok {
		t.Fatal("a standing was published from an unreadable store")
	}
}

func TestRepublishRefusesAStoredDigestThatDoesNotDecode(t *testing.T) {
	// Arrange
	w := newWorld(t)
	w.store.state.LatestID = "d1"
	w.store.state.Standing = []byte{0xff, 0xff}
	d := w.digester()

	// Act
	err := d.Republish(context.Background())

	// Assert
	if err == nil || len(records(w.log, "error", opStanding)) != 1 {
		t.Fatalf("Republish = %v, records %v, want a refusal recorded at ERROR", err, w.log.Records())
	}
}

func TestDecodeStandingRefusesAnIncompleteOverlay(t *testing.T) {
	tests := []struct {
		name    string
		overlay *frontendv1.NewsDigestOverlay
	}{
		{name: "no id", overlay: &frontendv1.NewsDigestOverlay{}},
		{name: "a section with no kind", overlay: &frontendv1.NewsDigestOverlay{
			Id:       &frontendv1.NewsDigestId{Value: "d"},
			Sections: []*frontendv1.NewsDigestSection{{Items: []*frontendv1.NewsDigestItem{{}}}},
		}},
		{name: "a section with no items", overlay: &frontendv1.NewsDigestOverlay{
			Id:       &frontendv1.NewsDigestId{Value: "d"},
			Sections: []*frontendv1.NewsDigestSection{{Kind: kinds[0].arm()}},
		}},
		{name: "a week with no outcome", overlay: &frontendv1.NewsDigestOverlay{
			Id:   &frontendv1.NewsDigestId{Value: "d"},
			Week: &frontendv1.NewsDigestWeek{Heading: &frontendv1.NewsDigestSectionHeading{Text: "Since last week"}},
		}},
		{name: "a week of no risks", overlay: &frontendv1.NewsDigestOverlay{
			Id: &frontendv1.NewsDigestId{Value: "d"},
			Week: &frontendv1.NewsDigestWeek{Outcome: &frontendv1.NewsDigestWeek_Risks{
				Risks: &frontendv1.NewsDigestWeekRisks{},
			}},
		}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			encoded, err := proto.Marshal(tt.overlay)
			if err != nil {
				t.Fatalf("marshal: %v", err)
			}

			// Act
			_, err = decodeStanding(encoded)

			// Assert
			if err == nil {
				t.Fatal("decodeStanding = nil, want a refusal")
			}
		})
	}
}

func TestDecodeStandingAcceptsADigestMadeBeforeTheWeek(t *testing.T) {
	// Arrange
	encoded := encodedOverlay(t, "d", now.UnixMilli())

	// Act
	overlay, err := decodeStanding(encoded)

	// Assert
	if err != nil || overlay.Week != nil {
		t.Fatalf("decodeStanding = (%v, %v), want the earlier digest with no week", overlay, err)
	}
}

func TestDismissingTheStandingDigestPublishesNone(t *testing.T) {
	// Arrange
	w := newWorld(t)
	w.store.state.LatestID = "d1"
	w.store.state.Standing = encodedOverlay(t, "d1", 1)
	d := w.digester()
	if err := d.Republish(context.Background()); err != nil {
		t.Fatalf("Republish: %v", err)
	}

	// Act
	resp, err := d.Dismiss(context.Background(), dismissReq("d1"))

	// Assert
	if err != nil || resp.GetSuccess() == nil {
		t.Fatalf("Dismiss = (%v, %v), want success", resp, err)
	}
	if latestStanding(t, d).GetNone() == nil {
		t.Fatalf("standing = %v, want none", latestStanding(t, d))
	}
}

func TestDismissingTwiceIsSuccess(t *testing.T) {
	// Arrange
	w := newWorld(t)
	w.store.state.LatestID = "d1"
	w.store.state.Standing = encodedOverlay(t, "d1", 1)
	d := w.digester()
	if _, err := d.Dismiss(context.Background(), dismissReq("d1")); err != nil {
		t.Fatalf("first Dismiss: %v", err)
	}

	// Act
	resp, err := d.Dismiss(context.Background(), dismissReq("d1"))

	// Assert
	if err != nil || resp.GetSuccess() == nil {
		t.Fatalf("second Dismiss = (%v, %v), want success", resp, err)
	}
}

func TestDismissingAnUnknownDigestAnswersUnknownDigest(t *testing.T) {
	// Arrange
	w := newWorld(t)
	w.store.state.LatestID = "d2"
	w.store.state.Standing = encodedOverlay(t, "d2", 1)
	d := w.digester()
	if err := d.Republish(context.Background()); err != nil {
		t.Fatalf("Republish: %v", err)
	}

	// Act
	resp, err := d.Dismiss(context.Background(), dismissReq("d1"))

	// Assert
	if err != nil || resp.GetError().GetUnknownDigest() == nil {
		t.Fatalf("Dismiss = (%v, %v), want unknown_digest", resp, err)
	}
	if latestStanding(t, d).GetShown() == nil {
		t.Fatal("an unknown dismiss took the standing digest down")
	}
}

func TestDismissReportsAStoreFailure(t *testing.T) {
	// Arrange
	w := newWorld(t)
	w.store.dismissErr = errScripted
	d := w.digester()

	// Act
	_, err := d.Dismiss(context.Background(), dismissReq("d1"))

	// Assert
	if !errors.Is(err, errScripted) || len(records(w.log, "error", opDismiss)) != 1 {
		t.Fatalf("Dismiss = %v, records %v, want the failure recorded at ERROR", err, w.log.Records())
	}
}

// localDay answers an instant on 2026-10-DAY at hour h on the LOCAL calendar,
// so "today" means the same thing to the test as to sameLocalDay.
func localDay(day, h int) time.Time {
	return time.Date(2026, 10, day, h, 0, 0, 0, time.Local)
}

// dismissedDigest seeds a digest minted at madeAt and dismissed since.
func dismissedDigest(t *testing.T, w *world, madeAt time.Time) {
	t.Helper()
	w.store.state.LatestID = "d1"
	w.store.state.LatestOverlay = encodedOverlay(t, "d1", 1)
	w.store.state.LatestMadeAt = madeAt
}

func TestRedisplayStandsTheDaysDismissedDigestAgain(t *testing.T) {
	// Arrange
	w := newWorld(t)
	w.clock.set(localDay(2, 23))
	dismissedDigest(t, w, localDay(2, 1))
	d := w.digester()

	// Act
	err := d.Redisplay(context.Background())

	// Assert
	if err != nil {
		t.Fatalf("Redisplay: %v", err)
	}
	if got := latestStanding(t, d).GetShown().GetId().GetValue(); got != "d1" {
		t.Fatalf("shown id = %q, want d1 standing again", got)
	}
	if w.store.state.Standing == nil {
		t.Fatal("the store does not hold the digest standing again")
	}
	if len(records(w.log, "info", opRestand)) != 1 {
		t.Fatalf("records = %v, want one INFO for the redisplay", w.log.Records())
	}
}

func TestRedisplayLeavesADigestFromAnEarlierDayDown(t *testing.T) {
	// Arrange
	w := newWorld(t)
	w.clock.set(localDay(2, 1))
	dismissedDigest(t, w, localDay(1, 23))
	d := w.digester()

	// Act
	err := d.Redisplay(context.Background())

	// Assert
	if err != nil || w.store.state.Standing != nil {
		t.Fatalf("Redisplay = %v, standing %q, want nothing stood", err, w.store.state.Standing)
	}
	if _, ok := d.Topic().Latest(); ok {
		t.Fatal("a standing was published for yesterday's digest")
	}
}

func TestRedisplayLeavesAStandingDigestUnchanged(t *testing.T) {
	// Arrange
	w := newWorld(t)
	w.clock.set(localDay(2, 23))
	dismissedDigest(t, w, localDay(2, 1))
	w.store.state.Standing = w.store.state.LatestOverlay
	d := w.digester()

	// Act
	err := d.Redisplay(context.Background())

	// Assert
	if err != nil {
		t.Fatalf("Redisplay: %v", err)
	}
	if _, ok := d.Topic().Latest(); ok {
		t.Fatal("a standing digest was republished by an Emacs restart")
	}
}

func TestRedisplayWithNoKeptDigestChangesNothing(t *testing.T) {
	// Arrange
	w := newWorld(t)
	d := w.digester()

	// Act
	err := d.Redisplay(context.Background())

	// Assert
	if err != nil || w.store.state.Standing != nil {
		t.Fatalf("Redisplay = %v, standing %q, want nothing", err, w.store.state.Standing)
	}
}

func TestRedisplayReportsAnUnreadableStore(t *testing.T) {
	// Arrange
	w := newWorld(t)
	w.store.stateErr = errScripted
	d := w.digester()

	// Act
	err := d.Redisplay(context.Background())

	// Assert
	if !errors.Is(err, errScripted) || len(records(w.log, "error", opRestand)) != 1 {
		t.Fatalf("Redisplay = %v, records %v, want the store's failure recorded once at ERROR", err, w.log.Records())
	}
}

func TestRedisplayReportsAFailedRestand(t *testing.T) {
	// Arrange
	w := newWorld(t)
	w.clock.set(localDay(2, 23))
	dismissedDigest(t, w, localDay(2, 1))
	w.store.restandErr = errScripted
	d := w.digester()

	// Act
	err := d.Redisplay(context.Background())

	// Assert
	if !errors.Is(err, errScripted) || len(records(w.log, "error", opRestand)) != 1 {
		t.Fatalf("Redisplay = %v, records %v, want the failure recorded once at ERROR", err, w.log.Records())
	}
	if _, ok := d.Topic().Latest(); ok {
		t.Fatal("a standing was published although the store refused it")
	}
}
