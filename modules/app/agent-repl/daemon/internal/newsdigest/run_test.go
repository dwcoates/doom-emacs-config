package newsdigest

import (
	"context"
	"errors"
	"strings"
	"testing"
	"time"

	frontendv1 "agentrepl/proto/frontend/v1"
	"google.golang.org/protobuf/proto"

	"claude-repld/internal/flock"
	"claude-repld/internal/headless"
)

func TestARefreshWithNewsStandsADigest(t *testing.T) {
	// Arrange
	w := newWorld(t)
	w.withNewFeedEntry()
	w.runner.text = answerJSON
	d := w.digester()

	// Act
	resp, err := d.Refresh(context.Background())

	// Assert
	if err != nil || resp.GetSuccess().GetShown().GetItems() != 1 {
		t.Fatalf("Refresh = (%v, %v), want shown with one item", resp, err)
	}
	shown := latestStanding(t, d).GetShown()
	if shown.GetId().GetValue() != "digest-1" || len(shown.GetSections()) != 1 {
		t.Fatalf("standing = %v, want digest-1 with its section", shown)
	}
}

func TestADigestsHeaderCoversTheBaselineToTheRunsEnd(t *testing.T) {
	// Arrange
	w := newWorld(t)
	w.withNewFeedEntry()
	w.runner.text = answerJSON
	d := w.digester()

	// Act
	if _, err := d.Refresh(context.Background()); err != nil {
		t.Fatalf("Refresh: %v", err)
	}

	// Assert
	header := latestStanding(t, d).GetShown().GetHeader()
	if header.GetPeriod().GetFromMs() != now.Add(-25*time.Hour).UnixMilli() || header.GetPeriod().GetToMs() != now.UnixMilli() {
		t.Fatalf("period = %v, want the baseline to now", header.GetPeriod())
	}
	if !strings.HasPrefix(header.GetTitle().GetText(), "Claude news · ") {
		t.Fatalf("title = %q, want the daemon's", header.GetTitle().GetText())
	}
}

func TestADigestRecordsTheRunWithItsSnapshotsAndOverlay(t *testing.T) {
	// Arrange
	w := newWorld(t)
	w.withNewFeedEntry()
	w.runner.text = answerJSON
	d := w.digester()

	// Act
	if _, err := d.Refresh(context.Background()); err != nil {
		t.Fatalf("Refresh: %v", err)
	}

	// Assert
	runs := w.store.recorded()
	if len(runs) != 1 || !runs[0].Recorded || runs[0].Digest == nil || runs[0].Digest.ID != "digest-1" {
		t.Fatalf("runs = %+v, want one recorded run with digest-1", runs)
	}
	if runs[0].Snapshots["feed"] != `["new","old"]` || runs[0].Snapshots["page"] != "Headline" {
		t.Fatalf("snapshots = %v, want both sources' new snapshots", runs[0].Snapshots)
	}
	stored := &frontendv1.NewsDigestOverlay{}
	if err := proto.Unmarshal(runs[0].Digest.Overlay, stored); err != nil || !proto.Equal(stored, latestStanding(t, d).GetShown()) {
		t.Fatalf("the stored overlay is not the one published (%v)", err)
	}
}

func TestADigestListsEverySourcesOutcome(t *testing.T) {
	// Arrange
	w := newWorld(t)
	w.withNewFeedEntry()
	w.fetcher.errs[pageSource.URL] = errScripted
	w.runner.text = answerJSON
	d := w.digester()

	// Act
	if _, err := d.Refresh(context.Background()); err != nil {
		t.Fatalf("Refresh: %v", err)
	}

	// Assert
	rows := latestStanding(t, d).GetShown().GetSources().GetSources()
	if len(rows) != 2 || rows[0].GetRead().GetNewEntries() != 1 || rows[0].GetUrl() != feedSource.Home {
		t.Fatalf("rows = %v, want the feed read with one new entry", rows)
	}
	if !strings.Contains(rows[1].GetFailed().GetReason(), "scripted failure") {
		t.Fatalf("page row = %v, want it failed with the cause", rows[1])
	}
}

func TestAFailedSourceKeepsItsOldSnapshot(t *testing.T) {
	// Arrange
	w := newWorld(t)
	w.withNewFeedEntry()
	w.fetcher.errs[pageSource.URL] = errScripted
	w.runner.text = answerJSON
	d := w.digester()

	// Act
	if _, err := d.Refresh(context.Background()); err != nil {
		t.Fatalf("Refresh: %v", err)
	}

	// Assert
	if _, recorded := w.store.recorded()[0].Snapshots["page"]; recorded {
		t.Fatal("a failed source's snapshot was replaced")
	}
	if len(records(w.log, "info", opSource)) != 1 {
		t.Fatalf("records = %v, want the failed source at INFO", w.log.Records())
	}
}

func TestARunWithNothingNewStandsNothingButIsRecorded(t *testing.T) {
	// Arrange
	w := newWorld(t)
	w.withNewFeedEntry()
	w.fetcher.bodies[feedSource.URL] = atomWith(now, "old")
	d := w.digester()

	// Act
	resp, err := d.Refresh(context.Background())

	// Assert
	if err != nil || resp.GetSuccess().GetNothingNew() == nil {
		t.Fatalf("Refresh = (%v, %v), want nothing_new", resp, err)
	}
	if runs := w.store.recorded(); len(runs) != 1 || !runs[0].Recorded || runs[0].Digest != nil {
		t.Fatalf("runs = %+v, want one recorded run with no digest", runs)
	}
	if len(w.runner.asked()) != 0 {
		t.Fatal("the model was asked with nothing new")
	}
	if _, published := d.Topic().Latest(); published {
		t.Fatal("a run with nothing new published a standing")
	}
}

func TestARunWhoseNewsTheModelFindsUnworthyStandsNothing(t *testing.T) {
	// Arrange
	w := newWorld(t)
	w.withNewFeedEntry()
	w.runner.text = `{"sections":[]}`
	d := w.digester()

	// Act
	resp, err := d.Refresh(context.Background())

	// Assert
	if err != nil || resp.GetSuccess().GetNothingNew() == nil {
		t.Fatalf("Refresh = (%v, %v), want nothing_new", resp, err)
	}
	if runs := w.store.recorded(); len(runs) != 1 || !runs[0].Recorded || runs[0].Digest != nil {
		t.Fatalf("runs = %+v, want one recorded run with no digest", runs)
	}
}

func TestAModelFailureRecordsOnlyTheRunsEnd(t *testing.T) {
	// Arrange
	w := newWorld(t)
	w.withNewFeedEntry()
	w.runner.err = &headless.Error{Cause: headless.CauseExitStatus, Detail: "exit 1"}
	d := w.digester()

	// Act
	resp, err := d.Refresh(context.Background())

	// Assert
	if err != nil || !strings.Contains(resp.GetError().GetModelFailed().GetReason(), "exit_status") {
		t.Fatalf("Refresh = (%v, %v), want model_failed naming the cause", resp, err)
	}
	if runs := w.store.recorded(); len(runs) != 1 || runs[0].Recorded {
		t.Fatalf("runs = %+v, want one unrecorded run (the material is read again next time)", runs)
	}
	if len(records(w.log, "error", opModel)) != 1 {
		t.Fatalf("records = %v, want the model failure at ERROR", w.log.Records())
	}
}

func TestAMalformedAnswerIsAModelFailureAndNoDigest(t *testing.T) {
	// Arrange
	w := newWorld(t)
	w.withNewFeedEntry()
	w.runner.text = `{"sections":[{"kind":"backend","items":[{"title":"T","summary":"S","links":[{"label":"L","url":"https://invented.test"}]}]}]}`
	d := w.digester()

	// Act
	resp, err := d.Refresh(context.Background())

	// Assert
	if err != nil || resp.GetError().GetModelFailed() == nil {
		t.Fatalf("Refresh = (%v, %v), want model_failed", resp, err)
	}
	if _, published := d.Topic().Latest(); published {
		t.Fatal("a malformed answer was published")
	}
}

func TestEverySourceFailingIsNoSourceRead(t *testing.T) {
	// Arrange
	w := newWorld(t)
	w.fetcher.errs[feedSource.URL] = errScripted
	w.fetcher.errs[pageSource.URL] = errScripted
	d := w.digester()

	// Act
	resp, err := d.Refresh(context.Background())

	// Assert
	if err != nil || resp.GetError().GetNoSourceRead() == nil {
		t.Fatalf("Refresh = (%v, %v), want no_source_read", resp, err)
	}
	if runs := w.store.recorded(); len(runs) != 1 || runs[0].Recorded {
		t.Fatalf("runs = %+v, want one unrecorded run", runs)
	}
	if len(records(w.log, "error", opRun)) != 1 {
		t.Fatalf("records = %v, want one ERROR", w.log.Records())
	}
}

func TestARefreshWhileOneRunsInThisDaemonIsAlreadyRunning(t *testing.T) {
	// Arrange
	w := newWorld(t)
	w.withNewFeedEntry()
	w.fetcher.block = make(chan struct{})
	w.fetcher.entered = make(chan string, 4)
	d := w.digester()
	first := make(chan error, 1)
	go func() {
		_, err := d.Refresh(context.Background())
		first <- err
	}()
	<-w.fetcher.entered

	// Act
	resp, err := d.Refresh(context.Background())

	// Assert
	close(w.fetcher.block)
	if firstErr := <-first; firstErr != nil {
		t.Fatalf("the first Refresh: %v", firstErr)
	}
	if err != nil || resp.GetError().GetAlreadyRunning() == nil {
		t.Fatalf("Refresh = (%v, %v), want already_running", resp, err)
	}
}

func TestARefreshWhileAnotherDaemonHoldsTheLockIsAlreadyRunning(t *testing.T) {
	// Arrange
	w := newWorld(t)
	held, ok, err := flock.TryExclusive(w.lock)
	if err != nil || !ok {
		t.Fatalf("taking the lock: %v %v", ok, err)
	}
	t.Cleanup(func() { _ = held.Release() })
	d := w.digester()

	// Act
	resp, err := d.Refresh(context.Background())

	// Assert
	if err != nil || resp.GetError().GetAlreadyRunning() == nil {
		t.Fatalf("Refresh = (%v, %v), want already_running", resp, err)
	}
	if len(w.store.recorded()) != 0 {
		t.Fatal("a refused run recorded something")
	}
}

func TestARefreshReportsAnUnreadableStore(t *testing.T) {
	// Arrange
	w := newWorld(t)
	w.store.stateErr = errScripted
	d := w.digester()

	// Act
	_, err := d.Refresh(context.Background())

	// Assert
	if !errors.Is(err, errScripted) {
		t.Fatalf("Refresh = %v, want the store's failure", err)
	}
}

func TestARefreshReportsAFailedRecord(t *testing.T) {
	// Arrange
	w := newWorld(t)
	w.withNewFeedEntry()
	w.runner.text = answerJSON
	w.store.recordErr = errScripted
	d := w.digester()

	// Act
	_, err := d.Refresh(context.Background())

	// Assert
	if !errors.Is(err, errScripted) {
		t.Fatalf("Refresh = %v, want the record's failure", err)
	}
	if _, published := d.Topic().Latest(); published {
		t.Fatal("a digest that was not recorded was published")
	}
}

func TestAStandingDigestIsCarriedIntoTheNextOne(t *testing.T) {
	// Arrange
	w := newWorld(t)
	w.withNewFeedEntry()
	earlier := now.Add(-50 * time.Hour).UnixMilli()
	w.store.state.LatestID = "d0"
	w.store.state.Standing = encodedOverlay(t, "d0", earlier)
	w.runner.text = answerJSON
	d := w.digester()

	// Act
	if _, err := d.Refresh(context.Background()); err != nil {
		t.Fatalf("Refresh: %v", err)
	}

	// Assert
	if !strings.Contains(w.runner.asked()[0].Prompt, "title: Earlier item") {
		t.Fatal("the still-unread digest was not handed to the model")
	}
	if from := latestStanding(t, d).GetShown().GetHeader().GetPeriod().GetFromMs(); from != earlier {
		t.Fatalf("period from = %d, want the carried digest's %d", from, earlier)
	}
}

func TestARunTheDaemonsStandDownEndsRecordsNothing(t *testing.T) {
	// Arrange
	w := newWorld(t)
	w.withNewFeedEntry()
	ctx, cancel := context.WithCancel(context.Background())
	w.runner.onRun = func(context.Context) { cancel() }
	w.runner.err = &headless.Error{Cause: headless.CauseExitStatus, Detail: "killed"}
	d := w.digester()

	// Act
	_, err := d.run(ctx, triggerRefresh, false)

	// Assert
	if !errors.Is(err, context.Canceled) {
		t.Fatalf("run = %v, want the cancellation", err)
	}
	if len(w.store.recorded()) != 0 || len(records(w.log, "error", opModel)) != 0 {
		t.Fatalf("a stood-down run recorded %v and logged %v", w.store.recorded(), w.log.Records())
	}
}

func TestAScheduledRunThatIsNoLongerDueDoesNotRun(t *testing.T) {
	// Arrange
	w := newWorld(t)
	w.store.state.LastRunEnd = now.Add(-time.Hour)
	d := w.digester()

	// Act
	_, err := d.run(context.Background(), triggerScheduled, true)

	// Assert
	if !errors.Is(err, errNotDue) || len(w.store.recorded()) != 0 {
		t.Fatalf("run = %v with %v recorded, want errNotDue and nothing recorded", err, w.store.recorded())
	}
}
