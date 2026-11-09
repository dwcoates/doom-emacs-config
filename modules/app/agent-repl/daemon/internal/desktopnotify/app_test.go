package desktopnotify

import (
	"context"
	"errors"
	"strings"
	"testing"
	"time"

	"agent-repld/internal/dlog"
)

// appTarget is the fixture's banner target.
var appTarget = Target{ID: "ws1", Dir: "/work/ws1"}

// appAnswer is a test's fake app: it takes the next banner off stream and
// answers it with outcome, handing the banner back on the returned channel.
func appAnswer(t *testing.T, app *App, stream *AppStream, outcome PostOutcome) <-chan AppBanner {
	t.Helper()
	got := make(chan AppBanner, 1)
	go func() {
		banner := <-stream.Banners()
		got <- banner
		if err := app.Answer(banner.PostID, outcome); err != nil {
			t.Errorf("Answer(%d): %v", banner.PostID, err)
		}
	}()
	return got
}

func TestAppPostWithNoStreamFailsAtOnce(t *testing.T) {
	// Arrange
	app := NewApp(dlog.NewTestLogger(), time.Minute)
	made := false

	// Act
	outcome, err := app.Post(context.Background(), appTarget, Banner{Title: "T", Body: "B"}, func() { made = true })

	// Assert
	if !errors.Is(err, ErrNoApp) || outcome != Unclicked || made {
		t.Fatalf("Post = (%v, %v), made %v; want (Unclicked, ErrNoApp) and nothing made", outcome, err, made)
	}
}

func TestAppPostHandsTheBannerAndIsMadeOnceThePlatformTookIt(t *testing.T) {
	// Arrange
	app := NewApp(dlog.NewTestLogger(), time.Minute)
	stream, release := app.Attach(42, "build-42")
	defer release()
	handed := appAnswer(t, app, stream, PostOutcome{Posted: true})
	made := 0

	// Act
	outcome, err := app.Post(context.Background(), appTarget, Banner{Title: "T", Body: "B"}, func() { made++ })

	// Assert
	if err != nil || outcome != Handed || made != 1 {
		t.Fatalf("Post = (%v, %v), made %d; want (Handed, nil) made once", outcome, err, made)
	}
	banner := <-handed
	if banner.Banner != (Banner{Title: "T", Body: "B"}) || banner.Target != appTarget || banner.PostID == 0 {
		t.Fatalf("handed %+v, want the banner, its target and a post id", banner)
	}
	if banner.Group != bannerGroup(appTarget.ID) {
		t.Fatalf("group = %q, want the workspace's group %q", banner.Group, bannerGroup(appTarget.ID))
	}
}

func TestAppPostSurfacesThePlatformsRefusal(t *testing.T) {
	cases := []struct {
		name    string
		outcome PostOutcome
		check   func(error) bool
	}{
		{name: "not allowed", outcome: PostOutcome{NotAllowed: true}, check: func(err error) bool { return errors.Is(err, ErrNotAllowed) }},
		{name: "failed", outcome: PostOutcome{PlatformError: "the center is unavailable"}, check: func(err error) bool {
			return err != nil && strings.Contains(err.Error(), "the center is unavailable")
		}},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			app := NewApp(dlog.NewTestLogger(), time.Minute)
			stream, release := app.Attach(42, "build-42")
			defer release()
			appAnswer(t, app, stream, tc.outcome)
			made := false

			// Act
			outcome, err := app.Post(context.Background(), appTarget, Banner{Title: "T", Body: "B"}, func() { made = true })

			// Assert
			if !tc.check(err) || outcome != Unclicked || made {
				t.Fatalf("Post = (%v, %v), made %v; want the refusal surfaced and nothing made", outcome, err, made)
			}
		})
	}
}

func TestAppPostFailsWhenNoAnswerArrivesWithinTheBound(t *testing.T) {
	// Arrange
	app := NewApp(dlog.NewTestLogger(), 50*time.Millisecond)
	stream, release := app.Attach(42, "build-42")
	defer release()
	taken := make(chan AppBanner, 1)
	go func() { taken <- <-stream.Banners() }()
	made := false

	// Act
	outcome, err := app.Post(context.Background(), appTarget, Banner{Title: "T", Body: "B"}, func() { made = true })

	// Assert
	if err == nil || !strings.Contains(err.Error(), "did not answer") || outcome != Unclicked || made {
		t.Fatalf("Post = (%v, %v), made %v; want an unanswered post's failure", outcome, err, made)
	}
	if err := app.Answer((<-taken).PostID, PostOutcome{Posted: true}); !errors.Is(err, ErrUnknownPost) {
		t.Fatalf("a late answer = %v, want ErrUnknownPost", err)
	}
}

func TestAppPostFailsWhenItsStreamEndsBeforeTheAnswer(t *testing.T) {
	// Arrange
	app := NewApp(dlog.NewTestLogger(), time.Minute)
	stream, release := app.Attach(42, "build-42")
	go func() {
		<-stream.Banners()
		release()
	}()

	// Act
	outcome, err := app.Post(context.Background(), appTarget, Banner{Title: "T", Body: "B"}, func() {})

	// Assert
	if !errors.Is(err, ErrNoApp) || outcome != Unclicked {
		t.Fatalf("Post = (%v, %v), want the stream's end surfaced as ErrNoApp", outcome, err)
	}
}

func TestAppAnswerOfAnUnknownPostIsRefusedAndRecorded(t *testing.T) {
	// Arrange
	log := dlog.NewTestLogger()
	app := NewApp(log, time.Minute)

	// Act
	err := app.Answer(7, PostOutcome{Posted: true})

	// Assert
	if !errors.Is(err, ErrUnknownPost) {
		t.Fatalf("Answer = %v, want ErrUnknownPost", err)
	}
	r, ok := dlog.FindRecord(log.Records(), dlog.RecordQuery{Level: "info", Message: "the notifier app answered a post nothing awaits"})
	if !ok || r.Context["post"] != uint64(7) {
		t.Fatalf("record = %+v (found=%v), want the unknown post recorded with its id", r, ok)
	}
}

func TestAppALaterStreamSupersedesTheEarlierOne(t *testing.T) {
	// Arrange
	log := dlog.NewTestLogger()
	app := NewApp(log, time.Minute)
	first, releaseFirst := app.Attach(1, "build-1")

	// Act
	second, releaseSecond := app.Attach(2, "build-2")
	defer releaseSecond()

	// Assert
	select {
	case <-first.Superseded():
	default:
		t.Fatal("the earlier stream was not told it was superseded")
	}
	releaseFirst()
	handed := appAnswer(t, app, second, PostOutcome{Posted: true})
	if _, err := app.Post(context.Background(), appTarget, Banner{Title: "T", Body: "B"}, func() {}); err != nil {
		t.Fatalf("Post after the superseded stream's release = %v, want the successor to post", err)
	}
	<-handed
	r, ok := dlog.FindRecord(log.Records(), dlog.RecordQuery{Level: "info", Message: "the notifier app attached its banner stream"})
	if !ok {
		t.Fatal("the attach was not recorded")
	}
	_ = r
	if _, ok := dlog.FindRecord(log.Records(), dlog.RecordQuery{Level: "debug", Message: "a superseded notifier app stream ended; its successor stands"}); !ok {
		t.Fatal("the superseded stream's end was not recorded")
	}
}

func TestAppReleaseOfTheStandingStreamEmptiesTheSlot(t *testing.T) {
	// Arrange
	log := dlog.NewTestLogger()
	app := NewApp(log, time.Minute)
	_, release := app.Attach(1, "build-1")

	// Act
	release()

	// Assert
	if _, err := app.Post(context.Background(), appTarget, Banner{Title: "T", Body: "B"}, func() {}); !errors.Is(err, ErrNoApp) {
		t.Fatalf("Post after release = %v, want ErrNoApp", err)
	}
	if _, ok := dlog.FindRecord(log.Records(), dlog.RecordQuery{Level: "info", Message: "the notifier app's banner stream ended; no app posts banners until one attaches"}); !ok {
		t.Fatal("the stream's end was not recorded")
	}
}

func TestAppPostHonorsItsContext(t *testing.T) {
	// Arrange
	app := NewApp(dlog.NewTestLogger(), time.Minute)
	_, release := app.Attach(1, "build-1")
	defer release()
	ctx, cancel := context.WithCancel(context.Background())
	cancel()

	// Act
	_, err := app.Post(ctx, appTarget, Banner{Title: "T", Body: "B"}, func() {})

	// Assert
	if !errors.Is(err, context.Canceled) {
		t.Fatalf("Post = %v, want the cancelled context", err)
	}
}

func TestPostOutcomeErrPanicsOnAnEmptyOutcome(t *testing.T) {
	// Assert
	defer func() {
		if recover() == nil {
			t.Fatal("an outcome that is none of the three did not panic")
		}
	}()

	// Act
	_ = PostOutcome{}.err()
}

func TestAppRunningBuildIsTheStandingStreamsBuild(t *testing.T) {
	// Arrange
	app := NewApp(dlog.NewTestLogger(), time.Minute)
	_, releaseFirst := app.Attach(1, "build-a")

	// Act
	_, releaseSecond := app.Attach(2, "build-b")
	build, attached := app.RunningBuild()
	releaseFirst()
	stillSecond, stillAttached := app.RunningBuild()
	releaseSecond()
	_, afterRelease := app.RunningBuild()

	// Assert
	if !attached || build != "build-b" || !stillAttached || stillSecond != "build-b" {
		t.Fatalf("RunningBuild = %q,%v then %q,%v; want the successor's build-b throughout", build, attached, stillSecond, stillAttached)
	}
	if afterRelease {
		t.Fatal("RunningBuild answered a build after every stream ended")
	}
}
