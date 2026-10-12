package clocktest

import (
	"os"
	"path/filepath"
	"strings"
	"testing"
	"time"
)

var epoch = time.Date(2026, 10, 11, 12, 0, 0, 0, time.UTC)

func TestNowIsTheInstantItStandsAt(t *testing.T) {
	// Arrange.
	s := New(epoch)

	// Act.
	got := s.Now()

	// Assert.
	if !got.Equal(epoch) {
		t.Fatalf("Now = %v, want %v", got, epoch)
	}
}

func TestSetMovesNow(t *testing.T) {
	// Arrange.
	s := New(epoch)

	// Act.
	s.Set(epoch.Add(time.Hour))

	// Assert.
	if got := s.Now(); !got.Equal(epoch.Add(time.Hour)) {
		t.Fatalf("Now = %v, want an hour on", got)
	}
}

func TestAfterAnnouncesTheWaitAndAnswersOnFire(t *testing.T) {
	// Arrange.
	s := New(epoch)

	// Act.
	fired := s.After(time.Minute)
	asked := <-s.Asked
	go func() { s.Fire <- epoch }()
	at := <-fired

	// Assert.
	if asked != time.Minute || !at.Equal(epoch) {
		t.Fatalf("asked %v, fired %v; want a minute announced and the sent instant", asked, at)
	}
}

func TestAfterDoesNotFireUntilTold(t *testing.T) {
	// Arrange.
	s := New(epoch)

	// Act.
	fired := s.After(time.Minute)

	// Assert.
	select {
	case <-fired:
		t.Fatal("After fired before the test sent on Fire")
	default:
	}
}

// TestTheMigratedSuitesShareTheScriptedClock pins the suites that drive a
// periodic schedule to this one clock: a hand-rolled After beside them fails
// here rather than drifting from Scripted.
func TestTheMigratedSuitesShareTheScriptedClock(t *testing.T) {
	for _, dir := range []string{"..", "../../worktreereap", "../../newsdigest", "../../repodivergence"} {
		files, err := filepath.Glob(filepath.Join(dir, "*_test.go"))
		if err != nil || len(files) == 0 {
			t.Fatalf("globbing %s's tests: %v (%d files)", dir, err, len(files))
		}
		for _, file := range files {
			// Arrange.
			source, err := os.ReadFile(file)
			if err != nil {
				t.Fatalf("reading %s: %v", file, err)
			}

			// Act.
			handRolled := strings.Contains(string(source), ") After(d time.Duration) <-chan time.Time")

			// Assert.
			if handRolled {
				t.Errorf("%s declares its own After; use clocktest.Scripted", file)
			}
		}
	}
}
