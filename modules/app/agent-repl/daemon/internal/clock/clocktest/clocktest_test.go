package clocktest

import (
	"io/fs"
	"os"
	"path/filepath"
	"regexp"
	"slices"
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

func TestSameInstant(t *testing.T) {
	early := epoch
	alsoEarly := epoch.In(time.FixedZone("elsewhere", 3600))
	late := epoch.Add(time.Second)
	tests := []struct {
		name string
		a, b *time.Time
		want bool
	}{
		{name: "both nil", want: true},
		{name: "one nil", a: &early, want: false},
		{name: "the other nil", b: &early, want: false},
		{name: "the same instant in another zone", a: &early, b: &alsoEarly, want: true},
		{name: "different instants", a: &early, b: &late, want: false},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act.
			got := SameInstant(tt.a, tt.b)

			// Assert.
			if got != tt.want {
				t.Fatalf("SameInstant(%v, %v) = %v, want %v", tt.a, tt.b, got, tt.want)
			}
		})
	}
}

// TestOptionalInstantComparisonsHaveOneHome fails when a test file of the
// daemon hand-rolls its own comparison of two optional instants instead of
// calling SameInstant.
func TestOptionalInstantComparisonsHaveOneHome(t *testing.T) {
	// Arrange.
	root, err := filepath.Abs(filepath.Join("..", "..", ".."))
	if err != nil {
		t.Fatal(err)
	}
	shape := regexp.MustCompile(`func \w+\(\w+, \w+ \*time\.Time\) bool`)
	var sites []string

	// Act.
	err = filepath.WalkDir(root, func(path string, d fs.DirEntry, err error) error {
		if err != nil || d.IsDir() || !strings.HasSuffix(path, ".go") {
			return err
		}
		source, err := os.ReadFile(path)
		if err != nil {
			return err
		}
		for range shape.FindAll(source, -1) {
			sites = append(sites, strings.TrimPrefix(path, root+string(filepath.Separator)))
		}
		return nil
	})

	// Assert.
	if err != nil {
		t.Fatal(err)
	}
	if want := []string{filepath.Join("internal", "clock", "clocktest", "clocktest.go")}; !slices.Equal(sites, want) {
		t.Fatalf("optional instants are compared at %v, want only SameInstant in %v", sites, want)
	}
}
