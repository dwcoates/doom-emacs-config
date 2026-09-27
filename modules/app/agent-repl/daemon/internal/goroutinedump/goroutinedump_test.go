package goroutinedump

import (
	"strings"
	"testing"
)

// TestRenderNamesTheBlockedGoroutine pins what the dump is FOR: a goroutine
// parked on a channel has to appear in it by name, because that stack is the
// only thing that identifies a wedge's blocking call on a host where no
// debugger can attach.
func TestRenderNamesTheBlockedGoroutine(t *testing.T) {
	// Arrange.
	release := make(chan struct{})
	parked := make(chan struct{})
	go func() {
		close(parked)
		<-release
	}()
	<-parked
	defer close(release)

	// Act.
	dump, count := Render()

	// Assert.
	if count < 2 {
		t.Fatalf("goroutine count = %d, want at least the test's own and the parked one", count)
	}
	if !strings.Contains(dump, "TestRenderNamesTheBlockedGoroutine") {
		t.Fatalf("dump = %q, want the parked goroutine's own stack in it", dump)
	}
}

// TestTruncate pins the cap: a rendering at the cap is kept whole, and one
// past it is cut there and says so rather than silently.
func TestTruncate(t *testing.T) {
	tests := []struct {
		name     string
		in       string
		wantHead int
		wantNote bool
	}{
		{name: "a rendering at the cap is kept whole", in: strings.Repeat("g", Cap), wantHead: Cap},
		{name: "a rendering past the cap is cut and says so", in: strings.Repeat("g", Cap+1), wantHead: Cap, wantNote: true},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange: the table row.

			// Act.
			got := truncate(tt.in)

			// Assert.
			if head := len(strings.SplitN(got, "\n", 2)[0]); head != tt.wantHead {
				t.Fatalf("kept %d bytes of the rendering, want %d", head, tt.wantHead)
			}
			if noted := strings.Contains(got, "was truncated at"); noted != tt.wantNote {
				t.Fatalf("truncation note present = %v, want %v", noted, tt.wantNote)
			}
		})
	}
}
