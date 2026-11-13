package kit

import (
	"strings"
	"sync"
	"testing"
)

// TestNextXvfbDisplayNeverRepeatsANumber pins the allocation that keeps every
// Xvfb start off every other server's display, concurrent callers included.
func TestNextXvfbDisplayNeverRepeatsANumber(t *testing.T) {
	t.Parallel()
	// Arrange
	const callers = 64
	got := make(chan int, callers)
	var wg sync.WaitGroup

	// Act
	for range callers {
		wg.Add(1)
		go func() {
			defer wg.Done()
			got <- NextXvfbDisplay()
		}()
	}
	wg.Wait()
	close(got)

	// Assert
	seen := map[int]bool{}
	for n := range got {
		if n < FirstXvfbDisplay {
			t.Fatalf("display %d is below the first handed out, %d", n, FirstXvfbDisplay)
		}
		if seen[n] {
			t.Fatalf("display %d was handed out twice", n)
		}
		seen[n] = true
	}
}

// TestXvfbArgvStartsTheServerOnItsHandedOutDisplay pins that the server is
// told its number rather than left to search for one, and still reports
// readiness through -displayfd.
func TestXvfbArgvStartsTheServerOnItsHandedOutDisplay(t *testing.T) {
	t.Parallel()
	// Act
	argv := XvfbArgv(17, "/fb")

	// Assert
	want := []string{"Xvfb", ":17", "-displayfd", "1", "-screen", "0", XvfbScreen, "-nolisten", "tcp", "-fbdir", "/fb"}
	if strings.Join(argv, " ") != strings.Join(want, " ") {
		t.Fatalf("argv = %q, want %q", argv, want)
	}
}
