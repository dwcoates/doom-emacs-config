package shim

import (
	"bytes"
	"os/exec"
	"strings"
	"testing"
)

func TestStderrTailBoundsWhatItRetains(t *testing.T) {
	// Arrange: a child that outtalks the cap by a wide margin.
	tail := &stderrTail{}

	// Act
	if _, err := tail.Write([]byte(strings.Repeat("a", maxStderrTail) + strings.Repeat("b", maxStderrTail))); err != nil {
		t.Fatal(err)
	}

	// Assert: capped, newest bytes kept, oldest dropped, drop announced.
	got := tail.String()
	if len(got) != len(stderrTruncationMarker)+maxStderrTail {
		t.Fatalf("tail length = %d, want marker + %d", len(got), maxStderrTail)
	}
	if !strings.HasPrefix(got, stderrTruncationMarker) {
		t.Fatalf("tail %q does not announce that it dropped bytes", got[:64])
	}
	if retained := strings.TrimPrefix(got, stderrTruncationMarker); strings.Contains(retained, "a") {
		t.Fatal("tail retained the oldest bytes instead of the newest")
	}
}

func TestStderrTailReportsAnUntruncatedTailUnmarked(t *testing.T) {
	// Arrange
	tail := &stderrTail{}

	// Act
	if _, err := tail.Write([]byte("boom\n")); err != nil {
		t.Fatal(err)
	}

	// Assert
	if got := tail.String(); got != "boom\n" {
		t.Fatalf("tail = %q, want the exact bytes with no truncation marker", got)
	}
}

func TestStderrTailIsEmptyForAProcessThatSaidNothing(t *testing.T) {
	// Arrange
	tail := &stderrTail{}

	// Act / Assert
	if got := tail.String(); got != "" {
		t.Fatalf("tail = %q, want empty", got)
	}
}

func TestSpawnRetainsUnparseableStderrForTheSpawnFailureRecord(t *testing.T) {
	// Arrange / Act: the crash text a dying node prints is not a shim record,
	// and it is exactly the evidence a spawn failure has to carry.
	p, err := Spawn(Options{
		Argv:   []string{"/bin/sh", "-c", "echo 'Error: Cannot find module dist/main.js' >&2; exit 1"},
		Logger: &recordingLogger{},
	})
	if err != nil {
		t.Fatal(err)
	}
	expectEventsClosed(t, p)
	waitErr := p.Wait()

	// Assert
	if !strings.Contains(p.StderrTail(), "Cannot find module") {
		t.Fatalf("StderrTail = %q, want the child's crash text", p.StderrTail())
	}
	if ExitCode(waitErr) != 1 {
		t.Fatalf("ExitCode = %d, want 1", ExitCode(waitErr))
	}
}

func TestSpawnTeesADirectedStderrIntoTheTail(t *testing.T) {
	// Arrange: a caller that owns stderr itself still gets the tail captured.
	var directed bytes.Buffer

	// Act
	p, err := Spawn(Options{
		Argv:   []string{"/bin/sh", "-c", "printf raw >&2"},
		Logger: &recordingLogger{},
		Stderr: &directed,
	})
	if err != nil {
		t.Fatal(err)
	}
	expectEventsClosed(t, p)
	if err := p.Wait(); err != nil {
		t.Fatal(err)
	}

	// Assert
	if directed.String() != "raw" || p.StderrTail() != "raw" {
		t.Fatalf("directed=%q tail=%q, want both to carry the child's stderr", directed.String(), p.StderrTail())
	}
}

// TestSpawnRetainsEveryStderrByteWhenWaitIsCalledAtOnce is the regression that
// names the defect directly: the daemon reaps the child the instant it exits,
// and the tail must already hold everything it said.
//
// The retention used to be a RACE the daemon lost under load — os/exec's Wait
// closed the pipe it had created, so a reader goroutine that had not yet been
// scheduled found nothing and the tail came back EMPTY. Nothing here waits for
// the reader, which is the point: Wait is the join.
func TestSpawnRetainsEveryStderrByteWhenWaitIsCalledAtOnce(t *testing.T) {
	// Arrange: more than one read's worth of lines, and no drain of any kind
	// between the spawn and the reap.
	const lines = 200
	p, err := Spawn(Options{
		Argv:   []string{"/bin/sh", "-c", "i=0; while [ $i -lt 200 ]; do echo \"line $i\" >&2; i=$((i+1)); done; exit 1"},
		Logger: &recordingLogger{},
	})
	if err != nil {
		t.Fatal(err)
	}

	// Act
	waitErr := p.Wait()

	// Assert
	tail := p.StderrTail()
	if ExitCode(waitErr) != 1 {
		t.Fatalf("ExitCode = %d, want 1", ExitCode(waitErr))
	}
	if !strings.Contains(tail, "line 199") {
		t.Fatalf("StderrTail = %q, want the child's last line", lastBytes(tail))
	}
	if got := strings.Count(tail, "line "); got != lines {
		t.Fatalf("StderrTail carried %d of %d lines: %q", got, lines, lastBytes(tail))
	}
}

// TestSpawnRetainsTheChildsStderrWhenADescendantHoldsTheStream is the other
// half: the reap may not WAIT for a stream the child left behind either.
//
// A descendant that inherited stderr keeps the pipe open indefinitely, so
// draining to EOF would wedge the reap. Everything the child itself wrote is
// already buffered by the time it is reaped, so the drain takes it all and the
// account says the stream is still held rather than pretending it ended.
func TestSpawnRetainsTheChildsStderrWhenADescendantHoldsTheStream(t *testing.T) {
	// Arrange
	logger := &recordingLogger{}
	p, err := Spawn(Options{
		Argv:   []string{"/bin/sh", "-c", "sleep 300 & printf 'Error: Cannot find module\n' >&2; exit 1"},
		Logger: logger,
	})
	if err != nil {
		t.Fatal(err)
	}

	// Act
	waitErr := p.Wait()

	// Assert
	if !strings.Contains(p.StderrTail(), "Cannot find module") {
		t.Fatalf("StderrTail = %q, want the child's own words despite the surviving descendant", p.StderrTail())
	}
	if ExitCode(waitErr) != 1 {
		t.Fatalf("ExitCode = %d, want 1", ExitCode(waitErr))
	}
	assertLogContains(t, logger, "stderr stream STILL HELD at the reap")
	assertLogContains(t, logger, "child reaped", "stderr_outcome="+stderrOutcomeHeldOpen)
}

// lastBytes trims a tail to something a failure message can carry.
func lastBytes(tail string) string {
	if len(tail) <= 256 {
		return tail
	}
	return "..." + tail[len(tail)-256:]
}

func TestExitDescriptionNamesTheSignalThatKilledTheChild(t *testing.T) {
	// Arrange
	cmd := exec.Command("/bin/sh", "-c", "kill -TERM $$")

	// Act
	waitErr := cmd.Run()

	// Assert
	if got := ExitDescription(waitErr); !strings.Contains(got, "killed by terminated") {
		t.Fatalf("ExitDescription = %q, want the signal named", got)
	}
	if got := ExitCode(waitErr); got != -1 {
		t.Fatalf("ExitCode = %d, want -1 for a signalled child", got)
	}
}

func TestExitDescriptionReportsACleanExit(t *testing.T) {
	// Arrange / Act / Assert
	if got := ExitDescription(nil); got != "exit code 0" {
		t.Fatalf("ExitDescription(nil) = %q", got)
	}
}

func TestExitDescriptionSurfacesAWaitFailureThatCarriesNoStatus(t *testing.T) {
	// Arrange
	waitErr := errFakeWait{}

	// Act / Assert: an unreapable child must not be rendered as a clean exit.
	if got := ExitDescription(waitErr); !strings.Contains(got, "wait failed") {
		t.Fatalf("ExitDescription = %q, want the raw wait failure surfaced", got)
	}
}

func TestExitCodeReportsUnknownForAWaitFailureThatCarriesNoStatus(t *testing.T) {
	// Arrange / Act / Assert: an unreapable child must not be reported as a
	// clean exit 0.
	if got := ExitCode(errFakeWait{}); got != -1 {
		t.Fatalf("ExitCode = %d, want -1", got)
	}
}

type errFakeWait struct{}

func (errFakeWait) Error() string { return "waitpid: no child processes" }
