package runtimemetrics

import (
	"io"
	"net/http"
	"os"
	"strings"
	"testing"

	"agent-repld/internal/dlog"

	"agentrepl/logging/metrics"
)

func scrape(t *testing.T, s *Surface) string {
	t.Helper()
	resp, err := http.Get("http://" + s.srv.Addr() + metrics.Path)
	if err != nil {
		t.Fatalf("scrape: %v", err)
	}
	defer resp.Body.Close()
	body, err := io.ReadAll(resp.Body)
	if err != nil {
		t.Fatalf("read the scrape: %v", err)
	}
	if resp.StatusCode != http.StatusOK {
		t.Fatalf("scrape = %d:\n%s", resp.StatusCode, body)
	}
	return string(body)
}

func TestOpenWithoutRootIsOffAndSaysSo(t *testing.T) {
	// Arrange
	log := dlog.NewTestLogger()

	// Act
	s, err := Open(log, "")

	// Assert
	if err != nil || s != nil {
		t.Fatalf("Open = (%v, %v), want (nil, nil)", s, err)
	}
	if s.Registry() != nil || s.Advertise() != nil {
		t.Fatalf("the off surface must have no registry and advertise nothing")
	}
	s.Close()
	recs := log.Records()
	if len(recs) != 1 || recs[0].Level != "info" || recs[0].Operation != Operation {
		t.Fatalf("records = %+v, want one info record saying the surface is off", recs)
	}
}

func TestOpenServesBuildInfoAndAdvertises(t *testing.T) {
	// Arrange
	log := dlog.NewTestLogger()
	root := t.TempDir()
	s, err := Open(log, root)
	if err != nil {
		t.Fatalf("Open: %v", err)
	}
	t.Cleanup(s.Close)

	// Act
	advertiseErr := s.Advertise()
	body := scrape(t, s)

	// Assert
	if advertiseErr != nil {
		t.Fatalf("Advertise: %v", advertiseErr)
	}
	if !strings.Contains(body, `agentrepl_build_info{build="`) || !strings.Contains(body, `program="daemon"`) {
		t.Fatalf("scrape lacks the daemon's build info:\n%s", body)
	}
	raw, err := os.ReadFile(metrics.TargetPath(root, metrics.ProgramDaemon))
	if err != nil || !strings.Contains(string(raw), s.srv.Addr()) {
		t.Fatalf("target = %q (%v), want it to name %s", raw, err, s.srv.Addr())
	}
	if s.Registry() == nil {
		t.Fatalf("an open surface has no registry")
	}
}

func TestAttachEmacsMeasuresTheEmacsUntilItsRelease(t *testing.T) {
	// Arrange
	s, err := Open(dlog.NewTestLogger(), t.TempDir())
	if err != nil {
		t.Fatalf("Open: %v", err)
	}
	t.Cleanup(s.Close)

	// Act
	release := s.AttachEmacs(4242)
	during, standing := s.emacs.PID()
	release()
	_, after := s.emacs.PID()

	// Assert
	if !standing || during != 4242 || after {
		t.Fatalf("during = %d (%v), after = %v; want 4242 while attached and nothing after", during, standing, after)
	}
}

func TestTheOffSurfaceMeasuresNoEmacs(t *testing.T) {
	// Arrange
	var s *Surface

	// Act / Assert: the release is callable and nothing panics.
	s.AttachEmacs(4242)()
}
