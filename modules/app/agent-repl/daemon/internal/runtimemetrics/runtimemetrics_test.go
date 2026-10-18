package runtimemetrics

import (
	"io"
	"net/http"
	"os"
	"path/filepath"
	"strings"
	"testing"

	"agent-repld/internal/dlog"

	"agentrepl/logging/metrics"
)

// fakeExe writes a stand-in binary for the build hash.
func fakeExe(t *testing.T) string {
	t.Helper()
	exe := filepath.Join(t.TempDir(), "agent-repld")
	if err := os.WriteFile(exe, []byte("binary bytes"), 0o755); err != nil {
		t.Fatal(err)
	}
	return exe
}

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
	s, err := Open(log, "", fakeExe(t))

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

func TestOpenRefusesAnUnreadableBinary(t *testing.T) {
	// Arrange
	log := dlog.NewTestLogger()
	root := t.TempDir()

	// Act
	s, err := Open(log, root, filepath.Join(root, "missing"))

	// Assert
	if err == nil || !strings.Contains(err.Error(), "hash the daemon binary") {
		t.Fatalf("err = %v, want the hash refusal", err)
	}
	if s != nil {
		t.Fatalf("a refused surface was answered")
	}
	if _, statErr := os.Stat(metrics.TargetPath(root, metrics.ProgramDaemon)); !os.IsNotExist(statErr) {
		t.Fatalf("a refused open left a target behind: %v", statErr)
	}
}

func TestOpenServesBuildInfoAndAdvertises(t *testing.T) {
	// Arrange
	log := dlog.NewTestLogger()
	root := t.TempDir()
	s, err := Open(log, root, fakeExe(t))
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

func TestDeployedVersion(t *testing.T) {
	tests := []struct {
		name        string
		content     *string
		wantSeries  string
		wantLogged  string
		wantNoLines bool
	}{
		{name: "a tagged landing", content: ptr("0.129.0\n"), wantSeries: `agentrepl_deployed_version_info{version="0.129.0"} 1`},
		{name: "a build past the tag, dirty", content: ptr("0.129.0-3-gabc1234-dirty"), wantSeries: `agentrepl_deployed_version_info{version="0.129.0-3-gabc1234-dirty"} 1`},
		{name: "no file yet", content: nil, wantLogged: "read the deployed version", wantNoLines: true},
		{name: "not a describe version", content: ptr("agent-repl-test-checkpoint-1"), wantLogged: "is not a `git describe` version", wantNoLines: true},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			log := dlog.NewTestLogger()
			root := t.TempDir()
			if tt.content != nil {
				if err := os.WriteFile(DeployedVersionPath(root), []byte(*tt.content), 0o644); err != nil {
					t.Fatal(err)
				}
			}
			s, err := Open(log, root, fakeExe(t))
			if err != nil {
				t.Fatalf("Open: %v", err)
			}
			t.Cleanup(s.Close)

			// Act: two scrapes, so a standing failure is shown to log once.
			body := scrape(t, s)
			scrape(t, s)

			// Assert
			if tt.wantSeries != "" && !strings.Contains(body, tt.wantSeries) {
				t.Fatalf("scrape lacks %s:\n%s", tt.wantSeries, body)
			}
			if tt.wantNoLines && strings.Contains(body, DeployedVersionName+"{") {
				t.Fatalf("an unreadable version still produced a series:\n%s", body)
			}
			var errs []dlog.Record
			for _, r := range log.Records() {
				if r.Level == "error" {
					errs = append(errs, r)
				}
			}
			if tt.wantLogged == "" {
				if len(errs) != 0 {
					t.Fatalf("a readable version logged errors: %+v", errs)
				}
				return
			}
			if len(errs) != 1 {
				t.Fatalf("error records = %+v, want exactly one across two scrapes", errs)
			}
			if errs[0].Operation != Operation || errs[0].Context["path"] != DeployedVersionPath(root) ||
				!strings.Contains(errs[0].Context["error"].(string), tt.wantLogged) {
				t.Fatalf("error record = %+v, want the path and %q", errs[0], tt.wantLogged)
			}
		})
	}
}

func TestDeployedVersionRecoveryIsRecorded(t *testing.T) {
	// Arrange
	log := dlog.NewTestLogger()
	root := t.TempDir()
	s, err := Open(log, root, fakeExe(t))
	if err != nil {
		t.Fatalf("Open: %v", err)
	}
	t.Cleanup(s.Close)
	scrape(t, s)
	if err := os.WriteFile(DeployedVersionPath(root), []byte("0.130.0"), 0o644); err != nil {
		t.Fatal(err)
	}

	// Act
	body := scrape(t, s)

	// Assert
	if !strings.Contains(body, `version="0.130.0"`) {
		t.Fatalf("the version written after boot is not served:\n%s", body)
	}
	var recovered bool
	for _, r := range log.Records() {
		if r.Level == "info" && r.Message == "the deployed version is readable again" && r.Context["version"] == "0.130.0" {
			recovered = true
		}
	}
	if !recovered {
		t.Fatalf("no recovery record in %+v", log.Records())
	}
}

func ptr(s string) *string { return &s }
