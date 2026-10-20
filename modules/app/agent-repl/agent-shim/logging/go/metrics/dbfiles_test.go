package metrics

import (
	"os"
	"path/filepath"
	"strings"
	"testing"

	"github.com/prometheus/client_golang/prometheus"
)

func TestDBFilesServesEachFileThatExists(t *testing.T) {
	// Arrange
	dir := t.TempDir()
	path := filepath.Join(dir, "events.db")
	if err := os.WriteFile(path, make([]byte, 4096), 0o644); err != nil {
		t.Fatal(err)
	}
	if err := os.WriteFile(path+"-wal", make([]byte, 100), 0o644); err != nil {
		t.Fatal(err)
	}
	reg := prometheus.NewRegistry()
	reg.MustRegister(NewDBFiles("events", path))

	// Act
	families, err := reg.Gather()

	// Assert: no shm series, since there is no shm file.
	if err != nil {
		t.Fatalf("Gather: %v", err)
	}
	got := map[string]float64{}
	for _, f := range families {
		for _, m := range f.GetMetric() {
			labels := map[string]string{}
			for _, l := range m.GetLabel() {
				labels[l.GetName()] = l.GetValue()
			}
			if labels["db"] != "events" {
				t.Fatalf("db label = %q, want events", labels["db"])
			}
			got[labels["file"]] = m.GetGauge().GetValue()
		}
	}
	if len(got) != 2 || got["db"] != 4096 || got["wal"] != 100 {
		t.Fatalf("files = %v, want db 4096 and wal 100 only", got)
	}
}

func TestDBFilesFailsTheScrapeOnAFileItCannotRead(t *testing.T) {
	// Arrange: a path through a regular file cannot be stat'ed as missing.
	dir := t.TempDir()
	notADir := filepath.Join(dir, "file")
	if err := os.WriteFile(notADir, nil, 0o644); err != nil {
		t.Fatal(err)
	}
	reg := prometheus.NewRegistry()
	reg.MustRegister(NewDBFiles("wsm", filepath.Join(notADir, "wsm.db")))

	// Act
	_, err := reg.Gather()

	// Assert
	if err == nil || !strings.Contains(err.Error(), "stat the wsm database's db file") {
		t.Fatalf("Gather = %v, want the unreadable file named", err)
	}
}
