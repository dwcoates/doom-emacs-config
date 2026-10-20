package metrics

import (
	"errors"
	"fmt"
	"io/fs"
	"os"

	"github.com/prometheus/client_golang/prometheus"
)

// DBBytesName is the size of each file of a SQLite database a runtime owns,
// labeled by `db` (which database) and `file` (db, wal, shm).
const DBBytesName = "agentrepl_db_bytes"

// dbFiles is a collector reading a SQLite database's files' sizes when
// scraped.
type dbFiles struct {
	db, path string
	desc     *prometheus.Desc
}

// NewDBFiles answers a collector of the sizes of the SQLite database at
// path and its write-ahead log and shared-memory files, named db. A file
// that does not exist (no WAL yet) is not served; one that cannot be read
// fails the scrape, which the surface records through its OnError.
func NewDBFiles(db, path string) prometheus.Collector {
	return &dbFiles{db: db, path: path, desc: prometheus.NewDesc(DBBytesName,
		"Size of each file of a SQLite database a runtime owns, by db and file.", []string{"db", "file"}, nil)}
}

// Describe implements prometheus.Collector.
func (d *dbFiles) Describe(ch chan<- *prometheus.Desc) { ch <- d.desc }

// Collect implements prometheus.Collector.
func (d *dbFiles) Collect(ch chan<- prometheus.Metric) {
	for _, f := range []struct{ file, suffix string }{{"db", ""}, {"wal", "-wal"}, {"shm", "-shm"}} {
		info, err := os.Stat(d.path + f.suffix)
		switch {
		case errors.Is(err, fs.ErrNotExist):
			continue
		case err != nil:
			ch <- prometheus.NewInvalidMetric(d.desc, fmt.Errorf("stat the %s database's %s file: %w", d.db, f.file, err))
			continue
		}
		ch <- prometheus.MustNewConstMetric(d.desc, prometheus.GaugeValue, float64(info.Size()), d.db, f.file)
	}
}
