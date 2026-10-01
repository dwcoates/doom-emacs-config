package cli

import (
	"bufio"
	"errors"
	"fmt"
	"io/fs"
	"os"
	"path/filepath"
	"strconv"
	"strings"
	"time"
)

// CSVHeader is test_time.csv's header.
const CSVHeader = "run_id,recorded_at_utc,commit,branch,suite,duration_seconds"

// TimingRow is one recorded suite timing.
type TimingRow struct {
	RunID, RecordedAt, Commit, Branch, Suite string
	Seconds                                  float64
}

// ValidateCSV checks the canonical timing file exists with its header.
func ValidateCSV(path string) error {
	f, err := os.Open(path)
	if err != nil {
		return fmt.Errorf("canonical timing file is missing: %s", path)
	}
	defer f.Close()
	sc := bufio.NewScanner(f)
	if !sc.Scan() {
		return fmt.Errorf("canonical timing file is unreadable: %s", path)
	}
	if sc.Text() != CSVHeader {
		return fmt.Errorf("canonical timing header is invalid: %s", sc.Text())
	}
	return nil
}

func csvSafe(name, value string) error {
	if value == "" {
		return fmt.Errorf("%s is empty", name)
	}
	if strings.ContainsAny(value, ",\n") {
		return fmt.Errorf("%s is not CSV-safe: %s", name, value)
	}
	return nil
}

// AppendTimings appends rows to the CSV atomically: a copy is written beside
// it and renamed over it, under an mkdir lock a concurrent writer fails on.
func AppendTimings(path string, rows []TimingRow) error {
	for _, r := range rows {
		for _, f := range []struct{ name, value string }{
			{"run id", r.RunID}, {"recorded timestamp", r.RecordedAt}, {"commit", r.Commit},
			{"branch", r.Branch}, {"suite", r.Suite},
		} {
			if err := csvSafe(f.name, f.value); err != nil {
				return err
			}
		}
	}
	lock := path + ".lock"
	if err := os.Mkdir(lock, 0o755); err != nil {
		if errors.Is(err, fs.ErrExist) {
			return fmt.Errorf("another timing writer holds %s", lock)
		}
		return fmt.Errorf("lock %s: %w", lock, err)
	}
	defer os.Remove(lock)
	old, err := os.ReadFile(path)
	if err != nil {
		return fmt.Errorf("read %s: %w", path, err)
	}
	var b strings.Builder
	b.Write(old)
	for _, r := range rows {
		fmt.Fprintf(&b, "%s,%s,%s,%s,%s,%.3f\n", r.RunID, r.RecordedAt, r.Commit, r.Branch, r.Suite, r.Seconds)
	}
	tmp, err := os.CreateTemp(filepath.Dir(path), ".test_time.csv.")
	if err != nil {
		return fmt.Errorf("create a temp file beside %s: %w", path, err)
	}
	defer os.Remove(tmp.Name())
	if _, err := tmp.WriteString(b.String()); err != nil {
		tmp.Close()
		return fmt.Errorf("write %s: %w", tmp.Name(), err)
	}
	if err := tmp.Close(); err != nil {
		return fmt.Errorf("close %s: %w", tmp.Name(), err)
	}
	if err := os.Rename(tmp.Name(), path); err != nil {
		return fmt.Errorf("replace %s: %w", path, err)
	}
	return nil
}

// RunID is a run's identifier in the CSV.
func RunID(now time.Time, commit string, pid int) string {
	short := commit
	if len(short) > 12 {
		short = short[:12]
	}
	return fmt.Sprintf("%s-%s-%d", now.UTC().Format("20060102T150405Z"), short, pid)
}

// Regressions compares this run's suites with the five most recent prior
// entries of the same branch. A suite needs three priors for a baseline; a big
// regression is at least one second AND at least 25% over the recent mean.
// It returns the report lines.
func Regressions(path, runID, branch string) ([]string, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, fmt.Errorf("read %s: %w", path, err)
	}
	prior := map[string][]float64{}
	current := map[string]float64{}
	var order []string
	for i, line := range strings.Split(strings.TrimRight(string(data), "\n"), "\n") {
		if i == 0 || line == "" {
			continue
		}
		f := strings.Split(line, ",")
		if len(f) != 6 {
			return nil, fmt.Errorf("%s line %d has %d fields, want 6", path, i+1, len(f))
		}
		secs, err := strconv.ParseFloat(f[5], 64)
		if err != nil {
			return nil, fmt.Errorf("%s line %d: unreadable seconds %q", path, i+1, f[5])
		}
		switch {
		case f[0] == runID:
			if _, seen := current[f[4]]; !seen {
				order = append(order, f[4])
			}
			current[f[4]] = secs
		case f[3] == branch:
			prior[f[4]] = append(prior[f[4]], secs)
		}
	}
	var lines []string
	regressions := 0
	for _, suite := range order {
		p := prior[suite]
		if len(p) > 5 {
			p = p[len(p)-5:]
		}
		if len(p) < 3 {
			lines = append(lines, fmt.Sprintf("%s: only %d prior %s timing entries, regression baseline needs 3", suite, len(p), branch))
			continue
		}
		mean := 0.0
		for _, v := range p {
			mean += v
		}
		mean /= float64(len(p))
		delta := current[suite] - mean
		if delta >= 1.0 && current[suite] >= mean*1.25 {
			lines = append(lines, fmt.Sprintf("TIMING REGRESSION: %s %.3fs vs %.3fs recent average (+%.1f%%, +%.3fs)",
				suite, current[suite], mean, delta/mean*100, delta))
			regressions++
		}
	}
	if regressions == 0 {
		lines = append(lines, "no big timing regressions detected")
	}
	return lines, nil
}
