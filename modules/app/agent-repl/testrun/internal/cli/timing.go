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
const CSVHeader = "run_id,recorded_at_utc,commit,branch,suite,duration_seconds,measure"

// csvFields is how many fields every test_time.csv row has.
const csvFields = 7

// A measure names what a row's duration_seconds means. Rows of different
// measures are different quantities, so the regression report compares a
// suite only with prior rows of the measure it was itself recorded under.
const (
	// MeasureSerialWall is the retired serial bin/test-all.sh's figure: the
	// suite's wall time while it ran ALONE on the host, its go/vitest/Emacs
	// processes free to use every core. Nothing records it any more.
	MeasureSerialWall = "serial-wall"
	// MeasureUnitWallSum is testrun's figure: the sum of the suite's own
	// units' wall times, each unit on one core slot. It excludes the time
	// the suite's units spent waiting for slots other suites held.
	MeasureUnitWallSum = "unit-wall-sum"
)

// RecordedMeasure is the measure a --record run writes.
const RecordedMeasure = MeasureUnitWallSum

// knownMeasure reports whether m is a measure some run has recorded.
func knownMeasure(m string) bool {
	return m == MeasureSerialWall || m == MeasureUnitWallSum
}

// TimingRow is one recorded suite timing.
type TimingRow struct {
	RunID, RecordedAt, Commit, Branch, Suite string
	Seconds                                  float64
	Measure                                  string
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
		if !knownMeasure(r.Measure) {
			return fmt.Errorf("suite %s has an unknown timing measure %q, want %s or %s", r.Suite, r.Measure, MeasureSerialWall, MeasureUnitWallSum)
		}
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
		fmt.Fprintf(&b, "%s,%s,%s,%s,%s,%.3f,%s\n", r.RunID, r.RecordedAt, r.Commit, r.Branch, r.Suite, r.Seconds, r.Measure)
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
// entries of the same branch AND the same measure: rows recorded under
// another measure are a different quantity and never form a baseline. A suite
// needs three priors for a baseline; a big regression is at least one second
// AND at least 25% over the recent mean. It returns the report lines.
//
// A row with an unknown measure, and a run whose rows disagree on their
// measure, are errors: either means the file no longer says what its rows
// measured, and no comparison drawn from it can be trusted.
func Regressions(path, runID, branch string) ([]string, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, fmt.Errorf("read %s: %w", path, err)
	}
	type entry struct {
		measure string
		secs    float64
	}
	var prior []struct {
		suite string
		entry
	}
	current := map[string]float64{}
	measure := ""
	var order []string
	for i, line := range strings.Split(strings.TrimRight(string(data), "\n"), "\n") {
		if i == 0 || line == "" {
			continue
		}
		f := strings.Split(line, ",")
		if len(f) != csvFields {
			return nil, fmt.Errorf("%s line %d has %d fields, want %d", path, i+1, len(f), csvFields)
		}
		secs, err := strconv.ParseFloat(f[5], 64)
		if err != nil {
			return nil, fmt.Errorf("%s line %d: unreadable seconds %q", path, i+1, f[5])
		}
		if !knownMeasure(f[6]) {
			return nil, fmt.Errorf("%s line %d: unknown timing measure %q, want %s or %s", path, i+1, f[6], MeasureSerialWall, MeasureUnitWallSum)
		}
		switch {
		case f[0] == runID:
			if measure != "" && f[6] != measure {
				return nil, fmt.Errorf("%s line %d: run %s recorded both %s and %s rows", path, i+1, runID, measure, f[6])
			}
			measure = f[6]
			if _, seen := current[f[4]]; !seen {
				order = append(order, f[4])
			}
			current[f[4]] = secs
		case f[3] == branch:
			prior = append(prior, struct {
				suite string
				entry
			}{f[4], entry{f[6], secs}})
		}
	}
	same := map[string][]float64{}
	other := map[string]int{}
	for _, p := range prior {
		if p.measure == measure {
			same[p.suite] = append(same[p.suite], p.secs)
		} else {
			other[p.suite]++
		}
	}
	var lines []string
	regressions := 0
	for _, suite := range order {
		p := same[suite]
		if len(p) > 5 {
			p = p[len(p)-5:]
		}
		if len(p) < 3 {
			line := fmt.Sprintf("%s: only %d prior %s %s timing entries, regression baseline needs 3", suite, len(p), branch, measure)
			if n := other[suite]; n > 0 {
				line += fmt.Sprintf(" (%d prior entries of another measure are not comparable)", n)
			}
			lines = append(lines, line)
			continue
		}
		mean := 0.0
		for _, v := range p {
			mean += v
		}
		mean /= float64(len(p))
		delta := current[suite] - mean
		if delta >= 1.0 && current[suite] >= mean*1.25 {
			lines = append(lines, fmt.Sprintf("TIMING REGRESSION: %s %.3fs vs %.3fs recent average %s (+%.1f%%, +%.3fs)",
				suite, current[suite], mean, measure, delta/mean*100, delta))
			regressions++
		}
	}
	if regressions == 0 {
		lines = append(lines, "no big timing regressions detected")
	}
	return lines, nil
}
