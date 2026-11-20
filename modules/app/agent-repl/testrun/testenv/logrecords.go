package testenv

import (
	"bufio"
	"encoding/json"
	"errors"
	"fmt"
	"os"
)

// LogRecordsOut names the file a test run gives each Go unit for its tests'
// log record counts: a harness that reads every record its test's production
// processes wrote anyway (the daemon harness's sweep) appends the count there
// (CountLogRecords), and the runner gives each test the sum of its counts
// (ReadLogRecords). Unset, nothing is counted.
const LogRecordsOut = "AGENT_REPL_TEST_LOG_RECORDS_OUT"

// logRecordsLine is one count in a LogRecordsOut file.
type logRecordsLine struct {
	Test    string `json:"test"`
	Records int    `json:"records"`
}

// CountLogRecords appends to the run's LogRecordsOut file that the test named
// TEST read RECORDS log records of the production processes it started. It
// does nothing outside a run that asked. Each count is one append-mode write,
// so the tests of one process never interleave theirs.
func CountLogRecords(getenv func(string) string, test string, records int) error {
	path := getenv(LogRecordsOut)
	if path == "" {
		return nil
	}
	line, err := json.Marshal(logRecordsLine{Test: test, Records: records})
	if err != nil {
		return fmt.Errorf("testenv: encode %s's log record count: %w", test, err)
	}
	f, err := os.OpenFile(path, os.O_APPEND|os.O_CREATE|os.O_WRONLY, 0o644)
	if err != nil {
		return fmt.Errorf("testenv: open the log record counts %s: %w", path, err)
	}
	if _, err := f.Write(append(line, '\n')); err != nil {
		return errors.Join(fmt.Errorf("testenv: count %s's log records in %s: %w", test, path, err), f.Close())
	}
	if err := f.Close(); err != nil {
		return fmt.Errorf("testenv: close the log record counts %s: %w", path, err)
	}
	return nil
}

// ReadLogRecords answers a LogRecordsOut file's counts, summed by test. A
// file no harness wrote answers none; a line that is not a count is an error.
func ReadLogRecords(path string) (map[string]int, error) {
	f, err := os.Open(path)
	if errors.Is(err, os.ErrNotExist) {
		return nil, nil
	}
	if err != nil {
		return nil, fmt.Errorf("testenv: open the log record counts %s: %w", path, err)
	}
	defer f.Close()
	out := map[string]int{}
	scan := bufio.NewScanner(f)
	for n := 1; scan.Scan(); n++ {
		var l logRecordsLine
		if err := json.Unmarshal(scan.Bytes(), &l); err != nil || l.Test == "" {
			return nil, fmt.Errorf("testenv: line %d of %s is no log record count: %q", n, path, scan.Text())
		}
		out[l.Test] += l.Records
	}
	if err := scan.Err(); err != nil {
		return nil, fmt.Errorf("testenv: read the log record counts %s: %w", path, err)
	}
	return out, nil
}
