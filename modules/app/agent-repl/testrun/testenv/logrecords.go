package testenv

import (
	"encoding/json"
	"fmt"
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
	return appendJSONLine(path, "log record counts", logRecordsLine{Test: test, Records: records})
}

// ReadLogRecords answers a LogRecordsOut file's counts, summed by test. A
// file no harness wrote answers none; a line that is not a count is an error.
func ReadLogRecords(path string) (map[string]int, error) {
	out := map[string]int{}
	ok, err := readJSONLines(path, "log record counts", func(n int, line []byte) error {
		var l logRecordsLine
		if err := json.Unmarshal(line, &l); err != nil || l.Test == "" {
			return fmt.Errorf("testenv: line %d of %s is no log record count: %q", n, path, line)
		}
		out[l.Test] += l.Records
		return nil
	})
	if err != nil || !ok {
		return nil, err
	}
	return out, nil
}
