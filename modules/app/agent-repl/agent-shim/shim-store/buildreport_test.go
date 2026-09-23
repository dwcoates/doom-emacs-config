package main

import (
	"bytes"
	"encoding/json"
	"errors"
	"strings"
	"testing"

	"agentrepl/logging/buildreport"
	"agentrepl/shim-store/internal/logging"
)

// buildReportRecord decodes the fields buildreport's own tests need, which
// storeLogRecord (pprof_test.go) does not carry: the structured context a
// failure logs.
type buildReportRecord struct {
	Operation string         `json:"operation"`
	Level     string         `json:"level"`
	Message   string         `json:"message"`
	Context   map[string]any `json:"context"`
}

func decodeBuildReportRecords(t *testing.T, durable *bytes.Buffer) []buildReportRecord {
	t.Helper()
	var records []buildReportRecord
	for _, line := range strings.Split(strings.TrimSpace(durable.String()), "\n") {
		if line == "" {
			continue
		}
		var record buildReportRecord
		if err := json.Unmarshal([]byte(line), &record); err != nil {
			t.Fatalf("decode %q: %v", line, err)
		}
		records = append(records, record)
	}
	return records
}

func buildReportTestLogger() (*logging.Logger, *bytes.Buffer) {
	var durable bytes.Buffer
	return logging.New(&durable, &bytes.Buffer{}, true), &durable
}

func TestReportBuildWritesTheReportToTheResolvedDirWithThisProcesssPidAndBuild(t *testing.T) {
	// Arrange.
	log, durable := buildReportTestLogger()
	var wroteDir, wroteService string
	var wroteReport buildreport.Report
	deps := buildReportDeps{
		getenv:     func(string) string { return "" },
		resolveDir: func(func(string) string) (string, error) { return "/resolved/dir", nil },
		self:       func() (buildreport.Report, error) { return buildreport.Report{PID: 4242, Build: "abc123"}, nil },
		write: func(dir, service string, r buildreport.Report) error {
			wroteDir, wroteService, wroteReport = dir, service, r
			return nil
		},
	}

	// Act.
	reportBuild(log, deps)

	// Assert.
	if wroteDir != "/resolved/dir" {
		t.Fatalf("wrote to dir %q, want /resolved/dir", wroteDir)
	}
	if wroteService != buildreport.ServiceStore {
		t.Fatalf("wrote service %q, want %q", wroteService, buildreport.ServiceStore)
	}
	if wroteReport.PID != 4242 || wroteReport.Build != "abc123" {
		t.Fatalf("wrote report %#v, want this process's pid and build", wroteReport)
	}
	records := decodeBuildReportRecords(t, durable)
	if len(records) != 1 || records[0].Operation != "store.buildreport" || records[0].Level != "info" {
		t.Fatalf("records = %#v, want one info record naming the build", records)
	}
	if !strings.Contains(records[0].Message, "abc123") {
		t.Fatalf("message %q does not name the build", records[0].Message)
	}
}

func TestReportBuildLogsAnErrorAndContinuesBootingWhenSelfFails(t *testing.T) {
	// Arrange.
	log, durable := buildReportTestLogger()
	writeCalled := false
	deps := buildReportDeps{
		getenv:     func(string) string { return "" },
		resolveDir: func(func(string) string) (string, error) { return "/resolved/dir", nil },
		self: func() (buildreport.Report, error) {
			return buildreport.Report{}, errors.New("executable resolution failed")
		},
		write: func(string, string, buildreport.Report) error {
			writeCalled = true
			return nil
		},
	}

	// Act.
	reportBuild(log, deps)

	// Assert.
	if writeCalled {
		t.Fatalf("write was called after self() failed")
	}
	records := decodeBuildReportRecords(t, durable)
	if len(records) != 1 || records[0].Operation != "store.buildreport.self-failed" || records[0].Level != "error" {
		t.Fatalf("records = %#v, want one error record for the self failure", records)
	}
	if !strings.Contains(records[0].Message, "executable resolution failed") {
		t.Fatalf("message %q does not carry the cause", records[0].Message)
	}
}

func TestReportBuildLogsAnErrorAndContinuesBootingWhenDirResolutionFails(t *testing.T) {
	// Arrange.
	log, durable := buildReportTestLogger()
	writeCalled := false
	deps := buildReportDeps{
		getenv:     func(string) string { return "" },
		resolveDir: func(func(string) string) (string, error) { return "", errors.New("home directory unresolvable") },
		self:       func() (buildreport.Report, error) { return buildreport.Report{PID: 1, Build: "abc"}, nil },
		write: func(string, string, buildreport.Report) error {
			writeCalled = true
			return nil
		},
	}

	// Act.
	reportBuild(log, deps)

	// Assert.
	if writeCalled {
		t.Fatalf("write was called after resolveDir() failed")
	}
	records := decodeBuildReportRecords(t, durable)
	if len(records) != 1 || records[0].Operation != "store.buildreport.dir-failed" || records[0].Level != "error" {
		t.Fatalf("records = %#v, want one error record for the dir failure", records)
	}
	if !strings.Contains(records[0].Message, "home directory unresolvable") {
		t.Fatalf("message %q does not carry the cause", records[0].Message)
	}
}

func TestReportBuildLogsAnErrorAndContinuesBootingWhenWriteFails(t *testing.T) {
	// Arrange.
	log, durable := buildReportTestLogger()
	deps := buildReportDeps{
		getenv:     func(string) string { return "" },
		resolveDir: func(func(string) string) (string, error) { return "/resolved/dir", nil },
		self:       func() (buildreport.Report, error) { return buildreport.Report{PID: 1, Build: "abc"}, nil },
		write:      func(string, string, buildreport.Report) error { return errors.New("disk full") },
	}

	// Act.
	reportBuild(log, deps)

	// Assert.
	records := decodeBuildReportRecords(t, durable)
	if len(records) != 1 || records[0].Operation != "store.buildreport.write-failed" || records[0].Level != "error" {
		t.Fatalf("records = %#v, want one error record for the write failure", records)
	}
	if records[0].Context["path"] != buildreport.Path("/resolved/dir", buildreport.ServiceStore) {
		t.Fatalf("context %#v missing the resolved report path", records[0].Context)
	}
	if !strings.Contains(records[0].Message, "disk full") {
		t.Fatalf("message %q does not carry the cause", records[0].Message)
	}
}

func TestDefaultBuildReportDepsWiresTheRealPackage(t *testing.T) {
	// Arrange / Act.
	deps := defaultBuildReportDeps()

	// Assert: a smoke check that every seam is populated, since a nil seam
	// here would panic in production the first time reportBuild ran rather
	// than at boot's other, more obvious failures.
	if deps.getenv == nil || deps.resolveDir == nil || deps.self == nil || deps.write == nil {
		t.Fatalf("defaultBuildReportDeps left a nil seam: %#v", deps)
	}
}
