package main

import (
	testrunv1 "agentrepl/proto/testrun/v1"
	"bytes"
	"google.golang.org/protobuf/proto"
	"os"
	"path/filepath"
	"reflect"
	"strconv"
	"strings"
	"testing"

	"agentrepl/testrun/internal/gobuild"
	"agentrepl/testrun/internal/run"
)

func TestForwardEvents(t *testing.T) {
	// Arrange: an open file standing in for the invoker's events stream.
	r, w, err := os.Pipe()
	if err != nil {
		t.Fatal(err)
	}
	defer r.Close()
	defer w.Close()
	fd := strconv.Itoa(int(w.Fd()))

	// Act
	argv, extra, err := forwardEvents([]string{"--module", "/m", "--events-fd", fd})

	// Assert
	if err != nil || !reflect.DeepEqual(argv, []string{"--module", "/m", "--events-fd", "4"}) || len(extra) != 1 || extra[0].Fd() != w.Fd() {
		t.Fatalf("forwardEvents = %v, %v, %v; want the runner told fd 4 and the stream forwarded", argv, extra, err)
	}
}

func TestForwardEventsWithoutAStream(t *testing.T) {
	// Act
	argv, extra, err := forwardEvents([]string{"--module", "/m"})

	// Assert
	if err != nil || !reflect.DeepEqual(argv, []string{"--module", "/m"}) || extra != nil {
		t.Fatalf("forwardEvents = %v, %v, %v; want argv unchanged and nothing forwarded", argv, extra, err)
	}
}

func TestForwardEventsRefusesAStandardStream(t *testing.T) {
	// Act
	_, _, err := forwardEvents([]string{"--events-fd", "1"})

	// Assert
	if err == nil || !strings.Contains(err.Error(), "not a file descriptor past stderr") {
		t.Fatalf("forwardEvents = %v, want the standard stream refused", err)
	}
}

func writeRequest(t *testing.T, req proto.Message) string {
	t.Helper()
	data, err := proto.Marshal(req)
	if err != nil {
		t.Fatal(err)
	}
	path := filepath.Join(t.TempDir(), "req.pb")
	if err := os.WriteFile(path, data, 0o644); err != nil {
		t.Fatal(err)
	}
	return path
}

func TestRedundancyRequestArgvLowersTheRequest(t *testing.T) {
	// Arrange
	path := writeRequest(t, &testrunv1.RedundancyRequest{Command: &testrunv1.RedundancyRequest_Report{Report: &testrunv1.RedundancyReport{MeasurementFile: "/m.json"}}})

	// Act
	got, err := redundancyRequestArgv([]string{"--request", path, "--repo", "/r"}, t.TempDir())

	// Assert
	if err != nil || strings.Join(got, " ") != "report -in /m.json" {
		t.Fatalf("redundancyRequestArgv = %q, %v; want the report's flags", got, err)
	}
}

func TestRedundancyRequestArgvRefuses(t *testing.T) {
	report := &testrunv1.RedundancyRequest{Command: &testrunv1.RedundancyRequest_Report{Report: &testrunv1.RedundancyReport{MeasurementFile: "/m.json"}}}
	cases := []struct {
		name string
		argv func(t *testing.T) []string
		want string
	}{
		{name: "no repository", argv: func(t *testing.T) []string { return []string{"--request", writeRequest(t, report)} }, want: "both required"},
		{name: "a flag with no value", argv: func(t *testing.T) []string { return []string{"--request"} }, want: "--request needs a value"},
		{name: "an unknown argument", argv: func(t *testing.T) []string { return []string{"--bogus", "x"} }, want: `unknown argument "--bogus"`},
		{name: "an unreadable request", argv: func(t *testing.T) []string {
			return []string{"--request", filepath.Join(t.TempDir(), "none.pb"), "--repo", "/r"}
		}, want: "read the request"},
		{name: "a request naming no command", argv: func(t *testing.T) []string {
			return []string{"--request", writeRequest(t, &testrunv1.RedundancyRequest{}), "--repo", "/r"}
		}, want: "names no command"},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Act
			_, err := redundancyRequestArgv(tc.argv(t), t.TempDir())

			// Assert
			if err == nil || !strings.Contains(err.Error(), tc.want) {
				t.Fatalf("redundancyRequestArgv = %v, want an error containing %q", err, tc.want)
			}
		})
	}
}

// logBuffers is a run.Log whose lines a test reads back.
func logBuffers() (*run.Log, *bytes.Buffer, *bytes.Buffer) {
	var out, errs bytes.Buffer
	return &run.Log{Out: &out, Err: &errs}, &out, &errs
}

func TestTheModuleBuildCommandsLogTheirFailures(t *testing.T) {
	bin := filepath.Join(t.TempDir(), "_.test")
	if err := os.WriteFile(gobuild.FailurePath(bin), []byte("a.go:1: bad\n"), 0o644); err != nil {
		t.Fatal(err)
	}
	tests := []struct {
		name     string
		cmd      func(*run.Log, []string) int
		argv     []string
		wantCode int
		wantLog  string
	}{
		{"build-tests without its directories", buildTestsCmd, []string{"--", "a"}, 2, "build-tests: gobuild: build-tests needs -module and -bindir"},
		{"stash-test without its binary", stashTestCmd, []string{"-module", "/m", "-bindir", "/b"}, 2, "stash-test: usage: stash-test -module DIR -bindir DIR BINARY"},
		{"stash-test outside the module", stashTestCmd, []string{"-module", t.TempDir(), "-bindir", t.TempDir(), bin}, 1, "is not in the module"},
		{"exec-test without a binary", execTestCmd, nil, 2, "exec-test: usage"},
		{"exec-test of a package that did not compile", execTestCmd, []string{bin, "-test.v"}, 1, "exec-test " + bin + ": the package's tests did not compile: " + gobuild.FailurePath(bin)},
		{"check-test of a package that did not compile", checkTestCmd, []string{bin}, 1, "check-test " + bin + ": the package's tests did not compile"},
		{"check-test without its binary", checkTestCmd, nil, 2, "check-test: usage: check-test BINARY"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			log, _, errs := logBuffers()

			// Act
			code := tt.cmd(log, tt.argv)

			// Assert
			if code != tt.wantCode || !strings.Contains(errs.String(), "ERROR: ") || !strings.Contains(errs.String(), tt.wantLog) {
				t.Fatalf("exit %d logging %q, want exit %d logging %q", code, errs.String(), tt.wantCode, tt.wantLog)
			}
		})
	}
}

func TestCheckTestCmdPassesABuiltBinary(t *testing.T) {
	// Arrange
	bin := filepath.Join(t.TempDir(), "_.test")
	if err := os.WriteFile(bin, []byte("binary"), 0o755); err != nil {
		t.Fatal(err)
	}
	log, _, errs := logBuffers()

	// Act
	code := checkTestCmd(log, []string{bin})

	// Assert
	if code != 0 || errs.Len() != 0 {
		t.Fatalf("exit %d logging %q, want a quiet pass", code, errs.String())
	}
}
