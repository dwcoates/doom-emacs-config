package main

import (
	testrunv1 "agentrepl/proto/testrun/v1"
	"google.golang.org/protobuf/proto"
	"os"
	"path/filepath"
	"reflect"
	"strconv"
	"strings"
	"testing"
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
