package main

import (
	"context"
	"errors"
	"os"
	"strings"
	"testing"
	"time"

	"connectrpc.com/connect"
	"google.golang.org/protobuf/proto"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
)

func (c *cliClient) ListLogSites(_ context.Context, req *connect.Request[agentreplv1.ListLogSitesRequest]) (*connect.Response[agentreplv1.ListLogSitesResponse], error) {
	if err := c.record(req.Msg); err != nil {
		return nil, err
	}
	if c.logSites != nil {
		return connect.NewResponse(c.logSites), nil
	}
	return connect.NewResponse(&agentreplv1.ListLogSitesResponse{Result: &agentreplv1.ListLogSitesResponse_Success{Success: &agentreplv1.ListLogSitesSuccess{
		Covered: &agentreplv1.ListLogSitesCovered{Extent: &agentreplv1.ListLogSitesCovered_Whole{Whole: &agentreplv1.ListLogSitesCoveredWhole{}}}}}}), nil
}

func TestLogsSitesAsksForTheWindowItsFlagsName(t *testing.T) {
	until := cliNow.Add(-time.Hour).UnixMilli()
	tests := []struct {
		name string
		args []string
		want *agentreplv1.ListLogSitesRequest
	}{
		{
			name: "an age with no end is every level up to now",
			args: []string{"sites", "-since", "90m"},
			want: &agentreplv1.ListLogSitesRequest{Window: &agentreplv1.ListLogSitesWindow{SinceMs: cliNow.Add(-90 * time.Minute).UnixMilli()}},
		},
		{
			name: "an end and a level",
			args: []string{"sites", "-since", "2d", "-until", "1h", "-level", "warn"},
			want: &agentreplv1.ListLogSitesRequest{
				Window:       &agentreplv1.ListLogSitesWindow{SinceMs: cliNow.Add(-48 * time.Hour).UnixMilli(), UntilMs: &until},
				MinimumLevel: &agentreplv1.ListLogSitesMinimumLevel{Level: &agentreplv1.ListLogSitesMinimumLevel_Warn{Warn: &agentreplv1.ClientLogLevelWarn{}}},
			},
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			f := newCLIFixture()

			// Act
			code := f.run(logsNoun, tt.args...)

			// Assert
			if code != exitSuccess {
				t.Fatalf("exit = %d, stderr %q", code, f.errOut.String())
			}
			if got := only[*agentreplv1.ListLogSitesRequest](t, f); !proto.Equal(got, tt.want) {
				t.Fatalf("request = %v, want %v", got, tt.want)
			}
			if !strings.Contains(f.out.String(), `"covered"`) {
				t.Fatalf("stdout = %q, want the answer printed", f.out.String())
			}
		})
	}
}

func TestLogsSitesRefusesFlagsItCannotRead(t *testing.T) {
	tests := []struct {
		name string
		args []string
		want string
	}{
		{name: "no start", args: []string{"sites"}, want: "-since is required"},
		{name: "a start that is no time", args: []string{"sites", "-since", "soon"}, want: "-since:"},
		{name: "an end that is no time", args: []string{"sites", "-since", "1h", "-until", "later"}, want: "-until:"},
		{name: "a level the contract does not have", args: []string{"sites", "-since", "1h", "-level", "fatal"}, want: `-level is debug, info, warn or error, not "fatal"`},
		{name: "a positional", args: []string{"sites", "-since", "1h", "extra"}, want: "nothing is positional"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			f := newCLIFixture()

			// Act
			code := f.run(logsNoun, tt.args...)

			// Assert
			if code != exitFailure || !strings.Contains(f.errOut.String(), tt.want) || len(f.client.requests) != 0 {
				t.Fatalf("exit = %d, stderr %q, requests %v; want a usage failure naming %q and no call", code, f.errOut.String(), f.client.requests, tt.want)
			}
		})
	}
}

func TestLogsSitesSaysAWindowBeforeEveryKeptRow(t *testing.T) {
	// Arrange
	f := newCLIFixture()
	f.client.logSites = &agentreplv1.ListLogSitesResponse{Result: &agentreplv1.ListLogSitesResponse_Error{Error: &agentreplv1.ListLogSitesError{
		Cause: &agentreplv1.ListLogSitesError_UnrecordedWindow{UnrecordedWindow: &agentreplv1.ListLogSitesUnrecordedWindow{OldestSinceMs: 1}},
	}}}

	// Act
	code := f.run(logsNoun, "sites", "-since", "30d")

	// Assert
	if code != exitRefused || !strings.Contains(f.errOut.String(), "the window lies before every log site row the daemon keeps") {
		t.Fatalf("exit = %d, stderr %q; want the refusal said", code, f.errOut.String())
	}
}

func TestLogsSitesReportsAFailedCall(t *testing.T) {
	// Arrange
	f := newCLIFixture()
	f.client.err = errors.New("connection refused")

	// Act
	code := f.run(logsNoun, "sites", "-since", "1h")

	// Assert
	if code != exitFailure || !strings.Contains(f.errOut.String(), "connection refused") {
		t.Fatalf("exit = %d, stderr %q; want the failure reported", code, f.errOut.String())
	}
}

// TestLogsSitesSpellsNoMinimumLevelArm fails a hand-kept mapping of the
// minimum level beside the server's one table (server.LogSitesMinimum).
func TestLogsSitesSpellsNoMinimumLevelArm(t *testing.T) {
	// Arrange
	src, err := os.ReadFile("logscli.go")
	if err != nil {
		t.Fatalf("read logscli.go: %v", err)
	}

	// Act
	got := strings.Count(string(src), "ListLogSitesMinimumLevel_")

	// Assert
	if got != 0 || !strings.Contains(string(src), "server.LogSitesMinimum(") {
		t.Fatalf("logscli.go spells a minimum level arm %d times, want none: it reads server.LogSitesMinimum", got)
	}
}
