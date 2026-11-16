package server

import (
	"bytes"
	"context"
	"net/http"
	"net/http/httptest"
	"path/filepath"
	"slices"
	"syscall"
	"testing"
	"time"

	sharedlogging "agentrepl/logging"
	"agentrepl/logging/recordlog"
	testservicev1 "agentrepl/proto/testservice/v1"
	"agentrepl/proto/testservice/v1/testservicev1connect"
	"agentrepl/testservice/internal/runs"

	"connectrpc.com/connect"
)

// waitBound bounds a watch the tests make: each run here ends within
// milliseconds (measured 2026-11-16: under 30ms), so a watch past it hangs.
const waitBound = 5 * time.Second

type fixture struct {
	client testservicev1connect.TestsClient
	log    *bytes.Buffer
}

func newFixture(t *testing.T) fixture {
	t.Helper()
	var buf bytes.Buffer
	log := recordlog.New("tests", &buf, &bytes.Buffer{}, sharedlogging.FixedWindow(sharedlogging.LevelDebug), time.Now)
	reg, err := runs.Open(t.TempDir(), log, time.Now)
	if err != nil {
		t.Fatal(err)
	}
	srv := httptest.NewServer(New(reg, log).http.Handler)
	t.Cleanup(srv.Close)
	return fixture{client: testservicev1connect.NewTestsClient(http.DefaultClient, srv.URL), log: &buf}
}

func (f fixture) start(t *testing.T, script string) string {
	t.Helper()
	resp, err := f.client.StartRun(context.Background(), connect.NewRequest(&testservicev1.StartRunRequest{
		Dir: t.TempDir(), Argv: []string{"sh", "-c", script}, Label: "ws-1 round 1"}))
	if err != nil || resp.Msg.GetSuccess() == nil {
		t.Fatalf("StartRun = (%v, %v)", resp, err)
	}
	return resp.Msg.GetSuccess().GetRun().GetValue()
}

// watch collects every event of RUN from FROM, bounded by waitBound.
func (f fixture) watch(t *testing.T, run string, from uint64) []*testservicev1.WatchRunResponse {
	t.Helper()
	ctx, cancel := context.WithTimeout(context.Background(), waitBound)
	defer cancel()
	stream, err := f.client.WatchRun(ctx, connect.NewRequest(&testservicev1.WatchRunRequest{Run: &testservicev1.RunId{Value: run}, FromLine: from}))
	if err != nil {
		t.Fatalf("WatchRun: %v", err)
	}
	var got []*testservicev1.WatchRunResponse
	for stream.Receive() {
		got = append(got, stream.Msg())
	}
	if err := stream.Err(); err != nil {
		t.Fatalf("WatchRun stream: %v", err)
	}
	return got
}

func texts(events []*testservicev1.WatchRunResponse) []string {
	var out []string
	for _, e := range events {
		if l := e.GetLine(); l != nil {
			out = append(out, l.GetText())
		}
	}
	return out
}

func TestARunIsStartedAndWatchedToItsExit(t *testing.T) {
	// Arrange
	f := newFixture(t)
	run := f.start(t, "echo one; echo two; exit 4")

	// Act
	got := f.watch(t, run, 0)

	// Assert
	if !slices.Equal(texts(got), []string{"one", "two"}) {
		t.Fatalf("lines = %v, want one and two", texts(got))
	}
	end := got[len(got)-1].GetEnded()
	if end.GetExited().GetExitCode() != 4 || end.GetExited().GetStopped() || end.GetWallMs() < 0 {
		t.Fatalf("end = %v, want exit 4", end)
	}
}

func TestAWatchFromALaterLineStartsThere(t *testing.T) {
	// Arrange
	f := newFixture(t)
	run := f.start(t, "echo one; echo two; echo three")
	_ = f.watch(t, run, 0)

	// Act
	got := f.watch(t, run, 3)

	// Assert
	if !slices.Equal(texts(got), []string{"three"}) || got[0].GetLine().GetNumber() != 3 {
		t.Fatalf("lines = %v, want only line 3", got)
	}
}

func TestAStartMissingAFieldIsRefusedNamingIt(t *testing.T) {
	tests := []struct {
		name  string
		req   *testservicev1.StartRunRequest
		field string
	}{
		{name: "no directory", req: &testservicev1.StartRunRequest{Argv: []string{"true"}, Label: "l"}, field: "dir"},
		{name: "no command", req: &testservicev1.StartRunRequest{Dir: "/", Label: "l"}, field: "argv"},
		{name: "an empty command", req: &testservicev1.StartRunRequest{Dir: "/", Argv: []string{" "}, Label: "l"}, field: "argv"},
		{name: "no label", req: &testservicev1.StartRunRequest{Dir: "/", Argv: []string{"true"}}, field: "label"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			f := newFixture(t)

			// Act
			resp, err := f.client.StartRun(context.Background(), connect.NewRequest(tt.req))

			// Assert
			if err != nil || resp.Msg.GetFailure().GetInvalidRequest().GetField() != tt.field {
				t.Fatalf("StartRun = (%v, %v), want invalid_request naming %s", resp, err, tt.field)
			}
		})
	}
}

func TestAStartWhoseCommandCannotBeSpawnedIsUnstartable(t *testing.T) {
	// Arrange
	f := newFixture(t)

	// Act
	resp, err := f.client.StartRun(context.Background(), connect.NewRequest(&testservicev1.StartRunRequest{
		Dir: t.TempDir(), Argv: []string{filepath.Join(t.TempDir(), "no-such-program")}, Label: "ws-1"}))

	// Assert
	if err != nil || resp.Msg.GetFailure().GetUnstartable() == nil {
		t.Fatalf("StartRun = (%v, %v), want unstartable", resp, err)
	}
}

func TestAWatchOrAStopOfNoRunIsRefused(t *testing.T) {
	tests := []struct {
		name string
		call func(f fixture, t *testing.T) (bool, error)
	}{
		{name: "a watch naming no run", call: func(f fixture, t *testing.T) (bool, error) {
			got := f.watch(t, "", 0)
			return len(got) == 1 && got[0].GetFailure().GetInvalidRequest().GetField() == "run", nil
		}},
		{name: "a watch of an unknown run", call: func(f fixture, t *testing.T) (bool, error) {
			got := f.watch(t, "nope", 0)
			return len(got) == 1 && got[0].GetFailure().GetUnknownRun() != nil, nil
		}},
		{name: "a stop naming no run", call: func(f fixture, t *testing.T) (bool, error) {
			resp, err := f.client.StopRun(context.Background(), connect.NewRequest(&testservicev1.StopRunRequest{}))
			return err == nil && resp.Msg.GetFailure().GetInvalidRequest().GetField() == "run", err
		}},
		{name: "a stop of an unknown run", call: func(f fixture, t *testing.T) (bool, error) {
			resp, err := f.client.StopRun(context.Background(), connect.NewRequest(&testservicev1.StopRunRequest{Run: &testservicev1.RunId{Value: "nope"}}))
			return err == nil && resp.Msg.GetFailure().GetUnknownRun() != nil, err
		}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			f := newFixture(t)

			// Act
			ok, err := tt.call(f, t)

			// Assert
			if err != nil || !ok {
				t.Fatalf("refusal = %v, %v; want the named refusal", ok, err)
			}
		})
	}
}

func TestAStoppedRunEndsStopped(t *testing.T) {
	// Arrange: the run blocks on a gate the test never opens.
	f := newFixture(t)
	gate := filepath.Join(t.TempDir(), "gate")
	if err := syscall.Mkfifo(gate, 0o600); err != nil {
		t.Fatal(err)
	}
	run := f.start(t, "read x < "+gate)

	// Act
	resp, err := f.client.StopRun(context.Background(), connect.NewRequest(&testservicev1.StopRunRequest{Run: &testservicev1.RunId{Value: run}}))

	// Assert
	if err != nil || resp.Msg.GetSuccess() == nil {
		t.Fatalf("StopRun = (%v, %v), want success", resp, err)
	}
	got := f.watch(t, run, 0)
	if end := got[len(got)-1].GetEnded().GetExited(); !end.GetStopped() || end.GetExitCode() != -1 {
		t.Fatalf("end = %v, want a stopped run killed by a signal", got[len(got)-1])
	}
}

func TestALostEndIsSaidAsLost(t *testing.T) {
	// Act
	got := endOf(runs.End{Lost: true, LostDetail: "the service stopped"})

	// Assert
	if got.GetLost().GetDetail() != "the service stopped" || got.GetExited() != nil {
		t.Fatalf("endOf = %v, want a lost end", got)
	}
}
