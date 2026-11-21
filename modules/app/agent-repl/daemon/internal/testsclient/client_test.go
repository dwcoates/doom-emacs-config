package testsclient

import (
	"context"
	"errors"
	"path/filepath"
	"slices"
	"strings"
	"testing"
	"time"

	testservicev1 "agentrepl/proto/testservice/v1"
	"agentrepl/proto/testservice/v1/testservicev1connect"

	"connectrpc.com/connect"

	"agent-repld/internal/h2cclient/h2ctest"
)

// fakeTests answers each call with what the test set.
type fakeTests struct {
	testservicev1connect.UnimplementedTestsHandler
	start   *testservicev1.StartRunResponse
	watch   []*testservicev1.WatchRunResponse
	stop    *testservicev1.StopRunResponse
	err     error
	started *testservicev1.StartRunRequest
	watched *testservicev1.WatchRunRequest
}

func (f *fakeTests) StartRun(_ context.Context, req *connect.Request[testservicev1.StartRunRequest]) (*connect.Response[testservicev1.StartRunResponse], error) {
	f.started = req.Msg
	if f.err != nil {
		return nil, f.err
	}
	return connect.NewResponse(f.start), nil
}

func (f *fakeTests) WatchRun(_ context.Context, req *connect.Request[testservicev1.WatchRunRequest], stream *connect.ServerStream[testservicev1.WatchRunResponse]) error {
	f.watched = req.Msg
	if f.err != nil {
		return f.err
	}
	for _, m := range f.watch {
		if err := stream.Send(m); err != nil {
			return err
		}
	}
	return nil
}

func (f *fakeTests) StopRun(context.Context, *connect.Request[testservicev1.StopRunRequest]) (*connect.Response[testservicev1.StopRunResponse], error) {
	if f.err != nil {
		return nil, f.err
	}
	return connect.NewResponse(f.stop), nil
}

func serve(t *testing.T, f *fakeTests) Client {
	t.Helper()
	path, handler := testservicev1connect.NewTestsHandler(f)
	return New(h2ctest.ServeUnix(t, path, handler))
}

func line(n uint64, text string) *testservicev1.WatchRunResponse {
	return &testservicev1.WatchRunResponse{Event: &testservicev1.WatchRunResponse_Line{Line: &testservicev1.RunLine{Number: n, Text: text}}}
}

func TestStartSendsTheSpecAndAnswersTheRun(t *testing.T) {
	// Arrange
	f := &fakeTests{start: &testservicev1.StartRunResponse{Result: &testservicev1.StartRunResponse_Success{Success: &testservicev1.StartRunSuccess{Run: &testservicev1.RunId{Value: "r1"}}}}}
	c := serve(t, f)

	// Act
	run, err := c.Start(context.Background(), Spec{Dir: "/d", Argv: []string{"bash", "x"}, Env: []string{"A=1"}, Label: "ws-1"})

	// Assert
	if err != nil || run != "r1" || f.started.GetDir() != "/d" || !slices.Equal(f.started.GetArgv(), []string{"bash", "x"}) || f.started.GetLabel() != "ws-1" {
		t.Fatalf("Start = (%q, %v), sent %v; want r1 and the spec", run, err, f.started)
	}
}

func TestAStartTheServiceRefusesNamesItsArm(t *testing.T) {
	tests := []struct {
		name    string
		failure *testservicev1.StartRunFailure
		kind    string
	}{
		{name: "unstartable", failure: &testservicev1.StartRunFailure{Kind: &testservicev1.StartRunFailure_Unstartable{Unstartable: &testservicev1.StartRunUnstartable{}}}, kind: "unstartable"},
		{name: "invalid", failure: &testservicev1.StartRunFailure{Kind: &testservicev1.StartRunFailure_InvalidRequest{InvalidRequest: &testservicev1.StartRunInvalidRequest{Field: "dir"}}}, kind: "invalid_request"},
		{name: "storage", failure: &testservicev1.StartRunFailure{Kind: &testservicev1.StartRunFailure_StorageFailure{StorageFailure: &testservicev1.StartRunStorageFailure{}}}, kind: "storage_failure"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			c := serve(t, &fakeTests{start: &testservicev1.StartRunResponse{Result: &testservicev1.StartRunResponse_Failure{Failure: tt.failure}}})

			// Act
			_, err := c.Start(context.Background(), Spec{Dir: "/d", Argv: []string{"x"}, Label: "l"})

			// Assert
			var refused *RefusedError
			if !errors.As(err, &refused) || refused.Kind != tt.kind {
				t.Fatalf("Start = %v, want refused %s", err, tt.kind)
			}
		})
	}
}

func TestWatchTellsEveryLineThenAnswersTheEnd(t *testing.T) {
	tests := []struct {
		name string
		end  *testservicev1.RunEnd
		want End
	}{
		{name: "an exit", end: &testservicev1.RunEnd{How: &testservicev1.RunEnd_Exited{Exited: &testservicev1.RunExited{ExitCode: 3}}, CpuMs: 1500, WallMs: 2000},
			want: End{ExitCode: 3, CPU: 1500 * time.Millisecond, Wall: 2 * time.Second}},
		{name: "a stop", end: &testservicev1.RunEnd{How: &testservicev1.RunEnd_Exited{Exited: &testservicev1.RunExited{ExitCode: -1, Stopped: true}}},
			want: End{ExitCode: -1, Stopped: true}},
		{name: "a lost run", end: &testservicev1.RunEnd{How: &testservicev1.RunEnd_Lost{Lost: &testservicev1.RunLost{Detail: "gone"}}},
			want: End{ExitCode: -1, Lost: true, LostDetail: "gone"}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			f := &fakeTests{watch: []*testservicev1.WatchRunResponse{line(4, "four"), line(5, "five"),
				{Event: &testservicev1.WatchRunResponse_Ended{Ended: tt.end}}}}
			c := serve(t, f)
			var got []string

			// Act
			end, err := c.Watch(context.Background(), "r1", 4, func(n uint64, text string) { got = append(got, text) })

			// Assert
			if err != nil || end != tt.want || !slices.Equal(got, []string{"four", "five"}) || f.watched.GetFromLine() != 4 {
				t.Fatalf("Watch = (%+v, %v), lines %v; want %+v and both lines", end, err, got, tt.want)
			}
		})
	}
}

func TestAWatchOfAnUnknownRunIsErrUnknownRun(t *testing.T) {
	// Arrange
	c := serve(t, &fakeTests{watch: []*testservicev1.WatchRunResponse{{Event: &testservicev1.WatchRunResponse_Failure{Failure: &testservicev1.WatchRunFailure{
		Kind: &testservicev1.WatchRunFailure_UnknownRun{UnknownRun: &testservicev1.WatchRunUnknownRun{}}}}}}})

	// Act
	_, err := c.Watch(context.Background(), "r1", 0, func(uint64, string) {})

	// Assert
	if !errors.Is(err, ErrUnknownRun) {
		t.Fatalf("Watch = %v, want ErrUnknownRun", err)
	}
}

func TestAWatchWhoseStreamEndsWithNoEndIsUnavailable(t *testing.T) {
	// Arrange
	c := serve(t, &fakeTests{watch: []*testservicev1.WatchRunResponse{line(1, "one")}})

	// Act
	_, err := c.Watch(context.Background(), "r1", 0, func(uint64, string) {})

	// Assert
	if !errors.Is(err, ErrUnavailable) {
		t.Fatalf("Watch = %v, want ErrUnavailable", err)
	}
}

func TestACallToNoServiceIsUnavailable(t *testing.T) {
	// Arrange
	c := New(filepath.Join(t.TempDir(), "nobody.sock"))

	// Act
	_, err := c.Start(context.Background(), Spec{Dir: "/d", Argv: []string{"x"}, Label: "l"})

	// Assert
	if !errors.Is(err, ErrUnavailable) {
		t.Fatalf("Start = %v, want ErrUnavailable", err)
	}
}

func TestAStopOfAnUnknownRunIsErrUnknownRun(t *testing.T) {
	// Arrange
	c := serve(t, &fakeTests{stop: &testservicev1.StopRunResponse{Result: &testservicev1.StopRunResponse_Failure{Failure: &testservicev1.StopRunFailure{
		Kind: &testservicev1.StopRunFailure_UnknownRun{UnknownRun: &testservicev1.StopRunUnknownRun{}}}}}})

	// Act
	err := c.Stop(context.Background(), "r1")

	// Assert
	if !errors.Is(err, ErrUnknownRun) {
		t.Fatalf("Stop = %v, want ErrUnknownRun", err)
	}
}

func TestAnswers(t *testing.T) {
	refusal := &testservicev1.WatchRunResponse{Event: &testservicev1.WatchRunResponse_Failure{Failure: &testservicev1.WatchRunFailure{
		Kind: &testservicev1.WatchRunFailure_InvalidRequest{InvalidRequest: &testservicev1.WatchRunInvalidRequest{Field: "run"}}}}}
	unknown := &testservicev1.WatchRunResponse{Event: &testservicev1.WatchRunResponse_Failure{Failure: &testservicev1.WatchRunFailure{
		Kind: &testservicev1.WatchRunFailure_UnknownRun{UnknownRun: &testservicev1.WatchRunUnknownRun{}}}}}
	cases := []struct {
		name string
		// client is the client the probe calls through.
		client      func(t *testing.T, f *fakeTests) Client
		watch       []*testservicev1.WatchRunResponse
		unavailable bool
		wantErr     string
	}{
		{name: "the service's refusal of a watch naming no run is its answer", watch: []*testservicev1.WatchRunResponse{refusal}},
		{name: "no service at the socket is unavailable", unavailable: true,
			client: func(t *testing.T, _ *fakeTests) Client { return New(filepath.Join(t.TempDir(), "nobody.sock")) }},
		{name: "a stream that ends with no answer is unavailable", unavailable: true},
		{name: "any answer but the refusal is the protocol broken", watch: []*testservicev1.WatchRunResponse{unknown}, wantErr: "not its refusal"},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			f := &fakeTests{watch: tc.watch}
			c := serve(t, f)
			if tc.client != nil {
				c = tc.client(t, f)
			}

			// Act
			err := c.Answers(context.Background())

			// Assert
			switch {
			case tc.unavailable:
				if !errors.Is(err, ErrUnavailable) {
					t.Fatalf("Answers = %v, want ErrUnavailable", err)
				}
			case tc.wantErr != "":
				if err == nil || errors.Is(err, ErrUnavailable) || !strings.Contains(err.Error(), tc.wantErr) {
					t.Fatalf("Answers = %v, want an error naming %q", err, tc.wantErr)
				}
			case err != nil:
				t.Fatalf("Answers = %v, want the service's answer", err)
			case f.watched.GetRun().GetValue() != "":
				t.Fatalf("watched %v, want a watch naming no run", f.watched)
			}
		})
	}
}
