package healthcheck

import (
	"bytes"
	"context"
	"errors"
	"io"
	"net"
	"os"
	"strings"
	"testing"
	"time"

	"agentrepl/shim-store/internal/logging"
	"agentrepl/wire"

	corev1 "agentrepl/proto/agentshim/core/v1"
	"google.golang.org/protobuf/proto"
	"google.golang.org/protobuf/types/known/wrapperspb"
)

func TestProbeClassifiesEveryHealthOutcomeAndLogsIt(t *testing.T) {
	requestID := "doctor-123"
	writeFailure := errors.New("write failed")
	readFailure := errors.New("not a protobuf frame")
	deadline := timeoutError{}
	cases := []struct {
		name        string
		config      Config
		deps        deps
		wantExit    int
		wantClass   string
		wantOK      bool
		wantComp    string
		wantReason  string
		checkReason bool
	}{
		{
			name:      "missing socket",
			config:    validConfig(requestID),
			deps:      missingSocketDeps(t),
			wantExit:  ExitMissingSocket,
			wantClass: FailureMissingSocket,
		},
		{
			name:   "connect failure",
			config: validConfig(requestID),
			deps: testDeps(func(context.Context, string) (net.Conn, error) {
				return nil, errors.New("connection refused")
			}),
			wantExit: ExitConnectFailure, wantClass: FailureConnectFailure,
		},
		{
			name:      "socket inspection failure",
			config:    validConfig(requestID),
			deps:      statFailureDeps(errors.New("permission denied")),
			wantExit:  ExitClientFailure,
			wantClass: FailureClientFailure,
		},
		{
			name:   "deadline setup failure",
			config: validConfig(requestID),
			deps: testDeps(func(context.Context, string) (net.Conn, error) {
				return &scriptedConn{deadlineErr: errors.New("deadline unsupported")}, nil
			}),
			wantExit: ExitClientFailure, wantClass: FailureClientFailure,
		},
		{
			name:   "write failure",
			config: validConfig(requestID),
			deps: testDeps(func(context.Context, string) (net.Conn, error) {
				return &scriptedConn{writeErr: writeFailure}, nil
			}),
			wantExit: ExitWriteFailure, wantClass: FailureWriteFailure,
		},
		{
			name:   "timeout",
			config: validConfig(requestID),
			deps: testDeps(func(context.Context, string) (net.Conn, error) {
				return &scriptedConn{readErr: deadline}, nil
			}),
			wantExit: ExitTimeout, wantClass: FailureTimeout,
		},
		{
			name:   "decode failure",
			config: validConfig(requestID),
			deps: testDeps(func(context.Context, string) (net.Conn, error) {
				return &scriptedConn{readErr: readFailure}, nil
			}),
			wantExit: ExitDecodeFailure, wantClass: FailureDecodeFailure,
		},
		{
			name:   "unexpected response type",
			config: validConfig(requestID),
			deps: testDeps(func(context.Context, string) (net.Conn, error) {
				return responseConn(t, wrapperspb.String("not health")), nil
			}),
			wantExit: ExitDecodeFailure, wantClass: FailureDecodeFailure,
		},
		{
			name:   "mismatched request id",
			config: validConfig(requestID),
			deps: testDeps(func(context.Context, string) (net.Conn, error) {
				return responseConn(t, &corev1.HealthStatus{RequestId: "another-request", Healthy: true, Component: "shim-store"}), nil
			}),
			wantExit: ExitMismatchedRequestID, wantClass: FailureMismatchedRequestID, wantComp: "shim-store",
		},
		{
			name:   "unhealthy response",
			config: validConfig(requestID),
			deps: testDeps(func(context.Context, string) (net.Conn, error) {
				return responseConn(t, &corev1.HealthStatus{RequestId: requestID, Healthy: false, Component: "shim-store", Reason: "database is draining"}), nil
			}),
			wantExit: ExitUnhealthyResponse, wantClass: FailureUnhealthyResponse, wantComp: "shim-store", wantReason: "database is draining", checkReason: true,
		},
		{
			name:   "healthy response without component",
			config: validConfig(requestID),
			deps: testDeps(func(context.Context, string) (net.Conn, error) {
				return responseConn(t, &corev1.HealthStatus{RequestId: requestID, Healthy: true}), nil
			}),
			wantExit: ExitUnhealthyResponse, wantClass: FailureUnhealthyResponse,
		},
		{
			name:   "correlated healthy response",
			config: validConfig(requestID),
			deps: testDeps(func(context.Context, string) (net.Conn, error) {
				return responseConn(t, &corev1.HealthStatus{RequestId: requestID, Healthy: true, Component: "shim-store", Reason: "wal checkpoint current"}), nil
			}),
			wantExit: ExitOK, wantClass: "", wantOK: true, wantComp: "shim-store", wantReason: "wal checkpoint current", checkReason: true,
		},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			var file, stderr bytes.Buffer
			log := logging.New(&file, &stderr, false)
			result, gotExit := probe(tc.config, log, tc.deps)
			if gotExit != tc.wantExit || result.FailureClass != tc.wantClass || result.Healthy != tc.wantOK || result.Component != tc.wantComp {
				t.Fatalf("Probe = (%+v, %d), want class=%q healthy=%t component=%q exit=%d", result, gotExit, tc.wantClass, tc.wantOK, tc.wantComp, tc.wantExit)
			}
			if tc.checkReason && result.Reason != tc.wantReason {
				t.Fatalf("Probe reason = %q, want protocol reason %q", result.Reason, tc.wantReason)
			}
			if result.RequestID != requestID || result.LatencyMS != 1 {
				t.Fatalf("Probe metadata = %+v, want request id %q and latency 1ms", result, requestID)
			}
			if !strings.Contains(file.String(), `"operation":"health-check"`) || !strings.Contains(file.String(), `"request_id":"doctor-123"`) || !strings.Contains(file.String(), tc.wantClass) {
				t.Fatalf("canonical health log missing outcome context: %s", file.String())
			}
		})
	}
}

func TestProbeRejectsInvalidInputsBeforeSocketMutation(t *testing.T) {
	cases := []struct {
		name   string
		config Config
	}{
		{name: "missing socket", config: Config{RequestID: "id", Timeout: time.Second}},
		{name: "missing request id", config: Config{SocketPath: "/socket", Timeout: time.Second}},
		{name: "non-positive timeout", config: Config{SocketPath: "/socket", RequestID: "id"}},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			called := false
			d := testDeps(func(context.Context, string) (net.Conn, error) { called = true; return nil, nil })
			var file, stderr bytes.Buffer
			result, exitCode := probe(tc.config, logging.New(&file, &stderr, false), d)
			if exitCode != ExitUsage || result.FailureClass != "usage" || called {
				t.Fatalf("Probe = (%+v, %d), dial=%t; want usage before dialing", result, exitCode, called)
			}
			if !strings.Contains(file.String(), `"operation":"health-check"`) {
				t.Fatalf("invalid input was not canonically logged: %s", file.String())
			}
		})
	}
}

func validConfig(requestID string) Config {
	return Config{SocketPath: "/socket", RequestID: requestID, Timeout: time.Second}
}

func testDeps(dial func(context.Context, string) (net.Conn, error)) deps {
	clock := time.Unix(100, 0)
	return deps{
		stat: func(string) (os.FileInfo, error) { return fakeFileInfo{}, nil },
		dial: dial,
		now: func() time.Time {
			clock = clock.Add(time.Millisecond)
			return clock
		},
	}
}

func missingSocketDeps(t *testing.T) deps {
	t.Helper()
	d := testDeps(func(context.Context, string) (net.Conn, error) {
		t.Fatal("dial called for missing socket")
		return nil, nil
	})
	d.stat = func(string) (os.FileInfo, error) { return nil, os.ErrNotExist }
	return d
}

func statFailureDeps(statErr error) deps {
	d := testDeps(func(context.Context, string) (net.Conn, error) {
		return nil, errors.New("dial must not run after stat failure")
	})
	d.stat = func(string) (os.FileInfo, error) { return nil, statErr }
	return d
}

func responseConn(t *testing.T, response proto.Message) net.Conn {
	t.Helper()
	var read bytes.Buffer
	if err := wire.WriteAny(&read, response); err != nil {
		t.Fatalf("encode response: %v", err)
	}
	return &scriptedConn{read: read}
}

type scriptedConn struct {
	read        bytes.Buffer
	write       bytes.Buffer
	readErr     error
	writeErr    error
	deadlineErr error
}

func (c *scriptedConn) Read(p []byte) (int, error) {
	if c.readErr != nil {
		return 0, c.readErr
	}
	return c.read.Read(p)
}
func (c *scriptedConn) Write(p []byte) (int, error) {
	if c.writeErr != nil {
		return 0, c.writeErr
	}
	return c.write.Write(p)
}
func (c *scriptedConn) Close() error                     { return nil }
func (c *scriptedConn) LocalAddr() net.Addr              { return fakeAddr("local") }
func (c *scriptedConn) RemoteAddr() net.Addr             { return fakeAddr("remote") }
func (c *scriptedConn) SetDeadline(time.Time) error      { return c.deadlineErr }
func (c *scriptedConn) SetReadDeadline(time.Time) error  { return nil }
func (c *scriptedConn) SetWriteDeadline(time.Time) error { return nil }

type fakeAddr string

func (a fakeAddr) Network() string { return "unix" }
func (a fakeAddr) String() string  { return string(a) }

type fakeFileInfo struct{}

func (fakeFileInfo) Name() string       { return "store.sock" }
func (fakeFileInfo) Size() int64        { return 0 }
func (fakeFileInfo) Mode() os.FileMode  { return os.ModeSocket }
func (fakeFileInfo) ModTime() time.Time { return time.Time{} }
func (fakeFileInfo) IsDir() bool        { return false }
func (fakeFileInfo) Sys() any           { return nil }

type timeoutError struct{}

func (timeoutError) Error() string   { return "probe timed out" }
func (timeoutError) Timeout() bool   { return true }
func (timeoutError) Temporary() bool { return true }

var _ net.Error = timeoutError{}
var _ io.Reader = (*scriptedConn)(nil)
