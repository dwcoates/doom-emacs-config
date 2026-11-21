// Package testsclient is the daemon's client of the test service
// (testservice.v1.Tests, on its unix socket), which owns every merge gate's
// run so a daemon deploy never cuts one. The daemon is its only caller.
//
// IT TRANSLATES, IT DOES NOT DECIDE. Every response's failure arm becomes a
// typed error naming the arm, a run the service holds no record of becomes
// ErrUnknownRun, and a transport failure becomes ErrUnavailable; the caller
// decides what each means and records it with the context only it has.
package testsclient

import (
	"context"
	"errors"
	"fmt"
	"time"

	testservicev1 "agentrepl/proto/testservice/v1"
	"agentrepl/proto/testservice/v1/testservicev1connect"

	"connectrpc.com/connect"

	"agent-repld/internal/h2cclient"
)

// EnvSocket overrides the test service socket's default path, so a test
// harness can point the daemon at a private service without editing a
// command line. It is the same variable the service reads.
const EnvSocket = "AGENT_REPL_TESTS_SOCKET"

// ErrUnavailable is a call that never reached the service or never came
// back: the service is down, restarting, or the socket is absent.
var ErrUnavailable = errors.New("testsclient: the test service is unavailable")

// ErrUnknownRun is a run the service holds no record of: started by another
// life of it that pruned the record, or never started.
var ErrUnknownRun = errors.New("testsclient: the test service holds no such run")

// RefusedError is a call the service answered with a failure arm.
type RefusedError struct {
	// RPC is the call refused.
	RPC string
	// Kind is the failure arm's proto name (invalid_request, unstartable, ...).
	Kind string
	// Detail is the service's one-line account.
	Detail string
}

func (e *RefusedError) Error() string {
	return fmt.Sprintf("testsclient: %s refused (%s): %s", e.RPC, e.Kind, e.Detail)
}

// Spec is one run to start: ARGV in DIR, with ENV added, for LABEL.
type Spec struct {
	Dir   string
	Argv  []string
	Env   []string
	Label string
}

// End is how a run ended.
type End struct {
	// ExitCode is the command's exit code, -1 when a signal ended it.
	ExitCode int
	// Stopped reports that a Stop ended it.
	Stopped bool
	// Lost reports that the service stopped while it ran; LostDetail says so.
	Lost       bool
	LostDetail string
	// CPU is the processor time it and every descendant it waited for spent.
	CPU time.Duration
	// Wall is how long it ran.
	Wall time.Duration
}

// Client is the daemon's view of the test service.
type Client interface {
	// Start starts SPEC, answering the run's id.
	Start(ctx context.Context, spec Spec) (string, error)
	// Watch tells ONLINE every line of RUN from line FROM on (one when zero),
	// with its number, then answers the run's end. It returns early with
	// ctx's error when ctx ends; the run goes on.
	Watch(ctx context.Context, run string, from uint64, onLine func(number uint64, text string)) (End, error)
	// Stop kills RUN's process group; a run already ended is left as it
	// ended.
	Stop(ctx context.Context, run string) error
	// Answers makes one call of the service and answers nil once the
	// service answered it: the proof that it serves, which a restart of it
	// awaits (deploy.Restarter).
	Answers(ctx context.Context) error
}

type client struct {
	rpc testservicev1connect.TestsClient
}

// New answers a client of the test service at socketPath. Nothing is dialed
// until the first call.
func New(socketPath string) Client {
	return &client{rpc: testservicev1connect.NewTestsClient(h2cclient.UnixClient(socketPath), "http://tests.uds")}
}

func (c *client) Start(ctx context.Context, spec Spec) (string, error) {
	resp, err := c.rpc.StartRun(ctx, connect.NewRequest(&testservicev1.StartRunRequest{Dir: spec.Dir, Argv: spec.Argv, Env: spec.Env, Label: spec.Label}))
	if err != nil {
		return "", transport(ctx, "StartRun", err)
	}
	if f := resp.Msg.GetFailure(); f != nil {
		return "", &RefusedError{RPC: "StartRun", Kind: startKind(f), Detail: f.GetDetail()}
	}
	id := resp.Msg.GetSuccess().GetRun().GetValue()
	if id == "" {
		return "", &RefusedError{RPC: "StartRun", Kind: "no_run", Detail: "the service answered a start naming no run"}
	}
	return id, nil
}

func (c *client) Watch(ctx context.Context, run string, from uint64, onLine func(uint64, string)) (End, error) {
	stream, err := c.rpc.WatchRun(ctx, connect.NewRequest(&testservicev1.WatchRunRequest{Run: &testservicev1.RunId{Value: run}, FromLine: from}))
	if err != nil {
		return End{}, transport(ctx, "WatchRun", err)
	}
	defer stream.Close()
	for stream.Receive() {
		msg := stream.Msg()
		switch {
		case msg.GetLine() != nil:
			onLine(msg.GetLine().GetNumber(), msg.GetLine().GetText())
		case msg.GetEnded() != nil:
			return endOf(msg.GetEnded()), nil
		case msg.GetFailure() != nil:
			f := msg.GetFailure()
			if f.GetUnknownRun() != nil {
				return End{}, fmt.Errorf("%w: %s", ErrUnknownRun, f.GetDetail())
			}
			return End{}, &RefusedError{RPC: "WatchRun", Kind: watchKind(f), Detail: f.GetDetail()}
		}
	}
	if err := stream.Err(); err != nil {
		return End{}, transport(ctx, "WatchRun", err)
	}
	// A STREAM THAT ENDS WITH NO END is the service going away mid-watch.
	if ctx.Err() != nil {
		return End{}, ctx.Err()
	}
	return End{}, fmt.Errorf("%w: the watch of run %s ended before the run's end", ErrUnavailable, run)
}

func (c *client) Stop(ctx context.Context, run string) error {
	resp, err := c.rpc.StopRun(ctx, connect.NewRequest(&testservicev1.StopRunRequest{Run: &testservicev1.RunId{Value: run}}))
	if err != nil {
		return transport(ctx, "StopRun", err)
	}
	if f := resp.Msg.GetFailure(); f != nil {
		if f.GetUnknownRun() != nil {
			return fmt.Errorf("%w: %s", ErrUnknownRun, f.GetDetail())
		}
		return &RefusedError{RPC: "StopRun", Kind: "invalid_request", Detail: f.GetDetail()}
	}
	return nil
}

// Answers implements Client. The call is a watch naming no run, which the
// service refuses at once as an invalid request and which touches no run:
// its refusal is the answer. A call that never reached the service or never
// came back is ErrUnavailable; any other answer is an error naming it, a
// service that does not keep the protocol.
func (c *client) Answers(ctx context.Context) error {
	stream, err := c.rpc.WatchRun(ctx, connect.NewRequest(&testservicev1.WatchRunRequest{}))
	if err != nil {
		return transport(ctx, "WatchRun", err)
	}
	defer stream.Close()
	if !stream.Receive() {
		if err := stream.Err(); err != nil {
			return transport(ctx, "WatchRun", err)
		}
		if ctx.Err() != nil {
			return ctx.Err()
		}
		return fmt.Errorf("%w: a watch naming no run ended with no answer", ErrUnavailable)
	}
	if stream.Msg().GetFailure().GetInvalidRequest() != nil {
		return nil
	}
	return fmt.Errorf("testsclient: the service answered a watch naming no run with %v, not its refusal", stream.Msg())
}

// transport is a call's transport failure: the caller's own end when ctx
// ended, ErrUnavailable otherwise.
func transport(ctx context.Context, rpc string, err error) error {
	if ctx.Err() != nil {
		return ctx.Err()
	}
	return fmt.Errorf("%w: %s: %v", ErrUnavailable, rpc, err)
}

func endOf(e *testservicev1.RunEnd) End {
	out := End{CPU: time.Duration(e.GetCpuMs()) * time.Millisecond, Wall: time.Duration(e.GetWallMs()) * time.Millisecond}
	if lost := e.GetLost(); lost != nil {
		out.Lost, out.LostDetail, out.ExitCode = true, lost.GetDetail(), -1
		return out
	}
	out.ExitCode, out.Stopped = int(e.GetExited().GetExitCode()), e.GetExited().GetStopped()
	return out
}

func startKind(f *testservicev1.StartRunFailure) string {
	switch {
	case f.GetInvalidRequest() != nil:
		return "invalid_request"
	case f.GetUnstartable() != nil:
		return "unstartable"
	case f.GetStorageFailure() != nil:
		return "storage_failure"
	}
	return "unspecified"
}

func watchKind(f *testservicev1.WatchRunFailure) string {
	switch {
	case f.GetInvalidRequest() != nil:
		return "invalid_request"
	case f.GetStorageFailure() != nil:
		return "storage_failure"
	}
	return "unspecified"
}
