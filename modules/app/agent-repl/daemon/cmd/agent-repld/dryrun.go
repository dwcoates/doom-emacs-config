package main

import (
	"context"
	"errors"
	"fmt"
	"io"
	"net/http"

	"connectrpc.com/connect"
	"google.golang.org/protobuf/encoding/protojson"
	"google.golang.org/protobuf/proto"
)

// A DRY RUN (`-dry-run`, on every verb) SENDS NOTHING THAT CHANGES ANYTHING.
// The verb runs as it would, reading what it needs to resolve its arguments
// (the roster, a workspace's status), and every request that would change
// something is printed instead of sent. What is a read is the contract's to
// say: an rpc the service marks `idempotency_level = NO_SIDE_EFFECTS` goes
// through, and every other one is stopped here, so an rpc added later
// without the mark is never sent by a dry run.

// errDryRun is the answer a dry run gives every request it stopped.
var errDryRun = errors.New("dry run: the request was printed, not sent")

// dryRunInterceptor stops every request that is not a read, printing it to
// OUT as the procedure it names and its protojson.
func dryRunInterceptor(out io.Writer) connect.Interceptor {
	return &dryRun{out: out}
}

type dryRun struct {
	out io.Writer
}

// isRead reports whether SPEC is an rpc the contract marks as changing
// nothing.
func isRead(spec connect.Spec) bool {
	return spec.IdempotencyLevel == connect.IdempotencyNoSideEffects
}

// print writes the request MSG for PROCEDURE as a dry run states it.
func (d *dryRun) print(procedure string, msg any) {
	text := fmt.Sprintf("%v", msg)
	if m, ok := msg.(proto.Message); ok {
		if rendered, err := (protojson.MarshalOptions{Multiline: true}).Marshal(m); err == nil {
			text = string(rendered)
		}
	}
	fmt.Fprintf(d.out, "dry run: would call %s with\n%s\n", procedure, text)
}

func (d *dryRun) WrapUnary(next connect.UnaryFunc) connect.UnaryFunc {
	return func(ctx context.Context, req connect.AnyRequest) (connect.AnyResponse, error) {
		if isRead(req.Spec()) {
			return next(ctx, req)
		}
		d.print(req.Spec().Procedure, req.Any())
		return nil, errDryRun
	}
}

func (d *dryRun) WrapStreamingClient(next connect.StreamingClientFunc) connect.StreamingClientFunc {
	return func(ctx context.Context, spec connect.Spec) connect.StreamingClientConn {
		if isRead(spec) {
			return next(ctx, spec)
		}
		return &stoppedStream{spec: spec, dry: d}
	}
}

func (d *dryRun) WrapStreamingHandler(next connect.StreamingHandlerFunc) connect.StreamingHandlerFunc {
	return next
}

// stoppedStream is a stream a dry run opened in place of one that is not a
// read: the first message is printed, and every operation answers errDryRun.
type stoppedStream struct {
	spec    connect.Spec
	dry     *dryRun
	printed bool
}

func (s *stoppedStream) Spec() connect.Spec          { return s.spec }
func (s *stoppedStream) Peer() connect.Peer          { return connect.Peer{} }
func (s *stoppedStream) RequestHeader() http.Header  { return http.Header{} }
func (s *stoppedStream) ResponseHeader() http.Header { return http.Header{} }
func (s *stoppedStream) ResponseTrailer() http.Header {
	return http.Header{}
}
func (s *stoppedStream) CloseRequest() error  { return nil }
func (s *stoppedStream) CloseResponse() error { return nil }
func (s *stoppedStream) Receive(any) error    { return errDryRun }
func (s *stoppedStream) Send(msg any) error {
	if !s.printed {
		s.printed = true
		s.dry.print(s.spec.Procedure, msg)
	}
	return errDryRun
}
