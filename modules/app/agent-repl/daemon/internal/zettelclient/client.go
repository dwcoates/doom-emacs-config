// Package zettelclient is the daemon's client of the zettel service
// (zettel.v1.Zettel, on its unix socket). The daemon is zettel's only caller.
//
// IT TRANSLATES, IT DOES NOT DECIDE. Every response's failure arm becomes a
// typed error naming the arm, and a transport failure becomes ErrUnavailable;
// the caller decides what each means and records it through its own logger,
// with the context only it has (the thread, the turn).
package zettelclient

import (
	"context"
	"errors"
	"fmt"

	zettelv1 "agentrepl/proto/zettel/v1"
	"agentrepl/proto/zettel/v1/zettelv1connect"

	"connectrpc.com/connect"

	"agent-repld/internal/h2cclient"
	"agent-repld/internal/ids"
	"agent-repld/internal/wsm"
)

// EnvSocket overrides the zettel socket's default path, so a test harness can
// point the daemon at a private zettel without editing a command line. It is
// the same variable the zettel service reads.
const EnvSocket = "AGENT_REPL_ZETTEL_SOCKET"

// ErrUnavailable is a call that never reached zettel or never came back: the
// service is down, restarting, or the socket is absent.
var ErrUnavailable = errors.New("zettelclient: the zettel service is unavailable")

// RefusedError is a call zettel answered with a failure arm.
type RefusedError struct {
	// RPC is the call refused.
	RPC string
	// Kind is the failure arm's proto name (storage_failure, unknown_parent, ...).
	Kind string
	// Detail is zettel's one-line account.
	Detail string
}

func (e *RefusedError) Error() string {
	return fmt.Sprintf("zettelclient: %s refused (%s): %s", e.RPC, e.Kind, e.Detail)
}

// Client is the daemon's view of zettel.
type Client interface {
	// RecordExchange records a thread turn's question and final answer as a
	// node, idempotent by turn, answering the node.
	RecordExchange(ctx context.Context, thread ids.WorkspaceID, turn ids.TurnID, question wsm.ThreadQuestion, answer string) (*zettelv1.Node, error)
	// Thread reads one thread's nodes, oldest first.
	Thread(ctx context.Context, thread ids.WorkspaceID) ([]*zettelv1.Node, error)
	// RecordSightings counts one sighting per node, all or nothing.
	RecordSightings(ctx context.Context, nodes []*zettelv1.NodeId) error
}

type client struct {
	rpc zettelv1connect.ZettelClient
}

// New answers a client of the zettel service at socketPath. Nothing is dialed
// until the first call.
func New(socketPath string) Client {
	return &client{rpc: zettelv1connect.NewZettelClient(h2cclient.UnixClient(socketPath), "http://zettel.uds")}
}

// NewWith answers a client over an existing zettel.v1 client (tests).
func NewWith(rpc zettelv1connect.ZettelClient) Client { return &client{rpc: rpc} }

func (c *client) RecordExchange(ctx context.Context, thread ids.WorkspaceID, turn ids.TurnID, question wsm.ThreadQuestion, answer string) (*zettelv1.Node, error) {
	const rpc = "RecordExchange"
	resp, err := c.rpc.RecordExchange(ctx, connect.NewRequest(&zettelv1.RecordExchangeRequest{
		Thread:         &zettelv1.ThreadId{Id: string(thread)},
		Turn:           &zettelv1.ExchangeTurn{TurnId: string(turn)},
		Parent:         question.Parent,
		Question:       &zettelv1.NodeQuestion{Text: question.Text},
		AnswerMarkdown: answer,
	}))
	if err != nil {
		return nil, unavailable(rpc, err)
	}
	switch result := resp.Msg.GetResult().(type) {
	case *zettelv1.RecordExchangeResponse_Success:
		return result.Success.GetNode(), nil
	case *zettelv1.RecordExchangeResponse_Failure:
		return nil, refused(rpc, result.Failure.GetDetail(), result.Failure.GetKind())
	default:
		return nil, unarmed(rpc)
	}
}

func (c *client) Thread(ctx context.Context, thread ids.WorkspaceID) ([]*zettelv1.Node, error) {
	const rpc = "GetThread"
	resp, err := c.rpc.GetThread(ctx, connect.NewRequest(&zettelv1.GetThreadRequest{Thread: &zettelv1.ThreadId{Id: string(thread)}}))
	if err != nil {
		return nil, unavailable(rpc, err)
	}
	switch result := resp.Msg.GetResult().(type) {
	case *zettelv1.GetThreadResponse_Success:
		return result.Success.GetNodes(), nil
	case *zettelv1.GetThreadResponse_Failure:
		return nil, refused(rpc, result.Failure.GetDetail(), result.Failure.GetKind())
	default:
		return nil, unarmed(rpc)
	}
}

func (c *client) RecordSightings(ctx context.Context, nodes []*zettelv1.NodeId) error {
	const rpc = "RecordSightings"
	resp, err := c.rpc.RecordSightings(ctx, connect.NewRequest(&zettelv1.RecordSightingsRequest{Nodes: nodes}))
	if err != nil {
		return unavailable(rpc, err)
	}
	switch result := resp.Msg.GetResult().(type) {
	case *zettelv1.RecordSightingsResponse_Success:
		return nil
	case *zettelv1.RecordSightingsResponse_Failure:
		return refused(rpc, result.Failure.GetDetail(), result.Failure.GetKind())
	default:
		return unarmed(rpc)
	}
}

func unavailable(rpc string, err error) error {
	return fmt.Errorf("%w: %s: %w", ErrUnavailable, rpc, err)
}

// refused names the failure arm set on kind, a oneof wrapper whose concrete
// type is the arm (its Go name ends in the arm's camel-cased proto name).
func refused(rpc, detail string, kind any) error {
	return &RefusedError{RPC: rpc, Kind: armName(kind), Detail: detail}
}

// unarmed is a response with no outcome set: zettel always sets one, so this is
// an invariant violation on the wire, answered as a refusal naming it.
func unarmed(rpc string) error {
	return &RefusedError{RPC: rpc, Kind: "unset", Detail: "the response set neither success nor failure"}
}
