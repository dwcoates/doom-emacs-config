package harness

import (
	workspacev1 "agentrepl/proto/workspace/v1"

	"google.golang.org/protobuf/proto"
)

// WatchKind is one of the daemon's server streams, reduced to a name and an
// opener, so an invariant that holds for EVERY stream can be asserted as one
// table rather than once per stream.
//
// It exists for two contract-level invariants that are stated over the whole
// family: the SUBSCRIPTION INVARIANT (a late subscriber's first push is the
// last-published view, and everything after it arrives in order) and
// FLUSH-ON-ACCEPT (the stream's headers reach the client at accept time, before
// any push, even when the daemon has no view to send yet).
type WatchKind struct {
	// Name is the rpc's name, used in failure messages and subtest names.
	Name string
	// PerWorkspace reports whether Open needs a workspace ref.
	PerWorkspace bool
	// Open subscribes and answers the stream, type-erased to proto.Message so
	// the family can be walked in one loop. ws is nil for a daemon-wide
	// stream.
	Open func(d *Daemon, ws *workspacev1.WorkspaceRef) *Stream[proto.Message]
}

// eraseStream retypes a stream's pushes as proto.Message, forwarding every
// push in order and preserving the terminal error and the cancel.
func eraseStream[T proto.Message](s *Stream[T]) *Stream[proto.Message] {
	ch := make(chan proto.Message, 256)
	go func() {
		defer close(ch)
		for v := range s.C {
			ch <- v
		}
	}()
	return &Stream[proto.Message]{C: ch, t: s.t, cancel: s.cancel, errCh: s.errCh, headers: s.headers}
}

// WatchKinds is every view stream the subscription invariant is stated over.
//
// WatchFeed is deliberately absent: it is not a view stream but a TAIL from a
// token minted by OpenFeed, so "the first push is the last-published view" is
// not its contract. WatchLoginTerminal is absent for the same reason — it
// carries pty bytes, not a view.
func WatchKinds() []WatchKind {
	return []WatchKind{
		{
			Name:         "WatchFooter",
			PerWorkspace: true,
			Open: func(d *Daemon, ws *workspacev1.WorkspaceRef) *Stream[proto.Message] {
				return eraseStream(d.WatchFooter(ws))
			},
		},
		{
			Name:         "WatchTopbar",
			PerWorkspace: true,
			Open: func(d *Daemon, ws *workspacev1.WorkspaceRef) *Stream[proto.Message] {
				return eraseStream(d.WatchTopbar(ws))
			},
		},
		{
			Name:         "WatchDaemonHolds",
			PerWorkspace: true,
			Open: func(d *Daemon, ws *workspacev1.WorkspaceRef) *Stream[proto.Message] {
				return eraseStream(d.WatchHolds(ws))
			},
		},
		{
			Name:         "WatchHostWorkspace",
			PerWorkspace: true,
			Open: func(d *Daemon, ws *workspacev1.WorkspaceRef) *Stream[proto.Message] {
				return eraseStream(d.WatchHost(ws))
			},
		},
		{
			Name:         "WatchWebWorkspace",
			PerWorkspace: true,
			Open: func(d *Daemon, ws *workspacev1.WorkspaceRef) *Stream[proto.Message] {
				return eraseStream(d.WatchWeb(ws))
			},
		},
		{
			Name:         "WatchWorkspaceRoster",
			PerWorkspace: false,
			Open: func(d *Daemon, _ *workspacev1.WorkspaceRef) *Stream[proto.Message] {
				return eraseStream(d.WatchRoster())
			},
		},
		{
			Name:         "WatchDaemon",
			PerWorkspace: false,
			Open: func(d *Daemon, _ *workspacev1.WorkspaceRef) *Stream[proto.Message] {
				return eraseStream(d.WatchDaemonStream())
			},
		},
	}
}
