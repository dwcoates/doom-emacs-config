package frontend

// roles.go — what a frontend connection IS, decided at accept.
//
// This file used to hold the paint gate as well: a per-workspace delivery
// sequencer that withheld every WorkspaceState from the Emacs tab bar until the
// rendering webview acknowledged having drawn it. That model is gone. A
// workspace's color is CONNECTION TRUTH — blue means no live backend session,
// and every non-blue color guarantees the session substrate is wired — and what
// a viewer has drawn says nothing about that. Every connected frontend now
// receives every emission immediately, in the same order and with the same
// content, so there is no painter/observer partition left to model.
//
// What remains is the CLIENT KIND, which was never about painting: it names the
// frontend product behind a connection, and it is the authority for the
// host-only frames and commands that may reach Emacs alone.

// ClientKind names the frontend product/transport behind a connection.
//
// The endpoint selects it at accept time; clients never self-assert it, so
// there is no window in which a connection exists without one.
type ClientKind int

const (
	ClientKindHost         ClientKind = iota // the Emacs frontend UDS host
	ClientKindGUIBootstrap                   // unscoped /frontend bootstrap WebSocket
	ClientKindGUIStream                      // scoped /sessions/{id}/stream webview
	ClientKindGUIObserver                    // scoped GUI test/auxiliary connection
)

func (k ClientKind) String() string {
	switch k {
	case ClientKindHost:
		return "host"
	case ClientKindGUIBootstrap:
		return "gui_bootstrap"
	case ClientKindGUIStream:
		return "gui_stream"
	case ClientKindGUIObserver:
		return "gui_observer"
	default:
		return "unknown"
	}
}

func (k ClientKind) isHost() bool { return k == ClientKindHost }
