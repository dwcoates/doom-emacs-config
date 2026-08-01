// Package storehistory reads a conversation's DURABLE history straight from
// the shim-store, for the one case no other route can serve: a workspace with
// NO live session controller and no shim to ask.
//
// # Why this exists beside repull.go, and why it is not the thing repull.go
// # refused to be
//
// repull.go serves a below-floor frontend resync THROUGH THE SHIM, and its
// header records why an earlier direct-to-store package was deleted: dialling
// the store while a shim was DOWN served history through a side door and
// masked the outage, which is a fallback under the no-fallbacks rule.
//
// This package is a different case, and the distinction is the whole
// justification. An UNWIRED workspace is not a shim outage. It is the ordinary
// resting state of every workspace after a daemon bounce, and of every
// hibernated workspace: there is no shim, nothing is broken, and nothing is
// being masked. There is also no second route to compare against — for such a
// workspace the store is the ONLY route, so serving from it cannot be
// "papering over" a route that would otherwise have worked. Spawning a shim to
// answer a read would instead charge a session bring-up (and a vendor process)
// to a frontend that merely mounted.
//
// The store is the AUTHORITY the shim itself reads. The shim's own bounded
// replay (agent-shim/claude/shim/src/uds/store-client.ts `replay`) opens a
// throwaway `Subscribe` against this same socket and streams the same rows;
// this package performs exactly that read with the shim's process removed from
// the middle. Nothing here parses transcripts: the `.jsonl` files the sidecar
// reads carry no store seq, and seq is what every floor, every ConversationDelta
// through_seq, and every frontend replay mark are expressed in.
//
// # Termination
//
// `Subscribe` is replay-then-live-tail and has no end-of-replay marker, so a
// replay ends on one of three bounds, mirroring the shim's own client:
//
//   - toSeq — an EXCLUSIVE upper bound the caller already covers. Structural.
//   - maxEvents — the caller's cap on one replay. A tripped cap is TRUNCATED.
//   - the idle window — no frame within it means the replay drained. For an
//     unwired workspace nothing is producing events, so quiet IS the end of
//     history rather than a guess about a slow producer.
//
// # Errors are loud
//
// An unreachable socket, a refused subscription, or a framing failure is
// returned as an error, never as an empty replay. A frontend that receives
// silence cannot tell "this conversation is empty" from "the daemon could not
// read it", and that ambiguity is the blank-feed bug this whole path exists to
// close.
package storehistory

import (
	"context"
	"errors"
	"fmt"
	"io"
	"net"
	"os"
	"path/filepath"
	"time"

	corev1 "agentrepl/proto/agentshim/core/v1"
	"agentrepl/wire"
)

// DefaultIdle is the quiet window that reports a drained replay. Generous
// against a large SQLite scan's first row, and irrelevant to correctness for an
// unwired workspace, which has no producer that could break the quiet.
const DefaultIdle = 5 * time.Second

// Result reports how one durable replay ended. Truncated is never presented as
// a complete answer: the caller must say so.
type Result struct {
	Delivered uint64
	FirstSeq  uint64
	LastSeq   uint64
	Truncated bool
	Reason    string
}

// VendorResolver maps a daemon session id to the VENDOR session uuid the store
// keys its seq space, dedup index, and fan-out on. Reports ok=false when the
// session has no uuid recorded yet.
//
// The keying rule stays behind this func rather than being spelled inside the
// read loop: subscribing under any other id registers a subscriber on a channel
// nothing publishes to, and the store answers it with a silent empty replay
// (shim-store/AGENTS.md).
type VendorResolver func(sessionID string) (vendorSessionID string, ok bool)

// Reader replays persisted conversation events from the shim-store socket.
type Reader struct {
	// Socket is the store's UDS path. Required.
	Socket string
	// Vendor resolves the store's session key. Required.
	Vendor VendorResolver
	// Idle is the drain window. Zero takes DefaultIdle.
	Idle time.Duration
	// Logf is the daemon logger. Required: this path is the only account of a
	// replay served without a shim, and a silent one would be unreviewable.
	Logf func(string, ...any)
}

// DefaultSocketPath is the launchd store singleton's socket, the same path a
// shim defaults to when the daemon spawns it without --store-socket.
func DefaultSocketPath() (string, error) {
	home, err := os.UserHomeDir()
	if err != nil {
		return "", fmt.Errorf("storehistory: resolving home dir: %w", err)
	}
	return filepath.Join(home, ".cache", "agent-repl", "sock", "store.sock"), nil
}

// ReplayHistory streams the session's persisted events with seq > fromSeq (and,
// when toSeq is non-zero, seq < toSeq) to onEvent in store order.
//
// fromSeq is EXCLUSIVE, matching Subscribe.from_seq and ReplayRequest.from_seq,
// so callers holding an INCLUSIVE first-seq-to-replay convert before calling.
func (r *Reader) ReplayHistory(ctx context.Context, workspace, sessionID string, fromSeq, toSeq uint64, maxEvents uint32, onEvent func(*corev1.Event)) (Result, error) {
	if r.Logf == nil {
		return Result{}, fmt.Errorf("storehistory: replay for ws %q needs a logger", workspace)
	}
	if onEvent == nil {
		return Result{}, fmt.Errorf("storehistory: replay for ws %q needs an event sink", workspace)
	}
	if r.Socket == "" {
		return Result{}, fmt.Errorf("storehistory: replay for ws %q has no store socket configured", workspace)
	}
	if r.Vendor == nil {
		return Result{}, fmt.Errorf("storehistory: replay for ws %q has no vendor session resolver configured", workspace)
	}
	vendor, ok := r.Vendor(sessionID)
	if !ok || vendor == "" {
		return Result{}, fmt.Errorf("storehistory: replay for ws %q session %s has no vendor session uuid recorded, which is the key the store's seq space is under — its history cannot be located", workspace, sessionID)
	}
	idle := r.Idle
	if idle <= 0 {
		idle = DefaultIdle
	}
	started := time.Now()
	r.Logf("storehistory: replaying DURABLE history ws=%q session=%s vendor_session=%s socket=%q from_seq=%d to_seq=%d max_events=%d idle_ms=%d (no shim involved; the store is the authority)",
		workspace, sessionID, vendor, r.Socket, fromSeq, toSeq, maxEvents, idle.Milliseconds())

	conn, err := net.Dial("unix", r.Socket)
	if err != nil {
		r.Logf("storehistory: durable history UNREADABLE ws=%q session=%s vendor_session=%s socket=%q from_seq=%d: dial failed: %v",
			workspace, sessionID, vendor, r.Socket, fromSeq, err)
		return Result{}, fmt.Errorf("storehistory: dialling the store at %q for ws %q: %w", r.Socket, workspace, err)
	}
	defer conn.Close()

	// The context owns the connection's lifetime: a cancelled resync closes the
	// socket, which unblocks a read parked on a store that stopped answering.
	readDone := make(chan struct{})
	defer close(readDone)
	go func() {
		select {
		case <-ctx.Done():
			_ = conn.Close()
		case <-readDone:
		}
	}()

	if err := wire.WriteAny(conn, &corev1.Subscribe{SessionId: vendor, FromSeq: fromSeq}); err != nil {
		r.Logf("storehistory: durable history UNREADABLE ws=%q session=%s vendor_session=%s socket=%q from_seq=%d: subscribe write failed: %v",
			workspace, sessionID, vendor, r.Socket, fromSeq, err)
		return Result{}, fmt.Errorf("storehistory: subscribing to the store for ws %q (vendor session %s): %w", workspace, vendor, err)
	}

	var res Result
	for {
		if maxEvents > 0 && res.Delivered >= uint64(maxEvents) {
			res.Truncated = true
			res.Reason = fmt.Sprintf("event cap %d reached", maxEvents)
			break
		}
		if err := conn.SetReadDeadline(time.Now().Add(idle)); err != nil {
			return res, fmt.Errorf("storehistory: arming the store read deadline for ws %q: %w", workspace, err)
		}
		msg, err := wire.ReadAny(conn)
		if err != nil {
			if ctxErr := ctx.Err(); ctxErr != nil {
				r.Logf("storehistory: durable history CANCELLED ws=%q session=%s vendor_session=%s from_seq=%d delivered=%d: %v",
					workspace, sessionID, vendor, fromSeq, res.Delivered, ctxErr)
				return res, fmt.Errorf("storehistory: replay for ws %q cancelled after %d event(s): %w", workspace, res.Delivered, ctxErr)
			}
			var netErr net.Error
			if errors.As(err, &netErr) && netErr.Timeout() {
				// Quiet for a whole idle window: the replay drained. This is the
				// COMPLETE answer, not a truncation — an unwired workspace has no
				// producer that could still be mid-write.
				break
			}
			if errors.Is(err, io.EOF) {
				// The store closed the subscription. Whatever arrived is real, but
				// it is not provably all of it.
				res.Truncated = true
				res.Reason = "the store closed the subscription"
				break
			}
			r.Logf("storehistory: durable history UNREADABLE ws=%q session=%s vendor_session=%s from_seq=%d delivered=%d: frame read failed: %v",
				workspace, sessionID, vendor, fromSeq, res.Delivered, err)
			return res, fmt.Errorf("storehistory: reading the store's replay for ws %q after %d event(s): %w", workspace, res.Delivered, err)
		}
		ev, isEvent := msg.(*corev1.Event)
		if !isEvent {
			r.Logf("storehistory: skipped a non-event store frame ws=%q session=%s vendor_session=%s type=%T (this path serves persisted conversation history only)",
				workspace, sessionID, vendor, msg)
			continue
		}
		if ev.GetSeq() == 0 {
			// EPHEMERAL: fanned to live subscribers, never persisted, and outside
			// the seq space every floor and replay mark counts in.
			continue
		}
		if toSeq > 0 && ev.GetSeq() >= toSeq {
			break
		}
		if res.Delivered == 0 {
			res.FirstSeq = ev.GetSeq()
		}
		res.Delivered++
		res.LastSeq = ev.GetSeq()
		onEvent(ev)
	}
	if res.Truncated {
		r.Logf("storehistory: durable history TRUNCATED ws=%q session=%s vendor_session=%s from_seq=%d to_seq=%d delivered=%d first_seq=%d last_seq=%d elapsed_ms=%d reason=%q",
			workspace, sessionID, vendor, fromSeq, toSeq, res.Delivered, res.FirstSeq, res.LastSeq, time.Since(started).Milliseconds(), res.Reason)
		return res, nil
	}
	r.Logf("storehistory: durable history COMPLETE ws=%q session=%s vendor_session=%s from_seq=%d to_seq=%d delivered=%d first_seq=%d last_seq=%d elapsed_ms=%d",
		workspace, sessionID, vendor, fromSeq, toSeq, res.Delivered, res.FirstSeq, res.LastSeq, time.Since(started).Milliseconds())
	return res, nil
}
