package frontend

import (
	"fmt"
	"os"
	"strconv"
	"time"

	frontendv1 "agentrepl/proto/agentshim/frontend/v1"
)

// EnvAckWarnMs is the daemon's one configuration surface for the frontend
// command ack-latency warning threshold, in milliseconds.
const EnvAckWarnMs = "AGENT_REPL_FRONTEND_ACK_WARN_MS"

// CommandAckDeadline is the ack budget the Emacs frontend client enforces. It
// is documented HERE because the warning threshold below is a fraction of it:
// the whole point of the warning is to name a slow ack while the client is
// still waiting, rather than to be discovered afterwards in the client's
// deadline-expiry record.
const CommandAckDeadline = 10 * time.Second

// DefaultAckWarnThreshold is a fifth of the client's ack deadline. A command
// that takes longer than this is already the dominant term in its own round
// trip, and four more like it in front of the next one would blow the budget.
const DefaultAckWarnThreshold = 2 * time.Second

// AckWarnFromEnv resolves the warning threshold from the process environment.
//
// A malformed or non-positive value is an ERROR, never a silent fall back to
// the default: an operator who set AGENT_REPL_FRONTEND_ACK_WARN_MS=0 meant
// something by it, and running the shipped two seconds while they believe they
// changed the knob is the failure a loud refusal exists to prevent.
func AckWarnFromEnv() (time.Duration, error) {
	raw := os.Getenv(EnvAckWarnMs)
	if raw == "" {
		return DefaultAckWarnThreshold, nil
	}
	ms, err := strconv.ParseInt(raw, 10, 64)
	if err != nil {
		return 0, fmt.Errorf("frontend: %s=%q is not an integer number of milliseconds: %w", EnvAckWarnMs, raw, err)
	}
	if ms <= 0 {
		return 0, fmt.Errorf("frontend: %s=%q must be a positive number of milliseconds", EnvAckWarnMs, raw)
	}
	return time.Duration(ms) * time.Millisecond, nil
}

// CommandLatencySample is one completed frontend command's lifecycle timing.
//
// THREE durations, deliberately separate, because they answer three different
// questions and the one that matters used to be missing:
//
//   - Delivery is receipt through the ack's bytes reaching the SOCKET. It is
//     what the client's deadline measures, and the only one of the three that
//     can be compared against it.
//   - Enqueue is receipt through the ack being handed to the outbound queue —
//     the daemon's own share, and everything the old measurement covered.
//   - Processing is the handler's share of Enqueue.
//
// Reading them together names the fault directly. Delivery ≈ Enqueue ≈
// Processing is a slow handler. Delivery ≈ Enqueue with a small Processing is
// queueing on the command's own path. A small Enqueue with a large Delivery is
// the outbound queue: the ack was ready and the writer had a backlog in front
// of it. That last shape is the head-of-line class this sample exists to make
// visible, and it was invisible while the record stopped at the enqueue.
type CommandLatencySample struct {
	// Workspace is the command's workspace directory, empty for the
	// workspace-less commands (daemon health, shutdown, roster publication).
	Workspace string
	// RequestID correlates the sample with the client's own record.
	RequestID string
	// Command is the FrontendCommand oneof field name, e.g. "submit_prompt".
	Command string
	// ClientKind names the frontend product the command arrived from.
	ClientKind string
	// QueueDepth is how many frontend commands this daemon was dispatching at
	// the moment this one was received, INCLUDING this one. One means it had
	// the daemon's command path to itself.
	QueueDepth int64
	// Enqueue is receipt through the ack being handed to the connection's
	// outbound queue: the daemon's own share of the round trip.
	Enqueue time.Duration
	// Delivery is receipt through the ack's bytes reaching the socket: exactly
	// what the client waits out, and what the threshold below judges.
	//
	// On an OVERDUE sample it is how long the command has been waiting so far,
	// which is a lower bound on a delivery that has not happened.
	Delivery time.Duration
	// Delivered reports whether the ack reached the socket at all. False with a
	// DeliveryError set means the client is never going to see this ack.
	Delivered bool
	// DeliveryError names why an ack never reached the socket. Empty when it
	// did, and empty on an overdue sample, whose delivery is merely pending.
	DeliveryError string
	// Processing is the dispatch call's share of Enqueue.
	Processing time.Duration
	// Threshold is the resolved ack-latency warning threshold this sample was
	// judged against, carried so a record is readable without knowing the
	// daemon's environment.
	Threshold time.Duration
	// Ok is the ack's own verdict, so a slow command is distinguishable from a
	// slow refusal.
	Ok bool
	// Overdue marks the sample as an IN-FLIGHT observation rather than a
	// completion: the command passed the client's ack deadline without
	// finishing, and this is the evidence emitted while it is still running.
	//
	// An overdue sample has no verdict and no final processing share yet — Ok
	// is false and Processing is zero because neither exists — and Delivery is
	// how long the command has been running so far. The command's ONE completion
	// sample still follows when it finishes. The recorder routes an overdue
	// sample to its own operation name so a count of completions is never
	// inflated by one.
	Overdue bool
}

// Slow reports whether this sample's ack DELIVERY reached its threshold.
//
// Delivery, not Enqueue, and that is the whole alarm. The warn exists to name a
// slow ack while the client is still waiting on it, and what the client waits
// on is the socket write. Judging the enqueue instead called a command fast
// while its ack sat fifteen seconds deep in a queue — the alarm stayed silent
// through the exact failure it was built for, and the client's own
// deadline-expiry record was the first evidence anything was wrong.
//
// An overdue sample is past the ack deadline by construction, and the deadline
// is a multiple of the threshold, so it is always slow.
func (s CommandLatencySample) Slow() bool {
	return s.Threshold > 0 && s.Delivery >= s.Threshold
}

// CommandLatencyRecorder persists one completed command's lifecycle timing
// through the daemon's canonical logging boundary.
//
// It is an injected dependency rather than a direct dlog call because the
// record is WORKSPACE-OWNED whenever the command names a workspace, and this
// package holds no workspace log targets. An error is returned rather than
// swallowed: a routing failure is an invariant violation the transport logs.
type CommandLatencyRecorder interface {
	RecordCommandLatency(sample CommandLatencySample) error
}

// CommandFieldName is the FrontendCommand oneof's set field name, the stable
// machine-readable command identity every latency record is keyed by.
//
// It is derived from the descriptor rather than from a hand-kept switch, so a
// command arm added to the proto is named correctly without a second edit here
// — and an arm nothing set is reported as "unset" rather than silently blank.
func CommandFieldName(cmd *frontendv1.FrontendCommand) string {
	if cmd == nil {
		return "nil"
	}
	message := cmd.ProtoReflect()
	oneof := message.Descriptor().Oneofs().ByName("command")
	if oneof == nil {
		return "unknown"
	}
	field := message.WhichOneof(oneof)
	if field == nil {
		return "unset"
	}
	return string(field.Name())
}
