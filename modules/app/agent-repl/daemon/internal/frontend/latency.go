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
// The two durations are deliberately separate. Ack is what the CLIENT waits
// for — receipt of the command through the moment its ack was handed to the
// connection's outbound queue — and Processing is the handler's share of it.
// A large Ack with a small Processing is queueing or a saturated outbox; a
// large Ack that IS its Processing is the handler itself.
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
	// Ack is receipt through ack enqueue: exactly what the client waits out.
	Ack time.Duration
	// Processing is the dispatch call's share of Ack.
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
	// is false and Processing is zero because neither exists — and Ack is how
	// long the command has been running so far. The command's ONE completion
	// sample still follows when it finishes. The recorder routes an overdue
	// sample to its own operation name so a count of completions is never
	// inflated by one.
	Overdue bool
}

// Slow reports whether this sample's ack latency reached its threshold. An
// overdue sample is past the ack deadline by construction, and the deadline is
// a multiple of the threshold, so it is always slow.
func (s CommandLatencySample) Slow() bool {
	return s.Threshold > 0 && s.Ack >= s.Threshold
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
