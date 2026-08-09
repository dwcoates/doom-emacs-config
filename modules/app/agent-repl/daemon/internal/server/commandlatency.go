package server

import (
	"errors"
	"fmt"
	"io"

	"claude-repld/internal/dlog"
	"claude-repld/internal/frontend"
)

// CommandLatencyOperation is the stable operation name every frontend command
// lifecycle-timing record carries. Operators query THIS rather than message
// text; see logging-contract.md.
const CommandLatencyOperation = "daemon.frontend.command-latency"

// CommandOverdueOperation is the stable operation name for a command still IN
// FLIGHT past the client's ack deadline.
//
// It is deliberately NOT CommandLatencyOperation. A completion record and an
// "it has not finished" record answer different questions, and an operator
// counting completions against receipts must not have that count inflated by
// commands that had merely announced themselves. The overdue record exists so a
// wedged bring-up is visible WHILE it is wedged: an open_workspace has been
// observed running for two minutes, and until this record the only evidence was
// other commands reporting a queue_depth with nothing to attribute it to — and
// none at all if the daemon exited before the command finished.
const CommandOverdueOperation = "daemon.frontend.command-overdue"

// targetCommandLatencyRecorder writes one lifecycle-timing record per completed
// frontend command.
//
// Routing follows the contract exactly: a command that names a workspace is
// workspace-owned and lands in that workspace's daemon.log, and a workspace it
// cannot resolve is a loud refusal rather than a demotion to the global sink.
// The genuinely workspace-less commands — daemon health, shutdown, roster
// publication — are the only ones the global service log carries.
type targetCommandLatencyRecorder struct {
	targets  *dlog.TargetManager
	global   *dlog.Logger
	terminal io.Writer
	verbose  bool
}

// NewTargetCommandLatencyRecorder constructs the explicit runtime dependency.
func NewTargetCommandLatencyRecorder(targets *dlog.TargetManager, global *dlog.Logger, terminal io.Writer, verbose bool) (frontend.CommandLatencyRecorder, error) {
	if targets == nil {
		return nil, errors.New("server: command latency recorder needs a target manager")
	}
	if global == nil {
		return nil, errors.New("server: command latency recorder needs the global daemon logger for workspace-less commands")
	}
	if terminal == nil {
		return nil, errors.New("server: command latency recorder needs a terminal")
	}
	return &targetCommandLatencyRecorder{targets: targets, global: global, terminal: terminal, verbose: verbose}, nil
}

// RecordCommandLatency emits the sample at debug/verbose when the ack was
// fast, and at warn/normal when it reached the threshold.
//
// THE WARN IS THE WHOLE POINT. A fast command is high-volume diagnostic detail
// and belongs behind the verbose gate; a slow ack is the thing an operator must
// see WITHOUT having enabled anything in advance, and without waiting for the
// client's own deadline-expiry record to be the first evidence there was a
// problem.
func (r *targetCommandLatencyRecorder) RecordCommandLatency(sample frontend.CommandLatencySample) error {
	event := dlog.Event{
		Runtime:   dlog.RuntimeDaemon,
		Level:     dlog.LevelDebug,
		Operation: CommandLatencyOperation,
		Message:   "frontend command completed",
		RequestID: sample.RequestID,
		Context: map[string]any{
			"command":     sample.Command,
			"client_kind": sample.ClientKind,
			"workspace":   sample.Workspace,
			"queue_depth": sample.QueueDepth,
			// duration_ms is the DELIVERED latency — receipt through the ack's
			// bytes reaching the socket — because that is the interval the
			// client's deadline measures. enqueue_ms is the daemon's own share
			// of it, and the two together separate a slow daemon from a
			// backlogged outbound queue without any further correlation.
			"duration_ms":     sample.Delivery.Milliseconds(),
			"enqueue_ms":      sample.Enqueue.Milliseconds(),
			"delivered":       sample.Delivered,
			"processing_ms":   sample.Processing.Milliseconds(),
			"ack_deadline_ms": frontend.CommandAckDeadline.Milliseconds(),
			"threshold_ms":    sample.Threshold.Milliseconds(),
			"ok":              sample.Ok,
		},
	}
	if sample.Slow() {
		event.Level = dlog.LevelWarn
		event.Message = "frontend command ack exceeded its latency threshold"
	}
	// A SLOW DELIVERY INSIDE A STRUCTURAL WINDOW IS CLASSIFIED, NOT SILENCED.
	//
	// The record keeps every figure it had and stays at normal verbosity, so an
	// operator sees the cost without having enabled anything; what changes is
	// that it says WHY. The alternative to naming the window is either raising
	// the threshold — which blinds the alarm to the same latency outside a boot
	// — or dropping the record, which loses the evidence that the bring-up is
	// expensive. Neither is acceptable, and neither is what this does.
	//
	// It applies only to a plain slow delivery. An ack that never arrived and a
	// command still in flight are different classes, and the two branches below
	// take the level back to warn for them.
	if sample.Slow() && sample.Decision != "" {
		event.Level = dlog.LevelInfo
		event.Message = "frontend command ack drained behind an expected window"
		event.Context["decision"] = sample.Decision
	}
	// An ack that never reached the socket is a command the client will never
	// see answered. It is louder than slow, and it names the reason rather than
	// leaving a fast-looking record behind.
	if sample.DeliveryError != "" {
		event.Level = dlog.LevelWarn
		event.Message = "frontend command ack never reached the client"
		event.Context["delivery_error"] = sample.DeliveryError
	}
	// An overdue sample reports a command that has NOT finished. It carries no
	// verdict and no final processing share, so those two keys are replaced by
	// the one fact it does have — how long the command has been running — and
	// it is always normal/warn: an unfinished command past the client's
	// deadline is exactly what an operator must see without verbose mode.
	if sample.Overdue {
		event.Operation = CommandOverdueOperation
		event.Level = dlog.LevelWarn
		event.Message = "frontend command still in flight past the client ack deadline"
		delete(event.Context, "processing_ms")
		delete(event.Context, "ok")
		delete(event.Context, "duration_ms")
		delete(event.Context, "enqueue_ms")
		event.Context["elapsed_ms"] = sample.Delivery.Milliseconds()
	}
	loud := sample.Slow() || sample.Overdue || sample.DeliveryError != ""
	if sample.Workspace == "" {
		return r.emitGlobal(event, loud)
	}
	workspace, err := dlog.WorkspaceFromDirectory(sample.Workspace)
	if err != nil {
		return fmt.Errorf("server: resolve command latency workspace %q: %w", sample.Workspace, err)
	}
	logger, err := r.targets.OpenWorkspaceLogger(workspace, r.terminal, r.verbose)
	if err != nil {
		return fmt.Errorf("server: open command latency workspace logger for %q: %w", workspace.Directory, err)
	}
	if loud {
		return logger.EmitWorkspaceNormal(workspace, event)
	}
	return logger.EmitWorkspaceVerbose(workspace, event)
}

func (r *targetCommandLatencyRecorder) emitGlobal(event dlog.Event, slow bool) error {
	if slow {
		return r.global.EmitNormal(dlog.GlobalScope(), event)
	}
	return r.global.EmitVerbose(dlog.GlobalScope(), event)
}
