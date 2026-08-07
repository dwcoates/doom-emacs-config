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
			"command":         sample.Command,
			"client_kind":     sample.ClientKind,
			"workspace":       sample.Workspace,
			"queue_depth":     sample.QueueDepth,
			"duration_ms":     sample.Ack.Milliseconds(),
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
	if sample.Workspace == "" {
		return r.emitGlobal(event, sample.Slow())
	}
	workspace, err := dlog.WorkspaceFromDirectory(sample.Workspace)
	if err != nil {
		return fmt.Errorf("server: resolve command latency workspace %q: %w", sample.Workspace, err)
	}
	logger, err := r.targets.OpenWorkspaceLogger(workspace, r.terminal, r.verbose)
	if err != nil {
		return fmt.Errorf("server: open command latency workspace logger for %q: %w", workspace.Directory, err)
	}
	if sample.Slow() {
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
