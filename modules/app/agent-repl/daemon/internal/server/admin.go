package server

import (
	"context"
	"fmt"
	"time"

	"connectrpc.com/connect"
	"google.golang.org/protobuf/proto"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	workspacev1 "agentrepl/proto/workspace/v1"

	"claude-repld/internal/dlog"
	"claude-repld/internal/drain"
	"claude-repld/internal/ids"
	"claude-repld/internal/merge"
	"claude-repld/internal/workspace"
	"claude-repld/internal/wsm"
)

// The daemon-admin verbs: the drain schedule, the merge queue, the two health
// answers and the console-less client's log record.

// UpdateShutdownSchedule is the deploy tooling's drain-and-exit control. A
// schedule is ENCODED before it is durable — the drain reason is stored as its
// encoded form and never as a guessed string — and a Cancel with nothing in
// force answers `nothing_scheduled`.
func (s *server) UpdateShutdownSchedule(
	ctx context.Context,
	req *connect.Request[agentreplv1.UpdateShutdownScheduleRequest],
) (*connect.Response[agentreplv1.UpdateShutdownScheduleResponse], error) {
	const rpc = "UpdateShutdownSchedule"
	if err := validateUpdateShutdownScheduleRequest(req.Msg); err != nil {
		return nil, err
	}
	resp := &agentreplv1.UpdateShutdownScheduleResponse{}

	var err error
	switch {
	case req.Msg.GetSchedule() != nil:
		schedule := req.Msg.GetSchedule()
		encoded, encodeErr := drain.EncodeReason(schedule.GetReason())
		if encodeErr != nil {
			return nil, fail(s.log, rpc, encodeErr)
		}
		err = s.deps.Drain.Schedule(ctx, wsm.DrainSchedule{
			Reason:   encoded,
			Deadline: time.UnixMilli(schedule.GetAtMs()),
			SetAt:    time.Now(),
		})
	case req.Msg.GetCancel() != nil:
		err = s.deps.Drain.Cancel(ctx)
	default:
		err = s.deps.Drain.ShutdownNow(ctx, req.Msg.GetNow().GetReason())
	}
	if err != nil {
		return answer(resp, s.answerRefusal(s.log, rpc, resp, err, nil))
	}
	s.log.Info("daemon.server.update_shutdown_schedule", "applied the shutdown schedule", nil)
	resp.Result = &agentreplv1.UpdateShutdownScheduleResponse_Success{
		Success: &agentreplv1.UpdateShutdownScheduleSuccess{},
	}
	return connect.NewResponse(resp), nil
}

// UpdateMergeQueue is the operator's control of the merge queue: pause or
// resume (daemon-wide when the repository ref is UNSET), or evict one
// workspace's queued merge.
func (s *server) UpdateMergeQueue(
	ctx context.Context,
	req *connect.Request[agentreplv1.UpdateMergeQueueRequest],
) (*connect.Response[agentreplv1.UpdateMergeQueueResponse], error) {
	const rpc = "UpdateMergeQueue"
	if err := validateUpdateMergeQueueRequest(req.Msg); err != nil {
		return nil, err
	}
	resp := &agentreplv1.UpdateMergeQueueResponse{}

	if evict := req.Msg.GetEvict(); evict != nil {
		subject, cerr, done := s.subjectFor(ctx, rpc, evict.GetWorkspace(), resp)
		if done {
			return answer(resp, cerr)
		}
		if err := s.deps.Merge.Evict(ctx, subject.Record.ID); err != nil {
			return answer(resp, s.answerRefusal(subject.Log, rpc, resp, err, nil))
		}
		resp.Result = &agentreplv1.UpdateMergeQueueResponse_Success{
			Success: &agentreplv1.UpdateMergeQueueSuccess{},
		}
		return connect.NewResponse(resp), nil
	}

	var scope *merge.RepositoryScope
	var err error
	if pause := req.Msg.GetPause(); pause != nil {
		scope = scopeOf(pause.GetRepository())
		err = s.deps.Merge.Pause(ctx, scope)
	} else {
		scope = scopeOf(req.Msg.GetResume().GetRepository())
		err = s.deps.Merge.Unpause(ctx, scope)
	}
	if err != nil {
		return answer(resp, s.answerRefusal(s.log, rpc, resp, err, nil))
	}
	s.log.Info("daemon.server.update_merge_queue", "applied the merge-queue action",
		dlog.Context{"scoped": scope != nil})
	resp.Result = &agentreplv1.UpdateMergeQueueResponse_Success{
		Success: &agentreplv1.UpdateMergeQueueSuccess{},
	}
	return connect.NewResponse(resp), nil
}

// scopeOf renders an optional RepositoryRef as the orchestrator's scope. An
// UNSET ref is every repository, which is the daemon-wide switch.
func scopeOf(ref *workspacev1.RepositoryRef) *merge.RepositoryScope {
	if ref == nil {
		return nil
	}
	return &merge.RepositoryScope{ID: ids.RepoID(ref.GetId()), Dir: ref.GetDir()}
}

// DaemonHealth answers whether the daemon itself is healthy. UNHEALTHY IS AN
// ANSWER: it is never a transport error.
func (s *server) DaemonHealth(
	ctx context.Context,
	_ *connect.Request[agentreplv1.DaemonHealthRequest],
) (*connect.Response[agentreplv1.DaemonHealthResponse], error) {
	const rpc = "DaemonHealth"
	report, err := s.deps.Health.Daemon(ctx)
	if err != nil {
		return nil, fail(s.log, rpc, err)
	}
	s.log.Debug("daemon.server.daemon_health", "answered the daemon's health", nil)
	return connect.NewResponse(report), nil
}

// SessionHealth answers whether one workspace's session is healthy. Unhealthy
// is an answer here too.
func (s *server) SessionHealth(
	ctx context.Context,
	req *connect.Request[agentreplv1.SessionHealthRequest],
) (*connect.Response[agentreplv1.SessionHealthResponse], error) {
	const rpc = "SessionHealth"
	resp := &agentreplv1.SessionHealthResponse{}
	subject, cerr, done := s.subjectFor(ctx, rpc, req.Msg.GetWorkspace(), resp)
	if done {
		return answer(resp, cerr)
	}
	report, err := s.deps.Health.Session(ctx, subject.Record.ID)
	if err != nil {
		return answer(resp, s.answerRefusal(subject.Log, rpc, resp, err, nil))
	}
	subject.Log.Debug("daemon.server.session_health", "answered the session's health", nil)
	return connect.NewResponse(report), nil
}

// ClientLog persists a console-less client's diagnostic record into the OWNING
// WORKSPACE's durable log. A record whose workspace cannot be resolved is an
// invariant violation, never a global write.
func (s *server) ClientLog(
	ctx context.Context,
	req *connect.Request[agentreplv1.ClientLogRequest],
) (*connect.Response[agentreplv1.ClientLogResponse], error) {
	const rpc = "ClientLog"
	if err := validateClientLogRequest(req.Msg); err != nil {
		return nil, err
	}
	resp := &agentreplv1.ClientLogResponse{}
	subject, cerr, done := s.subjectForClientLog(ctx, rpc, req.Msg.GetWorkspace(), resp)
	if done {
		return answer(resp, cerr)
	}
	record := req.Msg.GetRecord()
	clientKind, err := clientRuntime(record)
	if err != nil {
		return nil, fail(subject.Log, rpc, err)
	}
	if err := s.deps.Log.ClientLog(subject.Record.Dir, dlog.ClientRecord{
		ClientKind: clientKind,
		Level:      clientLevel(record),
		Operation:  record.GetOperation(),
		Message:    record.GetMessage(),
		Context:    dlog.Context(record.GetContext().AsMap()),
		Timestamp:  record.GetTimestamp(),
		Verbose:    record.GetVerbose(),
	}); err != nil {
		return nil, fail(subject.Log, rpc, fmt.Errorf("persist a client record: %w", err))
	}
	subject.Log.Debug("daemon.server.client_log", "persisted a forwarded client record", dlog.Context{
		"client_kind": clientKind,
		"operation":   record.GetOperation(),
		"verbose":     record.GetVerbose(),
	})
	resp.Result = &agentreplv1.ClientLogResponse_Success{Success: &agentreplv1.ClientLogSuccess{}}
	return connect.NewResponse(resp), nil
}

// subjectForClientLog is subjectFor for ClientLog, whose unknown-workspace
// refusal is ORDINARY TRAFFIC rather than a fault.
//
// A LATE LOG LINE AFTER A CLOSE IS EXPECTED. A forwarder learns its workspace
// is gone only by being told, and records it already wrote keep arriving for
// the seconds it takes that to happen (measured, realtest 8, 2026-09-12: a
// forgotten scratch repo drew a ClientLog eighteen seconds later). So the
// refusal is recorded at INFO — the record still names the workspace — and the
// forwarder reads `ClientLogError.unknown_workspace` as its cue to stop.
func (s *server) subjectForClientLog(
	ctx context.Context,
	rpc string,
	ref *workspacev1.WorkspaceRef,
	resp proto.Message,
) (resolved, *connect.Error, bool) {
	if err := validateWorkspaceRef("workspace", ref); err != nil {
		return resolved{}, err, true
	}
	subject, r, err := s.resolveRef(ctx, rpc, ref)
	if err != nil {
		return resolved{}, fail(s.log, rpc, err), true
	}
	if r != nil {
		if r.Arm == workspace.ArmUnknownWorkspace {
			r.Info = true
		}
		return resolved{}, s.refuse(s.log, rpc, resp, *r), true
	}
	return subject, nil, false
}

// clientRuntime maps the record's runtime arm onto the daemon-owned client
// sink. UNSET intentionally means webapp: the browser was the historical
// forwarder before the runtime oneof existed, so old senders retain their
// declared routing while every set arm is handled explicitly.
func clientRuntime(record *agentreplv1.ClientLogRecord) (string, error) {
	switch record.GetRuntime().(type) {
	case nil, *agentreplv1.ClientLogRecord_Webapp:
		return dlog.RuntimeWebapp, nil
	case *agentreplv1.ClientLogRecord_Sidecar:
		return dlog.RuntimeSidecar, nil
	default:
		return "", fmt.Errorf("client log record carries an unsupported runtime arm %T", record.GetRuntime())
	}
}

// clientLevel renders the record's level arm as the log level's name.
func clientLevel(record *agentreplv1.ClientLogRecord) string {
	switch {
	case record.GetDebug() != nil:
		return dlog.LevelDebug
	case record.GetInfo() != nil:
		return dlog.LevelInfo
	case record.GetWarn() != nil:
		return dlog.LevelWarn
	default:
		return dlog.LevelError
	}
}
