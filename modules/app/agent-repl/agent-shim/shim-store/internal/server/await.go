package server

import (
	"context"
	"errors"
	"fmt"

	"connectrpc.com/connect"

	conversationv1 "agentrepl/proto/conversation/v1"
	storev1 "agentrepl/proto/store/v1"
	"agentrepl/proto/store/v1/storev1connect"
	"agentrepl/shim-store/internal/logging"
)

// errStoreStopping ends a wait the store's shutdown cut short.
var errStoreStopping = errors.New("the store is shutting down")

// AwaitAgentByVendorTask answers which agent of the caller's lineage a vendor
// task locator names, with its commission, once the record holds both. It
// re-reads the record after every committed write batch and never otherwise,
// so the order of the pairing's write and this call cannot matter. The
// caller's deadline bounds it; the store's shutdown ends it UNAVAILABLE.
func (s *Server) AwaitAgentByVendorTask(ctx context.Context, req *connect.Request[storev1.AwaitAgentByVendorTaskRequest]) (*connect.Response[storev1.AwaitAgentByVendorTaskResponse], error) {
	session := req.Msg.GetSession().GetValue()
	task := req.Msg.GetVendorTaskId()
	fields := logging.Fields{AgentID: session, TaskID: task}
	log := s.rpcLogger(storev1connect.ShimStoreAwaitAgentByVendorTaskProcedure, req.Header()).With(fields)
	const op = "store.rpc.await-agent-by-vendor-task"

	if ref := validateAwaitAgentByVendorTaskRequest(req.Msg); ref != nil {
		logRefusal(log, op, ref, logging.Fields{})
		return awaitAgentByVendorTaskFailure(ref), nil
	}
	rctx := correlated(ctx, req.Header())
	for reads := 1; ; reads++ {
		// TAKEN BEFORE THE READ (commitSignal): a commit after the read wakes it.
		committed := s.commits.next()
		rec, err := s.recordedAgentAndCommission(rctx, session, task)
		if err != nil {
			ref := storeFailure(log, op, err, fields)
			return awaitAgentByVendorTaskFailure(ref), nil
		}
		agent, commission := rec.agent, rec.commission
		if rec.found && commission != nil {
			log.Log(logging.Fields{Operation: op}, "the record holds agent {agent} and its commission after {reads} read(s)", logging.Values{"agent": agent, "reads": reads})
			return connect.NewResponse(&storev1.AwaitAgentByVendorTaskResponse{
				Result: &storev1.AwaitAgentByVendorTaskResponse_Success{Success: &storev1.AwaitAgentByVendorTaskSuccess{
					Agent: &conversationv1.AgentId{Value: agent}, Commission: commission,
				}},
			}), nil
		}
		select {
		case <-committed:
		case <-s.done:
			log.Log(logging.Fields{Operation: op}, "the wait ended: the store is shutting down after {reads} read(s)", logging.Values{"reads": reads})
			return nil, connect.NewError(connect.CodeUnavailable, errStoreStopping)
		case <-ctx.Done():
			log.Log(logging.Fields{Operation: op}, "the wait ended: the caller went away after {reads} read(s): {cause}", logging.Values{"reads": reads, "cause": ctx.Err().Error()})
			code := connect.CodeCanceled
			if errors.Is(ctx.Err(), context.DeadlineExceeded) {
				code = connect.CodeDeadlineExceeded
			}
			return nil, connect.NewError(code, ctx.Err())
		}
	}
}

// vendorTaskRecord is what the record holds of one vendor task locator: the
// agent it is paired with (FOUND), and that agent's commission (nil when the
// record holds no start for it).
type vendorTaskRecord struct {
	agent      string
	found      bool
	commission *conversationv1.AgentSubagentPrompt
}

// recordedAgentAndCommission reads the pairing and, once it is on record, the
// agent's commission: the one read GetAgentByVendorTask and
// AwaitAgentByVendorTask share.
func (s *Server) recordedAgentAndCommission(ctx context.Context, session, task string) (vendorTaskRecord, error) {
	agent, found, err := s.store.AgentByVendorTask(ctx, session, task)
	if err != nil || !found {
		return vendorTaskRecord{}, err
	}
	commission, described, err := s.store.AgentCommission(ctx, agent)
	if err != nil {
		return vendorTaskRecord{}, fmt.Errorf("reading the commission of agent %s: %w", agent, err)
	}
	if !described {
		commission = nil
	}
	return vendorTaskRecord{agent: agent, found: true, commission: commission}, nil
}

// awaitAgentByVendorTaskFailure has GetAgentByVendorTask's two arms.
func awaitAgentByVendorTaskFailure(ref *refusal) *connect.Response[storev1.AwaitAgentByVendorTaskResponse] {
	failure := &storev1.AwaitAgentByVendorTaskFailure{Detail: ref.detail}
	if ref.class == classStorage {
		failure.Kind = &storev1.AwaitAgentByVendorTaskFailure_StorageFailure{StorageFailure: &storev1.AwaitAgentByVendorTaskStorageFailure{}}
	} else {
		failure.Kind = &storev1.AwaitAgentByVendorTaskFailure_InvalidRequest{
			InvalidRequest: &storev1.AwaitAgentByVendorTaskInvalidRequest{Field: ref.field},
		}
	}
	return connect.NewResponse(&storev1.AwaitAgentByVendorTaskResponse{
		Result: &storev1.AwaitAgentByVendorTaskResponse_Failure{Failure: failure},
	})
}
