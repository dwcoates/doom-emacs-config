package workspace

import (
	"context"
	"fmt"

	conversationv1 "agentrepl/proto/conversation/v1"
	shimv1 "agentrepl/proto/shim/v1"

	"agent-repld/internal/ids"
	"agent-repld/internal/resolve/feed"
	"agent-repld/internal/shimclient"
)

// THE FEED'S VENDOR TASK SOURCE (feed.VendorTaskSource): which agent of a
// session one vendor task id names, from the store's locator pairing, read
// through the workspace's own shim.

// vendorTaskResolver is the one shim call ResolveVendorTask needs.
type vendorTaskResolver interface {
	ResolveVendorTask(ctx context.Context, req *shimv1.ResolveVendorTaskRequest) (*shimv1.ResolveVendorTaskResponse, error)
}

// ResolveVendorTask asks WS's shim which agent of SESSION's lineage VENDORTASKID
// names. A workspace with no shim answers feed.ErrNoHistorySource, exactly as a
// history read does.
func (f *Fleet) ResolveVendorTask(ctx context.Context, ws ids.WorkspaceID, session, vendorTaskID string) (feed.VendorTaskAgent, error) {
	return throughShim(f, ws, func(client shimclient.Client) (feed.VendorTaskAgent, error) {
		return resolveVendorTask(ctx, client, session, vendorTaskID)
	})
}

// resolveVendorTask makes the one ResolveVendorTask call and unwraps its
// outcome: the agent and its name, an empty agent for a task id no agent is
// paired with yet, or an error.
func resolveVendorTask(ctx context.Context, client vendorTaskResolver, session, vendorTaskID string) (feed.VendorTaskAgent, error) {
	response, err := client.ResolveVendorTask(ctx, &shimv1.ResolveVendorTaskRequest{
		Session:      &conversationv1.AgentId{Value: session},
		VendorTaskId: vendorTaskID,
	})
	if err != nil {
		return feed.VendorTaskAgent{}, err
	}
	if failure := response.GetFailure(); failure != nil {
		return feed.VendorTaskAgent{}, &ShimRefusal{Verb: "ResolveVendorTask", Arm: resolveVendorTaskArm(failure), Detail: failure.GetDetail()}
	}
	success := response.GetSuccess()
	switch answer := success.GetAgent().(type) {
	case *shimv1.ResolveVendorTaskSuccess_Found:
		if answer.Found.GetAgent().GetValue() == "" {
			return feed.VendorTaskAgent{}, fmt.Errorf("shim ResolveVendorTask found task %q naming no agent", vendorTaskID)
		}
		return feed.VendorTaskAgent{Agent: answer.Found.GetAgent().GetValue(), RequestedName: answer.Found.GetRequestedName()}, nil
	case *shimv1.ResolveVendorTaskSuccess_NotRecorded:
		return feed.VendorTaskAgent{}, nil
	default:
		return feed.VendorTaskAgent{}, fmt.Errorf("shim ResolveVendorTask answered neither an agent nor a failure for task %q", vendorTaskID)
	}
}

// resolveVendorTaskArm names a ResolveVendorTask refusal's arm.
func resolveVendorTaskArm(failure *shimv1.ResolveVendorTaskFailure) string {
	if failure.GetStoreUnavailable() != nil {
		return "store_unavailable"
	}
	return ArmShimUnspecified
}
