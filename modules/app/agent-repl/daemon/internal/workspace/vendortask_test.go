package workspace

import (
	"context"
	"errors"
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"
	shimv1 "agentrepl/proto/shim/v1"

	"agent-repld/internal/resolve/feed"
)

// fakeVendorTaskResolver answers one scripted ResolveVendorTask and records the
// request.
type fakeVendorTaskResolver struct {
	req  *shimv1.ResolveVendorTaskRequest
	resp *shimv1.ResolveVendorTaskResponse
	err  error
}

func (f *fakeVendorTaskResolver) ResolveVendorTask(_ context.Context, req *shimv1.ResolveVendorTaskRequest) (*shimv1.ResolveVendorTaskResponse, error) {
	f.req = req
	return f.resp, f.err
}

// foundTask is a success naming AGENT with NAME ("" for none).
func foundTask(agent, name string) *shimv1.ResolveVendorTaskResponse {
	found := &shimv1.ResolveVendorTaskAgent{Agent: &conversationv1.AgentId{Value: agent}}
	if name != "" {
		found.RequestedName = &name
	}
	return &shimv1.ResolveVendorTaskResponse{Result: &shimv1.ResolveVendorTaskResponse_Success{Success: &shimv1.ResolveVendorTaskSuccess{
		Agent: &shimv1.ResolveVendorTaskSuccess_Found{Found: found},
	}}}
}

func TestResolveVendorTaskAsksWithinTheSessionForTheTaskItWasGiven(t *testing.T) {
	// Arrange.
	client := &fakeVendorTaskResolver{resp: foundTask("toolu_spawn", "")}

	// Act.
	_, _ = resolveVendorTask(context.Background(), client, "agent-main", "a47cd")

	// Assert.
	if got := client.req.GetSession().GetValue() + "/" + client.req.GetVendorTaskId(); got != "agent-main/a47cd" {
		t.Fatalf("request = %q, want agent-main/a47cd", got)
	}
}

func TestResolveVendorTaskAnswersTheAgentAndItsName(t *testing.T) {
	// Arrange.
	client := &fakeVendorTaskResolver{resp: foundTask("toolu_spawn", "vetter")}

	// Act.
	agent, err := resolveVendorTask(context.Background(), client, "agent-main", "a47cd")

	// Assert.
	if err != nil || agent != (feed.VendorTaskAgent{Agent: "toolu_spawn", RequestedName: "vetter"}) {
		t.Fatalf("agent = %+v, %v; want the agent and its name", agent, err)
	}
}

func TestResolveVendorTaskAnswersATaskNoAgentIsPairedWithAsEmpty(t *testing.T) {
	// Arrange.
	client := &fakeVendorTaskResolver{resp: &shimv1.ResolveVendorTaskResponse{Result: &shimv1.ResolveVendorTaskResponse_Success{
		Success: &shimv1.ResolveVendorTaskSuccess{Agent: &shimv1.ResolveVendorTaskSuccess_NotRecorded{NotRecorded: &shimv1.ResolveVendorTaskNotRecorded{}}},
	}}}

	// Act.
	agent, err := resolveVendorTask(context.Background(), client, "agent-main", "a47cd")

	// Assert.
	if err != nil || agent != (feed.VendorTaskAgent{}) {
		t.Fatalf("agent = %+v, %v; want no agent and no error", agent, err)
	}
}

func TestResolveVendorTaskRefusalIsATypedShimRefusal(t *testing.T) {
	// Arrange.
	client := &fakeVendorTaskResolver{resp: &shimv1.ResolveVendorTaskResponse{Result: &shimv1.ResolveVendorTaskResponse_Failure{
		Failure: &shimv1.ResolveVendorTaskFailure{
			Detail: "database is locked",
			Kind:   &shimv1.ResolveVendorTaskFailure_StoreUnavailable{StoreUnavailable: &shimv1.ResolveVendorTaskStoreUnavailable{}},
		},
	}}}

	// Act.
	_, err := resolveVendorTask(context.Background(), client, "agent-main", "a47cd")

	// Assert.
	var refusal *ShimRefusal
	if !errors.As(err, &refusal) || refusal.Arm != "store_unavailable" {
		t.Fatalf("err = %v, want a store_unavailable ShimRefusal", err)
	}
}

func TestResolveVendorTaskTransportErrorPassesThrough(t *testing.T) {
	// Arrange.
	cause := errors.New("connection refused")
	client := &fakeVendorTaskResolver{err: cause}

	// Act.
	_, err := resolveVendorTask(context.Background(), client, "agent-main", "a47cd")

	// Assert.
	if !errors.Is(err, cause) {
		t.Fatalf("err = %v, want the transport error", err)
	}
}

func TestResolveVendorTaskAnswerWithNoArmIsAnError(t *testing.T) {
	// Arrange.
	client := &fakeVendorTaskResolver{resp: &shimv1.ResolveVendorTaskResponse{}}

	// Act.
	_, err := resolveVendorTask(context.Background(), client, "agent-main", "a47cd")

	// Assert.
	if err == nil {
		t.Fatal("resolveVendorTask accepted an answer with no arm")
	}
}

func TestResolveVendorTaskFoundNamingNoAgentIsAnError(t *testing.T) {
	// Arrange.
	client := &fakeVendorTaskResolver{resp: foundTask("", "vetter")}

	// Act.
	_, err := resolveVendorTask(context.Background(), client, "agent-main", "a47cd")

	// Assert.
	if err == nil {
		t.Fatal("resolveVendorTask accepted a found answer naming no agent")
	}
}

func TestFleetResolveVendorTaskWithNoShimHasNoSource(t *testing.T) {
	// Arrange.
	f := newFleetFixture(t)
	ws := f.workspace("w1")

	// Act.
	_, err := f.fleet.ResolveVendorTask(context.Background(), ws.ID, "agent-main", "a47cd")

	// Assert.
	if !errors.Is(err, feed.ErrNoHistorySource) {
		t.Fatalf("err = %v, want ErrNoHistorySource", err)
	}
}
