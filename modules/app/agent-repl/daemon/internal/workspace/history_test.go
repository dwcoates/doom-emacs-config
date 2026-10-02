package workspace

import (
	"context"
	"errors"
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"
	shimv1 "agentrepl/proto/shim/v1"
)

// fakeHistoryReader answers one scripted ReadHistory and records the request.
type fakeHistoryReader struct {
	req  *shimv1.ReadHistoryRequest
	resp *shimv1.ReadHistoryResponse
	err  error
}

func (f *fakeHistoryReader) ReadHistory(_ context.Context, req *shimv1.ReadHistoryRequest) (*shimv1.ReadHistoryResponse, error) {
	f.req = req
	return f.resp, f.err
}

func pageResponse() *shimv1.ReadHistoryResponse {
	return &shimv1.ReadHistoryResponse{Result: &shimv1.ReadHistoryResponse_Success{Success: &shimv1.ReadHistorySuccess{
		Page: &conversationv1.HistoryPage{Boundary: &conversationv1.HistoryPage_Floor{Floor: &conversationv1.HistoryFloor{}}},
	}}}
}

func TestReadHistoryAsksForThePositionItWasGiven(t *testing.T) {
	tests := []struct {
		name      string
		after     *conversationv1.HistoryPointer
		wantFirst bool
		wantAfter string
	}{
		{name: "no pointer reads the newest page", wantFirst: true},
		{name: "a pointer reads the page before it", after: &conversationv1.HistoryPointer{Value: "p-7"}, wantAfter: "p-7"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			client := &fakeHistoryReader{resp: pageResponse()}

			// Act.
			if _, err := readHistory(context.Background(), client, nil, tt.after); err != nil {
				t.Fatalf("readHistory: %v", err)
			}

			// Assert.
			if got := client.req.GetFirst() != nil; got != tt.wantFirst {
				t.Fatalf("first = %v, want %v", got, tt.wantFirst)
			}
			if got := client.req.GetAfter().GetValue(); got != tt.wantAfter {
				t.Fatalf("after = %q, want %q", got, tt.wantAfter)
			}
		})
	}
}

func TestReadHistoryRefusalIsATypedShimRefusal(t *testing.T) {
	// Arrange.
	client := &fakeHistoryReader{resp: &shimv1.ReadHistoryResponse{Result: &shimv1.ReadHistoryResponse_Failure{Failure: &shimv1.ReadHistoryFailure{
		Detail: "store down",
		Kind:   &shimv1.ReadHistoryFailure_StoreUnavailable{StoreUnavailable: &shimv1.ReadHistoryStoreUnavailable{}},
	}}}}

	// Act.
	_, err := readHistory(context.Background(), client, nil, nil)

	// Assert.
	var refusal *ShimRefusal
	if !errors.As(err, &refusal) || refusal.Arm != "store_unavailable" {
		t.Fatalf("err = %v, want a store_unavailable ShimRefusal", err)
	}
}

func TestReadHistoryTransportErrorPassesThrough(t *testing.T) {
	// Arrange.
	broken := errors.New("link severed")
	client := &fakeHistoryReader{err: broken}

	// Act.
	_, err := readHistory(context.Background(), client, nil, nil)

	// Assert.
	if !errors.Is(err, broken) {
		t.Fatalf("err = %v, want the transport error", err)
	}
}

func TestReadHistoryAnswerWithNoArmIsAnError(t *testing.T) {
	// Arrange.
	client := &fakeHistoryReader{resp: &shimv1.ReadHistoryResponse{}}

	// Act.
	_, err := readHistory(context.Background(), client, nil, nil)

	// Assert.
	if err == nil {
		t.Fatal("readHistory accepted an answer with no arm")
	}
}
