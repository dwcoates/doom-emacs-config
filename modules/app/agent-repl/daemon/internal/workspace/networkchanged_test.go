package workspace

import (
	"context"
	"errors"
	"testing"
	"time"

	shimv1 "agentrepl/proto/shim/v1"

	"agent-repld/internal/dlog"
)

// networkRecord finds the fleet's record of a network change by level and
// message, naming its workspace.
func networkRecord(f *fleetFixture, level, message string) (dlog.Record, bool) {
	for _, r := range f.log.logger.Records() {
		if r.Level == level && r.Operation == opNetworkChanged && r.Message == message {
			return r, true
		}
	}
	return dlog.Record{}, false
}

func TestNetworkChangedTellsEveryHeldShim(t *testing.T) {
	// Arrange.
	f := newFleetFixture(t)
	one, two := &fakeClient{}, &fakeClient{}
	f.fleet.sessions["w1"] = &live{client: one}
	f.fleet.sessions["w2"] = &live{client: two}

	// Act.
	f.fleet.NetworkChanged(context.Background())

	// Assert.
	if one.networkChanges != 1 || two.networkChanges != 1 {
		t.Fatalf("network changes told = %d and %d, want one each", one.networkChanges, two.networkChanges)
	}
}

func TestNetworkChangedSkipsAReapedShim(t *testing.T) {
	// Arrange.
	f := newFleetFixture(t)
	reaped := &fakeClient{reaped: true}
	f.fleet.sessions["w1"] = &live{client: reaped}

	// Act.
	f.fleet.NetworkChanged(context.Background())

	// Assert.
	if reaped.networkChanges != 0 {
		t.Fatalf("a reaped shim was told the network changed %d times", reaped.networkChanges)
	}
}

func TestNetworkChangedRecordsEachAnswer(t *testing.T) {
	tests := []struct {
		name     string
		response *shimv1.NetworkChangedResponse
		err      error
		level    string
		message  string
	}{
		{
			name:     "a shim that probes",
			response: &shimv1.NetworkChangedResponse{Outcome: &shimv1.NetworkChangedResponse_Probing{Probing: &shimv1.NetworkChangedProbing{}}},
			level:    "info", message: "the shim probes the API now that the network changed",
		},
		{
			name:     "a shim with nothing waiting",
			response: &shimv1.NetworkChangedResponse{Outcome: &shimv1.NetworkChangedResponse_Idle{Idle: &shimv1.NetworkChangedIdle{}}},
			level:    "debug", message: "the shim has nothing waiting on the API",
		},
		{
			name:     "a shim answering no outcome",
			response: &shimv1.NetworkChangedResponse{},
			level:    "error", message: "the shim answered the network change with no outcome",
		},
		{
			name:  "a call the shim client already recorded",
			err:   errors.New("the shim died"),
			level: "debug", message: "the shim was not told the network changed; the shim client recorded why",
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			f := newFleetFixture(t)
			f.fleet.sessions["w1"] = &live{client: &fakeClient{networkResponse: tt.response, networkErr: tt.err}}

			// Act.
			f.fleet.NetworkChanged(context.Background())

			// Assert.
			record, ok := networkRecord(f, tt.level, tt.message)
			if !ok {
				t.Fatalf("no %s record %q: %+v", tt.level, tt.message, f.log.logger.Records())
			}
			if record.Context["workspace"] != "w1" {
				t.Fatalf("record names workspace %v, want w1", record.Context["workspace"])
			}
		})
	}
}

// blockingClient answers NetworkChanged only once its context ends.
type blockingClient struct{ *fakeClient }

func (c blockingClient) NetworkChanged(ctx context.Context, _ *shimv1.NetworkChangedRequest) (*shimv1.NetworkChangedResponse, error) {
	<-ctx.Done()
	return nil, ctx.Err()
}

func TestNetworkChangedRecordsAShimPastItsBoundAtError(t *testing.T) {
	// Arrange.
	f := newFleetFixture(t)
	f.fleet.networkBound = time.Millisecond
	f.fleet.sessions["w1"] = &live{client: blockingClient{&fakeClient{}}}

	// Act.
	f.fleet.NetworkChanged(context.Background())

	// Assert.
	record, ok := networkRecord(f, "error", "the shim did not answer the network change within its bound")
	if !ok {
		t.Fatalf("no error record of the bound: %+v", f.log.logger.Records())
	}
	if record.Context["bound_ms"] != int64(0) && record.Context["bound_ms"] != int64(1) {
		t.Fatalf("bound_ms = %v, want the bound", record.Context["bound_ms"])
	}
}

func TestNetworkChangedRecordsNothingAtErrorWhenItsCallerEnds(t *testing.T) {
	// Arrange.
	f := newFleetFixture(t)
	f.fleet.sessions["w1"] = &live{client: blockingClient{&fakeClient{}}}
	ctx, cancel := context.WithCancel(context.Background())
	cancel()

	// Act.
	f.fleet.NetworkChanged(ctx)

	// Assert.
	for _, r := range f.log.logger.Records() {
		if r.Level == "error" {
			t.Fatalf("an ended caller was recorded at error: %+v", r)
		}
	}
}
