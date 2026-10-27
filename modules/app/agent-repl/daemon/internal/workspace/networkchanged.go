package workspace

import (
	"context"
	"errors"
	"sync"
	"time"

	shimv1 "agentrepl/proto/shim/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/shimclient"
)

// THE NETWORK CHANGING REACHES EVERY SHIM (owner ruling, 2026-10-27). The
// daemon's one watcher of the operating system's network events (netwatch)
// is followed by NetworkChanged, which tells every shim this fleet holds
// (shim.v1.NetworkChanged): a shim with something waiting on the API probes
// it now instead of at its next beat. Whether anything waits is the shim's
// judgment, answered whole.

const opNetworkChanged = "daemon.workspace.network_changed"

// networkChangedBound bounds one shim's answer. The shim answers from its
// event loop without awaiting the probe it starts, so an answer is a local
// round trip; a shim that takes longer is wedged, which is recorded, and the
// next change asks it again.
const networkChangedBound = 2 * time.Second

// NetworkChanged tells every shim the fleet holds that this machine's network
// changed, all at once, and returns once each has answered or run out its
// bound.
func (f *Fleet) NetworkChanged(ctx context.Context) {
	held := f.heldClients()
	log := f.deps.Log.Global()
	log.Debug(opNetworkChanged, "telling every held shim the network changed", dlog.Context{"shims": len(held)})
	var wg sync.WaitGroup
	for ws, client := range held {
		wg.Add(1)
		go func() {
			defer wg.Done()
			f.tellNetworkChanged(ctx, log.With(dlog.Context{"workspace": string(ws)}), client)
		}()
	}
	wg.Wait()
}

// tellNetworkChanged asks one shim and records its answer. A failure the
// shim client already recorded (a refusal, a death, a stand-down) is noted
// at DEBUG only; the bound running out is this call's own, recorded at ERROR.
func (f *Fleet) tellNetworkChanged(ctx context.Context, log dlog.Logger, client shimclient.Client) {
	bounded, cancel := context.WithTimeout(ctx, f.networkBound)
	defer cancel()
	response, err := client.NetworkChanged(bounded, &shimv1.NetworkChangedRequest{})
	switch {
	case err != nil && ctx.Err() == nil && errors.Is(bounded.Err(), context.DeadlineExceeded):
		log.Error(opNetworkChanged, "the shim did not answer the network change within its bound", dlog.Context{
			"bound_ms": f.networkBound.Milliseconds(), "cause": err.Error(),
		})
	case err != nil:
		log.Debug(opNetworkChanged, "the shim was not told the network changed; the shim client recorded why", dlog.Context{"cause": err.Error()})
	default:
		switch response.GetOutcome().(type) {
		case *shimv1.NetworkChangedResponse_Probing:
			log.Info(opNetworkChanged, "the shim probes the API now that the network changed", nil)
		case *shimv1.NetworkChangedResponse_Idle:
			log.Debug(opNetworkChanged, "the shim has nothing waiting on the API", nil)
		default:
			log.Error(opNetworkChanged, "the shim answered the network change with no outcome", dlog.Context{
				"invariant_violation": "NetworkChangedResponse always names its outcome",
			})
		}
	}
}

// heldClients answers every shim client the fleet holds that is not reaped.
func (f *Fleet) heldClients() map[ids.WorkspaceID]shimclient.Client {
	f.mu.RLock()
	defer f.mu.RUnlock()
	held := make(map[ids.WorkspaceID]shimclient.Client, len(f.sessions))
	for ws, session := range f.sessions {
		if !session.held() {
			continue
		}
		held[ws] = session.client
	}
	return held
}
