package cachelapse

import (
	"context"
	"errors"
	"fmt"
	"sync"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	shimv1 "agentrepl/proto/shim/v1"
	workspacev1 "agentrepl/proto/workspace/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/shimclient"
	"agent-repld/internal/wsm"
)

// This file is the user's keep-alive for the hidden section
// (agentrepl.v1.SendHiddenKeepalives, `SPC j R`;
// docs/protobuf-design/hidden-keepalives-on-demand.md): one keep-alive to
// every hidden workspace's shim (shim.v1.SendKeepalive), each cadence left
// held. The keep-alive's answer re-anchors the cache, the held shim restates
// its projected lapse, and record takes it as it takes any projection: the
// countdown moves on the roster with no second path for it.

const opBeat = "daemon.cachelapse.beat"

// ErrBeatSubmitFailed is a shim's answer that it judged the keep-alive
// sendable and its submission failed. The *BeatSubmitFailed carries why.
var ErrBeatSubmitFailed = errors.New("cachelapse: the keep-alive's submission failed")

// BeatSubmitFailed is the shim's submit_failed answer.
type BeatSubmitFailed struct {
	// Detail is the submission's own failure, verbatim from the shim.
	Detail string
}

func (e *BeatSubmitFailed) Error() string {
	return fmt.Sprintf("%v: %s", ErrBeatSubmitFailed, e.Detail)
}

func (e *BeatSubmitFailed) Is(target error) bool { return target == ErrBeatSubmitFailed }

// SendHiddenKeepalives asks the shim of every workspace in the hidden section
// for one keep-alive, all at once, and answers when every one has answered.
// Each workspace's outcome is reported beside the others'; only a registry
// that cannot be read fails the request.
func (k *Keeper) SendHiddenKeepalives(ctx context.Context) *agentreplv1.SendHiddenKeepalivesResponse {
	log := k.deps.Log.Global()
	workspaces, err := k.deps.DB.ListWorkspaces(ctx)
	if err != nil {
		log.Error(opBeat, "could not list the workspaces to send the hidden section its keep-alives", dlog.Context{"cause": err.Error()})
		return &agentreplv1.SendHiddenKeepalivesResponse{Result: &agentreplv1.SendHiddenKeepalivesResponse_Error{
			Error: &agentreplv1.SendHiddenKeepalivesError{Cause: &agentreplv1.SendHiddenKeepalivesError_RegistryUnreadable{
				RegistryUnreadable: &agentreplv1.SendHiddenKeepalivesRegistryUnreadable{Detail: err.Error()},
			}},
		}}
	}
	var hidden []wsm.Workspace
	for _, ws := range workspaces {
		if ws.InHiddenSection() {
			hidden = append(hidden, ws)
		}
	}
	outcomes := make([]*agentreplv1.SendHiddenKeepalivesWorkspace, len(hidden))
	var wg sync.WaitGroup
	for i, ws := range hidden {
		wg.Add(1)
		go func() {
			defer wg.Done()
			outcomes[i] = k.beat(ctx, ws)
		}()
	}
	wg.Wait()
	log.Info(opBeat, "sent the hidden section its keep-alives", tally(outcomes))
	return &agentreplv1.SendHiddenKeepalivesResponse{Result: &agentreplv1.SendHiddenKeepalivesResponse_Success{
		Success: &agentreplv1.SendHiddenKeepalivesSuccess{Workspaces: outcomes},
	}}
}

// beat asks one hidden workspace's shim for its keep-alive, and records how
// it went.
func (k *Keeper) beat(ctx context.Context, ws wsm.Workspace) *agentreplv1.SendHiddenKeepalivesWorkspace {
	log := k.deps.Log.Global().With(dlog.Context{"workspace": string(ws.ID)})
	out := &agentreplv1.SendHiddenKeepalivesWorkspace{Workspace: &workspacev1.WorkspaceRef{Id: string(ws.ID), Dir: ws.Dir}}
	shim, ok := k.deps.Shim(ws.ID)
	if !ok {
		log.Info(opBeat, "no session is served for the hidden workspace; it has no cache to keep", nil)
		setNotRunning(out)
		return out
	}
	answer, err := shim.Beat(ctx)
	if err != nil {
		k.beatFailure(ctx, log, out, err)
		return out
	}
	SetBeatOutcome(out, answer)
	log.Info(opBeat, "the hidden workspace's shim answered its keep-alive", dlog.Context{"outcome": outcomeName(out)})
	return out
}

// beatFailure sets OUT's outcome for a keep-alive call that did not answer
// a judgment, and makes its one record.
func (k *Keeper) beatFailure(ctx context.Context, log dlog.Logger, out *agentreplv1.SendHiddenKeepalivesWorkspace, err error) {
	fields := dlog.Context{"cause": err.Error()}
	switch {
	case errors.Is(err, ErrNoSession):
		log.Info(opBeat, "the hidden workspace's shim holds no session; it has no cache to keep", fields)
		setNotRunning(out)
		return
	case errors.Is(err, shimclient.ErrShimDied):
		// THE DEATH IS RECORDED ONCE, by daemon.shimclient.exit.
		log.Info(opBeat, "the shim died under the keep-alive call; the death is recorded by daemon.shimclient.exit", fields)
	case leaving(ctx, log, opBeat, "sending a hidden workspace its keep-alive", err):
	default:
		log.Error(opBeat, "could not send the hidden workspace its keep-alive", fields)
	}
	var submit *BeatSubmitFailed
	detail := err.Error()
	if errors.As(err, &submit) {
		detail = submit.Detail
	}
	out.Outcome = &agentreplv1.SendHiddenKeepalivesWorkspace_Failed{Failed: &agentreplv1.SendHiddenKeepalivesFailed{Detail: detail}}
}

// SetBeatOutcome sets OUT's outcome from the shim's judgment of one asked-for
// keep-alive. It is THE ONE MAPPING from shim.v1.SendKeepalive into the
// editor's vocabulary (agentrepl.v1.SendHiddenKeepalives). A judgment this
// daemon does not name is a contract violation by the shim, and fails hard.
func SetBeatOutcome(out *agentreplv1.SendHiddenKeepalivesWorkspace, answer *shimv1.SendKeepaliveSuccess) {
	switch outcome := answer.GetOutcome().(type) {
	case *shimv1.SendKeepaliveSuccess_Sent:
		out.Outcome = &agentreplv1.SendHiddenKeepalivesWorkspace_Sent{Sent: &agentreplv1.SendHiddenKeepalivesSent{}}
	case *shimv1.SendKeepaliveSuccess_Skipped:
		skipped := &agentreplv1.SendHiddenKeepalivesSkipped{}
		switch reason := outcome.Skipped.GetReason().(type) {
		case *shimv1.SendKeepaliveSkipped_MainAgentBusy:
			skipped.Reason = &agentreplv1.SendHiddenKeepalivesSkipped_Busy{Busy: &agentreplv1.SendHiddenKeepalivesSkippedBusy{
				Detail: reason.MainAgentBusy.GetDetail(),
			}}
		case *shimv1.SendKeepaliveSkipped_DetachedWorkLive:
			skipped.Reason = &agentreplv1.SendHiddenKeepalivesSkipped_DetachedWorkLive{DetachedWorkLive: &agentreplv1.SendHiddenKeepalivesSkippedDetachedWorkLive{
				LiveItems: reason.DetachedWorkLive.GetLiveItems(),
			}}
		case *shimv1.SendKeepaliveSkipped_CacheLapsed:
			skipped.Reason = &agentreplv1.SendHiddenKeepalivesSkipped_CacheLapsed{CacheLapsed: &agentreplv1.SendHiddenKeepalivesSkippedCacheLapsed{
				LapsedAtMs: reason.CacheLapsed.GetLapsedAtMs(),
			}}
		default:
			panic(fmt.Sprintf("cachelapse: the shim skipped a keep-alive for a reason this daemon does not name (%T)", reason))
		}
		out.Outcome = &agentreplv1.SendHiddenKeepalivesWorkspace_Skipped{Skipped: skipped}
	default:
		panic(fmt.Sprintf("cachelapse: the shim answered a keep-alive with an outcome this daemon does not name (%T)", outcome))
	}
}

func setNotRunning(out *agentreplv1.SendHiddenKeepalivesWorkspace) {
	out.Outcome = &agentreplv1.SendHiddenKeepalivesWorkspace_NotRunning{NotRunning: &agentreplv1.SendHiddenKeepalivesNotRunning{}}
}

// outcomeName names a workspace's outcome for the records.
func outcomeName(out *agentreplv1.SendHiddenKeepalivesWorkspace) string {
	switch o := out.GetOutcome().(type) {
	case *agentreplv1.SendHiddenKeepalivesWorkspace_Sent:
		return "sent"
	case *agentreplv1.SendHiddenKeepalivesWorkspace_Skipped:
		switch o.Skipped.GetReason().(type) {
		case *agentreplv1.SendHiddenKeepalivesSkipped_Busy:
			return "skipped_busy"
		case *agentreplv1.SendHiddenKeepalivesSkipped_DetachedWorkLive:
			return "skipped_detached_work_live"
		case *agentreplv1.SendHiddenKeepalivesSkipped_CacheLapsed:
			return "skipped_cache_lapsed"
		}
	case *agentreplv1.SendHiddenKeepalivesWorkspace_NotRunning:
		return "not_running"
	case *agentreplv1.SendHiddenKeepalivesWorkspace_Failed:
		return "failed"
	}
	panic(fmt.Sprintf("cachelapse: a keep-alive outcome with no name (%T)", out.GetOutcome()))
}

// tally counts the outcomes by name for the summary record.
func tally(outcomes []*agentreplv1.SendHiddenKeepalivesWorkspace) dlog.Context {
	fields := dlog.Context{"hidden": len(outcomes)}
	for _, out := range outcomes {
		name := outcomeName(out)
		n, _ := fields[name].(int)
		fields[name] = n + 1
	}
	return fields
}
