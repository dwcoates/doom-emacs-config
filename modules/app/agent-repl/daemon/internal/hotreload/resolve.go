package hotreload

import (
	"fmt"

	frontendv1 "agentrepl/proto/frontend/v1"
)

// THE CARD IS RESOLVED HERE, AND ONLY HERE: every service's state, its
// reload step once its instances are decided, its summary, and the card's
// badge are read off the model, so the client derives nothing.

// headLabel is the card's head label.
const headLabel = "hot-reloading"

// shortHash is how many characters of a content hash the card draws.
const shortHash = 7

// Resolve answers the card C draws as.
func Resolve(c *Card) *frontendv1.FeedHotReload {
	out := &frontendv1.FeedHotReload{Head: &frontendv1.FeedHotReloadHead{Label: &frontendv1.FeedHotReloadLabel{Text: headLabel}}}
	for _, st := range c.Steps {
		out.Steps = append(out.Steps, wireStep(st))
	}
	live, failed := false, ""
	ended := c.StartedMS
	for _, svc := range c.Services {
		state := ServiceState(svc)
		switch {
		case !state.Kind.terminal():
			live = true
		case state.Kind == KindFailed && failed == "":
			failed = fmt.Sprintf("%s: %s", svc.Name, state.Words)
		}
		ended = max(ended, state.EndedMS)
		out.Services = append(out.Services, wireService(svc, state))
	}
	for _, st := range c.Steps {
		if st.State.Kind == KindFailed && failed == "" {
			failed = fmt.Sprintf("%s: %s", st.Name, st.State.Words)
		}
	}
	if c.Failure != "" {
		failed = c.Failure
	}
	switch {
	case live:
		out.State = &frontendv1.FeedHotReload_Live{Live: &frontendv1.FeedMergeTabLive{StartedAtMs: c.StartedMS}}
	case failed != "":
		out.State = &frontendv1.FeedHotReload_Settled{Settled: &frontendv1.FeedMergeTabSettled{StartedAtMs: c.StartedMS, EndedAtMs: ended,
			Outcome: &frontendv1.FeedMergeTabSettled_Failed{Failed: &frontendv1.FeedMergeTabFailed{Summary: failed}}}}
	default:
		out.State = &frontendv1.FeedHotReload_Settled{Settled: &frontendv1.FeedMergeTabSettled{StartedAtMs: c.StartedMS, EndedAtMs: ended,
			Outcome: &frontendv1.FeedMergeTabSettled_Succeeded{Succeeded: &frontendv1.FeedMergeTabSucceeded{}}}}
	}
	return out
}

// ReloadState answers where SVC's reload step stands: its own state until the
// deploy has decided every instance, and from then on its instances' state.
func ReloadState(svc *Service) State {
	own := svc.reloadStep().State
	// A DEPLOY THAT FAILED OR STOPPED ENDS THE STEP whatever its instances say.
	if !svc.Judged || own.Kind == KindFailed || own.Kind == KindSkipped {
		return own
	}
	sp := spec(svc.Name)
	if len(svc.Instances) == 0 {
		return State{Kind: KindUnchanged, Words: sp.current}
	}
	var running, waiting, failed *Instance
	started, ended := int64(0), int64(0)
	for i := range svc.Instances {
		in := &svc.Instances[i]
		if in.State.StartedMS != 0 && (started == 0 || in.State.StartedMS < started) {
			started = in.State.StartedMS
		}
		ended = max(ended, in.State.EndedMS)
		switch in.State.Kind {
		case KindRunning:
			running = firstOf(running, in)
		case KindWaiting:
			waiting = firstOf(waiting, in)
		case KindFailed:
			failed = firstOf(failed, in)
		}
	}
	started = nonZero(own.StartedMS, started)
	switch {
	case running != nil:
		return State{Kind: KindRunning, Words: running.State.Words, StartedMS: started}
	case waiting != nil:
		return State{Kind: KindWaiting, Words: waiting.State.Words, StartedMS: started}
	case failed != nil:
		return State{Kind: KindFailed, Words: fmt.Sprintf("%s %s", failed.Name, failed.State.Words), StartedMS: started, EndedMS: ended}
	default:
		return State{Kind: KindDone, Words: fmt.Sprintf("%d %s", countDone(svc), sp.counted), StartedMS: started, EndedMS: ended}
	}
}

func firstOf(have, in *Instance) *Instance {
	if have != nil {
		return have
	}
	return in
}

// nonZero answers A, or B when A is zero.
func nonZero(a, b int64) int64 {
	if a != 0 {
		return a
	}
	return b
}

// ServiceState answers where SVC stands as a whole: the first failed step, or
// the step at work, or, between steps, the last finished one's words; once
// every step has ended, how its reload step ended, spanning the service's
// whole work.
func ServiceState(svc *Service) State {
	steps := make([]State, len(svc.Steps))
	for i := range svc.Steps {
		steps[i] = svc.Steps[i].State
	}
	steps[len(steps)-1] = ReloadState(svc)
	started, ended := int64(0), int64(0)
	for _, st := range steps {
		if st.StartedMS != 0 && (started == 0 || st.StartedMS < started) {
			started = st.StartedMS
		}
		ended = max(ended, st.EndedMS)
	}
	var last *State
	for i := range steps {
		st := &steps[i]
		switch st.Kind {
		case KindFailed:
			return State{Kind: KindFailed, Words: st.Words, StartedMS: started, EndedMS: st.EndedMS}
		case KindRunning:
			return State{Kind: KindRunning, Words: st.Words, StartedMS: started}
		case KindWaiting:
			return State{Kind: KindWaiting, Words: st.Words, StartedMS: started}
		case KindSkipped:
			return State{Kind: KindSkipped, Words: st.Words}
		case KindScheduled:
			if last == nil {
				return State{Kind: KindScheduled, Words: "scheduled"}
			}
			return State{Kind: KindScheduled, Words: last.Words}
		default:
			last = st
		}
	}
	reload := steps[len(steps)-1]
	if reload.Kind == KindUnchanged {
		return State{Kind: KindUnchanged, Words: reload.Words}
	}
	return State{Kind: KindDone, Words: reload.Words, StartedMS: started, EndedMS: ended}
}

// Summary answers SVC's instance account while some instance is still at
// work ("2 of 5 replaced"), or "" when it has none to count or every one has
// ended, when the service's own words carry the count.
func Summary(svc *Service) string {
	sp := spec(svc.Name)
	if sp.counted == "" || len(svc.Instances) == 0 {
		return ""
	}
	open := false
	for _, in := range svc.Instances {
		open = open || !in.State.Kind.terminal()
	}
	if !open {
		return ""
	}
	return fmt.Sprintf("%d of %d %s", countDone(svc), len(svc.Instances), sp.counted)
}

// countDone counts SVC's instances done on the fresh build.
func countDone(svc *Service) int {
	done := 0
	for _, in := range svc.Instances {
		if in.State.Kind == KindDone {
			done++
		}
	}
	return done
}

// wireService is SVC as the card draws it, in state STATE.
func wireService(svc *Service, state State) *frontendv1.FeedHotReloadService {
	out := &frontendv1.FeedHotReloadService{
		Name:  &frontendv1.FeedHotReloadServiceName{Text: svc.Name},
		State: wireState(state),
		About: &frontendv1.FeedHotReloadServiceAbout{Text: spec(svc.Name).about},
	}
	if s := Summary(svc); s != "" {
		out.Summary = &frontendv1.FeedHotReloadServiceSummary{Text: s}
	}
	if svc.Fresh != "" {
		out.Builds = &frontendv1.FeedHotReloadBuilds{Fresh: &frontendv1.FeedHotReloadBuildFresh{ShortHash: short(svc.Fresh)}}
		if svc.Previous != "" {
			out.Builds.Previous = &frontendv1.FeedHotReloadBuildPrevious{ShortHash: short(svc.Previous)}
		}
	}
	for i, st := range svc.Steps {
		if i == len(svc.Steps)-1 {
			st.State = ReloadState(svc)
		}
		out.Steps = append(out.Steps, wireStep(st))
	}
	for _, in := range svc.Instances {
		out.Instances = append(out.Instances, &frontendv1.FeedHotReloadInstance{
			Name:  &frontendv1.FeedHotReloadInstanceName{Text: in.Name},
			State: wireState(in.State),
		})
	}
	return out
}

// short answers a content hash's first characters.
func short(hash string) string {
	if len(hash) > shortHash {
		return hash[:shortHash]
	}
	return hash
}

func wireStep(st Step) *frontendv1.FeedHotReloadStep {
	return &frontendv1.FeedHotReloadStep{Name: &frontendv1.FeedHotReloadStepName{Text: st.Name}, State: wireState(st.State)}
}

// wireState is ST as its oneof arm. A kind the model does not name is a bug.
func wireState(st State) *frontendv1.FeedHotReloadState {
	words := &frontendv1.FeedHotReloadWords{Text: st.Words}
	out := &frontendv1.FeedHotReloadState{}
	switch st.Kind {
	case KindScheduled:
		out.State = &frontendv1.FeedHotReloadState_Scheduled{Scheduled: &frontendv1.FeedHotReloadScheduled{Words: words}}
	case KindRunning:
		out.State = &frontendv1.FeedHotReloadState_Running{Running: &frontendv1.FeedHotReloadRunning{Words: words, StartedAtMs: st.StartedMS}}
	case KindWaiting:
		out.State = &frontendv1.FeedHotReloadState_Waiting{Waiting: &frontendv1.FeedHotReloadWaiting{Words: words, StartedAtMs: st.StartedMS}}
	case KindDone:
		out.State = &frontendv1.FeedHotReloadState_Done{Done: &frontendv1.FeedHotReloadDone{Words: words, StartedAtMs: st.StartedMS, EndedAtMs: st.EndedMS}}
	case KindUnchanged:
		out.State = &frontendv1.FeedHotReloadState_Unchanged{Unchanged: &frontendv1.FeedHotReloadUnchanged{Words: words}}
	case KindFailed:
		out.State = &frontendv1.FeedHotReloadState_Failed{Failed: &frontendv1.FeedHotReloadFailed{Words: words, StartedAtMs: st.StartedMS, EndedAtMs: st.EndedMS}}
	case KindSkipped:
		out.State = &frontendv1.FeedHotReloadState_Skipped{Skipped: &frontendv1.FeedHotReloadSkipped{Words: words}}
	default:
		panic(fmt.Sprintf("hotreload: %q is no state a card draws", st.Kind))
	}
	return out
}
