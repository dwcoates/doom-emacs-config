package footer

import (
	"time"

	frontendv1 "agentrepl/proto/frontend/v1"

	"claude-repld/internal/deployprogress"
	"claude-repld/internal/dlog"
)

// THE UPDATE LINE: a deploy's progress on every workspace's strip (owner
// request, 2026-09-27). Deploy feedback used to be the webapp's yellow
// "restarting" banner alone, drawn only once the daemon announced its
// shutdown: a 17s build said nothing at all, a workspace whose move waited on
// its own background work said nothing about why, and a shim left for later
// said nothing about being left.
//
// IT IS RESOLVER-WIDE, like a daemon-scoped fault: a deploy moves what serves
// every workspace, so ONE statement stands on every strip, and it is told
// through ONE entry point (deployprogress.Sink). Each statement replaces the
// last. What is per-workspace about it is derived HERE from what this
// resolver already holds for that workspace:
//
//   - WAITING is read off the workspace's own turn and live-work set while a
//     draining move is in flight. It is the same set the bounce registry
//     judges the move's freeness by (the one live-work authority), so the
//     counts fall as the work drains through the ordinary live-work edges and
//     nobody has to push them.
//   - the NOTES are the deploy's per-workspace deferrals, read by id.
//
// IT IS SALIENT while the deploy still moves under the workspace, ranked in
// each status arm's salient tier below the kind that explains the substatus
// and any escalating fault (activity.go). A deploy that fails is a fault,
// drawn through the fault path; this line then goes.
//
// `updated` IS AN EVENT, NOT A STANDING LINE (owner ruling, 2026-09-28): the
// finished deploy blocks nothing, so the statement that ends the story takes
// the salient line down and raises the transient `updated` on every strip,
// with that workspace's own deferrals. No timer retires anything: the
// transient's expiry is the client's to apply.

// opDeployProgress is the operation every update-line statement is recorded
// under.
const opDeployProgress = "daemon.footer.deploy_progress"

// deployState is the standing deploy progress and when it began standing.
type deployState struct {
	progress *deployprogress.Progress
	at       time.Time
}

// SetDeployProgress implements deployprogress.Sink: it stands (or, with nil,
// clears) the update line on every workspace's strip.
//
// A PROGRESS WITH NO PHASE IS A CALLER'S BUG and is refused at ERROR rather
// than drawn: an update line saying nothing about where the deploy is would
// be the silence the line exists to end.
func (r *resolver) SetDeployProgress(progress *deployprogress.Progress) {
	ctx := deployContext(progress)
	if progress != nil && !progress.Phase.Valid() {
		r.log.Global().Error(opDeployProgress, "refused a deploy progress that names no phase", ctx)
		return
	}
	if progress != nil && progress.Phase == deployprogress.Updated {
		r.log.Global().Info(opDeployProgress, "the deploy finished; the footer took its line down and announced it on every strip", ctx)
		r.mutateAll(opDeployProgress, "the footer announced a finished deploy on every strip", ctx,
			func(s *wsState) { r.raiseUpdated(s.id, s, updateNotes(progress.Notes[s.id])) },
			func() { r.deploy = nil })
		return
	}
	r.log.Global().Info(opDeployProgress, "the footer took a deploy's progress onto every strip", ctx)
	r.mutateAll(opDeployProgress, "the footer took a deploy's progress onto every strip", ctx,
		func(*wsState) {}, func() { r.standDeployLocked(progress) })
}

// standDeployLocked replaces the standing progress, nil to clear it. The caller
// holds r.mu.
func (r *resolver) standDeployLocked(progress *deployprogress.Progress) {
	if progress == nil {
		r.deploy = nil
		return
	}
	r.deploy = &deployState{progress: progress, at: r.opts.clock.Now()}
}

// updateLine is the update line for this workspace, or nil when no deploy is
// in flight, and the instant it began standing.
func (r *resolver) updateLine(s *wsState) (*frontendv1.FooterStatusActivityUpdate, time.Time) {
	if r.deploy == nil {
		return nil, time.Time{}
	}
	progress := r.deploy.progress
	line := &frontendv1.FooterStatusActivityUpdate{Notes: updateNotes(progress.Notes[s.id])}
	turns, background := s.inFlight()
	switch {
	case progress.Draining && (turns > 0 || background > 0):
		// THIS workspace's move waits on its own work. Nothing is interrupted
		// to hurry it, and the counts are the ones the move is waiting on.
		line.Phase = &frontendv1.FooterStatusActivityUpdate_Waiting{
			Waiting: &frontendv1.FooterStatusActivityUpdateWaiting{Turns: turns, Background: background}}
	case progress.Phase == deployprogress.Building:
		line.Phase = &frontendv1.FooterStatusActivityUpdate_Building{
			Building: &frontendv1.FooterStatusActivityUpdateBuilding{Components: updateComponents(progress.Components)}}
	case progress.Phase == deployprogress.Installing:
		line.Phase = &frontendv1.FooterStatusActivityUpdate_Installing{
			Installing: &frontendv1.FooterStatusActivityUpdateInstalling{}}
	case progress.Phase == deployprogress.RestartingServices:
		line.Phase = &frontendv1.FooterStatusActivityUpdate_RestartingServices{
			RestartingServices: &frontendv1.FooterStatusActivityUpdateRestartingServices{
				Services: updateComponents(progress.Components)}}
	default:
		// HANDING OVER is the last phase that stands: `updated` never does
		// (SetDeployProgress raises it as a transient), and an invalid phase
		// is refused before it is stood.
		line.Phase = &frontendv1.FooterStatusActivityUpdate_HandingOver{
			HandingOver: &frontendv1.FooterStatusActivityUpdateHandingOver{}}
	}
	return line, r.deploy.at
}

// inFlight is what a move off this daemon waits on for this workspace: the
// turn in flight and the detached items still running.
func (s *wsState) inFlight() (turns, background uint32) {
	if s.turn != nil {
		turns = 1
	}
	return turns, uint32(s.detachedCount())
}

// updateComponents renders the components onto their wire arms, in order.
func updateComponents(components []deployprogress.Component) []*frontendv1.FooterStatusActivityUpdateComponent {
	out := make([]*frontendv1.FooterStatusActivityUpdateComponent, 0, len(components))
	for _, c := range components {
		out = append(out, updateComponent(c))
	}
	return out
}

// updateComponent renders one component. Every component the vocabulary
// spells has an arm; a spelling outside it is a caller's bug that would draw
// an unset arm, which every client refuses, so it is caught by the test that
// walks the vocabulary rather than defaulted to something else here.
func updateComponent(c deployprogress.Component) *frontendv1.FooterStatusActivityUpdateComponent {
	out := &frontendv1.FooterStatusActivityUpdateComponent{}
	switch c {
	case deployprogress.Store:
		out.Component = &frontendv1.FooterStatusActivityUpdateComponent_Store{Store: &frontendv1.FooterStatusActivityUpdateComponentStore{}}
	case deployprogress.Sidecar:
		out.Component = &frontendv1.FooterStatusActivityUpdateComponent_Sidecar{Sidecar: &frontendv1.FooterStatusActivityUpdateComponentSidecar{}}
	case deployprogress.Daemon:
		out.Component = &frontendv1.FooterStatusActivityUpdateComponent_Daemon{Daemon: &frontendv1.FooterStatusActivityUpdateComponentDaemon{}}
	case deployprogress.Shim:
		out.Component = &frontendv1.FooterStatusActivityUpdateComponent_Shim{Shim: &frontendv1.FooterStatusActivityUpdateComponentShim{}}
	case deployprogress.Webapp:
		out.Component = &frontendv1.FooterStatusActivityUpdateComponent_Webapp{Webapp: &frontendv1.FooterStatusActivityUpdateComponentWebapp{}}
	}
	return out
}

// updateNotes renders one workspace's deferrals onto their wire arms.
func updateNotes(notes []deployprogress.Note) []*frontendv1.FooterStatusActivityUpdateNote {
	out := make([]*frontendv1.FooterStatusActivityUpdateNote, 0, len(notes))
	for _, n := range notes {
		note := &frontendv1.FooterStatusActivityUpdateNote{}
		if n == deployprogress.ShimWhenIdle {
			note.Note = &frontendv1.FooterStatusActivityUpdateNote_ShimWhenIdle{
				ShimWhenIdle: &frontendv1.FooterStatusActivityUpdateNoteShimWhenIdle{}}
		}
		out = append(out, note)
	}
	return out
}

// deployContext is a progress statement's record fields.
func deployContext(progress *deployprogress.Progress) dlog.Context {
	if progress == nil {
		return dlog.Context{"phase": "none"}
	}
	components := make([]string, 0, len(progress.Components))
	for _, c := range progress.Components {
		components = append(components, string(c))
	}
	return dlog.Context{
		"phase":      progress.Phase.String(),
		"components": components,
		"draining":   progress.Draining,
		"noted":      len(progress.Notes),
	}
}
