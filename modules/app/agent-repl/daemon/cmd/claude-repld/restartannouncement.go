package main

import (
	"errors"
	"time"

	"claude-repld/internal/dlog"
	"claude-repld/internal/restartannounce"

	frontendv1 "agentrepl/proto/agentshim/frontend/v1"
)

// restartAnnouncementPublisher is the narrow frontend surface the announcement
// rides on. *frontend.Server satisfies it.
//
// The two methods are SEPARATE rather than one broadcast because the two
// carriers fail independently — the webapp can be told while Emacs is not —
// and each returns the number of clients of its kind that were actually
// queued, so the shutdown log can name which frontends heard the notice.
type restartAnnouncementPublisher interface {
	PushRestartPendingToGUI(*frontendv1.RestartPendingView) int
	PushRestartPendingToHost(*frontendv1.RestartPendingView) int
}

// restartPendingView renders an announcement onto the wire.
//
// The outage hint is CLAMPED here, at the single point where the daemon's
// internal duration becomes a client-facing number: the wire field is whole
// seconds, and a sub-second duration would otherwise truncate to zero — a
// value the proto forbids, because "stay quiet for no time" is not a
// representable request. Anything positive is therefore floored at one second,
// and restartannounce.Validate has already refused a hint above the cap.
func restartPendingView(a restartannounce.Announcement) *frontendv1.RestartPendingView {
	seconds := int32(a.ExpectedOutage / time.Second)
	if seconds < 1 {
		seconds = 1
	}
	return &frontendv1.RestartPendingView{
		Cause:                 a.Cause,
		ExpectedOutageSeconds: seconds,
		StopShims:             a.StopShims,
		AnnouncedAtMs:         a.AtMs,
	}
}

// restartAnnouncementSinks returns the carriers an intentional-restart
// announcement is delivered over: the gui_stream broadcast and the Emacs UDS
// host connection.
//
// A ZERO DELIVERY COUNT IS NOT A FAILURE, and the distinction is the whole
// reason the counts are read at all. The announcement exists to stop a
// CONNECTED client from painting a deliberate bounce as a crash; a frontend
// that is not connected paints nothing, so telling zero clients of a kind is a
// complete delivery of the notice to everyone it could have helped. What would
// be a failure is a publisher that is absent entirely — there is then no
// carrier at all, and every connected client of that kind loses the notice —
// so a nil publisher is refused rather than turned into an empty sink list
// that would report a serene success.
//
// The counts are still LOGGED, because "the daemon bounced and Emacs was not
// listening" is exactly the fact that explains a degraded-link segment
// appearing in the editor a moment later.
func restartAnnouncementSinks(logf func(string, ...any), pub restartAnnouncementPublisher) ([]restartannounce.Sink, error) {
	if logf == nil {
		return nil, errors.New("claude-repld: restart announcement sinks need a logger")
	}
	if pub == nil {
		return nil, errors.New("claude-repld: restart announcement has no frontend publisher; no connected client can be told this bounce is deliberate")
	}
	return []restartannounce.Sink{
		restartannounce.SinkFunc{
			Label: "gui",
			Deliver: func(a restartannounce.Announcement) error {
				n := pub.PushRestartPendingToGUI(restartPendingView(a))
				logf("restart-announce: sink=gui gui_clients=%d", n)
				return nil
			},
		},
		restartannounce.SinkFunc{
			Label: "emacs-host",
			Deliver: func(a restartannounce.Announcement) error {
				n := pub.PushRestartPendingToHost(restartPendingView(a))
				if n == 0 {
					logf("restart-announce: sink=emacs-host host_clients=0; the editor was not connected and will render this deliberate bounce as a degraded link")
					return nil
				}
				logf("restart-announce: sink=emacs-host host_clients=%d", n)
				return nil
			},
		},
	}, nil
}

// announceIntentionalRestart composes and broadcasts the announcement for one
// shutdown request.
//
// A failure NEVER blocks the shutdown: the daemon was told to go down, and
// refusing to do so because the courtesy notice did not land would turn a
// cosmetic problem into an operational one. It is recorded at ERROR instead,
// because the consequence is real and visible — every connected client is
// about to render an unexplained disconnect.
func announceIntentionalRestart(logger *dlog.Logger, pub restartAnnouncementPublisher, req shutdownRequest) {
	log := logger.With("operation", "shutdown-announce-restart")
	sinks, err := restartAnnouncementSinks(log.Log, pub)
	if err != nil {
		log.LogError("claude-repld: restart announcement not composed: %v", err)
		return
	}
	announcer, err := restartannounce.New(log.Log, sinks...)
	if err != nil {
		log.LogError("claude-repld: restart announcement not composed: %v", err)
		return
	}
	ann, err := restartannounce.Compose(time.Now, req.cause.String(), req.stopShims, restartannounce.DefaultExpectedOutage)
	if err != nil {
		log.LogError("claude-repld: restart announcement not composed: %v", err)
		return
	}
	if err := announcer.Announce(ann); err != nil {
		log.LogError("claude-repld: restart announcement not delivered: %v; connected clients will render this deliberate bounce as an unexplained disconnect", err)
	}
}
