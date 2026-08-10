package main

import (
	"time"

	"claude-repld/internal/dlog"
	"claude-repld/internal/restartannounce"
)

// restartAnnouncementSinks returns the carriers an intentional-restart
// announcement is delivered over.
//
// IT IS EMPTY TODAY, AND THAT IS A CONTRACT GAP RATHER THAN AN OVERSIGHT. The
// announcement needs a frontend frame arm to ride on (a RestartPendingView on
// FrontendFrame), and the frontend proto is gated: it is agreed as a contract
// before it is written, never grown as a side effect of a daemon change. Until
// that arm exists there is no wire value for "I am going down on purpose", so
// there is nothing an honest sink could send.
//
// Everything AROUND the arm is wired and tested: the shutdown path calls the
// announcer at the one correct moment, the announcer validates and fans out,
// and both clients already act on the announced window through their own
// injectable entry points. Landing the arm is therefore one sink constructor
// here (frontend.Server.Broadcast of the new frame) plus one frame handler in
// each client.
func restartAnnouncementSinks() []restartannounce.Sink { return nil }

// announceIntentionalRestart composes and broadcasts the announcement for one
// shutdown request.
//
// A failure NEVER blocks the shutdown: the daemon was told to go down, and
// refusing to do so because the courtesy notice did not land would turn a
// cosmetic problem into an operational one. It is recorded at ERROR instead,
// because the consequence is real and visible — every connected client is
// about to render an unexplained disconnect.
func announceIntentionalRestart(logger *dlog.Logger, req shutdownRequest) {
	log := logger.With("operation", "shutdown-announce-restart")
	announcer, err := restartannounce.New(log.Log, restartAnnouncementSinks()...)
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
