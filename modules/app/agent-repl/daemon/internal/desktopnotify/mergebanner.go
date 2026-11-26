package desktopnotify

import (
	"context"
	"fmt"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
)

// The banner kinds a merge raises.
const (
	// KindMergeResolved is the banner a merge raises as it leaves the queue
	// without landing: failed or abandoned.
	KindMergeResolved = "merge_resolved"
	// KindDeployMediated is the banner a deploy mediator raises for the user:
	// a deploy it escalated, or one it decided nothing about.
	KindDeployMediated = "deploy_mediated"
)

// opMergeBanner is the operation the merge banners' own records carry.
const opMergeBanner = "daemon.desktopnotify.merge_banner"

// MergeEnd is how a merge left the queue for good.
type MergeEnd int

const (
	// MergeLanded is a merge whose branch landed on its target.
	MergeLanded MergeEnd = iota
	// MergeFailed is a merge that ran and gave up.
	MergeFailed
	// MergeAbandoned is a merge taken off the queue, or ended, before it
	// could land.
	MergeAbandoned
)

// String names the end for the log record.
func (e MergeEnd) String() string {
	switch e {
	case MergeLanded:
		return "landed"
	case MergeFailed:
		return "failed"
	case MergeAbandoned:
		return "abandoned"
	}
	return fmt.Sprintf("MergeEnd(%d)", int(e))
}

// MergeBanners raises the merge queue's banners: one as a merge leaves the
// queue without landing (failed or abandoned), and the deploy mediator's.
// A merge put in line, every step it takes, and a merge that LANDS raise
// nothing (owner ruling, 2026-11-25): the user is told only what went wrong
// or what waits on them.
type MergeBanners struct {
	poster Poster
	log    dlog.Logger
}

// NewMergeBanners builds the merge banners. It panics on a missing
// collaborator.
func NewMergeBanners(poster Poster, log dlog.Logger) *MergeBanners {
	if poster == nil || log == nil {
		panic("desktopnotify: NewMergeBanners requires a Poster and a Logger")
	}
	return &MergeBanners{poster: poster, log: log}
}

// Resolved posts the banner of a merge that left the queue without landing,
// over its one-line account; a landed merge raises none. An end outside the
// three panics here, on the caller's goroutine, rather than inside the
// banner's composition.
func (m *MergeBanners) Resolved(ws ids.WorkspaceID, end MergeEnd, summary string) {
	if end == MergeLanded {
		m.log.Info(opMergeBanner, "a landed merge raises no desktop banner", dlog.Context{"workspace": string(ws)})
		return
	}
	mark := mergeEndMark(end)
	m.poster.Post(ws, KindMergeResolved, func(_ context.Context, name string) Banner {
		return Banner{Title: eventTitle(mark, name), Body: summary}
	})
}

// Mediated posts the banner of a deploy the mediator WS put before the
// user, over what it said of it, whether or not Emacs is focused: the
// deploy waits on the user.
func (m *MergeBanners) Mediated(ws ids.WorkspaceID, summary string) {
	m.poster.PostRegardlessOfFocus(ws, KindDeployMediated, func(_ context.Context, name string) Banner {
		return Banner{Title: DeployMediatedTitle(name), Body: summary}
	})
}

// DeployMediatedTitle is the title of a deploy its mediator put before the
// user.
func DeployMediatedTitle(name string) string {
	return eventTitle("🩺", name)
}

// mergeEndMark is the mark of a merge that left without landing. Any other
// end is a defect in the caller, and panics.
func mergeEndMark(end MergeEnd) string {
	switch end {
	case MergeFailed:
		return "❌"
	case MergeAbandoned:
		return "⛔"
	}
	panic(fmt.Sprintf("desktopnotify: a merge banner for an unknown end %s", end))
}
