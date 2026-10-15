package desktopnotify

import (
	"context"
	"fmt"
	"time"

	"agent-repld/internal/ids"
)

// The banner kinds a merge raises.
const (
	// KindMergeQueued is the banner a merge raises as it is put in line.
	KindMergeQueued = "merge_queued"
	// KindMergeResolved is the banner a merge raises as it leaves the queue
	// for good: landed, failed or abandoned.
	KindMergeResolved = "merge_resolved"
)

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

// MergeBanners raises the merge queue's two banners, and no others: one as a
// merge is put in line, and one as it leaves the queue for good. Every step
// between -- admission, the rebase, the test gate, a repair turn, the
// handback to a detached merge's requester -- is the merge's own business and
// raises nothing.
type MergeBanners struct {
	poster Poster
	now    func() time.Time
}

// NewMergeBanners builds the merge banners. It panics on a missing
// collaborator.
func NewMergeBanners(poster Poster, now func() time.Time) *MergeBanners {
	if poster == nil || now == nil {
		panic("desktopnotify: NewMergeBanners requires a Poster and a clock")
	}
	return &MergeBanners{poster: poster, now: now}
}

// Queued posts the banner of a merge put in line: what it lands, under the
// workspace it runs in.
func (m *MergeBanners) Queued(ws ids.WorkspaceID, label string) {
	at := m.now()
	m.poster.Post(ws, KindMergeQueued, func(_ context.Context, name string) Banner {
		return Banner{Title: MergeQueuedTitle(name, at), Body: label}
	})
}

// Resolved posts the banner of a merge that left the queue for good, over
// its one-line account. An end outside the three panics here, on the caller's
// goroutine, rather than inside the banner's composition.
func (m *MergeBanners) Resolved(ws ids.WorkspaceID, end MergeEnd, summary string) {
	at := m.now()
	mark, what := mergeEndWords(end)
	m.poster.Post(ws, KindMergeResolved, func(_ context.Context, name string) Banner {
		return Banner{Title: eventTitle(mark, name, what, at), Body: summary}
	})
}

// MergeQueuedTitle is a queued merge's banner title.
func MergeQueuedTitle(name string, at time.Time) string {
	return eventTitle("🔀", name, "merge queued", at)
}

// mergeEndWords is a resolved merge's mark and what happened to it. An end
// outside the three is a defect in the caller, and panics.
func mergeEndWords(end MergeEnd) (mark, what string) {
	switch end {
	case MergeLanded:
		return "🎉", "merge landed"
	case MergeFailed:
		return "❌", "merge failed"
	case MergeAbandoned:
		return "⛔", "merge abandoned"
	}
	panic(fmt.Sprintf("desktopnotify: a merge banner for an unknown end %s", end))
}
