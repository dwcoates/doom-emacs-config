package deployprogress

import "sync"

// Edges is a Sink that passes every progress on to INNER and tells the two
// edges of a deploy: Began on the edge INTO Building (the deploy committed:
// it builds from here), and Ended on the edge that closes the deploy's
// story -- Updated (it finished, here or in the successor that took over) or
// the line taken down (it failed, or its rollout cannot finish), with
// whether it changed nothing (Progress.Unchanged). An edge is told once per
// deploy, whatever progress repeats it.
type Edges struct {
	inner Sink
	began func()
	ended func(Ending)

	mu sync.Mutex
	// open reports a deploy that began and has not ended.
	open bool
}

// Ending is how a deploy's story closed, as Ended is told it.
type Ending int

const (
	// EndedFinished: it reached Updated with something changed.
	EndedFinished Ending = iota + 1
	// EndedUnchanged: it reached Updated with nothing changed.
	EndedUnchanged
	// EndedStoppedShort: its line was taken down before Updated: it failed,
	// or its rollout could not finish.
	EndedStoppedShort
)

// endingOf is how PROGRESS, an ending progress, closed the deploy.
func endingOf(progress *Progress) Ending {
	switch {
	case progress == nil:
		return EndedStoppedShort
	case progress.Unchanged:
		return EndedUnchanged
	default:
		return EndedFinished
	}
}

// NewEdges wraps INNER, telling BEGAN and ENDED of each deploy's edges.
func NewEdges(inner Sink, began func(), ended func(Ending)) *Edges {
	if inner == nil || began == nil || ended == nil {
		panic("deployprogress: NewEdges requires a sink and both edges")
	}
	return &Edges{inner: inner, began: began, ended: ended}
}

// SetDeployProgress implements Sink.
func (e *Edges) SetDeployProgress(progress *Progress) {
	e.inner.SetDeployProgress(progress)
	e.mu.Lock()
	began := progress != nil && progress.Phase == Building && !e.open
	ended := progress == nil || progress.Phase == Updated
	switch {
	case began:
		e.open = true
	case ended:
		e.open = false
	}
	e.mu.Unlock()
	if began {
		e.began()
	}
	if ended {
		e.ended(endingOf(progress))
	}
}
