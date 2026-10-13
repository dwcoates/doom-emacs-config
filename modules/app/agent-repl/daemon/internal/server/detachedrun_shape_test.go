package server

import (
	"testing"

	"agent-repld/internal/sourcescan"
)

func TestEveryAsyncRunIsBuiltByNewDetachedRun(t *testing.T) {
	// Act: the package's own production sources.
	n := sourcescan.Count(t, "merge.DetachedRun{")

	// Assert: the async run is read off a request in merge.NewDetachedRun
	// alone, so this ingress cannot read it apart from the other.
	if n != 0 {
		t.Fatalf("%d hand-built merge.DetachedRun literals, want every run built by merge.NewDetachedRun", n)
	}
}
