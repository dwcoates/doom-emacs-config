package runtimemetrics

import "testing"

func TestTheViewsSkippedSeriesIsServed(t *testing.T) {
	// Arrange
	s, _ := openSurface(t)

	// Act
	s.CountViewRenderSkipped("sidebar")
	body := scrape(t, s)

	// Assert
	mustContain(t, body, `agentrepl_view_renders_skipped_total{view="sidebar"} 1`)
}

func TestTheOffSurfaceRecordsNoViewsSkippedSeries(t *testing.T) {
	// Arrange
	var s *Surface

	// Act and assert: the off surface takes every call and records nothing.
	s.CountViewRenderSkipped("feed")
}
