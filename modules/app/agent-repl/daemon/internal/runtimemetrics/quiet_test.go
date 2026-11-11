package runtimemetrics

import "testing"

func TestQuietSeriesAreServed(t *testing.T) {
	// Arrange
	s, _ := openSurface(t)

	// Act
	s.SetLidQuiet(true)
	s.CountAppFrozen("Slack")
	s.CountAppFrozen("Slack")
	s.CountAppThawed("Slack")
	s.CountViewRenderSkipped("sidebar")
	body := scrape(t, s)

	// Assert
	mustContain(t, body,
		`agentrepl_lid_quiet 1`,
		`agentrepl_app_freezes_total{app="Slack"} 2`,
		`agentrepl_app_thaws_total{app="Slack"} 1`,
		`agentrepl_view_renders_skipped_total{view="sidebar"} 1`,
	)
}

func TestTheLidQuietGaugeFallsBackToZero(t *testing.T) {
	// Arrange
	s, _ := openSurface(t)
	s.SetLidQuiet(true)

	// Act
	s.SetLidQuiet(false)
	body := scrape(t, s)

	// Assert
	mustContain(t, body, `agentrepl_lid_quiet 0`)
}

func TestTheOffSurfaceRecordsNoQuietSeries(t *testing.T) {
	// Arrange
	var s *Surface

	// Act and assert: the off surface takes every call and records nothing.
	s.SetLidQuiet(true)
	s.CountAppFrozen("Slack")
	s.CountAppThawed("Slack")
	s.CountViewRenderSkipped("feed")
}
