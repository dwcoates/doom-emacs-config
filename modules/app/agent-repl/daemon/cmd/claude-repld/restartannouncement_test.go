package main

import (
	"bytes"
	"io"
	"strings"
	"testing"

	"claude-repld/internal/dlog"
	"claude-repld/internal/sessioncontroller"
)

// The announcement path must survive the gated-proto state: with no arm to
// ride on there is no sink, and the shutdown still proceeds — loudly.
func TestAnnounceIntentionalRestartRecordsAnUndeliveredAnnouncement(t *testing.T) {
	// Arrange.
	var durable bytes.Buffer
	logger := dlog.New(&durable, io.Discard, false)

	// Act.
	announceIntentionalRestart(logger, shutdownRequest{
		stopShims: true,
		cause:     sessioncontroller.StopCauseDaemonShutdown(),
	})

	// Assert.
	if !strings.Contains(durable.String(), "NOT ANNOUNCED") {
		t.Fatalf("want the undelivered announcement recorded, got %q", durable.String())
	}
}

func TestRestartAnnouncementSinksAreEmptyUntilTheProtoArmLands(t *testing.T) {
	// Arrange / Act.
	sinks := restartAnnouncementSinks()

	// Assert.
	if len(sinks) != 0 {
		t.Fatalf("no wire arm exists yet, so no sink may claim to deliver; got %d", len(sinks))
	}
}
