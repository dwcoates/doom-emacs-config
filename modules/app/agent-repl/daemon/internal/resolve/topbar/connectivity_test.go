package topbar

import (
	"testing"

	"claude-repld/internal/shimclient"
	"claude-repld/internal/vocab"
)

func TestAnUnobservedLinkIsNoSession(t *testing.T) {
	// Arrange, Act
	got := connectivityKey(false, shimclient.LinkConnected)

	// Assert
	if got != linkNoSession {
		t.Fatalf("key = %q, want no_session: nothing has been asked to connect yet", got)
	}
}

func TestADialingLinkIsConnecting(t *testing.T) {
	// Arrange, Act
	got := connectivityKey(true, shimclient.LinkDialing)

	// Assert
	if got != linkConnecting {
		t.Fatalf("key = %q, want connecting", got)
	}
}

func TestAServingLinkIsConnected(t *testing.T) {
	// Arrange, Act
	got := connectivityKey(true, shimclient.LinkConnected)

	// Assert
	if got != linkConnected {
		t.Fatalf("key = %q, want connected", got)
	}
}

func TestARedialingLinkIsSevered(t *testing.T) {
	// Arrange, Act
	got := connectivityKey(true, shimclient.LinkRedialing)

	// Assert
	if got != linkSevered {
		t.Fatalf("key = %q, want severed: a retry in progress and a broken route read alike", got)
	}
}

func TestADeadLinkIsDead(t *testing.T) {
	// Arrange, Act
	got := connectivityKey(true, shimclient.LinkDead)

	// Assert
	if got != linkDead {
		t.Fatalf("key = %q, want dead", got)
	}
}

func TestTheToneComesFromTheVocabulary(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.ready(t)

	// Act
	h.r.OnLink(testWS, shimclient.LinkConnected)

	// Assert
	got := h.view(t).GetConnectivity()
	if got.GetTone() != "green" {
		t.Fatalf("tone = %q, want the vocabulary's green for a serving route", got.GetTone())
	}
	if got.GetGlyph() == "" || got.GetTitle() == "" {
		t.Fatalf("connectivity = %+v, want a literal glyph and a tooltip", got)
	}
}

func TestACompromisedRouteTakesTheVocabularysBlue(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.ready(t)

	// Act
	h.r.OnLink(testWS, shimclient.LinkRedialing)

	// Assert
	if got := h.view(t).GetConnectivity().GetTone(); got != "blue" {
		t.Fatalf("tone = %q, want blue for a compromised route", got)
	}
}

func TestNoSessionTakesTheVocabularysNoneTone(t *testing.T) {
	// Arrange
	h := newHarness(t)

	// Act
	h.ready(t)

	// Assert
	if got := h.view(t).GetConnectivity().GetTone(); got != "none" {
		t.Fatalf("tone = %q, want none when there is no route to have", got)
	}
}

func TestEachLinkStateDrawsItsOwnGlyph(t *testing.T) {
	// Arrange
	keys := []string{linkNoSession, linkConnecting, linkConnected, linkSevered, linkDead}
	seen := map[string]string{}

	// Act
	for _, key := range keys {
		seen[connectivityGlyph(key)] = key
	}

	// Assert
	if len(seen) != len(keys) {
		t.Fatalf("glyphs = %+v, want one distinct glyph per link state", seen)
	}
}

func TestAMissingConnectivityRowIsRefused(t *testing.T) {
	// Arrange
	colors := testColors()
	delete(colors.TopbarConnectivity, linkConnected)
	r, err := newResolver(colors, testSurfaces(t), WithClock(fixedClock{now: instant}))
	if err != nil {
		t.Fatalf("newResolver: %v", err)
	}

	// Act
	_, gotErr := r.connectivity(linkConnected)

	// Assert
	if gotErr == nil {
		t.Fatalf("a link state with no color row resolved; the daemon refuses an unpainted state")
	}
}

func TestAToneOutsideTheClosedSetIsRefused(t *testing.T) {
	// Arrange
	colors := testColors()
	colors.TopbarConnectivity[linkConnected] = "teal"
	r, err := newResolver(colors, testSurfaces(t), WithClock(fixedClock{now: instant}))
	if err != nil {
		t.Fatalf("newResolver: %v", err)
	}

	// Act
	_, gotErr := r.connectivity(linkConnected)

	// Assert
	if gotErr == nil {
		t.Fatalf("a tone outside topbar_tones resolved; the closed set is what a consumer validates against")
	}
}

func TestAnUnpaintedStateIsRecordedAndNothingIsPublished(t *testing.T) {
	// Arrange
	colors := vocab.RenderColors{
		TopbarConnectivity: map[string]string{linkNoSession: "none"},
		TopbarTones:        []string{"none"},
	}
	log := testSurfaces(t)
	r, err := newResolver(colors, log, WithClock(fixedClock{now: instant}))
	if err != nil {
		t.Fatalf("newResolver: %v", err)
	}
	if err := r.SetWorkspaceDir(testWS, t.TempDir()); err != nil {
		t.Fatalf("SetWorkspaceDir: %v", err)
	}

	// Act
	r.OnLink(testWS, shimclient.LinkConnected)

	// Assert
	if _, ok := r.Topic(testWS).Latest(); ok {
		t.Fatalf("a view was published for an unpainted state")
	}
	records := log.Records()
	if len(records) == 0 || records[len(records)-1].Level != "error" {
		t.Fatalf("records = %+v, want the failure recorded at ERROR", records)
	}
}
