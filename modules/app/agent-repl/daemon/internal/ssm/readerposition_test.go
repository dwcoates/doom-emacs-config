package ssm

import (
	"path/filepath"
	"testing"
)

// THE POSITION IS THE DAEMON'S, and these cases pin the four properties the
// pagination contract stands on: it is keyed per reader per workspace, absence
// is a distinct answer rather than a zero, a first page REPLACES it, and no
// stored place survives the daemon that recorded it.

// openPositions opens a manager over a temporary state store.
func openPositions(t *testing.T, path string) *Manager {
	t.Helper()
	m, err := Open(Options{DBPath: path, Logf: func(string, ...any) {}, Resolver: fakeResolver{}})
	if err != nil {
		t.Fatalf("Open: %v", err)
	}
	t.Cleanup(func() { m.Close() })
	return m
}

func TestAnUnestablishedReaderPositionReportsAbsenceRatherThanZero(t *testing.T) {
	// Arrange — zero is a legal bound and would be SERVED, so "no position" has
	// to be a distinct answer or a next page from a fresh reader reads the
	// floor instead of being refused.
	m := openPositions(t, filepath.Join(t.TempDir(), "state.db"))

	// Act.
	_, found, err := m.ConversationReaderPosition("conn-1", "/ws")

	// Assert.
	if err != nil {
		t.Fatalf("ConversationReaderPosition: %v", err)
	}
	if found {
		t.Fatal("a reader that never read reported an established position")
	}
}

func TestAReaderPositionIsKeyedPerReaderPerWorkspace(t *testing.T) {
	// Arrange — one reader's place in one workspace must say nothing about
	// another reader, or two tabs scrolled to different depths read each
	// other's history.
	m := openPositions(t, filepath.Join(t.TempDir(), "state.db"))
	if err := m.SetConversationReaderPosition("conn-1", "/ws", "g1", 500); err != nil {
		t.Fatalf("SetConversationReaderPosition: %v", err)
	}

	// Act.
	_, found, err := m.ConversationReaderPosition("conn-2", "/ws")

	// Assert.
	if err != nil {
		t.Fatalf("ConversationReaderPosition: %v", err)
	}
	if found {
		t.Fatal("one reader's position was visible to another")
	}
}

func TestWritingAReaderPositionReplacesTheOneItHeld(t *testing.T) {
	// Arrange — a reader paged back to seq 500 and then asked for a first page.
	// A second row rather than a replacement would leave two answers to where
	// one reader is.
	m := openPositions(t, filepath.Join(t.TempDir(), "state.db"))
	if err := m.SetConversationReaderPosition("conn-1", "/ws", "g1", 500); err != nil {
		t.Fatalf("SetConversationReaderPosition: %v", err)
	}

	// Act.
	if err := m.SetConversationReaderPosition("conn-1", "/ws", "g1", 900); err != nil {
		t.Fatalf("SetConversationReaderPosition: %v", err)
	}

	// Assert.
	got, found, err := m.ConversationReaderPosition("conn-1", "/ws")
	if err != nil || !found {
		t.Fatalf("ConversationReaderPosition: found=%t err=%v", found, err)
	}
	if got.BeforeSeq != 900 {
		t.Fatalf("before_seq = %d, want the replacement 900", got.BeforeSeq)
	}
}

func TestAReaderPositionCarriesTheGenerationItWasEstablishedUnder(t *testing.T) {
	// Arrange — the generation is stored WITH the position because it is what
	// replaces a fence: nothing is handed to the client to be handed back.
	m := openPositions(t, filepath.Join(t.TempDir(), "state.db"))
	if err := m.SetConversationReaderPosition("conn-1", "/ws", "g1", 500); err != nil {
		t.Fatalf("SetConversationReaderPosition: %v", err)
	}

	// Act.
	got, _, err := m.ConversationReaderPosition("conn-1", "/ws")

	// Assert.
	if err != nil {
		t.Fatalf("ConversationReaderPosition: %v", err)
	}
	if got.GenerationID != "g1" {
		t.Fatalf("generation = %q, want g1", got.GenerationID)
	}
}

func TestADroppedReaderPositionIsGone(t *testing.T) {
	// Arrange — a rotation drops the place so the next NextPageCmd is refused.
	// A drop that merely marked the row would leave it findable and served.
	m := openPositions(t, filepath.Join(t.TempDir(), "state.db"))
	if err := m.SetConversationReaderPosition("conn-1", "/ws", "g1", 500); err != nil {
		t.Fatalf("SetConversationReaderPosition: %v", err)
	}

	// Act.
	if err := m.DropConversationReaderPosition("conn-1", "/ws"); err != nil {
		t.Fatalf("DropConversationReaderPosition: %v", err)
	}

	// Assert.
	if _, found, _ := m.ConversationReaderPosition("conn-1", "/ws"); found {
		t.Fatal("a dropped position was still established")
	}
}

func TestNoReaderPositionSurvivesTheDaemonThatRecordedIt(t *testing.T) {
	// Arrange — a reader is a live frontend connection and connection ids
	// restart, so a surviving row would be inherited by an unrelated later
	// connection and turn its cold open into a silent read of someone's place.
	path := filepath.Join(t.TempDir(), "state.db")
	first := openPositions(t, path)
	if err := first.SetConversationReaderPosition("conn-1", "/ws", "g1", 500); err != nil {
		t.Fatalf("SetConversationReaderPosition: %v", err)
	}
	if err := first.Close(); err != nil {
		t.Fatalf("Close: %v", err)
	}

	// Act — the daemon comes back.
	second := openPositions(t, path)

	// Assert.
	if _, found, _ := second.ConversationReaderPosition("conn-1", "/ws"); found {
		t.Fatal("a reading position outlived the daemon that recorded it")
	}
}

func TestAReaderPositionWithNoReaderIsRefused(t *testing.T) {
	// Arrange — a position is per reader per workspace, so an empty reader key
	// is a row the next unidentified reader would inherit.
	m := openPositions(t, filepath.Join(t.TempDir(), "state.db"))

	// Act.
	err := m.SetConversationReaderPosition("", "/ws", "g1", 500)

	// Assert.
	if err == nil {
		t.Fatal("a position was filed under an empty reader")
	}
}
