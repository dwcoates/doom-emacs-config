package wsm

import (
	"context"
	"testing"
)

func TestAComposerDraftRoundTrips(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)

	// Act
	if err := s.PutComposerDraft(context.Background(), ws.ID, "half a thought"); err != nil {
		t.Fatalf("PutComposerDraft: %v", err)
	}
	got, found, err := s.ComposerDraft(context.Background(), ws.ID)

	// Assert
	if err != nil || !found || got != "half a thought" {
		t.Fatalf("ComposerDraft = %q, %v, %v; want the kept draft", got, found, err)
	}
}

func TestNoComposerDraftIsKeptUntilOneIsPut(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)

	// Act
	got, found, err := s.ComposerDraft(context.Background(), ws.ID)

	// Assert
	if err != nil || found || got != "" {
		t.Fatalf("ComposerDraft = %q, %v, %v; want none kept", got, found, err)
	}
}

func TestAComposerDraftReplacesTheOneKept(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)
	if err := s.PutComposerDraft(context.Background(), ws.ID, "first"); err != nil {
		t.Fatalf("seed: %v", err)
	}

	// Act
	if err := s.PutComposerDraft(context.Background(), ws.ID, "second"); err != nil {
		t.Fatalf("PutComposerDraft: %v", err)
	}

	// Assert
	got, _, err := s.ComposerDraft(context.Background(), ws.ID)
	if err != nil || got != "second" {
		t.Fatalf("ComposerDraft = %q, %v; want the replacement", got, err)
	}
	if n := scalar[int](t, s, `SELECT count(*) FROM composer_drafts`); n != 1 {
		t.Fatalf("%d drafts are kept, want exactly 1", n)
	}
}

func TestClearingAComposerDraftForgetsIt(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)
	if err := s.PutComposerDraft(context.Background(), ws.ID, "gone soon"); err != nil {
		t.Fatalf("seed: %v", err)
	}

	// Act
	if err := s.ClearComposerDraft(context.Background(), ws.ID); err != nil {
		t.Fatalf("ClearComposerDraft: %v", err)
	}

	// Assert
	if _, found, err := s.ComposerDraft(context.Background(), ws.ID); err != nil || found {
		t.Fatalf("ComposerDraft found = %v, %v; want none kept", found, err)
	}
}

func TestClearingAWorkspaceThatKeepsNoDraftChangesNothing(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)

	// Act
	err := s.ClearComposerDraft(context.Background(), ws.ID)

	// Assert
	if err != nil {
		t.Fatalf("ClearComposerDraft: %v", err)
	}
}

func TestAnEmptyComposerDraftIsRefused(t *testing.T) {
	// Arrange
	s, log := testStore(t)
	ws := testWorkspace(t, s)

	// Act
	err := s.PutComposerDraft(context.Background(), ws.ID, "")

	// Assert
	if err == nil {
		t.Fatal("PutComposerDraft kept an empty draft")
	}
	if !loggedOperation(log, "daemon.wsm.put_composer_draft", "error") {
		t.Fatalf("the refusal was not recorded at ERROR: %v", log.Records())
	}
	if n := scalar[int](t, s, `SELECT count(*) FROM composer_drafts`); n != 0 {
		t.Fatalf("%d drafts are kept after the refusal, want none", n)
	}
}
