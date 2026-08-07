package server

import (
	"context"
	"errors"
	"strings"
	"testing"

	"claude-repld/internal/errclass"
	"claude-repld/internal/registry"
)

// ---------------------------------------------------------------------------
// THE FRESH GATE. A blank conversation may be started for exactly one kind of
// workspace: one the daemon can PROVE never had one. These tests pin both
// halves — that the proof is granted where it must be, and that its absence is
// a hard fault rather than a quiet blank slate.
// ---------------------------------------------------------------------------

// evidenceOver builds a registry from recs and reads the workspace's evidence.
func evidenceOver(t *testing.T, recs []registry.Record) conversationEvidence {
	t.Helper()
	reg := openTestRegistry(t)
	for _, rec := range recs {
		if err := reg.Put(rec); err != nil {
			t.Fatalf("put %s: %v", rec.SessionID, err)
		}
	}
	return gatherConversationEvidence(reg, "/cfg", "/w")
}

func TestConversationEvidence(t *testing.T) {
	tests := []struct {
		name    string
		records []registry.Record
		want    conversationEvidence
	}{
		{
			name:    "a workspace with no records at all has never had a conversation",
			records: nil,
			want:    conversationEvidence{available: true},
		},
		{
			name: "a record that ran a turn is proof the workspace HAS a conversation",
			records: []registry.Record{
				{SessionID: "s1", CWD: "/w", ConfigDir: "/cfg", ClaudeSessionID: "uuid-1", LastTurnEndMs: 1786117000000},
			},
			want: conversationEvidence{available: true, everRan: true, conversations: []string{"uuid-1"}},
		},
		{
			name: "a handshake-only record names a uuid but proves no conversation",
			records: []registry.Record{
				{SessionID: "s1", CWD: "/w", ConfigDir: "/cfg", ClaudeSessionID: "uuid-1"},
			},
			want: conversationEvidence{available: true, conversations: []string{"uuid-1"}},
		},
		{
			name: "one record that spoke outvotes a later silent one for the same workspace",
			records: []registry.Record{
				{SessionID: "s1", CWD: "/w", ConfigDir: "/cfg", ClaudeSessionID: "uuid-1", LastTurnEndMs: 1786117000000},
				{SessionID: "s2", CWD: "/w", ConfigDir: "/cfg", ClaudeSessionID: "uuid-2"},
			},
			want: conversationEvidence{available: true, everRan: true, conversations: []string{"uuid-1", "uuid-2"}},
		},
		{
			name: "another workspace's conversation is not this workspace's evidence",
			records: []registry.Record{
				{SessionID: "s1", CWD: "/other", ConfigDir: "/cfg", ClaudeSessionID: "uuid-1", LastTurnEndMs: 1786117000000},
			},
			want: conversationEvidence{available: true},
		},
		{
			name: "another ACCOUNT's conversation at the same cwd is not this one's evidence",
			records: []registry.Record{
				{SessionID: "s1", CWD: "/w", ConfigDir: "/other-cfg", ClaudeSessionID: "uuid-1", LastTurnEndMs: 1786117000000},
			},
			want: conversationEvidence{available: true},
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange + Act.
			got := evidenceOver(t, tt.records)

			// Assert.
			if got.available != tt.want.available || got.everRan != tt.want.everRan {
				t.Fatalf("evidence = %+v, want %+v", got, tt.want)
			}
			if len(got.conversations) != len(tt.want.conversations) {
				t.Fatalf("conversations = %v, want %v", got.conversations, tt.want.conversations)
			}
			for i, uuid := range tt.want.conversations {
				if got.conversations[i] != uuid {
					t.Fatalf("conversations = %v, want %v", got.conversations, tt.want.conversations)
				}
			}
		})
	}
}

// TestAnUnreadableRegistryIsNotEvidenceOfAbsence: "I cannot tell" must never be
// spent as permission to destroy a conversation.
func TestAnUnreadableRegistryIsNotEvidenceOfAbsence(t *testing.T) {
	// Arrange + Act.
	got := gatherConversationEvidence(nil, "/cfg", "/w")

	// Assert.
	if got.available {
		t.Fatalf("evidence = %+v, want unavailable for a nil registry", got)
	}
}

func TestProveFreshEligible(t *testing.T) {
	tests := []struct {
		name      string
		evidence  conversationEvidence
		wantProof bool
	}{
		{
			name:      "a workspace proven never to have run a conversation may start fresh",
			evidence:  conversationEvidence{available: true},
			wantProof: true,
		},
		{
			name:      "a workspace that has run a conversation may NOT start fresh",
			evidence:  conversationEvidence{available: true, everRan: true, conversations: []string{"uuid-1"}},
			wantProof: false,
		},
		{
			name:      "unconsultable evidence yields no proof",
			evidence:  conversationEvidence{},
			wantProof: false,
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange + Act.
			proof, reason := proveFreshEligible(tt.evidence)

			// Assert.
			if (proof != nil) != tt.wantProof {
				t.Fatalf("proof = %v (reason %q), want proof=%v", proof, reason, tt.wantProof)
			}
			if proof == nil && reason == "" {
				t.Fatal("a refused proof must carry a reason a human can act on")
			}
		})
	}
}

// TestTheNoResumeSpawnRefusesWithoutAProof is the structural claim: the spawn
// site itself, not merely its callers, is what enforces the rule. A future rung
// that forgets to mint a proof fails loudly instead of starting a blank
// conversation.
func TestTheNoResumeSpawnRefusesWithoutAProof(t *testing.T) {
	// Arrange.
	spawned := 0
	sp := NewShimSpawner(openTestRegistry(t),
		func(string) (bool, error) { return false, nil }, nil,
		func(string, CreateOpts) (ShimHandle, error) { spawned++; return ShimHandle{}, nil },
		func(string, ...any) {})

	// Act.
	_, err := sp.spawnShim("s1", CreateOpts{CWD: "/w"}, nil)

	// Assert.
	if !errors.Is(err, errclass.ErrConversationUnresumable) {
		t.Fatalf("err = %v, want errclass.ErrConversationUnresumable", err)
	}
	if spawned != 0 {
		t.Fatalf("an unproven no-resume spawn launched %d shims, want none", spawned)
	}
}

// TestTheNoResumeSpawnProceedsWithAProof: the gate must not be a wall. A proof
// minted from real evidence is spendable.
func TestTheNoResumeSpawnProceedsWithAProof(t *testing.T) {
	// Arrange.
	spawned := 0
	sp := NewShimSpawner(openTestRegistry(t),
		func(string) (bool, error) { return false, nil }, nil,
		func(string, CreateOpts) (ShimHandle, error) { spawned++; return ShimHandle{}, nil },
		func(string, ...any) {})
	proof, reason := proveFreshEligible(conversationEvidence{available: true})
	if proof == nil {
		t.Fatalf("evidence of a never-used workspace refused a proof: %s", reason)
	}

	// Act.
	if _, err := sp.spawnShim("s1", CreateOpts{CWD: "/w"}, proof); err != nil {
		t.Fatalf("spawnShim with a proof: %v", err)
	}

	// Assert.
	if spawned != 1 {
		t.Fatalf("spawned = %d, want exactly one", spawned)
	}
}

// TestANeverUsedWorkspaceSpawnsFresh is the brand-new-workspace path
// workspace-create depends on: a record with no conversation pointer, in a
// workspace with no history, must come up.
func TestANeverUsedWorkspaceSpawnsFresh(t *testing.T) {
	// Arrange.
	reg := openTestRegistry(t)
	if err := reg.Put(registry.Record{SessionID: "s1", CWD: "/w", ConfigDir: "/cfg"}); err != nil {
		t.Fatalf("put: %v", err)
	}
	var got CreateOpts
	spawned := 0
	sp := NewShimSpawner(reg,
		func(string) (bool, error) { return false, nil }, nil,
		func(_ string, opts CreateOpts) (ShimHandle, error) { spawned++; got = opts; return ShimHandle{}, nil },
		func(string, ...any) {})

	// Act.
	_, err := sp.EnsureShim(context.Background(), "s1")

	// Assert.
	if err != nil {
		t.Fatalf("EnsureShim for a brand-new workspace: %v", err)
	}
	if spawned != 1 || got.Resume != "" {
		t.Fatalf("spawned=%d resume=%q, want one fresh spawn", spawned, got.Resume)
	}
}

// TestARecordWithNoConversationInAWorkspaceThatHasOneIsRefused is the case the
// ruling exists for: the record itself names nothing, but the workspace has
// been talking, and bringing it up blank would orphan that conversation.
func TestARecordWithNoConversationInAWorkspaceThatHasOneIsRefused(t *testing.T) {
	// Arrange.
	reg := openTestRegistry(t)
	if err := reg.Put(registry.Record{
		SessionID: "s-old", CWD: "/w", ConfigDir: "/cfg",
		ClaudeSessionID: "uuid-live", LastTurnEndMs: 1786117000000,
	}); err != nil {
		t.Fatalf("put old: %v", err)
	}
	if err := reg.Put(registry.Record{SessionID: "s-new", CWD: "/w", ConfigDir: "/cfg"}); err != nil {
		t.Fatalf("put new: %v", err)
	}
	spawned := 0
	sp := NewShimSpawner(reg,
		func(string) (bool, error) { return false, nil }, nil,
		func(string, CreateOpts) (ShimHandle, error) { spawned++; return ShimHandle{}, nil },
		func(string, ...any) {})

	// Act.
	_, err := sp.EnsureShim(context.Background(), "s-new")

	// Assert.
	if !errors.Is(err, errclass.ErrConversationUnresumable) {
		t.Fatalf("err = %v, want errclass.ErrConversationUnresumable", err)
	}
	if spawned != 0 {
		t.Fatalf("a blank spawn over an existing conversation launched %d shims", spawned)
	}
}

// TestTheRefusalNamesTheConversationItProtects: a refusal that names nothing
// leaves the user with no way to recover the conversation it is protecting.
func TestTheRefusalNamesTheConversationItProtects(t *testing.T) {
	// Arrange.
	reg := openTestRegistry(t)
	if err := reg.Put(registry.Record{
		SessionID: "s-old", CWD: "/w", ConfigDir: "/cfg",
		ClaudeSessionID: "uuid-live", LastTurnEndMs: 1786117000000,
	}); err != nil {
		t.Fatalf("put old: %v", err)
	}
	if err := reg.Put(registry.Record{SessionID: "s-new", CWD: "/w", ConfigDir: "/cfg"}); err != nil {
		t.Fatalf("put new: %v", err)
	}
	sp := NewShimSpawner(reg,
		func(string) (bool, error) { return false, nil }, nil,
		func(string, CreateOpts) (ShimHandle, error) { return ShimHandle{}, nil },
		func(string, ...any) {})

	// Act.
	_, err := sp.EnsureShim(context.Background(), "s-new")

	// Assert.
	if err == nil || !strings.Contains(err.Error(), "uuid-live") {
		t.Fatalf("err = %v, want the protected conversation named", err)
	}
}
