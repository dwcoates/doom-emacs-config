package server

import (
	"strings"
	"testing"
)

func TestMintReturnsADistinctTokenEachTime(t *testing.T) {
	// Arrange.
	registry := newTokenRegistry()

	// Act.
	first, err := registry.mint("a1", 1)
	if err != nil {
		t.Fatalf("mint = %v, want nil", err)
	}
	second, err := registry.mint("a1", 1)
	if err != nil {
		t.Fatalf("mint = %v, want nil", err)
	}

	// Assert.
	if first == second {
		t.Fatalf("two mints returned the same token %q", first)
	}
}

func TestMintBindsTheBookAndThePin(t *testing.T) {
	// Arrange.
	registry := newTokenRegistry()
	token, err := registry.mint("a1", 42)
	if err != nil {
		t.Fatalf("mint = %v, want nil", err)
	}

	// Act.
	entry, ok := registry.consume(token)

	// Assert.
	if !ok || entry.agentID != "a1" || entry.pinSeq != 42 {
		t.Fatalf("entry = %+v (ok %t), want {a1 42}", entry, ok)
	}
}

func TestConsumeRetiresTheToken(t *testing.T) {
	// Arrange. A token is a capability spent by exactly one watch.
	registry := newTokenRegistry()
	token, err := registry.mint("a1", 1)
	if err != nil {
		t.Fatalf("mint = %v, want nil", err)
	}
	if _, ok := registry.consume(token); !ok {
		t.Fatal("the first consume failed")
	}

	// Act.
	_, ok := registry.consume(token)

	// Assert.
	if ok {
		t.Fatal("the second consume succeeded, want a single-use token")
	}
}

func TestConsumeRefusesATokenThatWasNeverMinted(t *testing.T) {
	// Arrange.
	registry := newTokenRegistry()

	// Act.
	_, ok := registry.consume("deadbeef")

	// Assert.
	if ok {
		t.Fatal("consume of an unminted token succeeded, want a refusal")
	}
}

func TestOutstandingCountsUnspentTokens(t *testing.T) {
	// Arrange.
	registry := newTokenRegistry()
	if _, err := registry.mint("a1", 1); err != nil {
		t.Fatalf("mint = %v, want nil", err)
	}
	token, err := registry.mint("a2", 1)
	if err != nil {
		t.Fatalf("mint = %v, want nil", err)
	}

	// Act.
	registry.consume(token)

	// Assert.
	if got := registry.outstanding(); got != 1 {
		t.Fatalf("outstanding = %d, want 1", got)
	}
}

func TestTokenHashNeverDisclosesTheToken(t *testing.T) {
	// Arrange. A log is not a place to publish a capability.
	token := "0123456789abcdef0123456789abcdef"

	// Act.
	hash := tokenHash(token)

	// Assert.
	if strings.Contains(hash, token) || strings.Contains(token, hash) {
		t.Fatalf("hash %q discloses the token %q", hash, token)
	}
}

func TestTokenHashIsStableForOneToken(t *testing.T) {
	// Arrange. The same watch must be greppable across its records.
	token := "0123456789abcdef0123456789abcdef"

	// Act.
	first, second := tokenHash(token), tokenHash(token)

	// Assert.
	if first != second {
		t.Fatalf("hash = %q then %q, want a stable value", first, second)
	}
}
