package registry

import "testing"

// Model carries the normalization rather than trusting each call site to
// remember it. These cover the constructor's whole contract.

func TestNewModelPreservesARealID(t *testing.T) {
	// Arrange / Act.
	got := NewModel("claude-opus-5")

	// Assert — a real id is preserved byte-for-byte.
	if got.String() != "claude-opus-5" {
		t.Fatalf("NewModel(real).String() = %q, want it preserved", got.String())
	}
	if got.Empty() {
		t.Fatal("a real model id reports Empty")
	}
}

func TestNewModelNormalizesThePlaceholderAway(t *testing.T) {
	// Arrange — the marker means "pin nothing", and a type whose constructor
	// let it through would put it back on the spawn argv.
	got := NewModel(placeholderModel)

	// Assert.
	if got.String() != "" {
		t.Fatalf("NewModel(placeholder).String() = %q, want empty", got.String())
	}
	if !got.Empty() {
		t.Fatal("the placeholder does not report Empty")
	}
}

func TestNewModelNormalizesASurroundedPlaceholder(t *testing.T) {
	// Arrange — the marker arrives off a vendor stream, where surrounding
	// whitespace is ordinary.
	got := NewModel(" \t" + placeholderModel + "\n")

	// Assert.
	if !got.Empty() {
		t.Fatalf("NewModel(padded placeholder) = %q, want empty", got.String())
	}
}

func TestNewModelKeepsAnEmptyModelEmpty(t *testing.T) {
	// Arrange / Act / Assert — absence is already the canonical form.
	if got := NewModel(""); !got.Empty() || got.String() != "" {
		t.Fatalf("NewModel(\"\") = %q, want empty", got.String())
	}
}

func TestTheZeroModelIsTheHonestAbsence(t *testing.T) {
	// Arrange — the zero value is reachable without the constructor, so it
	// must degrade to the SAFE answer rather than to a wrong model.
	var zero Model

	// Act / Assert.
	if !zero.Empty() || zero.String() != "" {
		t.Fatalf("the zero Model = %q, want the honest absence", zero.String())
	}
}
