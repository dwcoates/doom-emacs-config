package paint

import (
	"testing"

	"claude-repld/internal/vocab"
)

// repoVocabDir is the checked-in vocabulary, which is the inventory every
// emitted class here is asserted against.
const repoVocabDir = "../../../proto/vocab"

func loadClasses(t *testing.T) vocab.PaintClasses {
	t.Helper()
	classes, err := vocab.LoadPaintClasses(repoVocabDir)
	if err != nil {
		t.Fatalf("LoadPaintClasses: %v", err)
	}
	return classes
}

func newPainter(t *testing.T) Painter {
	t.Helper()
	p, err := New(loadClasses(t))
	if err != nil {
		t.Fatalf("New: %v", err)
	}
	return p
}

// assertInventory checks the two invariants every painted sequence holds: the
// text is the input verbatim, and every class is in the inventory.
func assertInventory(t *testing.T, classes vocab.PaintClasses, spans Spans, input string) {
	t.Helper()
	if got := spans.Text(); got != input {
		t.Fatalf("concatenated text = %q, want %q", got, input)
	}
	for _, span := range spans {
		if !classes.Contains(span.Class) {
			t.Fatalf("span class %q is not in the inventory", span.Class)
		}
		if span.Text == "" {
			t.Fatalf("an empty span was emitted with class %q", span.Class)
		}
	}
}

func TestNewRefusesAnEmptyInventory(t *testing.T) {
	// Act.
	_, err := New(vocab.PaintClasses{})

	// Assert.
	if err == nil {
		t.Fatal("New accepted an empty inventory")
	}
}

func TestNewRefusesAnUnmodeledPrecedenceSlot(t *testing.T) {
	// Arrange.
	classes := loadClasses(t)
	classes.ANSIPrecedence = []string{"fg", "blink"}

	// Act.
	_, err := New(classes)

	// Assert.
	if err == nil {
		t.Fatal("New accepted a precedence slot the parser does not model")
	}
}

func TestNewRefusesAnInventoryMissingAHighlightClass(t *testing.T) {
	// Arrange: an inventory that lost the keyword class.
	classes := loadClasses(t)
	pruned := make([]string, 0, len(classes.Syntax))
	for _, class := range classes.Syntax {
		if class != "keyword" {
			pruned = append(pruned, class)
		}
	}
	classes.Syntax = pruned

	// Act.
	_, err := New(classes)

	// Assert.
	if err == nil {
		t.Fatal("New accepted an inventory with no keyword class")
	}
}

func TestNewRefusesAnInventoryWithNoPrecedence(t *testing.T) {
	// Arrange.
	classes := loadClasses(t)
	classes.ANSIPrecedence = nil

	// Act.
	_, err := New(classes)

	// Assert.
	if err == nil {
		t.Fatal("New accepted an inventory with no ansi_precedence")
	}
}
