package classifier

import "testing"

func TestExplicitInterruptRecognizesEveryFastPathWord(t *testing.T) {
	for _, word := range ExplicitInterrupts {
		t.Run(word, func(t *testing.T) {
			// Arrange / Act
			got := ExplicitInterrupt(word)
			// Assert
			if !got {
				t.Fatalf("ExplicitInterrupt(%q) = false, want true", word)
			}
		})
	}
}

func TestExplicitInterruptIgnoresCase(t *testing.T) {
	if !ExplicitInterrupt("STOP") {
		t.Fatal("ExplicitInterrupt(\"STOP\") = false, want true")
	}
}

func TestExplicitInterruptStripsTrailingPunctuation(t *testing.T) {
	if !ExplicitInterrupt("stop!") {
		t.Fatal(`ExplicitInterrupt("stop!") = false, want true`)
	}
}

func TestExplicitInterruptTakesTheFirstWordOfALongerPrompt(t *testing.T) {
	if !ExplicitInterrupt("stop and look at the failing test") {
		t.Fatal("a prompt opening with an interrupt word must take the fast path")
	}
}

func TestExplicitInterruptDeclinesAWordThatIsNotFirst(t *testing.T) {
	if ExplicitInterrupt("do not stop") {
		t.Fatal("an interrupt word that is not first must not take the fast path")
	}
}

func TestExplicitInterruptDeclinesEmptyText(t *testing.T) {
	if ExplicitInterrupt("   ") {
		t.Fatal("blank text must not take the fast path")
	}
}
