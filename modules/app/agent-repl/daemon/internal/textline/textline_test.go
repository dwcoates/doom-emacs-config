package textline

import (
	"reflect"
	"testing"
)

func TestLines(t *testing.T) {
	tests := []struct {
		name string
		text string
		want []string
	}{
		{name: "the empty text has no lines", text: "", want: nil},
		{name: "a final newline ends the last line", text: "a\nb\n", want: []string{"a", "b"}},
		{name: "an unterminated last line is still a line", text: "a\nb", want: []string{"a", "b"}},
		{name: "a lone newline is one empty line", text: "\n", want: []string{""}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act.
			got := Lines(tt.text)

			// Assert.
			if !reflect.DeepEqual(got, tt.want) {
				t.Fatalf("Lines(%q) = %q, want %q", tt.text, got, tt.want)
			}
		})
	}
}

func TestFirstNonBlank(t *testing.T) {
	tests := []struct {
		name string
		text string
		want string
	}{
		{name: "one line", text: "hello", want: "hello"},
		{name: "leading blank lines are skipped", text: "\n \n  hello  \nworld", want: "hello"},
		{name: "later lines are dropped", text: "first\nsecond", want: "first"},
		{name: "nothing but blanks", text: " \n\t\n", want: ""},
		{name: "empty text", text: "", want: ""},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			got := FirstNonBlank(tt.text)

			// Assert
			if got != tt.want {
				t.Fatalf("FirstNonBlank(%q) = %q, want %q", tt.text, got, tt.want)
			}
		})
	}
}

func TestOneSentenceAcceptsOneLineOfOneSentence(t *testing.T) {
	tests := []struct {
		name string
		text string
	}{
		{name: "at the fewest words", text: "Design doc"},
		{name: "at the most words", text: "one two three four"},
		{name: "a closing period", text: "The design doc."},
		{name: "a dotted word inside", text: "The v1.2 notes"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange, Act
			err := OneSentence("label", tt.text, 2, 4)

			// Assert
			if err != nil {
				t.Fatalf("OneSentence(%q) = %v, want nil", tt.text, err)
			}
		})
	}
}

func TestOneSentenceNamesWhyATextIsRefused(t *testing.T) {
	tests := []struct {
		name string
		text string
		want string
	}{
		{name: "too few words", text: "Doc", want: "a label is 2 to 4 words, not 1"},
		{name: "too many words", text: "one two three four five", want: "a label is 2 to 4 words, not 5"},
		{name: "a line break", text: "Design\ndoc", want: "a label is one line"},
		{name: "a carriage return", text: "Design\rdoc", want: "a label is one line"},
		{name: "surrounding whitespace", text: "Design doc ", want: "a label carries no whitespace at either end"},
		{name: "a second sentence", text: "Why? Because", want: `a label is one sentence, and "Why?" ends one before its last word`},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange, Act
			err := OneSentence("label", tt.text, 2, 4)

			// Assert
			if err == nil || err.Error() != tt.want {
				t.Fatalf("OneSentence(%q) = %v, want %q", tt.text, err, tt.want)
			}
		})
	}
}

func TestParagraphAcceptsOneLineOfItsSentences(t *testing.T) {
	tests := []struct {
		name string
		text string
	}{
		{name: "at the fewest sentences", text: "Fix the backoff. It retries."},
		{name: "at the most sentences", text: "One. Two. Three!"},
		{name: "at the most words", text: "One two three. Four five six."},
		{name: "a dotted word inside", text: "Ship v1.2 now. Then stop."},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange, Act
			err := Paragraph("long summary", tt.text, 2, 3, 6)

			// Assert
			if err != nil {
				t.Fatalf("Paragraph(%q) = %v, want nil", tt.text, err)
			}
		})
	}
}

func TestParagraphNamesWhyATextIsRefused(t *testing.T) {
	tests := []struct {
		name string
		text string
		want string
	}{
		{name: "too few sentences", text: "Fix the backoff.", want: "a long summary is 2 to 3 sentences, not 1"},
		{name: "too many sentences", text: "One. Two. Three. Four.", want: "a long summary is 2 to 3 sentences, not 4"},
		{name: "too many words", text: "One two three. Four five six seven.", want: "a long summary is at most 6 words, not 7"},
		{name: "no closing sentence end", text: "Fix it. Then stop", want: "a long summary ends its last sentence with its last word"},
		{name: "a line break", text: "Fix it.\nThen stop.", want: "a long summary is one paragraph, with no line break"},
		{name: "surrounding whitespace", text: " Fix it. Then stop.", want: "a long summary carries no whitespace at either end"},
		{name: "empty", text: "", want: "a long summary is never empty"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange, Act
			err := Paragraph("long summary", tt.text, 2, 3, 6)

			// Assert
			if err == nil || err.Error() != tt.want {
				t.Fatalf("Paragraph(%q) = %v, want %q", tt.text, err, tt.want)
			}
		})
	}
}

func TestTrimmedLineIsTheSharedFirstCheck(t *testing.T) {
	tests := []struct {
		name  string
		check func(text string) error
	}{
		{name: "OneSentence", check: func(text string) error { return OneSentence("text", text, 1, 9) }},
		{name: "Paragraph", check: func(text string) error { return Paragraph("text", text, 1, 9, 9) }},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			text := " Padded."

			// Act
			err := tt.check(text)

			// Assert
			if want := trimmedLine("text", text, "unused"); err == nil || err.Error() != want.Error() {
				t.Fatalf("%s(%q) = %v, want the shared refusal %v", tt.name, text, err, want)
			}
		})
	}
}
