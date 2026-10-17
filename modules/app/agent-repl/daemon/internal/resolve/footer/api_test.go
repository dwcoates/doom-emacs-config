package footer

import "testing"

func TestFiguredLineTextIsExactlyItsParts(t *testing.T) {
	tests := []struct {
		name string
		line FiguredLine
		want string
	}{
		{
			name: "lead, figure and tail in order",
			line: FiguredLine{Lead: "the conversation is cold at ", Figure: "409,051", Tail: " context tokens"},
			want: "the conversation is cold at 409,051 context tokens",
		},
		{
			name: "a figure that ends the line",
			line: FiguredLine{Lead: "spent ", Figure: "104.3k"},
			want: "spent 104.3k",
		},
		{
			name: "a figure that opens the line",
			line: FiguredLine{Figure: "12", Tail: " tokens"},
			want: "12 tokens",
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			got := tt.line.Text()

			// Assert
			if got != tt.want {
				t.Fatalf("Text() = %q, want %q", got, tt.want)
			}
		})
	}
}

func TestFiguredLineWireFigureCarriesTheFigureAndItsFill(t *testing.T) {
	// Arrange
	line := FiguredLine{Lead: "a ", Figure: "409,051", Tail: " b", WindowFill: 0.41}

	// Act
	figure := line.wireFigure()

	// Assert
	if figure.GetText() != "409,051" || figure.GetWindowFill() != 0.41 {
		t.Fatalf("wireFigure() = %v, want text 409,051 and fill 0.41", figure)
	}
}
