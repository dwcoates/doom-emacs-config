package paint

import "testing"

func TestParseANSIMapsEachSGRAttribute(t *testing.T) {
	tests := []struct {
		name  string
		input string
		class string
	}{
		{name: "bold", input: "\x1b[1mx\x1b[0m", class: "ansi-bold"},
		{name: "dim", input: "\x1b[2mx\x1b[0m", class: "ansi-dim"},
		{name: "italic", input: "\x1b[3mx\x1b[0m", class: "ansi-italic"},
		{name: "underline", input: "\x1b[4mx\x1b[0m", class: "ansi-underline"},
		{name: "inverse", input: "\x1b[7mx\x1b[0m", class: "ansi-inverse"},
		{name: "strike", input: "\x1b[9mx\x1b[0m", class: "ansi-strike"},
	}
	p := newPainter(t)
	classes := loadClasses(t)
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Act.
			spans, err := p.ParseANSI(tc.input)

			// Assert.
			if err != nil {
				t.Fatalf("ParseANSI: %v", err)
			}
			assertInventory(t, classes, spans, "x")
			if len(spans) != 1 || spans[0].Class != tc.class {
				t.Fatalf("spans = %+v, want one %q span", spans, tc.class)
			}
		})
	}
}

func TestParseANSIMapsEachBaseForeground(t *testing.T) {
	tests := []struct {
		code  string
		class string
	}{
		{code: "30", class: "ansi-fg-black"},
		{code: "31", class: "ansi-fg-red"},
		{code: "32", class: "ansi-fg-green"},
		{code: "33", class: "ansi-fg-yellow"},
		{code: "34", class: "ansi-fg-blue"},
		{code: "35", class: "ansi-fg-magenta"},
		{code: "36", class: "ansi-fg-cyan"},
		{code: "37", class: "ansi-fg-white"},
	}
	p := newPainter(t)
	for _, tc := range tests {
		t.Run(tc.class, func(t *testing.T) {
			// Act.
			spans, err := p.ParseANSI("\x1b[" + tc.code + "mx")

			// Assert.
			if err != nil {
				t.Fatalf("ParseANSI: %v", err)
			}
			if len(spans) != 1 || spans[0].Class != tc.class {
				t.Fatalf("spans = %+v, want one %q span", spans, tc.class)
			}
		})
	}
}

func TestParseANSIMapsBrightVariants(t *testing.T) {
	tests := []struct {
		name  string
		input string
		class string
	}{
		{name: "bright foreground", input: "\x1b[91mx", class: "ansi-fg-bright-red"},
		{name: "bright background", input: "\x1b[106mx", class: "ansi-bg-bright-cyan"},
	}
	p := newPainter(t)
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Act.
			spans, err := p.ParseANSI(tc.input)

			// Assert.
			if err != nil {
				t.Fatalf("ParseANSI: %v", err)
			}
			if len(spans) != 1 || spans[0].Class != tc.class {
				t.Fatalf("spans = %+v, want one %q span", spans, tc.class)
			}
		})
	}
}

func TestParseANSIMapsBaseBackgrounds(t *testing.T) {
	// Arrange.
	p := newPainter(t)

	// Act.
	spans, err := p.ParseANSI("\x1b[45mx")

	// Assert.
	if err != nil {
		t.Fatalf("ParseANSI: %v", err)
	}
	if len(spans) != 1 || spans[0].Class != "ansi-bg-magenta" {
		t.Fatalf("spans = %+v, want one ansi-bg-magenta span", spans)
	}
}

func TestParseANSIMaps256ColorOntoTheSixteenNames(t *testing.T) {
	tests := []struct {
		name  string
		input string
		class string
	}{
		{name: "base index", input: "\x1b[38;5;1mx", class: "ansi-fg-red"},
		{name: "bright index", input: "\x1b[38;5;9mx", class: "ansi-fg-bright-red"},
		{name: "cube index", input: "\x1b[38;5;196mx", class: "ansi-fg-bright-red"},
		{name: "grayscale low", input: "\x1b[38;5;232mx", class: "ansi-fg-black"},
		{name: "grayscale high", input: "\x1b[38;5;255mx", class: "ansi-fg-white"},
		{name: "background index", input: "\x1b[48;5;4mx", class: "ansi-bg-blue"},
	}
	p := newPainter(t)
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Act.
			spans, err := p.ParseANSI(tc.input)

			// Assert.
			if err != nil {
				t.Fatalf("ParseANSI: %v", err)
			}
			if len(spans) != 1 || spans[0].Class != tc.class {
				t.Fatalf("spans = %+v, want one %q span", spans, tc.class)
			}
		})
	}
}

func TestParseANSIMapsTruecolorOntoTheSixteenNames(t *testing.T) {
	tests := []struct {
		name  string
		input string
		class string
	}{
		{name: "pure red", input: "\x1b[38;2;255;0;0mx", class: "ansi-fg-bright-red"},
		{name: "dark green", input: "\x1b[38;2;0;200;0mx", class: "ansi-fg-green"},
		{name: "black background", input: "\x1b[48;2;0;0;0mx", class: "ansi-bg-black"},
	}
	p := newPainter(t)
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Act.
			spans, err := p.ParseANSI(tc.input)

			// Assert.
			if err != nil {
				t.Fatalf("ParseANSI: %v", err)
			}
			if len(spans) != 1 || spans[0].Class != tc.class {
				t.Fatalf("spans = %+v, want one %q span", spans, tc.class)
			}
		})
	}
}

func TestParseANSIPaintsOneClassPerSpanByPrecedence(t *testing.T) {
	// Arrange: bold AND red; foreground leads the declared precedence.
	p := newPainter(t)

	// Act.
	spans, err := p.ParseANSI("\x1b[1;31mx\x1b[0m")

	// Assert.
	if err != nil {
		t.Fatalf("ParseANSI: %v", err)
	}
	if len(spans) != 1 || spans[0].Class != "ansi-fg-red" {
		t.Fatalf("spans = %+v, want one ansi-fg-red span", spans)
	}
}

func TestParseANSIPrefersForegroundOverBackground(t *testing.T) {
	// Arrange.
	p := newPainter(t)

	// Act.
	spans, err := p.ParseANSI("\x1b[31;44mx")

	// Assert.
	if err != nil {
		t.Fatalf("ParseANSI: %v", err)
	}
	if spans[0].Class != "ansi-fg-red" {
		t.Fatalf("class = %q, want ansi-fg-red", spans[0].Class)
	}
}

func TestParseANSIFallsBackToTheNextSlotWhenTheStrongestClears(t *testing.T) {
	// Arrange: red then default-foreground, with bold still in force.
	p := newPainter(t)

	// Act.
	spans, err := p.ParseANSI("\x1b[1;31ma\x1b[39mb")

	// Assert.
	if err != nil {
		t.Fatalf("ParseANSI: %v", err)
	}
	if len(spans) != 2 || spans[0].Class != "ansi-fg-red" || spans[1].Class != "ansi-bold" {
		t.Fatalf("spans = %+v, want ansi-fg-red then ansi-bold", spans)
	}
}

func TestParseANSIResetsEveryAttribute(t *testing.T) {
	// Arrange.
	p := newPainter(t)

	// Act.
	spans, err := p.ParseANSI("\x1b[1;31ma\x1b[0mb")

	// Assert.
	if err != nil {
		t.Fatalf("ParseANSI: %v", err)
	}
	if len(spans) != 2 || spans[1].Class != "" {
		t.Fatalf("spans = %+v, want the second span plain", spans)
	}
}

func TestParseANSITreatsAnEmptyParameterListAsReset(t *testing.T) {
	// Arrange.
	p := newPainter(t)

	// Act.
	spans, err := p.ParseANSI("\x1b[31ma\x1b[mb")

	// Assert.
	if err != nil {
		t.Fatalf("ParseANSI: %v", err)
	}
	if len(spans) != 2 || spans[1].Class != "" {
		t.Fatalf("spans = %+v, want the second span plain", spans)
	}
}

func TestParseANSIClearsIndividualAttributes(t *testing.T) {
	tests := []struct {
		name  string
		input string
	}{
		{name: "bold off", input: "\x1b[1ma\x1b[22mb"},
		{name: "italic off", input: "\x1b[3ma\x1b[23mb"},
		{name: "underline off", input: "\x1b[4ma\x1b[24mb"},
		{name: "inverse off", input: "\x1b[7ma\x1b[27mb"},
		{name: "strike off", input: "\x1b[9ma\x1b[29mb"},
		{name: "background off", input: "\x1b[41ma\x1b[49mb"},
	}
	p := newPainter(t)
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Act.
			spans, err := p.ParseANSI(tc.input)

			// Assert.
			if err != nil {
				t.Fatalf("ParseANSI: %v", err)
			}
			if len(spans) != 2 || spans[1].Class != "" {
				t.Fatalf("spans = %+v, want the second span plain", spans)
			}
		})
	}
}

func TestParseANSIKeepsTheTextVerbatim(t *testing.T) {
	// Arrange.
	p := newPainter(t)
	classes := loadClasses(t)

	// Act.
	spans, err := p.ParseANSI("\x1b[1mPASS\x1b[0m ok  \tpkg\t0.01s\n")

	// Assert.
	if err != nil {
		t.Fatalf("ParseANSI: %v", err)
	}
	assertInventory(t, classes, spans, "PASS ok  \tpkg\t0.01s\n")
}

func TestParseANSIDropsEscapesItDoesNotModel(t *testing.T) {
	tests := []struct {
		name  string
		input string
		want  string
	}{
		{name: "cursor move", input: "a\x1b[2Kb", want: "ab"},
		{name: "osc with bel", input: "a\x1b]0;title\x07b", want: "ab"},
		{name: "osc with string terminator", input: "a\x1b]0;title\x1b\\b", want: "ab"},
		{name: "charset designation", input: "a\x1b(Bb", want: "ab"},
		{name: "two byte escape", input: "a\x1bMb", want: "ab"},
		{name: "trailing lone escape", input: "ab\x1b", want: "ab"},
		{name: "unterminated csi", input: "ab\x1b[38;5", want: "ab"},
	}
	p := newPainter(t)
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Act.
			spans, err := p.ParseANSI(tc.input)

			// Assert.
			if err != nil {
				t.Fatalf("ParseANSI: %v", err)
			}
			if got := spans.Text(); got != tc.want {
				t.Fatalf("text = %q, want %q", got, tc.want)
			}
			for _, span := range spans {
				if span.Class != "" {
					t.Fatalf("an unmodeled escape produced class %q", span.Class)
				}
			}
		})
	}
}

func TestParseANSIEmitsNoSpansForEmptyInput(t *testing.T) {
	// Arrange.
	p := newPainter(t)

	// Act.
	spans, err := p.ParseANSI("")

	// Assert.
	if err != nil {
		t.Fatalf("ParseANSI: %v", err)
	}
	if len(spans) != 0 {
		t.Fatalf("spans = %+v, want none", spans)
	}
}

func TestParseANSIMergesAdjacentRunsOfTheSameClass(t *testing.T) {
	// Arrange: two escapes that leave the same class in force.
	p := newPainter(t)

	// Act.
	spans, err := p.ParseANSI("\x1b[31ma\x1b[31mb")

	// Assert.
	if err != nil {
		t.Fatalf("ParseANSI: %v", err)
	}
	if len(spans) != 1 || spans[0].Text != "ab" {
		t.Fatalf("spans = %+v, want one span carrying ab", spans)
	}
}

func TestParseANSIIgnoresAMalformedExtendedColor(t *testing.T) {
	// Arrange: a 38 with no continuation at all.
	p := newPainter(t)

	// Act.
	spans, err := p.ParseANSI("\x1b[38mx")

	// Assert.
	if err != nil {
		t.Fatalf("ParseANSI: %v", err)
	}
	if len(spans) != 1 || spans[0].Class != "" {
		t.Fatalf("spans = %+v, want one plain span", spans)
	}
}

func TestParseANSIRefusesAClassOutsideTheInventory(t *testing.T) {
	// Arrange: an inventory that lost the bold class the input demands.
	classes := loadClasses(t)
	pruned := make([]string, 0, len(classes.ANSI))
	for _, class := range classes.ANSI {
		if class != "ansi-bold" {
			pruned = append(pruned, class)
		}
	}
	classes.ANSI = pruned
	p, err := New(classes)
	if err != nil {
		t.Fatalf("New: %v", err)
	}

	// Act.
	_, err = p.ParseANSI("\x1b[1mx")

	// Assert.
	if err == nil {
		t.Fatal("ParseANSI emitted a class outside the inventory")
	}
}
