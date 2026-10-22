package semver

import "testing"

func TestParseReadsOnlyBareThreeNumberTags(t *testing.T) {
	tests := []struct {
		name string
		tag  string
		want Version
		ok   bool
	}{
		{name: "a bare version", tag: "0.4.12", want: Version{0, 4, 12}, ok: true},
		{name: "zeros", tag: "0.0.0", want: Version{}, ok: true},
		{name: "a v prefix", tag: "v0.0.1"},
		{name: "two numbers", tag: "1.2"},
		{name: "four numbers", tag: "1.2.3.4"},
		{name: "a pre-release suffix", tag: "1.2.3-rc1"},
		{name: "a leading zero", tag: "1.02.3"},
		{name: "an empty number", tag: "1..3"},
		{name: "a sign", tag: "+1.2.3"},
		{name: "a branch-like tag", tag: "start/foo"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			got, ok := Parse(tt.tag)

			// Assert
			if ok != tt.ok || got != tt.want {
				t.Fatalf("Parse(%q) = %v, %v; want %v, %v", tt.tag, got, ok, tt.want, tt.ok)
			}
		})
	}
}

func TestHighestOrdersNumericallyAndIgnoresOtherTags(t *testing.T) {
	tests := []struct {
		name string
		tags []string
		want Version
		ok   bool
	}{
		{name: "numeric, not lexical", tags: []string{"0.9.0", "0.10.0", "0.2.0"}, want: Version{0, 10, 0}, ok: true},
		{name: "major outranks minor", tags: []string{"0.99.99", "1.0.0"}, want: Version{1, 0, 0}, ok: true},
		{name: "patch decides last", tags: []string{"0.1.2", "0.1.10"}, want: Version{0, 1, 10}, ok: true},
		{name: "non-versions ignored", tags: []string{"v9.9.9", "wip-x", "0.0.1"}, want: Version{0, 0, 1}, ok: true},
		{name: "no version at all", tags: []string{"v1.0.0", "start/x"}},
		{name: "one version tagged twice", tags: []string{"0.3.1", "0.3.1"}, want: Version{0, 3, 1}, ok: true},
		{name: "no tags", tags: nil},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			got, ok := Highest(tt.tags)

			// Assert
			if ok != tt.ok || got != tt.want {
				t.Fatalf("Highest(%v) = %v, %v; want %v, %v", tt.tags, got, ok, tt.want, tt.ok)
			}
		})
	}
}

func TestNextMovesThePartAndResetsWhatFollows(t *testing.T) {
	tests := []struct {
		name string
		from Version
		part Part
		want string
	}{
		{name: "minor resets patch", from: Version{0, 4, 7}, part: Minor, want: "0.5.0"},
		{name: "patch keeps minor", from: Version{0, 4, 7}, part: Patch, want: "0.4.8"},
		{name: "minor past nine", from: Version{1, 9, 0}, part: Minor, want: "1.10.0"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			got := tt.from.Next(tt.part).String()

			// Assert
			if got != tt.want {
				t.Fatalf("%v.Next(%s) = %s, want %s", tt.from, tt.part, got, tt.want)
			}
		})
	}
}

func TestNextPanicsOnAnUndeclaredPart(t *testing.T) {
	// Arrange
	defer func() {
		if recover() == nil {
			t.Fatal("Next(0) did not panic")
		}
	}()

	// Act
	Version{}.Next(Part(0))
}

func TestSplitConcatenatesToTheTag(t *testing.T) {
	tests := []struct {
		name                           string
		v                              Version
		part                           Part
		leading, incremented, trailing string
	}{
		{name: "minor", v: Version{0, 5, 0}, part: Minor, leading: "0.", incremented: "5", trailing: ".0"},
		{name: "patch", v: Version{0, 4, 8}, part: Patch, leading: "0.4.", incremented: "8", trailing: ""},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			l, i, tr := tt.v.Split(tt.part)

			// Assert
			if l != tt.leading || i != tt.incremented || tr != tt.trailing {
				t.Fatalf("Split = %q %q %q, want %q %q %q", l, i, tr, tt.leading, tt.incremented, tt.trailing)
			}
			if l+i+tr != tt.v.String() {
				t.Fatalf("Split does not concatenate to %s: %q", tt.v, l+i+tr)
			}
		})
	}
}

func TestParsePartRoundTripsAndRefusesMajor(t *testing.T) {
	tests := []struct {
		name string
		in   string
		want Part
		err  bool
	}{
		{name: "minor", in: "minor", want: Minor},
		{name: "patch", in: "patch", want: Patch},
		{name: "major is not a part", in: "major", err: true},
		{name: "empty", in: "", err: true},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			got, err := ParsePart(tt.in)

			// Assert
			if (err != nil) != tt.err || got != tt.want {
				t.Fatalf("ParsePart(%q) = %v, %v", tt.in, got, err)
			}
			if !tt.err && got.String() != tt.in {
				t.Fatalf("%v.String() = %q, want %q", got, got.String(), tt.in)
			}
		})
	}
}
