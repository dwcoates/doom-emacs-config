package desktopnotify

import "testing"

func TestItalic(t *testing.T) {
	cases := []struct {
		name  string
		input string
		want  string
	}{
		{name: "capital letters", input: "AZ", want: "𝘈𝘡"},
		{name: "small letters", input: "az", want: "𝘢𝘻"},
		{name: "digits and punctuation kept", input: "0 9.,'", want: "0 9.,'"},
		{name: "non-ascii kept", input: "é✅", want: "é✅"},
		{name: "empty", input: "", want: ""},
		{name: "sentence", input: "Fixed 2 bugs.", want: "𝘍𝘪𝘹𝘦𝘥 2 𝘣𝘶𝘨𝘴."},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Act
			got := Italic(tc.input)

			// Assert
			if got != tc.want {
				t.Fatalf("Italic(%q) = %q, want %q", tc.input, got, tc.want)
			}
		})
	}
}

func TestPlain(t *testing.T) {
	cases := []struct {
		name  string
		input string
		want  string
	}{
		{name: "italic capitals folded", input: "𝘈𝘡", want: "AZ"},
		{name: "italic small letters folded", input: "𝘢𝘻", want: "az"},
		{name: "ascii kept", input: "Fixed 2 bugs.", want: "Fixed 2 bugs."},
		{name: "non-ascii kept", input: "é✅", want: "é✅"},
		{name: "the letter past the small block kept", input: string(sansItalicSmallA + 26), want: string(sansItalicSmallA + 26)},
		{name: "empty", input: "", want: ""},
		{name: "inverts Italic", input: Italic("Fixed 2 bugs, Zed."), want: "Fixed 2 bugs, Zed."},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Act
			got := Plain(tc.input)

			// Assert
			if got != tc.want {
				t.Fatalf("Plain(%q) = %q, want %q", tc.input, got, tc.want)
			}
		})
	}
}
