package convert

// sessioncommand_test.go — the SessionCommand table is read off the schema, and
// text typed as a command is split the way the CLI splits it.

import "testing"

func TestTypedSessionCommand(t *testing.T) {
	tests := []struct {
		name string
		text string
		want bool
	}{
		{name: "a command that takes no argument, alone", text: "/status", want: true},
		{name: "a command that takes an argument, alone", text: "/compact", want: true},
		{name: "a command that takes an argument, with one", text: "/compact keep the plan", want: true},
		{name: "the argument split on a newline, as the CLI splits it", text: "/compact\nkeep the plan", want: true},
		{name: "an alias of a command", text: "/reset", want: true},
		{name: "surrounding whitespace is not part of the command", text: "  /model fable \n", want: true},
		{name: "/effort alone", text: "/effort", want: true},
		{name: "/effort with its argument", text: "/effort high", want: true},
		{name: "/plugin alone", text: "/plugin", want: true},
		{name: "/plugin with its argument", text: "/plugin install foo", want: true},
		{name: "/low-priority alone", text: "/low-priority", want: true},
		{name: "prose after /low-priority, which takes none, stays a prompt", text: "/low-priority please", want: false},
		{name: "prose after a command that takes none stays a prompt", text: "/status of the build", want: false},
		{name: "a custom command the schema does not name stays a prompt", text: "/loop keep going", want: false},
		{name: "a command name without its slash is prose", text: "compact", want: false},
		{name: "empty text is no command", text: "", want: false},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange: the table is the schema's, read once by reflection.

			// Act.
			got := typedSessionCommand(tc.text)

			// Assert.
			if got != tc.want {
				t.Fatalf("typedSessionCommand(%q) = %t, want %t", tc.text, got, tc.want)
			}
		})
	}
}

func TestSessionCommandTableIsReadOffTheSchema(t *testing.T) {
	// Arrange. Every SessionCommand value but UNSPECIFIED carries a spec, so the
	// table must hold at least one spelling per named value.

	// Act.
	table := sessionCommandTable()

	// Assert.
	for _, literal := range []string{"/clear", "/compact", "/model", "/status", "/login"} {
		if _, ok := table[literal]; !ok {
			t.Fatalf("the table read off the schema is missing %s: %v", literal, table)
		}
	}
}
