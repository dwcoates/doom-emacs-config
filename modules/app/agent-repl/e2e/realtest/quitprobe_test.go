//go:build realtest

package realtest

import (
	"os"
	"strings"
	"testing"
	"time"
)

// EVERY FIELD THE OWNER ASKED FOR IS IN THE CAPTURE. The table is what the next
// sweep answers "where did the key go" out of, and a field quietly dropped
// costs another sweep to notice.
func TestTheQuitProbeReadsEveryFieldItIsFor(t *testing.T) {
	tests := []struct {
		name string
		want string
	}{
		{name: "the key ring", want: "(recent-keys)"},
		{name: "the last input event", want: "last-input-event"},
		{name: "which frame that event went to", want: "last-event-frame"},
		{name: "the selected frame's name", want: "(frame-parameter (selected-frame) 'name)"},
		{name: "the nil frame's name", want: "(frame-parameter nil 'name)"},
		{name: "the standing minibuffer window", want: "(active-minibuffer-window)"},
		{name: "the frame that minibuffer is on", want: "(window-frame (active-minibuffer-window))"},
		{name: "how deep the reads are nested", want: "(minibuffer-depth)"},
		{name: "whether a quit is armed", want: "quit-flag"},
		{name: "whether a quit would be swallowed", want: "inhibit-quit"},
		{name: "whether the key is queued unread", want: "unread-command-events"},
		{name: "whether the quit character is still C-g", want: "(current-input-mode)"},
		{name: "what the command loop is running", want: "this-command"},
		{name: "what it last dispatched", want: "real-last-command"},
		{name: "what is typed into the prompt", want: "(minibuffer-contents)"},
		{name: "whether a webkit session is live", want: "(xwidget-webkit-current-session)"},
		{name: "whether the selected window shows an xwidget", want: "'xwidget-webkit-mode"},
		{name: "which frames the editor thinks hold focus", want: "frame-focus-state"},
	}

	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			form := quitProbeForm()

			if !strings.Contains(form, test.want) {
				t.Errorf("the quit probe form does not read %s", test.want)
			}
		})
	}
}

// ONE EVAL, NOT EIGHTEEN. The capture has to describe one instant, and probes
// taken a round trip apart would straddle whatever the key was doing.
func TestTheQuitProbeIsOneForm(t *testing.T) {
	form := quitProbeForm()

	if strings.Count(form, "(concat") != 1 || !strings.HasPrefix(form, "(concat") {
		t.Errorf("the quit probe is not one concatenating form: %s", tail(form, 120))
	}
}

// A FIELD THAT SIGNALS MUST NOT COST THE OTHER SEVENTEEN. Nobody yet knows
// which reading is the interesting one.
func TestEveryQuitProbeFieldIsWrappedOnItsOwn(t *testing.T) {
	form := quitProbeForm()

	if got := strings.Count(form, "condition-case"); got != len(quitProbeFields) {
		t.Errorf("the form holds %d condition-case wrappers for %d fields, so at least one field can take "+
			"the whole capture down with it", got, len(quitProbeFields))
	}
}

// Every field has to say why it is there, so a reader of a capture can tell
// what a value means without re-deriving the theory it was added for.
func TestEveryQuitProbeFieldSaysWhyItIsRead(t *testing.T) {
	for _, field := range quitProbeFields {
		if field.Why == "" {
			t.Errorf("the quit probe field %q says nothing about why it is read", field.Name)
		}
	}
}

// A CAPTURE THE EDITOR REFUSED IS ITSELF A READING, and it must say so rather
// than render as an empty one.
func TestAQuitProbeThatTheEditorRefusedSaysSo(t *testing.T) {
	probe := QuitProbe{When: "before the quit press", At: time.Now(), Failure: "the socket did not answer"}

	rendered := probe.Render()

	if !strings.Contains(rendered, "DID NOT ANSWER") {
		t.Errorf("a refused capture does not read as one: %s", rendered)
	}
}

// The capture is carried VERBATIM: it is the evidence the owner rules on, and a
// harness that reformatted it would be the place the interesting field went.
func TestAQuitProbeIsRenderedVerbatim(t *testing.T) {
	probe := QuitProbe{When: "after the quiet window", At: time.Now(), Raw: "quit-flag\tnil\nthis-command\tnil\n"}

	rendered := probe.Render()

	if !strings.Contains(rendered, "quit-flag\tnil\nthis-command\tnil\n") {
		t.Errorf("the capture was not carried verbatim: %q", rendered)
	}
}

// Realtests 5 through 8 press the quit at more than one prompt over one shared
// run directory, so a writer that truncated would leave only the last one — and
// which press matters is not known in advance.
func TestQuitProbesAreAppendedNotRewritten(t *testing.T) {
	runDir := t.TempDir()
	first := QuitProbe{When: "the first prompt", Raw: "quit-flag\tnil\n"}
	second := QuitProbe{When: "the second prompt", Raw: "quit-flag\tt\n"}

	if _, err := AppendQuitProbes(runDir, first); err != nil {
		t.Fatalf("AppendQuitProbes (first): %v", err)
	}
	path, err := AppendQuitProbes(runDir, second)
	if err != nil {
		t.Fatalf("AppendQuitProbes (second): %v", err)
	}

	body, err := os.ReadFile(path)
	if err != nil {
		t.Fatalf("read %s: %v", path, err)
	}
	for _, want := range []string{"the first prompt", "the second prompt"} {
		if !strings.Contains(string(body), want) {
			t.Errorf("%s does not carry %q, so a capture was overwritten", path, want)
		}
	}
}

// THE WHOLE CAPTURE GOES IN THE NOTE, not a path to it: the note is what a
// reader of a failing run sees, and the plan's bar for this loop is evidence
// verbatim.
func TestTheQuitFailureNoteCarriesBothCapturesVerbatim(t *testing.T) {
	before := QuitProbe{When: "before", Raw: "minibuffer-depth\t1\n"}
	after := QuitProbe{When: "after", Raw: "minibuffer-depth\t1\nquit-flag\tnil\n"}

	note := quitProbeNote("Initial prompt: ", before, after, "/run/quit-probe.txt")

	for _, want := range []string{"minibuffer-depth\t1\n", "quit-flag\tnil\n", "/run/quit-probe.txt"} {
		if !strings.Contains(note, want) {
			t.Errorf("the failure note does not carry %q:\n%s", want, note)
		}
	}
}

// The note must read as EVIDENCE and never as a verdict: the press is judged
// once, elsewhere, and this capture names no system.
func TestTheQuitFailureNoteNamesNoSystem(t *testing.T) {
	note := quitProbeNote("Initial prompt: ", QuitProbe{}, QuitProbe{}, "/run/quit-probe.txt")

	if !strings.Contains(note, "EVIDENCE, not a verdict") {
		t.Errorf("the capture note does not say it is evidence rather than a verdict:\n%s", note)
	}
}
