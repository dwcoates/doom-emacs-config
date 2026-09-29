package commandfile

import (
	"strings"
	"testing"
)

func TestValidateAcceptsEveryAcceptedShape(t *testing.T) {
	done := true
	tests := []struct {
		name  string
		entry Entry
	}{
		{name: "create", entry: Entry{Type: TypeCreate, GitRoot: "/repo", Name: "n"}},
		{name: "create with only a prompt", entry: Entry{Type: TypeCreate, GitRoot: "/repo", Prompt: "p"}},
		{name: "prompt", entry: Entry{Type: TypePrompt, Workspace: "w1", Prompt: "p"}},
		{name: "send", entry: Entry{Type: TypeSend, Dir: "/tree", Prompt: "p"}},
		{name: "merge", entry: Entry{Type: TypeMerge, Workspace: "w1"}},
		{name: "merge by the skill's project_dir", entry: Entry{Type: TypeMerge, Workspace: "a-display-name", ProjectDir: "/tree"}},
		{name: "close by project_dir alone", entry: Entry{Type: TypeClose, ProjectDir: "/tree"}},
		{name: "close", entry: Entry{Type: TypeClose, Dir: "/tree"}},
		{name: "forget", entry: Entry{Type: TypeForget, Workspace: "w1"}},
		{name: "open", entry: Entry{Type: TypeOpen, Workspace: "w1"}},
		{name: "switch", entry: Entry{Type: TypeSwitch, Dir: "/tree"}},
		{name: "task create", entry: Entry{Type: TypeTaskCreate, Title: "t"}},
		{name: "task toggle done", entry: Entry{Type: TypeTaskToggleDone, ID: "task-1", Done: &done}},
		{name: "task add workspace", entry: Entry{Type: TypeTaskAddWorkspace, ID: "task-1", Workspace: "w1"}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange in the table. Act.
			err := tt.entry.Validate()
			// Assert.
			if err != nil {
				t.Fatalf("Validate(%s): %v", tt.name, err)
			}
		})
	}
}

func TestValidateRefusesEveryIncompleteShape(t *testing.T) {
	blank := ""
	tests := []struct {
		name  string
		entry Entry
	}{
		{name: "no type", entry: Entry{}},
		{name: "unknown type", entry: Entry{Type: "teleport"}},
		{name: "create with no repository", entry: Entry{Type: TypeCreate, Name: "n"}},
		{name: "create naming nothing", entry: Entry{Type: TypeCreate, GitRoot: "/repo"}},
		{name: "prompt with no target", entry: Entry{Type: TypePrompt, Prompt: "p"}},
		{name: "prompt with no text", entry: Entry{Type: TypePrompt, Workspace: "w1"}},
		{name: "merge with no target", entry: Entry{Type: TypeMerge}},
		{name: "forget with no target", entry: Entry{Type: TypeForget}},
		{name: "task create with no title", entry: Entry{Type: TypeTaskCreate, Title: blank}},
		{name: "task toggle with no id", entry: Entry{Type: TypeTaskToggleDone}},
		{name: "task toggle with no done", entry: Entry{Type: TypeTaskToggleDone, ID: "task-1"}},
		{name: "task add workspace with no target", entry: Entry{Type: TypeTaskAddWorkspace, ID: "task-1"}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange in the table. Act.
			err := tt.entry.Validate()
			// Assert.
			if err == nil {
				t.Fatalf("Validate(%s) = nil error, want a refusal", tt.name)
			}
		})
	}
}

func TestValidateDistinguishesAbsentFromFalseOnDone(t *testing.T) {
	// Arrange: "done": false and no "done" at all are different requests.
	notDone := false
	entry := Entry{Type: TypeTaskToggleDone, ID: "task-1", Done: &notDone}

	// Act.
	err := entry.Validate()

	// Assert.
	if err != nil {
		t.Fatalf("Validate(explicit false): %v", err)
	}
}

func TestParseDecodesTheWholeArray(t *testing.T) {
	// Arrange.
	body := `[{"type":"merge","workspace":"w1"},{"type":"close","workspace":"w1"}]`

	// Act.
	entries, err := parse([]byte(body))

	// Assert.
	if err != nil {
		t.Fatalf("parse: %v", err)
	}
	if len(entries) != 2 || entries[0].Type != TypeMerge || entries[1].Type != TypeClose {
		t.Fatalf("parse() = %+v, want the merge and close entries in order", entries)
	}
}

func TestParseRefusesAnEmptyArray(t *testing.T) {
	// Arrange: an empty array reads as success while asking for nothing.
	// Act.
	_, err := parse([]byte(`[]`))

	// Assert.
	if err == nil {
		t.Fatal("parse([]) = nil error, want a refusal")
	}
}

func TestParseRefusesAHalfWrittenDocument(t *testing.T) {
	// Arrange: a file still being written often ends mid-token.
	// Act.
	_, err := parse([]byte(`[{"type":"merge","workspa`))

	// Assert.
	if err == nil {
		t.Fatal("parse(truncated) = nil error, want a decode failure")
	}
}

func TestParseRefusesTrailingBytesAfterTheArray(t *testing.T) {
	// Arrange.
	// Act.
	_, err := parse([]byte(`[{"type":"merge","workspace":"w1"}] trailing`))

	// Assert.
	if err == nil {
		t.Fatal("parse(trailing bytes) = nil error, want a decode failure")
	}
}

func TestParseRefusesTheWholeArrayForOneInvalidEntry(t *testing.T) {
	// Arrange: the array is ONE request, and half of it is not a smaller
	// request.
	body := `[{"type":"merge","workspace":"w1"},{"type":"merge"}]`

	// Act.
	_, err := parse([]byte(body))

	// Assert.
	if err == nil {
		t.Fatal("parse(one invalid entry) = nil error, want the whole array refused")
	}
	if !strings.Contains(err.Error(), "entry 1") {
		t.Fatalf("error = %v, want it to name the offending entry", err)
	}
}

func TestWithAbsolutePaths(t *testing.T) {
	tests := []struct {
		name    string
		entry   Entry
		want    Entry
		wantErr string
	}{
		{
			name:  "a create's tilde git_root is expanded",
			entry: Entry{Type: TypeCreate, Name: "n", GitRoot: "~/.config/doom"},
			want:  Entry{Type: TypeCreate, Name: "n", GitRoot: "/Users/me/.config/doom"},
		},
		{
			name:  "a tilde project_dir is expanded",
			entry: Entry{Type: TypeMerge, ProjectDir: "~/tree/w1"},
			want:  Entry{Type: TypeMerge, ProjectDir: "/Users/me/tree/w1"},
		},
		{
			name:  "a tilde dir is expanded",
			entry: Entry{Type: TypeClose, Dir: "~/tree/w1"},
			want:  Entry{Type: TypeClose, Dir: "/Users/me/tree/w1"},
		},
		{
			name:  "an absolute path is kept, cleaned",
			entry: Entry{Type: TypeMerge, ProjectDir: "/tree/w1/"},
			want:  Entry{Type: TypeMerge, ProjectDir: "/tree/w1"},
		},
		{
			name:  "an entry with no directory is unchanged",
			entry: Entry{Type: TypeMerge, Workspace: "w1"},
			want:  Entry{Type: TypeMerge, Workspace: "w1"},
		},
		{
			name:    "a relative git_root is refused, naming the field",
			entry:   Entry{Type: TypeCreate, Name: "n", GitRoot: "doom"},
			wantErr: "create: git_root: ",
		},
		{
			name:    "a relative project_dir is refused, naming the field",
			entry:   Entry{Type: TypeMerge, ProjectDir: "tree/w1"},
			wantErr: "merge: project_dir: ",
		},
		{
			name:    "another user's home in dir is refused, naming the field",
			entry:   Entry{Type: TypeClose, Dir: "~bob/w1"},
			wantErr: "close: dir: ",
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange: tt.entry, and the home it expands against.
			const home = "/Users/me"

			// Act.
			got, err := tt.entry.withAbsolutePaths(home)

			// Assert.
			if tt.wantErr != "" {
				if err == nil || !strings.Contains(err.Error(), tt.wantErr) {
					t.Fatalf("withAbsolutePaths() = (%+v, %v), want an error containing %q", got, err, tt.wantErr)
				}
				return
			}
			if err != nil || got != tt.want {
				t.Fatalf("withAbsolutePaths() = (%+v, %v), want %+v", got, err, tt.want)
			}
		})
	}
}

func TestDecodeRefusesTheWholeArrayForOneUnresolvableDirectory(t *testing.T) {
	// Arrange: a valid first entry and a second whose project_dir is relative.
	body := `[{"type":"merge","project_dir":"/tree/w1"},{"type":"merge","project_dir":"tree/w2"}]`

	// Act.
	entries, err := decode([]byte(body), "/Users/me")

	// Assert.
	if err == nil || !strings.Contains(err.Error(), "entry 1: merge: project_dir: ") {
		t.Fatalf("decode() = (%+v, %v), want entry 1's project_dir refused", entries, err)
	}
}

func TestDecodeRefusesWhatParseRefuses(t *testing.T) {
	// Arrange / Act.
	_, err := decode([]byte(`[]`), "/Users/me")

	// Assert.
	if err == nil {
		t.Fatal("decode([]) = nil error, want parse's refusal")
	}
}

func TestValidateRefusesAContradictoryOrUnknownCreate(t *testing.T) {
	tests := []struct {
		name    string
		entry   Entry
		wantErr string
	}{
		{name: "both base spellings", entry: Entry{Type: TypeCreate, Name: "n", GitRoot: "/r", BaseRef: "a", BaseCommit: "b"}, wantErr: "two spellings of one base"},
		{name: "a fork with a base", entry: Entry{Type: TypeCreate, Name: "n", GitRoot: "/r", ForkFrom: "w", BaseCommit: "HEAD"}, wantErr: "a fork_from create takes no base"},
		{name: "a fork with an older-spelled base", entry: Entry{Type: TypeCreate, Name: "n", GitRoot: "/r", ForkFrom: "w", BaseRef: "HEAD"}, wantErr: "a fork_from create takes no base"},
		{name: "a source with no path", entry: Entry{Type: TypeCreate, Name: "n", GitRoot: "/r", SourceWS: &SourceWorkspace{Name: "m"}}, wantErr: "source_ws.path is required"},
		{name: "an unknown priority", entry: Entry{Type: TypeCreate, Name: "n", GitRoot: "/r", Priority: "p0"}, wantErr: `priority "p0" is none of`},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange: tt.entry.

			// Act.
			err := tt.entry.Validate()

			// Assert.
			if err == nil || !strings.Contains(err.Error(), tt.wantErr) {
				t.Fatalf("Validate() = %v, want an error containing %q", err, tt.wantErr)
			}
		})
	}
}

func TestValidateRefusesACreateFieldOnAnyOtherVerb(t *testing.T) {
	tests := []struct {
		name  string
		entry Entry
		field string
	}{
		{name: "git_root", entry: Entry{GitRoot: "/r"}, field: "git_root"},
		{name: "name", entry: Entry{Name: "n"}, field: "name"},
		{name: "one_shot", entry: Entry{OneShot: true}, field: "one_shot"},
		{name: "base_ref", entry: Entry{BaseRef: "HEAD"}, field: "base_ref"},
		{name: "source_ws", entry: Entry{SourceWS: &SourceWorkspace{Path: "/r"}}, field: "source_ws"},
		{name: "base_commit", entry: Entry{BaseCommit: "HEAD"}, field: "base_commit"},
		{name: "fork_from", entry: Entry{ForkFrom: "w"}, field: "fork_from"},
		{name: "model", entry: Entry{Model: "sonnet"}, field: "model"},
		{name: "priority", entry: Entry{Priority: "p1"}, field: "priority"},
		{name: "before_ws_merge", entry: Entry{BeforeWSMerge: "b"}, field: "before_ws_merge"},
		{name: "postprocessing_prompt", entry: Entry{PostprocessingPrompt: "p"}, field: "postprocessing_prompt"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange: an otherwise valid merge carrying the create's field.
			entry := tt.entry
			entry.Type = TypeMerge
			entry.Workspace = "w1"

			// Act.
			err := entry.Validate()

			// Assert.
			if err == nil || !strings.Contains(err.Error(), "merge: "+tt.field+" is a create's field") {
				t.Fatalf("Validate() = %v, want %s refused as a create's field", err, tt.field)
			}
		})
	}
}

func TestValidateAcceptsEveryPriority(t *testing.T) {
	for _, priority := range []string{"p05", "p1", "p2", "p3"} {
		t.Run(priority, func(t *testing.T) {
			// Arrange.
			entry := Entry{Type: TypeCreate, Name: "n", GitRoot: "/r", Priority: priority}

			// Act.
			err := entry.Validate()

			// Assert.
			if err != nil {
				t.Fatalf("Validate() = %v, want %s accepted", err, priority)
			}
		})
	}
}

// TestParseRefusesAnUnknownField pins the strict decode: a field this reader
// does not declare quarantines the file rather than vanishing.
func TestParseRefusesAnUnknownField(t *testing.T) {
	tests := []struct {
		name string
		body string
	}{
		{name: "at the entry", body: `[{"type":"merge","workspace":"w1","pr_was_merged":true}]`},
		{name: "inside source_ws", body: `[{"type":"create","name":"n","git_root":"/r","source_ws":{"path":"/r","branch":"x"}}]`},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange: tt.body.

			// Act.
			_, err := parse([]byte(tt.body))

			// Assert.
			if err == nil || !strings.Contains(err.Error(), "unknown field") {
				t.Fatalf("parse() = %v, want the unknown field refused", err)
			}
		})
	}
}

func TestParseAcceptsTheSkillsVerbatimCreate(t *testing.T) {
	// Arrange / Act.
	entries, err := parse([]byte(skillCreate))

	// Assert.
	if err != nil || len(entries) != 1 || entries[0].SourceWS == nil || entries[0].SourceWS.Name != "master" {
		t.Fatalf("parse() = (%+v, %v), want the skill's create with its source", entries, err)
	}
}

func TestWithAbsolutePathsExpandsTheSourcePath(t *testing.T) {
	// Arrange.
	source := &SourceWorkspace{Name: "m", Path: "~/.config/doom"}
	entry := Entry{Type: TypeCreate, Name: "n", GitRoot: "/r", SourceWS: source}

	// Act.
	got, err := entry.withAbsolutePaths("/Users/me")

	// Assert.
	if err != nil || got.SourceWS.Path != "/Users/me/.config/doom" {
		t.Fatalf("withAbsolutePaths() = (%+v, %v), want the source path expanded", got.SourceWS, err)
	}
	if source.Path != "~/.config/doom" {
		t.Fatalf("the caller's source was rewritten to %q, want it untouched", source.Path)
	}
}

func TestWithAbsolutePathsRefusesARelativeSourcePath(t *testing.T) {
	// Arrange.
	entry := Entry{Type: TypeCreate, Name: "n", GitRoot: "/r", SourceWS: &SourceWorkspace{Path: "doom"}}

	// Act.
	_, err := entry.withAbsolutePaths("/Users/me")

	// Assert.
	if err == nil || !strings.Contains(err.Error(), "create: source_ws.path: ") {
		t.Fatalf("withAbsolutePaths() = %v, want source_ws.path refused", err)
	}
}
