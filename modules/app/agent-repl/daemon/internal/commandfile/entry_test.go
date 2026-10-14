package commandfile

import (
	"encoding/json"
	"reflect"
	"strings"
	"testing"

	"agent-repld/internal/semver"
	"agent-repld/internal/wsm"
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
		{name: "merge", entry: Entry{Type: TypeMerge, Workspace: "w1", OneShot: true}},
		{name: "merge by the skill's project_dir", entry: Entry{Type: TypeMerge, Workspace: "a-display-name", ProjectDir: "/tree", OneShot: true}},
		{name: "close by project_dir alone", entry: Entry{Type: TypeClose, ProjectDir: "/tree"}},
		{name: "close", entry: Entry{Type: TypeClose, Dir: "/tree"}},
		{name: "forget", entry: Entry{Type: TypeForget, Workspace: "w1"}},
		{name: "open", entry: Entry{Type: TypeOpen, Workspace: "w1"}},
		{name: "switch", entry: Entry{Type: TypeSwitch, Dir: "/tree"}},
		{name: "task create", entry: Entry{Type: TypeTaskCreate, Title: "t"}},
		{name: "task toggle done", entry: Entry{Type: TypeTaskToggleDone, ID: "task-1", Done: &done}},
		{name: "task add workspace", entry: Entry{Type: TypeTaskAddWorkspace, ID: "task-1", Workspace: "w1"}},
		{name: "merge evict of the requester's own", entry: Entry{Type: TypeMergeEvict, ProjectDir: "/tree"}},
		{name: "merge evict of another workspace", entry: Entry{Type: TypeMergeEvict, ProjectDir: "/tree", EvictDir: "/tree2"}},
		{name: "merge move up of the requester's own", entry: Entry{Type: TypeMergeMoveUp, ProjectDir: "/tree"}},
		{name: "merge move down of another workspace", entry: Entry{Type: TypeMergeMoveDown, ProjectDir: "/tree", MoveDir: "/tree2"}},
		{name: "merge pause of every repository", entry: Entry{Type: TypeMergePause, ProjectDir: "/tree"}},
		{name: "merge pause of one repository", entry: Entry{Type: TypeMergePause, ProjectDir: "/tree", RepositoryDir: "/repo"}},
		{name: "merge resume of every repository", entry: Entry{Type: TypeMergeResume, ProjectDir: "/tree"}},
		{name: "merge resume of one repository", entry: Entry{Type: TypeMergeResume, ProjectDir: "/tree", RepositoryDir: "/repo"}},
		{name: "merge hold rollouts", entry: Entry{Type: TypeMergeHoldRollouts, ProjectDir: "/tree"}},
		{name: "merge release rollouts", entry: Entry{Type: TypeMergeReleaseRollouts, ProjectDir: "/tree"}},
		{name: "merge disable racing", entry: Entry{Type: TypeMergeDisableRacing, ProjectDir: "/tree"}},
		{name: "merge enable racing", entry: Entry{Type: TypeMergeEnableRacing, ProjectDir: "/tree"}},
		{name: "retarget to a parent", entry: Entry{Type: TypeRetarget, ProjectDir: "/tree", Parent: PresentDir{Present: true, Dir: "/tree2"}}},
		{name: "retarget to top-level", entry: Entry{Type: TypeRetarget, ProjectDir: "/tree", Parent: PresentDir{Present: true}}},
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
		{name: "merge evict with no requester", entry: Entry{Type: TypeMergeEvict}},
		{name: "merge pause with no requester", entry: Entry{Type: TypeMergePause}},
		{name: "merge resume with no requester", entry: Entry{Type: TypeMergeResume}},
		{name: "merge hold rollouts with no requester", entry: Entry{Type: TypeMergeHoldRollouts}},
		{name: "merge release rollouts with no requester", entry: Entry{Type: TypeMergeReleaseRollouts}},
		{name: "merge disable racing with no requester", entry: Entry{Type: TypeMergeDisableRacing}},
		{name: "merge enable racing with no requester", entry: Entry{Type: TypeMergeEnableRacing}},
		{name: "repository_dir on a racing switch", entry: Entry{Type: TypeMergeDisableRacing, ProjectDir: "/tree", RepositoryDir: "/repo"}},
		{name: "repository_dir on a rollout hold", entry: Entry{Type: TypeMergeHoldRollouts, ProjectDir: "/tree", RepositoryDir: "/repo"}},
		{name: "merge move up with no requester", entry: Entry{Type: TypeMergeMoveUp}},
		{name: "merge move down with no requester", entry: Entry{Type: TypeMergeMoveDown}},
		{name: "evict_dir on a close", entry: Entry{Type: TypeClose, Dir: "/tree", EvictDir: "/tree2"}},
		{name: "move_dir on an evict", entry: Entry{Type: TypeMergeEvict, Dir: "/tree", MoveDir: "/tree2"}},
		{name: "repository_dir on a merge", entry: Entry{Type: TypeMerge, Dir: "/tree", RepositoryDir: "/repo"}},
		{name: "retarget with no target", entry: Entry{Type: TypeRetarget, Parent: PresentDir{Present: true}}},
		{name: "retarget with no parent key", entry: Entry{Type: TypeRetarget, ProjectDir: "/tree"}},
		{name: "parent on a close", entry: Entry{Type: TypeClose, Dir: "/tree", Parent: PresentDir{Present: true}}},
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
	body := `[{"type":"merge","workspace":"w1","one_shot":true},{"type":"close","workspace":"w1"}]`

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
	_, err := parse([]byte(`[{"type":"merge","workspace":"w1","one_shot":true}] trailing`))

	// Assert.
	if err == nil {
		t.Fatal("parse(trailing bytes) = nil error, want a decode failure")
	}
}

func TestParseRefusesTheWholeArrayForOneInvalidEntry(t *testing.T) {
	// Arrange: the array is ONE request, and half of it is not a smaller
	// request.
	body := `[{"type":"merge","workspace":"w1","one_shot":true},{"type":"merge"}]`

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
			entry: Entry{Type: TypeMerge, Workspace: "w1", OneShot: true},
			want:  Entry{Type: TypeMerge, Workspace: "w1", OneShot: true},
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
	body := `[{"type":"merge","project_dir":"/tree/w1","one_shot":true},{"type":"merge","project_dir":"tree/w2","one_shot":true}]`

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
		{name: "base_ref", entry: Entry{BaseRef: "HEAD"}, field: "base_ref"},
		{name: "source_ws", entry: Entry{SourceWS: &SourceWorkspace{Path: "/r"}}, field: "source_ws"},
		{name: "base_commit", entry: Entry{BaseCommit: "HEAD"}, field: "base_commit"},
		{name: "fork_from", entry: Entry{ForkFrom: "w"}, field: "fork_from"},
		{name: "model", entry: Entry{Model: "sonnet"}, field: "model"},
		{name: "priority", entry: Entry{Priority: "p1"}, field: "priority"},
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

func TestValidateAcceptsMergeActionsOnAnAsyncMerge(t *testing.T) {
	tests := []struct {
		name  string
		entry Entry
	}{
		{name: "before_ws_merge", entry: Entry{BeforeWSMerge: "b"}},
		{name: "postprocessing_prompt", entry: Entry{PostprocessingPrompt: "p"}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			entry := tt.entry
			entry.Type, entry.Workspace, entry.AsyncName, entry.Branch = TypeMerge, "w1", "reconnect-fix", "agent-1/fix"

			// Act.
			err := entry.Validate()

			// Assert.
			if err != nil {
				t.Fatalf("Validate() = %v, want an async merge's actions accepted", err)
			}
		})
	}
}

func TestValidateRefusesMergeActionsOnAOneShotMerge(t *testing.T) {
	tests := []struct {
		name  string
		entry Entry
		field string
	}{
		{name: "before_ws_merge", entry: Entry{BeforeWSMerge: "b"}, field: "before_ws_merge"},
		{name: "postprocessing_prompt", entry: Entry{PostprocessingPrompt: "p"}, field: "postprocessing_prompt"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			entry := tt.entry
			entry.Type, entry.Workspace, entry.OneShot = TypeMerge, "w1", true

			// Act.
			err := entry.Validate()

			// Assert.
			if err == nil || !strings.Contains(err.Error(), tt.field+" belongs to an async merge") {
				t.Fatalf("Validate() = %v, want %s refused on a one_shot", err, tt.field)
			}
		})
	}
}

func TestValidateRefusesMergeActionsOnAVerbThatIsNeitherACreateNorAMerge(t *testing.T) {
	tests := []struct {
		name  string
		entry Entry
		field string
	}{
		{name: "before_ws_merge", entry: Entry{BeforeWSMerge: "b"}, field: "before_ws_merge"},
		{name: "postprocessing_prompt", entry: Entry{PostprocessingPrompt: "p"}, field: "postprocessing_prompt"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			entry := tt.entry
			entry.Type, entry.Workspace = TypeClose, "w1"

			// Act.
			err := entry.Validate()

			// Assert.
			if err == nil || !strings.Contains(err.Error(), "close: "+tt.field+" is a create's or an async merge's field") {
				t.Fatalf("Validate() = %v, want %s refused on a close", err, tt.field)
			}
		})
	}
}

func TestEntryMergeActionsReadsBothFields(t *testing.T) {
	tests := []struct {
		name  string
		entry Entry
		want  wsm.MergeActions
	}{
		{name: "neither", entry: Entry{}, want: wsm.MergeActions{}},
		{name: "before alone", entry: Entry{BeforeWSMerge: "b"}, want: wsm.MergeActions{Before: []string{"b"}}},
		{name: "after alone", entry: Entry{PostprocessingPrompt: "p"}, want: wsm.MergeActions{After: []string{"p"}}},
		{name: "both", entry: Entry{BeforeWSMerge: "b", PostprocessingPrompt: "p"}, want: wsm.MergeActions{Before: []string{"b"}, After: []string{"p"}}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act.
			got := tt.entry.MergeActions()

			// Assert.
			if !reflect.DeepEqual(got, tt.want) {
				t.Fatalf("MergeActions() = %+v, want %+v", got, tt.want)
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
		{name: "at the entry", body: `[{"type":"merge","workspace":"w1","no_such_field":true}]`},
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

func TestAMergeEntryInOneModeValidates(t *testing.T) {
	tests := []struct {
		name  string
		entry Entry
	}{
		{name: "a one-shot", entry: Entry{Type: TypeMerge, Workspace: "w1", OneShot: true}},
		{name: "a one-shot already merged upstream", entry: Entry{Type: TypeMerge, Workspace: "w1", OneShot: true, PRWasMerged: true}},
		{name: "an async merge", entry: Entry{Type: TypeMerge, Workspace: "w1", AsyncName: "fix", Branch: "agent-1/fix"}},
		{name: "an async merge with an instruction", entry: Entry{Type: TypeMerge, Workspace: "w1", AsyncName: "fix", Branch: "agent-1/fix", Instruction: "rebase it"}},
		{name: "an async merge closing a workspace and cleaning up", entry: Entry{Type: TypeMerge, Workspace: "w1", AsyncName: "fix", Branch: "agent-1/fix", CloseDir: "/wt/two", CleanupWorktree: true}},
		{name: "an async merge compacting after", entry: Entry{Type: TypeMerge, Workspace: "w1", AsyncName: "fix", Branch: "agent-1/fix", CompactAfter: true}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act.
			err := tt.entry.Validate()

			// Assert.
			if err != nil {
				t.Fatalf("Validate() = %v, want accepted", err)
			}
		})
	}
}

func TestAMergeEntryThatIsNotExactlyOneModeIsRefused(t *testing.T) {
	tests := []struct {
		name    string
		entry   Entry
		wantErr string
	}{
		{name: "no mode", entry: Entry{Type: TypeMerge, Workspace: "w1"}, wantErr: "is a one_shot"},
		{name: "both modes", entry: Entry{Type: TypeMerge, Workspace: "w1", OneShot: true, AsyncName: "fix", Branch: "b"}, wantErr: "never both"},
		{name: "a blank async name", entry: Entry{Type: TypeMerge, Workspace: "w1", AsyncName: "  ", Branch: "b"}, wantErr: "names its merge workspace"},
		{name: "an async merge with no branch", entry: Entry{Type: TypeMerge, Workspace: "w1", AsyncName: "fix"}, wantErr: "names the branch it lands"},
		{name: "an async merge already merged upstream", entry: Entry{Type: TypeMerge, Workspace: "w1", AsyncName: "fix", Branch: "b", PRWasMerged: true}, wantErr: "pr_was_merged belongs to a one_shot"},
		{name: "a one-shot naming a branch", entry: Entry{Type: TypeMerge, Workspace: "w1", OneShot: true, Branch: "b"}, wantErr: "belong to an async merge"},
		{name: "a one-shot with an instruction", entry: Entry{Type: TypeMerge, Workspace: "w1", OneShot: true, Instruction: "x"}, wantErr: "belong to an async merge"},
		{name: "a one-shot closing another workspace", entry: Entry{Type: TypeMerge, Workspace: "w1", OneShot: true, CloseDir: "/wt/two"}, wantErr: "belong to an async merge"},
		{name: "a one-shot cleaning up", entry: Entry{Type: TypeMerge, Workspace: "w1", OneShot: true, CleanupWorktree: true}, wantErr: "belong to an async merge"},
		{name: "a one-shot compacting after", entry: Entry{Type: TypeMerge, Workspace: "w1", OneShot: true, CompactAfter: true}, wantErr: "belong to an async merge"},
		{name: "a blank instruction", entry: Entry{Type: TypeMerge, Workspace: "w1", AsyncName: "fix", Branch: "b", Instruction: "   "}, wantErr: "says how to prepare the branch"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act.
			err := tt.entry.Validate()

			// Assert.
			if err == nil || !strings.Contains(err.Error(), tt.wantErr) {
				t.Fatalf("Validate() = %v, want an error containing %q", err, tt.wantErr)
			}
		})
	}
}

func TestOneShotOnAnEntryOtherThanACreateOrAMergeIsRefused(t *testing.T) {
	// Arrange.
	entry := Entry{Type: TypeClose, Workspace: "w1", OneShot: true}

	// Act.
	err := entry.Validate()

	// Assert.
	if err == nil || !strings.Contains(err.Error(), "close: one_shot is a create's or a merge's field") {
		t.Fatalf("Validate() = %v, want one_shot refused on a close", err)
	}
}

func TestAMergeFieldOnAnotherEntryIsRefused(t *testing.T) {
	// Arrange.
	entry := Entry{Type: TypeClose, Workspace: "w1", PRWasMerged: true}

	// Act.
	err := entry.Validate()

	// Assert.
	if err == nil || !strings.Contains(err.Error(), "pr_was_merged is a merge's field") {
		t.Fatalf("Validate() = %v, want the merge-only field refused", err)
	}
}

func TestFrontOnAnotherEntryIsRefused(t *testing.T) {
	// Arrange.
	entry := Entry{Type: TypeMergeEvict, Workspace: "w1", Front: true}

	// Act.
	err := entry.Validate()

	// Assert.
	if err == nil || !strings.Contains(err.Error(), "front is a merge's field") {
		t.Fatalf("Validate() = %v, want the merge-only field refused", err)
	}
}

func TestParseReadsTheSkillsPRMergedEntry(t *testing.T) {
	// Act.
	entries, err := parse([]byte(`[{"type":"merge","project_dir":"/wt/one","workspace":"one","one_shot":true,"pr_was_merged":true}]`))

	// Assert.
	if err != nil || len(entries) != 1 || !entries[0].PRWasMerged {
		t.Fatalf("parse() = (%+v, %v), want the merged-upstream entry", entries, err)
	}
}

func TestWithAbsolutePathsExpandsTheCloseDir(t *testing.T) {
	// Arrange.
	entry := Entry{Type: TypeMerge, Workspace: "w1", AsyncName: "fix", Branch: "b", CloseDir: "~/wt/two"}

	// Act.
	got, err := entry.withAbsolutePaths("/home/u")

	// Assert.
	if err != nil || got.CloseDir != "/home/u/wt/two" {
		t.Fatalf("withAbsolutePaths = (%+v, %v), want the source dir under home", got, err)
	}
}

func TestParentKeepsAPresentNullApartFromAnAbsentKey(t *testing.T) {
	tests := []struct {
		name string
		json string
		want PresentDir
	}{
		{name: "a directory", json: `{"type":"retarget","project_dir":"/t","parent":"/p"}`, want: PresentDir{Present: true, Dir: "/p"}},
		{name: "null", json: `{"type":"retarget","project_dir":"/t","parent":null}`, want: PresentDir{Present: true}},
		{name: "absent", json: `{"type":"close","project_dir":"/t"}`, want: PresentDir{}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act.
			entries, err := parse([]byte("[" + tt.json + "]"))

			// Assert.
			if err != nil {
				t.Fatalf("parse: %v", err)
			}
			if entries[0].Parent != tt.want {
				t.Fatalf("parent = %+v, want %+v", entries[0].Parent, tt.want)
			}
		})
	}
}

func TestParentRefusesWhatIsNeitherADirectoryNorNull(t *testing.T) {
	tests := []struct {
		name   string
		parent string
	}{
		{name: "an empty string", parent: `""`},
		{name: "a number", parent: `7`},
		{name: "an object", parent: `{"path":"/p"}`},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act.
			_, err := parse([]byte(`[{"type":"retarget","project_dir":"/t","parent":` + tt.parent + `}]`))

			// Assert.
			if err == nil {
				t.Fatalf("parse accepted parent %s", tt.parent)
			}
		})
	}
}

func TestParentRoundTripsThroughWrite(t *testing.T) {
	tests := []struct {
		name   string
		parent PresentDir
	}{
		{name: "a directory", parent: PresentDir{Present: true, Dir: "/p"}},
		{name: "null", parent: PresentDir{Present: true}},
		{name: "absent", parent: PresentDir{}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			typ := TypeRetarget
			if !tt.parent.Present {
				typ = TypeClose
			}
			entry := Entry{Type: typ, ProjectDir: "/t", Parent: tt.parent}

			// Act.
			data, err := json.Marshal([]Entry{entry})
			if err != nil {
				t.Fatalf("Marshal: %v", err)
			}
			entries, err := parse(data)

			// Assert.
			if err != nil {
				t.Fatalf("parse(%s): %v", data, err)
			}
			if entries[0].Parent != tt.parent {
				t.Fatalf("parent = %+v after %s, want %+v", entries[0].Parent, data, tt.parent)
			}
		})
	}
}

func TestAMergeEntryCarriesItsVersionIncrement(t *testing.T) {
	tests := []struct {
		name  string
		entry Entry
		want  *wsm.VersionIncrement
	}{
		{name: "a minor", entry: Entry{Type: TypeMerge, Workspace: "w1", OneShot: true, VersionPart: "minor", ChangelogLine: "Merges record a version"},
			want: &wsm.VersionIncrement{Part: semver.Minor, ChangelogLine: "Merges record a version"}},
		{name: "a patch on an async merge", entry: Entry{Type: TypeMerge, Workspace: "w1", AsyncName: "fix", Branch: "b", VersionPart: "patch", ChangelogLine: "Fix the footer"},
			want: &wsm.VersionIncrement{Part: semver.Patch, ChangelogLine: "Fix the footer"}},
		{name: "none", entry: Entry{Type: TypeMerge, Workspace: "w1", OneShot: true}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			validateErr := tt.entry.Validate()
			got, err := tt.entry.VersionIncrement()

			// Assert
			if validateErr != nil || err != nil {
				t.Fatalf("Validate = %v, VersionIncrement = %v; want accepted", validateErr, err)
			}
			if (got == nil) != (tt.want == nil) || (got != nil && *got != *tt.want) {
				t.Fatalf("VersionIncrement = %+v, want %+v", got, tt.want)
			}
		})
	}
}

func TestAMalformedVersionIncrementIsRefused(t *testing.T) {
	tests := []struct {
		name    string
		entry   Entry
		wantErr string
	}{
		{name: "a part without a line", entry: Entry{Type: TypeMerge, Workspace: "w1", OneShot: true, VersionPart: "patch"}, wantErr: "given together"},
		{name: "a line without a part", entry: Entry{Type: TypeMerge, Workspace: "w1", OneShot: true, ChangelogLine: "x"}, wantErr: "given together"},
		{name: "a major increment", entry: Entry{Type: TypeMerge, Workspace: "w1", OneShot: true, VersionPart: "major", ChangelogLine: "x"}, wantErr: "is not a version part"},
		{name: "a two-line changelog", entry: Entry{Type: TypeMerge, Workspace: "w1", OneShot: true, VersionPart: "patch", ChangelogLine: "a\nb"}, wantErr: "one non-blank changelog line"},
		{name: "an increment on another entry", entry: Entry{Type: TypeClose, Workspace: "w1", VersionPart: "patch"}, wantErr: "version_part is a merge's field"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			err := tt.entry.Validate()

			// Assert
			if err == nil || !strings.Contains(err.Error(), tt.wantErr) {
				t.Fatalf("Validate() = %v, want an error containing %q", err, tt.wantErr)
			}
		})
	}
}
