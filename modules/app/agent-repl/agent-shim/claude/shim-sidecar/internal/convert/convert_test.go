package convert

import (
	"encoding/json"
	"os"
	"path/filepath"
	"sort"
	"strings"
	"testing"

	datav1 "agentrepl/proto/agentshim/data/v1"
)

// whichResultArm returns the set ToolUseResult oneof arm's field name, or "" if
// none is set (the empty/unset case; the unclassified arm reports "unclassified").
func whichResultArm(r *datav1.ToolUseResult) string {
	m := r.ProtoReflect()
	od := m.Descriptor().Oneofs().ByName("result")
	fd := m.WhichOneof(od)
	if fd == nil {
		return ""
	}
	if string(fd.Name()) == "unclassified" {
		return "" // treated as "no typed arm" for the test's want=="" cases
	}
	return string(fd.Name())
}

// corpusRoot walks up from the test's working directory to locate
// testdata/corpus (design §14.1 G13), the golden fixtures shared by G2/G3/G4.
func corpusRoot(t *testing.T) string {
	t.Helper()
	dir, err := os.Getwd()
	if err != nil {
		t.Fatalf("getwd: %v", err)
	}
	for {
		cand := filepath.Join(dir, "testdata", "corpus")
		if fi, err := os.Stat(cand); err == nil && fi.IsDir() {
			return cand
		}
		parent := filepath.Dir(dir)
		if parent == dir {
			t.Fatalf("could not locate testdata/corpus above %s", dir)
		}
		dir = parent
	}
}

// knownSchemaGaps is the CLOSED, DOCUMENTED enumeration of fields the corpus
// exposes that the G1 proto (agentshim.data.v1) does not model faithfully. Each
// is a REAL on-disk field with no correct proto home (wrong scalar type, a
// Struct field where the disk carries an array, an unlisted enum value, or an
// absent field). The converter captures every one losslessly into Event.extras
// (never dropped, loud-logged once per name); this test asserts that NO OTHER,
// undocumented extra appears anywhere in the corpus (which fails the build).
//
// This is the "known, listed quantity" discipline (§11) applied to the file
// plane: closing each gap is a G1 proto change (see the group report), NOT a
// silent fallback. The value is the exact proto amendment required.
//
// Keys are canonical leaf names (see convert.canon).
//
// NOTE: as of proto commit bc3d014f the following ADDITIVE fields closed their
// gaps (now decoded as typed data with zero extras, hence removed from this map):
//   iterations → ApiUsage.iterations (google.protobuf.ListValue)
//   speed → ApiUsage.speed (string)
//   inferencegeo → ApiUsage.inference_geo (string)
//   tooldenialkind → ToolDenialKind.TOOL_DENIAL_KIND_PERMISSION_RULE enum value
//   precompactdiscoveredtools → DiskCompactMetadata.pre_compact_discovered_tools
//   cumulativedroppedtokens → DiskCompactMetadata.cumulative_dropped_tokens
//   blockingerror → HookBlockingErrorAttachment.blocking_error (BlockingErrorDetail)
//
// The remaining 10 gap names were BREAKING type mismatches (a Struct/scalar proto
// field where the disk carries an array/object/other type). All were corrected in
// the proto under explicit user approval and are now decoded as typed data:
//   structuredpatch      → Edit/WriteResult.structured_patch (ListValue)
//   questions            → AskUserQuestionResult.questions (repeated Question)
//   results              → WebSearchResult.results (ListValue)
//   tasks                → TaskListResult.tasks (ListValue)
//   updatedfields        → TaskUpdateResult.updated_fields (repeated string)
//   statuschange         → TaskUpdateResult.status_change (TaskStatusChange)
//   pin                  → SendMessageResult.pin (MessagePin)
//   scheduledfor         → ScheduleWakeupResult.scheduled_for (int64)
//   automodeconsentflow  → AutoModeAttachment.auto_mode_consent_flow (bool)
//   files                → DiagnosticsAttachment.files (ListValue)
//   content              → FileAttachment.content (AttachedFileContent) /
//                          TaskReminderAttachment.content (ListValue)
//
// A LATER pass replaced four of the schemaless homes above with typed messages
// (updated_fields, status_change, pin, FileAttachment.content), so those shapes
// are now modeled field-by-field rather than absorbed wholesale — see
// TestTypedToolResultShapes / TestFileAttachmentContentTyped.
//
// The map is therefore EMPTY: the corpus is fully modeled and the contract is now
// ZERO extras anywhere in it. The mechanism stays so a future corpus shape the
// proto cannot express is documented here deliberately rather than tolerated
// silently — an undocumented extra fails the build.
var knownSchemaGaps = map[string]string{}

// isKnownGap reports whether an extras dotted-path is a documented schema gap.
func isKnownGap(path string) bool {
	segs := strings.Split(path, ".")
	leaf := stripIndex(segs[len(segs)-1])
	_, ok := knownSchemaGaps[canon(leaf)]
	return ok
}

func stripIndex(s string) string {
	if i := strings.IndexByte(s, '['); i >= 0 {
		return s[:i]
	}
	return s
}

func readLines(t *testing.T, path string) []map[string]any {
	t.Helper()
	data, err := os.ReadFile(path)
	if err != nil {
		t.Fatalf("read %s: %v", path, err)
	}
	var out []map[string]any
	lines := strings.Split(string(data), "\n")
	for i, ln := range lines {
		ln = strings.TrimSpace(ln)
		if ln == "" {
			continue
		}
		var obj map[string]any
		if err := json.Unmarshal([]byte(ln), &obj); err != nil {
			// A spool captured mid-write (bash-midoutput, agent.output) legitimately
			// ends on a partial line; the codec's carry handles it live. Tolerate an
			// unparseable FINAL line only; anything else is a real fixture error.
			if i == len(lines)-1 || strings.TrimSpace(strings.Join(lines[i+1:], "")) == "" {
				t.Logf("%s: skipping truncated trailing line (codec carry territory)", path)
				break
			}
			t.Fatalf("%s: json line %d: %v", path, i, err)
		}
		out = append(out, obj)
	}
	return out
}

// checkExtras fails the test if extras contains any path whose leaf is not a
// documented schema gap. Returns the sorted list of gap leaves for reporting.
func checkExtras(t *testing.T, fixture string, obj any, extras map[string]any) {
	t.Helper()
	for path := range extras {
		if !isKnownGap(path) {
			t.Errorf("%s: UNDOCUMENTED extras field %q — schema gap not in knownSchemaGaps (STOP and report per golden contract)", fixture, path)
		} else {
			t.Logf("%s: known schema gap captured: %s", fixture, path)
		}
	}
}

// TestGoldenTranscriptLines drives every enveloped/metadata line fixture through
// the transcript converter with ZERO UnparsedEvents (hard errors).
func TestGoldenTranscriptLines(t *testing.T) {
	root := corpusRoot(t)
	dirs := []string{"transcript-lines", "attachments", "content-blocks", "tool-results"}
	for _, d := range dirs {
		files, _ := filepath.Glob(filepath.Join(root, d, "*.jsonl"))
		if len(files) == 0 {
			t.Fatalf("no fixtures under %s", d)
		}
		for _, f := range files {
			name := d + "/" + filepath.Base(f)
			t.Run(name, func(t *testing.T) {
				c := New(func(string, ...any) {})
				for _, obj := range readLines(t, f) {
					line, extras, err := c.TranscriptLine(obj)
					if err != nil {
						t.Fatalf("%s: hard conversion error (would become UnparsedEvent): %v", name, err)
					}
					if line.GetLine() == nil {
						t.Fatalf("%s: converted line has no oneof arm set", name)
					}
					if extras != nil {
						checkExtras(t, name, obj, extras.AsMap())
					}
				}
			})
		}
	}
}

// TestGoldenToolInputs drives the bare tool_use content-block fixtures.
func TestGoldenToolInputs(t *testing.T) {
	root := corpusRoot(t)
	files, _ := filepath.Glob(filepath.Join(root, "tool-inputs", "*.jsonl"))
	if len(files) == 0 {
		t.Fatal("no tool-inputs fixtures")
	}
	for _, f := range files {
		name := "tool-inputs/" + filepath.Base(f)
		t.Run(name, func(t *testing.T) {
			c := New(func(string, ...any) {})
			for _, obj := range readLines(t, f) {
				block, extras, err := c.ContentBlock(obj)
				if err != nil {
					t.Fatalf("%s: hard conversion error: %v", name, err)
				}
				if block.GetToolUse() == nil {
					t.Fatalf("%s: expected tool_use block", name)
				}
				if extras != nil {
					checkExtras(t, name, obj, extras.AsMap())
				}
			}
		})
	}
}

// TestGoldenJournals drives the workflow-journal fixtures.
func TestGoldenJournals(t *testing.T) {
	root := corpusRoot(t)
	files, _ := filepath.Glob(filepath.Join(root, "journals", "*.jsonl"))
	if len(files) == 0 {
		t.Fatal("no journal fixtures")
	}
	for _, f := range files {
		name := "journals/" + filepath.Base(f)
		t.Run(name, func(t *testing.T) {
			c := New(func(string, ...any) {})
			for _, obj := range readLines(t, f) {
				rec, extras, err := c.JournalRecord(obj)
				if err != nil {
					t.Fatalf("%s: hard conversion error: %v", name, err)
				}
				if rec.GetRecord() == nil {
					t.Fatalf("%s: journal record has no arm", name)
				}
				if extras != nil {
					checkExtras(t, name, obj, extras.AsMap())
				}
			}
		})
	}
}

// TestGoldenSidechain drives the agent sidechain transcript + its meta.json and
// the agent-task spool (a*.output, itself sidechain JSONL).
func TestGoldenSidechain(t *testing.T) {
	root := corpusRoot(t)
	jsonl, _ := filepath.Glob(filepath.Join(root, "sidechain", "*.jsonl"))
	jsonl2, _ := filepath.Glob(filepath.Join(root, "spools", "agent.output"))
	for _, f := range append(jsonl, jsonl2...) {
		name := filepath.Base(filepath.Dir(f)) + "/" + filepath.Base(f)
		t.Run(name, func(t *testing.T) {
			c := New(func(string, ...any) {})
			for _, obj := range readLines(t, f) {
				line, extras, err := c.TranscriptLine(obj)
				if err != nil {
					t.Fatalf("%s: hard conversion error: %v", name, err)
				}
				if line.GetLine() == nil {
					t.Fatalf("%s: no oneof arm", name)
				}
				if extras != nil {
					checkExtras(t, name, obj, extras.AsMap())
				}
			}
		})
	}
	// meta.json companions.
	metas, _ := filepath.Glob(filepath.Join(root, "sidechain", "*.meta.json"))
	for _, f := range metas {
		name := "sidechain/" + filepath.Base(f)
		t.Run(name, func(t *testing.T) {
			c := New(func(string, ...any) {})
			data, err := os.ReadFile(f)
			if err != nil {
				t.Fatalf("read: %v", err)
			}
			var obj map[string]any
			if err := json.Unmarshal(data, &obj); err != nil {
				t.Fatalf("json: %v", err)
			}
			_, extras := c.AgentMeta(obj)
			if extras != nil {
				checkExtras(t, name, obj, extras.AsMap())
			}
		})
	}
}

// TestListValueFieldAbsorbsArray covers the singular google.protobuf.ListValue
// path: a JSON array (ApiUsage.iterations) is decoded as a typed ListValue with
// no extras, rather than being captured verbatim.
func TestListValueFieldAbsorbsArray(t *testing.T) {
	// Arrange
	c := New(nil)
	obj := map[string]any{
		"type": "assistant",
		"message": map[string]any{
			"role": "assistant",
			"usage": map[string]any{
				"iterations": []any{
					map[string]any{"input_tokens": float64(1), "output_tokens": float64(2)},
				},
			},
		},
	}
	// Act
	line, extras, err := c.TranscriptLine(obj)
	// Assert
	if err != nil {
		t.Fatalf("conversion error: %v", err)
	}
	if extras != nil {
		t.Fatalf("expected zero extras, got %v", extras.AsMap())
	}
	usage := line.GetAssistant().GetMessage().GetUsage()
	if usage.GetIterations() == nil {
		t.Fatal("iterations ListValue not populated")
	}
	if got := len(usage.GetIterations().GetValues()); got != 1 {
		t.Fatalf("iterations length = %d, want 1", got)
	}
}

// TestHookBlockingErrorRoutesOuterDetail covers the split routing: the disk
// blockingError object lands in the OUTER blocking_error detail while the other
// keys populate the wrapped HookSuccessAttachment, with no extras.
func TestHookBlockingErrorRoutesOuterDetail(t *testing.T) {
	// Arrange
	c := New(nil)
	obj := map[string]any{
		"type": "attachment",
		"attachment": map[string]any{
			"type":      "hook_blocking_error",
			"hookName":  "PostToolUse:Edit",
			"toolUseID": "toolu_x",
			"hookEvent": "PostToolUse",
			"blockingError": map[string]any{
				"blockingError": "tests failed",
				"command":       "run-tests.sh",
			},
		},
	}
	// Act
	line, extras, err := c.TranscriptLine(obj)
	// Assert
	if err != nil {
		t.Fatalf("conversion error: %v", err)
	}
	if extras != nil {
		t.Fatalf("expected zero extras, got %v", extras.AsMap())
	}
	att := line.GetAttachment().GetHookBlockingError()
	if att.GetBlockingError().GetBlockingError() != "tests failed" {
		t.Fatalf("outer blocking_error not routed: %+v", att.GetBlockingError())
	}
	if att.GetBlockingError().GetCommand() != "run-tests.sh" {
		t.Fatalf("outer blocking_error command not routed: %+v", att.GetBlockingError())
	}
	if att.GetFields().GetHookName() != "PostToolUse:Edit" {
		t.Fatalf("wrapped fields.hook_name not populated: %+v", att.GetFields())
	}
}

// TestPermissionRuleDenialKind covers the additive enum value: the corpus string
// "permission-rule" now resolves to the typed ToolDenialKind rather than extras.
func TestPermissionRuleDenialKind(t *testing.T) {
	// Arrange
	c := New(nil)
	obj := map[string]any{
		"type":           "user",
		"toolDenialKind": "permission-rule",
		"message":        map[string]any{"role": "user", "content": "x"},
	}
	// Act
	line, extras, err := c.TranscriptLine(obj)
	// Assert
	if err != nil {
		t.Fatalf("conversion error: %v", err)
	}
	if extras != nil {
		t.Fatalf("expected zero extras, got %v", extras.AsMap())
	}
	got := line.GetUser().GetEnvelope().GetToolDenialKind()
	if got != datav1.ToolDenialKind_TOOL_DENIAL_KIND_PERMISSION_RULE {
		t.Fatalf("tool_denial_kind = %v, want PERMISSION_RULE", got)
	}
}

// TestAutomodeUnavailableDenialKind pins the fourth ToolDenialKind value, which
// the deployed sidecar loud-logged as an unmodeled enum value on real
// transcripts (27 occurrences) before it was added.
func TestAutomodeUnavailableDenialKind(t *testing.T) {
	// Arrange
	c := New(nil)
	obj := map[string]any{
		"type":           "user",
		"toolDenialKind": "automode-unavailable",
		"message":        map[string]any{"role": "user", "content": "x"},
	}
	// Act
	line, extras, err := c.TranscriptLine(obj)
	// Assert
	if err != nil {
		t.Fatalf("conversion error: %v", err)
	}
	if extras != nil {
		t.Fatalf("expected zero extras, got %v", extras.AsMap())
	}
	got := line.GetUser().GetEnvelope().GetToolDenialKind()
	if got != datav1.ToolDenialKind_TOOL_DENIAL_KIND_AUTOMODE_UNAVAILABLE {
		t.Fatalf("tool_denial_kind = %v, want AUTOMODE_UNAVAILABLE", got)
	}
}

// TestSidecarLoggedFieldsNowTyped covers the tool-result fields the DEPLOYED
// sidecar loud-logged as unknown on real transcripts. The sidecar's reflective
// assign resolves them by canonical name, so these pin that it really does pick
// each newly-added proto field up — populated and absent (AAA, one field per
// case).
func TestSidecarLoggedFieldsNowTyped(t *testing.T) {
	tests := []struct {
		name     string
		result   map[string]any
		populate map[string]any
		want     func(r *datav1.ToolUseResult) any
		wantSet  any
		wantZero any
	}{
		{
			name:     "bash dangerouslyDisableSandbox",
			result:   map[string]any{"stdout": "", "stderr": "", "interrupted": false},
			populate: map[string]any{"dangerouslyDisableSandbox": true},
			want:     func(r *datav1.ToolUseResult) any { return r.GetBash().GetDangerouslyDisableSandbox() },
			wantSet:  true, wantZero: false,
		},
		{
			name:     "bash backgroundedByUser",
			result:   map[string]any{"stdout": "", "stderr": "", "interrupted": false},
			populate: map[string]any{"backgroundedByUser": true},
			want:     func(r *datav1.ToolUseResult) any { return r.GetBash().GetBackgroundedByUser() },
			wantSet:  true, wantZero: false,
		},
		{
			name:     "write memdirStamped",
			result:   map[string]any{"type": "update", "filePath": "/f", "content": "c", "structuredPatch": []any{}},
			populate: map[string]any{"memdirStamped": true},
			want:     func(r *datav1.ToolUseResult) any { return r.GetWrite().GetMemdirStamped() },
			wantSet:  true, wantZero: false,
		},
		{
			name:     "schedule_wakeup stopped",
			result:   map[string]any{"scheduledFor": float64(1), "clampedDelaySeconds": float64(60)},
			populate: map[string]any{"stopped": true},
			want:     func(r *datav1.ToolUseResult) any { return r.GetScheduleWakeup().GetStopped() },
			wantSet:  true, wantZero: false,
		},
		{
			name:     "schedule_wakeup cancelledWakeups",
			result:   map[string]any{"scheduledFor": float64(1), "clampedDelaySeconds": float64(60)},
			populate: map[string]any{"cancelledWakeups": float64(2)},
			want:     func(r *datav1.ToolUseResult) any { return r.GetScheduleWakeup().GetCancelledWakeups() },
			wantSet:  int64(2), wantZero: int64(0),
		},
		{
			name:     "ask_user_question afkTimeoutMs",
			result:   map[string]any{"questions": []any{}, "answers": map[string]any{}},
			populate: map[string]any{"afkTimeoutMs": float64(60000)},
			want:     func(r *datav1.ToolUseResult) any { return r.GetAskUserQuestion().GetAfkTimeoutMs() },
			wantSet:  int64(60000), wantZero: int64(0),
		},
		{
			name:     "agent worktreePath",
			result:   map[string]any{"agentType": "general-purpose", "totalDurationMs": float64(1), "totalToolUseCount": float64(1)},
			populate: map[string]any{"worktreePath": "/w/tree"},
			want:     func(r *datav1.ToolUseResult) any { return r.GetAgent().GetWorktreePath() },
			wantSet:  "/w/tree", wantZero: "",
		},
		{
			name:     "agent worktreeBranch",
			result:   map[string]any{"agentType": "general-purpose", "totalDurationMs": float64(1), "totalToolUseCount": float64(1)},
			populate: map[string]any{"worktreeBranch": "worktree-agent-a1"},
			want:     func(r *datav1.ToolUseResult) any { return r.GetAgent().GetWorktreeBranch() },
			wantSet:  "worktree-agent-a1", wantZero: "",
		},
	}
	for _, tc := range tests {
		t.Run(tc.name+"/populated", func(t *testing.T) {
			// Arrange
			obj := map[string]any{}
			for k, v := range tc.result {
				obj[k] = v
			}
			for k, v := range tc.populate {
				obj[k] = v
			}
			c := New(nil)
			// Act
			res, extras := c.ToolUseResult(obj)
			// Assert
			if extras != nil {
				t.Fatalf("expected zero extras, got %v", extras.AsMap())
			}
			if got := tc.want(res); got != tc.wantSet {
				t.Fatalf("field = %v, want %v", got, tc.wantSet)
			}
		})
		t.Run(tc.name+"/absent", func(t *testing.T) {
			// Arrange
			c := New(nil)
			// Act
			res, extras := c.ToolUseResult(tc.result)
			// Assert
			if extras != nil {
				t.Fatalf("expected zero extras, got %v", extras.AsMap())
			}
			if got := tc.want(res); got != tc.wantZero {
				t.Fatalf("field = %v, want zero %v", got, tc.wantZero)
			}
		})
	}
}

// TestRetypedFieldsNowDecode covers the four fields RETYPED IN PLACE under the
// user's explicit approval. Each previously had a proto type the disk never
// matched, so the converter captured every value into extras; each must now
// decode as typed data with ZERO extras. Populated + absent per field (AAA).
func TestRetypedFieldsNowDecode(t *testing.T) {
	userLine := func(extra map[string]any) map[string]any {
		obj := map[string]any{
			"type":    "user",
			"message": map[string]any{"role": "user", "content": "x"},
		}
		for k, v := range extra {
			obj[k] = v
		}
		return obj
	}
	tests := []struct {
		name     string
		envelope map[string]any
		lineType string
		get      func(l *datav1.TranscriptLine) any
		want     any
	}{
		{
			name:     "classifier_meta_lines populated (raw NDJSON text)",
			envelope: map[string]any{"classifierMetaLines": "{\"meta\":{\"gitStatus\":{\"clean\":true}}}\n"},
			get: func(l *datav1.TranscriptLine) any {
				return l.GetUser().GetEnvelope().GetClassifierMetaLines()
			},
			want: "{\"meta\":{\"gitStatus\":{\"clean\":true}}}\n",
		},
		{
			name:     "classifier_meta_lines absent",
			envelope: map[string]any{},
			get: func(l *datav1.TranscriptLine) any {
				return l.GetUser().GetEnvelope().GetClassifierMetaLines()
			},
			want: "",
		},
		{
			name:     "error_details populated (status code plus raw body)",
			envelope: map[string]any{"errorDetails": `429 {"type":"error"}`},
			get: func(l *datav1.TranscriptLine) any {
				return l.GetUser().GetEnvelope().GetErrorDetails()
			},
			want: `429 {"type":"error"}`,
		},
		{
			name:     "error_details absent",
			envelope: map[string]any{},
			get: func(l *datav1.TranscriptLine) any {
				return l.GetUser().GetEnvelope().GetErrorDetails()
			},
			want: "",
		},
		{
			name:     "queue_priority populated (named priority, not a rank)",
			envelope: map[string]any{"queuePriority": "later"},
			get: func(l *datav1.TranscriptLine) any {
				return l.GetUser().GetEnvelope().GetQueuePriority()
			},
			want: "later",
		},
		{
			name:     "queue_priority absent",
			envelope: map[string]any{},
			get: func(l *datav1.TranscriptLine) any {
				return l.GetUser().GetEnvelope().GetQueuePriority()
			},
			want: "",
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			c := New(nil)
			// Act
			line, extras, err := c.TranscriptLine(userLine(tc.envelope))
			// Assert
			if err != nil {
				t.Fatalf("conversion error: %v", err)
			}
			if extras != nil {
				t.Fatalf("expected zero extras, got %v", extras.AsMap())
			}
			if got := tc.get(line); got != tc.want {
				t.Fatalf("field = %q, want %q", got, tc.want)
			}
		})
	}
}

// TestGitOperationDecodes pins BashResult.git_operation, retyped in place from
// string to a typed GitOperation. The sidecar's reflective assign has to walk
// the nested arms and their enums, so these assert it really does (AAA).
func TestGitOperationDecodes(t *testing.T) {
	bash := func(op any) map[string]any {
		r := map[string]any{"stdout": "", "stderr": "", "interrupted": false}
		if op != nil {
			r["gitOperation"] = op
		}
		return r
	}
	tests := []struct {
		name   string
		op     any
		assert func(t *testing.T, g *datav1.GitOperation)
	}{
		{
			name: "commit sha and kind",
			op:   map[string]any{"commit": map[string]any{"sha": "868db15d", "kind": "committed"}},
			assert: func(t *testing.T, g *datav1.GitOperation) {
				if g.GetCommit().GetSha() != "868db15d" ||
					g.GetCommit().GetKind() != datav1.GitCommitKind_GIT_COMMIT_KIND_COMMITTED {
					t.Fatalf("commit = %+v", g.GetCommit())
				}
			},
		},
		{
			name: "hyphenated commit kind",
			op:   map[string]any{"commit": map[string]any{"sha": "x", "kind": "cherry-picked"}},
			assert: func(t *testing.T, g *datav1.GitOperation) {
				if g.GetCommit().GetKind() != datav1.GitCommitKind_GIT_COMMIT_KIND_CHERRY_PICKED {
					t.Fatalf("kind = %v", g.GetCommit().GetKind())
				}
			},
		},
		{
			name: "branch ref and action",
			op:   map[string]any{"branch": map[string]any{"ref": "master", "action": "rebased"}},
			assert: func(t *testing.T, g *datav1.GitOperation) {
				if g.GetBranch().GetRef() != "master" ||
					g.GetBranch().GetAction() != datav1.GitBranchAction_GIT_BRANCH_ACTION_REBASED {
					t.Fatalf("branch = %+v", g.GetBranch())
				}
			},
		},
		{
			name: "pr number url and action",
			op:   map[string]any{"pr": map[string]any{"number": float64(9155), "url": "https://x/y", "action": "created"}},
			assert: func(t *testing.T, g *datav1.GitOperation) {
				if g.GetPr().GetNumber() != 9155 || g.GetPr().GetUrl() != "https://x/y" ||
					g.GetPr().GetAction() != datav1.GitPullRequestAction_GIT_PULL_REQUEST_ACTION_CREATED {
					t.Fatalf("pr = %+v", g.GetPr())
				}
			},
		},
		{
			name: "hyphenated pr action",
			op:   map[string]any{"pr": map[string]any{"number": float64(1), "action": "auto-merge-enabled"}},
			assert: func(t *testing.T, g *datav1.GitOperation) {
				want := datav1.GitPullRequestAction_GIT_PULL_REQUEST_ACTION_AUTO_MERGE_ENABLED
				if g.GetPr().GetAction() != want {
					t.Fatalf("action = %v, want %v", g.GetPr().GetAction(), want)
				}
			},
		},
		{
			name: "two arms at once (why it is not a oneof)",
			op: map[string]any{
				"commit": map[string]any{"sha": "abc", "kind": "committed"},
				"push":   map[string]any{"branch": "b"},
			},
			assert: func(t *testing.T, g *datav1.GitOperation) {
				if g.GetCommit().GetSha() != "abc" || g.GetPush().GetBranch() != "b" {
					t.Fatalf("commit+push = %+v", g)
				}
			},
		},
		{
			name: "absent",
			op:   nil,
			assert: func(t *testing.T, g *datav1.GitOperation) {
				if g != nil {
					t.Fatalf("git_operation = %+v, want nil", g)
				}
			},
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			c := New(nil)
			// Act
			res, extras := c.ToolUseResult(bash(tc.op))
			// Assert
			if extras != nil {
				t.Fatalf("expected zero extras, got %v", extras.AsMap())
			}
			tc.assert(t, res.GetBash().GetGitOperation())
		})
	}
}

// TestClassifyToolResultArms pins the ToolUseResult classifier to the arm the
// MANIFEST documents for each tool-results fixture (AAA, one arm per case).
func TestClassifyToolResultArms(t *testing.T) {
	root := corpusRoot(t)
	// fixture basename → expected ToolUseResult arm (the oneof field name), or
	// "" for the unclassified Struct arm.
	cases := map[string]string{
		"agent.jsonl":                        "agent",
		"agent_async_launch.jsonl":           "agent_async_launch",
		"ask_user_question.jsonl":            "ask_user_question",
		"bash.jsonl":                         "bash",
		"bash-background.jsonl":              "bash",
		"edit.jsonl":                         "edit",
		"monitor.jsonl":                      "monitor",
		"read.jsonl":                         "read",
		"read-image.jsonl":                   "read",
		"schedule_wakeup.jsonl":              "schedule_wakeup",
		"send_message.jsonl":                 "send_message",
		"skill.jsonl":                        "skill",
		"task_create.jsonl":                  "task_create",
		"task_list.jsonl":                    "task_list",
		"task_output.jsonl":                  "task_output",
		"task_output-local_agent.jsonl":      "task_output",
		"task_stop.jsonl":                    "task_stop",
		"task_update.jsonl":                  "task_update",
		"tool_search.jsonl":                  "tool_search",
		"web_fetch.jsonl":                    "web_fetch",
		"web_search.jsonl":                   "web_search",
		"workflow_launch.jsonl":              "workflow_launch",
		"write.jsonl":                        "write",
		"raw_string.jsonl":                   "raw_string",
		"unclassified-message_success.jsonl": "",
		"unclassified-path_title_url.jsonl":  "",
	}
	names := make([]string, 0, len(cases))
	for n := range cases {
		names = append(names, n)
	}
	sort.Strings(names)
	for _, base := range names {
		want := cases[base]
		t.Run(base, func(t *testing.T) {
			// Arrange
			objs := readLines(t, filepath.Join(root, "tool-results", base))
			obj := objs[0]
			tur, ok := obj["toolUseResult"]
			if !ok {
				t.Fatalf("%s: fixture has no toolUseResult", base)
			}
			c := New(func(string, ...any) {})
			// Act
			res, _ := c.ToolUseResult(tur)
			// Assert
			got := whichResultArm(res)
			if got != want {
				t.Fatalf("%s: classified as %q, want %q", base, got, want)
			}
		})
	}
}

// TestTypedToolResultShapes pins the tool-result fields that were tightened from
// a schemaless Struct/ListValue to a typed shape: each must decode from its
// GOLDEN FIXTURE as typed data with zero extras. A future corpus shape the typed
// message cannot express would surface here as an undocumented extra (and in
// TestGoldenTranscriptLines), which is the discipline the empty knownSchemaGaps
// map exists to enforce.
func TestTypedToolResultShapes(t *testing.T) {
	root := corpusRoot(t)
	tests := []struct {
		name    string
		fixture string
		assert  func(t *testing.T, r *datav1.ToolUseResult)
	}{
		{
			name:    "task_update status_change decodes as TaskStatusChange",
			fixture: "task_update.jsonl",
			assert: func(t *testing.T, r *datav1.ToolUseResult) {
				sc := r.GetTaskUpdate().GetStatusChange()
				if sc.GetFrom() != "pending" || sc.GetTo() != "in_progress" {
					t.Fatalf("status_change = %+v, want {from:pending to:in_progress}", sc)
				}
			},
		},
		{
			name:    "task_update updated_fields decodes as repeated string",
			fixture: "task_update.jsonl",
			assert: func(t *testing.T, r *datav1.ToolUseResult) {
				got := r.GetTaskUpdate().GetUpdatedFields()
				if len(got) != 1 || got[0] != "status" {
					t.Fatalf("updated_fields = %v, want [status]", got)
				}
			},
		},
		{
			name:    "task_output dispatches its task oneof to local_bash",
			fixture: "task_output.jsonl",
			assert: func(t *testing.T, r *datav1.ToolUseResult) {
				got := r.GetTaskOutput().GetLocalBash()
				if got.GetTaskId() != "b86pl7ir1" {
					t.Fatalf("local_bash = %+v, want task_id b86pl7ir1", got)
				}
			},
		},
		{
			name:    "task_output dispatches its task oneof to local_agent",
			fixture: "task_output-local_agent.jsonl",
			assert: func(t *testing.T, r *datav1.ToolUseResult) {
				got := r.GetTaskOutput().GetLocalAgent()
				if got.GetTaskId() != "a0cbd94e5da2d662d" {
					t.Fatalf("local_agent = %+v, want task_id a0cbd94e5da2d662d", got)
				}
			},
		},
		{
			name:    "send_message pin decodes as MessagePin",
			fixture: "send_message.jsonl",
			assert: func(t *testing.T, r *datav1.ToolUseResult) {
				pin := r.GetSendMessage().GetPin()
				if pin.GetId() != "acd910f5fefb75908" ||
					pin.GetName() != "acd910f5fefb75908" ||
					pin.GetRef() != "2175c2" {
					t.Fatalf("pin = %+v, want {id/name:acd910f5fefb75908 ref:2175c2}", pin)
				}
			},
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			objs := readLines(t, filepath.Join(root, "tool-results", tc.fixture))
			c := New(func(string, ...any) {})
			// Act
			res, extras := c.ToolUseResult(objs[0]["toolUseResult"])
			// Assert
			if extras != nil {
				t.Fatalf("expected zero extras, got %v", extras.AsMap())
			}
			tc.assert(t, res)
		})
	}
}

// TestFileAttachmentContentTyped pins FileAttachment.content, tightened from a
// Struct to AttachedFileContent{type, file:AttachedFileBody}: the golden
// attachment fixture must decode every field as typed data with zero extras.
func TestFileAttachmentContentTyped(t *testing.T) {
	// Arrange
	root := corpusRoot(t)
	objs := readLines(t, filepath.Join(root, "attachments", "file.jsonl"))
	c := New(func(string, ...any) {})
	// Act
	line, extras, err := c.TranscriptLine(objs[0])
	// Assert
	if err != nil {
		t.Fatalf("conversion error: %v", err)
	}
	if extras != nil {
		t.Fatalf("expected zero extras, got %v", extras.AsMap())
	}
	content := line.GetAttachment().GetFile().GetContent()
	if content.GetType() != "text" {
		t.Fatalf("content.type = %q, want %q", content.GetType(), "text")
	}
	body := content.GetFile()
	if !strings.HasSuffix(body.GetFilePath(), "approval-vs-verification-semantics.md") {
		t.Fatalf("content.file.file_path = %q, want the fixture's memory path", body.GetFilePath())
	}
	if body.GetContent() == "" {
		t.Fatal("content.file.content decoded empty")
	}
	if body.GetNumLines() != 16 || body.GetStartLine() != 1 || body.GetTotalLines() != 16 {
		t.Fatalf("content.file line bounds = {num:%d start:%d total:%d}, want {16 1 16}",
			body.GetNumLines(), body.GetStartLine(), body.GetTotalLines())
	}
}
