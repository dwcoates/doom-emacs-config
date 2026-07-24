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
// Keys are canonical leaf names (see convert.canon); "content" is special-cased
// in isKnownGap by parent because "content" is a valid modeled field elsewhere.
var knownSchemaGaps = map[string]string{
	"iterations":               "ApiUsage: add repeated per-iteration usage (usage.iterations)",
	"speed":                    "ApiUsage: add string speed",
	"inferencegeo":             "ApiUsage: add string inference_geo",
	"tooldenialkind":           "ToolDenialKind enum: add value for \"permission-rule\"",
	"precompactdiscoveredtools": "DiskCompactMetadata: add repeated string pre_compact_discovered_tools",
	"cumulativedroppedtokens":  "DiskCompactMetadata: add int64 cumulative_dropped_tokens",
	"structuredpatch":          "Edit/WriteResult.structured_patch: model as ListValue/repeated (disk is an array, proto is Struct)",
	"questions":                "AskUserQuestionResult.questions: model as ListValue (disk is an array, proto is Struct)",
	"results":                  "WebSearchResult.results: model as ListValue (disk is an array, proto is Struct)",
	"tasks":                    "TaskListResult.tasks: model as ListValue (disk is an array, proto is Struct)",
	"updatedfields":            "TaskUpdateResult.updated_fields: model as ListValue (disk is an array, proto is Struct)",
	"statuschange":             "TaskUpdateResult.status_change: model as Struct (disk is an object, proto is string)",
	"pin":                      "SendMessageResult.pin: model as Struct (disk is an object, proto is bool)",
	"scheduledfor":             "ScheduleWakeupResult.scheduled_for: model as int64 (disk is an int, proto is string)",
	"blockingerror":            "HookSuccessAttachment: add blocking_error Struct (hook_blocking_error carries it)",
	"automodeconsentflow":      "AutoModeAttachment.auto_mode_consent_flow: model as bool (disk is a bool, proto is Struct)",
	"files":                    "DiagnosticsAttachment.files: model as ListValue (disk is an array, proto is Struct)",
}

// isKnownGap reports whether an extras dotted-path is a documented schema gap.
func isKnownGap(path string) bool {
	segs := strings.Split(path, ".")
	leaf := stripIndex(segs[len(segs)-1])
	parent := ""
	if len(segs) >= 2 {
		parent = stripIndex(segs[len(segs)-2])
	}
	if canon(leaf) == "content" {
		// FileAttachment.content (object on disk) and TaskReminderAttachment.content
		// (array on disk) are both modeled as string.
		return canon(parent) == "file" || canon(parent) == "taskreminder"
	}
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

// TestClassifyToolResultArms pins the ToolUseResult classifier to the arm the
// MANIFEST documents for each tool-results fixture (AAA, one arm per case).
func TestClassifyToolResultArms(t *testing.T) {
	root := corpusRoot(t)
	// fixture basename → expected ToolUseResult arm (the oneof field name), or
	// "" for the unclassified Struct arm.
	cases := map[string]string{
		"agent.jsonl":                      "agent",
		"agent_async_launch.jsonl":         "agent_async_launch",
		"ask_user_question.jsonl":          "ask_user_question",
		"bash.jsonl":                       "bash",
		"bash-background.jsonl":            "bash",
		"edit.jsonl":                       "edit",
		"monitor.jsonl":                    "monitor",
		"read.jsonl":                       "read",
		"read-image.jsonl":                 "read",
		"schedule_wakeup.jsonl":            "schedule_wakeup",
		"send_message.jsonl":               "send_message",
		"skill.jsonl":                      "skill",
		"task_create.jsonl":                "task_create",
		"task_list.jsonl":                  "task_list",
		"task_stop.jsonl":                  "task_stop",
		"task_update.jsonl":                "task_update",
		"tool_search.jsonl":                "tool_search",
		"web_fetch.jsonl":                  "web_fetch",
		"web_search.jsonl":                 "web_search",
		"workflow_launch.jsonl":            "workflow_launch",
		"write.jsonl":                      "write",
		"raw_string.jsonl":                 "raw_string",
		"unclassified-message_success.jsonl":  "",
		"unclassified-path_title_url.jsonl":   "",
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
