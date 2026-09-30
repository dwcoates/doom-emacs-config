package convert

import (
	"bytes"
	"io"
	"strings"
	"testing"

	"agentrepl/shim-claude-sidecar/internal/logging"
)

// launch_test.go — WHAT A LAUNCH REPORTS TO OWNER RESOLUTION.
//
// `backgrounded` is the one fact on the observation that nothing downstream can
// re-derive without reading the launch result a second time, and it is what
// decides a subagent's `top_level`. It was reported as nothing at all until the
// a*-spool top_level subject caught it: the field existed on the reader's
// observation, `topLevel` branched on it, and no producer ever set it — so a
// backgrounded subagent's frames named the session's main agent, which is the
// exact reading AGENTS.md forbids.

// spawnObserver records the spawn observations a conversion reports.
type spawnObserver struct {
	spawns *[]spawnReport
}

// spawnReport is one reported launch.
type spawnReport struct {
	TaskID       string
	ToolUseID    string
	OwnerAgentID string
	OutputPath   string
	Backgrounded bool
}

func (o spawnObserver) TaskSpawned(taskID, toolUseID, ownerAgentID, outputPath string, backgrounded bool) {
	*o.spawns = append(*o.spawns, spawnReport{taskID, toolUseID, ownerAgentID, outputPath, backgrounded})
}

func (o spawnObserver) TaskConcluded(string) {}

func (o spawnObserver) ShellConcluded(string, string, int64) {}

func (o spawnObserver) TaskStopped(string) {}

// TestALaunchReportsWhetherTheSpawnWasBackgrounded states the rule over the
// three launch signatures the vendor writes: only the async AGENT launch is a
// backgrounded spawn.
func TestALaunchReportsWhetherTheSpawnWasBackgrounded(t *testing.T) {
	for _, tc := range []struct {
		name string
		// toolUseResult is the launch signature, verbatim.
		toolUseResult  string
		wantTask       string
		wantBackground bool
	}{
		{
			name:           "an async Agent launch is backgrounded",
			toolUseResult:  `{"isAsync":true,"agentId":"a15b5267244c1360e","outputFile":"/tmp/a15.output"}`,
			wantTask:       "a15b5267244c1360e",
			wantBackground: true,
		},
		{
			name:           "a detached shell run is not a backgrounded AGENT",
			toolUseResult:  `{"backgroundTaskId":"bbkqcvn8k","outputFile":"/tmp/b.output"}`,
			wantTask:       "bbkqcvn8k",
			wantBackground: false,
		},
		{
			name:           "a workflow run is not a backgrounded AGENT",
			toolUseResult:  `{"runId":"wf_0001","transcriptDir":"/tmp/wf"}`,
			wantTask:       "wf_0001",
			wantBackground: false,
		},
	} {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			c := newTestConverter(t)
			var spawns []spawnReport
			c.SetObserver(spawnObserver{spawns: &spawns})
			call := assistantWith("a1", "msg_1", ts1, toolCall("toolu_spawn", "Agent", `{"description":"d","prompt":"p"}`))
			result := toolResultLine("u1", "toolu_spawn", ts2, `[{"type":"text","text":"launched"}]`, tc.toolUseResult)

			// Act.
			convertLines(t, c, call, result)

			// Assert.
			if len(spawns) != 1 {
				t.Fatalf("the launch reported %d spawn observations, want exactly one: %v", len(spawns), spawns)
			}
			if got := spawns[0].TaskID; got != tc.wantTask {
				t.Errorf("the observation names task %q, want %q", got, tc.wantTask)
			}
			if got := spawns[0].Backgrounded; got != tc.wantBackground {
				t.Errorf("the observation reports backgrounded=%v, want %v; it is what decides the spawned agent's top_level",
					got, tc.wantBackground)
			}
		})
	}
}

// backgroundSentenceText is the vendor's backgrounding sentence, verbatim from
// the 2026-09-23 subagent transcript whose toolUseResult was absent.
const backgroundSentenceText = `Command running in background with ID: bmo77o6cu. Output is being written to: /private/tmp/claude-501/p/s/tasks/bmo77o6cu.output. You will be notified when it completes.`

// timeoutSentenceText is the vendor's sentence for a foreground shell its own
// timeout moved to the background, verbatim from the 2026-09-28 subagent
// transcript whose toolUseResult was absent.
const timeoutSentenceText = `Command did not complete within its 600s timeout and was moved to the background (ID: b3e6urkt6). Output is being written to: /private/tmp/claude-501/p/s/tasks/b3e6urkt6.output.`

// TestTheTimeoutSentenceRestatesTheLimitItExceeded pins the restated launch's
// shape for a timeout: the task id and the limit, as the vendor's structured
// result states them.
func TestTheTimeoutSentenceRestatesTheLimitItExceeded(t *testing.T) {
	// Arrange.
	block := map[string]any{"content": timeoutSentenceText}

	// Act.
	launch, ok := backgroundLaunchFromProse(block)

	// Assert.
	if !ok || launch["backgroundTaskId"] != "b3e6urkt6" || launch["timedOutAfterMs"] != float64(600_000) {
		t.Fatalf("launch = %v (ok %t), want b3e6urkt6 timed out after 600000ms", launch, ok)
	}
}

// TestAShellLaunchIsReportedFromItsSentenceWhenTheStructuredResultIsAbsent
// pins the file-plane half of a subagent's background shell: a subagent's
// transcript can omit `toolUseResult` outright, and the sentence is then the
// only statement that the call launched a task. Unreported, the spool was held
// until it expired into residue and the store never held the run's rows.
func TestAShellLaunchIsReportedFromItsSentenceWhenTheStructuredResultIsAbsent(t *testing.T) {
	for _, tc := range []struct {
		name string
		// content is the tool_result block's content, as JSON.
		content string
		// toolUseResult is the structured result, "" when the record has none.
		toolUseResult string
		wantSpawns    []spawnReport
	}{
		{
			name:          "the sentence names the launched task when no structured result exists",
			content:       `[{"type":"text","text":"` + backgroundSentenceText + `"}]`,
			toolUseResult: "",
			wantSpawns:    []spawnReport{{TaskID: "bmo77o6cu", ToolUseID: "toolu_bg", OwnerAgentID: "session-uuid"}},
		},
		{
			name:          "a plain string content carries the same sentence",
			content:       `"` + backgroundSentenceText + `"`,
			toolUseResult: "",
			wantSpawns:    []spawnReport{{TaskID: "bmo77o6cu", ToolUseID: "toolu_bg", OwnerAgentID: "session-uuid"}},
		},
		{
			name:          "the timeout sentence names the task the vendor's timeout moved",
			content:       `[{"type":"text","text":"` + timeoutSentenceText + `"}]`,
			toolUseResult: "",
			wantSpawns:    []spawnReport{{TaskID: "b3e6urkt6", ToolUseID: "toolu_bg", OwnerAgentID: "session-uuid"}},
		},
		{
			name:          "a result that states no launch reports nothing",
			content:       `[{"type":"text","text":"ok"}]`,
			toolUseResult: "",
			wantSpawns:    nil,
		},
		{
			name:          "a structured result is read rather than the sentence",
			content:       `[{"type":"text","text":"` + backgroundSentenceText + `"}]`,
			toolUseResult: `{"stdout":"","backgroundTaskId":"bstructured"}`,
			wantSpawns:    []spawnReport{{TaskID: "bstructured", ToolUseID: "toolu_bg", OwnerAgentID: "session-uuid"}},
		},
	} {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			c := newTestConverter(t)
			var spawns []spawnReport
			c.SetObserver(spawnObserver{spawns: &spawns})
			call := assistantWith("a1", "msg_1", ts1, toolCall("toolu_bg", "Bash", `{"command":"npm test","run_in_background":true}`))
			result := toolResultLine("u1", "toolu_bg", ts2, tc.content, tc.toolUseResult)

			// Act.
			convertLines(t, c, call, result)

			// Assert.
			if len(spawns) != len(tc.wantSpawns) {
				t.Fatalf("the result reported %d spawn observations, want %d: %v", len(spawns), len(tc.wantSpawns), spawns)
			}
			for i, want := range tc.wantSpawns {
				if spawns[i] != want {
					t.Errorf("spawn %d = %+v, want %+v", i, spawns[i], want)
				}
			}
		})
	}
}

// TestASentenceOnlyLaunchDoesNotSettleTheCallAsAFinishedCommand pins the other
// half: the call's work LEFT, so its unit settles later — never as a command
// that succeeded with the sentence as its output.
func TestASentenceOnlyLaunchDoesNotSettleTheCallAsAFinishedCommand(t *testing.T) {
	// Arrange.
	c := newTestConverter(t)
	call := assistantWith("a1", "msg_1", ts1, toolCall("toolu_bg", "Bash", `{"command":"npm test","run_in_background":true}`))
	result := toolResultLine("u1", "toolu_bg", ts2, `[{"type":"text","text":"`+backgroundSentenceText+`"}]`, "")

	// Act.
	entries := convertLines(t, c, call, result)

	// Assert.
	for _, entry := range entries {
		if bash := activityOf(entry).GetBash(); bash.GetSuccess() != nil {
			t.Fatalf("the backgrounded call settled as a finished command: %v", bash.GetSuccess())
		}
	}
}

// TestASentenceOnlyLaunchIsLogged pins the record: reading the launch off the
// sentence is an action a reader must be able to see.
func TestASentenceOnlyLaunchIsLogged(t *testing.T) {
	// Arrange.
	var file bytes.Buffer
	c := New(logging.New(io.Discard, &file).With(logging.Context{Component: "test"}))
	call := assistantWith("a1", "msg_1", ts1, toolCall("toolu_bg", "Bash", `{"command":"npm test","run_in_background":true}`))
	result := toolResultLine("u1", "toolu_bg", ts2, `[{"type":"text","text":"`+backgroundSentenceText+`"}]`, "")

	// Act.
	convertLines(t, c, call, result)

	// Assert.
	if !strings.Contains(file.String(), "its backgrounding sentence names the launched task") ||
		!strings.Contains(file.String(), "bmo77o6cu") {
		t.Fatalf("no record of the sentence-read launch naming its task; log:\n%s", file.String())
	}
}
