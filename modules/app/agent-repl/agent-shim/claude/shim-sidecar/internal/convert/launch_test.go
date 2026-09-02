package convert

import "testing"

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
