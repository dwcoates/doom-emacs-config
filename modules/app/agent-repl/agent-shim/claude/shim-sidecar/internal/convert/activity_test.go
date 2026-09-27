package convert

// activity_test.go — the item mapping across the activity vocabulary: each
// recognized built-in reaching its OWN arm rather than the unmodeled fallback.

import (
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"
)

func TestRecognizedBuiltinsReachTheirOwnArm(t *testing.T) {
	// Arrange. A RECOGNIZABLE BUILT-IN ARRIVING AT AgentUnmodeled IS A PRODUCER
	// DEFECT, so this table is what keeps the vocabulary honest as tools are added.
	cases := []struct {
		tool  string
		input string
		arm   func(*conversationv1.AgentActivity) bool
	}{
		{tool: "Read", input: `{"file_path":"/f"}`, arm: func(a *conversationv1.AgentActivity) bool { return a.GetRead() != nil }},
		{tool: "Write", input: `{"file_path":"/f","content":"x"}`, arm: func(a *conversationv1.AgentActivity) bool { return a.GetWrite() != nil }},
		{tool: "Edit", input: `{"file_path":"/f"}`, arm: func(a *conversationv1.AgentActivity) bool { return a.GetEdit() != nil }},
		{tool: "Grep", input: `{"pattern":"x"}`, arm: func(a *conversationv1.AgentActivity) bool { return a.GetGrep() != nil }},
		{tool: "Glob", input: `{"pattern":"**"}`, arm: func(a *conversationv1.AgentActivity) bool { return a.GetGlob() != nil }},
		{tool: "Bash", input: `{"command":"ls"}`, arm: func(a *conversationv1.AgentActivity) bool { return a.GetBash() != nil }},
		{tool: "Skill", input: `{"skill":"g"}`, arm: func(a *conversationv1.AgentActivity) bool { return a.GetSkillUse() != nil }},
		{tool: "SendMessage", input: `{"to":"x","message":"m"}`, arm: func(a *conversationv1.AgentActivity) bool { return a.GetSendMessage() != nil }},
		{tool: "WebFetch", input: `{"url":"https://x"}`, arm: func(a *conversationv1.AgentActivity) bool { return a.GetWebFetch() != nil }},
		{tool: "WebSearch", input: `{"query":"x"}`, arm: func(a *conversationv1.AgentActivity) bool { return a.GetWebSearch() != nil }},
		{tool: "Monitor", input: `{"description":"d","command":"c"}`, arm: func(a *conversationv1.AgentActivity) bool { return a.GetMonitor() != nil }},
		{tool: "ScheduleWakeup", input: `{"delay_seconds":5,"reason":"r"}`, arm: func(a *conversationv1.AgentActivity) bool { return a.GetScheduleWakeup() != nil }},
		{tool: "Artifact", input: `{"file_path":"/p.html"}`, arm: func(a *conversationv1.AgentActivity) bool { return a.GetArtifact() != nil }},
		{tool: "EnterPlanMode", input: `{}`, arm: func(a *conversationv1.AgentActivity) bool { return a.GetPlanMode() != nil }},
		{tool: "ExitPlanMode", input: `{"plan":"p"}`, arm: func(a *conversationv1.AgentActivity) bool { return a.GetPlanMode() != nil }},
		{tool: "ReportFindings", input: `{"findings":[]}`, arm: func(a *conversationv1.AgentActivity) bool { return a.GetReportFindings() != nil }},
		{tool: "EnterWorktree", input: `{}`, arm: func(a *conversationv1.AgentActivity) bool { return a.GetWorktree() != nil }},
		{tool: "ExitWorktree", input: `{}`, arm: func(a *conversationv1.AgentActivity) bool { return a.GetWorktree() != nil }},
		{tool: "CronCreate", input: `{"cron":"* * * * *","prompt":"p"}`, arm: func(a *conversationv1.AgentActivity) bool { return a.GetCron() != nil }},
		{tool: "CronDelete", input: `{"job_id":"j"}`, arm: func(a *conversationv1.AgentActivity) bool { return a.GetCron() != nil }},
		{tool: "CronList", input: `{}`, arm: func(a *conversationv1.AgentActivity) bool { return a.GetCron() != nil }},
		{tool: "PushNotification", input: `{"message":"m"}`, arm: func(a *conversationv1.AgentActivity) bool { return a.GetPushNotification() != nil }},
		{tool: "SubagentHandback", input: `{"message":"m"}`, arm: func(a *conversationv1.AgentActivity) bool { return a.GetSubagentHandback() != nil }},
	}
	for _, tc := range cases {
		t.Run(tc.tool, func(t *testing.T) {
			c := newTestConverter(t)

			// Act.
			entries := convertLines(t, c, assistantWith("a1", "msg_1", ts1, toolCall("toolu_x", tc.tool, tc.input)))

			// Assert.
			entry := entryByKey(t, entries, ActivityKey("toolu_x"))
			activity := activityOf(entry)
			if activity.GetUnmodeled() != nil {
				t.Fatalf("%s reached AgentUnmodeled, which is a producer defect for a recognized built-in", tc.tool)
			}
			if !tc.arm(activity) {
				t.Fatalf("%s did not reach its own arm", tc.tool)
			}
		})
	}
}

func TestSandboxDisabledIsStatedRatherThanAssumed(t *testing.T) {
	// Arrange. CONSENT-RELEVANT: a command that ran with the sandbox deliberately
	// disabled reached the host directly and is otherwise indistinguishable.
	c := newTestConverter(t)

	// Act.
	entries := convertLines(t, c, assistantWith("a1", "msg_1", ts1,
		toolCall("toolu_b", "Bash", `{"command":"rm -rf /","dangerouslyDisableSandbox":true}`)))

	// Assert.
	command := activityOf(entryByKey(t, entries, ActivityKey("toolu_b"))).GetBash().GetStart().GetCommand()
	if command.GetSandboxDisabled() == nil {
		t.Fatal("a deliberately disabled sandbox must be STATED")
	}
}

func TestAbsentSandboxReportStaysUnsetNeverSandboxed(t *testing.T) {
	// Arrange. UNSET means the producer reported nothing either way, and a
	// consumer must NOT read absence as "sandboxed".
	c := newTestConverter(t)

	// Act.
	entries := convertLines(t, c, assistantWith("a1", "msg_1", ts1, toolCall("toolu_b", "Bash", `{"command":"ls"}`)))

	// Assert.
	command := activityOf(entryByKey(t, entries, ActivityKey("toolu_b"))).GetBash().GetStart().GetCommand()
	if command.GetSandbox() != nil {
		t.Fatal("an unreported sandbox must stay UNSET rather than claiming the command was sandboxed")
	}
}

func TestPersistentMonitorCarriesNoTimeout(t *testing.T) {
	// Arrange. The arms are exclusive by the VENDOR'S OWN RULE: the timeout is
	// ignored when the watch is persistent.
	c := newTestConverter(t)

	// Act.
	entries := convertLines(t, c, assistantWith("a1", "msg_1", ts1,
		toolCall("toolu_m", "Monitor", `{"description":"d","command":"c","persistent":true,"timeout":9999}`)))

	// Assert.
	start := activityOf(entryByKey(t, entries, ActivityKey("toolu_m"))).GetMonitor().GetStart()
	if start.GetPersistent() == nil {
		t.Fatal("a persistent watch must land on the persistent arm")
	}
	if start.GetDeadline() != nil {
		t.Fatal("a persistent watch must carry no deadline: the vendor ignores it")
	}
}

func TestWakeupStopIgnoresTheScheduleFields(t *testing.T) {
	// Arrange. Every schedule field is ignored when stop is set, again by the
	// vendor's own rule.
	c := newTestConverter(t)

	// Act.
	entries := convertLines(t, c, assistantWith("a1", "msg_1", ts1,
		toolCall("toolu_w", "ScheduleWakeup", `{"stop":true,"delay_seconds":30,"reason":"r"}`)))

	// Assert.
	start := activityOf(entryByKey(t, entries, ActivityKey("toolu_w"))).GetScheduleWakeup().GetStart()
	if start.GetStop() == nil {
		t.Fatal("stop must land on the stop arm")
	}
	if start.GetSchedule() != nil {
		t.Fatal("a stop must not also carry a schedule")
	}
}

func TestWorktreeExitStatesWhatWasAskedForTheTree(t *testing.T) {
	// Arrange.
	c := newTestConverter(t)

	// Act.
	entries := convertLines(t, c, assistantWith("a1", "msg_1", ts1,
		toolCall("toolu_x", "ExitWorktree", `{"remove":true,"discard_changes":true}`)))

	// Assert.
	exit := activityOf(entryByKey(t, entries, ActivityKey("toolu_x"))).GetWorktree().GetStart().GetExit()
	if exit.GetRemove() == nil {
		t.Fatal("a removal request must land on the remove arm")
	}
	if !exit.GetRemove().GetDiscardChanges() {
		t.Fatal("discard_changes must be carried: it is the only signal uncommitted work was thrown away")
	}
}

func TestPlanModeExitWithNoEnterIsLegal(t *testing.T) {
	// Arrange. A session started in the plan permission mode never calls
	// EnterPlanMode at all, so an exit standing alone must convert cleanly.
	c := newTestConverter(t)

	// Act.
	entries := convertLines(t, c, assistantWith("a1", "msg_1", ts1, toolCall("toolu_x", "ExitPlanMode", `{"plan":"the plan"}`)))

	// Assert.
	start := activityOf(entryByKey(t, entries, ActivityKey("toolu_x"))).GetPlanMode().GetStart()
	if start.GetExit() == nil {
		t.Fatal("an exit with no enter must still convert as an exit")
	}
}

func TestGrepQueryCarriesTheFlagsThatAreInvisibleInItsOutput(t *testing.T) {
	// Arrange. Case-insensitivity is invisible in the rendered result: the same
	// output could have come from either mode, and only this says which.
	c := newTestConverter(t)
	input := `{"pattern":"x","path":"/r","glob":"*.go","type":"go","-i":true,"multiline":true}`

	// Act.
	entries := convertLines(t, c, assistantWith("a1", "msg_1", ts1, toolCall("toolu_g", "Grep", input)))

	// Assert.
	query := activityOf(entryByKey(t, entries, ActivityKey("toolu_g"))).GetGrep().GetStart().GetQuery()
	if !query.GetCaseInsensitive() {
		t.Fatal("case-insensitivity must be carried")
	}
	if !query.GetMultiline() {
		t.Fatal("multiline must be carried")
	}
	if got := query.GetFileType(); got != "go" {
		t.Fatalf("file_type = %q, want go", got)
	}
	if got := query.GetPath(); got != "/r" {
		t.Fatalf("path = %q, want /r", got)
	}
}

func TestUnnamedGrepScopeStaysUnsetRatherThanInvented(t *testing.T) {
	// Arrange. UNSET means the caller named no root and the performer chose one,
	// so a consumer draws no scope rather than a default it cannot know.
	c := newTestConverter(t)

	// Act.
	entries := convertLines(t, c, assistantWith("a1", "msg_1", ts1, toolCall("toolu_g", "Grep", `{"pattern":"x"}`)))

	// Assert.
	query := activityOf(entryByKey(t, entries, ActivityKey("toolu_g"))).GetGrep().GetStart().GetQuery()
	if query.Path != nil {
		t.Fatal("an unnamed search root must stay UNSET")
	}
	if query.Glob != nil || query.FileType != nil {
		t.Fatal("absent filters must stay UNSET, which is different from a filter matching everything")
	}
}

// A sandbox the vendor reported AS ENABLED is stated too, not merely inferred
// from the absence of a disable. The three states — sandboxed, disabled, and
// unreported — are what a consent reader has to be able to tell apart.
func TestTheReportedSandboxStateIsCarriedThrough(t *testing.T) {
	cases := []struct {
		name  string
		input string
		want  func(*conversationv1.AgentBashCommand) bool
	}{
		{
			name:  "the vendor reported the sandbox on",
			input: `{"command":"ls","sandbox":true}`,
			want:  func(c *conversationv1.AgentBashCommand) bool { return c.GetSandboxed() != nil },
		},
		{
			name:  "the vendor reported the sandbox off",
			input: `{"command":"ls","sandbox":false}`,
			want:  func(c *conversationv1.AgentBashCommand) bool { return c.GetSandboxDisabled() != nil },
		},
		{
			name:  "the vendor wrote something that is not a bool",
			input: `{"command":"ls","sandbox":"maybe"}`,
			want:  func(c *conversationv1.AgentBashCommand) bool { return c.GetSandbox() == nil },
		},
		{
			name:  "an explicit disable overrides a reported sandbox",
			input: `{"command":"ls","sandbox":true,"dangerouslyDisableSandbox":true}`,
			want:  func(c *conversationv1.AgentBashCommand) bool { return c.GetSandboxDisabled() != nil },
		},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			c := newTestConverter(t)

			// Act.
			entries := convertLines(t, c, assistantWith("a1", "msg_1", ts1,
				toolCall("toolu_b", "Bash", tc.input)))

			// Assert.
			command := activityOf(entryByKey(t, entries, ActivityKey("toolu_b"))).GetBash().GetStart().GetCommand()
			if !tc.want(command) {
				t.Fatalf("sandbox arm = %v, which is not what the vendor reported", command.GetSandbox())
			}
		})
	}
}

// An Artifact LISTING reads its own fields and ignores the publish's, which is
// exactly why the two are exclusive arms rather than one flat message.
func TestAnArtifactListingLandsOnTheListArmWithItsOwnFields(t *testing.T) {
	// Arrange.
	c := newTestConverter(t)

	// Act.
	entries := convertLines(t, c, assistantWith("a1", "msg_1", ts1,
		toolCall("toolu_a", "Artifact", `{"action":"list","limit":25,"scope":"shared"}`)))

	// Assert.
	start := activityOf(entryByKey(t, entries, ActivityKey("toolu_a"))).GetArtifact().GetStart()
	listing := start.GetList()
	if listing == nil {
		t.Fatalf("a list action landed on %v, not the list arm", start.GetAct())
	}
	if listing.GetLimit() != 25 {
		t.Fatalf("limit = %d, want 25", listing.GetLimit())
	}
	if listing.GetScope() != "shared" {
		t.Fatalf("scope = %q, want %q", listing.GetScope(), "shared")
	}
}

// A subagent's hand-back announces the REPORT it carries, read off the real
// corpus call: the call's input is the only place the report exists.
func TestAHandbackStartCarriesTheCorpusReportVerbatim(t *testing.T) {
	// Arrange.
	c := newTestConverter(t)
	call := assistantWith("a1", "msg_1", ts1, corpusLine(t, "tool-inputs/subagent_handback.jsonl"))
	want := corpusToolInputField(t, "tool-inputs/subagent_handback.jsonl", "message")

	// Act.
	entries := convertLines(t, c, call)

	// Assert.
	start := activityOf(entryByKey(t, entries, ActivityKey("toolu_01XimbQmvHTszbgRxyRy5VEf"))).GetSubagentHandback().GetStart()
	if start.GetReport().GetText() != want {
		t.Fatalf("report = %q, want the corpus message verbatim", start.GetReport().GetText())
	}
}
