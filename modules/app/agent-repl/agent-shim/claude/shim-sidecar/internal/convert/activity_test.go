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

func TestAskUserQuestionRidesTheUpdateNotTheActivityEnvelope(t *testing.T) {
	// Arrange. A question is NOT read-only work: it BLOCKS the agent until the
	// user writes back, so it rides AgentUpdate directly and is keyed in its own
	// identity space.
	c := newTestConverter(t)
	input := `{"questions":[{"question":"Which?","header":"Pick","multiSelect":false,` +
		`"options":[{"label":"A","description":"first"},{"label":"B","description":"second"}]}]}`

	// Act.
	entries := convertLines(t, c, assistantWith("a1", "msg_1", ts1, toolCall("toolu_ask", "AskUserQuestion", input)))

	// Assert.
	entry := entryByKey(t, entries, QuestionKey("toolu_ask"))
	question := frameOf(entry).GetUpdate().GetQuestion()
	if question == nil {
		t.Fatal("an ask must land on AgentUpdate.question, never inside the activity envelope")
	}
	if frameOf(entry).GetUpdate().GetActivity() != nil {
		t.Fatal("an ask is not an activity")
	}
	asked := question.GetStart().GetBatch().GetQuestions()
	if len(asked) != 1 {
		t.Fatalf("questions = %d, want 1", len(asked))
	}
	if got := asked[0].GetQuestion().GetText(); got != "Which?" {
		t.Fatalf("question text = %q, want it verbatim (it is the producer's own answer key)", got)
	}
	if asked[0].GetSingleSelect() == nil {
		t.Fatal("multiSelect false must land on the single-select arm")
	}
	if got := len(asked[0].GetSingleSelect().GetOptions()); got != 2 {
		t.Fatalf("options = %d, want 2", got)
	}
}

func TestMultiSelectModeIsReadPerQuestion(t *testing.T) {
	// Arrange. THE MODE IS PER QUESTION, NEVER PER ASK: one batch can mix a
	// pick-one with a pick-any, so it must not be lifted to the batch.
	c := newTestConverter(t)
	input := `{"questions":[` +
		`{"question":"one","header":"h","multiSelect":false,"options":[{"label":"A"}]},` +
		`{"question":"many","header":"h","multiSelect":true,"options":[{"label":"B"}]}]}`

	// Act.
	entries := convertLines(t, c, assistantWith("a1", "msg_1", ts1, toolCall("toolu_ask", "AskUserQuestion", input)))

	// Assert.
	asked := frameOf(entryByKey(t, entries, QuestionKey("toolu_ask"))).GetUpdate().GetQuestion().GetStart().GetBatch().GetQuestions()
	if len(asked) != 2 {
		t.Fatalf("questions = %d, want 2", len(asked))
	}
	if asked[0].GetSingleSelect() == nil {
		t.Fatal("the first question must be single-select")
	}
	if asked[1].GetMultiSelect() == nil {
		t.Fatal("the second question must be multi-select")
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
