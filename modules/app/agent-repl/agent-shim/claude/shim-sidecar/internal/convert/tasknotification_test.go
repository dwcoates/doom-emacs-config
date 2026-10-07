package convert

// tasknotification_test.go — a background task's notification settles the
// spawn it names on the spawn's own key, as the stream plane's task_notification
// does, and is never a prompt.

import (
	"encoding/json"
	"strings"
	"testing"
	"time"

	storev1 "agentrepl/proto/store/v1"
)

// The captured notification's own identities (transcript-lines/
// user-task-notification.jsonl).
const (
	notifiedTask     = "aca335b8c99998251"
	notifiedCall     = "toolu_01FAamQEcjJ3KZKDFc5QRcoG"
	notifiedTokens   = 262422
	notifiedSummary  = `Agent "Map iOS CEE usage and o11y" finished`
	notificationFile = "transcript-lines/user-task-notification.jsonl"
)

// agentLaunchLines are the backgrounded Agent call the captured notification
// names, and its launch receipt.
func agentLaunchLines() []string {
	call := assistantWith("a0", "msg_0", ts1, toolCall(notifiedCall, "Agent", `{"description":"Map iOS","prompt":"map it"}`))
	launched := toolResultLine("u0", notifiedCall, ts1, `[{"type":"text","text":"launched"}]`,
		`{"isAsync":true,"agentId":"`+notifiedTask+`","outputFile":"/tmp/`+notifiedTask+`.output"}`)
	return []string{call, launched}
}

// notificationWithStatus is the captured notification with its status swapped.
func notificationWithStatus(t *testing.T, status string) string {
	t.Helper()
	line := corpusLine(t, notificationFile)
	return strings.Replace(line, `<status>completed</status>`, `<status>`+status+`</status>`, 1)
}

// settleOf converts the launch then the notification, and answers the
// notification's own entries.
func settleOf(t *testing.T, notification string) []*storev1.StoreEntry {
	t.Helper()
	c := newTestConverter(t)
	lines := append(agentLaunchLines(), notification)
	entries := convertLines(t, c, lines...)
	launched := convertLines(t, newTestConverter(t), agentLaunchLines()...)
	return entries[len(launched):]
}

func TestCompletedNotificationSettlesTheSpawnAsSuccess(t *testing.T) {
	// Arrange.
	line := corpusLine(t, notificationFile)

	// Act.
	entries := settleOf(t, line)

	// Assert.
	if rows := promptRows(entries); len(rows) != 0 {
		t.Fatalf("minted %d prompt row(s) for a task notification", len(rows))
	}
	success := activityOf(entryByKey(t, entries, ActivityKey(notifiedCall))).GetSubagent().GetSuccess()
	if success == nil {
		t.Fatal("a completed notification must settle the spawn's success arm")
	}
	if got := success.GetReport().GetProse().GetMarkdown(); got != notifiedSummary {
		t.Fatalf("report = %q, want the notification's summary %q", got, notifiedSummary)
	}
}

func TestCompletedNotificationRestatesTheLaunchesCommission(t *testing.T) {
	// Arrange. A settle stands alone: a replay serves it with no start beside it.
	line := corpusLine(t, notificationFile)

	// Act.
	entries := settleOf(t, line)

	// Assert.
	success := activityOf(entryByKey(t, entries, ActivityKey(notifiedCall))).GetSubagent().GetSuccess()
	if got := success.GetPrompt().GetText(); got != "map it" {
		t.Fatalf("prompt = %q, want the launch's commission", got)
	}
	if got := success.GetCreatedAgentId().GetValue(); got != notifiedCall {
		t.Fatalf("created agent = %q, want the spawning call %q (the minting rule)", got, notifiedCall)
	}
	if got := success.GetSettledAt().GetStartedAt().GetAtMs(); got != parseInstant(ts1) {
		t.Fatalf("restated start = %d, want the spawning call's instant %d", got, parseInstant(ts1))
	}
}

func TestCompletedNotificationCarriesTheReportedTokenTotal(t *testing.T) {
	// Arrange.
	line := corpusLine(t, notificationFile)

	// Act.
	entries := settleOf(t, line)

	// Assert.
	totals := activityOf(entryByKey(t, entries, ActivityKey(notifiedCall))).GetSubagent().GetSuccess().GetTotals()
	if got := totals.GetTotalOnly().GetTotalTokens(); got != notifiedTokens {
		t.Fatalf("total tokens = %d, want %d", got, notifiedTokens)
	}
}

func TestNotificationWithNoUsageLeavesTheTokenTotalUnset(t *testing.T) {
	// Arrange. An unreported total is never zero.
	line := corpusLine(t, notificationFile)
	start := strings.Index(line, `<usage>`)
	end := strings.Index(line, `</usage>`) + len(`</usage>`)
	line = line[:start] + line[end:]

	// Act.
	entries := settleOf(t, line)

	// Assert.
	totals := activityOf(entryByKey(t, entries, ActivityKey(notifiedCall))).GetSubagent().GetSuccess().GetTotals()
	if totals.GetTotalOnly().TotalTokens != nil {
		t.Fatalf("total tokens = %d, want UNSET", totals.GetTotalOnly().GetTotalTokens())
	}
}

func TestFailedNotificationSettlesTheSpawnAsFailureCarryingTheSummary(t *testing.T) {
	// Arrange.
	line := notificationWithStatus(t, taskStatusFailed)

	// Act.
	entries := settleOf(t, line)

	// Assert.
	failure := activityOf(entryByKey(t, entries, ActivityKey(notifiedCall))).GetSubagent().GetFailure()
	if failure == nil || failure.GetStoppedByUser() != nil {
		t.Fatalf("failure = %v, want a failure that is not a person's stop", failure)
	}
	if got := failure.GetError().GetContent().GetBlocks()[0].GetText().GetText(); got != notifiedSummary {
		t.Fatalf("error content = %q, want the summary %q", got, notifiedSummary)
	}
}

func TestStoppedNotificationSettlesTheSpawnAsStoppedByUser(t *testing.T) {
	// Arrange.
	line := notificationWithStatus(t, taskStatusStopped)

	// Act.
	entries := settleOf(t, line)

	// Assert.
	failure := activityOf(entryByKey(t, entries, ActivityKey(notifiedCall))).GetSubagent().GetFailure()
	if failure.GetStoppedByUser() == nil {
		t.Fatalf("failure = %v, want stopped_by_user", failure)
	}
}

func TestNotificationWithAStatusNoArmModelsIsWithheld(t *testing.T) {
	// Arrange.
	line := notificationWithStatus(t, "running")

	// Act.
	entries := settleOf(t, line)

	// Assert.
	if len(entries) != 1 || vendorKindOf(entries[0]) != kindUserTaskNotification {
		t.Fatalf("entries = %v, want one %s residue", allKeys(entries), kindUserTaskNotification)
	}
}

func TestNotificationNamingNoSpawningCallIsWithheld(t *testing.T) {
	// Arrange. A monitor's event names a task but no call.
	line := corpusLine(t, notificationFile)
	line = strings.Replace(line, `<tool-use-id>`+notifiedCall+`</tool-use-id>`, ``, 1)

	// Act.
	entries := settleOf(t, line)

	// Assert.
	if len(entries) != 1 || vendorKindOf(entries[0]) != kindUserTaskNotification {
		t.Fatalf("entries = %v, want one %s residue", allKeys(entries), kindUserTaskNotification)
	}
}

func TestShellRunNotificationIsWithheldForItsSpoolToSettle(t *testing.T) {
	// Arrange. A detached shell's terminal is its spool's EXIT line, which
	// carries the output the notification lacks.
	c := newTestConverter(t)
	call := assistantWith("a0", "msg_0", ts1, toolCall("toolu_sh", "Bash", `{"command":"sleep 1","run_in_background":true}`))
	launched := toolResultLine("u0", "toolu_sh", ts1, `[{"type":"text","text":"started"}]`,
		`{"stdout":"","backgroundTaskId":"bsh1","outputFile":"/tmp/bsh1.output"}`)
	notice := `{"type":"user","uuid":"n1","isSidechain":false,"entrypoint":"cli","origin":{"kind":"task-notification"},"timestamp":"` + ts2 +
		`","message":{"role":"user","content":"<task-notification>\n<task-id>bsh1</task-id>\n<tool-use-id>toolu_sh</tool-use-id>\n<status>completed</status>\n</task-notification>"}}`

	// Act.
	entries := convertLines(t, c, call, launched, notice)

	// Assert.
	last := entries[len(entries)-1]
	if vendorKindOf(last) != kindUserTaskNotification {
		t.Fatalf("kind = %q, want %s", vendorKindOf(last), kindUserTaskNotification)
	}
}

func TestShellRunNotificationReportsItsConclusionToTheReader(t *testing.T) {
	// Arrange. The reader writes a shell run's one terminal, so the fact has to
	// reach it with the status and the instant the vendor wrote.
	c := newTestConverter(t)
	var stopped, concluded []string
	var shells []shellConclusion
	c.SetObserver(recordingObserver{stopped: &stopped, concluded: &concluded, shells: &shells})
	call := assistantWith("a0", "msg_0", ts1, toolCall("toolu_sh", "Bash", `{"command":"sleep 1","run_in_background":true}`))
	launched := toolResultLine("u0", "toolu_sh", ts1, `[{"type":"text","text":"started"}]`,
		`{"stdout":"","backgroundTaskId":"bsh1","outputFile":"/tmp/bsh1.output"}`)
	notice := `{"type":"user","uuid":"n1","isSidechain":false,"entrypoint":"cli","origin":{"kind":"task-notification"},"timestamp":"` + ts2 +
		`","message":{"role":"user","content":"<task-notification>\n<task-id>bsh1</task-id>\n<tool-use-id>toolu_sh</tool-use-id>\n<status>failed</status>\n</task-notification>"}}`
	wantAt, err := time.Parse(time.RFC3339Nano, ts2)
	if err != nil {
		t.Fatalf("parse ts2: %v", err)
	}

	// Act.
	convertLines(t, c, call, launched, notice)

	// Assert.
	want := []shellConclusion{{taskID: "bsh1", status: "failed", atMs: wantAt.UnixMilli()}}
	if len(shells) != 1 || shells[0] != want[0] {
		t.Fatalf("shell conclusions = %v, want %v", shells, want)
	}
	if len(concluded) != 0 {
		t.Fatalf("agent conclusions = %v, want none: a shell run is not an agent run", concluded)
	}
}

func TestAgentRunNotificationReportsNoShellConclusion(t *testing.T) {
	// Arrange.
	c := newTestConverter(t)
	var stopped, concluded []string
	var shells []shellConclusion
	c.SetObserver(recordingObserver{stopped: &stopped, concluded: &concluded, shells: &shells})
	lines := append(agentLaunchLines(), corpusLine(t, notificationFile))

	// Act.
	convertLines(t, c, lines...)

	// Assert.
	if len(shells) != 0 {
		t.Fatalf("shell conclusions = %v, want none for an agent run", shells)
	}
}

func TestNotificationSettlesOnTheKeyTheStreamPlaneSettlesOn(t *testing.T) {
	// Arrange. The shim settles a run from task_notification under
	// `activity:<tool_use_id>` (shim/src/store/keys.ts activityUpsertKey), and the
	// launch's start on this plane rides the same key: the two planes' rows for
	// one run converge on one row.
	c := newTestConverter(t)
	lines := append(agentLaunchLines(), corpusLine(t, notificationFile))

	// Act.
	entries := convertLines(t, c, lines...)

	// Assert.
	var start, settle bool
	for _, e := range entries {
		if e.GetUpsertKey() != "activity:"+notifiedCall {
			continue
		}
		subagent := activityOf(e).GetSubagent()
		start = start || subagent.GetStart() != nil
		settle = settle || subagent.GetSuccess() != nil
	}
	if !start || !settle {
		t.Fatalf("start=%t settle=%t under activity:%s, want both on the one key (keys: %v)", start, settle, notifiedCall, allKeys(entries))
	}
}

func TestSdkCliNotificationSettlesTheSpawnToo(t *testing.T) {
	// Arrange. R15 withholds agent-repl's own PROMPTS; a notification is not one,
	// so an agent-repl session's replayed history settles its spawns the same way.
	line := strings.Replace(corpusLine(t, notificationFile), `"entrypoint":"cli"`, `"entrypoint":"sdk-cli"`, 1)

	// Act.
	entries := settleOf(t, line)

	// Assert.
	if activityOf(entryByKey(t, entries, ActivityKey(notifiedCall))).GetSubagent().GetSuccess() == nil {
		t.Fatal("an sdk-cli notification must settle its backgrounded spawn")
	}
}

func TestASettlingNotificationReportsTheRunConcluded(t *testing.T) {
	tests := []struct {
		name   string
		status string
		want   []string
	}{
		{name: "completed", status: "completed", want: []string{notifiedTask}},
		{name: "failed", status: "failed", want: []string{notifiedTask}},
		{name: "stopped", status: "stopped", want: []string{notifiedTask}},
		{name: "a status no arm settles", status: "paused", want: nil},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			c := newTestConverter(t)
			var stopped, concluded []string
			c.SetObserver(recordingObserver{stopped: &stopped, concluded: &concluded})
			lines := append(agentLaunchLines(), notificationWithStatus(t, tt.status))

			// Act.
			convertLines(t, c, lines...)

			// Assert.
			if strings.Join(concluded, ",") != strings.Join(tt.want, ",") {
				t.Fatalf("conclusions reported = %v, want %v", concluded, tt.want)
			}
		})
	}
}

// asQueuedCommand rewrites a user-role notification line into the form the
// vendor writes for a notification that arrived MID-TURN: an `attachment`
// record of type `queued_command` carrying the notification in `prompt`.
func asQueuedCommand(t *testing.T, userLine string) string {
	t.Helper()
	var record map[string]any
	if err := json.Unmarshal([]byte(userLine), &record); err != nil {
		t.Fatalf("decode the notification line: %v", err)
	}
	message, _ := record["message"].(map[string]any)
	prompt, _ := message["content"].(string)
	delete(record, "message")
	delete(record, "origin")
	record["type"] = "attachment"
	record["attachment"] = map[string]any{
		"type":        "queued_command",
		"commandMode": "task-notification",
		"prompt":      prompt,
	}
	out, err := json.Marshal(record)
	if err != nil {
		t.Fatalf("encode the queued command: %v", err)
	}
	return string(out)
}

// withoutToolUseID drops the notification's `<tool-use-id>` element, as the
// vendor writes a stop for a run the previous process left behind.
func withoutToolUseID(line string) string {
	return strings.Replace(line, `<tool-use-id>`+notifiedCall+`</tool-use-id>\n`, "", 1)
}

func TestAMidTurnNotificationSettlesTheSpawn(t *testing.T) {
	// Arrange: the 2026-10-07 regression's encoding.
	line := asQueuedCommand(t, corpusLine(t, notificationFile))

	// Act.
	entries := settleOf(t, line)

	// Assert.
	if activityOf(entryByKey(t, entries, ActivityKey(notifiedCall))).GetSubagent().GetSuccess() == nil {
		t.Fatal("a queued_command notification must settle the spawn's success arm")
	}
}

func TestAQueuedCommandCarryingNoNotificationIsWithheld(t *testing.T) {
	// Arrange: a command the user queued mid-turn, which is not a notification.
	var record map[string]any
	if err := json.Unmarshal([]byte(asQueuedCommand(t, corpusLine(t, notificationFile))), &record); err != nil {
		t.Fatalf("decode the queued command: %v", err)
	}
	record["attachment"] = map[string]any{"type": "queued_command", "commandMode": "prompt", "prompt": "also check the logs"}
	raw, err := json.Marshal(record)
	if err != nil {
		t.Fatalf("encode the queued command: %v", err)
	}
	line := string(raw)

	// Act.
	entries := settleOf(t, line)

	// Assert.
	for _, entry := range entries {
		if entry.GetUpsertKey() == ActivityKey(notifiedCall) {
			t.Fatalf("a queued command with no notification settled the spawn: %+v", entry)
		}
	}
}

func TestAStopNamingNoCallSettlesTheRunItsTaskNamesAsRestarted(t *testing.T) {
	// Arrange: the resumed process's account of a run it found orphaned.
	line := withoutToolUseID(notificationWithStatus(t, "stopped"))

	// Act.
	entries := settleOf(t, line)

	// Assert.
	failure := activityOf(entryByKey(t, entries, ActivityKey(notifiedCall))).GetSubagent().GetFailure()
	if failure.GetWorkerRestarted() == nil {
		t.Fatalf("failure = %+v, want the run settled worker_restarted", failure)
	}
}

func TestAStopNamingNoCallAndNoLaunchedTaskIsWithheld(t *testing.T) {
	// Arrange: a stop for a task this stream never launched.
	line := strings.Replace(withoutToolUseID(notificationWithStatus(t, "stopped")), notifiedTask, "a-task-never-launched", 1)

	// Act.
	entries := settleOf(t, line)

	// Assert.
	for _, entry := range entries {
		if entry.GetUpsertKey() == ActivityKey(notifiedCall) {
			t.Fatalf("a stop naming an unknown task settled a spawn: %+v", entry)
		}
	}
}
