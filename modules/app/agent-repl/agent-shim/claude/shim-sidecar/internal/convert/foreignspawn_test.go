package convert

import (
	"fmt"
	"testing"
)

// taskNotificationLine is the queue-operation carrier the harness writes about a
// background agent, naming the run's spool under the LAUNCHING session.
func taskNotificationLine(taskID, launchingSession string) string {
	body := fmt.Sprintf("<task-notification>\\n<task-id>%s</task-id>\\n<output-file>/tmp/proj/%s/tasks/%s.output</output-file>\\n<status>completed</status>\\n</task-notification>",
		taskID, launchingSession, taskID)
	return fmt.Sprintf(`{"type":"queue-operation","operation":"enqueue","content":%q}`, body)
}

// taskStopLines are the call and result that settle an agent task.
func taskStopLines(taskID string) (string, string) {
	call := assistantWith("a1", "msg_1", ts1, toolCall("toolu_stop", "TaskStop", fmt.Sprintf(`{"task_id":%q}`, taskID)))
	result := toolResultLine("u1", "toolu_stop", ts2, `[{"type":"text","text":"stopped"}]`,
		fmt.Sprintf(`{"command":"stop","task_type":"local_agent","task_id":%q,"message":"stopped"}`, taskID))
	return call, result
}

// TestAnUnlaunchedAgentStopIsInfoWhenTheLaunchWasWrittenToAnotherStream is the
// third answer to "why is there no launch", and the one the join-offset split
// missed: the vendor's background agents survive a `/clear`, so a transcript
// read FROM BYTE 0 can still carry a stop whose launch was written to the
// PREVIOUS session's file. The vendor says so itself — the run's spool sits
// under the launching session's directory — so the reader states it instead of
// warning about a gap it was never in a position to have.
func TestAnUnlaunchedAgentStopIsInfoWhenTheLaunchWasWrittenToAnotherStream(t *testing.T) {
	cases := []struct {
		name        string
		spoolOwner  string
		wantLevel   string
		wantMessage string
	}{
		{
			name:        "the spool belongs to another session, so its launch is in another file",
			spoolOwner:  "37dc1374-2566-474f-bd04-faee69848fc8",
			wantLevel:   "debug",
			wantMessage: "launched, so its launch was written to that session's transcript",
		},
		{
			name:        "the spool belongs to this session, so the missing launch is a real gap",
			spoolOwner:  "session-uuid",
			wantLevel:   "warn",
			wantMessage: "no launch on this stream opened",
		},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			c, sink := loggedConverter(t)
			notification := taskNotificationLine("a80b994f0e2c791cc", tc.spoolOwner)
			call, result := taskStopLines("a80b994f0e2c791cc")

			// Act: read from byte 0, so the join-offset carve-out cannot apply.
			entries := convertLinesFrom(t, c, 0, notification, call, result)

			// Assert: the residue is identical either way; only the severity moves.
			last := entries[len(entries)-1]
			if got := last.GetAgentUpdate().GetUnservedItem().GetVendorSpecific().GetKind(); got != "task_stop/unlaunched" {
				t.Fatalf("kind = %q, want task_stop/unlaunched in both cases", got)
			}
			if got := levelForMessage(t, sink, tc.wantMessage); got != tc.wantLevel {
				t.Fatalf("the unattributable stop was recorded at %q, want %q", got, tc.wantLevel)
			}
		})
	}
}

// TestAForeignSpawnIsAlsoLearnedFromTheAttachmentCarrier pins the SECOND place
// the harness writes the same notification. The queue-operation announces what
// was queued and the attachment delivers it, and a reader whose window holds
// only the delivery must learn the same fact from it.
func TestAForeignSpawnIsAlsoLearnedFromTheAttachmentCarrier(t *testing.T) {
	// Arrange.
	c, sink := loggedConverter(t)
	body := "<task-notification>\\n<task-id>a7ae9d98acf3bd5c0</task-id>\\n<output-file>/tmp/proj/37dc1374/tasks/a7ae9d98acf3bd5c0.output</output-file>\\n</task-notification>"
	attachment := fmt.Sprintf(`{"type":"attachment","uuid":"att1","attachment":{"type":"queued_command","prompt":%q}}`, body)
	call, result := taskStopLines("a7ae9d98acf3bd5c0")

	// Act.
	convertLinesFrom(t, c, 0, attachment, call, result)

	// Assert.
	if got := levelForMessage(t, sink, "launched, so its launch was written to that session's transcript"); got != "debug" {
		t.Fatalf("the stop was recorded at %q, want debug (the attachment named the launching session)", got)
	}
}

// TestASpoolPathThatNamesNoTasksDirectoryTeachesNothing keeps the inference
// honest: the segment above a spool is the launching session ONLY because the
// vendor puts spools in a `tasks` directory. A path shaped any other way must
// not be read as naming a session, or an unrelated directory name would demote
// a genuine gap to info.
func TestASpoolPathThatNamesNoTasksDirectoryTeachesNothing(t *testing.T) {
	// Arrange.
	c, sink := loggedConverter(t)
	body := "<task-notification>\\n<task-id>a9</task-id>\\n<output-file>/tmp/proj/37dc1374/elsewhere/a9.output</output-file>\\n</task-notification>"
	notification := fmt.Sprintf(`{"type":"queue-operation","operation":"enqueue","content":%q}`, body)
	call, result := taskStopLines("a9")

	// Act.
	convertLinesFrom(t, c, 0, notification, call, result)

	// Assert.
	if got := levelForMessage(t, sink, "no launch on this stream opened"); got != "warn" {
		t.Fatalf("the stop was recorded at %q, want warn (nothing named a launching session)", got)
	}
}

// TestANotificationForARunThisSessionLaunchedIsNotForeign is the ordinary case,
// pinned so the new inference cannot start demoting the runs it must not touch.
func TestANotificationForARunThisSessionLaunchedIsNotForeign(t *testing.T) {
	// Arrange.
	c := newTestConverter(t)

	// Act.
	convertLinesFrom(t, c, 0, taskNotificationLine("a9", "session-uuid"))

	// Assert.
	if owner, foreign := c.foreignSpawns["a9"]; foreign {
		t.Fatalf("a run spooled under this session was recorded as launched by %q; it must not be foreign", owner)
	}
}
