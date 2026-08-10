package sessioncontroller

import (
	"fmt"

	datav1 "agentrepl/proto/agentshim/data/v1"
	frontendv1 "agentrepl/proto/agentshim/frontend/v1"

	"claude-repld/internal/frontend"
)

// tasknotification.go — WITHHOLDING THE DETACHED-WORK COMPLETION NOTICE.
//
// THE DEFECT THIS FILE CURATES AWAY. When detached work finishes — a
// backgrounded Agent, a background Bash command, a scheduled wakeup — the
// harness feeds the completion back to the model by writing a `"user"` record
// into the conversation whose whole body is a `<task-notification>` envelope:
//
//	<task-notification>
//	<task-id>a8abc09d2bc681337</task-id>
//	<tool-use-id>toolu_012Hbds...</tool-use-id>
//	<status>completed</status>
//	<summary>...</summary>
//	</task-notification>
//
// It is addressed to the model, nobody typed it, and it arrived on the feed as
// an ordinary purple prompt bubble full of markup.
//
// THE COMPLETION IS ALREADY RENDERED, PROPERLY, ELSEWHERE. The same event
// reaches the frontend as the typed `system`/`task_notification` message, which
// is what settles the launching tool's watcher bubble. Withholding the user
// record removes the DUPLICATE prose rendering of a fact the watcher already
// states; it takes nothing away from the UI.
//
// THE VERDICT IS TYPED, NEVER PROSE. The harness stamps `origin.kind:
// "task-notification"` on the record, on BOTH planes (LineEnvelope.origin on
// disk, UserMessage.origin on the stream), and the daemon now carries it to
// this layer as RecordEnvelope.OriginKind. Matching the `<task-notification>`
// text instead would swallow a real prompt that quotes the envelope — the very
// failure machinery.go's head-only matching exists to bound — and would go
// blind the moment the harness reformats its own markup.
//
// WITHHELD, NOT DELETED, exactly as in machinery.go and noresponse.go: the
// store keeps the record, and the delta is still pushed so its through_seq
// advances the frontend's cursor. Only the rendered item is suppressed.
//
// RUNS BEFORE ATTRIBUTION (promptecho.go), with the other user-record curators
// and for the same reason: a record nobody typed must never claim an
// outstanding prompt's receipt, which would retire that receipt and leave the
// real prompt's own line unattributed behind it.

// isTaskNotificationRecord reports whether one curated item is the harness's
// detached-work completion notice.
//
// BOTH HALVES ARE REQUIRED. The origin names the producer, and the user_message
// arm is what would be RENDERED as a prompt bubble — the thing being withheld.
// An item on any other arm carrying this origin (there is none today) is not a
// bubble and is left alone rather than silently dropped.
func isTaskNotificationRecord(it *frontendv1.ConversationItem, env frontend.RecordEnvelope) bool {
	return env.OriginKind == datav1.OriginKind_ORIGIN_KIND_TASK_NOTIFICATION && it.GetUserMessage() != nil
}

// withholdTaskNotifications removes the harness's detached-work completion
// records from a curated delta, loud-logging every one it takes out.
//
// An item whose envelope this delta does not carry is KEPT. An absent envelope
// is an absent claim about provenance, and the only records that reach here
// without one are the ordinary conversation's — withholding on a missing
// envelope would hide the user's own words on the strength of nothing.
func (c *consumer) withholdTaskNotifications(cd *frontendv1.ConversationDelta, envs map[string]frontend.RecordEnvelope) {
	c.withholdItems(cd, func(it *frontendv1.ConversationItem) withholdVerdict {
		env, ok := envs[it.GetUuid()]
		if !ok || !isTaskNotificationRecord(it, env) {
			return keepItem
		}
		return withholdItem(fmt.Sprintf("session-controller: user turn WITHHELD as a detached-work task notification ws=%q session=%s seq=%d uuid=%s — the harness writes this completion notice as an unflagged \"user\" record addressed to the model, and origin.kind says so; the watcher bubble already renders the completion; the store keeps the record, the conversation feed does not",
			c.workspace, c.sessionID, cd.GetThroughSeq(), it.GetUuid()))
	})
}
