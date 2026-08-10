package frontend

import (
	"testing"

	corev1 "agentrepl/proto/agentshim/core/v1"
	datav1 "agentrepl/proto/agentshim/data/v1"
)

// origin_test.go — the record's provenance reaching the daemon's own curators.
//
// RecordEnvelope.OriginKind is the STRUCTURED discriminator the task-notification
// curator (sessioncontroller/tasknotification.go) reads instead of matching a
// record's prose. Both observation planes carry it, and a curator that saw it on
// only one of them would withhold a record on the stream and render the same
// record again when the file plane redelivered it.

const taskNotificationBody = "<task-notification>\n<task-id>a1</task-id>\n<status>completed</status>\n</task-notification>"

// userLineWithOrigin is one file-plane user record stamping the given origin.
func userLineWithOrigin(uuid, text string, kind datav1.OriginKind) *datav1.TranscriptLine {
	env := &datav1.LineEnvelope{Uuid: uuid}
	if kind != datav1.OriginKind_ORIGIN_KIND_UNSPECIFIED {
		env.Origin = &datav1.Origin{Kind: kind}
	}
	return &datav1.TranscriptLine{Line: &datav1.TranscriptLine_User{User: &datav1.UserLine{
		Envelope: env,
		Message: &datav1.ApiUserMessage{
			Content: &datav1.ApiUserMessage_ContentString{ContentString: text},
		},
	}}}
}

// streamUserEvent is one stream-plane user message as the store event a
// consumer sees. parentToolUseID empty is the MAIN conversation, which is where
// a task notification lands.
func streamUserEvent(t *testing.T, uuid, text, parentToolUseID string, kind datav1.OriginKind) *corev1.Event {
	t.Helper()
	u := &datav1.UserMessage{
		Uuid:            uuid,
		ParentToolUseId: parentToolUseID,
		Message: &datav1.ApiUserMessage{
			Content: &datav1.ApiUserMessage_ContentString{ContentString: text},
		},
	}
	if kind != datav1.OriginKind_ORIGIN_KIND_UNSPECIFIED {
		u.Origin = &datav1.Origin{Kind: kind}
	}
	return &corev1.Event{
		Seq:          7,
		ProducedAtMs: producedMs,
		SessionId:    "s1",
		Payload:      &corev1.Event_Vendor{Vendor: mustAny(t, &datav1.ClaudeStreamMessage{Msg: &datav1.ClaudeStreamMessage_User{User: u}})},
	}
}

func TestAFilePlaneUserRecordCarriesItsOriginKind(t *testing.T) {
	// Arrange / Act
	c, err := CurateEvent("/ws", "f1", transcriptEvent(t, userLineWithOrigin("u1", taskNotificationBody, datav1.OriginKind_ORIGIN_KIND_TASK_NOTIFICATION)))
	if err != nil {
		t.Fatal(err)
	}

	// Assert
	if got := c.Envelopes["u1"].OriginKind; got != datav1.OriginKind_ORIGIN_KIND_TASK_NOTIFICATION {
		t.Errorf("origin kind = %v, want the transcript's own TASK_NOTIFICATION", got)
	}
}

func TestAStreamPlaneUserRecordCarriesItsOriginKind(t *testing.T) {
	// Arrange: a task notification lands on the MAIN conversation, so its
	// envelope cannot be keyed off detachment.
	c, err := CurateEvent("/ws", "f1", streamUserEvent(t, "u1", taskNotificationBody, "", datav1.OriginKind_ORIGIN_KIND_TASK_NOTIFICATION))
	if err != nil {
		t.Fatal(err)
	}

	// Assert
	if got := c.Envelopes["u1"].OriginKind; got != datav1.OriginKind_ORIGIN_KIND_TASK_NOTIFICATION {
		t.Errorf("origin kind = %v, want the stream message's own TASK_NOTIFICATION", got)
	}
}

func TestAStreamPlaneUserRecordWithAnOriginIsNotDetached(t *testing.T) {
	// Arrange: origin evidence alone must not route the record to a bubble.
	c, err := CurateEvent("/ws", "f1", streamUserEvent(t, "u1", taskNotificationBody, "", datav1.OriginKind_ORIGIN_KIND_TASK_NOTIFICATION))
	if err != nil {
		t.Fatal(err)
	}

	// Assert
	if len(c.Detached) != 0 {
		t.Errorf("routed %d fold(s) for a main-conversation record, want none", len(c.Detached))
	}
	if c.Envelopes["u1"].IsSidechain {
		t.Error("a main-conversation record was marked sidechain")
	}
}

func TestAHumanTypedRecordCarriesTheHumanOriginKind(t *testing.T) {
	// Arrange: the curator tests for the kind it withholds, never for
	// "not human", so HUMAN must arrive as itself rather than as UNSPECIFIED.
	c, err := CurateEvent("/ws", "f1", transcriptEvent(t, userLineWithOrigin("u1", "carry on", datav1.OriginKind_ORIGIN_KIND_HUMAN)))
	if err != nil {
		t.Fatal(err)
	}

	// Assert
	if got := c.Envelopes["u1"].OriginKind; got != datav1.OriginKind_ORIGIN_KIND_HUMAN {
		t.Errorf("origin kind = %v, want HUMAN", got)
	}
}

func TestAnOriginlessStreamUserRecordStaysEnvelopeless(t *testing.T) {
	// Arrange: an unattributed main-conversation record has no envelope, which
	// is the shape it has always had.
	c, err := CurateEvent("/ws", "f1", streamUserEvent(t, "u1", "carry on", "", datav1.OriginKind_ORIGIN_KIND_UNSPECIFIED))
	if err != nil {
		t.Fatal(err)
	}

	// Assert
	if _, present := c.Envelopes["u1"]; present {
		t.Error("a record with neither detachment nor origin evidence got an envelope")
	}
	if len(c.Feed.GetItems()) != 1 {
		t.Fatalf("feed carried %d item(s), want the record itself", len(c.Feed.GetItems()))
	}
}

func TestAStreamPlaneDetachedUserRecordStillNamesItsLaunchingCall(t *testing.T) {
	// Arrange: adding origin to the stream envelope must not cost detachment
	// the fact it was always keyed on.
	c, err := CurateEvent("/ws", "f1", streamUserEvent(t, "u1", "subagent prompt", "tu_task", datav1.OriginKind_ORIGIN_KIND_UNSPECIFIED))
	if err != nil {
		t.Fatal(err)
	}

	// Assert: a sidechain user record with no emission arm is withheld from the
	// feed rather than promoted, and it is the detachment that decided so.
	if len(c.Feed.GetItems()) != 0 {
		t.Errorf("feed carried %d item(s) for a detached record, want none", len(c.Feed.GetItems()))
	}
	if len(c.WithheldDetached) != 1 || c.WithheldDetached[0] != "u1" {
		t.Errorf("withheld detached = %v, want the detached record u1", c.WithheldDetached)
	}
}
