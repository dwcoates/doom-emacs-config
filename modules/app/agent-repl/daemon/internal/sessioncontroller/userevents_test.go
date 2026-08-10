package sessioncontroller

import (
	"testing"

	corev1 "agentrepl/proto/agentshim/core/v1"
	datav1 "agentrepl/proto/agentshim/data/v1"

	"google.golang.org/protobuf/types/known/anypb"
)

// userevents_test.go — the two user records the consumer sees, built once.
//
// Every curator test in this package feeds the consumer a user record on one of
// the two observation planes, and each one used to hand-roll the same proto:
// the same envelope, the same string-content ApiUserMessage, the same
// anypb.New. The variations that matter — the record's origin, the request it
// answers, its timestamp — are parameters, not a reason for a second builder.

// userLineEvent is the DURABLE account of a user record as the real pipeline
// delivers it: a file-plane transcript user line, carrying NO request id of its
// own (that field is empty on every line the file plane produces).
//
// An UNSPECIFIED kind stamps no origin at all, which is the shape a record the
// harness did not attribute has on disk.
func userLineEvent(t *testing.T, seq uint64, uuid, text string, kind datav1.OriginKind) *corev1.Event {
	t.Helper()
	env := &datav1.LineEnvelope{Uuid: uuid}
	if kind != datav1.OriginKind_ORIGIN_KIND_UNSPECIFIED {
		env.Origin = &datav1.Origin{Kind: kind}
	}
	a, err := anypb.New(&datav1.TranscriptLine{
		Line: &datav1.TranscriptLine_User{User: &datav1.UserLine{
			Envelope: env,
			Message: &datav1.ApiUserMessage{
				Content: &datav1.ApiUserMessage_ContentString{ContentString: text},
			},
		}},
	})
	if err != nil {
		t.Fatalf("anypb.New: %v", err)
	}
	return &corev1.Event{SessionId: "vendor-uuid", Seq: seq, Payload: &corev1.Event_Vendor{Vendor: a}}
}

// userStreamEvent is the same record on the STREAM plane, which is where the
// daemon sees it FIRST. requestID and tsMs are the event's own control-request
// correlation and stamp; both are empty on a record no submit of this daemon's
// is outstanding for.
func userStreamEvent(t *testing.T, seq uint64, uuid, requestID, text string, tsMs int64, kind datav1.OriginKind) *corev1.Event {
	t.Helper()
	u := &datav1.UserMessage{
		Uuid: uuid,
		Message: &datav1.ApiUserMessage{
			Content: &datav1.ApiUserMessage_ContentString{ContentString: text},
		},
	}
	if kind != datav1.OriginKind_ORIGIN_KIND_UNSPECIFIED {
		u.Origin = &datav1.Origin{Kind: kind}
	}
	a, err := anypb.New(&datav1.ClaudeStreamMessage{Msg: &datav1.ClaudeStreamMessage_User{User: u}})
	if err != nil {
		t.Fatalf("anypb.New: %v", err)
	}
	return &corev1.Event{
		SessionId: "vendor-uuid", Seq: seq, ProducedAtMs: tsMs, RequestId: requestID,
		Payload: &corev1.Event_Vendor{Vendor: a},
	}
}
