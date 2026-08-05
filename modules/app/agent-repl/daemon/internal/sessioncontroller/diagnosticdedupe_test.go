package sessioncontroller

import (
	"testing"

	"google.golang.org/protobuf/types/known/structpb"
)

func TestDiagnosticDeduperEmitsFirstAndOneBoundedRepeatSummary(t *testing.T) {
	payload, err := structpb.NewStruct(map[string]any{"field": "value"})
	if err != nil {
		t.Fatal(err)
	}
	d := newDiagnosticDeduper(2, 2)
	first, err := d.observe("message-1", payload)
	if err != nil || !first.Emit || !first.First || first.Summary || first.RepeatCount != 0 {
		t.Fatalf("first observation = %+v, %v", first, err)
	}
	second, err := d.observe("message-1", payload)
	if err != nil || second.Emit || second.RepeatCount != 1 {
		t.Fatalf("second observation = %+v, %v", second, err)
	}
	summary, err := d.observe("message-1", payload)
	if err != nil || !summary.Emit || !summary.Summary || summary.RepeatCount != 2 {
		t.Fatalf("summary observation = %+v, %v", summary, err)
	}
	suppressed, err := d.observe("message-1", payload)
	if err != nil || suppressed.Emit || suppressed.RepeatCount != 2 {
		t.Fatalf("suppressed observation = %+v, %v", suppressed, err)
	}
}

func TestDiagnosticDeduperKeysOnBothMessageAndPayloadAndEvictsFIFO(t *testing.T) {
	a, _ := structpb.NewStruct(map[string]any{"field": "a"})
	b, _ := structpb.NewStruct(map[string]any{"field": "b"})
	d := newDiagnosticDeduper(2, 1)
	firstA, err := d.observe("message", a)
	if err != nil || !firstA.Emit {
		t.Fatalf("first A = %+v, %v", firstA, err)
	}
	otherPayload, err := d.observe("message", b)
	if err != nil || !otherPayload.Emit || otherPayload.Fingerprint == firstA.Fingerprint {
		t.Fatalf("other payload = %+v, %v", otherPayload, err)
	}
	otherMessage, err := d.observe("message-2", a)
	if err != nil || !otherMessage.Emit {
		t.Fatalf("other message = %+v, %v", otherMessage, err)
	}
	evicted, err := d.observe("message", a)
	if err != nil || !evicted.Emit || !evicted.First {
		t.Fatalf("evicted key observation = %+v, %v", evicted, err)
	}
}
