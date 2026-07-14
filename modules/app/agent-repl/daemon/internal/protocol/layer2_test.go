package protocol

import (
	"encoding/json"
	"testing"
)

func TestL2FrameMarshalling(t *testing.T) {
	tests := []struct {
		name  string
		frame L2Frame
		want  map[string]any
	}{
		{
			name: "hello frame carries the resume cursor",
			frame: &HelloFrame{
				Envelope:       Envelope{Type: "hello", Seq: 7, TS: "T", SessionID: "s1"},
				DaemonVersion:  "0.1.0",
				ResumeFromSeq:  3,
				PermissionMode: PermissionModeDefault,
				Model:          "m",
				CWD:            "/w",
			},
			want: map[string]any{"type": "hello", "seq": float64(7), "resume_from_seq": float64(3)},
		},
		{
			name: "text-delta frame",
			frame: &TextDeltaFrame{
				Envelope: Envelope{Type: "text-delta", Seq: 1, TS: "T", SessionID: "s1"},
				BlockID:  "b1",
				Text:     "chunk",
			},
			want: map[string]any{"type": "text-delta", "block_id": "b1", "text": "chunk"},
		},
		{
			name: "tool-use-result with bash render hint",
			frame: &ToolUseResultFrame{
				Envelope:  Envelope{Type: "tool-use-result", Seq: 2, TS: "T", SessionID: "s1"},
				ToolUseID: "t1",
				Content:   json.RawMessage(`"out"`),
				Render:    &RenderHint{Kind: "bash", Stdout: "out"},
			},
			want: map[string]any{"type": "tool-use-result", "tool_use_id": "t1"},
		},
		{
			name: "permission-resolved cancel",
			frame: &PermissionResolvedFrame{
				Envelope:  Envelope{Type: "permission-resolved", Seq: 3, TS: "T", SessionID: "s1"},
				RequestID: "r1",
				Decision:  "cancel",
				Message:   "interrupted",
			},
			want: map[string]any{"type": "permission-resolved", "decision": "cancel"},
		},
		{
			name: "interrupt frame carries only its envelope",
			frame: &InterruptFrame{
				Envelope: Envelope{Type: "interrupt", Seq: 8, TS: "T", SessionID: "s1"},
			},
			want: map[string]any{"type": "interrupt", "seq": float64(8)},
		},
		{
			name: "queue-added carries the classifying status",
			frame: &QueueAddedFrame{
				Envelope:  Envelope{Type: "queue-added", Seq: 4, TS: "T", SessionID: "s1"},
				QueueID:   "q1",
				RequestID: "r1",
				Content:   json.RawMessage(`[{"type":"text","text":"hi"}]`),
				Status:    "classifying",
			},
			want: map[string]any{"type": "queue-added", "queue_id": "q1", "status": "classifying"},
		},
		{
			name: "queue-classified carries verdict and source",
			frame: &QueueClassifiedFrame{
				Envelope: Envelope{Type: "queue-classified", Seq: 5, TS: "T", SessionID: "s1"},
				QueueID:  "q1",
				Verdict:  "interrupt",
				Reason:   "countermands the running task",
				Source:   "classifier",
			},
			want: map[string]any{"type": "queue-classified", "verdict": "interrupt", "source": "classifier"},
		},
		{
			name: "queue-removed drained carries the request id",
			frame: &QueueRemovedFrame{
				Envelope:  Envelope{Type: "queue-removed", Seq: 6, TS: "T", SessionID: "s1"},
				QueueID:   "q1",
				Reason:    "drained",
				RequestID: "r1",
			},
			want: map[string]any{"type": "queue-removed", "reason": "drained", "request_id": "r1"},
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			data, err := json.Marshal(tt.frame)
			// Assert
			if err != nil {
				t.Fatalf("marshal: %v", err)
			}
			var got map[string]any
			if err := json.Unmarshal(data, &got); err != nil {
				t.Fatalf("unmarshal: %v", err)
			}
			for k, want := range tt.want {
				if got[k] != want {
					t.Errorf("field %q = %v, want %v", k, got[k], want)
				}
			}
			for _, k := range []string{"seq", "ts", "session_id"} {
				if _, ok := got[k]; !ok {
					t.Errorf("envelope field %q missing", k)
				}
			}
		})
	}
}

func TestEnvelopeEnvReturnsMutableHeader(t *testing.T) {
	// Arrange
	frame := &TextStartFrame{Envelope: Envelope{Type: "text-start"}}
	// Act
	frame.Env().Seq = 41
	// Assert
	if frame.Seq != 41 {
		t.Errorf("Seq = %d, want 41", frame.Seq)
	}
}

func TestQueueRemovedOmitsRequestIDWhenNotDrained(t *testing.T) {
	// Arrange — a cancelled removal carries no request_id.
	frame := &QueueRemovedFrame{
		Envelope: Envelope{Type: "queue-removed"},
		QueueID:  "q1",
		Reason:   "cancelled",
	}
	// Act
	data, err := json.Marshal(frame)
	if err != nil {
		t.Fatalf("marshal: %v", err)
	}
	var got map[string]any
	if err := json.Unmarshal(data, &got); err != nil {
		t.Fatalf("unmarshal: %v", err)
	}
	// Assert
	if _, ok := got["request_id"]; ok {
		t.Error("request_id should be omitted when the item was not drained")
	}
}

func TestHelloOmitsQueueWhenEmpty(t *testing.T) {
	// Arrange
	frame := &HelloFrame{Envelope: Envelope{Type: "hello"}}
	// Act
	data, err := json.Marshal(frame)
	if err != nil {
		t.Fatalf("marshal: %v", err)
	}
	var got map[string]any
	if err := json.Unmarshal(data, &got); err != nil {
		t.Fatalf("unmarshal: %v", err)
	}
	// Assert
	if _, ok := got["queue"]; ok {
		t.Error("queue should be omitted when empty")
	}
}

func TestRenderHintOmitsEmptyFields(t *testing.T) {
	// Arrange
	frame := &ToolUseResultFrame{
		Envelope:  Envelope{Type: "tool-use-result"},
		ToolUseID: "t1",
		Content:   json.RawMessage(`"x"`),
	}
	// Act
	data, err := json.Marshal(frame)
	// Assert
	if err != nil {
		t.Fatalf("marshal: %v", err)
	}
	var got map[string]any
	if err := json.Unmarshal(data, &got); err != nil {
		t.Fatalf("unmarshal: %v", err)
	}
	if _, ok := got["render"]; ok {
		t.Error("render should be omitted when nil")
	}
}
