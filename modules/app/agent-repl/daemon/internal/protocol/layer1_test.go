package protocol

import (
	"encoding/json"
	"strings"
	"testing"
)

func TestDecodeL1Event(t *testing.T) {
	tests := []struct {
		name    string
		line    string
		wantNil bool
		wantErr string
		check   func(t *testing.T, evt *L1Event)
	}{
		{
			name: "ready event decodes handshake fields",
			line: `{"type":"ready","session_id":"s1","shim_version":"1.2.3","sdk_version":"0.1.77","permission_mode":"plan"}`,
			check: func(t *testing.T, evt *L1Event) {
				if evt.ShimVersion != "1.2.3" || evt.SDKVersion != "0.1.77" || evt.PermissionMode != "plan" {
					t.Errorf("ready fields = %q/%q/%q", evt.ShimVersion, evt.SDKVersion, evt.PermissionMode)
				}
			},
		},
		{
			name: "stream-event keeps the raw event payload",
			line: `{"type":"stream-event","session_id":"s1","uuid":"u1","event":{"type":"ping"}}`,
			check: func(t *testing.T, evt *L1Event) {
				if string(evt.Event) != `{"type":"ping"}` {
					t.Errorf("Event = %s", evt.Event)
				}
			},
		},
		{
			name: "result event decodes numeric fields",
			line: `{"type":"result","session_id":"s1","uuid":"u1","subtype":"success","duration_ms":42,"duration_api_ms":40,"num_turns":2,"total_cost_usd":0.5,"usage":{"input_tokens":1,"output_tokens":2},"result":"done","is_error":false}`,
			check: func(t *testing.T, evt *L1Event) {
				if evt.DurationMS != 42 || evt.NumTurns != 2 || *evt.Result != "done" {
					t.Errorf("result fields = %+v", evt)
				}
			},
		},
		{
			name: "error event message is readable via MessageText",
			line: `{"type":"error","session_id":"s1","code":"sdk_throw","message":"boom"}`,
			check: func(t *testing.T, evt *L1Event) {
				if got := evt.MessageText(); got != "boom" {
					t.Errorf("MessageText() = %q, want boom", got)
				}
			},
		},
		{
			name: "tool-result event decodes correlation fields",
			line: `{"type":"tool-result","session_id":"s1","uuid":"u1","tool_use_id":"t1","is_error":true,"content":"nope"}`,
			check: func(t *testing.T, evt *L1Event) {
				if evt.ToolUseID != "t1" || !evt.IsError {
					t.Errorf("tool-result fields = %+v", evt)
				}
			},
		},
		{
			name:    "unknown event type is ignored",
			line:    `{"type":"quantum-flux","session_id":"s1"}`,
			wantNil: true,
		},
		{
			name:    "invalid JSON errors",
			line:    `{nope`,
			wantErr: "invalid JSON",
		},
		{
			name:    "missing type discriminator errors",
			line:    `{"session_id":"s1"}`,
			wantErr: "missing type",
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			evt, err := DecodeL1Event([]byte(tt.line))
			// Assert
			if tt.wantErr != "" {
				if err == nil || !strings.Contains(err.Error(), tt.wantErr) {
					t.Fatalf("err = %v, want containing %q", err, tt.wantErr)
				}
				return
			}
			if err != nil {
				t.Fatalf("unexpected error: %v", err)
			}
			if tt.wantNil {
				if evt != nil {
					t.Fatalf("evt = %+v, want nil", evt)
				}
				return
			}
			if evt == nil {
				t.Fatal("evt = nil, want event")
			}
			tt.check(t, evt)
		})
	}
}

func TestDecodeCommand(t *testing.T) {
	tests := []struct {
		name    string
		line    string
		wantNil bool
		wantErr string
		check   func(t *testing.T, cmd *L1Command)
	}{
		{
			name: "user-message with string content",
			line: `{"type":"user-message","request_id":"r1","content":"hi"}`,
			check: func(t *testing.T, cmd *L1Command) {
				if string(cmd.Content) != `"hi"` {
					t.Errorf("Content = %s", cmd.Content)
				}
			},
		},
		{
			name: "permission-decision allow",
			line: `{"type":"permission-decision","request_id":"r1","decision":{"behavior":"allow"}}`,
			check: func(t *testing.T, cmd *L1Command) {
				if cmd.Decision.Behavior != "allow" {
					t.Errorf("Behavior = %q", cmd.Decision.Behavior)
				}
			},
		},
		{
			name: "replay-request needs no request_id",
			line: `{"type":"replay-request","from_seq":17}`,
			check: func(t *testing.T, cmd *L1Command) {
				if cmd.FromSeq != 17 {
					t.Errorf("FromSeq = %d", cmd.FromSeq)
				}
			},
		},
		{
			name:    "unknown command type is ignored",
			line:    `{"type":"telepathy","request_id":"r1"}`,
			wantNil: true,
		},
		{
			name:    "missing request_id errors",
			line:    `{"type":"interrupt"}`,
			wantErr: "missing request_id",
		},
		{
			name:    "user-message without content errors",
			line:    `{"type":"user-message","request_id":"r1"}`,
			wantErr: "missing content",
		},
		{
			name:    "permission-decision without decision errors",
			line:    `{"type":"permission-decision","request_id":"r1"}`,
			wantErr: "missing decision",
		},
		{
			name:    "permission-decision with bad behavior errors",
			line:    `{"type":"permission-decision","request_id":"r1","decision":{"behavior":"shrug"}}`,
			wantErr: "behavior must be",
		},
		{
			name:    "deny without message errors",
			line:    `{"type":"permission-decision","request_id":"r1","decision":{"behavior":"deny"}}`,
			wantErr: "requires a message",
		},
		{
			name:    "set-permission-mode with invalid mode errors",
			line:    `{"type":"set-permission-mode","request_id":"r1","mode":"yolo"}`,
			wantErr: "invalid mode",
		},
		{
			// Empty is a caller who forgot to name a model, not a request
			// for the default one: reading it as "default" would switch the
			// session to a model nobody chose.
			name:    "set-model with an empty model errors",
			line:    `{"type":"set-model","request_id":"r1","model":""}`,
			wantErr: "non-empty model",
		},
		{
			name:    "set-model with no model field errors",
			line:    `{"type":"set-model","request_id":"r1"}`,
			wantErr: "non-empty model",
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			cmd, err := DecodeCommand([]byte(tt.line))
			// Assert
			if tt.wantErr != "" {
				if err == nil || !strings.Contains(err.Error(), tt.wantErr) {
					t.Fatalf("err = %v, want containing %q", err, tt.wantErr)
				}
				return
			}
			if err != nil {
				t.Fatalf("unexpected error: %v", err)
			}
			if tt.wantNil {
				if cmd != nil {
					t.Fatalf("cmd = %+v, want nil", cmd)
				}
				return
			}
			if cmd == nil {
				t.Fatal("cmd = nil, want command")
			}
			tt.check(t, cmd)
		})
	}
}

func TestEncodeNDJSON(t *testing.T) {
	// Arrange
	cmd := NewShutdownCmd("r9", "bye")
	// Act
	line, err := EncodeNDJSON(cmd)
	// Assert
	if err != nil {
		t.Fatalf("EncodeNDJSON: %v", err)
	}
	if line[len(line)-1] != '\n' {
		t.Fatal("line not newline-terminated")
	}
	var back map[string]any
	if err := json.Unmarshal(line, &back); err != nil {
		t.Fatalf("round-trip: %v", err)
	}
	if back["type"] != "shutdown" || back["request_id"] != "r9" || back["reason"] != "bye" {
		t.Errorf("round-trip = %v", back)
	}
}

func TestValidPermissionMode(t *testing.T) {
	tests := []struct {
		mode string
		want bool
	}{
		{"default", true},
		{"acceptEdits", true},
		{"bypassPermissions", true},
		{"plan", true},
		{"auto", true},
		{"manual", true},
		{"dontAsk", true},
		{"delegate", true},
		{"yolo", false},
		{"", false},
	}
	for _, tt := range tests {
		t.Run(tt.mode, func(t *testing.T) {
			// Act + Assert
			if got := ValidPermissionMode(tt.mode); got != tt.want {
				t.Errorf("ValidPermissionMode(%q) = %v, want %v", tt.mode, got, tt.want)
			}
		})
	}
}

func TestDecodeCommandSetModel(t *testing.T) {
	// Arrange
	line := `{"type":"set-model","request_id":"r1","model":"claude-opus-4-5"}`
	// Act
	cmd, err := DecodeCommand([]byte(line))
	// Assert
	if err != nil {
		t.Fatalf("DecodeCommand: %v", err)
	}
	if cmd.Type != "set-model" || cmd.Model != "claude-opus-4-5" {
		t.Errorf("cmd = %+v, want set-model claude-opus-4-5", cmd)
	}
}

func TestDecodeL1EventModels(t *testing.T) {
	// Arrange
	line := `{"type":"models","session_id":"s1","models":[{"value":"opus","displayName":"Opus 4.5","description":"smartest"}]}`
	// Act
	evt, err := DecodeL1Event([]byte(line))
	// Assert
	if err != nil {
		t.Fatalf("DecodeL1Event: %v", err)
	}
	if len(evt.Models) != 1 || evt.Models[0].Value != "opus" || evt.Models[0].DisplayName != "Opus 4.5" {
		t.Errorf("models = %+v, want one Opus 4.5 entry", evt.Models)
	}
}
