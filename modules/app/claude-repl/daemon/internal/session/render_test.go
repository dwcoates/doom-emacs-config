package session

import (
	"encoding/json"
	"strings"
	"testing"
)

func TestPermissionPreview(t *testing.T) {
	tests := []struct {
		name     string
		tool     string
		input    string
		wantKind string
		check    func(t *testing.T, p map[string]any)
	}{
		{
			name:     "bash preview carries the command",
			tool:     "Bash",
			input:    `{"command":"ls -la"}`,
			wantKind: "bash",
			check: func(t *testing.T, p map[string]any) {
				if p["command"] != "ls -la" {
					t.Errorf("command = %v", p["command"])
				}
			},
		},
		{
			name:     "edit preview carries a unified diff",
			tool:     "Edit",
			input:    `{"file_path":"/f.go","old_string":"a\nb","new_string":"a\nc"}`,
			wantKind: "diff",
			check: func(t *testing.T, p map[string]any) {
				diff := p["unified_diff"].(string)
				if !strings.Contains(diff, "-b\n") || !strings.Contains(diff, "+c\n") {
					t.Errorf("diff = %q", diff)
				}
			},
		},
		{
			name:     "write preview carries size and content preview",
			tool:     "Write",
			input:    `{"file_path":"/f.txt","content":"hello"}`,
			wantKind: "write",
			check: func(t *testing.T, p map[string]any) {
				if p["bytes"] != float64(5) || p["preview"] != "hello" {
					t.Errorf("preview = %v", p)
				}
			},
		},
		{
			name:     "unknown tool falls back to generic",
			tool:     "WebFetch",
			input:    `{"url":"https://x"}`,
			wantKind: "generic",
			check: func(t *testing.T, p map[string]any) {
				if !strings.Contains(p["summary"].(string), "WebFetch") {
					t.Errorf("summary = %v", p["summary"])
				}
			},
		},
		{
			name:     "bash with undecodable input falls back to generic",
			tool:     "Bash",
			input:    `{"command":42}`,
			wantKind: "generic",
			check:    func(t *testing.T, p map[string]any) {},
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			preview := permissionPreview(tt.tool, json.RawMessage(tt.input))
			// Assert
			data, err := json.Marshal(preview)
			if err != nil {
				t.Fatalf("marshal: %v", err)
			}
			var p map[string]any
			if err := json.Unmarshal(data, &p); err != nil {
				t.Fatalf("unmarshal: %v", err)
			}
			if p["kind"] != tt.wantKind {
				t.Fatalf("kind = %v, want %v", p["kind"], tt.wantKind)
			}
			tt.check(t, p)
		})
	}
}

func TestRenderHint(t *testing.T) {
	tests := []struct {
		name     string
		tool     string
		input    string
		content  string
		wantKind string
		wantNil  bool
		check    func(t *testing.T, h map[string]any)
	}{
		{
			name:     "bash render carries stdout",
			tool:     "Bash",
			input:    `{"command":"ls"}`,
			content:  `"a\nb"`,
			wantKind: "bash",
			check: func(t *testing.T, h map[string]any) {
				if h["stdout"] != "a\nb" {
					t.Errorf("stdout = %v", h["stdout"])
				}
			},
		},
		{
			name:     "edit render is a diff",
			tool:     "Edit",
			input:    `{"file_path":"/f","old_string":"x","new_string":"y"}`,
			content:  `"ok"`,
			wantKind: "diff",
			check: func(t *testing.T, h map[string]any) {
				diff := h["unified_diff"].(string)
				if !strings.Contains(diff, "-x\n") || !strings.Contains(diff, "+y\n") {
					t.Errorf("diff = %q", diff)
				}
			},
		},
		{
			name:     "write render is an all-added diff",
			tool:     "Write",
			input:    `{"file_path":"/f","content":"line1\nline2"}`,
			content:  `"ok"`,
			wantKind: "diff",
			check: func(t *testing.T, h map[string]any) {
				diff := h["unified_diff"].(string)
				if !strings.Contains(diff, "+line1\n+line2\n") || strings.Contains(diff, "\n-") {
					t.Errorf("diff = %q", diff)
				}
			},
		},
		{
			name:     "grep render parses file:line:text",
			tool:     "Grep",
			input:    `{"pattern":"x"}`,
			content:  `"src/a.go:12:  x := 1\nsrc/b.go:9:x()"`,
			wantKind: "grep",
			check: func(t *testing.T, h map[string]any) {
				matches := h["matches"].([]any)
				if len(matches) != 2 {
					t.Fatalf("matches = %v", matches)
				}
				first := matches[0].(map[string]any)
				if first["file"] != "src/a.go" || first["line"] != float64(12) {
					t.Errorf("first = %v", first)
				}
			},
		},
		{
			name:     "task render summarizes content blocks",
			tool:     "Task",
			input:    `{"description":"d","prompt":"p"}`,
			content:  `[{"type":"text","text":"agent says hi"}]`,
			wantKind: "task",
			check: func(t *testing.T, h map[string]any) {
				if h["summary"] != "agent says hi" {
					t.Errorf("summary = %v", h["summary"])
				}
			},
		},
		{
			name:    "read render is nil",
			tool:    "Read",
			input:   `{"file_path":"/f"}`,
			content: `"contents"`,
			wantNil: true,
		},
		{
			name:    "grep with unparseable content is nil",
			tool:    "Grep",
			input:   `{"pattern":"x"}`,
			content: `"no matches found"`,
			wantNil: true,
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			hint := renderHint(tt.tool, json.RawMessage(tt.input), json.RawMessage(tt.content))
			// Assert
			if tt.wantNil {
				if hint != nil {
					t.Fatalf("hint = %+v, want nil", hint)
				}
				return
			}
			if hint == nil {
				t.Fatal("hint = nil")
			}
			data, err := json.Marshal(hint)
			if err != nil {
				t.Fatalf("marshal: %v", err)
			}
			var h map[string]any
			if err := json.Unmarshal(data, &h); err != nil {
				t.Fatalf("unmarshal: %v", err)
			}
			if h["kind"] != tt.wantKind {
				t.Fatalf("kind = %v, want %v", h["kind"], tt.wantKind)
			}
			tt.check(t, h)
		})
	}
}

func TestUnifiedDiffTrimsCommonContext(t *testing.T) {
	// Arrange
	oldText := "keep1\nchange-me\nkeep2"
	newText := "keep1\nchanged\nkeep2"
	// Act
	diff := unifiedDiff("f.txt", oldText, newText)
	// Assert
	if strings.Contains(diff, "keep1") || strings.Contains(diff, "keep2") {
		t.Errorf("diff should trim unchanged context: %q", diff)
	}
	if !strings.Contains(diff, "-change-me\n+changed\n") {
		t.Errorf("diff = %q", diff)
	}
}

func TestContentTextFlattensBlockArrays(t *testing.T) {
	// Arrange
	content := json.RawMessage(`[{"type":"text","text":"a"},{"type":"text","text":"b"}]`)
	// Act + Assert
	if got := contentText(content); got != "a\nb" {
		t.Errorf("contentText = %q", got)
	}
}

func TestTruncateAddsEllipsisPastLimit(t *testing.T) {
	// Arrange + Act + Assert
	if got := truncate("abcdef", 3); got != "abc…" {
		t.Errorf("truncate = %q", got)
	}
}
