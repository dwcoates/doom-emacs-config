package session

import (
	"encoding/json"
	"fmt"
	"strconv"
	"strings"

	"claude-repld/internal/protocol"
)

const (
	writePreviewLimit = 500
	taskSummaryLimit  = 200
)

// permissionPreview builds the §2.7 preview payload for a
// permission-request frame from the proposed tool input.
func permissionPreview(toolName string, input json.RawMessage) *protocol.PermissionPreview {
	switch toolName {
	case "Bash":
		var in struct {
			Command string `json:"command"`
		}
		if err := json.Unmarshal(input, &in); err == nil && in.Command != "" {
			return &protocol.PermissionPreview{Kind: "bash", Command: in.Command}
		}
	case "Edit":
		var in struct {
			FilePath  string `json:"file_path"`
			OldString string `json:"old_string"`
			NewString string `json:"new_string"`
		}
		if err := json.Unmarshal(input, &in); err == nil && in.FilePath != "" {
			return &protocol.PermissionPreview{
				Kind:        "diff",
				FilePath:    in.FilePath,
				UnifiedDiff: unifiedDiff(in.FilePath, in.OldString, in.NewString),
			}
		}
	case "Write":
		var in struct {
			FilePath string `json:"file_path"`
			Content  string `json:"content"`
		}
		if err := json.Unmarshal(input, &in); err == nil && in.FilePath != "" {
			return &protocol.PermissionPreview{
				Kind:     "write",
				FilePath: in.FilePath,
				Bytes:    len(in.Content),
				Preview:  truncate(in.Content, writePreviewLimit),
			}
		}
	}
	return &protocol.PermissionPreview{
		Kind:    "generic",
		Summary: fmt.Sprintf("%s %s", toolName, truncate(string(input), writePreviewLimit)),
	}
}

// renderHint builds the optional §2.6 render payload for a
// tool-use-result frame. Returns nil when no richer rendering applies.
func renderHint(toolName string, input, content json.RawMessage) *protocol.RenderHint {
	switch toolName {
	case "Bash":
		return &protocol.RenderHint{Kind: "bash", Stdout: contentText(content)}
	case "Edit":
		var in struct {
			FilePath  string `json:"file_path"`
			OldString string `json:"old_string"`
			NewString string `json:"new_string"`
		}
		if err := json.Unmarshal(input, &in); err == nil && in.FilePath != "" {
			return &protocol.RenderHint{
				Kind:        "diff",
				FilePath:    in.FilePath,
				UnifiedDiff: unifiedDiff(in.FilePath, in.OldString, in.NewString),
			}
		}
	case "Write":
		var in struct {
			FilePath string `json:"file_path"`
			Content  string `json:"content"`
		}
		if err := json.Unmarshal(input, &in); err == nil && in.FilePath != "" {
			return &protocol.RenderHint{
				Kind:        "diff",
				FilePath:    in.FilePath,
				UnifiedDiff: unifiedDiff(in.FilePath, "", in.Content),
			}
		}
	case "Grep":
		if matches := parseGrepMatches(contentText(content)); len(matches) > 0 {
			return &protocol.RenderHint{Kind: "grep", Matches: matches}
		}
	case "Task":
		return &protocol.RenderHint{Kind: "task", Summary: truncate(contentText(content), taskSummaryLimit)}
	}
	return nil
}

// contentText flattens a Layer-1 tool content payload (string or
// [{type:"text",text}] array) into plain text.
func contentText(content json.RawMessage) string {
	var s string
	if err := json.Unmarshal(content, &s); err == nil {
		return s
	}
	var blocks []struct {
		Type string `json:"type"`
		Text string `json:"text"`
	}
	if err := json.Unmarshal(content, &blocks); err == nil {
		parts := make([]string, 0, len(blocks))
		for _, b := range blocks {
			if b.Type == "text" {
				parts = append(parts, b.Text)
			}
		}
		return strings.Join(parts, "\n")
	}
	return ""
}

// parseGrepMatches parses "file:line:text" content-mode grep output.
// Lines that do not match the shape are skipped; if none match, nil.
func parseGrepMatches(text string) []protocol.GrepMatch {
	var matches []protocol.GrepMatch
	for _, line := range strings.Split(text, "\n") {
		first := strings.Index(line, ":")
		if first <= 0 {
			continue
		}
		rest := line[first+1:]
		second := strings.Index(rest, ":")
		if second <= 0 {
			continue
		}
		lineNo, err := strconv.Atoi(rest[:second])
		if err != nil {
			continue
		}
		matches = append(matches, protocol.GrepMatch{
			File: line[:first],
			Line: lineNo,
			Text: rest[second+1:],
		})
	}
	return matches
}

// unifiedDiff renders a minimal one-hunk unified diff between old and
// new, trimming the common prefix/suffix lines for compactness. It is a
// presentation hint, not a patch: correctness bar is "valid unified diff
// describing the change", not "minimal edit script".
func unifiedDiff(path, oldText, newText string) string {
	oldLines := splitLines(oldText)
	newLines := splitLines(newText)

	prefix := 0
	for prefix < len(oldLines) && prefix < len(newLines) && oldLines[prefix] == newLines[prefix] {
		prefix++
	}
	suffix := 0
	for suffix < len(oldLines)-prefix && suffix < len(newLines)-prefix &&
		oldLines[len(oldLines)-1-suffix] == newLines[len(newLines)-1-suffix] {
		suffix++
	}
	oldChanged := oldLines[prefix : len(oldLines)-suffix]
	newChanged := newLines[prefix : len(newLines)-suffix]

	var b strings.Builder
	fmt.Fprintf(&b, "--- a/%s\n+++ b/%s\n", path, path)
	fmt.Fprintf(&b, "@@ -%d,%d +%d,%d @@\n", prefix+1, len(oldChanged), prefix+1, len(newChanged))
	for _, line := range oldChanged {
		b.WriteString("-" + line + "\n")
	}
	for _, line := range newChanged {
		b.WriteString("+" + line + "\n")
	}
	return b.String()
}

func splitLines(s string) []string {
	if s == "" {
		return nil
	}
	return strings.Split(strings.TrimSuffix(s, "\n"), "\n")
}

func truncate(s string, n int) string {
	if len(s) <= n {
		return s
	}
	return s[:n] + "…"
}
