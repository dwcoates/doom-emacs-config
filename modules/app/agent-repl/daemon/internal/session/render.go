package session

import (
	"encoding/json"
	"fmt"
	"os"
	"path/filepath"
	"strconv"
	"strings"

	"claude-repld/internal/protocol"
)

const writePreviewLimit = 500

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

// bashStructured is the part of a Bash tool_use_result the card renders.
// The SDK keeps stdout and stderr APART here; `content` is the flattened
// blob it hands the model, where the two are already spliced together and
// unrecoverable.
type bashStructured struct {
	Stdout      string `json:"stdout"`
	Stderr      string `json:"stderr"`
	Interrupted bool   `json:"interrupted"`
}

// renderHint builds the optional §2.6 render payload for a
// tool-use-result frame. Returns nil when no richer rendering applies.
// structured is the SDK's tool_use_result (§1.2), absent from a shim
// predating it; configDir and cwd are the session's roots the Skill case
// resolves a skill name against, and the other kinds ignore them.
func renderHint(toolName string, input, structured, content json.RawMessage, configDir, cwd string) *protocol.RenderHint {
	switch toolName {
	case "Bash":
		// RenderHint.Stderr has been on the wire, and rendered by the card,
		// since §2.6 shipped — with no producer, because the flattened
		// `content` is the only thing this ever read and stdout/stderr are
		// already merged in it. The structured result is where the split
		// actually lives, so the field finally has a source.
		var b bashStructured
		if len(structured) > 0 && json.Unmarshal(structured, &b) == nil {
			return &protocol.RenderHint{Kind: "bash", Stdout: b.Stdout, Stderr: b.Stderr}
		}
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
	// Task/Agent take no hint. They used to take a `task` one whose Summary
	// was the result truncated to 200 bytes — which saved nothing, since the
	// full result ships in Content on the same frame regardless, and cost
	// the reader everything past the cap. Without a hint the card renders
	// that Content whole, through the generic path.
	case "TaskUpdate":
		// A task's stream is its transitions, and the SDK reports each one
		// structurally. The webapp used to recover the id by matching
		// `#(\S+)` against the CREATE's prose; this is the same fact typed.
		var u struct {
			TaskID        string   `json:"taskId"`
			UpdatedFields []string `json:"updatedFields"`
			StatusChange  *struct {
				From string `json:"from"`
				To   string `json:"to"`
			} `json:"statusChange"`
		}
		if err := json.Unmarshal(structured, &u); err == nil && u.StatusChange != nil {
			return &protocol.RenderHint{
				Kind:       "task-update",
				TaskID:     u.TaskID,
				StatusFrom: u.StatusChange.From,
				StatusTo:   u.StatusChange.To,
				Fields:     u.UpdatedFields,
			}
		}
	case "Skill":
		var in struct {
			Skill string `json:"skill"`
		}
		if err := json.Unmarshal(input, &in); err == nil && in.Skill != "" {
			if body := skillContent(in.Skill, configDir, cwd); body != "" {
				return &protocol.RenderHint{Kind: "skill", Content: body}
			}
		}
	}
	return nil
}

// skillContent reads a named skill's SKILL.md — the body the skill injects
// when it launches — so the card can show the full skill contents. It
// searches the project's local skills first (the nearest .claude/skills
// walking up from cwd, so a local skill overrides a personal one of the
// same name), then the personal <configDir>/skills. Namespaced plugin
// skills (name contains ':') resolve through neither, and any skill with no
// readable SKILL.md, so both yield "".
func skillContent(name, configDir, cwd string) string {
	if name == "" || strings.ContainsRune(name, ':') {
		return ""
	}
	for _, dir := range skillSearchDirs(configDir, cwd) {
		if body, err := os.ReadFile(filepath.Join(dir, name, "SKILL.md")); err == nil {
			return string(body)
		}
	}
	return ""
}

// skillSearchDirs is the ordered list of skills/ roots a skill name is
// resolved against: every .claude/skills from cwd up to the filesystem
// root, then the personal <configDir>/skills.
func skillSearchDirs(configDir, cwd string) []string {
	var dirs []string
	for dir := cwd; dir != ""; {
		dirs = append(dirs, filepath.Join(dir, ".claude", "skills"))
		parent := filepath.Dir(dir)
		if parent == dir {
			break
		}
		dir = parent
	}
	if cd := ClaudeConfigDir(configDir); cd != "" {
		dirs = append(dirs, filepath.Join(cd, "skills"))
	}
	return dirs
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
