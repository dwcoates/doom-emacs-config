package main

import (
	"bytes"
	"context"
	"os"
	"path/filepath"
	"strings"
	"testing"
	"time"

	"claude-repld/internal/session"
)

// writeTranscript writes one `<uuid>.jsonl` for workspace under configDir and
// returns its path.
func writeTranscript(t *testing.T, configDir, workspace, uuid string, lines ...string) string {
	t.Helper()
	dir := session.ProjectDir(configDir, workspace)
	if err := os.MkdirAll(dir, 0o755); err != nil {
		t.Fatal(err)
	}
	path := filepath.Join(dir, uuid+".jsonl")
	body := ""
	for _, line := range lines {
		body += line + "\n"
	}
	if err := os.WriteFile(path, []byte(body), 0o644); err != nil {
		t.Fatal(err)
	}
	return path
}

// userLine is a transcript record for one typed user prompt.
func userLine(text string) string {
	return `{"type":"user","message":{"role":"user","content":` + mustJSONString(text) + `}}`
}

func mustJSONString(s string) string {
	out := []byte{'"'}
	for _, r := range s {
		switch r {
		case '"':
			out = append(out, '\\', '"')
		case '\\':
			out = append(out, '\\', '\\')
		case '\n':
			out = append(out, '\\', 'n')
		default:
			out = append(out, string(r)...)
		}
	}
	return string(append(out, '"'))
}

// transcriptRow splits one stdout line into its six fields.
func transcriptRow(t *testing.T, line string) []string {
	t.Helper()
	fields := strings.Split(line, "\t")
	if len(fields) != 6 {
		t.Fatalf("row %q has %d fields, want 6", line, len(fields))
	}
	return fields
}

func TestParseListTranscriptsArgsAcceptsTheCompleteCommandLine(t *testing.T) {
	// Arrange.
	args := []string{"--workspace", "/ws", "--config-dir", "/root/.claude", "--skip-empty", "--verbose"}

	// Act.
	opts, err := parseListTranscriptsArgs(args, new(bytes.Buffer))

	// Assert.
	if err != nil {
		t.Fatalf("parseListTranscriptsArgs: %v", err)
	}
	want := listTranscriptsOptions{Workspace: "/ws", ConfigDir: "/root/.claude", SkipEmpty: true, Verbose: true}
	if opts != want {
		t.Fatalf("options = %+v, want %+v", opts, want)
	}
}

func TestParseListTranscriptsArgsDefaultsToMarkingEmptyTranscripts(t *testing.T) {
	// Arrange.
	args := []string{"--workspace", "/ws"}

	// Act.
	opts, err := parseListTranscriptsArgs(args, new(bytes.Buffer))

	// Assert.
	if err != nil {
		t.Fatalf("parseListTranscriptsArgs: %v", err)
	}
	if opts.SkipEmpty {
		t.Fatal("skip-empty defaulted to true, want false")
	}
}

func TestParseListTranscriptsArgsRejectsIncompleteCommandLines(t *testing.T) {
	tests := []struct {
		name string
		args []string
		want string
	}{
		{name: "no workspace", args: []string{"--config-dir", "/root/.claude"}, want: "--workspace is required"},
		{name: "positional argument", args: []string{"--workspace", "/ws", "extra"}, want: `unexpected positional argument "extra"`},
		{name: "unknown flag", args: []string{"--workspace", "/ws", "--git-root", "/repo"}, want: "flag provided but not defined"},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			stderr := new(bytes.Buffer)

			// Act.
			_, err := parseListTranscriptsArgs(tc.args, stderr)

			// Assert.
			if err == nil {
				t.Fatalf("parseListTranscriptsArgs(%v) succeeded, want an error", tc.args)
			}
			if !strings.Contains(err.Error(), tc.want) {
				t.Fatalf("error = %v, want it to contain %q", err, tc.want)
			}
		})
	}
}

func TestConfigDirsForAppliesTheAccountRule(t *testing.T) {
	tests := []struct {
		name        string
		multiRoot   string
		workspace   string
		override    string
		wantPrimary string
		wantOthers  []string
	}{
		{
			name:        "override wins outright",
			multiRoot:   "/multi",
			workspace:   "/multi/repo/ws",
			override:    "/explicit/.claude",
			wantPrimary: "/explicit/.claude",
		},
		{
			name:        "workspace under the multi-repo root takes that account",
			multiRoot:   "/multi",
			workspace:   "/multi/repo/ws",
			wantPrimary: "/cfg/.claude-chesscom",
			wantOthers:  []string{"/cfg/.claude"},
		},
		{
			name:        "workspace outside it takes the default root",
			multiRoot:   "/multi",
			workspace:   "/elsewhere/ws",
			wantPrimary: "/cfg/.claude",
			wantOthers:  []string{"/cfg/.claude-chesscom"},
		},
		{
			name:        "no multi-repo root at all takes the default root",
			workspace:   "/multi/repo/ws",
			wantPrimary: "/cfg/.claude",
			wantOthers:  []string{"/cfg/.claude-chesscom"},
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			t.Setenv(session.MultiRepoRootEnv, tc.multiRoot)
			t.Setenv("CLAUDE_CONFIG_DIR", "/cfg/.claude")
			t.Setenv(session.MultiRepoConfigDirEnv, "/cfg/.claude-chesscom")

			// Act.
			primary, others, err := configDirsFor(tc.workspace, tc.override)

			// Assert.
			if err != nil {
				t.Fatalf("configDirsFor: %v", err)
			}
			if primary != tc.wantPrimary {
				t.Fatalf("primary = %q, want %q", primary, tc.wantPrimary)
			}
			if strings.Join(others, ",") != strings.Join(tc.wantOthers, ",") {
				t.Fatalf("others = %v, want %v", others, tc.wantOthers)
			}
		})
	}
}

func TestConfigDirsForRejectsARelativeOverride(t *testing.T) {
	// Arrange.
	t.Setenv("CLAUDE_CONFIG_DIR", "/cfg/.claude")

	// Act.
	_, _, err := configDirsFor("/ws", "relative/.claude")

	// Assert.
	if err == nil {
		t.Fatal("configDirsFor accepted a relative --config-dir, want an error")
	}
	if !strings.Contains(err.Error(), "absolute path") {
		t.Fatalf("error = %v, want it to name the absolute-path requirement", err)
	}
}

func TestTranscriptExcerptReadsBothContentShapes(t *testing.T) {
	tests := []struct {
		name    string
		content string
		want    string
	}{
		{name: "bare string", content: `"fix the parser"`, want: "fix the parser"},
		{name: "content block array", content: `[{"type":"text","text":"fix the parser"}]`, want: "fix the parser"},
		{name: "tool result blocks carry no prompt", content: `[{"type":"tool_result","text":"ok"}]`, want: ""},
		{name: "slash command envelope is not a prompt", content: `"<command-name>/status</command-name>"`, want: ""},
		{name: "local command envelope is not a prompt", content: `"<local-command-stdout>hi</local-command-stdout>"`, want: ""},
		{name: "whitespace only is not a prompt", content: `"   \n  "`, want: ""},
		{name: "newlines and tabs flatten to single spaces", content: `"one\ntwo"`, want: "one two"},
		{name: "unparseable content yields nothing", content: `12`, want: ""},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Act.
			got := transcriptExcerpt([]byte(tc.content))

			// Assert.
			if got != tc.want {
				t.Fatalf("excerpt = %q, want %q", got, tc.want)
			}
		})
	}
}

func TestFlattenExcerptTruncatesAtTheRuneCap(t *testing.T) {
	// Arrange.
	text := strings.Repeat("é", transcriptExcerptRuneCap+10)

	// Act.
	got := flattenExcerpt(text)

	// Assert.
	want := strings.Repeat("é", transcriptExcerptRuneCap) + "…"
	if got != want {
		t.Fatalf("excerpt = %q, want %q", got, want)
	}
}

func TestSummarizeTranscriptCountsEveryRecordAndTakesTheFirstPrompt(t *testing.T) {
	// Arrange.
	dir := t.TempDir()
	path := writeTranscript(t, dir, "/ws", "uuid-a",
		`{"type":"system","subtype":"init"}`,
		userLine("<command-name>/status</command-name>"),
		userLine("first real prompt"),
		userLine("second prompt"),
	)

	// Act.
	records, excerpt, err := summarizeTranscript(path)

	// Assert.
	if err != nil {
		t.Fatalf("summarizeTranscript: %v", err)
	}
	if records != 4 {
		t.Fatalf("records = %d, want 4", records)
	}
	if excerpt != "first real prompt" {
		t.Fatalf("excerpt = %q, want %q", excerpt, "first real prompt")
	}
}

func TestSummarizeTranscriptCountsATornLineWithoutFailing(t *testing.T) {
	// Arrange. A live transcript's last line is routinely a partial write.
	dir := t.TempDir()
	path := writeTranscript(t, dir, "/ws", "uuid-a", userLine("hello"), `{"type":"assistant","mess`)

	// Act.
	records, excerpt, err := summarizeTranscript(path)

	// Assert.
	if err != nil {
		t.Fatalf("summarizeTranscript: %v", err)
	}
	if records != 2 {
		t.Fatalf("records = %d, want 2", records)
	}
	if excerpt != "hello" {
		t.Fatalf("excerpt = %q, want %q", excerpt, "hello")
	}
}

func TestSummarizeTranscriptFailsOnAnUnreadableFile(t *testing.T) {
	// Arrange.
	path := filepath.Join(t.TempDir(), "missing.jsonl")

	// Act.
	_, _, err := summarizeTranscript(path)

	// Assert.
	if err == nil {
		t.Fatal("summarizeTranscript succeeded on a missing file, want an error")
	}
	if !strings.Contains(err.Error(), "open transcript") {
		t.Fatalf("error = %v, want it to name the unreadable transcript", err)
	}
}

func TestRunListTranscriptsPrintsTheWorkspacesTranscripts(t *testing.T) {
	// Arrange.
	cfg := t.TempDir()
	ws := t.TempDir()
	writeTranscript(t, cfg, ws, "uuid-a", userLine("teach the CLI to list transcripts"))
	stdout, stderr := new(bytes.Buffer), new(bytes.Buffer)

	// Act.
	err := runListTranscripts(context.Background(), []string{"--workspace", ws, "--config-dir", cfg}, stdout, stderr)

	// Assert.
	if err != nil {
		t.Fatalf("runListTranscripts: %v (stderr %s)", err, stderr)
	}
	row := transcriptRow(t, strings.TrimSuffix(stdout.String(), "\n"))
	if row[0] != "uuid-a" || row[1] != cfg || row[3] != "1" || row[4] != transcriptStatusOK {
		t.Fatalf("row = %v, want uuid-a under %s with 1 record and status ok", row, cfg)
	}
	if row[5] != "teach the CLI to list transcripts" {
		t.Fatalf("excerpt = %q, want the first user prompt", row[5])
	}
}

func TestRunListTranscriptsMarksAHandshakeOnlyTranscriptEmpty(t *testing.T) {
	// Arrange.
	cfg := t.TempDir()
	ws := t.TempDir()
	writeTranscript(t, cfg, ws, "uuid-handshake", `{"type":"system","subtype":"init"}`)
	stdout, stderr := new(bytes.Buffer), new(bytes.Buffer)

	// Act.
	err := runListTranscripts(context.Background(), []string{"--workspace", ws, "--config-dir", cfg}, stdout, stderr)

	// Assert.
	if err != nil {
		t.Fatalf("runListTranscripts: %v (stderr %s)", err, stderr)
	}
	row := transcriptRow(t, strings.TrimSuffix(stdout.String(), "\n"))
	if row[4] != transcriptStatusEmpty || row[5] != "-" {
		t.Fatalf("row = %v, want status %q with a %q excerpt", row, transcriptStatusEmpty, "-")
	}
}

func TestRunListTranscriptsOmitsEmptyTranscriptsUnderSkipEmpty(t *testing.T) {
	// Arrange.
	cfg := t.TempDir()
	ws := t.TempDir()
	writeTranscript(t, cfg, ws, "uuid-handshake", `{"type":"system","subtype":"init"}`)
	writeTranscript(t, cfg, ws, "uuid-real", userLine("a real prompt"))
	stdout, stderr := new(bytes.Buffer), new(bytes.Buffer)

	// Act.
	err := runListTranscripts(context.Background(), []string{"--workspace", ws, "--config-dir", cfg, "--skip-empty"}, stdout, stderr)

	// Assert.
	if err != nil {
		t.Fatalf("runListTranscripts: %v (stderr %s)", err, stderr)
	}
	lines := strings.Split(strings.TrimSuffix(stdout.String(), "\n"), "\n")
	if len(lines) != 1 {
		t.Fatalf("stdout = %q, want only the non-empty transcript", stdout.String())
	}
	if transcriptRow(t, lines[0])[0] != "uuid-real" {
		t.Fatalf("stdout = %q, want uuid-real", stdout.String())
	}
}

func TestRunListTranscriptsOrdersNewestFirst(t *testing.T) {
	// Arrange.
	cfg := t.TempDir()
	ws := t.TempDir()
	older := writeTranscript(t, cfg, ws, "uuid-older", userLine("older"))
	writeTranscript(t, cfg, ws, "uuid-newer", userLine("newer"))
	aged := time.Now().Add(-time.Hour)
	if err := os.Chtimes(older, aged, aged); err != nil {
		t.Fatal(err)
	}
	stdout, stderr := new(bytes.Buffer), new(bytes.Buffer)

	// Act.
	err := runListTranscripts(context.Background(), []string{"--workspace", ws, "--config-dir", cfg}, stdout, stderr)

	// Assert.
	if err != nil {
		t.Fatalf("runListTranscripts: %v (stderr %s)", err, stderr)
	}
	lines := strings.Split(strings.TrimSuffix(stdout.String(), "\n"), "\n")
	if len(lines) != 2 {
		t.Fatalf("stdout = %q, want two rows", stdout.String())
	}
	if transcriptRow(t, lines[0])[0] != "uuid-newer" {
		t.Fatalf("first row = %q, want the newest transcript first", lines[0])
	}
}

func TestRunListTranscriptsAlsoReportsTheOtherAccountsTranscripts(t *testing.T) {
	// Arrange. The workspace is outside $MULTI_REPO_ROOT, so its computed
	// account is the default root — but a transcript exists only under the
	// multi-repo root, which the doom-multi-repo-mode rule can produce and this
	// process cannot evaluate.
	root := t.TempDir()
	multi := filepath.Join(root, "chesscom")
	fallback := filepath.Join(root, "claude")
	ws := t.TempDir()
	t.Setenv(session.MultiRepoRootEnv, filepath.Join(root, "nowhere"))
	t.Setenv("CLAUDE_CONFIG_DIR", fallback)
	t.Setenv(session.MultiRepoConfigDirEnv, multi)
	writeTranscript(t, multi, ws, "uuid-elsewhere", userLine("written under the other account"))
	stdout, stderr := new(bytes.Buffer), new(bytes.Buffer)

	// Act.
	err := runListTranscripts(context.Background(), []string{"--workspace", ws}, stdout, stderr)

	// Assert.
	if err != nil {
		t.Fatalf("runListTranscripts: %v (stderr %s)", err, stderr)
	}
	row := transcriptRow(t, strings.TrimSuffix(stdout.String(), "\n"))
	if row[0] != "uuid-elsewhere" || row[1] != multi {
		t.Fatalf("row = %v, want uuid-elsewhere named under %s", row, multi)
	}
}

func TestRunListTranscriptsPrintsNothingForAWorkspaceNobodyHasTalkedTo(t *testing.T) {
	// Arrange.
	cfg := t.TempDir()
	ws := t.TempDir()
	stdout, stderr := new(bytes.Buffer), new(bytes.Buffer)

	// Act.
	err := runListTranscripts(context.Background(), []string{"--workspace", ws, "--config-dir", cfg}, stdout, stderr)

	// Assert.
	if err != nil {
		t.Fatalf("runListTranscripts: %v (stderr %s)", err, stderr)
	}
	if stdout.String() != "" {
		t.Fatalf("stdout = %q, want an empty listing", stdout.String())
	}
}

func TestRunListTranscriptsRejectsARelativeWorkspace(t *testing.T) {
	// Arrange.
	stdout, stderr := new(bytes.Buffer), new(bytes.Buffer)

	// Act.
	err := runListTranscripts(context.Background(), []string{"--workspace", "relative/ws"}, stdout, stderr)

	// Assert.
	if err == nil {
		t.Fatal("runListTranscripts accepted a relative --workspace, want an error")
	}
	if !strings.Contains(err.Error(), "absolute path") {
		t.Fatalf("error = %v, want it to name the absolute-path requirement", err)
	}
}

func TestRunListTranscriptsFailsOnAnUnreadableProjectDir(t *testing.T) {
	// Arrange.
	cfg := t.TempDir()
	ws := t.TempDir()
	dir := session.ProjectDir(cfg, ws)
	if err := os.MkdirAll(dir, 0o755); err != nil {
		t.Fatal(err)
	}
	if err := os.Chmod(dir, 0o000); err != nil {
		t.Fatal(err)
	}
	t.Cleanup(func() { _ = os.Chmod(dir, 0o755) })
	stdout, stderr := new(bytes.Buffer), new(bytes.Buffer)

	// Act.
	err := runListTranscripts(context.Background(), []string{"--workspace", ws, "--config-dir", cfg}, stdout, stderr)

	// Assert.
	if err == nil {
		t.Fatal("runListTranscripts succeeded on an unreadable project dir, want an error")
	}
	if !strings.Contains(err.Error(), "read project dir") {
		t.Fatalf("error = %v, want it to name the unreadable project dir", err)
	}
}

func TestDispatchSubcommandRoutesListTranscripts(t *testing.T) {
	// Arrange.
	ws := t.TempDir()
	argv := []string{"claude-repld", listTranscriptsSubcommand, "--workspace", ws, "--config-dir", t.TempDir()}
	stdout, stderr := new(bytes.Buffer), new(bytes.Buffer)

	// Act.
	handled, code := dispatchSubcommand(context.Background(), argv, stdout, stderr)

	// Assert.
	if !handled {
		t.Fatal("dispatchSubcommand did not handle list-transcripts")
	}
	if code != 0 {
		t.Fatalf("exit code = %d, want 0 (stderr %s)", code, stderr)
	}
}
