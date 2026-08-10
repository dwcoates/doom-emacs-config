package main

import (
	"bufio"
	"context"
	"encoding/json"
	"flag"
	"fmt"
	"io"
	"os"
	"path/filepath"
	"sort"
	"strings"
	"time"

	"claude-repld/internal/dlog"
	"claude-repld/internal/session"
)

// listTranscriptsSubcommand is the one-off, runtime-free transcript listing
// entry point: `claude-repld list-transcripts --workspace W`.
//
// It answers "which conversations can I resume in this workspace?" for a caller
// that has no daemon, no registry session, and no Emacs — the resume half of
// opening a workspace from a terminal. The daemon's own resolver
// (server.ConversationResolver) answers a DIFFERENT question, "which single
// conversation should this create resume", from the registry; this command
// deliberately reads the transcripts on disk instead, because the registry
// names only conversations the daemon itself ran and the human at a terminal
// wants to pick from everything the CLI has written for the directory.
//
// CONFIG DIR RESOLUTION mirrors Emacs's agent-repl--compute-config-dir, whose
// account choice is a function of the workspace path:
//
//  1. --config-dir wins outright. It is this command's stand-in for the
//     workspace-level `:config-dir-override` an account switch leaves, which
//     lives in Emacs state this process cannot read.
//  2. Otherwise, a workspace under $MULTI_REPO_ROOT resolves to the multi-repo
//     account's root (~/.claude-chesscom, overridable with
//     AGENT_REPL_MULTI_REPO_CONFIG_DIR).
//  3. Otherwise the CLI's own default root: $CLAUDE_CONFIG_DIR, else ~/.claude
//     (session.DefaultClaudeConfigDir).
//
// The remaining rule — `agent-repl-doom-multi-repo-mode`, which re-points the
// doom config tree at the multi-repo account — is an Emacs-session toggle with
// no on-disk representation, so this command cannot evaluate it. Rather than
// guess, it PROBES every candidate root and prints what each one holds, with
// the root named in every row. That is session.Discover's discipline: name the
// transcripts under the other account loudly instead of silently resolving to
// one root and reporting a workspace as having no conversations at all.
const listTranscriptsSubcommand = "list-transcripts"

// The path-to-account rule this command applies lives in
// session.AccountConfigDirFor and its neighbours, shared with workspace
// creation.  It used to be a private copy here, and the copy was the only Go
// evaluation of the rule in existence — so creation had none.
const transcriptExcerptRuneCap = 120

// transcript status values. A transcript with no user prompt in it at all is a
// handshake — the CLI opened a conversation nobody said anything in — and
// resuming one gets an empty session back.
const (
	transcriptStatusOK    = "ok"
	transcriptStatusEmpty = "empty"
)

// listTranscriptsOptions is the parsed command line.
type listTranscriptsOptions struct {
	Workspace string
	ConfigDir string
	SkipEmpty bool
	Verbose   bool
}

// parseListTranscriptsArgs parses the subcommand's flags.
func parseListTranscriptsArgs(args []string, stderr io.Writer) (listTranscriptsOptions, error) {
	fs := flag.NewFlagSet("claude-repld "+listTranscriptsSubcommand, flag.ContinueOnError)
	fs.SetOutput(stderr)
	var opts listTranscriptsOptions
	fs.StringVar(&opts.Workspace, "workspace", "", "absolute path (or ~/ path) of the workspace whose transcripts are listed (required)")
	fs.StringVar(&opts.ConfigDir, "config-dir", "", "the Claude config root to read, overriding the account computed from the workspace path (empty = compute it, and also probe the other known roots)")
	fs.BoolVar(&opts.SkipEmpty, "skip-empty", false, "omit transcripts holding no user prompt at all instead of marking them \"empty\"")
	fs.BoolVar(&opts.Verbose, "verbose", false, "also emit verbose records to stderr")
	if err := fs.Parse(args); err != nil {
		return listTranscriptsOptions{}, err
	}
	if fs.NArg() != 0 {
		return listTranscriptsOptions{}, fmt.Errorf("unexpected positional argument %q; every input is a flag", fs.Arg(0))
	}
	if opts.Workspace == "" {
		return listTranscriptsOptions{}, fmt.Errorf("--workspace is required")
	}
	return opts, nil
}

// configDirsFor returns the config roots to read for a workspace: the primary
// one the account rule computes first, then every other known root, deduped.
//
// The primary is FIRST so a caller that wants one answer takes the head, and
// the others are still returned so nothing found on disk goes unreported.
func configDirsFor(workspace, override string) (primary string, others []string, err error) {
	if override != "" {
		primary, err = normalizeWorkspacePath(override)
		if err != nil {
			return "", nil, fmt.Errorf("--config-dir %q: %w", override, err)
		}
		return primary, nil, nil
	}
	multi, err := session.MultiRepoConfigDir()
	if err != nil {
		return "", nil, fmt.Errorf("resolve the multi-repo config dir: %w", err)
	}
	fallbackRoot := session.DefaultClaudeConfigDir()
	if fallbackRoot == "" {
		return "", nil, fmt.Errorf("resolve the default Claude config dir: neither $CLAUDE_CONFIG_DIR nor a home directory is available")
	}
	fallback, err := normalizeWorkspacePath(fallbackRoot)
	if err != nil {
		return "", nil, fmt.Errorf("default Claude config dir %q: %w", fallbackRoot, err)
	}
	// The primary root comes from the SHARED account rule rather than a second
	// evaluation of it here. That rule spells the default account "" (the way
	// every record spells it), which this command renders as the absolute root
	// it must actually stat.
	routed, err := session.AccountConfigDirFor(workspace)
	if err != nil {
		return "", nil, fmt.Errorf("resolve the account for %q: %w", workspace, err)
	}
	primary, other := fallback, multi
	if routed != "" {
		primary, other = routed, fallback
	}
	if other != primary {
		others = []string{other}
	}
	return primary, others, nil
}

// transcriptListing is one stdout row.
type transcriptListing struct {
	UUID      string
	ConfigDir string
	ModTime   time.Time
	Records   int
	Status    string
	Excerpt   string
}

// runListTranscripts enumerates a workspace's transcripts and writes them to
// stdout, newest first, one per line.
//
// FORMAT: tab-separated, six fields, no header and no quoting:
//
//	<uuid>\t<config-dir>\t<modified-rfc3339>\t<records>\t<status>\t<excerpt>
//
// `uuid` is the transcript's filename stem, which is exactly what `claude
// --resume` takes. `config-dir` is the root it was found under, which the
// caller must pass as CLAUDE_CONFIG_DIR when resuming it — a resume against the
// other account's root is a CLI that exits 1 on a uuid it cannot see.
// `records` is the transcript's JSON line count. `status` is "ok", or "empty"
// for a transcript with no user prompt in it at all. `excerpt` is the first
// user prompt flattened to one line and truncated, or "-" when there is none.
//
// A MISSING PROJECT DIR IS NOT AN ERROR: a workspace nobody has talked to in
// that account simply has no transcripts, and the empty listing says so.
func runListTranscripts(_ context.Context, args []string, stdout, stderr io.Writer) error {
	opts, err := parseListTranscriptsArgs(args, stderr)
	if err != nil {
		return err
	}
	// Records go to STDERR only: stdout is the machine-readable channel a shell
	// menu reads line by line.
	logger := dlog.New(io.Discard, stderr, opts.Verbose)
	logf := dlog.Legacy(logger.With("operation", listTranscriptsSubcommand))

	workspace, err := normalizeWorkspacePath(opts.Workspace)
	if err != nil {
		return fmt.Errorf("--workspace %q: %w", opts.Workspace, err)
	}
	primary, others, err := configDirsFor(workspace, opts.ConfigDir)
	if err != nil {
		return err
	}
	logf("list-transcripts: workspace=%s primary_config_dir=%s other_config_dirs=%v multi_repo_root=%q",
		workspace, primary, others, os.Getenv(session.MultiRepoRootEnv))

	var rows []transcriptListing
	for _, dir := range append([]string{primary}, others...) {
		found, err := transcriptsUnder(dir, workspace)
		if err != nil {
			return err
		}
		logf("list-transcripts: config_dir=%s project_dir=%s transcripts=%d", dir, session.ProjectDir(dir, workspace), len(found))
		rows = append(rows, found...)
	}
	// Newest first: the conversation a human means by "the last one" is the one
	// most recently written, whichever account root it lives under.
	sort.SliceStable(rows, func(i, j int) bool { return rows[i].ModTime.After(rows[j].ModTime) })

	printed := 0
	for _, row := range rows {
		if opts.SkipEmpty && row.Status == transcriptStatusEmpty {
			continue
		}
		if _, err := fmt.Fprintf(stdout, "%s\t%s\t%s\t%d\t%s\t%s\n",
			row.UUID, row.ConfigDir, row.ModTime.UTC().Format(time.RFC3339), row.Records, row.Status, row.Excerpt); err != nil {
			return fmt.Errorf("write transcript listing: %w", err)
		}
		printed++
	}
	logf("list-transcripts: found=%d listed=%d skip_empty=%v", len(rows), printed, opts.SkipEmpty)
	return nil
}

// transcriptsUnder reads every `<uuid>.jsonl` the CLI has written for workspace
// under configDir.
//
// A missing project dir is an empty result. Every other failure is surfaced: a
// project dir that exists but cannot be read is a listing that would silently
// hide resumable conversations.
func transcriptsUnder(configDir, workspace string) ([]transcriptListing, error) {
	dir := session.ProjectDir(configDir, workspace)
	entries, err := os.ReadDir(dir)
	if err != nil {
		if os.IsNotExist(err) {
			return nil, nil
		}
		return nil, fmt.Errorf("read project dir %s: %w", dir, err)
	}
	var out []transcriptListing
	for _, e := range entries {
		if e.IsDir() || !strings.HasSuffix(e.Name(), ".jsonl") {
			continue
		}
		info, err := e.Info()
		if err != nil {
			return nil, fmt.Errorf("stat transcript %s: %w", filepath.Join(dir, e.Name()), err)
		}
		records, excerpt, err := summarizeTranscript(filepath.Join(dir, e.Name()))
		if err != nil {
			return nil, err
		}
		status := transcriptStatusOK
		if excerpt == "" {
			status = transcriptStatusEmpty
			excerpt = "-"
		}
		out = append(out, transcriptListing{
			UUID:      strings.TrimSuffix(e.Name(), ".jsonl"),
			ConfigDir: configDir,
			ModTime:   info.ModTime(),
			Records:   records,
			Status:    status,
			Excerpt:   excerpt,
		})
	}
	return out, nil
}

// transcriptRecord is the sliver of the CLI's JSONL schema this command reads.
// Everything else in a record is ignored on purpose: a listing that decoded the
// whole schema would break every time the CLI added a field.
type transcriptRecord struct {
	Type    string `json:"type"`
	IsMeta  bool   `json:"isMeta"`
	Message struct {
		Role    string          `json:"role"`
		Content json.RawMessage `json:"content"`
	} `json:"message"`
}

// summarizeTranscript counts a transcript's records and extracts its first real
// user prompt.
//
// A line that is not valid JSON still COUNTS as a record but contributes no
// excerpt. A transcript is a file another process is appending to, so its last
// line is routinely a partial write, and refusing to list a live conversation
// over a torn tail would hide exactly the conversation the caller wants.
func summarizeTranscript(path string) (records int, excerpt string, err error) {
	f, err := os.Open(path)
	if err != nil {
		return 0, "", fmt.Errorf("open transcript %s: %w", path, err)
	}
	defer func() { _ = f.Close() }()

	scanner := bufio.NewScanner(f)
	// Transcript lines carry whole tool results and can be far larger than the
	// scanner's 64KiB default, which would otherwise abort the count.
	scanner.Buffer(make([]byte, 0, 64*1024), 16*1024*1024)
	for scanner.Scan() {
		line := strings.TrimSpace(scanner.Text())
		if line == "" {
			continue
		}
		records++
		if excerpt != "" {
			continue
		}
		var rec transcriptRecord
		if json.Unmarshal([]byte(line), &rec) != nil {
			continue
		}
		if rec.Type != "user" || rec.IsMeta || rec.Message.Role != "user" {
			continue
		}
		excerpt = transcriptExcerpt(rec.Message.Content)
	}
	if err := scanner.Err(); err != nil {
		return 0, "", fmt.Errorf("read transcript %s: %w", path, err)
	}
	return records, excerpt, nil
}

// transcriptExcerpt flattens a user message's content into one printable line,
// or "" when it carries no prompt a human typed.
//
// The CLI writes content either as a bare string or as a content-block array;
// both shapes are read. Tool results are skipped (they are user-role records
// the user never wrote), as are the `<command-...>` and `<local-command-...>`
// envelopes a slash command expands into.
func transcriptExcerpt(content json.RawMessage) string {
	var text string
	var asString string
	if json.Unmarshal(content, &asString) == nil {
		text = asString
	} else {
		var blocks []struct {
			Type string `json:"type"`
			Text string `json:"text"`
		}
		if json.Unmarshal(content, &blocks) != nil {
			return ""
		}
		for _, b := range blocks {
			if b.Type == "text" && strings.TrimSpace(b.Text) != "" {
				text = b.Text
				break
			}
		}
	}
	text = strings.TrimSpace(text)
	if text == "" || strings.HasPrefix(text, "<command-") || strings.HasPrefix(text, "<local-command-") {
		return ""
	}
	return flattenExcerpt(text)
}

// flattenExcerpt makes one tab-free, newline-free, length-capped line, which is
// what the row format can express. Truncation is by rune so a multi-byte
// character is never cut in half.
func flattenExcerpt(text string) string {
	text = strings.Join(strings.Fields(text), " ")
	runes := []rune(text)
	if len(runes) > transcriptExcerptRuneCap {
		return string(runes[:transcriptExcerptRuneCap]) + "…"
	}
	return text
}
