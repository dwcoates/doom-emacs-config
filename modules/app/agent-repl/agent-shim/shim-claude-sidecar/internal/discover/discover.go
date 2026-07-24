// Package discover enumerates the Claude harness's on-disk artifacts (design
// §7.1) and classifies each path into a tail Target: its session (from the
// PATH), file kind, task id, and codec. Discovery unions config-root globs
// (session transcripts, agent sidechains + meta, workflow journals) with the
// /tmp task spools (kind by a*/b*/w* filename prefix). A periodic full Scan is
// the completeness backstop; fsnotify (Watcher) supplies latency.
package discover

import (
	"path/filepath"
	"strings"

	"agentrepl/shim-claude-sidecar/internal/tail"
)

// Logf is the loud-logging sink (§12).
type Logf = func(format string, args ...any)

// Target is one discovered file plus the attribution the tailer/handler need.
type Target struct {
	Path      string
	Kind      tail.Kind
	SessionID string
	TaskID    string    // agent/shell/workflow files
	RunID     string    // workflow journals
	SpoolDir  string    // session's /tmp task dir (shell output_path construction)
	MetaPath  string    // agent sidechain companion agent-<id>.meta.json, if any
	Raw       bool      // true → RawTextCodec (shell spool); else JSONLCodec
}

// Codec returns the framing codec for this target.
func (t Target) Codec() tail.Codec {
	if t.Raw {
		return tail.RawTextCodec{}
	}
	return tail.JSONLCodec{}
}

// Discoverer holds the configured roots and performs discovery.
type Discoverer struct {
	configRoots []string // e.g. ~/.claude, ~/.claude-chesscom
	spoolRoot   string    // e.g. /tmp (resolves /tmp/claude-<uid>/… itself)
	log         Logf
}

// New builds a Discoverer.
func New(configRoots []string, spoolRoot string, log Logf) *Discoverer {
	if log == nil {
		log = func(string, ...any) {}
	}
	return &Discoverer{configRoots: configRoots, spoolRoot: spoolRoot, log: log}
}

// Scan performs a full glob-based discovery across all roots (§7.1). It is the
// backstop that catches files that appeared while fsnotify was down.
func (d *Discoverer) Scan() []Target {
	var out []Target
	seen := map[string]bool{}
	add := func(t Target, ok bool) {
		if !ok || seen[t.Path] {
			return
		}
		seen[t.Path] = true
		out = append(out, t)
	}
	for _, root := range d.configRoots {
		for _, m := range globAll(
			filepath.Join(root, "projects", "*", "*.jsonl"),
			filepath.Join(root, "projects", "*", "*", "subagents", "agent-*.jsonl"),
			filepath.Join(root, "projects", "*", "*", "subagents", "workflows", "wf_*", "journal.jsonl"),
		) {
			add(d.Classify(m))
		}
	}
	for _, m := range globAll(filepath.Join(d.spoolRoot, "claude-*", "*", "*", "tasks", "*.output")) {
		add(d.Classify(m))
	}
	return out
}

// Classify maps one absolute path to a Target. ok is false for a path that
// matches none of the §7.1 shapes (e.g. a meta.json companion, which is not
// tailed on its own).
func (d *Discoverer) Classify(path string) (Target, bool) {
	if t, ok := d.classifyConfig(path); ok {
		return t, true
	}
	return d.classifySpool(path)
}

func (d *Discoverer) classifyConfig(path string) (Target, bool) {
	for _, root := range d.configRoots {
		prefix := filepath.Join(root, "projects") + string(filepath.Separator)
		if !strings.HasPrefix(path, prefix) {
			continue
		}
		segs := strings.Split(filepath.ToSlash(path[len(prefix):]), "/")
		// segs[0] = project dir.
		switch {
		case len(segs) == 2 && strings.HasSuffix(segs[1], ".jsonl"):
			// projects/<project>/<session>.jsonl
			return Target{
				Path:      path,
				Kind:      tail.KindSessionTranscript,
				SessionID: strings.TrimSuffix(segs[1], ".jsonl"),
			}, true
		case len(segs) == 4 && segs[2] == "subagents" && matchesAgent(segs[3]):
			// projects/<project>/<session>/subagents/agent-<id>.jsonl
			session := segs[1]
			id := strings.TrimSuffix(strings.TrimPrefix(segs[3], "agent-"), ".jsonl")
			return Target{
				Path:      path,
				Kind:      tail.KindAgentTranscript,
				SessionID: session,
				TaskID:    id,
				MetaPath:  strings.TrimSuffix(path, ".jsonl") + ".meta.json",
			}, true
		case len(segs) == 6 && segs[2] == "subagents" && segs[3] == "workflows" &&
			strings.HasPrefix(segs[4], "wf_") && segs[5] == "journal.jsonl":
			// projects/<project>/<session>/subagents/workflows/wf_<id>/journal.jsonl
			return Target{
				Path:      path,
				Kind:      tail.KindWorkflowJournal,
				SessionID: segs[1],
				RunID:     segs[4],
				TaskID:    segs[4],
			}, true
		}
		return Target{}, false
	}
	return Target{}, false
}

func (d *Discoverer) classifySpool(path string) (Target, bool) {
	prefix := d.spoolRoot + string(filepath.Separator) + "claude-"
	if !strings.HasPrefix(path, d.spoolRoot+string(filepath.Separator)) || !strings.Contains(path, "claude-") {
		return Target{}, false
	}
	if !strings.HasPrefix(filepath.ToSlash(path), filepath.ToSlash(prefix)) {
		return Target{}, false
	}
	// /tmp/claude-<uid>/<slug>/<session>/tasks/<taskid>.output
	rel := path[len(d.spoolRoot)+1:] // claude-<uid>/<slug>/<session>/tasks/<file>
	segs := strings.Split(filepath.ToSlash(rel), "/")
	if len(segs) != 5 || segs[3] != "tasks" || !strings.HasSuffix(segs[4], ".output") {
		return Target{}, false
	}
	session := segs[2]
	taskID := strings.TrimSuffix(segs[4], ".output")
	spoolDir := filepath.Dir(path)
	t := Target{
		Path:      path,
		SessionID: session,
		TaskID:    taskID,
		SpoolDir:  spoolDir,
	}
	switch {
	case strings.HasPrefix(taskID, "a"):
		t.Kind = tail.KindAgentTranscript // a*.output is agent JSONL
	case strings.HasPrefix(taskID, "b"):
		t.Kind = tail.KindShellSpool
		t.Raw = true
	case strings.HasPrefix(taskID, "w"):
		t.Kind = tail.KindWorkflowJournal
	default:
		d.log("discover: spool task id %q has no a/b/w kind prefix (%s)", taskID, path)
		return Target{}, false
	}
	return t, true
}

func matchesAgent(name string) bool {
	return strings.HasPrefix(name, "agent-") && strings.HasSuffix(name, ".jsonl") &&
		!strings.HasSuffix(name, ".meta.json")
}

func globAll(patterns ...string) []string {
	var out []string
	for _, p := range patterns {
		if m, err := filepath.Glob(p); err == nil {
			out = append(out, m...)
		}
	}
	return out
}
