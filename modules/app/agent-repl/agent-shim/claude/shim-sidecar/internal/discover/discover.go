// Package discover enumerates the Claude harness's on-disk artifacts (design
// §7.1) and classifies each path into a tail Target: file kind, task id, codec,
// and — for CONFIG-ROOT paths only — the session that owns it. Discovery unions
// config-root globs (session transcripts, agent sidechains + meta, workflow
// journals) with the /tmp task spools (kind by a*/b*/w* filename prefix). A
// periodic full Scan is the completeness backstop; fsnotify (Watcher) supplies
// latency.
//
// A SPOOL PATH IS A LOCATION, NEVER AN IDENTITY. The /tmp layout embeds a
// session-shaped segment (…/<session>/tasks/<taskid>.output), and this package
// used to read it as the owning session. It is not one: the harness names that
// directory with its RUNTIME session id, which differs from the id the
// conversation's transcript carries whenever a session was resumed. Trusting it
// filed one task under two different session ids, which is a state the store
// accepts (it keys by session_id + task_id) but the staleness tracker treats as
// an impossible collision — taking the whole file plane down with it.
//
// So a spool Target carries NO SessionID. Its owner is resolved by the sidecar
// from the task id, against the transcript that launched it (see the owner
// index in main.go). That leaves exactly one session identifier in the system:
// the transcript's.
package discover

import (
	"path/filepath"
	"strings"

	"agentrepl/shim-claude-sidecar/internal/logging"
	"agentrepl/shim-claude-sidecar/internal/tail"
)

// Target is one discovered file plus the attribution the tailer/handler need.
type Target struct {
	Path string
	Kind tail.Kind
	// SessionID is the owning session, read from a CONFIG-ROOT path only.
	// EMPTY for a /tmp spool: that path states where the bytes live, not whose
	// they are (see the package comment). The sidecar resolves a spool's owner
	// by TaskID before it is tailed.
	SessionID string
	TaskID    string // agent/shell/workflow files
	RunID     string // workflow journals
	SpoolDir  string // session's /tmp task dir (shell output_path construction)
	MetaPath  string // agent sidechain companion agent-<id>.meta.json, if any
	Raw       bool   // true → RawTextCodec (shell spool); else JSONLCodec
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
	spoolRoot   string   // e.g. /tmp (resolves /tmp/claude-<uid>/… itself)
	log         *logging.Bound
}

// New builds a Discoverer.
func New(configRoots []string, spoolRoot string, log *logging.Bound) *Discoverer {
	log.With(logging.Context{Operation: "discover-new"}).LogVerbose("constructing discoverer config_roots=%d spool_root=%q", len(configRoots), spoolRoot)
	return &Discoverer{configRoots: configRoots, spoolRoot: spoolRoot, log: log}
}

// Scan performs a full glob-based discovery across all roots (§7.1). It is the
// backstop that catches files that appeared while fsnotify was down.
func (d *Discoverer) Scan() []Target {
	d.log.With(logging.Context{Operation: "discover-scan"}).LogVerbose("scan start config_roots=%d spool_root=%q", len(d.configRoots), d.spoolRoot)
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
	d.log.With(logging.Context{Operation: "discover-scan"}).LogVerbose("scan complete targets=%d", len(out))
	return out
}

// Classify maps one absolute path to a Target. ok is false for a path that
// matches none of the §7.1 shapes (e.g. a meta.json companion, which is not
// tailed on its own).
func (d *Discoverer) Classify(path string) (Target, bool) {
	d.log.With(logging.Context{Operation: "discover-classify", Path: path}).LogVerbose("classify requested")
	if t, ok := d.classifyConfig(path); ok {
		d.log.With(logging.Context{Operation: "discover-classify", Path: path, Task: t.TaskID}).LogVerbose("classified config target kind=%d", t.Kind)
		return t, true
	}
	t, ok := d.classifySpool(path)
	if ok {
		d.log.With(logging.Context{Operation: "discover-classify", Path: path, Task: t.TaskID}).LogVerbose("classified spool target kind=%d raw=%t", t.Kind, t.Raw)
	} else {
		d.log.With(logging.Context{Operation: "discover-classify", Path: path}).LogVerbose("path does not match a watched artifact")
	}
	return t, ok
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
	//
	// segs[2] is session-SHAPED and is deliberately NOT read: it is the
	// harness's runtime session id, which disagrees with the transcript's
	// whenever a session was resumed. See the package comment — the owner is
	// resolved from TaskID instead, so no second identifier ever enters the
	// system.
	rel := path[len(d.spoolRoot)+1:] // claude-<uid>/<slug>/<session>/tasks/<file>
	segs := strings.Split(filepath.ToSlash(rel), "/")
	if len(segs) != 5 || segs[3] != "tasks" || !strings.HasSuffix(segs[4], ".output") {
		return Target{}, false
	}
	taskID := strings.TrimSuffix(segs[4], ".output")
	spoolDir := filepath.Dir(path)
	t := Target{
		Path:     path,
		TaskID:   taskID,
		SpoolDir: spoolDir,
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
		d.log.With(logging.Context{Operation: "classify-spool", Path: path, Task: taskID}).Log("spool task has no a/b/w kind prefix")
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
