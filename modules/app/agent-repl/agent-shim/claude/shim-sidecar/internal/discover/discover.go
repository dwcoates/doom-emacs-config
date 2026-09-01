// Package discover enumerates the vendor's on-disk artifacts across BOTH
// account config roots and the task-spool root, and classifies each path into a
// tail Target: file kind, identity, codec, and the companion meta file a kind
// requires. A periodic full Scan is the completeness backstop; fsnotify
// (Watcher) supplies latency.
//
// FOUR KINDS OF FILE, all written by the vendor's agent binary:
//
//  1. session transcripts      projects/<project>/<session>.jsonl
//  2. subagent transcripts     projects/<project>/<session>/subagents/agent-<id>.jsonl
//     (+ agent-<id>.meta.json, REQUIRED: the only source of the agent's type,
//     spawn depth, model and worktree)
//  3. workflow journals and their per-agent transcripts, under
//     .../subagents/workflows/wf_<id>/
//  4. task spools              <spool root>/[claude-<uid>/]<cwd-slug>/<vendor session>/tasks/<task>.output
//
// MULTI-ROOT IS NOT OPTIONAL: the second account's config dir holds real
// transcripts, and a single-root scan simply cannot see them.
//
// NOTHING IS EVER DECODED FROM THE <cwd-slug> DIRECTORY NAME. The vendor builds
// it by replacing every byte of the absolute cwd outside [A-Za-z0-9] with `-`
// (underscores included, case preserved), so `/private/var/folders/_m/x`
// becomes `-private-var-folders--m-x`. THAT MAPPING IS LOSSY AND NOT
// INVERTIBLE: two different directories can produce one slug, so a slug read
// back as a workspace path would be a guess. This package therefore treats the
// slug as an opaque directory name and reads its POSITION only — never its
// content. Every identity comes from what is INSIDE it: the session uuid file
// names, `subagents/`, `wf_*`, `agent-<id>`, and the `tasks/` basenames. Do not
// add a slug-to-path decoder, and never compare slugs across roots as though
// they were paths.
//
// A SPOOL PATH IS A LOCATION, NEVER AN IDENTITY. The spool layout embeds a
// session-shaped segment, and this package used to read it as the owning
// session. It is not one: the harness names that directory with its RUNTIME
// session id, which differs from the id the transcript carries whenever a
// session was resumed. Trusting it filed one task under two ids. So a spool
// Target carries NO SessionID; its owner is resolved by task id against the
// call that launched it (see the root package's owner index).
//
// A SPOOL'S KIND IS ITS TASK-ID PREFIX: b* shell output, a* an agent
// transcript, w* a workflow spool (declared residue — workflow is kicked this
// wave). Any other prefix is a TOTAL-INGESTION
// VIOLATION — logged at error, and still discovered as KindResidueSpool so its
// bytes land whole as residue. Dropping the file from discovery, which is what
// this package used to do, is the one outcome the mandate forbids.
//
// EVERY DISCOVERED PATH IS SYMLINK-RESOLVED. On macOS /tmp is a symlink to
// /private/tmp, so the same spool reaches this package under two spellings; a
// path that is compared (against an owner's output path, against a watcher's
// key) must first be resolved, or one file reads as two.
package discover

import (
	"os"
	"path/filepath"
	"strings"

	"agentrepl/shim-claude-sidecar/internal/logging"
	"agentrepl/shim-claude-sidecar/internal/tail"
)

// Target is one discovered file plus the attribution the tailer and handler
// need. Path is always symlink-resolved.
type Target struct {
	Path string
	Kind tail.Kind

	// SessionID is the owning session's vendor uuid, read from a CONFIG-ROOT
	// path only. EMPTY for a spool: that path states where the bytes live, not
	// whose they are.
	SessionID string
	// AgentID is the agent's IDENTITY: for a subagent transcript, the spawning
	// call's tool_use_id read from the companion meta file (the cross-plane
	// minting rule), NEVER the `agent-<id>` of the file name.
	AgentID string
	// VendorAgentID is the `agent-<id>` of the file name — a LOCATOR that names
	// which file this is, and the id the parent transcript's sidechain records
	// carry. It is deliberately kept apart from AgentID: it is not an identity
	// and never reaches the wire.
	VendorAgentID string
	// Meta is the parsed companion file, present whenever MetaMissing is false
	// for a target that requires one.
	Meta Meta

	TaskID   string // spool task id, and the subagent id for a sidechain file
	RunID    string // workflow run id from the journal path
	SpoolDir string // the session's task spool dir
	// MetaPath is the companion agent-<id>.meta.json a subagent or
	// workflow-agent transcript REQUIRES before it can be ingested.
	MetaPath string
	// MetaMissing reports that MetaPath is required and not on disk yet. Such a
	// target is HELD — discovered and re-checked, never tailed and never
	// dropped — because its records cannot be attributed without the meta.
	MetaMissing bool
	// Raw selects RawTextCodec (a shell spool) over JSONLCodec.
	Raw bool
	// ConfigRoot is the discovery root the path was found under; empty for a
	// spool, whose root is the spool root.
	ConfigRoot string
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
	configRoots []string
	spoolRoot   string
	log         *logging.Bound

	// warnedMeta remembers which held transcripts have already had their
	// warning, so a rescan every few seconds does not repeat one line forever
	// while a meta file is being written.
	warnedMeta map[string]bool

	// statedUnclassifiable remembers which spools have already had their
	// classification defect stated.
	//
	// EVERY SCAN RE-CLASSIFIES EVERY FILE, and a spool whose task id carries no
	// kind prefix will never grow one — so without this the same defect is
	// restated at ERROR on every rescan, for as long as the file exists. That is
	// the identical loop the parking and suspension rulings forbid elsewhere:
	// one unfixable defect drowning every other reader's records. The fact is
	// stated ONCE per path, and the re-classifications after it are verbose.
	statedUnclassifiable map[string]bool
}

// New builds a Discoverer over the given config roots and spool root. Every
// root is symlink-resolved once here, so a path compared against a root is
// compared in the same spelling.
func New(configRoots []string, spoolRoot string, log *logging.Bound) *Discoverer {
	resolved := make([]string, 0, len(configRoots))
	for _, root := range configRoots {
		resolved = append(resolved, Normalize(root))
	}
	log.With(logging.Context{Operation: "discover-new"}).
		LogVerbose("constructing discoverer config_roots=%v spool_root=%q", resolved, Normalize(spoolRoot))
	return &Discoverer{
		configRoots: resolved,
		spoolRoot:   Normalize(spoolRoot),
		log:         log,
		warnedMeta:  map[string]bool{},

		statedUnclassifiable: map[string]bool{},
	}
}

// ConfigRoots returns the resolved config roots.
func (d *Discoverer) ConfigRoots() []string { return d.configRoots }

// SpoolRoot returns the resolved spool root.
func (d *Discoverer) SpoolRoot() string { return d.spoolRoot }

// Scan performs a full glob-based discovery across every root. It is the
// backstop that catches files that appeared while fsnotify was down.
func (d *Discoverer) Scan() []Target {
	d.log.With(logging.Context{Operation: "discover-scan"}).
		LogVerbose("scan start config_roots=%d spool_root=%q", len(d.configRoots), d.spoolRoot)
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
		for _, match := range globAll(
			filepath.Join(root, "projects", "*", "*.jsonl"),
			filepath.Join(root, "projects", "*", "*", "subagents", "agent-*.jsonl"),
			filepath.Join(root, "projects", "*", "*", "subagents", "workflows", "wf_*", "journal.jsonl"),
			filepath.Join(root, "projects", "*", "*", "subagents", "workflows", "wf_*", "agent-*.jsonl"),
		) {
			add(d.Classify(match))
		}
	}
	// BOTH SPOOL-ROOT SPELLINGS ARE ACCEPTED. The vendor writes spools under
	// <uid dir>/<cwd-slug>/<vendor session>/tasks/, and a caller may point
	// --spool-root either at the parent of the uid dir (production: /tmp, which
	// resolves claude-<uid> itself) or directly at the uid dir (a mock harness,
	// whose own root default IS that directory). Neither spelling may make a
	// spool invisible, so both are globbed.
	for _, match := range globAll(
		filepath.Join(d.spoolRoot, "claude-*", "*", "*", "tasks", "*.output"),
		filepath.Join(d.spoolRoot, "*", "*", "tasks", "*.output"),
	) {
		add(d.Classify(match))
	}
	d.log.With(logging.Context{Operation: "discover-scan"}).LogVerbose("scan complete targets=%d", len(out))
	return out
}

// Classify maps one path to a Target. ok is false only for a path that matches
// none of the four shapes (a meta.json companion, say, which is never tailed on
// its own).
func (d *Discoverer) Classify(path string) (Target, bool) {
	path = Normalize(path)
	if target, ok := d.classifyConfig(path); ok {
		d.log.With(logging.Context{Operation: "discover-classify", Path: path, TaskID: target.TaskID, AgentID: target.AgentID}).
			LogVerbose("classified config target kind=%s meta_missing=%t", target.Kind, target.MetaMissing)
		return target, true
	}
	target, ok := d.classifySpool(path)
	if ok {
		d.log.With(logging.Context{Operation: "discover-classify", Path: path, TaskID: target.TaskID}).
			LogVerbose("classified spool target kind=%s raw=%t", target.Kind, target.Raw)
	} else {
		d.log.With(logging.Context{Operation: "discover-classify", Path: path}).
			LogVerbose("path matches none of the four watched shapes")
	}
	return target, ok
}

func (d *Discoverer) classifyConfig(path string) (Target, bool) {
	for _, root := range d.configRoots {
		prefix := filepath.Join(root, "projects") + string(filepath.Separator)
		if !strings.HasPrefix(path, prefix) {
			continue
		}
		segs := strings.Split(filepath.ToSlash(path[len(prefix):]), "/")
		// segs[0] is the <cwd-slug> directory. It is matched POSITIONALLY and
		// never read: the vendor's slug is a lossy, non-invertible rendering of
		// the cwd (see the package comment), so anything decoded from it is a
		// guess. Every identity below comes from a segment INSIDE it.
		switch {
		case len(segs) == 2 && strings.HasSuffix(segs[1], ".jsonl"):
			// projects/<project>/<session>.jsonl
			//
			// THE FILE'S BASENAME IS THE MAIN AGENT'S IDENTITY, not the
			// per-record sessionId field, which diverges from it.
			return Target{
				Path:       path,
				Kind:       tail.KindSessionTranscript,
				SessionID:  strings.TrimSuffix(segs[1], ".jsonl"),
				ConfigRoot: root,
			}, true
		case len(segs) == 4 && segs[2] == "subagents" && isAgentTranscript(segs[3]):
			// projects/<project>/<session>/subagents/agent-<id>.jsonl
			return d.withMeta(Target{
				Path:          path,
				Kind:          tail.KindAgentTranscript,
				SessionID:     segs[1],
				VendorAgentID: agentIDOf(segs[3]),
				TaskID:        agentIDOf(segs[3]),
				ConfigRoot:    root,
			}), true
		case len(segs) == 6 && segs[2] == "subagents" && segs[3] == "workflows" &&
			strings.HasPrefix(segs[4], "wf_") && segs[5] == "journal.jsonl":
			// projects/<project>/<session>/subagents/workflows/wf_<id>/journal.jsonl
			return Target{
				Path:       path,
				Kind:       tail.KindWorkflowJournal,
				SessionID:  segs[1],
				RunID:      segs[4],
				TaskID:     segs[4],
				ConfigRoot: root,
			}, true
		case len(segs) == 6 && segs[2] == "subagents" && segs[3] == "workflows" &&
			strings.HasPrefix(segs[4], "wf_") && isAgentTranscript(segs[5]):
			// projects/<project>/<session>/subagents/workflows/wf_<id>/agent-<id>.jsonl
			//
			// A workflow's PER-AGENT transcript. It is tailed like any other
			// file, but workflow conversion is kicked this wave, so its records
			// land as residue rather than as feed rows.
			return d.withMeta(Target{
				Path:          path,
				Kind:          tail.KindWorkflowJournal,
				SessionID:     segs[1],
				RunID:         segs[4],
				VendorAgentID: agentIDOf(segs[5]),
				TaskID:        agentIDOf(segs[5]),
				ConfigRoot:    root,
			}), true
		}
		return Target{}, false
	}
	return Target{}, false
}

// withMeta attaches the companion meta path and states whether it exists yet.
//
// A TRANSCRIPT WITHOUT ITS META IS HELD, NEVER DROPPED: agent-<id>.meta.json is
// the ONLY source of the agent's type, spawn depth, model and worktree, so its
// records cannot be attributed without it. The file keeps being discovered and
// re-checked until the meta appears.
func (d *Discoverer) withMeta(target Target) Target {
	target.MetaPath = strings.TrimSuffix(target.Path, ".jsonl") + ".meta.json"
	if _, err := os.Stat(target.MetaPath); err == nil {
		meta, err := ReadMeta(target.MetaPath)
		if err != nil {
			// A meta file that is THERE but unreadable is held exactly as a
			// missing one is: the agent has no identity either way, and naming
			// it by its filename would mint a second book for one agent. Loud,
			// because unlike a missing file this one will not fix itself.
			target.MetaMissing = true
			d.log.With(logging.Context{
				Operation: "discover-meta", Path: target.Path, Level: "error",
			}).Log("transcript held: its meta file could not be read, so the agent has no identity and its records cannot be attributed: %v", err)
			return target
		}
		// THE AGENT'S IDENTITY IS THE SPAWNING CALL, per the cross-plane minting
		// rule: the same id the stream plane names this agent by.
		target.Meta = meta
		target.AgentID = meta.ToolUseID
		if d.warnedMeta[target.Path] {
			delete(d.warnedMeta, target.Path)
			d.log.With(logging.Context{Operation: "discover-meta", Path: target.Path, AgentID: target.AgentID}).
				Log("the held transcript's meta file appeared at %s; it is ingestible now", target.MetaPath)
		}
		return target
	}
	target.MetaMissing = true
	if !d.warnedMeta[target.Path] {
		d.warnedMeta[target.Path] = true
		d.log.With(logging.Context{Operation: "discover-meta", Path: target.Path, TaskID: target.VendorAgentID, Level: "warn"}).
			Log("transcript held: its required meta file %s is not on disk yet, so the agent has no identity (its spawning call is stated only there); it is re-checked every rescan and never dropped", target.MetaPath)
		return target
	}
	d.log.With(logging.Context{Operation: "discover-meta", Path: target.Path, AgentID: target.AgentID}).
		LogVerbose("transcript still held: %s has not appeared", target.MetaPath)
	return target
}

func (d *Discoverer) classifySpool(path string) (Target, bool) {
	prefix := d.spoolRoot + string(filepath.Separator)
	if !strings.HasPrefix(path, prefix) {
		return Target{}, false
	}
	segs := strings.Split(filepath.ToSlash(path[len(prefix):]), "/")
	// Two accepted shapes, because --spool-root may be pointed at either level:
	//
	//	claude-<uid>/<cwd-slug>/<vendor session>/tasks/<task>.output   (root = /tmp)
	//	<cwd-slug>/<vendor session>/tasks/<task>.output                (root = the uid dir)
	//
	// NEITHER MIDDLE SEGMENT IS READ. <cwd-slug> is the vendor's lossy,
	// non-invertible rendering of the cwd; <vendor session> is the harness's
	// RUNTIME session id, which disagrees with the transcript's whenever a
	// session was resumed. Only the task basename below carries an identity.
	switch {
	case len(segs) == 5 && strings.HasPrefix(segs[0], "claude-"):
		segs = segs[1:]
	case len(segs) == 4:
	default:
		return Target{}, false
	}
	if segs[2] != "tasks" || !strings.HasSuffix(segs[3], ".output") {
		return Target{}, false
	}
	taskID := strings.TrimSuffix(segs[3], ".output")
	target := Target{Path: path, TaskID: taskID, SpoolDir: filepath.Dir(path)}
	switch {
	case strings.HasPrefix(taskID, "b"):
		target.Kind = tail.KindShellSpool
		target.Raw = true
	case strings.HasPrefix(taskID, "a"):
		target.Kind = tail.KindAgentTranscript
	case strings.HasPrefix(taskID, "w"):
		// R-S4: workflow is KICKED this wave, so a w* spool is discovered and
		// cursor-tailed like any other file but converted only to DECLARED
		// residue. It is read RAW because there is no conversion that would use
		// its record structure, and reading it as JSONL would refuse lines a
		// workflow spool is under no obligation to make parseable.
		target.Kind = tail.KindWorkflowSpool
		target.Raw = true
	default:
		// THE PREFIX IS HOW A SPOOL'S CONVERSION IS SELECTED, so one we do not
		// recognize means the bytes cannot be converted. They are still
		// ingested — whole, as residue — because a file dropped from discovery
		// is the one thing total ingestion forbids.
		target.Kind = tail.KindResidueSpool
		target.Raw = true
		bound := d.log.With(logging.Context{Operation: "classify-spool", Path: path, TaskID: taskID})
		if d.statedUnclassifiable[path] {
			bound.LogVerbose("spool task id still has no a/b/w kind prefix; the defect was already stated for this path")
			break
		}
		d.statedUnclassifiable[path] = true
		bound.With(logging.Context{Level: "error"}).
			Log("spool task id has no a/b/w kind prefix: its conversion cannot be selected, so its bytes are ingested as unparsed residue rather than dropped")
	}
	return target, true
}

// Normalize resolves a path to the spelling everything else compares against.
//
// Two spellings of one file — notably macOS's /tmp symlink onto /private/tmp —
// must collapse to one key, so symlinks are RESOLVED rather than merely
// cleaned. A path is observed both before and after it exists and
// filepath.EvalSymlinks fails on a missing path, so resolution walks up to the
// deepest existing ancestor, resolves that, and rejoins the not-yet-created
// suffix. When nothing on the path exists the cleaned path is returned:
// normalization never fails and never drops a path.
func Normalize(path string) string {
	if path == "" {
		return ""
	}
	cleaned := filepath.Clean(path)
	var suffix []string
	current := cleaned
	for {
		if resolved, err := filepath.EvalSymlinks(current); err == nil {
			for i := len(suffix) - 1; i >= 0; i-- {
				resolved = filepath.Join(resolved, suffix[i])
			}
			return resolved
		}
		parent := filepath.Dir(current)
		if parent == current {
			return cleaned
		}
		suffix = append(suffix, filepath.Base(current))
		current = parent
	}
}

func isAgentTranscript(name string) bool {
	return strings.HasPrefix(name, "agent-") && strings.HasSuffix(name, ".jsonl")
}

func agentIDOf(name string) string {
	return strings.TrimSuffix(strings.TrimPrefix(name, "agent-"), ".jsonl")
}

func globAll(patterns ...string) []string {
	var out []string
	for _, pattern := range patterns {
		if matches, err := filepath.Glob(pattern); err == nil {
			out = append(out, matches...)
		}
	}
	return out
}
