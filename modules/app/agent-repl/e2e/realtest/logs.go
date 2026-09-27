//go:build realtest

package realtest

import (
	"fmt"
	"os"
	"path/filepath"
	"regexp"
	"sort"
)

// WHERE THE LOGS ARE is settled by modules/app/agent-repl/logging-contract.md,
// and this file is nothing but that document's persistence layout read into Go.
// Nothing here may invent a path. A separate logging track owns the operator's
// reader (bin/logs.sh, docs/LOGGING.md); the harvester below stays deliberately
// minimal and reads exactly what the contract names:
//
//   - The five CANONICAL SYMLINKS inside each workspace,
//     `<workspace>/.claude/emacs/{emacs,daemon,shim,webapp,sidecar}.log`. Each
//     link points at an external target the owning runtime created and opened.
//     The link is the only supported way in: the contract has the runtime
//     atomically REPLACE the link after a restart rather than trusting the old
//     destination, so a reader that resolved a target once and remembered it
//     would be reading a file nobody writes to any more.
//
//   - Each runtime's canonical GLOBAL sink, for records that genuinely have no
//     workspace: the module's own elisp log, the daemon's `daemon.run.log` with
//     its rotation siblings, and the store's and sidecar's logs under
//     ~/.cache/agent-repl/log.
//
// The per-workspace targets themselves live under ~/.claude-emacs/logs and
// under the OS temporary directory. They are NOT enumerated: every one of them
// is already reachable through the workspace link that names it, and harvesting
// both would report every record twice.

// SourceKind is how a source's bytes are to be read.
type SourceKind int

const (
	// KindJSONL is one JSON object per line — the shape the logging contract
	// requires of every persisted record. Human-formatted persisted records
	// are forbidden, which is what makes an unparseable line a finding
	// rather than noise.
	KindJSONL SourceKind = iota
	// KindStderr is unstructured process stderr: the store's and the
	// sidecar's `.err.log`. The contract permits emergency output only when
	// the canonical sink cannot record its own failure, so anything appended
	// to one of these during a run window is itself the finding — there is no
	// level to filter on.
	KindStderr
)

func (k SourceKind) String() string {
	switch k {
	case KindJSONL:
		return "jsonl"
	case KindStderr:
		return "stderr"
	default:
		return fmt.Sprintf("SourceKind(%d)", int(k))
	}
}

// Source is one log a run harvests.
type Source struct {
	// Name is the logical name a finding is reported under, stable across
	// rotations and relinks so a report reads as "the daemon log" rather than
	// as a list of temporary file names.
	Name string
	// Path is the path the harvester READS — for a per-workspace source, the
	// canonical symlink, never its resolved target.
	Path string
	Kind SourceKind
	// Workspace is the workspace this sink belongs to, which for the five
	// canonical links is the workspace whose directory holds them. Empty for a
	// global sink, where attribution comes from the record's own fields.
	//
	// It is kept separate from the record's attribution on purpose: when the
	// two disagree, the routing invariant the contract calls out has been
	// broken, and a single merged field could not have said so.
	Workspace string
}

// Workspace is one workspace a run knows about, as the state database holds it.
type Workspace struct {
	ID   string
	Dir  string
	Name string
}

// workspaceSinks are the five canonical per-workspace links, in the contract's
// own order. Emacs owns emacs.log; the daemon owns daemon.log and persists
// forwarded browser and sidecar records into webapp.log and sidecar.log; the
// shim writes shim.log directly through the descriptor the daemon passes it.
var workspaceSinks = []string{"emacs.log", "daemon.log", "shim.log", "webapp.log", "sidecar.log"}

// Env is every root the harvester reads from, stated explicitly rather than
// resolved from the ambient environment — which is what lets a unit test build
// an Env over t.TempDir() and run these exact code paths.
type Env struct {
	// StateDir is ~/.claude-emacs, which holds the daemon's global sink.
	StateDir string
	// ModuleLog is the elisp global sink, `agent-repl-log-file-name` in
	// lisp/core.el. Its rotation sibling is `<path>.prev`.
	ModuleLog string
	// CacheLogDir is ~/.cache/agent-repl/log: the store's and the sidecar's
	// global sinks, structured and stderr.
	CacheLogDir string
	// Workspaces is what the state database holds. Each contributes its five
	// canonical links.
	Workspaces []Workspace
}

// RealEnv is the owner's machine.
//
// The module log resolves the way lisp/core.el resolves it — the durable
// central sink `logs/emacs.central.log` under the state root, beside the
// daemon's own run log. Both earlier defaults (`doom-agent-repl.log` at the
// state root, then under the UID-qualified `temporary-file-directory`) are
// RETIRED and redirected by `agent-repl--normalize-log-file-name`, so files
// still standing at those paths hold only historical records and no run may
// harvest them.
func RealEnv(home string, workspaces []Workspace) Env {
	return Env{
		StateDir:    filepath.Join(home, ".claude-emacs"),
		ModuleLog:   filepath.Join(home, ".claude-emacs", "logs", "emacs.central.log"),
		CacheLogDir: filepath.Join(home, ".cache", "agent-repl", "log"),
		Workspaces:  workspaces,
	}
}

// daemonRunLogRe matches the daemon's global sink and its rotation siblings.
var daemonRunLogRe = regexp.MustCompile(`^daemon\.run\.log(\.[0-9]+)?$`)

// cacheServiceLogRe matches the two launchd services' global sinks and their
// rotation siblings. `.out.log` is excluded: it is empty in practice and, when
// it is not, it is stderr-shaped, which `.err.log` already covers as its own
// kind.
var cacheServiceLogRe = regexp.MustCompile(`^(shim-store|shim-claude-sidecar)\.log(\.[0-9]+)?$`)

// cacheServiceErrRe matches the two services' stderr files.
var cacheServiceErrRe = regexp.MustCompile(`^(shim-store|shim-claude-sidecar)\.err\.log$`)

// EnumerateSources returns every log a run harvests, in a stable order.
//
// A named path that does not exist is included anyway: a source recorded at
// snapshot time as absent and read from zero afterwards is exactly how a file
// the run itself created gets harvested, and dropping absent paths here would
// need a second enumeration pass to catch them.
//
// The two DIRECTORY globs cannot be settled in advance — rotation siblings
// appear during a run — so they are re-globbed at harvest time and anything new
// is picked up then.
func EnumerateSources(env Env) ([]Source, error) {
	var sources []Source

	if env.ModuleLog != "" {
		sources = append(sources,
			Source{Name: "emacs.global", Path: env.ModuleLog, Kind: KindJSONL},
			Source{Name: "emacs.global", Path: env.ModuleLog + ".prev", Kind: KindJSONL},
		)
	}

	for _, ws := range env.Workspaces {
		if ws.Dir == "" {
			continue
		}
		for _, sink := range workspaceSinks {
			sources = append(sources, Source{
				Name:      "workspace." + sink,
				Path:      filepath.Join(ws.Dir, ".claude", "emacs", sink),
				Kind:      KindJSONL,
				Workspace: ws.ID,
			})
		}
	}

	if env.StateDir != "" {
		found, err := globDir(filepath.Join(env.StateDir, "logs"), func(name string) (Source, bool) {
			if daemonRunLogRe.MatchString(name) {
				return Source{Name: "daemon.global", Kind: KindJSONL}, true
			}
			return Source{}, false
		})
		if err != nil {
			return nil, err
		}
		sources = append(sources, found...)
	}

	if env.CacheLogDir != "" {
		found, err := globDir(env.CacheLogDir, func(name string) (Source, bool) {
			if m := cacheServiceLogRe.FindStringSubmatch(name); m != nil {
				return Source{Name: m[1] + ".global", Kind: KindJSONL}, true
			}
			if m := cacheServiceErrRe.FindStringSubmatch(name); m != nil {
				return Source{Name: m[1] + ".stderr", Kind: KindStderr}, true
			}
			return Source{}, false
		})
		if err != nil {
			return nil, err
		}
		sources = append(sources, found...)
	}

	sort.SliceStable(sources, func(i, j int) bool {
		if sources[i].Name != sources[j].Name {
			return sources[i].Name < sources[j].Name
		}
		return sources[i].Path < sources[j].Path
	})
	return sources, nil
}

// globDir enumerates one directory through a classifier.
//
// A missing directory is not an error: before the first daemon has ever run
// there is nothing there, and a run that finds nothing to harvest is a fact
// about the run rather than a failure of the enumeration.
func globDir(dir string, classify func(name string) (Source, bool)) ([]Source, error) {
	entries, err := os.ReadDir(dir)
	if err != nil {
		if os.IsNotExist(err) {
			return nil, nil
		}
		return nil, fmt.Errorf("enumerate the logs at %s: %w", dir, err)
	}
	var sources []Source
	for _, entry := range entries {
		if entry.IsDir() {
			continue
		}
		src, ok := classify(entry.Name())
		if !ok {
			continue
		}
		src.Path = filepath.Join(dir, entry.Name())
		sources = append(sources, src)
	}
	return sources, nil
}
