//go:build realtest

package realtest

import (
	"bufio"
	"encoding/json"
	"fmt"
	"os"
	"regexp"
	"sort"
	"strings"
	"time"
)

// WHAT REALTEST 1 MEASURES: process spawn to usable. The phases are read off
// the module log's own records, which carry RFC3339 timestamps to the
// microsecond, rather than off anything this package times itself — with one
// exception, the spawn, which only the launcher knows because it happens before
// Emacs exists.
//
// Reading the phases off the LOG rather than out of Emacs by polling is the
// whole point. A poll says "by the time I asked, it had happened"; the record
// says when. And every marker below is a line the module already writes in
// ordinary operation, so measuring the startup does not change it.
//
// The marker vocabulary is documented in AGENTS.md "Logs"; the phases here name
// which of those markers bound each phase.

// PhaseName is one measured interval's name, used in reports and in budget
// lookups. Spelled as a type so a budget table cannot be keyed by a typo.
type PhaseName string

const (
	// PhaseDoomBoot is spawn to the first module record: Doom finished
	// booting far enough to load this module. The module's first record is
	// the earliest evidence the process reached lisp at all, and on a GUI
	// launch it lands after the first frame maps.
	PhaseDoomBoot PhaseName = "doom-boot"
	// PhaseModuleLoaded is spawn to the module's own startup completing —
	// the daemon ensure being COMMANDED, which config.el schedules once the
	// module is loaded.
	PhaseModuleLoaded PhaseName = "module-loaded"
	// PhaseDaemonAnswered is the ensure being commanded to the daemon
	// answering: adopted (an already-answering daemon) or booted (one this
	// launch spawned). Which of the two happened is reported, because the
	// two paths are different work and comparing their times would be
	// comparing different things.
	PhaseDaemonAnswered PhaseName = "daemon-answered"
	// PhaseLinkUp is spawn to `elisp.link.up`: the frontend holds a live
	// link to the daemon.
	PhaseLinkUp PhaseName = "link-up"
	// PhaseFirstRoster is spawn to the first roster reconcile: the daemon
	// pushed the workspace set and Emacs applied it.
	PhaseFirstRoster PhaseName = "first-roster"
	// PhaseTabDrawn is spawn to a WORKSPACE'S tab appearing
	// (`elisp.roster.tab-open`). Measured per workspace.
	PhaseTabDrawn PhaseName = "tab-drawn"
	// PhasePanelPainted is spawn to a WORKSPACE'S webview reporting its load
	// finished (`elisp.frontend.watch-load: load-changed`). Measured per
	// workspace. This is the only signal in the whole startup that comes
	// from the PAGE, and it is a fact the widget emits rather than an answer
	// to a question — which is what makes it trustworthy for a page too
	// broken to answer one.
	PhasePanelPainted PhaseName = "panel-painted"
	// PhaseTotal is spawn to the last workspace's panel painted: usable.
	PhaseTotal PhaseName = "total"
)

// marker is one log line the phase reader recognizes.
//
// Matching is on the record's `message`, not on its `operation`: the operation
// name is derived from the format string and folds the arguments away, so two
// materially different events can share one operation, and `message` is the
// line an operator would grep for anyway.
type marker struct {
	name PhaseName
	re   *regexp.Regexp
	// perWorkspace says the marker is expected once per workspace rather
	// than once per run, so the reader keeps every occurrence keyed by the
	// record's workspace instead of the first one.
	perWorkspace bool
}

var markers = []marker{
	{name: PhaseModuleLoaded, re: regexp.MustCompile(`^elisp\.daemon\.ensure-command`)},
	{name: PhaseDaemonAnswered, re: regexp.MustCompile(`^elisp\.daemon\.(adopted|booted)\b`)},
	{name: PhaseLinkUp, re: regexp.MustCompile(`^elisp\.link\.up\b`)},
	{name: PhaseFirstRoster, re: regexp.MustCompile(`^elisp\.roster\.reconcile:`)},
	{name: PhaseTabDrawn, re: regexp.MustCompile(`^elisp\.roster\.tab-open:`), perWorkspace: true},
	{name: PhasePanelPainted, re: regexp.MustCompile(`^elisp\.frontend\.watch-load: load-changed`), perWorkspace: true},
}

// tabOpenRe pulls the workspace out of `elisp.roster.tab-open: ws=NAME id=ID
// dir=DIR`. The tab-open record is written with the workspace as its log
// subject, so it also carries `workspace_id` — but it is the ONE marker that
// can be written before the workspace's sink exists, so the message's own
// `id=` is read as a fallback rather than trusted to be in the fields.
var tabOpenRe = regexp.MustCompile(`\bid=([^\s]+)`)

// Observation is one marker seen in the log.
type Observation struct {
	Phase     PhaseName
	Workspace string // GlobalWorkspace for a once-per-run marker
	At        time.Time
	Raw       string
}

// Phases is what a cold start's log says happened, and when.
type Phases struct {
	// SpawnedAt is when the launcher started the Emacs process. The only
	// timestamp in the set this package produces itself, because it precedes
	// the process that would otherwise report it.
	SpawnedAt time.Time
	// FirstRecord is the earliest module record of the run: PhaseDoomBoot's
	// end.
	FirstRecord time.Time
	// Observations is every recognized marker, in log order.
	Observations []Observation
	// DaemonPath is "adopted" or "booted", read off whichever
	// PhaseDaemonAnswered marker fired. Empty when neither did.
	DaemonPath string
}

// ReadPhases reads a cold start's phases out of the module log.
//
// It reads from `offset` — the snapshot byte offset — and keeps only records at
// or after `spawnedAt`, which together bound the run exactly: the offset
// excludes everything a previous Emacs wrote, and the timestamp excludes a
// straggler the outgoing process wrote after the offset was taken.
func ReadPhases(moduleLog string, offset int64, spawnedAt time.Time) (Phases, error) {
	phases := Phases{SpawnedAt: spawnedAt}

	file, err := os.Open(moduleLog)
	if err != nil {
		return phases, fmt.Errorf("open the module log %s to read the startup phases: %w", moduleLog, err)
	}
	defer file.Close()
	if offset > 0 {
		if _, err := file.Seek(offset, 0); err != nil {
			return phases, fmt.Errorf("seek the module log to the snapshot offset %d: %w", offset, err)
		}
	}

	scanner := bufio.NewScanner(file)
	scanner.Buffer(make([]byte, 0, 1<<20), 1<<24)
	for scanner.Scan() {
		text := scanner.Text()
		if strings.TrimSpace(text) == "" {
			continue
		}
		var rec record
		if err := json.Unmarshal([]byte(text), &rec); err != nil {
			// The harvester reports this line as a malformed record. The phase
			// reader's job is the timeline, and a line it cannot parse simply
			// carries no marker.
			continue
		}
		at, err := time.Parse(time.RFC3339Nano, rec.Timestamp)
		if err != nil || at.Before(spawnedAt) {
			continue
		}
		if phases.FirstRecord.IsZero() {
			phases.FirstRecord = at
		}
		for _, m := range markers {
			if !m.re.MatchString(rec.Message) {
				continue
			}
			ws := GlobalWorkspace
			if m.perWorkspace {
				ws = rec.WorkspaceID
				if ws == "" {
					if got := tabOpenRe.FindStringSubmatch(rec.Message); got != nil {
						ws = got[1]
					}
				}
				if ws == "" {
					ws = GlobalWorkspace
				}
			}
			if m.name == PhaseDaemonAnswered && phases.DaemonPath == "" {
				switch {
				case strings.Contains(rec.Message, "adopted"):
					phases.DaemonPath = "adopted"
				case strings.Contains(rec.Message, "booted"):
					phases.DaemonPath = "booted"
				}
			}
			phases.Observations = append(phases.Observations, Observation{
				Phase:     m.name,
				Workspace: ws,
				At:        at,
				Raw:       rec.Message,
			})
			break
		}
	}
	if err := scanner.Err(); err != nil {
		return phases, fmt.Errorf("read the module log %s: %w", moduleLog, err)
	}
	return phases, nil
}

// Measurement is one phase's elapsed time from spawn.
//
// EVERY phase is measured FROM SPAWN, not from the phase before it. A
// per-phase delta would hide the one thing a startup measurement is for: the
// user is waiting from the moment they launched the editor, and a phase that
// is fast in isolation but starts late is exactly as slow to them.
type Measurement struct {
	Phase     PhaseName
	Workspace string
	Elapsed   time.Duration
	// Note is set when a phase was not observed at all.
	Note string
}

// Measure turns observed markers into elapsed times from spawn.
//
// A once-per-run phase takes its FIRST observation: the marker firing again
// later is a reconnect or a re-reconcile, not the startup. A per-workspace
// phase takes the first per workspace, for the same reason.
func (p Phases) Measure() []Measurement {
	var out []Measurement

	if !p.FirstRecord.IsZero() {
		out = append(out, Measurement{
			Phase:     PhaseDoomBoot,
			Workspace: GlobalWorkspace,
			Elapsed:   p.FirstRecord.Sub(p.SpawnedAt),
		})
	} else {
		out = append(out, Measurement{
			Phase:     PhaseDoomBoot,
			Workspace: GlobalWorkspace,
			Note:      "the module wrote no record at all after the spawn: either Doom never reached this module, or the module log is not where AGENTS.md says it is",
		})
	}

	seen := make(map[string]time.Time)
	for _, obs := range p.Observations {
		key := string(obs.Phase) + "\x00" + obs.Workspace
		if prior, ok := seen[key]; ok && !obs.At.Before(prior) {
			continue
		}
		seen[key] = obs.At
	}

	latest := p.FirstRecord
	for key, at := range seen {
		parts := strings.SplitN(key, "\x00", 2)
		out = append(out, Measurement{
			Phase:     PhaseName(parts[0]),
			Workspace: parts[1],
			Elapsed:   at.Sub(p.SpawnedAt),
		})
		if at.After(latest) {
			latest = at
		}
	}

	if !latest.IsZero() {
		out = append(out, Measurement{
			Phase:     PhaseTotal,
			Workspace: GlobalWorkspace,
			Elapsed:   latest.Sub(p.SpawnedAt),
		})
	}

	sort.SliceStable(out, func(i, j int) bool {
		if out[i].Elapsed != out[j].Elapsed {
			return out[i].Elapsed < out[j].Elapsed
		}
		if out[i].Phase != out[j].Phase {
			return out[i].Phase < out[j].Phase
		}
		return out[i].Workspace < out[j].Workspace
	})
	return out
}

// DrawnWorkspaces is every workspace whose tab was observed, and PaintedWorkspaces
// every workspace whose panel was. Realtest 1 asserts both against what the
// state database holds.
func (p Phases) DrawnWorkspaces() []string   { return p.workspacesFor(PhaseTabDrawn) }
func (p Phases) PaintedWorkspaces() []string { return p.workspacesFor(PhasePanelPainted) }

func (p Phases) workspacesFor(phase PhaseName) []string {
	set := make(map[string]bool)
	for _, obs := range p.Observations {
		if obs.Phase == phase && obs.Workspace != GlobalWorkspace {
			set[obs.Workspace] = true
		}
	}
	out := make([]string, 0, len(set))
	for ws := range set {
		out = append(out, ws)
	}
	sort.Strings(out)
	return out
}
