//go:build realtest

package realtest

import (
	"bufio"
	"encoding/json"
	"fmt"
	"os"
	"regexp"
	"sort"
	"strconv"
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
	//
	// ITS MARKER IS `elisp.daemon.ensure-scheduled`, NOT
	// `elisp.daemon.ensure-command`. The reader matched the latter for the
	// whole life of this file and consequently never once observed this
	// phase: across the 23 MANIFEST.md runs under ~/.claude-emacs/realtest/
	// there is not a single `ensure-command` record, because that record is
	// written by `agent-repl-frontend-daemon-ensure` — the INTERACTIVE
	// command, the retry a person invokes after a build failure — and a
	// startup never invokes it. What config.el actually registers on
	// `emacs-startup-hook` is `agent-repl-daemon-schedule-ensure`, whose
	// record is `elisp.daemon.ensure-scheduled idle=N` (46 of them in the
	// same history). That is the commanding this phase names, so that is
	// what is matched.
	//
	// `ensure-command` is deliberately NOT matched as well. It fires only on
	// a manual retry, which is not a startup edge, and crediting it to a
	// startup phase would report a person's keypress as module-load latency.
	PhaseModuleLoaded PhaseName = "module-loaded"
	// PhaseDaemonSpawned is spawn to the daemon process existing:
	// `elisp.daemon.started` for one this launch spawned, or
	// `elisp.daemon.adopted` for one that was already answering. Which of
	// the two happened is reported, because the two paths are different work
	// and comparing their times would be comparing different things.
	//
	// IT IS NOT THE DAEMON BEING USABLE. Realtest 1 run 1 recorded
	// `elisp.daemon.booted` three milliseconds after the spawn, against an
	// address file a dead daemon had left behind, while the link the frontend
	// actually talks over came up ten seconds later. A phase that ends at the
	// boot claim measures the claim.
	PhaseDaemonSpawned PhaseName = "daemon-spawned"
	// PhaseDaemonAnswered is spawn to the daemon ANSWERING THIS FRONTEND:
	// the primary link is up AND the roster subscription was accepted. It
	// ends at the LATER of the two, because either one alone leaves the
	// frontend unable to draw anything — a link with no roster has nothing to
	// draw, and a subscription with no link cannot be delivered.
	PhaseDaemonAnswered PhaseName = "daemon-answered"
	// PhaseLinkUp is spawn to the primary link being up: `elisp.link.up`, or
	// `elisp.link.reconnected` / `elisp.host.link-up` for a link that came up
	// on a retry, which is what a cold start against a stale address file
	// actually produces.
	PhaseLinkUp PhaseName = "link-up"
	// PhaseRosterSubscribed is spawn to `elisp.roster.subscribed`: the daemon
	// ACCEPTED the roster subscription. It is written from the acceptance
	// rather than from the request, which is what makes it evidence.
	PhaseRosterSubscribed PhaseName = "roster-subscribed"
	// PhaseFirstRoster is spawn to the first roster reconcile: the daemon
	// pushed the workspace set and Emacs applied it. Its marker is
	// `elisp.roster.reconcile: tabs=N order=(...)`, written by
	// `agent-repl-roster-reconcile` once the tab walk has settled the order.
	//
	// That record existed but was invisible until 2026-09-12: roster.el wrote
	// it through `agent-repl--log`, which is the DEBUG rung, and the durable
	// sink's default level is `info`, so it never reached any file. Across
	// the same 23-run history there are 108 `elisp.roster.tab-open` records
	// (INFO, written by the very same walk) and zero `elisp.roster.reconcile`
	// ones. Per AGENTS.md, "An invisible action is a logging defect": the
	// record now goes out at INFO like its siblings in that file, so the
	// phase is observable.
	PhaseFirstRoster PhaseName = "first-roster"
	// PhaseTabDrawn is spawn to a WORKSPACE'S tab appearing
	// (`elisp.roster.tab-open`). Measured per workspace.
	PhaseTabDrawn PhaseName = "tab-drawn"
	// PhaseWebviewArmed is spawn to the pre-creation queue reporting every
	// open workspace queued, or parked awaiting focus:
	// `elisp.webview-recovery.precreate-all: queued=N` and
	// `elisp.webview-recovery.precreate-parked queued=N`. Both fire on the
	// module's central sink with no per-workspace attribution at all — the
	// queue reports a COUNT, not names — so this is judged run-wide, by the
	// largest `queued=` seen reaching the number of open workspaces, not by
	// naming each one the way PhaseTabDrawn does.
	//
	// THIS IS THE HIDDEN-STARTUP GATE, REPLACING PANEL-PAINTED THERE (owner
	// ruling 2026-09-11). `open -gj` leaves the frame visible-but-unfocused
	// on this machine, and the settled webview invariant PARKS pre-creation
	// in that state rather than steal focus — so a workspace's panel does
	// not paint until the first focus edge, no matter how long the hidden
	// window runs. Requiring a painted panel while hidden was asserting a
	// bug that was never there; what "usable while hidden" means for a
	// panel is that it is QUEUED to paint the moment it is shown.
	PhaseWebviewArmed PhaseName = "webview-armed"
	// PhaseFocusEdge is spawn to the harness bringing Emacs forward for the
	// first time: `elisp.webview-recovery.precreate-drained-on-focus`, the
	// parked pre-creation queue releasing on that focus change. This is a
	// REAL observed edge — Emacs really was brought forward at this instant —
	// but it is also where the harness's own deliberate hidden-window wait
	// ends, so it is never folded into PhaseTotal (owner ruling 2026-09-11):
	// a realtest measurement is never allowed to bake in the harness's own
	// arbitrary delay before it chooses to show Emacs. It exists so
	// PhasePanelPainted can be reported as the delta FROM this edge, which is
	// the panel's own intrinsic paint cost, rather than from spawn, which
	// would be paint cost plus however long the harness felt like waiting.
	PhaseFocusEdge PhaseName = "focus-edge"
	// PhasePanelPainted is a WORKSPACE'S webview reporting its load finished,
	// which happens on FIRST SHOW, not while hidden (see PhaseWebviewArmed):
	// either `elisp.frontend.watch-load: load-changed`, the page's own
	// account of its load, or `elisp.webview-recovery.precreate-created
	// ws=NAME reason=focused`, the parked drain resuming on the focus edge
	// and mounting directly. Measured per workspace.
	// `load-changed` is attributed to the workspace normally (its JSON
	// record carries `workspace_id`); `precreate-created` is written on the
	// central sink like PhaseWebviewArmed's markers, so it carries no
	// `workspace_id` either — its `ws=` names the workspace by its
	// registered NAME, not its daemon id, and the reader falls back to that
	// name when no id is present (readPhaseRecords, wsEqualsRe).
	//
	// UNLIKE EVERY OTHER PHASE, THIS ONE IS NOT MEASURED FROM SPAWN (owner
	// ruling 2026-09-11). It is measured from PhaseFocusEdge: the panel
	// cannot start loading before the harness decides to show Emacs, so a
	// spawn-based elapsed time would report the harness's own wait as if it
	// were product latency. What is reported here is the panel's INTRINSIC
	// paint cost — focus edge to load — and nothing else. See Measure.
	PhasePanelPainted PhaseName = "panel-painted"
	// PhaseTotal is startup-usable: spawn to the LATEST of tab-drawn (across
	// every workspace), link-up, roster-subscribed, first-roster and
	// webview-armed. This is the number the owner actually experiences
	// waiting for the editor to be usable while it launches hidden.
	//
	// PhasePanelPainted is DELIBERATELY EXCLUDED from this set (owner ruling
	// 2026-09-11). A panel does not paint until the harness brings Emacs
	// forward, which happens only for the key self-test at the end of the
	// run — an artifact of how this harness protects the owner's focus, not
	// something the owner ever waits on. A PhaseTotal that included it would
	// report "spawn to shown-and-painted", which bakes in the harness's own
	// arbitrary wait before it reveals Emacs. That is never reported as
	// latency; PhasePanelPainted's own intrinsic cost is reported instead,
	// separately, from PhaseFocusEdge.
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
	// valueRe, when set, pulls a trailing count out of the message (the
	// pre-creation queue's `queued=N`) into the Observation's Value. Only
	// PhaseWebviewArmed uses it; every other marker leaves it nil and gets
	// Value 0, which is never read for them.
	valueRe *regexp.Regexp
}

// The boundaries are spelled with an explicit `(\s|$)` rather than `\b`,
// because `-` is not a word character: `link-up\b` also matches
// `link-up-skipped`, and `adopted\b` also matches `adopted-unhealthy`. Both of
// those are the OPPOSITE of the phase they would be credited to.
var markers = []marker{
	{name: PhaseModuleLoaded, re: regexp.MustCompile(`^elisp\.daemon\.ensure-scheduled(\s|$)`)},
	{name: PhaseDaemonSpawned, re: regexp.MustCompile(`^elisp\.daemon\.(started|adopted)(\s|$)`)},
	{name: PhaseLinkUp, re: regexp.MustCompile(`^elisp\.(link\.(up|reconnected)|host\.link-up)(\s|$)`)},
	{name: PhaseRosterSubscribed, re: regexp.MustCompile(`^elisp\.roster\.subscribed(\s|$)`)},
	{name: PhaseFirstRoster, re: regexp.MustCompile(`^elisp\.roster\.reconcile:`)},
	{name: PhaseTabDrawn, re: regexp.MustCompile(`^elisp\.roster\.tab-open:`), perWorkspace: true},
	{name: PhaseWebviewArmed, re: regexp.MustCompile(`^elisp\.webview-recovery\.precreate-(all|parked)(:|\s)`), valueRe: queuedRe},
	{name: PhaseFocusEdge, re: regexp.MustCompile(`^elisp\.webview-recovery\.precreate-drained-on-focus(\s|$)`)},
	{name: PhasePanelPainted, re: regexp.MustCompile(`^elisp\.frontend\.watch-load: load-changed|^elisp\.webview-recovery\.precreate-created ws=\S+ reason=focused`), perWorkspace: true},
}

// tabOpenRe pulls the workspace out of `elisp.roster.tab-open: ws=NAME id=ID
// dir=DIR`. The tab-open record is written with the workspace as its log
// subject, so it also carries `workspace_id` — but it is the ONE marker that
// can be written before the workspace's sink exists, so the message's own
// `id=` is read as a fallback rather than trusted to be in the fields.
var tabOpenRe = regexp.MustCompile(`\bid=([^\s]+)`)

// wsEqualsRe pulls the workspace out of `... ws=NAME ...` for the ONE other
// marker that can carry no `workspace_id`: `precreate-created`, written on
// the module's central sink (lisp/webview-recovery.el) rather than on the
// workspace's own, because the drain that emits it runs before any one
// workspace is "current". Its `ws=` is the workspace's registered NAME, not
// its daemon id — see the PaintedWorkspace matching in the test file, which
// checks a workspace's Name as well as its ID for exactly this marker.
var wsEqualsRe = regexp.MustCompile(`\bws=([^\s]+)`)

// queuedRe pulls the count out of `precreate-all: queued=N` and
// `precreate-parked queued=N`.
var queuedRe = regexp.MustCompile(`\bqueued=(\d+)`)

// Observation is one marker seen in the log.
type Observation struct {
	Phase     PhaseName
	Workspace string // GlobalWorkspace for a once-per-run marker
	At        time.Time
	Raw       string
	// Value is the marker's `valueRe` capture, or 0 when the marker has none.
	Value int
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
	// DaemonPath is "adopted" or "spawned", read off whichever
	// PhaseDaemonSpawned marker fired. Empty when neither did.
	DaemonPath string
}

// ReadPhases reads a cold start's phases out of the Emacs log sinks.
//
// THE GLOBAL MODULE LOG IS NOT ENOUGH. The workspace-agnostic markers
// (module-loaded, daemon-spawned, link-up, roster-subscribed, first-roster,
// webview-armed) land in `agent-repl-log-file-name`, but `tab-open` and one
// of panel-painted's two markers — `watch-load: load-changed` — are
// workspace-owned records, and lisp/core.el's `agent-repl--do-log-to-file`
// routes those to the workspace's own canonical `.claude/emacs/emacs.log` sink,
// never to the global log. (Panel-painted's other marker, `precreate-created`,
// is written on the central sink like webview-armed's markers, and so lands
// in the global log even though it names a workspace.) A reader that opened
// only the global log would find every startup timing except tab-open, which
// is exactly how run 3 reported a startup that had drawn every tab as "no tab
// drawn".
//
// So it reads every Emacs sink: the global module log and each workspace's
// `emacs.log`. Each is resolved through `resolveReads`, which takes its read
// offset from the resolved target's inode rather than from the symlink path, so
// a workspace sink whose target changed between the run-start snapshot and this
// read (a new instance appended to the standing target, minted a fresh one, or
// a cap rotation replaced it) is still read from the right place instead of
// from past the end of a file this run never wrote (row 28 of the judgement
// ledger).
//
// Only records at or after `spawnedAt` are kept, which together with the
// snapshot offset bounds the run exactly: the offset excludes everything a
// previous Emacs wrote to the same bytes, and the timestamp excludes a
// straggler the outgoing process wrote after the offset was taken.
func ReadPhases(sources []Source, snap Snapshot, spawnedAt time.Time) (Phases, error) {
	phases := Phases{SpawnedAt: spawnedAt}
	for _, src := range sources {
		if !isEmacsPhaseSource(src) {
			continue
		}
		reads, _, err := resolveReads(src, snap)
		if err != nil {
			return phases, err
		}
		for _, r := range reads {
			if err := readPhaseRecords(&phases, r.path, r.offset, spawnedAt); err != nil {
				return phases, err
			}
		}
	}
	return phases, nil
}

// isEmacsPhaseSource is true for the two sinks the startup markers land in: the
// module's global sink and each workspace's own `emacs.log`. The other four
// per-workspace sinks and the two service sinks carry no `elisp.*` marker, so
// reading them here would be wasted work on files that reach gigabytes.
func isEmacsPhaseSource(src Source) bool {
	return src.Name == "emacs.global" || src.Name == "workspace.emacs.log"
}

// readPhaseRecords reads ONE file from `offset` to end and folds its markers
// into `phases`.
//
// A file that does not exist is not an error: an early poll runs before the
// module log exists, and a workspace whose sink holds no records yet has no
// file behind its link. `FirstRecord` is the earliest record across every file
// read, which on a cold start is the global log's first line — the earliest
// evidence the process reached lisp at all.
func readPhaseRecords(phases *Phases, path string, offset int64, spawnedAt time.Time) error {
	file, err := os.Open(path)
	if err != nil {
		if os.IsNotExist(err) {
			return nil
		}
		return fmt.Errorf("open the Emacs log %s to read the startup phases: %w", path, err)
	}
	defer file.Close()
	if offset > 0 {
		if _, err := file.Seek(offset, 0); err != nil {
			return fmt.Errorf("seek %s to the snapshot offset %d: %w", path, offset, err)
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
		if phases.FirstRecord.IsZero() || at.Before(phases.FirstRecord) {
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
					} else if got := wsEqualsRe.FindStringSubmatch(rec.Message); got != nil {
						ws = got[1]
					}
				}
				if ws == "" {
					ws = GlobalWorkspace
				}
			}
			if m.name == PhaseDaemonSpawned && phases.DaemonPath == "" {
				switch {
				case strings.HasPrefix(rec.Message, "elisp.daemon.adopted"):
					phases.DaemonPath = "adopted"
				case strings.HasPrefix(rec.Message, "elisp.daemon.started"):
					phases.DaemonPath = "spawned"
				}
			}
			value := 0
			if m.valueRe != nil {
				if got := m.valueRe.FindStringSubmatch(rec.Message); got != nil {
					if n, convErr := strconv.Atoi(got[1]); convErr == nil {
						value = n
					}
				}
			}
			phases.Observations = append(phases.Observations, Observation{
				Phase:     m.name,
				Workspace: ws,
				At:        at,
				Raw:       rec.Message,
				Value:     value,
			})
			break
		}
	}
	if err := scanner.Err(); err != nil {
		return fmt.Errorf("read the Emacs log %s: %w", path, err)
	}
	return nil
}

// Measurement is one phase's elapsed time.
//
// EVERY phase except PhasePanelPainted is measured FROM SPAWN, not from the
// phase before it. A per-phase delta would hide the one thing a startup
// measurement is for: the user is waiting from the moment they launched the
// editor, and a phase that is fast in isolation but starts late is exactly as
// slow to them.
//
// PhasePanelPainted is the one exception, and it is measured from
// PhaseFocusEdge instead (owner ruling 2026-09-11): the panel cannot paint
// before the harness decides to show Emacs, so a spawn-based number would
// report the harness's own wait as if it were the panel's cost. See Measure.
type Measurement struct {
	Phase     PhaseName
	Workspace string
	Elapsed   time.Duration
	// Note is set when a phase was not observed at all.
	Note string
}

// usableEdges is the set of phases whose LATEST observation bounds
// PhaseTotal (startup-usable). PhasePanelPainted (and PhaseFocusEdge, the
// harness's own show-edge) are deliberately absent: including either would
// let the harness's arbitrary hidden-window wait leak into a number reported
// as product latency (owner ruling 2026-09-11; PhaseTotal's doc says why).
var usableEdges = map[PhaseName]bool{
	PhaseTabDrawn:         true,
	PhaseLinkUp:           true,
	PhaseRosterSubscribed: true,
	PhaseFirstRoster:      true,
	PhaseWebviewArmed:     true,
}

// Measure turns observed markers into elapsed times.
//
// A once-per-run phase takes its FIRST observation: the marker firing again
// later is a reconnect or a re-reconcile, not the startup. A per-workspace
// phase takes the first per workspace, for the same reason.
//
// Every phase is measured from spawn EXCEPT PhasePanelPainted, which is
// measured from PhaseFocusEdge — its own doc, and usableEdges above, say why.
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

	out = append(out, daemonAnswered(p.SpawnedAt, seen))

	focusEdgeAt, haveFocusEdge := seen[string(PhaseFocusEdge)+"\x00"+GlobalWorkspace]

	var usableLatest time.Time
	haveUsableLatest := false

	for key, at := range seen {
		parts := strings.SplitN(key, "\x00", 2)
		phase := PhaseName(parts[0])
		workspace := parts[1]

		if phase == PhasePanelPainted {
			// The panel's own intrinsic cost: focus edge to load, never
			// spawn to load (owner ruling 2026-09-11 — PhasePanelPainted's
			// doc says why).
			m := Measurement{Phase: phase, Workspace: workspace}
			switch {
			case !haveFocusEdge:
				m.Note = "the focus edge (elisp.webview-recovery.precreate-drained-on-focus) was never observed, " +
					"so the panel-paint cost cannot be computed as an intrinsic delta"
			case at.Before(focusEdgeAt):
				m.Note = "the panel paint was recorded before the focus edge, which should never happen; " +
					"not reporting a negative delta"
			default:
				m.Elapsed = at.Sub(focusEdgeAt)
			}
			out = append(out, m)
			continue
		}

		out = append(out, Measurement{
			Phase:     phase,
			Workspace: workspace,
			Elapsed:   at.Sub(p.SpawnedAt),
		})

		if usableEdges[phase] && (!haveUsableLatest || at.After(usableLatest)) {
			usableLatest = at
			haveUsableLatest = true
		}
	}

	if haveUsableLatest {
		out = append(out, Measurement{
			Phase:     PhaseTotal,
			Workspace: GlobalWorkspace,
			Elapsed:   usableLatest.Sub(p.SpawnedAt),
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

// MaxArmed is the largest `queued=` count seen on any PhaseWebviewArmed
// marker (`precreate-all` or `precreate-parked`) across the whole run.
//
// It is a MAXIMUM, not a first-or-last: `precreate-all` reports how many were
// newly added to the queue by that one call (a cold start's link-up edge
// typically adds 0, since no workspace ref exists yet, and the first roster
// push then adds the rest), while `precreate-parked` reports the queue's
// full remaining length at the moment it was held. Either can be the largest
// depending on timing, and the largest is what proves every open workspace
// was, at some point, accounted for by the pre-creation queue.
func (p Phases) MaxArmed() int {
	max := 0
	for _, obs := range p.Observations {
		if obs.Phase == PhaseWebviewArmed && obs.Value > max {
			max = obs.Value
		}
	}
	return max
}

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

// daemonAnswered is the composite phase: the daemon answering THIS frontend.
//
// It is computed rather than read off a marker because no single record says
// it. The link coming up says the frontend can talk; the roster subscription
// being accepted says the daemon is answering what the frontend asked for. The
// phase ends at the later of the two, and when either is missing it reports
// WHICH, because "the daemon never answered" and "the roster was never
// subscribed" send a reader to different places.
func daemonAnswered(spawnedAt time.Time, seen map[string]time.Time) Measurement {
	measurement := Measurement{Phase: PhaseDaemonAnswered, Workspace: GlobalWorkspace}
	linkUp, haveLink := seen[string(PhaseLinkUp)+"\x00"+GlobalWorkspace]
	subscribed, haveRoster := seen[string(PhaseRosterSubscribed)+"\x00"+GlobalWorkspace]
	switch {
	case !haveLink && !haveRoster:
		measurement.Note = "the primary link never came up and the roster was never subscribed: the daemon never answered this frontend"
		return measurement
	case !haveLink:
		measurement.Note = "the roster subscription was accepted but no link-up record was written: the phase has no end"
		return measurement
	case !haveRoster:
		measurement.Note = "the primary link came up but the roster subscription was never accepted: the frontend has a link and nothing to draw"
		return measurement
	}
	at := linkUp
	if subscribed.After(at) {
		at = subscribed
	}
	measurement.Elapsed = at.Sub(spawnedAt)
	return measurement
}
