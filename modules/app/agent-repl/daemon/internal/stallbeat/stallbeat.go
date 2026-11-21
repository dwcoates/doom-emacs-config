// Package stallbeat raises an error when the web content process's
// main-thread stalls fall on one phase of a 30-second cycle.
//
// WHY IT EXISTS (2026-10-18). WebKit's MemoryPressureHandler polls the web
// content process's footprint every 30 seconds and, while it stands above
// WebKit's threshold, runs a memory-pressure relief, with a full garbage
// collection, on the main thread. Every workspace page shares that thread, so
// each relief is a stall the user feels, and a stall on every beat is how
// leaked documents showed: on 2026-10-15, 52 of an hour's `webhost.stall`
// records fell on the same second of the cycle. A stall pattern locked to the
// beat is therefore the sign that the process is under sustained memory
// pressure, whatever the cause, and it is raised so a regression surfaces in
// the logs instead of only in a slow switch.
//
// THE THRESHOLD IS MEASURED. Stalls with no beat behind them put at most 4 of a
// 10-minute window's stalls on any one phase (2026-10-18). Sustained relief
// put 14 to 20 there, nearly every one of the window's 20 beats. Beat sets the
// line at 8, twice the chance clustering.
//
// THE LINE RISES WITH THE STALL COUNT. Those 4 were of about 20 stalls. A
// window of 67 or 92 stalls with no beat behind them puts 7 to 10 on its
// busiest phase by chance alone, so a fixed 8 raised on busy windows that had
// no beat (2026-11-16 17:12 and 17:23: 8 of 67 and 8 of 92, at chance odds of
// 1 in 5 and better than even), and a window of 364 put 19 on its busiest
// phase (16:11). The line is therefore the larger of Beat and
// the count chance reaches on some phase with odds below FalseAlarm
// (chanceBound).
//
// EACH WEB VIEW IS ITS OWN PROCESS. The webhost's two documents, the host that
// holds every workspace page and the sidebar, run in separate web content
// processes, each with its own memory-pressure poll on its own phase: of
// 9,857 sidebar stalls only 259 fell within 300ms of a host stall
// (2026-11-16). One document's stalls are never counted against another's
// beat, so the detector keeps a window per document.
//
// THE ERROR NAMES WHAT THE PROCESS HOLDS (2026-11-14). Nothing recorded which
// page kept a process under pressure. The error now names every document of
// the beating process whose census stands (the host view's process holds the
// host and every workspace page; the sidebar's holds the sidebar alone), each
// one's elements, feed rows and animations, largest first, and the web
// content processes' latest footprint, so the next occurrence names its
// holder.
package stallbeat

import (
	"errors"
	"fmt"
	"math"
	"sort"
	"sync"
	"time"

	"agent-repld/internal/dlog"
	"agent-repld/internal/webfootprint"
)

const op = "daemon.webprocess.stall_beat"

// Period is the cycle WebKit's memory-pressure poll runs on.
const Period = 30 * time.Second

// Window is how far back stalls are counted, and how often the error repeats
// at most while the pattern holds.
const Window = 10 * time.Minute

// Tolerance is how far from the beat's phase a stall still counts as on it.
const Tolerance = 500 * time.Millisecond

// Beat is the fewest stalls of one window on one phase that raise the error,
// however few stalls the window holds.
const Beat = 8

// FalseAlarm is the odds, per window, that stalls with no beat behind them
// reach the line on some phase (chanceBound).
const FalseAlarm = 1e-3

// phases is how many distinct phases a stall's arc can stand on: arcs of
// 2*Tolerance whose centers lie Tolerance apart cover every placement.
const phases = int(Period / Tolerance)

// CensusFresh is how long a document's census stands. Every page and webhost
// document states its size each minute (webapp CENSUS_EVERY_MS), so one
// silent for two has been unloaded.
const CensusFresh = 2 * time.Minute

// HostDocument is the webhost view whose web content process holds every
// workspace page; the other view (the sidebar) has a process of its own.
const HostDocument = "host"

// Document is one web document in a web content process.
type Document struct {
	// Kind is KindPage, a workspace's page named by its workspace ID, or
	// KindWebhost, one of Emacs's two web views named "host" or "sidebar".
	Kind, Name string
}

// The kinds of Document.
const (
	KindPage    = "page"
	KindWebhost = "webhost"
)

// Census is a document's own statement of its size (webapp page.census and
// webhost.census). No per-page byte figure exists; a document's elements and
// feed rows are what each can measure of itself.
type Census struct {
	DomNodes, FeedRows int64
	// Animations is a page's live animations; nil for a webhost document,
	// which does not count them.
	Animations *int64
	// Visibility is a page's visibility state; empty for a webhost document.
	Visibility string
}

// stated is a census and the moment its document stated it.
type stated struct {
	at time.Time
	Census
}

// Footprint answers the web content processes' latest measured footprint
// (webfootprint.Sampler).
type Footprint interface {
	Latest() (webfootprint.Reading, bool)
}

// Deps are the detector's collaborators, every one required.
type Deps struct {
	Log dlog.Logger
	// Footprint is the web content processes' footprint, named in the error.
	Footprint Footprint
}

// stall is one recorded main-thread stall.
type stall struct {
	at      time.Time
	blocked int64
}

// clock is one web view's window of stalls and when its error was last raised.
type clock struct {
	stalls []stall
	raised time.Time
}

// Detector watches the stalls the webhost reports, one window per document.
type Detector struct {
	log dlog.Logger

	footprint Footprint

	mu     sync.Mutex
	clocks map[string]*clock
	// censuses is every document's latest census.
	censuses map[Document]stated
}

// New builds a detector.
func New(deps Deps) (*Detector, error) {
	if deps.Log == nil || deps.Footprint == nil {
		return nil, errors.New("stallbeat: a logger and the web footprint are required")
	}
	return &Detector{log: deps.Log, footprint: deps.Footprint, clocks: map[string]*clock{}, censuses: map[Document]stated{}}, nil
}

// Observe takes one stall: the web view DOCUMENT that recorded it, the moment
// it did and how long that document's main thread was blocked. It raises the
// error once per Window per document while the document's stalls on one phase
// of Period, within Tolerance, reach the larger of Beat and chanceBound.
func (d *Detector) Observe(document string, at time.Time, blockedMS int64) {
	d.mu.Lock()
	defer d.mu.Unlock()
	c, ok := d.clocks[document]
	if !ok {
		c = &clock{}
		d.clocks[document] = c
	}
	c.stalls = append(within(c.stalls, at), stall{at: at, blocked: blockedMS})
	if !c.raised.IsZero() && at.Sub(c.raised) < Window {
		return
	}
	phase, onBeat, blocked := bestBeat(c.stalls)
	byChance := chanceBound(len(c.stalls))
	if onBeat < max(Beat, byChance) {
		return
	}
	c.raised = at
	documents := d.documentsAt(document, at)
	ctx := dlog.Context{
		"document":           document,
		"stalls_on_beat":     onBeat,
		"stalls_in_window":   len(c.stalls),
		"stalls_by_chance":   byChance - 1,
		"blocked_ms_on_beat": blocked,
		"phase_ms":           phase.Milliseconds(),
		"window_s":           int(Window.Seconds()),
		"period_s":           int(Period.Seconds()),
		"documents":          documents,
	}
	if reading, ok := d.footprint.Latest(); ok {
		ctx["largest_web_content_footprint_mb"] = reading.LargestBytes >> 20
		ctx["web_content_processes"] = reading.Processes
		ctx["footprint_age_s"] = int64(at.Sub(reading.At).Seconds())
	} else {
		ctx["footprint_sampled"] = false
	}
	largest := "no document has stated its size"
	if len(documents) > 0 {
		largest = fmt.Sprintf("the largest document is %s %s at %d elements",
			documents[0]["kind"], documents[0]["name"], documents[0]["dom_nodes"])
	}
	d.log.Error(op, fmt.Sprintf(
		"%d of the last %s's %d %s web-process stalls fell on one phase of WebKit's %s memory-pressure cycle, where chance puts at most %d: the process is under sustained memory pressure; %d documents are loaded in it, %s",
		onBeat, Window, len(c.stalls), document, Period, byChance-1, len(documents), largest), ctx)
}

// Census records DOC's latest statement of its size, stated AT.
func (d *Detector) Census(doc Document, at time.Time, c Census) {
	d.mu.Lock()
	defer d.mu.Unlock()
	d.censuses[doc] = stated{at: at, Census: c}
}

// inProcess reports whether DOC lives in the web content process of the
// webhost view PROCESS: the host view's process holds every workspace page.
func inProcess(doc Document, process string) bool {
	if doc.Kind == KindPage {
		return process == HostDocument
	}
	return doc.Name == process
}

// documentsAt answers every document of PROCESS's web content process whose
// census stands at AT, most elements first, and forgets every census, of any
// process, that no longer stands. Caller holds mu.
func (d *Detector) documentsAt(process string, at time.Time) []map[string]any {
	type entry struct {
		doc Document
		stated
	}
	var standing []entry
	for doc, c := range d.censuses {
		if at.Sub(c.at) >= CensusFresh {
			delete(d.censuses, doc)
			continue
		}
		if inProcess(doc, process) {
			standing = append(standing, entry{doc, c})
		}
	}
	sort.Slice(standing, func(i, j int) bool {
		a, b := standing[i], standing[j]
		if a.DomNodes != b.DomNodes {
			return a.DomNodes > b.DomNodes
		}
		if a.doc.Kind != b.doc.Kind {
			return a.doc.Kind < b.doc.Kind
		}
		return a.doc.Name < b.doc.Name
	})
	out := make([]map[string]any, 0, len(standing))
	for _, e := range standing {
		m := map[string]any{
			"kind": e.doc.Kind, "name": e.doc.Name,
			"dom_nodes": e.DomNodes, "feed_rows": e.FeedRows,
			"census_age_s": int64(at.Sub(e.at).Seconds()),
		}
		if e.Animations != nil {
			m["animations"] = *e.Animations
		}
		if e.Visibility != "" {
			m["visibility"] = e.Visibility
		}
		out = append(out, m)
	}
	return out
}

// within answers the STALLS still inside Window before AT, reusing their array.
func within(stalls []stall, at time.Time) []stall {
	kept := stalls[:0]
	for _, s := range stalls {
		if at.Sub(s.at) < Window {
			kept = append(kept, s)
		}
	}
	return kept
}

// chanceBound answers the fewest of N stalls with no beat behind them that
// fall on one phase only with odds below FalseAlarm: the least k for which
// phases times the chance that k or more of N uniform stalls land in one arc
// of 2*Tolerance stays within FalseAlarm. Below it, a crowded phase is what a
// busy window looks like by chance.
func chanceBound(n int) int {
	q := float64(2*Tolerance) / float64(Period)
	pmf := make([]float64, n+1)
	pmf[0] = math.Pow(1-q, float64(n))
	for i := 0; i < n; i++ {
		pmf[i+1] = pmf[i] * float64(n-i) / float64(i+1) * q / (1 - q)
	}
	tail := 0.0
	for k := n; k >= 0; k-- {
		tail += pmf[k]
		if float64(phases)*tail > FalseAlarm {
			return k + 1
		}
	}
	return 0
}

// Recent answers how many stalls, of every document, fell within Window
// before NOW and how long they blocked in all, in milliseconds: the strain
// line of the rail's memory popup (memorypopup). A stall older than Window is
// not counted, whether or not a later Observe has pruned it yet.
func (d *Detector) Recent(now time.Time) (int, int64) {
	d.mu.Lock()
	defer d.mu.Unlock()
	count, blocked := 0, int64(0)
	for _, c := range d.clocks {
		for _, s := range c.stalls {
			if now.Sub(s.at) < Window {
				count++
				blocked += s.blocked
			}
		}
	}
	return count, blocked
}

// bestBeat answers the phase of Period that the most STALLS fall within
// Tolerance of, how many do, and how long they blocked in all.
func bestBeat(stalls []stall) (time.Duration, int, int64) {
	var bestPhase time.Duration
	bestCount, bestBlocked := 0, int64(0)
	for _, candidate := range stalls {
		center := phaseOf(candidate.at)
		count, blocked := 0, int64(0)
		for _, s := range stalls {
			if distance(phaseOf(s.at), center) <= Tolerance {
				count++
				blocked += s.blocked
			}
		}
		if count > bestCount {
			bestPhase, bestCount, bestBlocked = center, count, blocked
		}
	}
	return bestPhase, bestCount, bestBlocked
}

// phaseOf answers where in Period the moment AT falls.
func phaseOf(at time.Time) time.Duration {
	return time.Duration(at.UnixNano() % int64(Period))
}

// distance answers how far apart two phases are around the cycle.
func distance(a, b time.Duration) time.Duration {
	d := a - b
	if d < 0 {
		d = -d
	}
	if Period-d < d {
		return Period - d
	}
	return d
}
