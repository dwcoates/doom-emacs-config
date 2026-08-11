// Package bounceledger accounts for what a daemon bounce did to each session's
// shim, BY PID IDENTITY.
//
// WHY COUNTING IS NOT ACCOUNTING. A bounce was scored by asking how many shim
// processes existed before and after. Six before, seven after: healthy. But
// every pid had changed — the preserved shims had all died and been replaced —
// and the count said nothing, because a replacement is indistinguishable from a
// survivor when you only count. That is how a total fleet loss (2026-08-10
// 19:41, five sessions, every in-flight async task destroyed) passed for a
// clean restart while the outgoing daemon logged "every session shim is
// PRESERVED".
//
// So the outgoing daemon writes down WHICH pid it is leaving behind for each
// session and what it meant to happen to it, and the incoming daemon reads that
// against the kernel's answer for who holds each workspace lock today. The
// three verdicts are not interchangeable:
//
//   - PRESERVED — the same process is still there. The promise was kept.
//   - ROLLED — the shim was ended ON PURPOSE, and the reason travels with it.
//     A roll is a legitimate outcome (a superseded bundle) and it is NOT
//     preservation; saying so is the point.
//   - DIED — the ledger promised preservation and the process is gone. Nobody
//     decided this. It is the incident, and it now has a name at boot instead
//     of being invisible until a user notices their work vanished.
//
// UNKNOWN is its own answer and never collapses into DIED: a lock probe that
// failed did not observe an absence, and reporting a death nobody witnessed
// would be a claim the probe did not make.
package bounceledger

import (
	"encoding/json"
	"fmt"
	"os"
	"path/filepath"
	"sort"
	"sync"
)

// Disposition is what the OUTGOING daemon intended for a shim.
type Disposition string

const (
	// DispositionPreserved — the daemon left the shim running on purpose and
	// expects the next daemon to reattach to the very same process.
	DispositionPreserved Disposition = "preserved"
	// DispositionRolled — the daemon deliberately ended the shim, for the
	// reason recorded beside it.
	DispositionRolled Disposition = "rolled"
)

// Entry is one session's line in the ledger.
type Entry struct {
	SessionID   string      `json:"session_id"`
	Workspace   string      `json:"workspace"`
	PID         int         `json:"pid"`
	Disposition Disposition `json:"disposition"`
	// Reason is required for a roll and empty for a preservation: a deliberate
	// kill that cannot say why is exactly the account this package exists to
	// stop accepting.
	Reason string `json:"reason,omitempty"`
}

// The verdicts an incoming daemon reaches about one entry.
const (
	VerdictPreserved = "PRESERVED"
	VerdictRolled    = "ROLLED"
	VerdictDied      = "DIED"
	VerdictUnknown   = "UNKNOWN"
)

// Validate rejects an entry that could not be judged later.
func (e Entry) Validate() error {
	if e.SessionID == "" {
		return fmt.Errorf("bounceledger: entry has no session id")
	}
	if e.Workspace == "" {
		return fmt.Errorf("bounceledger: entry %s has no workspace", e.SessionID)
	}
	if e.PID <= 0 {
		return fmt.Errorf("bounceledger: entry %s has no pid", e.SessionID)
	}
	switch e.Disposition {
	case DispositionPreserved:
		return nil
	case DispositionRolled:
		if e.Reason == "" {
			return fmt.Errorf("bounceledger: entry %s is a roll with no reason", e.SessionID)
		}
		return nil
	default:
		return fmt.Errorf("bounceledger: entry %s has unknown disposition %q", e.SessionID, e.Disposition)
	}
}

// Write records the ledger atomically: a torn ledger read by the next boot
// would be worse than none, because it would be believed.
func Write(path string, entries []Entry) error {
	if path == "" {
		return fmt.Errorf("bounceledger: empty ledger path")
	}
	for _, entry := range entries {
		if err := entry.Validate(); err != nil {
			return err
		}
	}
	if err := os.MkdirAll(filepath.Dir(path), 0o755); err != nil {
		return fmt.Errorf("bounceledger: creating ledger dir: %w", err)
	}
	payload, err := json.Marshal(entries)
	if err != nil {
		return fmt.Errorf("bounceledger: encoding ledger: %w", err)
	}
	tmp := path + ".tmp"
	if err := os.WriteFile(tmp, append(payload, '\n'), 0o644); err != nil {
		return fmt.Errorf("bounceledger: writing ledger: %w", err)
	}
	if err := os.Rename(tmp, path); err != nil {
		return fmt.Errorf("bounceledger: installing ledger: %w", err)
	}
	return nil
}

// Load reads the ledger. A missing ledger is an empty one and not an error:
// the first boot after an unclean death has nothing to compare against.
func Load(path string) ([]Entry, error) {
	if path == "" {
		return nil, fmt.Errorf("bounceledger: empty ledger path")
	}
	payload, err := os.ReadFile(path)
	if err != nil {
		if os.IsNotExist(err) {
			return nil, nil
		}
		return nil, fmt.Errorf("bounceledger: reading ledger: %w", err)
	}
	var entries []Entry
	if err := json.Unmarshal(payload, &entries); err != nil {
		return nil, fmt.Errorf("bounceledger: decoding ledger: %w", err)
	}
	return entries, nil
}

// Judge rules on one entry against the pids the kernel reports holding that
// workspace's lock right now.
//
// probeErr is the lock probe's own failure. It produces UNKNOWN rather than a
// death: "I could not tell" must never be recorded as "it is gone".
func Judge(entry Entry, holders []int, probeErr error) (verdict, reason string) {
	if probeErr != nil {
		return VerdictUnknown, fmt.Sprintf("the workspace lock probe failed, so whether pid %d survived is unobserved: %v", entry.PID, probeErr)
	}
	alive := false
	for _, pid := range holders {
		if pid == entry.PID {
			alive = true
			break
		}
	}
	switch entry.Disposition {
	case DispositionRolled:
		if alive {
			return VerdictRolled, fmt.Sprintf("the roll was ordered (%s) and pid %d is still finishing; it ends at its own boundary", entry.Reason, entry.PID)
		}
		return VerdictRolled, fmt.Sprintf("the roll was ordered: %s", entry.Reason)
	case DispositionPreserved:
		if alive {
			return VerdictPreserved, fmt.Sprintf("pid %d still holds the workspace lock, so the process serving this session never changed", entry.PID)
		}
		return VerdictDied, fmt.Sprintf("pid %d was left running on purpose and is gone, and NOBODY ORDERED THAT — every async task it owned died with it", entry.PID)
	default:
		return VerdictUnknown, fmt.Sprintf("the ledger recorded disposition %q, which this daemon cannot judge", entry.Disposition)
	}
}

// Tally counts verdicts so a bounce is summarized by what happened to each
// named process rather than by how many processes exist.
type Tally struct {
	Preserved int
	Rolled    int
	Died      int
	Unknown   int
}

// Add records one verdict.
func (t *Tally) Add(verdict string) {
	switch verdict {
	case VerdictPreserved:
		t.Preserved++
	case VerdictRolled:
		t.Rolled++
	case VerdictDied:
		t.Died++
	default:
		t.Unknown++
	}
}

// Report judges every entry and logs one line per session plus a summary.
//
// holders answers "who holds this workspace's lock" and its error is passed
// straight to Judge rather than being read as an absence. Report returns the
// tally so a caller can act on a non-zero death count.
func Report(logf func(string, ...any), entries []Entry, holders func(workspace string) ([]int, error)) Tally {
	sorted := append([]Entry(nil), entries...)
	sort.Slice(sorted, func(i, j int) bool { return sorted[i].SessionID < sorted[j].SessionID })
	var tally Tally
	for _, entry := range sorted {
		pids, err := holders(entry.Workspace)
		verdict, reason := Judge(entry, pids, err)
		tally.Add(verdict)
		logf("server: bounce accounting session=%s ws=%q shim_pid=%d verdict=%s — %s",
			entry.SessionID, entry.Workspace, entry.PID, verdict, reason)
	}
	if len(sorted) == 0 {
		logf("server: bounce accounting EMPTY — no predecessor ledger, so this boot makes no claim about what happened to any shim")
		return tally
	}
	logf("server: bounce accounting SUMMARY sessions=%d preserved=%d rolled=%d died=%d unknown=%d — a bounce is scored by pid identity, never by process count",
		len(sorted), tally.Preserved, tally.Rolled, tally.Died, tally.Unknown)
	if tally.Died > 0 {
		logf("server: bounce accounting FLEET LOSS died=%d of %d — shims this daemon's predecessor promised to preserve are gone; the async work they owned is gone with them",
			tally.Died, len(sorted))
	}
	return tally
}

// ---------------------------------------------------------------------------
// THE END-STATE HALF: what actually became of a shim the boot judged PRESERVED.
//
// WHY A SECOND VERDICT EXISTS. Judge above rules at BOOT, from the kernel's
// answer to "who holds this workspace lock right now". That answer was true and
// useless on 2026-08-10 20:11: all six shims were judged PRESERVED at 20:11:26
// and all six were killed by this same daemon at 20:12:03, because the bring-up
// that was supposed to reattach to them could not, treated them as failed
// bring-ups, and replaced them. A ledger that stops at the boot verdict
// therefore records a preservation that did not hold — the exact accounting
// failure this package was written to end, one layer later.
//
// So a bounce is closed out with a SECOND verdict, taken after the bring-ups
// have ruled: ADOPTED (the promise held end to end) or REPLACED (the survivor
// was ended and a new process took the workspace, with the reason that decided
// it). A session nothing ruled on is UNSETTLED and never counted as either:
// silence is not adoption.
// ---------------------------------------------------------------------------

// The end-state outcomes a bring-up reaches about a preserved shim.
const (
	// OutcomeAdopted — the surviving process was reattached to and still serves
	// the session. This is the only outcome that keeps the promise.
	OutcomeAdopted = "ADOPTED"
	// OutcomeReplaced — the survivor was deliberately ended and replaced. It is
	// a legitimate outcome (an unreachable shim MUST be replaced) and it is NOT
	// preservation; the reason that decided it travels with it.
	OutcomeReplaced = "REPLACED"
	// OutcomeUnsettled — no bring-up ruled on this session during the bounce,
	// so nothing is claimed about it either way.
	OutcomeUnsettled = "UNSETTLED"
)

// Settlement collects the end-state outcomes, keyed by WORKSPACE because that
// is the identity the bring-up gate owns: it decides between adopting a live
// holder of a workspace lock and replacing it before any session id is minted.
//
// It is written from bring-up goroutines and read by the boot sweep, so it
// carries its own mutex.
type Settlement struct {
	mu       sync.Mutex
	outcomes map[string]outcome
}

type outcome struct {
	verdict string
	reason  string
}

// NewSettlement returns an empty settlement.
func NewSettlement() *Settlement {
	return &Settlement{outcomes: map[string]outcome{}}
}

// Adopted records that the workspace's surviving shim was reattached to.
func (s *Settlement) Adopted(workspace, reason string) {
	s.record(workspace, OutcomeAdopted, reason)
}

// Replaced records that the workspace's surviving shim was ended and replaced.
//
// The reason is REQUIRED in the same sense a roll's is: a replacement that
// cannot say why is the silent death this package exists to stop accepting. An
// empty one is recorded as the omission it is rather than dropped.
func (s *Settlement) Replaced(workspace, reason string) {
	if reason == "" {
		reason = "NO REASON WAS RECORDED — the replacement is real and its cause was not stated, which is itself a defect"
	}
	s.record(workspace, OutcomeReplaced, reason)
}

func (s *Settlement) record(workspace, verdict, reason string) {
	if s == nil || workspace == "" {
		return
	}
	s.mu.Lock()
	defer s.mu.Unlock()
	if s.outcomes == nil {
		s.outcomes = map[string]outcome{}
	}
	// A REPLACEMENT IS NEVER OVERWRITTEN BY AN ADOPTION. A workspace whose
	// survivor was killed and whose REPLACEMENT then connected has been
	// replaced, and adopting the new process must not launder that into a
	// preservation of the old one.
	if prev, ok := s.outcomes[workspace]; ok && prev.verdict == OutcomeReplaced && verdict == OutcomeAdopted {
		return
	}
	s.outcomes[workspace] = outcome{verdict: verdict, reason: reason}
}

// Outcome reports the end-state verdict recorded for a workspace. The third
// result is false when nothing ruled on it.
func (s *Settlement) Outcome(workspace string) (verdict, reason string, ok bool) {
	if s == nil {
		return OutcomeUnsettled, "", false
	}
	s.mu.Lock()
	defer s.mu.Unlock()
	got, ok := s.outcomes[workspace]
	if !ok {
		return OutcomeUnsettled, "", false
	}
	return got.verdict, got.reason, true
}

// EndTally counts end-state outcomes over the preserved entries.
type EndTally struct {
	Adopted   int
	Replaced  int
	Unsettled int
}

// ReportEnd closes a bounce's accounting: for every entry the OUTGOING daemon
// meant to preserve, it says what the incoming daemon's bring-up actually did
// with that process.
//
// It deliberately reports only preserved entries: a roll's end state was
// decided when it was ordered, and Judge already reports it.
func ReportEnd(logf func(string, ...any), entries []Entry, settlement *Settlement) EndTally {
	sorted := append([]Entry(nil), entries...)
	sort.Slice(sorted, func(i, j int) bool { return sorted[i].SessionID < sorted[j].SessionID })
	var tally EndTally
	preserved := 0
	for _, entry := range sorted {
		if entry.Disposition != DispositionPreserved {
			continue
		}
		preserved++
		verdict, reason, ruled := settlement.Outcome(entry.Workspace)
		switch {
		case !ruled:
			tally.Unsettled++
			logf("server: bounce end-state session=%s ws=%q shim_pid=%d verdict=%s — no bring-up ruled on this workspace during the bounce, so whether pid %d was reattached to is unclaimed",
				entry.SessionID, entry.Workspace, entry.PID, OutcomeUnsettled, entry.PID)
		case verdict == OutcomeAdopted:
			tally.Adopted++
			logf("server: bounce end-state session=%s ws=%q shim_pid=%d verdict=%s — %s",
				entry.SessionID, entry.Workspace, entry.PID, OutcomeAdopted, reason)
		default:
			tally.Replaced++
			logf("server: bounce end-state session=%s ws=%q shim_pid=%d verdict=%s — PRESERVATION DID NOT HOLD: the ledger judged this pid preserved at boot and this daemon replaced it anyway: %s",
				entry.SessionID, entry.Workspace, entry.PID, OutcomeReplaced, reason)
		}
	}
	if preserved == 0 {
		return tally
	}
	logf("server: bounce end-state SUMMARY preserved_entries=%d adopted=%d replaced=%d unsettled=%d — a preservation is only real if the same process is still serving the session after bring-up",
		preserved, tally.Adopted, tally.Replaced, tally.Unsettled)
	if tally.Replaced > 0 {
		logf("server: bounce end-state PRESERVATION BROKEN replaced=%d of %d — shims that survived the bounce were killed by THIS daemon's bring-up instead of being reattached to",
			tally.Replaced, preserved)
	}
	return tally
}
