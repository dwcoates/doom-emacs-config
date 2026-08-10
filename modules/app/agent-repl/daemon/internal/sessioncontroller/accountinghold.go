package sessioncontroller

import (
	"sort"
	"sync"

	frontendv1 "agentrepl/proto/agentshim/frontend/v1"

	"claude-repld/internal/dlog"

	"google.golang.org/protobuf/proto"
)

// accountinghold.go — THE LEVEL-TRIGGERED ENRICHMENT LEDGER.
//
// # The failure class it makes unrepresentable
//
// A turn's accounting stamp is not complete until every response it names
// carries the vendor's FINAL usage, and that final usage arrives on a separate
// frame (`message_delta`) from the response it corrects. Whether the correction
// lands before or after the thing that waits for it is the vendor's business,
// not the daemon's: on one observed turn the correction for
// msg_011CdswkkMZyUqE6amdCWSjJ arrived 52ms BEFORE the hold that would have
// waited for it was installed.
//
// An EDGE-TRIGGERED release cannot survive that ordering. It wakes the waiter on
// the correction's ARRIVAL, so a correction that arrived first wakes nobody, and
// the hold installed afterwards waits for an edge that has already passed. The
// wait then ends only when something else knocks it loose, which in production
// meant a ten-minute shim watchdog.
//
// This ledger is LEVEL-TRIGGERED instead. Corrections and holds live in ONE map
// pair behind ONE mutex, and the release predicate — "is every api_message_id
// this turn depends on already on file?" — is evaluated AT INSTALL TIME against
// that ledger as well as on every later filing. A correction that precedes its
// hold and one that follows it are therefore structurally indistinguishable:
// both leave the same level in the ledger, and the predicate reads the level
// rather than the transition. There is no wakeup to miss.
//
// # Late updates are a first-class outcome, not a race
//
// A hold is not discharged when it releases. It stays registered, so a
// correction filed after the turn's stamp already settled re-fires the release
// with late=true, and the settled stamp is REVISED rather than the correction
// being dropped on the floor. That is what lets the turn settle immediately
// (terminalsettlement.go) without trading away accounting fidelity.

// enrichmentRetention bounds how many turns' holds and filed corrections the
// ledger keeps. It is a MEMORY bound and nothing else: the ledger must outlive
// each turn's own settlement so a late correction still has somewhere to land,
// so it cannot be cleared per turn, and an unbounded map keyed by a
// per-turn-generated id would grow for the life of the session.
//
// Retirement is oldest-installed-first and is LOGGED, because a dropped hold is
// a turn that can no longer be revised and that fact must not be silent.
const enrichmentRetention = 16

// accountingCorrections is the one ledger of response usage corrections and the
// one registry of the enrichment holds that read it. Both are guarded by mu:
// serializing them against each other is the whole mechanism.
type accountingCorrections struct {
	mu sync.Mutex
	// usage is the FINAL vendor usage per api_message_id, as relayed from the
	// vendor's message_delta frame.
	usage map[string]*frontendv1.VendorTokenUsage
	// facts is every VALUELESS dependency on file — an observation a stamp
	// waits for but reads from the reducer rather than from this ledger, such
	// as the turn-end account-usage sample. They share the hold machinery
	// because they share the ordering problem exactly: the observation may land
	// on either side of the hold that depends on it.
	facts map[string]struct{}
	// holds is one enrichment hold per turn id, kept after release so a late
	// correction can still find it.
	holds map[string]*enrichmentHold
	// order is the hold install order, for enrichmentRetention.
	order []string
	logf  dlog.Logf
}

// enrichmentHold is one turn's dependency on the corrections ledger.
type enrichmentHold struct {
	turnID string
	// members is every api_message_id this turn's stamp is derived from.
	members map[string]struct{}
	// awaiting is the members with no correction on file. It is computed at
	// INSTALL time from the ledger, never from the arrival of an event.
	awaiting map[string]struct{}
	// released records that the predicate has already been satisfied once, so a
	// further filing is reported as a LATE revision rather than a first release.
	released bool
	release  func(turnID string, late bool)
}

func newAccountingCorrections(logf dlog.Logf) *accountingCorrections {
	if logf == nil {
		logf = func(string, ...any) {}
	}
	return &accountingCorrections{
		usage: map[string]*frontendv1.VendorTokenUsage{},
		facts: map[string]struct{}{},
		holds: map[string]*enrichmentHold{},
		logf:  logf,
	}
}

// turnEndUsageKey names one turn's END-BOUNDARY account-usage observation as a
// dependency key.
//
// The stamp is not complete without it — usage_at_end is what the turn's
// reconciliation is measured against — and it arrives on its own frame, beside
// the shim's `TurnEnded`, which is after the vendor's result on every observed
// turn. Naming it here is what keeps the stamp waiting for it while the TURN
// itself settles immediately.
func turnEndUsageKey(turnID string) string { return "turn_end_usage:" + turnID }

// RecordFile files one valueless dependency and releases every hold it
// completes, on exactly the terms Record files a correction on.
func (l *accountingCorrections) RecordFact(key string) {
	if key == "" {
		return
	}
	l.mu.Lock()
	l.facts[key] = struct{}{}
	releases := l.satisfyLocked(key)
	l.mu.Unlock()
	l.fire(releases)
}

// onFileLocked reports whether a dependency key is satisfied by the ledger as
// it stands. Caller holds mu.
func (l *accountingCorrections) onFileLocked(key string) bool {
	if _, corrected := l.usage[key]; corrected {
		return true
	}
	_, observed := l.facts[key]
	return observed
}

// Record files one response's FINAL vendor usage and releases every enrichment
// hold the filing completes.
//
// The release callbacks run OUTSIDE the lock, because enrichment resolves,
// persists and republishes a turn's stamp and may re-enter this ledger to read
// the very corrections it was released for.
func (l *accountingCorrections) Record(apiMessageID string, usage *frontendv1.VendorTokenUsage) {
	if apiMessageID == "" || usage == nil {
		return
	}
	l.mu.Lock()
	l.usage[apiMessageID] = proto.Clone(usage).(*frontendv1.VendorTokenUsage)
	releases := l.satisfyLocked(apiMessageID)
	l.mu.Unlock()
	l.fire(releases)
}

// Install registers turnID's enrichment hold over apiMessageIDs and evaluates
// its release predicate AGAINST THE LEDGER, right here, under the same lock a
// concurrent Record would take.
//
// A turn whose corrections are ALREADY all on file releases immediately and
// synchronously — that is the production interleaving this exists for. A turn
// naming no responses at all has an empty predicate and releases the same way,
// because there is nothing for its stamp to wait on.
//
// Re-installing over an existing hold RE-EVALUATES it rather than replacing it
// blind: the turn's member set can only grow as more of its responses are
// observed, and a member added after the first release is a new dependency the
// predicate must account for.
func (l *accountingCorrections) Install(turnID string, dependencies []string, release func(turnID string, late bool)) {
	if turnID == "" || release == nil {
		return
	}
	l.mu.Lock()
	hold := l.holds[turnID]
	if hold == nil {
		hold = &enrichmentHold{turnID: turnID, members: map[string]struct{}{}, awaiting: map[string]struct{}{}}
		l.holds[turnID] = hold
		l.order = append(l.order, turnID)
	}
	hold.release = release
	for _, id := range dependencies {
		if id == "" {
			continue
		}
		hold.members[id] = struct{}{}
		if l.onFileLocked(id) {
			delete(hold.awaiting, id)
			continue
		}
		hold.awaiting[id] = struct{}{}
	}
	var releases []pendingRelease
	if len(hold.awaiting) == 0 {
		releases = append(releases, pendingRelease{fn: hold.release, turnID: turnID, late: hold.released})
		hold.released = true
	}
	l.retireLocked()
	l.mu.Unlock()
	l.fire(releases)
}

// Correction reports the final vendor usage on file for one response, or nil.
// The copy is the caller's: the ledger's own entry may never be mutated by a
// reducer patching a response record in place.
func (l *accountingCorrections) Correction(apiMessageID string) *frontendv1.VendorTokenUsage {
	l.mu.Lock()
	defer l.mu.Unlock()
	filed := l.usage[apiMessageID]
	if filed == nil {
		return nil
	}
	return proto.Clone(filed).(*frontendv1.VendorTokenUsage)
}

// Awaiting names the api_message_ids one turn's stamp is still missing a
// correction for, sorted. An unknown turn awaits nothing.
func (l *accountingCorrections) Awaiting(turnID string) []string {
	l.mu.Lock()
	defer l.mu.Unlock()
	hold := l.holds[turnID]
	if hold == nil {
		return nil
	}
	awaiting := make([]string, 0, len(hold.awaiting))
	for id := range hold.awaiting {
		awaiting = append(awaiting, id)
	}
	sort.Strings(awaiting)
	return awaiting
}

// ApplyTo patches every response of a settled accounting record with the
// correction on file for it, and reports whether anything changed.
//
// It is how a LATE correction reaches a turn whose reducer entry has already
// been retired: the durable record is the only remaining statement of that
// turn's responses, so it is the thing that gets enriched.
func (l *accountingCorrections) ApplyTo(accounting *frontendv1.TurnAccounting) bool {
	if accounting == nil {
		return false
	}
	l.mu.Lock()
	defer l.mu.Unlock()
	changed := false
	for _, response := range accounting.GetResponses() {
		filed := l.usage[response.GetApiMessageId()]
		if filed == nil || proto.Equal(response.GetUsage(), filed) {
			continue
		}
		response.Usage = proto.Clone(filed).(*frontendv1.VendorTokenUsage)
		changed = true
	}
	return changed
}

// satisfyLocked drops apiMessageID from every hold awaiting it and collects the
// holds that the filing completed. Caller holds mu.
func (l *accountingCorrections) satisfyLocked(apiMessageID string) []pendingRelease {
	var releases []pendingRelease
	for turnID, hold := range l.holds {
		if _, member := hold.members[apiMessageID]; !member {
			continue
		}
		delete(hold.awaiting, apiMessageID)
		if len(hold.awaiting) > 0 || hold.release == nil {
			continue
		}
		releases = append(releases, pendingRelease{fn: hold.release, turnID: turnID, late: hold.released})
		hold.released = true
	}
	// Deterministic order, so two turns completed by one filing are enriched in
	// a stated sequence rather than in Go's map order.
	sort.Slice(releases, func(i, j int) bool { return releases[i].turnID < releases[j].turnID })
	return releases
}

// retireLocked drops the oldest holds past enrichmentRetention. Caller holds mu.
func (l *accountingCorrections) retireLocked() {
	for len(l.order) > enrichmentRetention {
		turnID := l.order[0]
		l.order = l.order[1:]
		hold := l.holds[turnID]
		delete(l.holds, turnID)
		if hold == nil {
			continue
		}
		for id := range hold.members {
			delete(l.usage, id)
			delete(l.facts, id)
		}
		l.logf("session-controller: accounting enrichment hold RETIRED turn_id=%s responses=%d awaiting=%d retention=%d — this turn is now past the enrichment window, so a correction arriving for it from here on is filed against nothing and its settled stamp keeps the figures it already carries",
			turnID, len(hold.members), len(hold.awaiting), enrichmentRetention)
	}
}

// pendingRelease is one enrichment callback collected under the lock and run
// after it is dropped.
type pendingRelease struct {
	fn     func(turnID string, late bool)
	turnID string
	late   bool
}

func (l *accountingCorrections) fire(releases []pendingRelease) {
	for _, release := range releases {
		release.fn(release.turnID, release.late)
	}
}
