// Package apiresponses files activity units under the API RESPONSE each one
// arrived in, which is the ONLY grouping the vendor's usage reconciles over.
//
// THE WIRE NAMES NO API RESPONSE. The grouping is read off the one rule the
// contract states (agent_activity.proto, and daemon.md's standing gotcha):
// usage rides EXACTLY ONE unit per API response — the unit for the response's
// FIRST content block — so a unit WITHOUT usage means "not the carrying unit",
// never "free". Comparing the settled-response set against the usage-carrying
// set compares two different key spaces and is always wrong: one API response
// routinely produces several response units (a `[text, tool_use, text]`
// message), and its single usage stamp accounts for all of them.
//
// One ledger, shared by every resolver that reconciles token accounting, so
// the rule cannot drift between the footer's per-turn verdict and the topbar's
// per-session warning.
package apiresponses

// Ledger is one accumulation's unit-to-response filing.
//
// Its scope is the OWNER's: the footer builds a fresh one per turn, the topbar
// keeps one per session. The ledger itself has no opinion about either.
type Ledger struct {
	// unitResponse is the API response each unit seen so far belongs to.
	unitResponse map[string]int
	// carried reports, per API response, whether its carrying unit's usage has
	// been observed.
	carried map[int]bool
	// settled is every settled RESPONSE unit, mapped to the API response it
	// arrived in.
	settled map[string]int
	// current is the API response units are arriving for. Zero is the response
	// NOTHING has claimed — the units seen before any usage-carrying unit —
	// and it never counts as carrying usage.
	current int
	// seq mints API response identities. It starts at one so zero stays the
	// unclaimed response.
	seq int
}

// New builds an empty ledger.
func New() *Ledger {
	return &Ledger{
		unitResponse: map[string]int{},
		carried:      map[int]bool{},
		settled:      map[string]int{},
	}
}

// Observe files one unit under an API response.
//
// A unit arriving WITH usage opens a new response; every unit after it, until
// the next usage-carrying one, belongs to that same response.
//
// A UNIT IS FILED ONCE. Its later frames (progress, settle) restate the same
// unit and must not re-open a response; a unit that reports its usage on a
// LATER frame marks the response it was already filed under, which is how a
// verdict reached before the usage landed corrects itself when it does.
func (l *Ledger) Observe(unit string, carriesUsage bool) {
	if response, filed := l.unitResponse[unit]; filed {
		if carriesUsage {
			l.carried[response] = true
		}
		return
	}
	if carriesUsage {
		l.seq++
		l.current = l.seq
		l.carried[l.current] = true
	}
	l.unitResponse[unit] = l.current
}

// Settle records that a RESPONSE unit reached its terminal, under the API
// response it arrived in. It is the reconciliation's denominator.
func (l *Ledger) Settle(unit string) {
	l.settled[unit] = l.unitResponse[unit]
}

// Unaccounted counts the settled response units whose API response never
// carried any usage. A response whose usage rode a DIFFERENT unit — the
// thinking unit that opened it, say — is fully accounted for and is not one of
// them.
func (l *Ledger) Unaccounted() int {
	missing := 0
	for _, response := range l.settled {
		if !l.carried[response] {
			missing++
		}
	}
	return missing
}
