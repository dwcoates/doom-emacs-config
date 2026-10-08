package footer

import (
	"fmt"
	"sort"
	"strings"
	"time"

	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"
	"claude-repld/internal/apiresponses"
	"claude-repld/internal/dlog"
	"claude-repld/internal/figures"
	"claude-repld/internal/freshinput"
	"claude-repld/internal/ids"
)

// idleFigure is the cell's figure with no turn in flight. The daemon states it
// so the client never infers idleness from an absent or zero figure.
const idleFigure = "--"

// mainAgentLabel is the panel's name for the main agent's share.
const mainAgentLabel = "main"

// mainGroup is the accounting's key for the MAIN agent's share. It is not an
// agent id: every unit no subagent row claims is the main agent's, whatever id
// it arrived under, so the panel lists one main entry however the producer
// spelled the main agent.
const mainGroup = "\x00main"

// tokenState is ONE TURN's token accounting. It is reset at every turn start,
// because the strip's cell is the turn's figure and not the session's (the
// session's is the topbar's, a different fact and a different component).
//
// IT HOLDS TWO DIFFERENT FACTS, and they never feed each other:
//   - the per-unit usage (usage below), per agent and summed, subagents and
//     detached agents included. The CELL's figure is the MAIN agent's share of
//     it — its fresh input (freshinput.Of) — and the PANEL draws every agent's.
//     The expensive-turn alarm reads the whole spend, never the growth: a cold
//     cache re-bills the whole prefix without growing the context at all;
//   - the main agent's context growth (ctx below), read from
//     `SessionContextUsage.total_tokens` — the same fact the topbar's context
//     chip draws — which only the panel's context-growth line shows.
//
// DOUBLE-COUNT PREVENTION IS STRUCTURAL HERE: usage is stamped on exactly one
// unit per API response, and a unit's frames UPSERT, so the same usage arrives
// again on every later frame of that unit. Usage is therefore kept KEYED BY
// UNIT and REPLACED, never added, and the turn's figures are a sum over the
// map. Summing frames as they arrive would multiply the bill by the number of
// frames each unit produced.
type tokenState struct {
	// usage is the last usage seen for each unit that carried any.
	usage map[string]*conversationv1.TokenUsage
	// unitAgent files which agent produced each usage-carrying unit. It is the
	// KEY to carrying a still-running DETACHED agent's spend across a turn
	// boundary: a unit belongs to the turn that must reset it unless the agent
	// that produced it is still live and detached, in which case reset keeps it
	// (see reset). Keyed by unit, exactly like `usage`, so the two are dropped
	// together.
	unitAgent map[string]string
	// unitGroup files each unit under the panel entry it is drawn in: mainGroup
	// for the main agent, the agent id for a subagent or detached agent.
	unitGroup map[string]string
	// groups names and orders the panel's per-agent entries, by group key.
	groups map[string]*tokenGroup
	// nextGroup is the order the next new entry takes.
	nextGroup int
	// carried marks the units a reset carried over from an earlier turn: a
	// live detached agent's spend, which leaves the moment the agent retires.
	carried map[string]struct{}
	// outside marks the units first observed while no turn was open — a
	// detached agent's background spend, which also leaves when it retires.
	outside map[string]struct{}
	// opened reports whether a turn has opened on this accounting (reset sets
	// it); together with settled it says whether a turn is open right now.
	opened bool
	// ctx is the main agent's context growth, the panel's context-growth line.
	ctx contextGrowth
	// responses files this turn's units under the API RESPONSE each arrived
	// in, and is the verdict's denominator. THE RECONCILIATION IS PER API
	// RESPONSE, NEVER PER UNIT: usage rides exactly one unit per API response,
	// so counting usage-carrying units against settled response units compares
	// two different key spaces and calls an ordinary prose turn incomplete.
	responses *apiresponses.Ledger
	// contradictions are the reconciliation problems observed this turn.
	contradictions []string
	// settled reports whether the turn has concluded, which is when a verdict
	// exists at all.
	settled bool
	// firstTokenAt is the latency from the current response's start to its
	// first update, nil until one lands.
	firstToken *time.Duration
	// responseStart is when each open response unit began.
	responseStart map[string]time.Time
	// alarmTripped reports whether the expensive-turn alarm tripped this turn.
	alarmTripped bool
	// alarmLine is the composed alarm sentence.
	alarmLine string
}

// tokenGroup is one panel entry's name and place.
type tokenGroup struct {
	label string
	order int
}

// usageAgent is who produced a usage-carrying unit, as the panel names it.
type usageAgent struct {
	// id is the producer's agent id, verbatim; it is what a retirement names.
	id string
	// group is the panel entry the unit is drawn under.
	group string
	// label is that entry's name.
	label string
}

// usageAgent classifies the agent a usage frame arrived under. An agent a
// subagent row claims is that subagent, named as its row is; every other agent
// is the main agent. The row is matched on all three of its ids, because the
// handle, the spawn unit and the created agent are one value by contract (see
// retireWork).
func (s *wsState) usageAgent(id string) usageAgent {
	if row := s.subagentRow(id); row != nil {
		return usageAgent{id: id, group: id, label: subagentUsageLabel(row)}
	}
	return usageAgent{id: id, group: mainGroup, label: mainAgentLabel}
}

// subagentUsageLabel names a subagent's panel entry: its type, then its
// description when the commission gave one.
func subagentUsageLabel(row *agentRow) string {
	if row.description == "" || row.description == row.label {
		return row.label
	}
	return row.label + " · " + truncate(row.description, 32)
}

// contextGrowth is the main agent's context growth: the context held now
// against the baseline taken when the turn opened. Both are readings of
// `SessionContextUsage.total_tokens`.
type contextGrowth struct {
	// held is the latest reading; heldKnown whether any has arrived.
	held      int64
	heldKnown bool
	// baseline is what the growth is measured from; baselineKnown whether one
	// has been taken since a turn opened.
	baseline      int64
	baselineKnown bool
	// sinceCut reports that a cut re-took the baseline during this turn.
	sinceCut bool
}

// contextEvent is what one reading did to the growth, for the record.
type contextEvent int

// The reading outcomes.
const (
	// contextHeld moved the held figure and nothing else.
	contextHeld contextEvent = iota
	// contextBaselineTaken was the first reading since a turn opened with no
	// reading in hand, so it became the baseline.
	contextBaselineTaken
	// contextCutRebased fell below the baseline, which only a cut does, so
	// the baseline became the post-cut reading.
	contextCutRebased
)

// rebased is the growth a new turn opens on: the baseline is the context held
// right now, and nothing has been cut yet.
func (c contextGrowth) rebased() contextGrowth {
	return contextGrowth{held: c.held, heldKnown: c.heldKnown, baseline: c.held, baselineKnown: c.heldKnown}
}

// value is the growth, and whether there is one to state.
func (c contextGrowth) value() (uint64, bool) {
	if !c.baselineKnown || !c.heldKnown {
		return 0, false
	}
	return uint64(c.held - c.baseline), true
}

// newTokenState builds an empty turn accounting.
func newTokenState() tokenState {
	return tokenState{
		usage:         map[string]*conversationv1.TokenUsage{},
		unitAgent:     map[string]string{},
		unitGroup:     map[string]string{},
		groups:        map[string]*tokenGroup{},
		carried:       map[string]struct{}{},
		outside:       map[string]struct{}{},
		responseStart: map[string]time.Time{},
		responses:     apiresponses.New(),
	}
}

// turnOpen reports whether a turn is open on this accounting right now.
func (t *tokenState) turnOpen() bool {
	return t.opened && !t.settled
}

// observeContext takes one reading of the context held. A turn that opened
// with no reading in hand takes its baseline from the first one; a reading
// below the baseline is a cut, and re-takes it. Readings before any turn has
// opened only move the held figure.
func (t *tokenState) observeContext(total int64) contextEvent {
	c := &t.ctx
	c.held, c.heldKnown = total, true
	switch {
	case !t.opened:
		return contextHeld
	case !c.baselineKnown:
		c.baseline, c.baselineKnown = total, true
		return contextBaselineTaken
	case total < c.baseline:
		c.baseline, c.sinceCut = total, true
		return contextCutRebased
	default:
		return contextHeld
	}
}

// observeContextUsage folds one `context_usage` reading into the cell's figure.
// A reading the contract cannot hold — no payload, or a negative count — is a
// producer defect: it is recorded at WARN and the figure keeps standing on the
// last good reading rather than being moved by a bad one.
func (r *resolver) observeContextUsage(ws ids.WorkspaceID, s *wsState, usage *conversationv1.SessionContextUsage) {
	log := r.logOf(ws, s)
	if usage == nil {
		log.Warn("daemon.footer.context_usage_unreadable",
			"a context_usage update carried no reading; the context growth stands on the last one",
			dlog.Context{"reason": "no_payload"})
		return
	}
	total := usage.GetTotalTokens()
	if total < 0 {
		log.Warn("daemon.footer.context_usage_unreadable",
			"a context_usage update carried a negative token count; the context growth stands on the last one",
			dlog.Context{"reason": "negative_total", "total_tokens": total})
		return
	}
	previousBaseline := s.tok.ctx.baseline
	switch s.tok.observeContext(total) {
	case contextBaselineTaken:
		log.Info("daemon.footer.context_baseline_taken",
			"the turn opened with no context reading in hand, so its first reading is the baseline",
			dlog.Context{"total_tokens": total})
	case contextCutRebased:
		log.Info("daemon.footer.context_cut_rebased",
			"the context held fell below the turn's baseline, which only a cut does; the growth is measured from the cut",
			dlog.Context{"total_tokens": total, "previous_baseline": previousBaseline})
	default:
		growth, known := s.tok.ctx.value()
		log.Debug("daemon.footer.context_held",
			"the footer took a context reading",
			dlog.Context{"total_tokens": total, "growth": growth, "growth_known": known})
	}
}

// reset clears the accounting for a new turn, EXCEPT the usage of units that a
// still-live detached agent produced: those agents run in the background across
// turn boundaries and their uncached input is real input the account is still
// paying for, so wiping it at the next turn start is what made the panel's
// spend fall back to the new turn alone and read as though the background work
// cost nothing. `keep` is the set of agent ids that are live and detached RIGHT
// NOW (the resolver derives it from the live agents chip); a unit whose agent
// is in it carries forward, everything else — the concluded turn's own units,
// the alarm, the response ledger, the latency — is cleared.
//
// The alarm is cleared here and nowhere else: the contract keeps the glyph
// until the NEXT turn starts.
//
// DOUBLE-COUNTING IS STILL STRUCTURALLY PREVENTED: a carried-over unit keeps
// its own key, so a later frame of it still UPSERTS rather than adds, exactly
// as within a turn.
//
// THE CONTEXT BASELINE IS RE-TAKEN HERE, from the context held right now. The
// resolver resets at the accepted turn AND at the turn-open edge, so the last
// baseline taken is the turn-open edge's — after any reading the previous
// turn's close pushed.
func (t *tokenState) reset(keep map[string]struct{}) {
	fresh := newTokenState()
	fresh.opened = true
	fresh.ctx = t.ctx.rebased()
	fresh.nextGroup = t.nextGroup
	for unit, agent := range t.unitAgent {
		if _, live := keep[agent]; !live {
			continue
		}
		u, ok := t.usage[unit]
		if !ok {
			continue
		}
		group := t.unitGroup[unit]
		fresh.usage[unit] = u
		fresh.unitAgent[unit] = agent
		fresh.unitGroup[unit] = group
		fresh.carried[unit] = struct{}{}
		if g, ok := t.groups[group]; ok {
			fresh.groups[group] = g
		}
	}
	*t = fresh
}

// forgetAgent drops a now-retired agent's BACKGROUND spend — the units carried
// over from an earlier turn and the units it reported while no turn was open —
// so a detached agent's spend stops counting the instant its run settles
// rather than standing in the panel for the rest of the session. Units it
// reported during the open turn stay: they are that turn's spend, and the
// panel keeps the most recent turn's breakdown until the next turn resets it.
// Called from the chip's own retirement, the one place a run is known to be
// over.
func (t *tokenState) forgetAgent(agent string) {
	if agent == "" {
		return
	}
	for unit, owner := range t.unitAgent {
		if owner != agent {
			continue
		}
		_, carried := t.carried[unit]
		_, outside := t.outside[unit]
		if !carried && !outside {
			continue
		}
		delete(t.usage, unit)
		delete(t.unitAgent, unit)
		delete(t.unitGroup, unit)
		delete(t.carried, unit)
		delete(t.outside, unit)
	}
}

// observeUsage records one unit's usage, replacing whatever that unit reported
// before, and files the agent that produced it so reset can carry a live
// detached agent's units across the turn boundary and the panel can draw the
// unit under its agent. A unit whose usage CHANGES between frames is a producer
// contradiction and is recorded as one rather than silently taking the later
// value.
func (t *tokenState) observeUsage(unit string, agent usageAgent, u *conversationv1.TokenUsage) {
	if u == nil {
		return
	}
	if _, seen := t.usage[unit]; !seen && !t.turnOpen() {
		t.outside[unit] = struct{}{}
	}
	if prev, ok := t.usage[unit]; ok && !sameUsage(prev, u) {
		t.contradictions = append(t.contradictions,
			fmt.Sprintf("unit %s reported two different usages", unit))
	}
	if u.GetOutputThinkingTokens() > u.GetOutputTokens() {
		t.contradictions = append(t.contradictions,
			fmt.Sprintf("unit %s reports more thinking tokens than output tokens", unit))
	}
	t.usage[unit] = u
	if agent.id != "" {
		t.unitAgent[unit] = agent.id
	}
	t.unitGroup[unit] = agent.group
	if g, ok := t.groups[agent.group]; ok {
		g.label = agent.label
		return
	}
	t.groups[agent.group] = &tokenGroup{label: agent.label, order: t.nextGroup}
	t.nextGroup++
}

// sameUsage compares two usage records field for field.
func sameUsage(a, b *conversationv1.TokenUsage) bool {
	return a.GetInputHits().GetRead() == b.GetInputHits().GetRead() &&
		a.GetInputMisses().GetWritten() == b.GetInputMisses().GetWritten() &&
		a.GetInputMisses().GetUnwritten() == b.GetInputMisses().GetUnwritten() &&
		a.GetOutputTokens() == b.GetOutputTokens() &&
		a.GetOutputThinkingTokens() == b.GetOutputThinkingTokens()
}

// totals are the turn's summed figures.
type totals struct {
	// misses is input_misses — written plus unwritten, the figure that costs
	// money at full price and the ONLY one the strip's cell shows.
	misses uint64
	// cacheRead is what the prompt cache served.
	cacheRead uint64
	// cacheWrite is what was processed fresh AND entered the cache.
	cacheWrite uint64
	// output is every generated token, thinking included.
	output uint64
	// thinking is the part of output the vendor attributed to reasoning. NEVER
	// added to output: it is already inside it.
	thinking uint64
}

// add folds one unit's usage into the figures.
func (out *totals) add(u *conversationv1.TokenUsage) {
	out.cacheRead += u.GetInputHits().GetRead()
	out.cacheWrite += u.GetInputMisses().GetWritten()
	out.misses += freshinput.Of(u)
	out.output += u.GetOutputTokens()
	out.thinking += u.GetOutputThinkingTokens()
}

// sum folds every unit's usage into the turn's figures — the spend across
// every agent.
func (t *tokenState) sum() totals {
	var out totals
	for _, u := range t.usage {
		out.add(u)
	}
	return out
}

// groupSums folds each unit's usage into its agent's figures.
func (t *tokenState) groupSums() map[string]*totals {
	out := map[string]*totals{}
	for unit, u := range t.usage {
		group := t.unitGroup[unit]
		sums, ok := out[group]
		if !ok {
			sums = &totals{}
			out[group] = sums
		}
		sums.add(u)
	}
	return out
}

// verdict is the settled turn's reconciliation.
type verdict int

// The verdicts, one per FooterTokensCellVerdict arm.
const (
	verdictNone verdict = iota
	verdictComplete
	verdictIncomplete
	verdictInvalid
)

// reconcile decides the settled turn's verdict and the evidence behind it. A
// running turn has no verdict at all, which is what UNSET means on the wire.
func (t *tokenState) reconcile() (verdict, string) {
	if !t.settled {
		return verdictNone, ""
	}
	if len(t.contradictions) > 0 {
		problems := append([]string(nil), t.contradictions...)
		sort.Strings(problems)
		return verdictInvalid, strings.Join(problems, "; ")
	}
	missing := t.responses.Unaccounted()
	if missing > 0 {
		return verdictIncomplete, fmt.Sprintf("%s missing usage", plural(missing, "response"))
	}
	return verdictComplete, ""
}

// plural renders "1 response" / "2 responses".
func plural(n int, noun string) string {
	if n == 1 {
		return fmt.Sprintf("1 %s", noun)
	}
	return fmt.Sprintf("%d %ss", n, noun)
}

// evaluateAlarm trips the expensive-turn alarm once the turn's uncached input
// crosses the threshold, composing the sentence the panel draws. It never
// un-trips: the contract keeps the glyph for the rest of the turn.
func (t *tokenState) evaluateAlarm(threshold uint64) {
	if t.alarmTripped || threshold == 0 {
		return
	}
	misses := t.sum().misses
	if misses <= threshold {
		return
	}
	t.alarmTripped = true
	over := misses - threshold
	t.alarmLine = fmt.Sprintf("expensive turn — %s over %s",
		figures.Tokens(over), figures.Tokens(threshold))
}

// mainFresh answers the main agent's fresh input this turn — the fresh input
// of every unit filed under the main agent's panel entry — and whether the
// main agent has stated any usage in this accounting at all.
func (t *tokenState) mainFresh() (uint64, bool) {
	sums, ok := t.groupSums()[mainGroup]
	if !ok {
		return 0, false
	}
	return sums.misses, true
}

// cell renders the strip's tokens cell: the main agent's fresh input, with its
// heat. The glyphs keep their own lifetimes, so an idle cell still carries the
// most recent turn's alarm and verdict.
//
// THE FIGURE OUTLIVES ITS TURN (owner ruling, 2026-10-08): a turn's figure
// stands after the turn ends, however it ended, and clears only at the next
// turn's start — the accepted submission (SetTurn → reset), the same edge that
// raises `working · submitting`. A held or queued prompt submits nothing, so
// the figure stands through it. With no turn in flight the figure is drawn
// only when the most recent turn's main agent stated usage; otherwise (no turn
// has run, a turn that spent nothing such as a /clear, a refused submission
// whose reset left nothing) the cell is the uncolored idle figure, never a
// `0 in` that no turn spent.
func (t *tokenState) cell(inFlight bool) *frontendv1.FooterTokensCell {
	input := &frontendv1.FooterTokensCellInput{Text: idleFigure}
	if fresh, stated := t.mainFresh(); inFlight || stated {
		input.Text = figures.Tokens(fresh) + " in"
		input.Heat = &frontendv1.TokenHeat{Position: figures.TokenHeat(fresh)}
	}
	out := &frontendv1.FooterTokensCell{Input: input}
	if t.alarmTripped {
		out.Alarm = &frontendv1.FooterTokensCellAlarm{}
	}
	switch v, _ := t.reconcile(); v {
	case verdictComplete:
		out.Verdict = &frontendv1.FooterTokensCellVerdict{
			Verdict: &frontendv1.FooterTokensCellVerdict_Complete{
				Complete: &frontendv1.FooterTokensCellVerdictComplete{},
			},
		}
	case verdictIncomplete:
		out.Verdict = &frontendv1.FooterTokensCellVerdict{
			Verdict: &frontendv1.FooterTokensCellVerdict_Incomplete{
				Incomplete: &frontendv1.FooterTokensCellVerdictIncomplete{},
			},
		}
	case verdictInvalid:
		out.Verdict = &frontendv1.FooterTokensCellVerdict{
			Verdict: &frontendv1.FooterTokensCellVerdict_Invalid{
				Invalid: &frontendv1.FooterTokensCellVerdictInvalid{},
			},
		}
	}
	return out
}

// panel renders the tokens panel. EVERY line message is always set so the
// panel's shape is stable while the turn runs; a figure not yet known carries
// no value and draws an empty slot.
func (t *tokenState) panel() *frontendv1.FooterExpandedTokens {
	sums := t.sum()
	known := len(t.usage) > 0
	out := &frontendv1.FooterExpandedTokens{
		Input:         &frontendv1.FooterTokensLineInput{Value: figure(known, sums.misses)},
		CacheRead:     &frontendv1.FooterTokensLineCacheRead{Value: figure(known, sums.cacheRead)},
		CacheWrite:    &frontendv1.FooterTokensLineCacheWrite{Value: figure(known, sums.cacheWrite)},
		Output:        &frontendv1.FooterTokensLineOutput{Value: figure(known, sums.output)},
		Thinking:      &frontendv1.FooterTokensLineThinking{Value: figure(known, sums.thinking)},
		FirstToken:    &frontendv1.FooterTokensLineFirstToken{},
		ContextGrowth: &frontendv1.FooterTokensLineContextGrowth{},
		Agents:        t.agentEntries(),
	}
	if growth, ok := t.ctx.value(); ok {
		v := figures.Tokens(growth)
		out.ContextGrowth.Value = &v
	}
	if t.ctx.sinceCut {
		out.ContextGrowth.SinceCut = &frontendv1.FooterTokensLineContextGrowthSinceCut{}
	}
	if t.firstToken != nil {
		v := formatLatency(*t.firstToken)
		out.FirstToken.Value = &v
	}
	if t.alarmTripped {
		out.Alarm = &frontendv1.FooterTokensLineAlarm{Text: t.alarmLine}
	}
	switch v, evidence := t.reconcile(); v {
	case verdictComplete:
		out.Verdict = &frontendv1.FooterTokensLineVerdict{
			Verdict: &frontendv1.FooterTokensLineVerdict_Complete{
				Complete: &frontendv1.FooterTokensLineVerdictComplete{},
			},
		}
	case verdictIncomplete:
		out.Verdict = &frontendv1.FooterTokensLineVerdict{
			Verdict: &frontendv1.FooterTokensLineVerdict_Incomplete{
				Incomplete: &frontendv1.FooterTokensLineVerdictIncomplete{Text: evidence},
			},
		}
	case verdictInvalid:
		out.Verdict = &frontendv1.FooterTokensLineVerdict{
			Verdict: &frontendv1.FooterTokensLineVerdict_Invalid{
				Invalid: &frontendv1.FooterTokensLineVerdictInvalid{Text: evidence},
			},
		}
	}
	return out
}

// agentEntries renders the per-agent entries: the main agent first, then every
// other agent in the order its usage first arrived. Only an agent with usage in
// the accounting is listed, so every entry's lines are set.
func (t *tokenState) agentEntries() []*frontendv1.FooterTokensAgent {
	sums := t.groupSums()
	keys := make([]string, 0, len(sums))
	for key := range sums {
		keys = append(keys, key)
	}
	// EVERY GROUP KEY HAS A tokenGroup: observeUsage files one with the unit,
	// and reset copies it with every unit it carries.
	sort.Slice(keys, func(i, j int) bool {
		if (keys[i] == mainGroup) != (keys[j] == mainGroup) {
			return keys[i] == mainGroup
		}
		if oi, oj := t.groups[keys[i]].order, t.groups[keys[j]].order; oi != oj {
			return oi < oj
		}
		return keys[i] < keys[j]
	})
	out := make([]*frontendv1.FooterTokensAgent, 0, len(keys))
	for _, key := range keys {
		s := sums[key]
		out = append(out, &frontendv1.FooterTokensAgent{
			Label:      t.groups[key].label,
			Input:      &frontendv1.FooterTokensLineInput{Value: figure(true, s.misses)},
			CacheRead:  &frontendv1.FooterTokensLineCacheRead{Value: figure(true, s.cacheRead)},
			CacheWrite: &frontendv1.FooterTokensLineCacheWrite{Value: figure(true, s.cacheWrite)},
			Output:     &frontendv1.FooterTokensLineOutput{Value: figure(true, s.output)},
		})
	}
	return out
}

// figure renders a panel line's value, UNSET while nothing is known yet.
func figure(known bool, n uint64) *string {
	if !known {
		return nil
	}
	v := figures.Tokens(n)
	return &v
}
