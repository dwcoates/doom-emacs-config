package footer

import (
	"fmt"
	"sort"
	"strings"
	"time"

	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"
	"claude-repld/internal/apiresponses"
	"claude-repld/internal/figures"
)

// tokenState is ONE TURN's token accounting. It is reset at every turn start,
// because the strip's cell is the turn's figure and not the session's (the
// session's is the topbar's, a different fact and a different component).
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
	// coldKeepalive marks a turn the daemon ran as a cache keep-alive, which
	// the alarm sentence phrases differently.
	coldKeepalive bool
}

// newTokenState builds an empty turn accounting.
func newTokenState() tokenState {
	return tokenState{
		usage:         map[string]*conversationv1.TokenUsage{},
		unitAgent:     map[string]string{},
		responseStart: map[string]time.Time{},
		responses:     apiresponses.New(),
	}
}

// reset clears the accounting for a new turn, EXCEPT the usage of units that a
// still-live detached agent produced: those agents run in the background across
// turn boundaries and their uncached input is real input the account is still
// paying for, so wiping it at the next turn start is what made the strip's
// figure fall back to the new turn alone and read as though the background work
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
func (t *tokenState) reset(keep map[string]struct{}) {
	fresh := newTokenState()
	for unit, agent := range t.unitAgent {
		if _, live := keep[agent]; !live {
			continue
		}
		if u, ok := t.usage[unit]; ok {
			fresh.usage[unit] = u
			fresh.unitAgent[unit] = agent
		}
	}
	*t = fresh
}

// forgetAgent drops every usage unit a now-retired agent produced, so a
// detached agent's spend stops counting the instant its run settles rather than
// standing in the figure for the rest of the session. Called from the chip's
// own retirement, the one place a run is known to be over.
func (t *tokenState) forgetAgent(agent string) {
	if agent == "" {
		return
	}
	for unit, owner := range t.unitAgent {
		if owner == agent {
			delete(t.usage, unit)
			delete(t.unitAgent, unit)
		}
	}
}

// observeUsage records one unit's usage, replacing whatever that unit reported
// before, and files the agent that produced it so reset can carry a live
// detached agent's units across the turn boundary. A unit whose usage CHANGES
// between frames is a producer contradiction and is recorded as one rather than
// silently taking the later value.
func (t *tokenState) observeUsage(unit, agent string, u *conversationv1.TokenUsage) {
	if u == nil {
		return
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
	if agent != "" {
		t.unitAgent[unit] = agent
	}
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

// sum folds every unit's usage into the turn's figures.
func (t *tokenState) sum() totals {
	var out totals
	for _, u := range t.usage {
		out.cacheRead += u.GetInputHits().GetRead()
		out.cacheWrite += u.GetInputMisses().GetWritten()
		out.misses += u.GetInputMisses().GetWritten() + u.GetInputMisses().GetUnwritten()
		out.output += u.GetOutputTokens()
		out.thinking += u.GetOutputThinkingTokens()
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
	if t.coldKeepalive {
		t.alarmLine = fmt.Sprintf(
			"expensive keep-alive — %s over %s, a cache keep-alive that came back cold",
			figures.Tokens(over), figures.Tokens(threshold))
		return
	}
	t.alarmLine = fmt.Sprintf("expensive turn — %s over %s",
		figures.Tokens(over), figures.Tokens(threshold))
}

// cell renders the strip's tokens cell.
func (t *tokenState) cell() *frontendv1.FooterTokensCell {
	out := &frontendv1.FooterTokensCell{
		Input: &frontendv1.FooterTokensCellInput{
			Text: figures.Tokens(t.sum().misses) + " in",
		},
	}
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
		Input:      &frontendv1.FooterTokensLineInput{Value: figure(known, sums.misses)},
		CacheRead:  &frontendv1.FooterTokensLineCacheRead{Value: figure(known, sums.cacheRead)},
		CacheWrite: &frontendv1.FooterTokensLineCacheWrite{Value: figure(known, sums.cacheWrite)},
		Output:     &frontendv1.FooterTokensLineOutput{Value: figure(known, sums.output)},
		Thinking:   &frontendv1.FooterTokensLineThinking{Value: figure(known, sums.thinking)},
		FirstToken: &frontendv1.FooterTokensLineFirstToken{},
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

// figure renders a panel line's value, UNSET while nothing is known yet.
func figure(known bool, n uint64) *string {
	if !known {
		return nil
	}
	v := figures.Tokens(n)
	return &v
}
