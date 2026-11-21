package runtimemetrics

import (
	"fmt"
	"sort"
	"sync"
	"sync/atomic"

	"github.com/prometheus/client_golang/prometheus"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/vendorspend"
	"agentrepl/logging/causelog"
	conversationv1 "agentrepl/proto/conversation/v1"
	shimv1 "agentrepl/proto/shim/v1"
)

// The vendor spend series (skills/agent-repl-metrics/CATALOG.md).
const (
	// VendorTokensName is every vendor call's tokens by kind.
	VendorTokensName = "agentrepl_vendor_tokens_total"
	// VendorCallsName is how many vendor calls were made.
	VendorCallsName = "agentrepl_vendor_calls_total"
	// VendorContextTokensName is the tokens by the context bucket of the
	// call that spent them.
	VendorContextTokensName = "agentrepl_vendor_context_tokens_total"
	// VendorCallContextName is the distribution of the calls' contexts.
	VendorCallContextName = "agentrepl_vendor_call_context_tokens"
)

// spendKey names one kind of call of one workspace.
type spendKey struct {
	ws                                                          ids.WorkspaceID
	account, model, agent, subagentType, cause, outcome, reason string
}

// contextKey names the tokens of one kind spent by calls of one context bucket.
type contextKey struct {
	account, model, context, kind string
}

// contextHistKey names one distribution of the calls' contexts.
type contextHistKey struct {
	account, model, agent string
}

// kindCounts are tokens by kind, in vendorspend.Kinds order.
type kindCounts [5]float64

// spendRun is one shim counting run's last folded frame.
type spendRun struct {
	since   int64
	account string
	entries map[string]*conversationv1.VendorSpend
}

// vendorSpend folds every producer's vendor spend into the spend series.
type vendorSpend struct {
	log    dlog.Logger
	causes causelog.Tracker
	// accountOf labels an account root (SetAccountLabeler); nil until the
	// daemon's account resolver is built.
	accountOf atomic.Pointer[func(configDir string) string]

	mu       sync.Mutex
	accounts map[string]string
	runs     map[ids.WorkspaceID]*spendRun
	calls    map[spendKey]float64
	tokens   map[spendKey]*kindCounts
	context  map[contextKey]float64
	contexts map[contextHistKey]*histogram
	// keepalive is each workspace's keep-alive spend in this daemon's life,
	// which the keeper's run accounting reads (KeepaliveSpent).
	keepalive map[ids.WorkspaceID]*kindCounts

	tokensDesc, callsDesc, contextDesc, histDesc *prometheus.Desc
}

// contextBounds are the context histogram's bounds.
var contextBounds = func() []float64 {
	out := make([]float64, len(vendorspend.ContextBuckets))
	for i, b := range vendorspend.ContextBuckets {
		out[i] = float64(b)
	}
	return out
}()

var spendLabels = []string{"workspace", "account", "model", "agent", "subagent_type", "cause", "cache_outcome", "cold_reason"}

func newVendorSpend(log dlog.Logger) *vendorSpend {
	return &vendorSpend{
		log:       log,
		accounts:  map[string]string{},
		runs:      map[ids.WorkspaceID]*spendRun{},
		calls:     map[spendKey]float64{},
		tokens:    map[spendKey]*kindCounts{},
		context:   map[contextKey]float64{},
		contexts:  map[contextHistKey]*histogram{},
		keepalive: map[ids.WorkspaceID]*kindCounts{},
		tokensDesc: prometheus.NewDesc(VendorTokensName, "Tokens vendor calls spent, by kind, with the call's workspace, account, model, agent, cause and cache outcome.",
			append(append([]string{}, spendLabels...), "kind"), nil),
		callsDesc:   prometheus.NewDesc(VendorCallsName, "Vendor calls made, by workspace, account, model, agent, cause and cache outcome.", spendLabels, nil),
		contextDesc: prometheus.NewDesc(VendorContextTokensName, "Tokens vendor calls spent, by kind and the context bucket the call fell in.", []string{"account", "model", "context", "kind"}, nil),
		histDesc:    prometheus.NewDesc(VendorCallContextName, "The context each vendor call sent, in tokens.", []string{"account", "model", "agent"}, nil),
	}
}

// accountLabel labels account root CONFIGDIR, resolving each root once.
// Called with mu held.
func (v *vendorSpend) accountLabel(configDir string) string {
	if configDir == "" {
		return vendorspend.AccountUnstated
	}
	if label, ok := v.accounts[configDir]; ok {
		return label
	}
	of := v.accountOf.Load()
	if of == nil {
		v.log.Error(Operation, "vendor spend was folded before the account labeler was set; it is labeled with the unstated account", dlog.Context{
			"config_dir": configDir, "invariant_violation": "the daemon sets the account labeler before any session is watched",
		})
		return vendorspend.AccountUnstated
	}
	label := (*of)(configDir)
	v.accounts[configDir] = label
	return label
}

// validateSpend checks every entry and that no tag set repeats, answering
// the entries by tag.
func validateSpend(entries []*conversationv1.VendorSpend) (map[string]*conversationv1.VendorSpend, error) {
	byTag := make(map[string]*conversationv1.VendorSpend, len(entries))
	for _, e := range entries {
		if err := vendorspend.Validate(e); err != nil {
			return nil, err
		}
		tag := tagOf(e)
		if _, dup := byTag[tag]; dup {
			return nil, fmt.Errorf("%w: the tag set %s appears twice", vendorspend.ErrInvalid, tag)
		}
		byTag[tag] = e
	}
	return byTag, nil
}

// tagOf is an entry's tag set as one key.
func tagOf(e *conversationv1.VendorSpend) string {
	return fmt.Sprintf("%s|%d|%s|%d|%d|%d|%s", e.GetModel(), e.GetAgent(), e.GetSubagentType(), e.GetCause(), e.GetCacheOutcome(), e.GetColdReason(), vendorspend.ContextLabel(e))
}

// countsOf answers an entry's tokens by kind.
func countsOf(e *conversationv1.VendorSpend) kindCounts {
	var out kindCounts
	for i, kind := range vendorspend.Kinds {
		out[i] = float64(vendorspend.TokensOf(e.GetTokens(), kind))
	}
	return out
}

// foldShim folds one shim measurements frame's spend for WS. Within a
// counting run it adds each entry's growth since the run's last frame; a
// frame from a new run adds its entries whole. A frame that breaks the
// contract (an invalid entry, a repeated tag set, a figure that went down
// within one run) is refused whole, recorded once per cause, and leaves the
// folded state as it was.
func (v *vendorSpend) foldShim(ws ids.WorkspaceID, m *shimv1.ShimMeasurements) {
	step := "spend:" + string(ws)
	byTag, err := validateSpend(m.GetVendorSpend())
	if err != nil {
		if v.causes.Failed(step, err.Error()) {
			v.log.Error(Operation, "a shim's vendor spend was refused; it is not folded until the shim sends a valid frame", dlog.Context{
				"workspace_id": string(ws), "counted_since_unix_ms": m.GetCountedSinceUnixMs(), "error": err.Error(),
			})
		}
		return
	}
	v.mu.Lock()
	defer v.mu.Unlock()
	prev := v.runs[ws]
	sameRun := prev != nil && prev.since == m.GetCountedSinceUnixMs()
	if sameRun {
		if err := spendRegressed(prev, byTag); err != nil {
			if v.causes.Failed(step, err.Error()) {
				v.log.Error(Operation, "a shim's vendor spend went backwards within one counting run; the frame is refused", dlog.Context{
					"workspace_id": string(ws), "counted_since_unix_ms": m.GetCountedSinceUnixMs(), "error": err.Error(),
				})
			}
			return
		}
	}
	if v.causes.Succeeded(step) {
		v.log.Info(Operation, "a shim's vendor spend is folded again", dlog.Context{"workspace_id": string(ws)})
	}
	run := &spendRun{since: m.GetCountedSinceUnixMs(), entries: byTag}
	if sameRun && prev.account != "" {
		run.account = prev.account
	} else {
		run.account = v.accountLabel(m.GetConfigDir())
	}
	for tag, e := range byTag {
		calls, counts := float64(e.GetCalls()), countsOf(e)
		if sameRun {
			if before := prev.entries[tag]; before != nil {
				calls -= float64(before.GetCalls())
				was := countsOf(before)
				for i := range counts {
					counts[i] -= was[i]
				}
			}
		}
		v.add(ws, run.account, e, calls, counts)
	}
	if sameRun {
		// An entry the frame left out keeps the run's last figures for it.
		for tag, e := range prev.entries {
			if _, ok := run.entries[tag]; !ok {
				run.entries[tag] = e
			}
		}
	}
	v.runs[ws] = run
}

// spendRegressed answers an error when any entry of PREV is missing from or
// greater than the frame's: a counting run's figures only grow.
func spendRegressed(prev *spendRun, byTag map[string]*conversationv1.VendorSpend) error {
	for tag, before := range prev.entries {
		now, ok := byTag[tag]
		if !ok {
			continue
		}
		if now.GetCalls() < before.GetCalls() {
			return fmt.Errorf("the calls of %s went from %d to %d", tag, before.GetCalls(), now.GetCalls())
		}
		was, is := countsOf(before), countsOf(now)
		for i, kind := range vendorspend.Kinds {
			if is[i] < was[i] {
				return fmt.Errorf("the %s tokens of %s went from %.0f to %.0f", kind, tag, was[i], is[i])
			}
		}
	}
	return nil
}

// observe folds entries a producer reported once (Emacs's run, a headless
// run) for WS, billed to CONFIGDIR: each is added whole. An invalid report is
// refused whole and answered to the caller, who records it.
func (v *vendorSpend) observe(ws ids.WorkspaceID, configDir string, entries []*conversationv1.VendorSpend) error {
	if len(entries) == 0 {
		return fmt.Errorf("%w: a report carries no entry", vendorspend.ErrInvalid)
	}
	if _, err := validateSpend(entries); err != nil {
		return err
	}
	v.mu.Lock()
	defer v.mu.Unlock()
	account := v.accountLabel(configDir)
	for _, e := range entries {
		v.add(ws, account, e, float64(e.GetCalls()), countsOf(e))
	}
	return nil
}

// add folds CALLS more calls of entry E's kind, spending COUNTS, for WS.
// Called with mu held.
func (v *vendorSpend) add(ws ids.WorkspaceID, account string, e *conversationv1.VendorSpend, calls float64, counts kindCounts) {
	agent := vendorspend.AgentLabel(e.GetAgent())
	key := spendKey{
		ws: ws, account: account, model: e.GetModel(), agent: agent, subagentType: e.GetSubagentType(),
		cause: vendorspend.CauseLabel(e.GetCause()), outcome: vendorspend.OutcomeLabel(e.GetCacheOutcome()), reason: vendorspend.ReasonLabel(e.GetColdReason()),
	}
	v.calls[key] += calls
	tokens := v.tokens[key]
	if tokens == nil {
		tokens = &kindCounts{}
		v.tokens[key] = tokens
	}
	context := vendorspend.ContextLabel(e)
	var input float64
	for i, kind := range vendorspend.Kinds {
		tokens[i] += counts[i]
		v.context[contextKey{account: account, model: e.GetModel(), context: context, kind: kind}] += counts[i]
		if kind != vendorspend.KindOutput {
			input += counts[i]
		}
	}
	hk := contextHistKey{account: account, model: e.GetModel(), agent: agent}
	h := v.contexts[hk]
	if h == nil {
		h = newHistogram(contextBounds)
		v.contexts[hk] = h
	}
	n := uint64(calls)
	for i, bound := range vendorspend.ContextBuckets {
		if e.ContextLeTokens != nil && e.GetContextLeTokens() <= bound {
			h.counts[i] += n
		}
	}
	h.count += n
	h.sum += input
	if e.GetCause() == conversationv1.VendorCallCause_VENDOR_CALL_CAUSE_KEEPALIVE {
		k := v.keepalive[ws]
		if k == nil {
			k = &kindCounts{}
			v.keepalive[ws] = k
		}
		for i := range counts {
			k[i] += counts[i]
		}
	}
}

// keepaliveSpent answers WS's keep-alive spend in this daemon's life.
func (v *vendorSpend) keepaliveSpent(ws ids.WorkspaceID) *conversationv1.VendorTokens {
	v.mu.Lock()
	defer v.mu.Unlock()
	k := v.keepalive[ws]
	if k == nil {
		return &conversationv1.VendorTokens{}
	}
	return &conversationv1.VendorTokens{
		UncachedInput: uint64(k[0]), CacheRead: uint64(k[1]), CacheWrite_5M: uint64(k[2]), CacheWrite_1H: uint64(k[3]), Output: uint64(k[4]),
	}
}

// forget drops every series of WS.
func (v *vendorSpend) forget(ws ids.WorkspaceID) {
	v.mu.Lock()
	defer v.mu.Unlock()
	for key := range v.calls {
		if key.ws == ws {
			delete(v.calls, key)
			delete(v.tokens, key)
		}
	}
	delete(v.runs, ws)
	delete(v.keepalive, ws)
}

// Describe implements prometheus.Collector.
func (v *vendorSpend) Describe(ch chan<- *prometheus.Desc) {
	ch <- v.tokensDesc
	ch <- v.callsDesc
	ch <- v.contextDesc
	ch <- v.histDesc
}

// Collect implements prometheus.Collector.
func (v *vendorSpend) Collect(ch chan<- prometheus.Metric) {
	v.mu.Lock()
	defer v.mu.Unlock()
	keys := make([]spendKey, 0, len(v.calls))
	for k := range v.calls {
		keys = append(keys, k)
	}
	sort.Slice(keys, func(i, j int) bool { return fmt.Sprint(keys[i]) < fmt.Sprint(keys[j]) })
	for _, k := range keys {
		labels := []string{string(k.ws), k.account, k.model, k.agent, k.subagentType, k.cause, k.outcome, k.reason}
		ch <- prometheus.MustNewConstMetric(v.callsDesc, prometheus.CounterValue, v.calls[k], labels...)
		for i, kind := range vendorspend.Kinds {
			ch <- prometheus.MustNewConstMetric(v.tokensDesc, prometheus.CounterValue, v.tokens[k][i], append(labels, kind)...)
		}
	}
	for k, n := range v.context {
		ch <- prometheus.MustNewConstMetric(v.contextDesc, prometheus.CounterValue, n, k.account, k.model, k.context, k.kind)
	}
	for k, h := range v.contexts {
		ch <- h.metric(v.histDesc, k.account, k.model, k.agent)
	}
}

// SetAccountLabeler sets how the spend series label an account root: the
// daemon's account resolver, once it is built. The off surface labels
// nothing.
func (s *Surface) SetAccountLabeler(isMultiRepo func(configDir string) bool) {
	if s == nil {
		return
	}
	label := func(configDir string) string {
		if isMultiRepo(configDir) {
			return vendorspend.AccountMultiRepo
		}
		return vendorspend.AccountDefault
	}
	s.spend.accountOf.Store(&label)
}

// ObserveVendorSpend folds the vendor calls one run made for WS, billed to
// CONFIGDIR (Emacs's report, the daemon's headless runs). An invalid report
// is refused whole with an error wrapping vendorspend.ErrInvalid. The off
// surface records nothing and answers nil.
func (s *Surface) ObserveVendorSpend(ws ids.WorkspaceID, configDir string, entries []*conversationv1.VendorSpend) error {
	if s == nil {
		return nil
	}
	return s.spend.observe(ws, configDir, entries)
}

// KeepaliveSpent answers the tokens WS's keep-alives spent in this daemon's
// life, by kind: what the keeper's run accounting reads at each keep-alive's
// answer. The off surface answers none.
func (s *Surface) KeepaliveSpent(ws ids.WorkspaceID) *conversationv1.VendorTokens {
	if s == nil {
		return &conversationv1.VendorTokens{}
	}
	return s.spend.keepaliveSpent(ws)
}
