package feed

import (
	"context"
	"runtime"
	"testing"
	"time"

	conversationv1 "agentrepl/proto/conversation/v1"

	"agent-repld/internal/ids"
	"agent-repld/internal/pullrefs"
	"agent-repld/internal/wsm"
)

// THE RESOLVER NEVER HOLDS ITS MUTEX ACROSS A STATE-DATABASE CALL, nor across
// a read through a shim (storewrites.go). guardStores pins it for every test
// that builds a resolver through the harness: each database-backed dependency
// is wrapped so a call made while r.mu is held fails the test, naming the
// dependency and its caller.

// storeGuard fails T when R's mutex is held at a store call.
type storeGuard struct {
	t testing.TB
	r *resolver
}

// check fails the test when the mutex is held at the call named WHAT.
func (g storeGuard) check(what string) {
	if !g.r.mu.Held() {
		return
	}
	caller := "unknown"
	if pc, _, line, ok := runtime.Caller(2); ok {
		caller = functionAt(pc) + ":" + itoa(line)
	}
	g.t.Errorf("%s was called while the resolver's mutex was held (from %s)", what, caller)
}

func itoa(n int) string {
	if n == 0 {
		return "0"
	}
	var digits []byte
	for n > 0 {
		digits = append([]byte{byte('0' + n%10)}, digits...)
		n /= 10
	}
	return string(digits)
}

// guardStores wraps every store R reaches so a call under its mutex fails T.
func guardStores(t testing.TB, r *resolver) {
	g := storeGuard{t: t, r: r}
	d := &r.deps
	if f := d.WorkspaceDir; f != nil {
		d.WorkspaceDir = func(ws ids.WorkspaceID) (string, error) { g.check("WorkspaceDir"); return f(ws) }
	}
	if f := d.WorkspaceName; f != nil {
		d.WorkspaceName = func(ws ids.WorkspaceID) (string, error) { g.check("WorkspaceName"); return f(ws) }
	}
	d.TurnCloses = guardedRead(g, "TurnCloses", d.TurnCloses)
	d.TurnAddresses = guardedRead(g, "TurnAddresses", d.TurnAddresses)
	d.TurnMirrors = guardedRead(g, "TurnMirrors", d.TurnMirrors)
	d.TurnSenders = guardedRead(g, "TurnSenders", d.TurnSenders)
	if d.DurableRows != nil {
		d.DurableRows = guardedDurableRows{g, d.DurableRows}
	}
	if d.RolledBack != nil {
		d.RolledBack = guardedTurnSet{g, "RolledBack", d.RolledBack.RecordRolledBackTurns, d.RolledBack.RolledBackTurns}
	}
	if d.SignedIn != nil {
		d.SignedIn = guardedTurnSet{g, "SignedIn", d.SignedIn.RecordSignedInTurns, d.SignedIn.SignedInTurns}
	}
	if d.Reconnected != nil {
		d.Reconnected = guardedTurnSet{g, "Reconnected", d.Reconnected.RecordReconnectedTurns, d.Reconnected.ReconnectedTurns}
	}
	if d.PullReferences != nil {
		d.PullReferences = guardedPullReferences{g, d.PullReferences}
	}
	if d.Faults != nil {
		d.Faults = guardedFaults{g, d.Faults}
	}
	if d.ResponseLinks != nil {
		d.ResponseLinks = guardedResponseLinks{g, d.ResponseLinks}
	}
	if d.History != nil {
		d.History = guardedHistory{g, d.History}
	}
	if d.PeerDeliveries != nil {
		d.PeerDeliveries = guardedPeerDeliveries{g, d.PeerDeliveries}
	}
	if d.VendorTasks != nil {
		d.VendorTasks = guardedVendorTasks{g, d.VendorTasks}
	}
}

func guardedRead[T any](g storeGuard, what string, f func(context.Context, ids.WorkspaceID, []ids.TurnID) (map[ids.TurnID]T, error)) func(context.Context, ids.WorkspaceID, []ids.TurnID) (map[ids.TurnID]T, error) {
	if f == nil {
		return nil
	}
	return func(ctx context.Context, ws ids.WorkspaceID, turns []ids.TurnID) (map[ids.TurnID]T, error) {
		g.check(what)
		return f(ctx, ws, turns)
	}
}

type guardedDurableRows struct {
	g     storeGuard
	inner DurableRows
}

func (d guardedDurableRows) PutDurableFeedRow(ctx context.Context, row wsm.DurableFeedRow) error {
	d.g.check("PutDurableFeedRow")
	return d.inner.PutDurableFeedRow(ctx, row)
}

func (d guardedDurableRows) DurableFeedRows(ctx context.Context, id ids.WorkspaceID) ([]wsm.DurableFeedRow, error) {
	d.g.check("DurableFeedRows")
	return d.inner.DurableFeedRows(ctx, id)
}

func (d guardedDurableRows) ClearDurableFeedRows(ctx context.Context, id ids.WorkspaceID) error {
	d.g.check("ClearDurableFeedRows")
	return d.inner.ClearDurableFeedRows(ctx, id)
}

func (d guardedDurableRows) DurableFeedRowRank(ctx context.Context, id ids.WorkspaceID, rowID string) (int, string, bool, error) {
	d.g.check("DurableFeedRowRank")
	return d.inner.DurableFeedRowRank(ctx, id, rowID)
}

type guardedTurnSet struct {
	g      storeGuard
	what   string
	record func(context.Context, ids.WorkspaceID, []ids.TurnID) error
	load   func(context.Context, ids.WorkspaceID) ([]ids.TurnID, error)
}

func (s guardedTurnSet) record_(ctx context.Context, id ids.WorkspaceID, turns []ids.TurnID) error {
	s.g.check(s.what + " record")
	return s.record(ctx, id, turns)
}

func (s guardedTurnSet) load_(ctx context.Context, id ids.WorkspaceID) ([]ids.TurnID, error) {
	s.g.check(s.what + " load")
	return s.load(ctx, id)
}

func (s guardedTurnSet) RecordRolledBackTurns(ctx context.Context, id ids.WorkspaceID, turns []ids.TurnID) error {
	return s.record_(ctx, id, turns)
}
func (s guardedTurnSet) RolledBackTurns(ctx context.Context, id ids.WorkspaceID) ([]ids.TurnID, error) {
	return s.load_(ctx, id)
}
func (s guardedTurnSet) RecordSignedInTurns(ctx context.Context, id ids.WorkspaceID, turns []ids.TurnID) error {
	return s.record_(ctx, id, turns)
}
func (s guardedTurnSet) SignedInTurns(ctx context.Context, id ids.WorkspaceID) ([]ids.TurnID, error) {
	return s.load_(ctx, id)
}
func (s guardedTurnSet) RecordReconnectedTurns(ctx context.Context, id ids.WorkspaceID, turns []ids.TurnID) error {
	return s.record_(ctx, id, turns)
}
func (s guardedTurnSet) ReconnectedTurns(ctx context.Context, id ids.WorkspaceID) ([]ids.TurnID, error) {
	return s.load_(ctx, id)
}

type guardedPullReferences struct {
	g     storeGuard
	inner PullReferenceStore
}

func (p guardedPullReferences) RecordPullReference(ctx context.Context, id ids.WorkspaceID, ref pullrefs.Ref, at time.Time) error {
	p.g.check("RecordPullReference")
	return p.inner.RecordPullReference(ctx, id, ref, at)
}

func (p guardedPullReferences) PullReferences(ctx context.Context, id ids.WorkspaceID) ([]pullrefs.Ref, error) {
	p.g.check("PullReferences")
	return p.inner.PullReferences(ctx, id)
}

type guardedFaults struct {
	g     storeGuard
	inner FaultRecorder
}

func (f guardedFaults) OpenFault(ctx context.Context, fault wsm.Fault) (ids.FaultID, error) {
	f.g.check("OpenFault")
	return f.inner.OpenFault(ctx, fault)
}

func (f guardedFaults) CloseFault(ctx context.Context, id ids.FaultID, at time.Time) error {
	f.g.check("CloseFault")
	return f.inner.CloseFault(ctx, id, at)
}

func (f guardedFaults) OpenFaults(ctx context.Context, scope wsm.FaultScope) ([]wsm.Fault, error) {
	f.g.check("OpenFaults")
	return f.inner.OpenFaults(ctx, scope)
}

func (f guardedFaults) FaultRecorded(ctx context.Context, m wsm.FaultMatch) (bool, error) {
	f.g.check("FaultRecorded")
	return f.inner.FaultRecorded(ctx, m)
}

type guardedResponseLinks struct {
	g     storeGuard
	inner ResponseLinkSink
}

func (l guardedResponseLinks) AddResponseLinks(ctx context.Context, ws ids.WorkspaceID, urls []string) error {
	l.g.check("AddResponseLinks")
	return l.inner.AddResponseLinks(ctx, ws, urls)
}

type guardedHistory struct {
	g     storeGuard
	inner HistorySource
}

func (h guardedHistory) ReadHistory(ctx context.Context, ws ids.WorkspaceID, target *conversationv1.AgentId, after *conversationv1.HistoryPointer) (*conversationv1.HistoryPage, error) {
	h.g.check("ReadHistory")
	return h.inner.ReadHistory(ctx, ws, target, after)
}

func (h guardedHistory) ReadInherited(ctx context.Context, ws ids.WorkspaceID, book *conversationv1.AgentId, after *conversationv1.HistoryPointer, throughMs int64) (*conversationv1.HistoryPage, error) {
	h.g.check("ReadInherited")
	return h.inner.ReadInherited(ctx, ws, book, after, throughMs)
}

type guardedPeerDeliveries struct {
	g     storeGuard
	inner PeerDeliverySource
}

func (p guardedPeerDeliveries) ResolvePeerDelivery(ctx context.Context, ws ids.WorkspaceID, id string) (PeerDeliveryEnds, error) {
	p.g.check("ResolvePeerDelivery")
	return p.inner.ResolvePeerDelivery(ctx, ws, id)
}

type guardedVendorTasks struct {
	g     storeGuard
	inner VendorTaskSource
}

func (v guardedVendorTasks) ResolveVendorTask(ctx context.Context, ws ids.WorkspaceID, session, id string) (VendorTaskAgent, error) {
	v.g.check("ResolveVendorTask")
	return v.inner.ResolveVendorTask(ctx, ws, session, id)
}
