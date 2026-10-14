package merge

import (
	"context"
	"errors"
	"sort"

	"agent-repld/internal/cpugov"
	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/wsm"
)

// This file is the queue's answer to the host's POWER SOURCE (owner ruling,
// 2026-10-14): on AC power a queue works its top two merges, which race to
// land and take turns at the tests; on battery it works its head alone
// (cpugov's window). The operator's RACING SWITCH (racing.go) narrows the
// window to the head alone on any power source, and a flip of it is answered
// exactly as plugging in or unplugging is.
//
// PLUGGING IN starts the merge behind every head at once (extendChain).
// UNPLUGGING suspends every merge beyond the window at the step it stands on,
// exactly as a pause suspends a merge (suspendFor), and takes it straight back
// from its durable rows (AdoptWorkspace): it resumes there once it heads its
// queue, or once the host is plugged in again and the window has room.

// errOnBattery is the cause a merge beyond the battery window is suspended
// with.
var errOnBattery = errors.New("merge: the host went on battery; only the queue's head is worked on, and this merge is suspended until it heads the queue or the host is plugged in")

// suspendBattery is the suspension a merge beyond the battery window takes.
var suspendBattery = suspension{op: "daemon.merge.power", cause: errOnBattery,
	moved: "a merge beyond the battery window is suspended at the step it recorded; it resumes there once it heads its queue or the host is plugged in"}

// errRacingDisabled is the cause a merge beyond the head is suspended with
// when the operator disabled racing.
var errRacingDisabled = errors.New("merge: the operator disabled racing; only the queue's head is worked on, and this merge is suspended until it heads the queue or racing is enabled")

// suspendRacingDisabled is the suspension a merge beyond the head takes when
// the operator disabled racing.
var suspendRacingDisabled = suspension{op: "daemon.merge.power", cause: errRacingDisabled,
	moved: "a merge beyond the head is suspended at the step it recorded, racing being disabled; it resumes there once it heads its queue or racing is enabled"}

// suspensionFor is the suspension a merge beyond the window mode allows
// takes: the operator's when racing is disabled, else the battery's.
func suspensionFor(mode cpugov.Mode) suspension {
	if mode.RacingDisabled {
		return suspendRacingDisabled
	}
	return suspendBattery
}

// modeChanged is the governor's report of a new mode: a new power source, or
// a flip of the racing switch.
func (o *orchestrator) modeChanged(mode cpugov.Mode) {
	const op = "daemon.merge.power"
	ctx := context.Background()
	o.deps.Log.Global().Info(op, "the merge queue's mode changed; the queues work as many merges as its window holds", dlog.Context{
		"power": mode.Power.String(), "racing_disabled": mode.RacingDisabled, "window": o.queueWindow()})
	for _, repo := range o.chainRepos() {
		o.extendChain(ctx, repo)
	}
	for _, ws := range o.beyondWindow() {
		if err := o.suspendFor(ctx, ws, suspensionFor(mode)); err != nil {
			o.deps.Log.Global().Error(op, "could not suspend a merge beyond the window", dlog.Context{
				"workspace": string(ws), "racing_disabled": mode.RacingDisabled, "error": err.Error()})
			continue
		}
		if err := o.AdoptWorkspace(ctx, ws); err != nil {
			o.deps.Log.Global().Error(op, "could not take back a merge suspended beyond the window", dlog.Context{
				"workspace": string(ws), "racing_disabled": mode.RacingDisabled, "error": err.Error()})
		}
	}
}

// chainRepos answers every repository with a chain, in a stable order.
func (o *orchestrator) chainRepos() []wsm.RepoKey {
	o.mu.Lock()
	defer o.mu.Unlock()
	repos := make([]wsm.RepoKey, 0, len(o.chains))
	for repo := range o.chains {
		repos = append(repos, repo)
	}
	sort.Slice(repos, func(i, j int) bool { return repos[i] < repos[j] })
	return repos
}

// beyondWindow answers the workspace of every chain member the window in
// force no longer holds, in a stable order.
func (o *orchestrator) beyondWindow() []ids.WorkspaceID {
	window := o.queueWindow()
	o.mu.Lock()
	defer o.mu.Unlock()
	var out []ids.WorkspaceID
	for _, chain := range o.chains {
		for i, member := range chain {
			if i >= window {
				out = append(out, member.ws)
			}
		}
	}
	sort.Slice(out, func(i, j int) bool { return out[i] < out[j] })
	return out
}
