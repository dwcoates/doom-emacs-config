package main

import (
	"fmt"
	"os"
	"path/filepath"
	"time"

	"claude-repld/internal/clock"
	"claude-repld/internal/dlog"
	"claude-repld/internal/ids"
	"claude-repld/internal/worktreereap"
)

// envWorktreeReapIdle is the landed-worktree reaper's idle threshold: how long
// a worktree must have shown no activity before it is eligible. It is an
// OPERATOR knob, a Go duration; unset is worktreereap.DefaultIdleAfter.
const envWorktreeReapIdle = "AGENT_REPL_WORKTREE_REAP_IDLE"

// envWorktreeReapExpire is the reaper's expiry: how long a worktree that never
// lands may exist, over unbroken repository activity, before it is preserved
// and removed. It is an OPERATOR knob, a Go duration; unset is
// worktreereap.DefaultExpireAfter.
const envWorktreeReapExpire = "AGENT_REPL_WORKTREE_REAP_EXPIRE"

// envWorktreeReapStartDelay and envWorktreeReapEvery compress the reaper's
// schedule for tests. A suite that must observe a sweep cannot wait five
// minutes for the first one, nor a day for the next.
const (
	envWorktreeReapStartDelay = "AGENT_REPL_WORKTREE_REAP_START_DELAY"
	envWorktreeReapEvery      = "AGENT_REPL_WORKTREE_REAP_EVERY"
)

// worktreeReapLockName is the sweep's cross-process lock, in the kernel-lock
// directory: an incumbent and its handover successor never sweep at once.
const worktreeReapLockName = "worktree-reap.lock"

// resolveWorktreeReapIdle reads the idle threshold's override. Empty is the
// default; a malformed or non-positive value is a REFUSAL.
func resolveWorktreeReapIdle(value string) (time.Duration, error) {
	return resolveDurationKnob(envWorktreeReapIdle, value, worktreereap.DefaultIdleAfter)
}

// resolveWorktreeReapExpire reads the expiry's override.
func resolveWorktreeReapExpire(value string) (time.Duration, error) {
	return resolveDurationKnob(envWorktreeReapExpire, value, worktreereap.DefaultExpireAfter)
}

// resolveWorktreeReapStartDelay reads the start delay's override.
func resolveWorktreeReapStartDelay(value string) (time.Duration, error) {
	return resolveDurationKnob(envWorktreeReapStartDelay, value, worktreereap.DefaultStartDelay)
}

// resolveWorktreeReapEvery reads the cadence's override.
func resolveWorktreeReapEvery(value string) (time.Duration, error) {
	return resolveDurationKnob(envWorktreeReapEvery, value, worktreereap.DefaultEvery)
}

// buildWorktreeReaper builds the landed-worktree reaper from its four knobs.
// A refused knob is a BOOT FATAL, recorded here.
func buildWorktreeReaper(git worktreereap.Git, registry worktreereap.Registry, live func() []ids.WorkspaceID, runDir string, log dlog.Logger) (*worktreereap.Reaper, error) {
	var (
		idle, expire, start, every time.Duration
		err                        error
	)
	if idle, err = resolveWorktreeReapIdle(os.Getenv(envWorktreeReapIdle)); err != nil {
		return nil, refusedWindow(log, err)
	}
	if expire, err = resolveWorktreeReapExpire(os.Getenv(envWorktreeReapExpire)); err != nil {
		return nil, refusedWindow(log, err)
	}
	if start, err = resolveWorktreeReapStartDelay(os.Getenv(envWorktreeReapStartDelay)); err != nil {
		return nil, refusedWindow(log, err)
	}
	if every, err = resolveWorktreeReapEvery(os.Getenv(envWorktreeReapEvery)); err != nil {
		return nil, refusedWindow(log, err)
	}
	reaper, err := worktreereap.New(worktreereap.Deps{
		Git:          git,
		Registry:     registry,
		LiveSessions: live,
		Clock:        clock.System{},
		LockPath:     filepath.Join(runDir, worktreeReapLockName),
		IdleAfter:    idle,
		ExpireAfter:  expire,
		StartDelay:   start,
		Every:        every,
		Log:          log,
	})
	if err != nil {
		return nil, fmt.Errorf("claude-repld: build the landed-worktree reaper: %w", err)
	}
	log.Debug(graphOperation, "the landed-worktree reaper is built", dlog.Context{
		"idle_after": idle.String(), "expire_after": expire.String(), "start_delay": start.String(), "every": every.String(),
	})
	return reaper, nil
}

// refusedWindow records a refused reaper knob.
func refusedWindow(log dlog.Logger, err error) error {
	log.Error(graphOperation, "a landed-worktree reaper window was refused", dlog.Context{"cause": err.Error()})
	return err
}
