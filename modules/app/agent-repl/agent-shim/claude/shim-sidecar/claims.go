// claims.go claims a held SHELL spool from the shim's claim in the store,
// when no transcript line claimed it.
//
// WHY A SECOND SOURCE OF CLAIMS. A spool is read only once a spawning call
// claims it, and the transcript states that claim for most launches: a
// structured `backgroundTaskId`, or the vendor's backgrounding sentence. A
// shell the vendor moved to the background on its own timeout inside a
// subagent carries neither in a form this reader could rely on, so its spool
// stayed held and unread, its `[killed]` terminal was never seen, and the run
// stayed live everywhere downstream (2026-09-28). The vendor's task stream
// states the pairing for every run, and the shim, which reads that stream,
// writes it to the store (store.v1 ShellRunClaim).
//
// THE CLAIM IS THE SAME OBSERVATION A LAUNCH IS. It enters the owner index
// through TaskSpawned exactly as a converter's launch does, so conflict
// handling, held stops and held notifications apply unchanged.
//
// ATTRIBUTION COMES FROM THIS READER'S OWN FILES. The store answers which book
// holds the run's launching call; the workspace and transcript session are
// the ones this reader already resolved for that book's transcript. A claim
// whose book is not on record yet, or whose book's transcript is not
// attributed yet, stays held and is asked again next pass.
package main

import (
	"sort"

	"agentrepl/shim-claude-sidecar/internal/discover"
	"agentrepl/shim-claude-sidecar/internal/logging"
	"agentrepl/shim-claude-sidecar/internal/tail"
)

// bookAttribution is where one book's transcript was attributed.
type bookAttribution struct {
	workspaceDir    string
	workspaceID     string
	claudeSessionID string
}

// rememberBookAttribution records the attribution of a transcript this reader
// started watching, under the book its records land in. Spools and
// unattributed files record nothing.
func (s *sidecar) rememberBookAttribution(target discover.Target, book string) {
	if target.SessionID == "" || book == "" || target.WorkspaceDir == "" || target.WorkspaceID == "" || target.ClaudeSessionID == "" {
		return
	}
	s.attributedBooks[book] = bookAttribution{
		workspaceDir:    target.WorkspaceDir,
		workspaceID:     target.WorkspaceID,
		claudeSessionID: target.ClaudeSessionID,
	}
}

// noteUnclaimedShell remembers a held shell spool no launch claimed, so the
// next pass asks the store for its claim.
func (s *sidecar) noteUnclaimedShell(target discover.Target) {
	if target.Kind != tail.KindShellSpool || target.TaskID == "" {
		return
	}
	s.unclaimedShells[target.TaskID] = target.Path
}

// claimHeldShells asks the store for the claims of every held shell spool and
// observes each claim this reader can attribute. It runs before the pass's
// targets are resolved, so a spool claimed here is read this pass.
func (s *sidecar) claimHeldShells() {
	if len(s.unclaimedShells) == 0 {
		return
	}
	ids := make([]string, 0, len(s.unclaimedShells))
	for id := range s.unclaimedShells {
		ids = append(ids, id)
	}
	sort.Strings(ids)
	ctx, cancel := s.rpcContext()
	defer cancel()
	claims, err := s.store.ShellRunClaims(ctx, ids)
	if err != nil {
		// storeclient owns the causal record with its rpc and refusal detail.
		s.noteStoreErr("shell-run-claims", err)
		return
	}
	for _, claimed := range claims {
		taskID := claimed.GetClaim().GetVendorTaskId()
		run := claimed.GetClaim().GetRun().GetValue()
		owner := claimed.GetOwner().GetValue()
		bound := s.log.With(logging.Context{
			Operation: "shell-run-claim", TaskID: taskID, ActivityID: run, AgentID: owner,
			Path: s.unclaimedShells[taskID],
		})
		if owner == "" {
			bound.LogVerbose("the shim claimed this spool, but its launching call is not on record in any book yet; it stays held")
			continue
		}
		at, ok := s.attributedBooks[owner]
		if !ok {
			bound.LogVerbose("the shim claimed this spool for book %s, whose transcript this reader has not attributed yet; it stays held", owner)
			continue
		}
		bound.Log("the shim's claim names this held spool's run; it is claimed as that run's spawn")
		s.TaskSpawned(taskID, run, owner, "", false, at.workspaceDir, at.workspaceID, at.claudeSessionID)
		delete(s.unclaimedShells, taskID)
	}
}
