package gitclient

import (
	"context"
	"fmt"

	"agent-repld/internal/dlog"
)

// MoveWorktree moves the worktree at FROM to TO with `git worktree move`,
// for a workspace's rename (agentrepl.v1.SetWorkspaceName). It is IDEMPOTENT
// so a move a crash interrupted is finished by running it again: a FROM that
// is gone with TO present is a move already made, and only the log sinks are
// attached to TO.
//
// THE LOG SINKS LET GO OF FROM BEFORE GIT MOVES IT, as a removal's do, so no
// sink of the old path creates anything there once the tree is gone; TO is
// attached once the tree stands there. A move git refuses leaves the tree
// whole at FROM, which is attached again, and is answered as the failure.
func (c *client) MoveWorktree(ctx context.Context, repoDir, from, to string) error {
	const operation = "daemon.gitclient.move_worktree"
	log := c.forWorktree(from)
	fields := dlog.Context{"dir": repoDir, "from": from, "to": to}
	fromPresent, err := pathPresent(from)
	if err != nil {
		log.Error(operation, "could not tell whether the worktree to move is present", dlog.WithCause(fields, err))
		return err
	}
	toPresent, err := pathPresent(to)
	if err != nil {
		log.Error(operation, "could not tell whether the worktree's destination is present", dlog.WithCause(fields, err))
		return err
	}
	switch {
	case !fromPresent && toPresent:
		log.Info(operation, "the worktree already stands at its destination; the move was made before", fields)
		return c.attachMoved(operation, to, fields)
	case !fromPresent:
		err := fmt.Errorf("gitclient: move %s to %s: neither the worktree nor its destination exists", from, to)
		log.Error(operation, "the worktree to move is gone and nothing stands at its destination", dlog.WithCause(fields, err))
		return err
	case toPresent:
		err := fmt.Errorf("gitclient: move %s to %s: the destination already exists", from, to)
		log.Error(operation, "the worktree's destination already exists; nothing is moved", dlog.WithCause(fields, err))
		return err
	}
	if err := c.log.DetachDir(from); err != nil {
		log.Error(operation, "the worktree could not be detached from its log sinks; it is not moved", dlog.WithCause(fields, err))
		return fmt.Errorf("gitclient: detach the log sinks of %s: %w", from, err)
	}
	in, err := c.runRaw(ctx, operation, repoDir, "worktree", "move", from, to)
	if err == nil && in.exitCode != 0 {
		err = in.fail()
		log.Error(operation, "git would not move the worktree; it stays whole at its path", dlog.WithCause(fields, err))
	}
	if err != nil {
		if attachErr := c.log.AttachDir(from); attachErr != nil {
			log.Error(operation, "the unmoved worktree could not be re-attached to its log sinks", dlog.WithCause(fields, attachErr))
			return fmt.Errorf("gitclient: move %s: %w; re-attach: %w", from, err, attachErr)
		}
		return err
	}
	return c.attachMoved(operation, to, fields)
}

// attachMoved attaches the moved worktree at TO to its log sinks.
func (c *client) attachMoved(operation, to string, fields dlog.Context) error {
	if err := c.log.AttachDir(to); err != nil {
		c.forWorktree(to).Error(operation, "the moved worktree could not be attached to its log sinks", dlog.WithCause(fields, err))
		return fmt.Errorf("gitclient: attach the log sinks of %s: %w", to, err)
	}
	return nil
}

// RenameBranch renames the local branch FROM to TO with `git branch -m`,
// which also moves the branch's reflog and its config and repoints every
// worktree that has it checked out. It is IDEMPOTENT: a FROM that is gone
// with TO present is a rename already made. A TO that already exists while
// FROM does too is refused, and nothing is renamed.
func (c *client) RenameBranch(ctx context.Context, repoDir, from, to string) error {
	const operation = "daemon.gitclient.rename_branch"
	log := c.log.Global()
	fields := dlog.Context{"dir": repoDir, "from": from, "to": to}
	fromExists, err := c.branchExists(ctx, operation, repoDir, from)
	if err != nil {
		return err
	}
	toExists, err := c.branchExists(ctx, operation, repoDir, to)
	if err != nil {
		return err
	}
	switch {
	case !fromExists && toExists:
		log.Info(operation, "the branch already has its new name; the rename was made before", fields)
		return nil
	case !fromExists:
		err := fmt.Errorf("gitclient: rename %s to %s: neither branch exists", from, to)
		log.Error(operation, "the branch to rename is gone and no branch has its new name", dlog.WithCause(fields, err))
		return err
	case toExists:
		err := fmt.Errorf("gitclient: rename %s to %s: the new name is already a branch", from, to)
		log.Error(operation, "a branch already has the new name; nothing is renamed", dlog.WithCause(fields, err))
		return err
	}
	in, err := c.runRaw(ctx, operation, repoDir, "branch", "-m", from, to)
	if err != nil {
		return err
	}
	if in.exitCode != 0 {
		failure := in.fail()
		log.Error(operation, "git would not rename the branch", dlog.WithCause(fields, failure))
		return failure
	}
	return nil
}
