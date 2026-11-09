package wsm

import (
	"context"
	"path/filepath"

	"agent-repld/internal/inflight"
	"agent-repld/internal/kernelevent"
)

// Changes implements DB.
//
// A FILE IS FOLLOWED IN THE KERNEL. In WAL mode every commit, this process's
// or another daemon's, appends to the -wal file, and a checkpoint writes the
// database file itself; the directory is followed too, so a -wal file made
// again after the last connection removed it is watched once it exists
// (kernelevent.Follow). A reader's own reads write neither, so a wait never
// wakes itself.
//
// AN IN-MEMORY DATABASE IS THIS HANDLE'S ALONE, so its commits are the ones
// write makes, each signaled by noteCommit.
func (s *store) Changes(ctx context.Context) (<-chan error, func()) {
	if s.keeper == nil {
		return kernelevent.Follow(ctx, CommitPaths(s.path)...)
	}
	return s.followMemoryCommits(ctx)
}

// CommitPaths answers what a commit to the WAL-mode database at PATH changes:
// its directory, the database file, and its -wal file.
func CommitPaths(path string) []string {
	return []string{filepath.Dir(path), path, path + "-wal"}
}

// noteCommit says a write of this handle committed.
func (s *store) noteCommit() {
	s.commitMu.Lock()
	defer s.commitMu.Unlock()
	if s.committed != nil {
		close(s.committed)
	}
	s.committed = make(chan struct{})
}

// nextCommit answers a channel that closes at this handle's next commit.
func (s *store) nextCommit() <-chan struct{} {
	s.commitMu.Lock()
	defer s.commitMu.Unlock()
	if s.committed == nil {
		s.committed = make(chan struct{})
	}
	return s.committed
}

// followMemoryCommits is Changes for an in-memory database.
func (s *store) followMemoryCommits(ctx context.Context) (<-chan error, func()) {
	ctx, cancel := context.WithCancel(ctx)
	out := make(chan error, 1)
	done := make(chan struct{})
	// TAKEN BEFORE Changes RETURNS, so a commit after the return is seen.
	next := s.nextCommit()
	inflight.Standing(func() {
		defer close(done)
		for {
			select {
			case <-ctx.Done():
				return
			case <-next:
			}
			next = s.nextCommit()
			select {
			case out <- nil:
			default:
				// A commit is already waiting to be read: this one is in it.
			}
		}
	})
	return out, func() {
		cancel()
		<-done
	}
}
