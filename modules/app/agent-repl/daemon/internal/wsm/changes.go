package wsm

import (
	"context"
	"fmt"
	"os"
	"path/filepath"

	"agent-repld/internal/dlog"
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
// THE -wal WRITES COME BEFORE THE COMMIT CAN BE READ. SQLite appends a
// commit's frames to the -wal file and only then publishes them in the
// mmapped -shm index, a step that raises no file event. A follower woken by
// the last frame could read data_version before the commit was visible, see
// nothing new, and then get no further event: another daemon's selection went
// unseen until some unrelated commit woke it (TestASelectOnTheSuccessor-
// ReachesTheIncumbentsRosterMidHandover, 30s, under a loaded merge gate). So
// every write that commits also writes the -commits signal file once the
// commit has returned (signalCommit). The last event a commit raises therefore
// comes after the commit is readable, and any read it wakes sees the commit.
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
// its directory, the database file, its -wal file, and the commit signal
// written after every commit (CommitSignalPath).
func CommitPaths(path string) []string {
	return []string{filepath.Dir(path), path, path + "-wal", CommitSignalPath(path)}
}

// CommitSignalPath is the file every committed write of the database at PATH
// writes once the commit is readable (signalCommit).
func CommitSignalPath(path string) string {
	return path + "-commits"
}

// opSignalCommit names the commit signal's failure record.
const opSignalCommit = "daemon.wsm.signal_commit"

// signalCommit writes the commit signal of a file database. The commit has
// already landed, so a failure is not the write's: it is recorded at ERROR,
// because another daemon following this file may miss the commit until the
// next one.
func (s *store) signalCommit() {
	if s.keeper != nil {
		return
	}
	path := CommitSignalPath(s.path)
	if err := writeCommitSignal(path); err != nil {
		s.log.Error(opSignalCommit, "could not write the commit signal; a daemon following this file may miss the commit until the next one",
			withError(dlog.Context{"path": path}, err))
	}
}

// writeCommitSignal writes one byte at the start of the signal file at PATH,
// creating it if needed. The file never grows: only the write's event matters.
func writeCommitSignal(path string) error {
	f, err := os.OpenFile(path, os.O_WRONLY|os.O_CREATE, 0o600)
	if err != nil {
		return fmt.Errorf("wsm: open the commit signal %s: %w", path, err)
	}
	_, werr := f.WriteAt([]byte{1}, 0)
	cerr := f.Close()
	if werr != nil {
		return fmt.Errorf("wsm: write the commit signal %s: %w", path, werr)
	}
	if cerr != nil {
		return fmt.Errorf("wsm: close the commit signal %s: %w", path, cerr)
	}
	return nil
}

// noteCommit says a write of this handle committed.
func (s *store) noteCommit() {
	s.signalCommit()
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
