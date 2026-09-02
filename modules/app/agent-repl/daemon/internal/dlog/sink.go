package dlog

import (
	"errors"
	"fmt"
	"os"
	"path/filepath"
	"sync"
)

// CapBytes is the per-workspace durable sink cap from logging-contract.md.
const CapBytes = 64 << 20

// SinkNames are the four per-workspace sinks the daemon manages. emacs.log is
// absent on purpose: Emacs owns it, and the daemon never creates, replaces or
// writes it.
var SinkNames = []string{"daemon", "shim", "webapp", "sidecar"}

// linkDirRel is the canonical per-workspace log directory, relative to the
// workspace.
var linkDirRel = filepath.Join(".claude", "emacs")

// ErrPoisoned reports that a sink stopped accepting records because its cap
// maintenance or a write failed. It is workspace-attributed and never
// silently downgraded into a global write.
var ErrPoisoned = errors.New("log sink poisoned")

// sink is one workspace-owned durable JSONL file: a canonical symlink inside
// the workspace pointing at a unique target the daemon created under the state
// root's logs directory, plus the append-mode descriptor on that target.
//
// The daemon never follows a workspace-provided regular file or foreign
// symlink: it always creates its own target and atomically replaces the link,
// so whatever the workspace had there is displaced rather than written to.
type sink struct {
	// name is one of SinkNames.
	name string
	// workspaceDir and workspaceID attribute every failure this sink reports.
	workspaceDir string
	workspaceID  string
	// link is the canonical symlink path inside the workspace.
	link string
	// target is the daemon-owned file the link names.
	target string

	mu     sync.Mutex
	f      *os.File
	size   int64
	poison error
}

// openSink creates this runtime's unique target for one workspace sink, opens
// it with append semantics, and atomically replaces the canonical symlink so
// it names that target.
// A target already minted for this workspace sink in THIS runtime is REUSED
// (target != ""): a sink evicted on close and re-opened by the next
// workspace-bound record must go on appending to the same file, or the
// workspace's whole log narrative would be replaced by whatever came after the
// eviction.
func openSink(logsDir, workspaceDir, workspaceID, name, target string) (*sink, error) {
	linkDir := filepath.Join(workspaceDir, linkDirRel)
	if err := os.MkdirAll(linkDir, 0o755); err != nil {
		return nil, fmt.Errorf("create log directory %q: %w", linkDir, err)
	}
	if target == "" {
		minted, err := createTarget(logsDir, workspaceID, name)
		if err != nil {
			return nil, err
		}
		target = minted
	}
	f, err := os.OpenFile(target, os.O_APPEND|os.O_WRONLY, 0o600)
	if err != nil {
		return nil, fmt.Errorf("open log target %q for append: %w", target, err)
	}
	info, err := f.Stat()
	if err != nil {
		f.Close()
		return nil, fmt.Errorf("stat log target %q: %w", target, err)
	}
	s := &sink{
		name:         name,
		workspaceDir: workspaceDir,
		workspaceID:  workspaceID,
		link:         filepath.Join(linkDir, name+".log"),
		target:       target,
		f:            f,
		size:         info.Size(),
	}
	if err := replaceLink(s.link, target); err != nil {
		f.Close()
		return nil, err
	}
	return s, nil
}

// createTarget makes a fresh, uniquely named target under the STATE ROOT's
// logs directory, per ARCHITECTURE.md's "State root layout". A restart never
// trusts the previous run's destination, so this is called once per sink per
// runtime lifetime and the result is remembered in memory.
//
// IT IS NOT THE OS TEMP DIR. A durable log a person is asked to read must not
// live where the operating system may sweep it, must not be scattered across a
// TMPDIR that differs per launcher, and must not accumulate one orphan per run
// in a directory nothing owns.
func createTarget(logsDir, workspaceID, name string) (string, error) {
	if logsDir == "" {
		return "", fmt.Errorf("create log target for %s.log: no logs directory was resolved", name)
	}
	if err := os.MkdirAll(logsDir, 0o755); err != nil {
		return "", fmt.Errorf("create the logs directory %q: %w", logsDir, err)
	}
	f, err := os.CreateTemp(logsDir, "agent-repl-"+workspaceID+"-"+name+"-*.log")
	if err != nil {
		return "", fmt.Errorf("create log target for %s.log: %w", name, err)
	}
	target := f.Name()
	if err := f.Close(); err != nil {
		return "", fmt.Errorf("close freshly created log target %q: %w", target, err)
	}
	return target, nil
}

// replaceLink points link at target atomically: a symlink is made at a
// temporary name in the same directory and renamed onto the canonical path, so
// a reader either sees the old link or the new one and never an absent path.
func replaceLink(link, target string) error {
	dir := filepath.Dir(link)
	tmp, err := os.CreateTemp(dir, "."+filepath.Base(link)+".*")
	if err != nil {
		return fmt.Errorf("create temporary link name beside %q: %w", link, err)
	}
	tmpName := tmp.Name()
	if err := tmp.Close(); err != nil {
		return fmt.Errorf("close temporary link placeholder %q: %w", tmpName, err)
	}
	// os.Symlink refuses an existing path, so the placeholder goes first.
	if err := os.Remove(tmpName); err != nil {
		return fmt.Errorf("remove temporary link placeholder %q: %w", tmpName, err)
	}
	if err := os.Symlink(target, tmpName); err != nil {
		return fmt.Errorf("link %q -> %q: %w", tmpName, target, err)
	}
	if err := os.Rename(tmpName, link); err != nil {
		os.Remove(tmpName)
		return fmt.Errorf("atomically replace %q with the link to %q: %w", link, target, err)
	}
	return nil
}

// write appends one already-rendered JSONL line, enforcing the cap
// synchronously before the write. A poisoned sink refuses every further
// record with the failure that poisoned it.
func (s *sink) write(line []byte) error {
	s.mu.Lock()
	defer s.mu.Unlock()
	if s.poison != nil {
		return s.poison
	}
	if s.size+int64(len(line)) > CapBytes {
		if err := s.maintainLocked(); err != nil {
			return err
		}
	}
	n, err := s.f.Write(line)
	s.size += int64(n)
	if err != nil {
		s.poisonLocked(fmt.Errorf("append to log target %q: %w", s.target, err))
		return s.poison
	}
	return nil
}

// scan re-reads the target's real size and enforces the cap. The shim writes
// to the same inode through inherited fd 3, so the daemon's own byte count is
// only a lower bound and a periodic scan is the only thing that sees those
// writes.
func (s *sink) scan() error {
	s.mu.Lock()
	defer s.mu.Unlock()
	if s.poison != nil {
		return s.poison
	}
	info, err := s.f.Stat()
	if err != nil {
		s.poisonLocked(fmt.Errorf("stat log target %q during cap maintenance: %w", s.target, err))
		return s.poison
	}
	s.size = info.Size()
	if s.size <= CapBytes {
		return nil
	}
	return s.maintainLocked()
}

// maintainLocked clears the target in place. It first proves the canonical
// symlink still names the manager-owned target, because truncating a file the
// workspace has since redirected the link away from would destroy someone
// else's data. Readers holding the target open keep observing the same inode.
func (s *sink) maintainLocked() error {
	dest, err := os.Readlink(s.link)
	if err != nil {
		s.poisonLocked(fmt.Errorf("read canonical link %q during cap maintenance: %w", s.link, err))
		return s.poison
	}
	if dest != s.target {
		s.poisonLocked(fmt.Errorf(
			"canonical link %q names %q, not the manager-owned target %q: refusing to truncate",
			s.link, dest, s.target))
		return s.poison
	}
	if err := s.f.Truncate(0); err != nil {
		s.poisonLocked(fmt.Errorf("truncate log target %q at the cap: %w", s.target, err))
		return s.poison
	}
	s.size = 0
	return nil
}

// poisonLocked records the failure that took this sink out of service.
func (s *sink) poisonLocked(cause error) {
	if s.poison != nil {
		return
	}
	s.poison = fmt.Errorf("%w: workspace %s (%s) sink %s.log: %w",
		ErrPoisoned, s.workspaceID, s.workspaceDir, s.name, cause)
}

// poisoned reports the sink's poison, or nil while it is healthy.
func (s *sink) poisoned() error {
	s.mu.Lock()
	defer s.mu.Unlock()
	return s.poison
}

// file is the descriptor a borrower inherits as fd 3.
func (s *sink) file() *os.File { return s.f }

// close releases the descriptor. The canonical link and the target are left in
// place: readers keep resolving them, and a later runtime makes its own
// target rather than trusting this one.
func (s *sink) close() error {
	s.mu.Lock()
	defer s.mu.Unlock()
	if s.f == nil {
		return nil
	}
	err := s.f.Close()
	s.f = nil
	if s.poison == nil {
		s.poison = fmt.Errorf("%w: workspace %s sink %s.log is closed",
			ErrPoisoned, s.workspaceID, s.name)
	}
	if err != nil {
		return fmt.Errorf("close log target %q: %w", s.target, err)
	}
	return nil
}
