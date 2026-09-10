package dlog

import (
	"errors"
	"fmt"
	"os"
	"path/filepath"
	"sync"

	"agentrepl/logging"
)

// CapBytes is the per-workspace durable sink cap from logging-contract.md.
const CapBytes = 64 << 20

// HardCapBytes is the shim sink's hard ceiling. A shim target that reaches it
// refuses daemon-managed writes, records one error, and requests a process
// roll at the next free turn boundary.
const HardCapBytes = CapBytes + CapBytes/10

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

	mu            sync.Mutex
	file          *logging.RotatingFile
	cap           int64
	size          int64
	rotatePending bool
	hardCeiling   bool
	hardReported  bool
	poison        error
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
	return openSinkSized(logsDir, workspaceDir, workspaceID, name, target, CapBytes, logging.DefaultBackups)
}

// openSinkSized is the test seam for the generation cap. Production always
// supplies the contract's 64 MiB cap and shared generation count through
// openSink.
func openSinkSized(logsDir, workspaceDir, workspaceID, name, target string, capBytes int64, backups int) (*sink, error) {
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
	file, err := logging.OpenRotating(target, capBytes, backups)
	if err != nil {
		return nil, fmt.Errorf("open log target %q for append: %w", target, err)
	}
	info, err := os.Stat(target)
	if err != nil {
		file.Close()
		return nil, fmt.Errorf("stat log target %q: %w", target, err)
	}
	s := &sink{
		name:         name,
		workspaceDir: workspaceDir,
		workspaceID:  workspaceID,
		link:         filepath.Join(linkDir, name+".log"),
		target:       target,
		file:         file,
		cap:          capBytes,
		size:         info.Size(),
	}
	if err := replaceLink(s.link, target); err != nil {
		file.Close()
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

// write appends one already-rendered JSONL line. Daemon-owned sinks rotate
// synchronously before a record would cross the cap. The shim's inherited
// descriptor cannot be swapped beneath it, so daemon-managed shim writes mark
// the target at the soft cap and are refused at the hard ceiling until the
// next shim roll installs a fresh descriptor.
func (s *sink) write(line []byte) error {
	s.mu.Lock()
	defer s.mu.Unlock()
	if s.poison != nil {
		return s.poison
	}
	if s.name == "shim" {
		return s.writeShimLocked(line)
	}
	rolled := s.size > 0 && s.size+int64(len(line)) > s.cap
	if rolled {
		if err := s.verifyLinkLocked("size rotation"); err != nil {
			return err
		}
	}
	n, err := s.file.Write(line)
	if err != nil {
		s.poisonLocked(fmt.Errorf("append to log target %q: %w", s.target, err))
		return s.poison
	}
	if n != len(line) {
		s.poisonLocked(fmt.Errorf("append to log target %q: wrote %d of %d bytes", s.target, n, len(line)))
		return s.poison
	}
	if rolled {
		s.size = int64(n)
		if err := s.repointAfterRollLocked(); err != nil {
			return err
		}
	} else {
		s.size += int64(n)
	}
	return nil
}

// writeShimLocked is the daemon-controlled write path for shim.log. Production
// shim records arrive through the inherited descriptor instead, but keeping
// this path bounded makes the sink's contract complete for every caller.
func (s *sink) writeShimLocked(line []byte) error {
	if s.hardCeiling || s.size+int64(len(line)) > hardCap(s.cap) {
		s.rotatePending = true
		s.hardCeiling = true
		return fmt.Errorf("shim log target %q reached its hard ceiling of %d bytes", s.target, hardCap(s.cap))
	}
	n, err := s.file.File().Write(line)
	s.size += int64(n)
	if err != nil {
		s.poisonLocked(fmt.Errorf("append to shim log target %q: %w", s.target, err))
		return s.poison
	}
	if n != len(line) {
		s.poisonLocked(fmt.Errorf("append to shim log target %q: wrote %d of %d bytes", s.target, n, len(line)))
		return s.poison
	}
	if s.size >= s.cap {
		s.rotatePending = true
	}
	return nil
}

// scan re-reads the target's real size and enforces the cap. The shim writes
// to the same inode through inherited fd 3, so the daemon's own byte count is
// only a lower bound and a periodic scan is the only thing that sees those
// writes.
func (s *sink) scan() (scanResult, error) {
	s.mu.Lock()
	defer s.mu.Unlock()
	if s.poison != nil {
		return scanResult{}, s.poison
	}
	if s.name != "shim" {
		return scanResult{}, nil
	}
	info, err := s.file.File().Stat()
	if err != nil {
		s.poisonLocked(fmt.Errorf("stat log target %q during cap maintenance: %w", s.target, err))
		return scanResult{}, s.poison
	}
	s.size = info.Size()
	result := scanResult{size: s.size}
	if s.size >= s.cap && !s.rotatePending {
		s.rotatePending = true
		result.marked = true
	}
	if s.size >= hardCap(s.cap) {
		s.hardCeiling = true
		if !s.hardReported {
			s.hardReported = true
			result.hardFirst = true
		}
	}
	return result, nil
}

// retryHardReport lets the next cap scan retry the canonical error record and
// roll request after acquiring the workspace logger failed. The hard ceiling
// remains active, so no daemon-managed byte is accepted while reporting is
// retried.
func (s *sink) retryHardReport() {
	s.mu.Lock()
	defer s.mu.Unlock()
	s.hardReported = false
}

type scanResult struct {
	size      int64
	marked    bool
	hardFirst bool
}

// rotateShim rolls a marked shim target at the process boundary where a fresh
// fd 3 is being requested. The retiring shim's duplicated descriptor stays on
// generation .1 while the replacement inherits the fresh current descriptor.
func (s *sink) rotateShim() (bool, error) {
	s.mu.Lock()
	defer s.mu.Unlock()
	if s.poison != nil {
		return false, s.poison
	}
	if s.name != "shim" || !s.rotatePending {
		return false, nil
	}
	if err := s.verifyLinkLocked("shim process roll"); err != nil {
		return false, err
	}
	if err := s.file.Roll(); err != nil {
		s.poisonLocked(fmt.Errorf("roll shim log target %q at process replacement: %w", s.target, err))
		return false, s.poison
	}
	if err := s.repointAfterRollLocked(); err != nil {
		return false, err
	}
	s.size = 0
	s.rotatePending = false
	s.hardCeiling = false
	s.hardReported = false
	return true, nil
}

// repointAfterRollLocked atomically refreshes the canonical link after the
// current path has acquired a fresh inode. The target spelling is stable for
// one runtime, but replacing the link makes the generation switch itself an
// atomic workspace-directory operation rather than trusting an old link.
func (s *sink) repointAfterRollLocked() error {
	if err := s.verifyLinkLocked("canonical re-point"); err != nil {
		return err
	}
	if err := os.Chmod(s.target, 0o600); err != nil {
		s.poisonLocked(fmt.Errorf("set fresh log target %q permissions after rotation: %w", s.target, err))
		return s.poison
	}
	if err := replaceLink(s.link, s.target); err != nil {
		s.poisonLocked(fmt.Errorf("re-point canonical link after rotating %q: %w", s.target, err))
		return s.poison
	}
	return nil
}

func (s *sink) verifyLinkLocked(operation string) error {
	dest, err := os.Readlink(s.link)
	if err != nil {
		s.poisonLocked(fmt.Errorf("read canonical link %q during %s: %w", s.link, operation, err))
		return s.poison
	}
	if dest != s.target {
		s.poisonLocked(fmt.Errorf(
			"canonical link %q names %q, not the manager-owned target %q: refusing %s",
			s.link, dest, s.target, operation))
		return s.poison
	}
	return nil
}

func hardCap(capBytes int64) int64 { return capBytes + capBytes/10 }

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

// borrowFile is the current descriptor a borrower inherits as fd 3.
func (s *sink) borrowFile() (*os.File, error) {
	s.mu.Lock()
	defer s.mu.Unlock()
	if s.poison != nil {
		return nil, s.poison
	}
	f := s.file.File()
	if f == nil {
		return nil, fmt.Errorf("borrow shim log target %q: the rotating file is closed", s.target)
	}
	return f, nil
}

// close releases the descriptor. The canonical link and the target are left in
// place: readers keep resolving them, and a later runtime makes its own
// target rather than trusting this one.
func (s *sink) close() error {
	s.mu.Lock()
	defer s.mu.Unlock()
	if s.file == nil {
		return nil
	}
	err := s.file.Close()
	s.file = nil
	if s.poison == nil {
		s.poison = fmt.Errorf("%w: workspace %s sink %s.log is closed",
			ErrPoisoned, s.workspaceID, s.name)
	}
	if err != nil {
		return fmt.Errorf("close log target %q: %w", s.target, err)
	}
	return nil
}
