package dlog

import (
	"errors"
	"fmt"
	"io/fs"
	"os"
	"path/filepath"
	"regexp"
	"syscall"

	"agentrepl/logging"
)

// DiscardedLog is one log file DiscardWorkspace removed.
type DiscardedLog struct {
	Path  string
	Bytes int64
}

// DiscardReport is what one DiscardWorkspace did.
type DiscardReport struct {
	// Removed are the log files removed, or under judgeOnly that would be.
	Removed []DiscardedLog
	// Held are the files skipped because a writer outside this daemon (a
	// shim that inherited its descriptor) still holds them.
	Held []string
	// Links are the canonical links removed with the targets they named, or
	// under judgeOnly that would be.
	Links []string
}

// workspaceLogFamily matches the base of every daemon-written log family of
// one workspace: the targets createTarget mints under its minted id.
func workspaceLogFamily(id string) *regexp.Regexp {
	return regexp.MustCompile(`^agent-repl-` + regexp.QuoteMeta(id) + `-(daemon|shim|webapp|sidecar)-[A-Za-z0-9]+\.log$`)
}

// DiscardWorkspace implements Surfaces.
//
// THE WHOLE OF IT RUNS UNDER THE MUTEX EVERY SINK OPEN HOLDS, so no sink of
// the workspace is opened between the daemon letting go of its descriptors
// and the canonical links being read: a link removed here is one that named a
// file removed here, never one a concurrent open had just re-pointed.
//
// THE DAEMON'S OWN DESCRIPTORS ARE RELEASED FIRST, under judgeOnly too: the
// lock probe that proves nobody else holds a family is then the removal's own,
// so a judgement reports exactly what a removal would take. Releasing them
// costs nothing: the workspace's next record opens its sink afresh.
//
// EMACS'S OWN FILES (`agent-repl-<id>-emacs-*.log`) ARE NOT TOUCHED: Emacs
// opens them per write and holds no lock, so nothing can prove them unheld,
// and Emacs ages them itself (lisp/core.el).
func (s *surfaces) DiscardWorkspace(dir, id string, judgeOnly bool) (DiscardReport, error) {
	if id == "" {
		return DiscardReport{}, errors.New("discard a workspace's logs: no workspace id was given")
	}
	errs, err := s.releaseWorkspace(id)
	if err != nil {
		return DiscardReport{}, err
	}
	// EVERY FILE THE DISCARD REMOVES IS TALLIED FIRST (BindBeforePrune),
	// after this daemon's own sinks of it closed, so nothing it writes is
	// left untallied, and with no lock held, since the tally logs.
	if !judgeOnly {
		s.runBeforePrune("discard")
	}
	s.mu.Lock()
	defer s.mu.Unlock()
	if s.closed {
		return DiscardReport{}, errSurfacesClosed
	}

	pruned, err := logging.RemoveFamilies(s.logsDir, workspaceLogFamily(id).MatchString, judgeOnly)
	if err != nil {
		errs = append(errs, fmt.Errorf("discard workspace %s's logs: %w", id, err))
	}
	report := DiscardReport{Held: pruned.Held}
	gone := make(map[string]bool, len(pruned.Removed))
	for _, path := range pruned.Removed {
		report.Removed = append(report.Removed, DiscardedLog{Path: path, Bytes: pruned.Sizes[path]})
		gone[path] = true
	}

	// A CANONICAL LINK GOES WITH THE TARGET IT NAMED. The log reader takes a
	// link naming nothing for a lost log unless the link is older than the
	// writers' retention (bin/logs-reader.go linkAgedOut); a workspace with no
	// link for a runtime never wrote that log, which is what is now true.
	for _, name := range SinkNames {
		link := filepath.Join(dir, linkDirRel, name+".log")
		dest, err := os.Readlink(link)
		switch {
		case errors.Is(err, fs.ErrNotExist), errors.Is(err, syscall.EINVAL), errors.Is(err, syscall.ENOTDIR):
			continue
		case err != nil:
			errs = append(errs, fmt.Errorf("discard workspace %s's logs: read the canonical link %q: %w", id, link, err))
			continue
		case !gone[dest]:
			continue
		}
		if !judgeOnly {
			if err := os.Remove(link); err != nil {
				errs = append(errs, fmt.Errorf("discard workspace %s's logs: remove the canonical link %q: %w", id, link, err))
				continue
			}
		}
		report.Links = append(report.Links, link)
	}
	return report, errors.Join(errs...)
}

// releaseWorkspace closes every sink this daemon holds open for the workspace
// with the minted id ID and forgets its targets, answering the sinks that
// could not be released.
func (s *surfaces) releaseWorkspace(id string) ([]error, error) {
	s.mu.Lock()
	defer s.mu.Unlock()
	if s.closed {
		return nil, errSurfacesClosed
	}
	var errs []error
	for key, ws := range s.workspaces {
		if ws.id != id {
			continue
		}
		delete(s.workspaces, key)
		if err := closeWorkspace(ws); err != nil {
			errs = append(errs, fmt.Errorf("discard workspace %s's logs: release its open sinks: %w", id, err))
		}
	}
	for _, name := range SinkNames {
		delete(s.targets, id+"/"+name)
	}
	return errs, nil
}
