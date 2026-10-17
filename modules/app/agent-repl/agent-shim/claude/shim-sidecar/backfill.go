package main

// backfill.go — A ONE-SHOT READ OF NAMED CONVERSATIONS THAT NO SHIM HOLDS.
//
// Only an active workspace's files are watched (active.go), so a conversation
// whose shim has not started a session is never read. That is the right steady
// state, and the wrong one after the store has lost its rows: on 2026-10-17 a
// deploy rollback nuked events.db, the sidecar rebuilt the 26 conversations
// whose shims held their locks, and every open or hidden workspace whose shim
// was idle stayed empty until its next prompt.
//
// A BACKFILL REQUEST is a file in `<state dir>/sidecar-backfill/` naming one
// vendor session id per line. Each named conversation that is not already
// active is entered as DRAINING, exactly as a workspace whose lock was just
// released: its dormant files are admitted and read from the cursor the store
// holds (offset 0 for a conversation the store has never seen), and the drain
// bounds in active.go drop them back to dormant once nothing more is owed. So
// a backfill reads each file to its end once and leaves the watched set as it
// found it, with no second reading path.
//
// A writer creates the request under a name ending in `.tmp` and renames it, so
// a half-written request is never read. A request is consumed by removing it;
// one that cannot be read or parsed is renamed with `.rejected` and stated at
// ERROR, so it is neither retried every tick nor dropped silently.

import (
	"bufio"
	"bytes"
	"fmt"
	"os"
	"path/filepath"
	"sort"
	"strings"
	"unicode"

	"agentrepl/shim-claude-sidecar/internal/logging"
)

// backfillDirName is the request directory under the state dir.
const backfillDirName = "sidecar-backfill"

// backfillSuffixes are the names a request directory may hold that are not
// requests: one still being written, and one already rejected.
var backfillSuffixes = []string{".tmp", ".rejected"}

// backfillDir is where backfill requests are dropped.
func (s *sidecar) backfillDir() string {
	return filepath.Join(s.options.StateDir, backfillDirName)
}

// parseBackfillRequest answers the vendor session ids a request names, one per
// line; blank lines are allowed and nothing else is.
func parseBackfillRequest(body []byte) ([]string, error) {
	var sessions []string
	scanner := bufio.NewScanner(bytes.NewReader(body))
	for n := 1; scanner.Scan(); n++ {
		line := strings.TrimSpace(scanner.Text())
		if line == "" {
			continue
		}
		if strings.IndexFunc(line, unicode.IsSpace) >= 0 {
			return nil, fmt.Errorf("line %d names more than one session id: %q", n, line)
		}
		sessions = append(sessions, line)
	}
	if err := scanner.Err(); err != nil {
		return nil, err
	}
	if len(sessions) == 0 {
		return nil, fmt.Errorf("the request names no session id")
	}
	return sessions, nil
}

// consumeBackfillRequests enters every conversation the pending requests name
// as draining, consumes each request, and answers how many conversations it
// newly entered.
func (s *sidecar) consumeBackfillRequests() int {
	dir := s.backfillDir()
	entries, err := os.ReadDir(dir)
	if err != nil {
		if os.IsNotExist(err) {
			return 0
		}
		s.stateBackfillDirFailure(dir, err)
		return 0
	}
	s.backfillFailure = ""
	names := make([]string, 0, len(entries))
	for _, entry := range entries {
		if entry.IsDir() || hasAnySuffix(entry.Name(), backfillSuffixes) {
			continue
		}
		names = append(names, entry.Name())
	}
	sort.Strings(names)
	entered := 0
	for _, name := range names {
		entered += s.consumeBackfillRequest(filepath.Join(dir, name))
	}
	return entered
}

// consumeBackfillRequest consumes one request and answers how many
// conversations it newly entered as draining.
func (s *sidecar) consumeBackfillRequest(path string) int {
	bound := s.log.With(logging.Context{Operation: "backfill-request", Path: path})
	body, err := os.ReadFile(path)
	if err == nil {
		var sessions []string
		sessions, err = parseBackfillRequest(body)
		if err == nil {
			entered, already := s.enterBackfill(sessions)
			if removeErr := os.Remove(path); removeErr != nil {
				bound.With(logging.Context{Level: "error"}).Log(
					"the backfill request was applied but could not be removed, so it will be applied again next tick: %v", removeErr)
			}
			bound.Log("backfilling %d conversation(s) named by %d session id(s); %d were already watched",
				entered, len(sessions), already)
			return entered
		}
	}
	rejected := path + ".rejected"
	if renameErr := os.Rename(path, rejected); renameErr != nil {
		bound.With(logging.Context{Level: "error"}).Log(
			"the backfill request is unusable (%v) and could not be set aside: %v", err, renameErr)
		return 0
	}
	bound.With(logging.Context{Level: "error"}).Log(
		"the backfill request is unusable and was set aside as %s: %v", filepath.Base(rejected), err)
	return 0
}

// enterBackfill enters each named conversation that is not already watched as
// draining. It answers how many it entered and how many were already members.
func (s *sidecar) enterBackfill(sessions []string) (entered, already int) {
	for _, session := range sessions {
		original := s.identity.Lookup(session).Original
		if s.member(original) {
			already++
			continue
		}
		s.draining[original] = drainingConversation{scansAtEnd: s.fullScans}
		s.log.With(logging.Context{Operation: "backfill-request", VendorSessionID: session}).LogVerbose(
			"conversation %s is read once from the store's cursor and dropped when drained", original)
		entered++
	}
	return entered, already
}

// stateBackfillDirFailure states once per distinct error that the request
// directory could not be read; a repeat is verbose.
func (s *sidecar) stateBackfillDirFailure(dir string, err error) {
	detail := err.Error()
	bound := s.log.With(logging.Context{Operation: "backfill-request", Path: dir, Level: "error"})
	if s.backfillFailure == detail {
		bound.With(logging.Context{Level: "debug"}).LogVerbose("the backfill request directory still cannot be read: %v", err)
		return
	}
	s.backfillFailure = detail
	bound.Log("the backfill request directory cannot be read, so no backfill request is applied: %v", err)
}

func hasAnySuffix(name string, suffixes []string) bool {
	for _, suffix := range suffixes {
		if strings.HasSuffix(name, suffix) {
			return true
		}
	}
	return false
}
