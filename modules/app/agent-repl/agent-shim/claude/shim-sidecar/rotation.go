package main

// rotation.go — A ROTATED TRANSCRIPT IS NOT READ BEFORE ITS BOOK IS KNOWN.
//
// A `/clear` mints a new vendor session id and a new transcript file, and the
// file opens with the harness's local-command trio: the caveat (isMeta), the
// `/clear` COMMAND ENVELOPE, and an empty `system:local_command`. Nothing in the
// file names the conversation it continues; the shim writes that link
// (`<state>/shim/<key>/vendor-id/<new-id>.json`) only once the vendor's NEXT
// `system:init` has told it the new id — which is AFTER the vendor has already
// created the file.
//
// THAT WINDOW SPLIT ONE CONVERSATION ACROSS TWO BOOKS FOR GOOD. A reader that
// read the new file inside it booked every record under the file's own id (the
// R9 resume default), and the store keeps the FIRST book an upsert key is
// written under: the shim's stream-plane writes of the very same keys — the
// cleared cut, the turn's reasoning and its answer — were then SKIPPED as book
// moves, so the daemon's watch on the conversation's real book never saw them.
// Measured in `TestSecondRotateUnderRotatedIdentity` (e2e, 2026-09-23): the
// sidecar read the rotated transcript at 23:02:46.322, the shim wrote its link at
// .323, and the turn's terminal named an answer the feed never drew
// (`daemon.feed.final_answer_unresolved`, why=answer_row_unresolved) while the
// cleared divider never arrived at all. Re-keying the watcher on the next poll
// moved only the records read AFTER it; the ones already committed stayed in the
// wrong book, and the store is right to keep them there.
//
// SO THE FILE IS HELD, UNREAD, UNTIL A RECORD NAMES ITS BOOK. No byte of it is
// converted under a guess, so there is nothing to move and nothing for the store
// to skip. The hold is decided by evidence on disk, never by a timer:
//
//   - the file is a MAIN transcript that no identity record names yet;
//   - a SIBLING transcript in the same project directory IS named by one — the
//     conversation there is the shim's, and the shim links every rotation it
//     runs, so the link is owed rather than hoped for;
//   - and the file OPENS WITH A CLEAR, or has not yet written the user record
//     that says whether it does.
//
// A conversation the shim never recorded is untouched: its transcripts book
// under their own ids exactly as before. A clear the vendor ran OUTSIDE the shim
// in a directory a shim conversation also lives in stays held for as long as
// nothing links it, which is stated once, at INFO, when the hold begins.
//
// It is re-examined on every poll tick — after `rekeyRotations` has asked the
// link directories whether anything changed — so a link that lands is honored
// by the next read, not by the next rescan.

import (
	"bufio"
	"bytes"
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"io/fs"
	"os"
	"path/filepath"
	"strings"
	"time"

	"agentrepl/shim-claude-sidecar/internal/convert"
	"agentrepl/shim-claude-sidecar/internal/discover"
	"agentrepl/shim-claude-sidecar/internal/identity"
	"agentrepl/shim-claude-sidecar/internal/logging"
	"agentrepl/shim-claude-sidecar/internal/tail"
)

// transcriptOpening is what a transcript's first user record says about it.
type transcriptOpening int

const (
	// openingUndetermined: no complete non-meta user record has been written
	// yet, so the file cannot say whether it is a clear's continuation.
	openingUndetermined transcriptOpening = iota
	// openingClear: the first non-meta user record is a `/clear` envelope.
	openingClear
	// openingOther: the first non-meta user record is anything else.
	openingOther
)

// openingRecordBudget bounds how many complete records the opening read walks
// before calling the file an ordinary one. A rotation writes its envelope as the
// SECOND record, so this is two orders of magnitude over the shape it looks for;
// it only keeps a long transcript with no user record from being read whole.
const openingRecordBudget = 64

// String names an opening for a log record.
func (o transcriptOpening) String() string {
	switch o {
	case openingClear:
		return "clear"
	case openingOther:
		return "other"
	default:
		return "undetermined"
	}
}

// readTranscriptOpening reads a transcript from its start up to its first
// non-meta `user` record, and answers what that record is.
//
// ONLY COMPLETE LINES ARE READ. A trailing line with no newline is a record
// still being written, and a verdict on half a record would be a guess.
func readTranscriptOpening(path string) (transcriptOpening, error) {
	file, err := os.Open(path)
	if err != nil {
		return openingUndetermined, err
	}
	defer file.Close()
	reader := bufio.NewReader(file)
	for records := 0; records < openingRecordBudget; {
		line, err := reader.ReadBytes('\n')
		if errors.Is(err, io.EOF) {
			return openingUndetermined, nil
		}
		if err != nil {
			return openingUndetermined, fmt.Errorf("read the opening of %q: %w", path, err)
		}
		line = bytes.TrimSpace(line)
		if len(line) == 0 {
			continue
		}
		records++
		var record map[string]any
		if err := json.Unmarshal(line, &record); err != nil {
			// A malformed record says nothing about a clear. The converter owns
			// stating it as residue once the file is read.
			continue
		}
		if record["type"] != "user" {
			continue
		}
		if meta, _ := record["isMeta"].(bool); meta {
			continue
		}
		message, _ := record["message"].(map[string]any)
		if convert.IsClearCommand(message) {
			return openingClear, nil
		}
		return openingOther, nil
	}
	return openingOther, nil
}

// projectRecorded reports whether another main transcript in the target's
// project directory is booked through one of the shim's identity records — the
// evidence that the conversation living in that directory is the shim's, and
// that the shim will link a rotation of it.
//
// THE DIRECTORY IS ASKED, NOT THE WATCHED SET. A sidecar that boots onto a
// directory holding both the conversation's first transcript and its rotation
// discovers them in one scan, in whatever order it lists them, so a watched-set
// answer would depend on which one it happened to watch first.
func (s *sidecar) projectRecorded(target discover.Target) (bool, error) {
	entries, err := os.ReadDir(filepath.Dir(target.Path))
	if err != nil {
		return false, fmt.Errorf("list the project directory of %q: %w", target.Path, err)
	}
	for _, entry := range entries {
		name := entry.Name()
		if entry.IsDir() || !strings.HasSuffix(name, ".jsonl") {
			continue
		}
		sibling := strings.TrimSuffix(name, ".jsonl")
		if sibling == target.SessionID {
			continue
		}
		if s.identity.Resolve(sibling).Source != identity.SourceUnrecorded {
			return true, nil
		}
	}
	return false, nil
}

// awaitsRotationLink decides whether a discovered transcript is HELD because its
// book is owed by a link the shim has not written yet, and states the hold's
// beginning and end once each.
func (s *sidecar) awaitsRotationLink(target discover.Target) bool {
	path := target.Path
	held, why, readErr := s.rotationHoldVerdict(target)
	if !held {
		if _, was := s.rotationHeld[path]; was {
			delete(s.rotationHeld, path)
			s.log.With(logging.Context{
				Operation: "rotation-hold", Path: path, VendorSessionID: target.SessionID,
			}).Log("released from the rotation hold (%s); it is read from its start", why)
		}
		return false
	}
	if _, was := s.rotationHeld[path]; was {
		s.rotationHeld[path] = target
		s.log.With(logging.Context{
			Operation: "rotation-hold", Path: path, VendorSessionID: target.SessionID,
		}).LogVerbose("still held: %s", why)
		return true
	}
	s.rotationHeld[path] = target
	if readErr != nil {
		s.log.With(logging.Context{
			Operation: "rotation-hold", Path: path, VendorSessionID: target.SessionID, Level: "warn",
		}).Log("held unread: %s, so whether it continues a cleared conversation is unknown; it is examined again on every poll: %v", why, readErr)
		return true
	}
	s.log.With(logging.Context{
		Operation: "rotation-hold", Path: path, VendorSessionID: target.SessionID,
	}).Log("held unread: %s; no identity record names this transcript yet, and a record converted under its own id would split the conversation across two books for good", why)
	return true
}

// rotationHoldVerdict is the hold's evidence, answered with the sentence that
// states it, and the read failure when the opening could not be examined.
func (s *sidecar) rotationHoldVerdict(target discover.Target) (bool, string, error) {
	if target.Kind != tail.KindSessionTranscript || target.SessionID == "" {
		return false, "it is not a main transcript", nil
	}
	if source := s.identity.Resolve(target.SessionID).Source; source != identity.SourceUnrecorded {
		return false, "an identity record names it (source=" + string(source) + ")", nil
	}
	opening, err := readTranscriptOpening(target.Path)
	if errors.Is(err, fs.ErrNotExist) {
		// Gone before it could be examined: the watch path states a vanished
		// transcript itself, and there is nothing left to hold.
		return false, "it vanished before its opening could be read", nil
	}
	if err != nil {
		return true, "its opening could not be read", err
	}
	if opening == openingOther {
		return false, "it does not open with a /clear", nil
	}
	// ONLY NOW IS THE DIRECTORY LISTED: a transcript that opens with anything
	// but a clear is decided by its own first records, and a boot walk over the
	// whole corpus must not pay a readdir per file for an answer it never needs.
	recorded, err := s.projectRecorded(target)
	if err != nil {
		return true, "its project directory could not be listed", err
	}
	if !recorded {
		return false, "no conversation in its project directory is the shim's", nil
	}
	if opening == openingClear {
		return true, "it opens with a /clear in a project directory whose conversation the shim has recorded, so the shim's link will name its book", nil
	}
	return true, "it has not yet written the user record that says whether it opens with a /clear, in a project directory whose conversation the shim has recorded", nil
}

// reexamineRotationHolds offers every held transcript to the watch path again,
// which re-decides its hold and watches the ones whose book is now known.
func (s *sidecar) reexamineRotationHolds(now time.Time) {
	if len(s.rotationHeld) == 0 {
		return
	}
	targets := make([]discover.Target, 0, len(s.rotationHeld))
	for _, target := range s.rotationHeld {
		targets = append(targets, target)
	}
	// The pass's own verdict is not needed here: a store that could not answer
	// for a released file's cursor has already opened the suspension, which
	// ends this cycle, and the next cycle's rescan discovers the file again.
	_, _ = s.watchTargets(targets, now)
}
