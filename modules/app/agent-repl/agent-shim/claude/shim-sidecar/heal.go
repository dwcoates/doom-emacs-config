package main

// heal.go — A CONVERSION CHANGE HEALS THE ROWS IT MADE WRONG.
//
// Every row this process writes records the conversion that produced it
// (convert.ConversionVersion, stamped in storeWrite), and every cursor records
// the conversion its file was read under (store.v1 CursorConversion). A
// transcript whose cursor names an OLDER conversion — or none, for a cursor
// stored before versions existed — holds rows that conversion produced, some
// of which the current one would never write: owner ruling 2026-09-27, the 214
// `prompt:` rows the old conversion minted for task notifications, command
// envelopes, local-command output, bare /compact lines and interrupt markers.
// Re-reading alone could never remove them, because the new conversion writes
// different keys.
//
// SO SUCH A TRANSCRIPT IS RE-READ FROM ITS START under the current conversion
// — a RE-DERIVATION, or heal:
//
//   - its cursor restarts at offset 0 with `healing.through` set to where the
//     older conversion had read to, and every batch's advance states it, so a
//     restart resumes the heal at the last committed offset;
//   - every row a record still converts to is written again; the store keeps
//     one whose content did not change exactly as it is, telling no reader
//     (the store's restamp);
//   - every row a record NO LONGER converts to is named on the batch
//     (EntryBatch.retirements, from convert.RetiredKeys) and the store retires
//     it: out of every page, and withdrawn from every standing watch;
//   - the batch that reaches `through` states `current`, and the file is read
//     like any other from there.
//
// A HEAL NEVER HOLDS A LIVE FILE BACK. A healing watcher is skipped by the
// poll pass and read by healStep instead, on its own budget after the pass, so
// re-reading the owner's whole corpus once costs the live files nothing; a
// dormant transcript (no active workspace owns it) is admitted to the watched
// set for its heal and drops back to dormant once it has drained, and a file
// of an active conversation is healed first.

import (
	"os"
	"sort"
	"time"

	storev1 "agentrepl/proto/store/v1"
	"agentrepl/shim-claude-sidecar/internal/convert"
	"agentrepl/shim-claude-sidecar/internal/discover"
	"agentrepl/shim-claude-sidecar/internal/logging"
	"agentrepl/shim-claude-sidecar/internal/storeclient"
	"agentrepl/shim-claude-sidecar/internal/tail"
)

// healState is one watched transcript's re-derivation in progress.
type healState struct {
	// through is the offset the older conversion had read the file to: every
	// byte below it was converted by that conversion and is being re-read.
	through int64
	// from is the conversion version the file's rows were produced by, 0 for a
	// cursor stored before versions existed. It is unknown (and unused) for a
	// heal resumed after a restart: the cursor then already names the current
	// version, and only `through` survives.
	from uint32
	// resumed says a restart found this heal already in progress.
	resumed bool
	// batches, retirements and skips count what the heal did, for the one
	// record stating it is over.
	batches     int
	retirements int
	skips       int
}

// healOwed answers the re-derivation a transcript's stored cursor owes under
// the current conversion, or nil when it owes none.
//
// ONLY A TRANSCRIPT IS HEALED. A spool's rows are one rendered tail and a
// terminal that every batch supersedes, and re-reading a spool from its start
// would walk that tail back through its history on every watcher; a journal
// carries no record-owned rows. Their cursors state the current conversion on
// their next batch and nothing is re-read.
func healOwed(kind tail.Kind, cursor *storev1.CursorState) *healState {
	if cursor == nil {
		// Never read: this file produced no row an older conversion could have
		// made wrong.
		return nil
	}
	switch kind {
	case tail.KindSessionTranscript, tail.KindAgentTranscript:
	default:
		return nil
	}
	conv := cursor.GetConversion()
	offset := cursor.GetOffset()
	switch {
	case conv.GetVersion() > convert.ConversionVersion:
		// A NEWER conversion read it (a rollback). Its rows are not stale to
		// this binary, which cannot know what that version decided.
		return nil
	case conv != nil && conv.GetVersion() == convert.ConversionVersion:
		if healing := conv.GetHealing(); healing != nil && healing.GetThrough() > offset {
			return &healState{through: healing.GetThrough(), resumed: true}
		}
		return nil
	}
	through := offset
	if healing := conv.GetHealing(); healing != nil && healing.GetThrough() > through {
		// An older heal was itself cut short: its `through` is further than the
		// offset it reached, and everything up to it predates this version.
		through = healing.GetThrough()
	}
	if through <= 0 {
		return nil
	}
	return &healState{through: through, from: conv.GetVersion()}
}

// conversionAt is the CursorConversion a batch advancing this watcher to
// `offset` states: the current version, healing while the re-read is still
// short of where the older conversion stopped.
func (w *watched) conversionAt(offset int64) *storev1.CursorConversion {
	conv := &storev1.CursorConversion{Version: convert.ConversionVersion}
	if w.heal != nil && offset < w.heal.through {
		conv.State = &storev1.CursorConversion_Healing{Healing: &storev1.CursorConversionHealing{Through: w.heal.through}}
		return conv
	}
	conv.State = &storev1.CursorConversion_Current{Current: &storev1.CursorConversionCurrent{}}
	return conv
}

// retirementsOf spells a batch's retired keys as the store's retirements, at
// the current conversion version.
func retirementsOf(keys []string) []*storev1.StoreRetirement {
	if len(keys) == 0 {
		return nil
	}
	out := make([]*storev1.StoreRetirement, 0, len(keys))
	for _, key := range keys {
		out = append(out, &storev1.StoreRetirement{UpsertKey: key, ConversionVersion: convert.ConversionVersion})
	}
	return out
}

// owesHeal reports whether a DORMANT target is owed a re-derivation, which
// admits it to the watched set although no active workspace owns it.
//
// A FILE WHOSE IDENTITY CANNOT BE READ IS ADMITTED. "Could not tell" is never
// "owes nothing": watchTargets reads the identity again, states the failure,
// and leaves the file for the next rescan exactly as it does for any file.
func (s *sidecar) owesHeal(target discover.Target) bool {
	switch target.Kind {
	case tail.KindSessionTranscript, tail.KindAgentTranscript:
	default:
		return false
	}
	identity, err := tail.Identity(target.Path)
	if err != nil {
		return true
	}
	return healOwed(target.Kind, s.cursors[identity]) != nil
}

// beginHeal restores a transcript's tailer for the re-derivation its cursor
// owes, and says so. A fresh heal restarts the file at offset 0 and takes no
// boot rewind (it re-reads the turn anyway); a resumed one restores the stored
// position and keeps its rewind, which re-warms the joins the restart lost.
func (s *sidecar) beginHeal(target discover.Target, identity string, tailer *tail.Tailer, cursor *storev1.CursorState, heal *healState, now time.Time) {
	bound := s.log.With(logging.Context{
		Operation: "conversion-heal", Path: target.Path, FileID: identity,
		Offset: logging.Off(heal.through), VendorSessionID: target.SessionID, AgentID: target.AgentID,
	})
	if heal.resumed {
		tailer.Restore(cursor)
		s.rewindOnce(target, identity, tailer, now)
		bound.Log("resuming this transcript's re-derivation under conversion version %d at offset %d; bytes up to offset %d were converted by an older conversion",
			convert.ConversionVersion, cursor.GetOffset(), heal.through)
		return
	}
	tailer.Restore(&storev1.CursorState{FileId: identity, Path: target.Path, Offset: 0})
	s.rewound[identity] = true
	bound.Log("re-deriving this transcript from its start under conversion version %d: its rows were produced by conversion version %d up to offset %d, and every row a record no longer converts to is retired",
		convert.ConversionVersion, heal.from, heal.through)
}

// noteHealProgress records one durable batch of a re-derivation, and ends the
// heal once the batch reached where the older conversion stopped.
func (s *sidecar) noteHealProgress(path string, w *watched, result tail.PollResult, skips, retired int) {
	w.heal.batches++
	w.heal.retirements += retired
	w.heal.skips += skips
	offset := result.Next.GetOffset()
	bound := s.log.With(logging.Context{Operation: "conversion-heal", Path: path, FileID: result.Next.GetFileId(), Offset: logging.Off(offset)})
	if offset < w.heal.through {
		bound.LogVerbose("re-derived through offset %d of %d (batch %d, %d retirement candidate(s) named)",
			offset, w.heal.through, w.heal.batches, retired)
		return
	}
	s.endHeal(path, w, result.Next.GetFileId(), offset, "reached where the older conversion stopped")
}

// endHeal states a finished re-derivation once and returns the watcher to
// ordinary reading.
func (s *sidecar) endHeal(path string, w *watched, fileID string, offset int64, why string) {
	s.log.With(logging.Context{Operation: "conversion-heal", Path: path, FileID: fileID, Offset: logging.Off(offset)}).Log(
		"re-derivation under conversion version %d is complete (%s): %d batch(es), %d retirement candidate(s) named, %d legacy book-conflict entrie(s) kept by the store; the file is read like any other from here",
		convert.ConversionVersion, why, w.heal.batches, w.heal.retirements, w.heal.skips)
	w.heal = nil
}

// healShort ends a re-derivation whose file no longer reaches where the older
// conversion stopped — rewritten or truncated since — by committing the
// position it did reach as `current`. Without it the heal would wait forever
// for bytes that no longer exist, and the stored cursor would say `healing`
// across every restart.
func (s *sidecar) healShort(path string, w *watched, result tail.PollResult) (abandon bool) {
	offset := result.Next.GetOffset()
	w.heal.through = offset
	skips, _, err := s.writeBatch(w, result)
	if err != nil {
		if s.interrupted(err) {
			return true
		}
		if field, invalid := storeclient.InvalidRequest(err); invalid {
			s.park(path, w, result, field, err)
			return false
		}
		s.log.With(logging.Context{
			Operation: "store-write", Path: path, Offset: logging.Off(offset), Level: "error",
		}).Log("the end of a re-derivation that stopped short was not committed: %v", err)
		return true
	}
	w.tailer.Commit(result)
	s.cursors[result.Next.GetFileId()] = result.Next
	w.heal.skips += len(skips)
	s.endHeal(path, w, result.Next.GetFileId(), offset, "the file now ends before where the older conversion stopped")
	return false
}

// healBudgetFraction is the share of one poll slice healStep may spend after
// the pass. It is derived from the interval the operator already chose, like
// the slice itself, so the two cannot be configured into disagreement.
const healBudgetFraction = 2

// healStep advances the re-derivations in progress, on a budget of its own,
// after the poll pass has had its slice.
//
// ONE FILE AT A TIME, ACTIVE CONVERSATIONS FIRST. A file is read batch after
// batch until its heal ends or the budget does, then the next; a file that
// cannot make progress this tick (parked, unreadable, a failed read) is passed
// over rather than retried, so it can never starve the rest. At least one batch
// is read per tick whatever the budget says, or a heal could never advance.
func (s *sidecar) healStep() {
	s.requireCursors("healStep")
	order := s.healOrder()
	if len(order) == 0 {
		return
	}
	nowMs := s.now().UnixMilli()
	budget := s.pollSlice() / healBudgetFraction
	deadline := s.now().Add(budget)
	read := 0
	for _, path := range order {
		for {
			if read > 0 && !s.now().Before(deadline) {
				s.log.With(logging.Context{Operation: "conversion-heal", Repeat: logging.Repeat(read)}).LogVerbose(
					"this tick's %s heal budget is spent after %d batch(es); %d file(s) are still being re-derived", budget, read, len(s.healOrder()))
				return
			}
			w, watching := s.watchers[path]
			if !watching || w.heal == nil {
				break
			}
			read++
			progressed, abandon := s.healOnce(path, w, nowMs)
			if abandon {
				return
			}
			if !progressed {
				break
			}
		}
	}
}

// healOnce reads one batch of a re-derivation and answers whether it made
// progress, and whether the caller must stop because production is suspended
// or the process is going away.
func (s *sidecar) healOnce(path string, w *watched, nowMs int64) (progressed, abandon bool) {
	if s.parked[path] {
		return false, false
	}
	before := w.tailer.Offset()
	if size, sized := fileSize(path); sized && size <= before && before < w.heal.through {
		result, err := w.tailer.Poll()
		if err != nil {
			s.pollFailed(path, w, err, nowMs)
			return false, false
		}
		if !result.Changed {
			return false, s.healShort(path, w, result)
		}
		// THE FILE CHANGED IDENTITY OR SHRANK UNDER US. The ordinary read below
		// commits that reset (its poll reads no record, so nothing is converted
		// twice), and the heal carries on from where the reset put it.
	}
	_, abandon = s.pollOnce(path, w, nowMs)
	return w.tailer.Offset() != before || w.heal == nil, abandon
}

// healOrder is every healing watcher, a file of an active or draining
// conversation first, then by path.
func (s *sidecar) healOrder() []string {
	var member, other []string
	for path, w := range s.watchers {
		if w.heal == nil {
			continue
		}
		if original, known := s.conversationOf(w.target); known && s.member(original) {
			member = append(member, path)
			continue
		}
		other = append(other, path)
	}
	sort.Strings(member)
	sort.Strings(other)
	return append(member, other...)
}

// fileSize reads a file's size, and whether it could. A file it cannot stat
// is left to the ordinary read, whose own stat fails the same way and states
// it (pollFailed).
func fileSize(path string) (int64, bool) {
	info, err := os.Stat(path)
	if err != nil {
		return 0, false
	}
	return info.Size(), true
}
