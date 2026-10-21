package feed

import (
	"fmt"

	"agent-repld/internal/dlog"
	"agent-repld/internal/linediff"
	"agent-repld/internal/pyedit"

	frontendv1 "agentrepl/proto/frontend/v1"
)

// bashEdits reads the file edits a shell command states through python
// (pyedit), drawn as FeedSimpleToolCall.edits. A command that states none
// answers nil; one that runs python pyedit declines to read is recorded, so a
// reader who expected a diff can find why the card drew none.
func (r *resolver) bashEdits(s *wsState, unitID, command string) *frontendv1.FeedToolCallEdits {
	edits, err := pyedit.Parse(command)
	if err != nil {
		r.logger(s.id).Debug("daemon.feed.bash_edits_declined",
			"a shell command runs python whose edits cannot be read whole; the card draws no diff",
			dlog.Context{"unit": unitID, "reason": err.Error()})
		return nil
	}
	if len(edits) == 0 {
		return nil
	}
	out := &frontendv1.FeedToolCallEdits{}
	for _, edit := range edits {
		out.Files = append(out.Files, fileEdit(edit))
	}
	r.logger(s.id).Debug("daemon.feed.bash_edits_stated",
		"a shell command states file edits; the card draws them as a diff",
		dlog.Context{"unit": unitID, "files": len(out.Files)})
	return out
}

// unitBashEdits is the edits a shell unit's command states, read once per
// command line.
func (r *resolver) unitBashEdits(s *wsState, unitID string, u *unitState) *frontendv1.FeedToolCallEdits {
	if !u.editsRead || u.editsOf != u.input {
		u.edits = r.bashEdits(s, unitID, u.input)
		u.editsOf = u.input
		u.editsRead = true
	}
	return u.edits
}

// fileEdit draws one file's stated edit: a header opening each replacement,
// then the line diff of its old text against its new.
func fileEdit(edit pyedit.FileEdit) *frontendv1.FeedToolCallFileEdit {
	out := &frontendv1.FeedToolCallFileEdit{Path: edit.Path}
	for i, replacement := range edit.Replacements {
		out.Lines = append(out.Lines, headerLine(replacementHeader(i, len(edit.Replacements), replacement)))
		for _, line := range linediff.Diff(replacement.Old, replacement.New) {
			out.Lines = append(out.Lines, lineOfKind(line.Kind, line.Text))
		}
	}
	return out
}

// replacementHeader names which replacement a hunk is, and how much of the
// file it reaches: the command states no line numbers, only whether every
// occurrence of the old text is replaced or only the first.
func replacementHeader(i, n int, replacement pyedit.Replacement) string {
	reach := "every occurrence"
	if replacement.First {
		reach = "first occurrence"
	}
	if n == 1 {
		return fmt.Sprintf("@@ %s @@", reach)
	}
	return fmt.Sprintf("@@ %d of %d · %s @@", i+1, n, reach)
}

// lineOfKind is the wire diff line a line-diff kind draws as.
func lineOfKind(kind linediff.Kind, text string) *frontendv1.FeedDiffLine {
	switch kind {
	case linediff.Added:
		return addedLine(text)
	case linediff.Removed:
		return removedLine(text)
	case linediff.Context:
		return contextLine(text)
	}
	panic(fmt.Sprintf("feed.lineOfKind: linediff produced the unknown kind %d", kind))
}

// withEdits folds a shell command's stated edits onto its card, while it runs
// and once it has returned. A nil edits changes nothing.
func withEdits(outcome toolOutcome, edits *frontendv1.FeedToolCallEdits) toolOutcome {
	if edits == nil {
		return outcome
	}
	return func(card *frontendv1.FeedSimpleToolCall) {
		outcome(card)
		if card.GetRunning() != nil || card.GetReturned() != nil {
			card.Edits = edits
		}
	}
}
