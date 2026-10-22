// Package untoldnotice is the ONE supersession every subscription notifier
// shares (internal/mergenotice, internal/workspacesubs): a subscriber holds at
// most one untold notice per subject, the next change supersedes it, and the
// new notice says every change the superseded one said, oldest first.
//
// A notice waits as a deferred prompt behind the subscriber's running turn, so
// a busy subscriber was otherwise told each change as its own turn long after
// its subject had moved on (2026-10-22). Each notifier keeps its untold notice
// durably, in a table of its own subject, and asks this package what the next
// notice must say again.
package untoldnotice

import (
	"context"
	"fmt"
	"strings"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
)

// Notice is a subscriber's latest notice about one subject that may not have
// been told yet.
type Notice struct {
	// Turn is the turn the notice was submitted as; empty when its submission
	// failed and nothing was submitted.
	Turn ids.TurnID
	// Sentences states every change the notice says, oldest first.
	Sentences []string
}

// Holds retires a subscriber's untold notice (promptqueue.Queue's
// Supersede): true when it was retired, false when it was already told.
type Holds interface {
	Supersede(ctx context.Context, ws ids.WorkspaceID, turn ids.TurnID) (bool, error)
}

// Carried answers the sentences SUBSCRIBER's untold notice PREV said that its
// next notice must say again: all of them when the notice was retired before it
// was told (or never submitted), none when it was told or no notice is
// recorded (FOUND false). A notice that could not be retired stands and is
// told, so nothing it said is carried. Every outcome is recorded under OP
// through LOG with FIELDS naming the subject.
func Carried(ctx context.Context, holds Holds, log dlog.Logger, op string, subscriber ids.WorkspaceID, prev Notice, found bool, fields dlog.Context) []string {
	if !found {
		return nil
	}
	fields = dlog.Merge(fields, dlog.Context{"subscriber": string(subscriber), "carried": len(prev.Sentences)})
	if prev.Turn == "" {
		log.Info(op, "the subscriber's last notice was never submitted; the next one says its changes", fields)
		return prev.Sentences
	}
	fields["superseded_turn"] = string(prev.Turn)
	retired, err := holds.Supersede(ctx, subscriber, prev.Turn)
	if err != nil {
		log.Error(op, "could not supersede the subscriber's untold notice; it stands and is told, and the next one says only the new change", dlog.WithCause(fields, err))
		return nil
	}
	if !retired {
		log.Debug(op, "the subscriber's last notice was told; the next one says only the new change", fields)
		return nil
	}
	log.Info(op, "the subscriber's untold notice was superseded; the next one says its changes first", fields)
	return prev.Sentences
}

// Sentences states every change, oldest first: one change as its sentence,
// several as a list under how many there were. A notice of no change is a
// caller's defect.
func Sentences(sentences []string) string {
	switch len(sentences) {
	case 0:
		panic("untoldnotice: a notice was composed of no change")
	case 1:
		return sentences[0]
	}
	var b strings.Builder
	b.WriteString(fmt.Sprintf("It changed %d times since the last notice you were told, oldest first:", len(sentences)))
	for _, sentence := range sentences {
		b.WriteString("\n- " + sentence)
	}
	return b.String()
}
