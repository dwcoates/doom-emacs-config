package slackwatch

import (
	"context"
	"fmt"
	"strings"
	"time"

	"agent-repld/internal/briefprompt"
	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/prompthandler"
	"agent-repld/internal/untoldnotice"
	"agent-repld/internal/wsm"
)

// The batching periods. A thread's pending messages are told as one digest
// once the thread has been quiet for the quiet period (the short one when any
// pending message is urgent), and never later than MaxWait after the oldest
// pending message was read, however busy the thread stays. They are design
// choices, not bounds on an operation: HighQuiet coalesces a burst of urgent
// messages without holding them back, LowQuiet lets a conversation finish a
// few messages before the workspace is told, and MaxWait keeps a thread that
// never falls quiet from never being told.
const (
	HighQuiet = 5 * time.Second
	LowQuiet  = 45 * time.Second
	MaxWait   = 3 * time.Minute
)

// The briefs a digest is composed from (prompts/).
const (
	BriefNotice             = "slack-thread-notice"
	BriefContractListen     = "slack-thread-contract-listen"
	BriefContractAutonomous = "slack-thread-contract-autonomous"
)

// DueAt answers when a digest of PENDING is owed. PENDING is never empty.
func DueAt(pending []wsm.SlackThreadMessage) time.Time {
	if len(pending) == 0 {
		panic("slackwatch: a digest of no pending message")
	}
	first, last, quiet := pending[0].ReceivedAt, pending[0].ReceivedAt, LowQuiet
	for _, m := range pending {
		if m.ReceivedAt.Before(first) {
			first = m.ReceivedAt
		}
		if m.ReceivedAt.After(last) {
			last = m.ReceivedAt
		}
		if m.Urgent {
			quiet = HighQuiet
		}
	}
	if cap := first.Add(MaxWait); cap.Before(last.Add(quiet)) {
		return cap
	}
	return last.Add(quiet)
}

// renderMessage is one message as a digest lists it.
func renderMessage(m wsm.SlackThreadMessage) string {
	return fmt.Sprintf("- %s (ts %s): %s", m.Author, m.TS, strings.TrimSpace(m.Text))
}

// renderAll is MSGS as a digest lists them, one line each, oldest first.
func renderAll(msgs []wsm.SlackThreadMessage) []string {
	out := make([]string, len(msgs))
	for i, m := range msgs {
		out[i] = renderMessage(m)
	}
	return out
}

// deliver tells KEY's pending messages as one digest, carrying the last
// digest's when it was superseded before it was told, and answers whether
// nothing is left owed by a failure.
func (w *Watcher) deliver(ctx context.Context, prompts prompthandler.Submitter, key subKey) bool {
	fields := key.fields()
	sub, ok, err := w.lookup(ctx, key)
	if err != nil {
		w.deps.Log.Error(opNotice, "could not read the subscription; its digest is told later", dlog.WithCause(fields, err))
		return false
	}
	if !ok {
		w.deps.Log.Debug(opNotice, "the subscription ended before its digest was told", fields)
		return true
	}
	pending, err := w.deps.Store.SlackThreadPending(ctx, key.ws, key.thread)
	if err != nil {
		w.deps.Log.Error(opNotice, "could not read the pending messages; the digest is told later", dlog.WithCause(fields, err))
		return false
	}
	if len(pending) == 0 {
		return true
	}
	prev := untoldnotice.Notice{Turn: ids.TurnID(sub.UntoldTurn), Sentences: renderAll(sub.UntoldMessages)}
	var messages []wsm.SlackThreadMessage
	if untoldnotice.Carried(ctx, w.deps.Holds, w.deps.Log, opNotice, key.ws, prev, sub.UntoldTurn != "" || len(sub.UntoldMessages) > 0, fields) != nil {
		messages = append(messages, sub.UntoldMessages...)
	}
	carried := len(messages)
	messages = append(messages, pending...)
	took := make([]string, len(pending))
	urgent := false
	for i, m := range pending {
		took[i] = m.TS
		urgent = urgent || m.Urgent
	}
	fields = dlog.Merge(fields, dlog.Context{"messages": len(messages), "carried": carried, "urgent": urgent})
	turn, err := w.submit(ctx, prompts, sub, messages, urgent, fields)
	if err != nil {
		// What the superseded digest said is kept to be said again; the
		// pending messages stay pending for the next attempt.
		if carried > 0 {
			if rerr := w.deps.Store.RecordSlackThreadDigest(ctx, wsm.SlackThreadDigest{Subscriber: key.ws, Thread: key.thread, Messages: messages[:carried]}); rerr != nil {
				w.deps.Log.Error(opNotice, "could not keep the superseded digest's messages; they are not said again", dlog.WithCause(fields, rerr))
			}
		}
		return false
	}
	if err := w.deps.Store.RecordSlackThreadDigest(ctx, wsm.SlackThreadDigest{Subscriber: key.ws, Thread: key.thread, Turn: string(turn), Messages: messages, Took: took}); err != nil {
		w.deps.Log.Error(opNotice, "could not record the told digest; its messages may be told again", dlog.WithCause(fields, err))
		return true
	}
	w.deps.Log.Info(opNotice, "told the subscriber its thread's new messages", dlog.Merge(fields, dlog.Context{"turn": string(turn)}))
	return true
}

// submit composes and submits one digest, answering its turn. A failure is
// recorded here, naming the step that failed, under FIELDS.
func (w *Watcher) submit(ctx context.Context, prompts prompthandler.Submitter, sub wsm.SlackThreadSubscription, messages []wsm.SlackThreadMessage, urgent bool, fields dlog.Context) (ids.TurnID, error) {
	contractBrief := BriefContractAutonomous
	if sub.Mode == wsm.SlackThreadModeListen {
		contractBrief = BriefContractListen
	}
	contract, err := w.deps.Briefs(contractBrief, map[string]string{"thread": sub.URL})
	if err != nil {
		w.deps.Log.Error(opNotice, "could not compose the mode's contract for a Slack digest; it is told later", dlog.WithCause(fields, err))
		return "", err
	}
	delivery := wsm.DeliveryDeferred
	if urgent {
		delivery = wsm.DeliveryOrdinary
	}
	turn, err := briefprompt.Composer{Briefs: w.deps.Briefs, MintKey: w.deps.MintKey, Meter: w.deps.Notices}.SubmitAs(ctx, prompts, sub.Subscriber, BriefNotice, map[string]string{
		"thread":   sub.URL,
		"mode":     string(sub.Mode),
		"contract": contract,
		"messages": strings.Join(renderAll(messages), "\n"),
	}, BriefNotice, Origin, delivery)
	switch briefprompt.StepOf(err) {
	case briefprompt.StepCompose:
		w.deps.Log.Error(opNotice, "could not compose a Slack digest; it is told later", dlog.WithCause(fields, err))
		return "", err
	case briefprompt.StepMint:
		w.deps.Log.Error(opNotice, "could not mint a Slack digest's idempotency key; it is told later", dlog.WithCause(fields, err))
		return "", err
	case briefprompt.StepSubmit:
		w.deps.Log.Error(opNotice, "could not submit a Slack digest to its subscriber; it is told later", dlog.WithCause(fields, err))
		return "", err
	}
	return turn, nil
}
