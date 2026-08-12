package sessioncontroller

import (
	"context"
	"errors"
	"fmt"

	frontendv1 "agentrepl/proto/agentshim/frontend/v1"

	"claude-repld/internal/ssm"
)

// THE POSITIONLESS HISTORY SURFACE: the daemon half of
// conversation-history.proto's two verbs.
//
// # The principle every decision here follows from
//
// THE CLIENT CANNOT NAME A POSITION. Not a seq, not an offset, not a cursor it
// authored or holds. "Give me everything" is not a request this contract can
// express, because there is no value that means it. So:
//
//   - FirstPageCmd serves the MOST RECENT page and RESETS this reader's
//     position to it. It is the cold open and the whole recovery story.
//   - NextPageCmd serves the page immediately OLDER than the last one served to
//     this reader, and it carries NO POSITION AT ALL. That absence is the
//     design.
//
// # A NextPage with no established position is REFUSED
//
// Not answered with the tail. Defaulting would turn a client bug into a silent
// tail read — a client that lost its place would keep receiving plausible
// history forever and neither end would record that anything was wrong — and "I
// have no position" already has its own verb. The refusal is the client's cue
// to send FirstPageCmd, which is the same cue a rotation produces.
//
// # NO FENCE, and nothing echoed
//
// The daemon owns the position, so it already knows when a generation change
// invalidates it: the generation is stored WITH the position (ssm's
// readerposition.go), a mismatch DROPS the row, and the next NextPageCmd
// arrives with none and is refused. Nothing is sent to the client to be sent
// back — that is the same category error as from_seq. The admission ladder is
// therefore climbed with an EMPTY echoed fence, which it reads as the
// identityless wildcard it already has a rung for.
//
// A page in flight across a transition is handled by request_id, which the
// frontend command layer stamps and the client already uses to apply only the
// pages it awaits.

// historyPageSlots is how many messages one ConversationHistoryPage can carry.
//
// It is the TYPE's number, not a policy: the wire message has ten discrete
// message slots and no eleventh field, so a producer holding an eleventh has
// nowhere to put it. Naming it here is what keeps the read this package asks
// for and the shape it must fit in agreement.
const historyPageSlots = 10

// ErrHistoryReaderPositionless reports a NextPageCmd from a reader that has no
// established position for the workspace.
//
// It is a distinct sentinel because the client acts on it: a refusal for THIS
// reason is answered with FirstPageCmd, while a refusal because the store could
// not be read is not.
var ErrHistoryReaderPositionless = errors.New("session-controller: this reader has no established position, so the page immediately older than its last one does not exist; ask for the first page")

// ConversationReaderPositions is the persisted reader position this package
// needs from the SSM.
//
// It is a narrow interface asserted off the configured SSM rather than a new
// Config field for the same reason WorkspaceStateReader is: the position is SSM
// state, and a second injection point would be a second answer to where a
// reader is.
type ConversationReaderPositions interface {
	// ConversationReaderPosition reads the reader's place, reporting ABSENCE as
	// a distinct fact rather than as a zero position — a zero position is a
	// legal bound and would be served.
	ConversationReaderPosition(reader, workspace string) (ssm.ConversationReaderPosition, bool, error)
	// SetConversationReaderPosition records where the page just served ends.
	SetConversationReaderPosition(reader, workspace, generationID string, beforeSeq uint64) error
	// DropConversationReaderPosition forgets a place that no longer names
	// anything, so the next NextPageCmd is refused rather than misread.
	DropConversationReaderPosition(reader, workspace string) error
}

// FirstConversationHistoryPage serves the MOST RECENT page and resets this
// reader's position to it.
func (m *Manager) FirstConversationHistoryPage(ctx context.Context, reader, workspace string) (*frontendv1.ConversationHistoryPage, error) {
	return m.historyPage(ctx, reader, workspace, true)
}

// NextConversationHistoryPage serves the page immediately OLDER than the last
// one served to this reader, or REFUSES when the reader has no position.
func (m *Manager) NextConversationHistoryPage(ctx context.Context, reader, workspace string) (*frontendv1.ConversationHistoryPage, error) {
	return m.historyPage(ctx, reader, workspace, false)
}

// historyPage is the body both verbs share. `first` is the ONLY difference
// between them, and it decides three things together — where the read is
// anchored, whether an absent position is a refusal, and whether the page
// carries a live edge — so they can never be set inconsistently.
func (m *Manager) historyPage(ctx context.Context, reader, workspace string, first bool) (*frontendv1.ConversationHistoryPage, error) {
	verb := "next_page"
	if first {
		verb = "first_page"
	}
	if reader == "" {
		// A page has to be filed under SOMEONE. Serving one to an unidentified
		// reader would write its position under an empty key, where the next
		// unidentified reader would inherit it.
		return nil, fmt.Errorf("session-controller: %s ws=%q carries no reader identity, and a reading position is per reader per workspace", verb, workspace)
	}
	positions, ok := m.cfg.SSM.(ConversationReaderPositions)
	if !ok {
		return nil, fmt.Errorf("session-controller: %s ws=%q reader=%q cannot be served: the configured SSM keeps no conversation reader positions", verb, workspace, reader)
	}

	m.mu.Lock()
	// The DURABLE route keeps the manager lock through the read and the live
	// route has already released it; deferring the release is what makes that
	// difference impossible to get wrong here (historyadmission.go).
	admission, release, err := m.admitHistoryRequest("conversation history page", fmt.Sprintf("verb=%s reader=%q", verb, reader), workspace, "")
	defer release()
	if err != nil {
		return nil, err
	}

	resolve, err := m.historyPageBound(positions, admission, reader, workspace, verb, first)
	if err != nil {
		return nil, err
	}
	outcome, err := m.serveHistoryPage(ctx, admission, workspace, resolve, first)
	if err != nil {
		return nil, err
	}
	page, err := newHistoryPage(workspace, outcome, first)
	if err != nil {
		// A page that cannot be shaped is never half-served: the reader's
		// position stays where it was, so the retry reads the same range rather
		// than skipping past whatever could not be encoded.
		m.logf("session-controller: %s REFUSED ws=%q reader=%q: %v", verb, workspace, reader, err)
		return nil, err
	}
	// THE POSITION IS WRITTEN ONLY ONCE THE PAGE EXISTS, and a failure to write
	// it FAILS THE REQUEST. A page served under a position that was not
	// recorded would have the reader's next NextPageCmd re-serve the same
	// history silently, which is the one failure mode a client cannot detect.
	if err := positions.SetConversationReaderPosition(reader, workspace, admission.generationID, outcome.nextBeforeSeq); err != nil {
		m.logf("session-controller: %s FAILED to record the reader position ws=%q reader=%q before_seq=%d: %v",
			verb, workspace, reader, outcome.nextBeforeSeq, err)
		return nil, err
	}
	m.logf("session-controller: %s SERVED ws=%q reader=%q generation=%q messages=%d continuation=%s live_join_seq=%d next_before_seq=%d",
		verb, workspace, reader, admission.generationID, len(outcome.items), continuationName(outcome.reachedStart), page.GetLiveJoinSeq(), outcome.nextBeforeSeq)
	return page, nil
}

// serveHistoryPage is the ROUTE SWITCH the positionless surface uses, and it
// differs from the older surface's (servePage) in exactly one place.
//
//   - An UNWIRED workspace is served from the STORE'S BOUNDED PAGE
//     (storepage.go). There is no shim, the store is the only route, and the
//     store can now answer the question this surface actually asks: the newest
//     ten MESSAGES, resolved by the store itself. The forward windowed scan is
//     gone from this route entirely.
//   - A workspace with a LIVE session controller is served THROUGH THE SHIM,
//     and now by the SAME bounded page (livepage.go). The shim carries
//     MessagePageRequest and passes it to the store, so the live route no longer
//     needs the windowed walk — and it still never dials the store itself, which
//     would be the side door repull.go's header forbids.
//
// BOTH ROUTES CURATE THROUGH consumer.pushConversation, so which one served a
// page is invisible in the page.
func (m *Manager) serveHistoryPage(ctx context.Context, admission historyAdmission, workspace string, resolve pageBoundResolver, first bool) (pageOutcome, error) {
	if admission.route == historyRouteLiveController {
		return m.pageFromControllerPage(ctx, admission.controller, admission.generationID, resolve, first)
	}
	return m.pageFromStorePage(ctx, workspace, admission.generationID, resolve, first)
}

// historyPageBound resolves where this verb's read is anchored — and, for a
// next page, rules on whether the reader has a position at all.
func (m *Manager) historyPageBound(positions ConversationReaderPositions, admission historyAdmission, reader, workspace, verb string, first bool) (pageBoundResolver, error) {
	if first {
		// THE RESET. A first page is the newest end of the conversation, and
		// whatever position the reader held is replaced by this page's — which
		// is what makes this verb the recovery from every lost place.
		return func(string) (pageBound, error) { return pageBound{name: "first", tail: true}, nil }, nil
	}
	position, found, err := positions.ConversationReaderPosition(reader, workspace)
	if err != nil {
		return nil, err
	}
	if !found {
		m.logf("session-controller: %s REFUSED ws=%q reader=%q decision=no_reader_position — the tail is deliberately NOT served here; a defaulted next page would turn a client bug into a silent tail read",
			verb, workspace, reader)
		return nil, fmt.Errorf("%w: ws=%q reader=%q", ErrHistoryReaderPositionless, workspace, reader)
	}
	if position.GenerationID != admission.generationID {
		// THE DROP THAT REPLACES A FENCE. The position was established in a seq
		// space this workspace no longer runs in, so it names nothing. It is
		// forgotten here, and the refusal this request gets is the same refusal
		// a reader with no position at all gets — one recovery, one verb.
		if dropErr := positions.DropConversationReaderPosition(reader, workspace); dropErr != nil {
			return nil, dropErr
		}
		m.logf("session-controller: %s REFUSED ws=%q reader=%q decision=generation_rotated position_generation=%q live_generation=%q — the position was dropped, and the client recovers with a first page",
			verb, workspace, reader, position.GenerationID, admission.generationID)
		return nil, fmt.Errorf("%w: ws=%q reader=%q: the position was established under generation %q and the workspace now runs generation %q",
			ErrHistoryReaderPositionless, workspace, reader, position.GenerationID, admission.generationID)
	}
	return func(string) (pageBound, error) {
		return pageBound{name: "next", upper: position.BeforeSeq}, nil
	}, nil
}

// newHistoryPage shapes an assembled page into the wire message.
//
// The messages arrive OLDEST FIRST from the walk and are placed OLDEST FIRST
// into the slots, which is what the contract states and what lets a frontend
// render a paged message with the code that renders a pushed one.
func newHistoryPage(workspace string, outcome pageOutcome, first bool) (*frontendv1.ConversationHistoryPage, error) {
	page := &frontendv1.ConversationHistoryPage{Workspace: workspace}
	if err := fillHistoryPageSlots(page, outcome.items); err != nil {
		return nil, fmt.Errorf("session-controller: conversation history page for ws %q: %w", workspace, err)
	}
	if outcome.reachedStart {
		page.Continuation = &frontendv1.ConversationHistoryPage_Start{Start: &frontendv1.HistoryAtStart{}}
	} else {
		page.Continuation = &frontendv1.ConversationHistoryPage_More{More: &frontendv1.HistoryHasMore{}}
	}
	// FIRST PAGES ONLY. A next page is history and carries no live edge, so its
	// live_join_seq is zero — not because nothing was computed, but because
	// there is nothing for a client to splice onto from the middle of history.
	if first {
		page.LiveJoinSeq = outcome.liveJoinSeq
	}
	return page, nil
}

// fillHistoryPageSlots places the messages DENSELY from message_1 upward.
//
// DENSITY IS THE PRODUCER'S INVARIANT and the wire shape cannot enforce it, so
// it is enforced here: this is the only writer of the slots, a nil message
// never occupies one, and an eleventh message is a LOUD REFUSAL rather than a
// silent truncation. Truncating would deliver a page that claims to be the ten
// newest messages while being something else, and no client could tell.
func fillHistoryPageSlots(page *frontendv1.ConversationHistoryPage, messages []*frontendv1.Message) error {
	if len(messages) > historyPageSlots {
		return fmt.Errorf("the page assembled %d message(s) for %d slots — one event curated to more messages than a page can carry, and truncating it would state the page's width falsely",
			len(messages), historyPageSlots)
	}
	slots := []func(*frontendv1.Message){
		func(m *frontendv1.Message) { page.Message_1 = m },
		func(m *frontendv1.Message) { page.Message_2 = m },
		func(m *frontendv1.Message) { page.Message_3 = m },
		func(m *frontendv1.Message) { page.Message_4 = m },
		func(m *frontendv1.Message) { page.Message_5 = m },
		func(m *frontendv1.Message) { page.Message_6 = m },
		func(m *frontendv1.Message) { page.Message_7 = m },
		func(m *frontendv1.Message) { page.Message_8 = m },
		func(m *frontendv1.Message) { page.Message_9 = m },
		func(m *frontendv1.Message) { page.Message_10 = m },
	}
	for i, msg := range messages {
		if msg == nil {
			// A nil in the middle of the run would leave a hole no slot index
			// can describe, which is precisely the density defect.
			return fmt.Errorf("message %d of %d is nil, and an empty slot below a filled one is unrepresentable in a page", i+1, len(messages))
		}
		slots[i](msg)
	}
	return nil
}

// compile-time proof the SSM's position store really has the shape this package
// asserts off the configured SSM.
var _ ConversationReaderPositions = (*ssm.Manager)(nil)
