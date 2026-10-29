// Package history renders the store's pages in the conversation vocabulary a
// feed draws (conversation.v1.HistoryPage): THE ONE PLACE A STORE PAGE BECOMES
// A HISTORY PAGE for the store's read-only face (storeread.v1).
//
// It is a translation and nothing else: every function maps one store message
// to its history twin, and remembers nothing. A store message that cannot be
// drawn -- a line with no item, a page with no boundary -- is an error, never
// a blank row or a guessed arm, because forwarding it would put a hole in
// someone's feed.
//
// THE SHIM'S LIVE TAIL MAPS LINES THE SAME WAY in TypeScript
// (agent-shim/claude/shim/src/store/reader.ts, toHistoryEntryAt). A page and a
// tail must render one stored line identically; history_test.go's golden
// cases are shared with the shim's reader tests through testdata, so the two
// copies cannot drift apart unnoticed.
package history

import (
	"errors"
	"fmt"

	conversationv1 "agentrepl/proto/conversation/v1"
	storev1 "agentrepl/proto/store/v1"
)

// ErrUndrawable is a store page holding a message no history page can carry.
var ErrUndrawable = errors.New("history: the store served a page that cannot be drawn")

// Page renders one store page: its lines, the prompts of the turns it touches
// that sit below it, and whether older lines remain. The opening page
// (storev1.AgentSessionPage) and a continuation (storev1.ReadAgentPageSuccess)
// both carry these three, so both are rendered here.
func Page(lines []*storev1.StoreLineAt, heads []*storev1.StoreTurnHead, more *storev1.ReadAgentPageMore, floor bool) (*conversationv1.HistoryPage, error) {
	page := &conversationv1.HistoryPage{}
	for _, line := range lines {
		entry, err := EntryAt(line)
		if err != nil {
			return nil, err
		}
		page.Entries = append(page.Entries, entry)
	}
	for _, head := range heads {
		rendered, err := turnHead(head)
		if err != nil {
			return nil, err
		}
		page.Heads = append(page.Heads, rendered)
	}
	switch {
	case more != nil && floor:
		return nil, fmt.Errorf("%w: the page says both that older lines remain and that it reached the floor", ErrUndrawable)
	case more != nil:
		if more.GetLastItem() == nil || more.GetLastItem().GetValue() == "" {
			return nil, fmt.Errorf("%w: the page says older lines remain but names no pointer to walk from", ErrUndrawable)
		}
		page.Boundary = &conversationv1.HistoryPage_More{More: &conversationv1.HistoryMore{LastEntry: pointer(more.GetLastItem())}}
	case floor:
		page.Boundary = &conversationv1.HistoryPage_Floor{Floor: &conversationv1.HistoryFloor{}}
	default:
		return nil, fmt.Errorf("%w: the page has no boundary arm set", ErrUndrawable)
	}
	return page, nil
}

// OpeningPage renders the store's opening page.
func OpeningPage(page *storev1.AgentSessionPage) (*conversationv1.HistoryPage, error) {
	if page == nil {
		return nil, fmt.Errorf("%w: the store produced no page", ErrUndrawable)
	}
	return Page(page.GetLines(), page.GetHeads(), page.GetMore(), page.GetFloor() != nil)
}

// ContinuationPage renders a page read below a pointer or through an instant.
func ContinuationPage(page *storev1.ReadAgentPageSuccess) (*conversationv1.HistoryPage, error) {
	if page == nil {
		return nil, fmt.Errorf("%w: the store produced no page", ErrUndrawable)
	}
	return Page(page.GetLines(), page.GetHeads(), page.GetMore(), page.GetFloor() != nil)
}

// EntryAt renders one stored line with its pointer, its turn and its place.
//
// THE PLACE ARM PASSES THROUGH UNCHANGED: the store's recorded place is one a
// producer stated, its received place the store's receipt instant standing in,
// and HistoryEntryAt spells the same two arms. A line the store served
// unplaced is passed through unplaced, which a reader orders by its own
// receipt.
func EntryAt(line *storev1.StoreLineAt) (*conversationv1.HistoryEntryAt, error) {
	if line.GetAt() == nil || line.GetAt().GetValue() == "" || line.GetLine() == nil {
		return nil, fmt.Errorf("%w: a line has no pointer or no content", ErrUndrawable)
	}
	entry, err := Entry(line.GetLine())
	if err != nil {
		return nil, err
	}
	out := &conversationv1.HistoryEntryAt{At: pointer(line.GetAt()), Entry: entry, Turn: line.GetTurn()}
	switch place := line.GetPlace().(type) {
	case *storev1.StoreLineAt_RecordedPlace:
		out.Place = &conversationv1.HistoryEntryAt_RecordedPlace{RecordedPlace: place.RecordedPlace}
	case *storev1.StoreLineAt_ReceivedPlace:
		out.Place = &conversationv1.HistoryEntryAt_ReceivedPlace{ReceivedPlace: place.ReceivedPlace}
	case nil:
	default:
		return nil, fmt.Errorf("%w: a line's place arm is one this store does not name (%T)", ErrUndrawable, place)
	}
	return out, nil
}

// Entry renders one stored line's item: a prompt, a frame or a peer message.
func Entry(line *storev1.StorePageLine) (*conversationv1.HistoryEntry, error) {
	switch item := line.GetAgentItem().GetItem().(type) {
	case *storev1.StoreAgentItem_AgentPrompt:
		return &conversationv1.HistoryEntry{Entry: &conversationv1.HistoryEntry_UserPrompt{UserPrompt: item.AgentPrompt}}, nil
	case *storev1.StoreAgentItem_AgentFrame:
		return &conversationv1.HistoryEntry{Entry: &conversationv1.HistoryEntry_AgentFrame{AgentFrame: item.AgentFrame}}, nil
	case *storev1.StoreAgentItem_PeerMessage:
		return &conversationv1.HistoryEntry{Entry: &conversationv1.HistoryEntry_PeerMessage{PeerMessage: item.PeerMessage}}, nil
	case nil:
		return nil, fmt.Errorf("%w: a line's item arm is unset", ErrUndrawable)
	default:
		return nil, fmt.Errorf("%w: a line's item arm is one this store does not name (%T)", ErrUndrawable, item)
	}
}

// turnHead renders the prompt of a turn the page touches whose prompt sits
// below it, with whether steps sit between that prompt and the page.
func turnHead(head *storev1.StoreTurnHead) (*conversationv1.HistoryTurnHead, error) {
	if head.GetLine() == nil {
		return nil, fmt.Errorf("%w: a turn head has no prompt line", ErrUndrawable)
	}
	entry, err := EntryAt(head.GetLine())
	if err != nil {
		return nil, err
	}
	out := &conversationv1.HistoryTurnHead{Entry: entry}
	switch head.GetBetween().(type) {
	case *storev1.StoreTurnHead_Adjacent:
		out.Between = &conversationv1.HistoryTurnHead_Adjacent{Adjacent: &conversationv1.HistoryTurnHeadAdjacent{}}
	case *storev1.StoreTurnHead_StepsBetween:
		out.Between = &conversationv1.HistoryTurnHead_StepsBetween{StepsBetween: &conversationv1.HistoryTurnHeadStepsBetween{}}
	default:
		return nil, fmt.Errorf("%w: a turn head has no between arm set", ErrUndrawable)
	}
	return out, nil
}

// pointer is the store's pointer as the history pointer a reader echoes.
func pointer(at *storev1.StoreItemPointer) *conversationv1.HistoryPointer {
	return &conversationv1.HistoryPointer{Value: at.GetValue()}
}
