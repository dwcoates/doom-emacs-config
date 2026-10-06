package main

import (
	"sync"

	conversationv1 "agentrepl/proto/conversation/v1"
	shimv1 "agentrepl/proto/shim/v1"

	"google.golang.org/protobuf/proto"
)

// THE FAKE STORE'S BOOK. The real shim reads history off the store: every
// entry it ever served on a watch is in its agent's book, at the pointer it
// was served at, and ReadHistory pages that book in the store's own page size.
// The daemon loads history ONLY through ReadHistory now (feed paging on
// demand) and opens every watch tail_only or from a pointer it holds, so the
// fake keeps the same book: a resumed session's history, then every entry
// pushed since, each at one pointer every stream and every page agree on.

// DefaultHistoryPageSize is the fake store's page size when the profile
// states none: the real store's.
const DefaultHistoryPageSize = 50

// book is every agent's history, OLDEST FIRST, keyed by agent id.
type book struct {
	mu      sync.Mutex
	entries map[string][]*conversationv1.HistoryEntryAt
	minted  map[string]int
}

func newBook() *book {
	return &book{entries: map[string][]*conversationv1.HistoryEntryAt{}, minted: map[string]int{}}
}

// bookAgent is the book an entry of AGENT belongs to: the main agent's for an
// unnamed one.
func bookAgent(agent string) string {
	if agent == "" {
		return MainAgentID
	}
	return agent
}

// seed files a resumed session's history, stated NEWEST FIRST, into the main
// agent's book. Each entry takes a resume pointer and a recorded place in its
// order, as the store states one for every entry it holds, and the turn TURNS
// names at its index (newest first too; absent or "" is an unstamped entry).
func (b *book) seed(newestFirst [][]byte, turns []string) {
	b.mu.Lock()
	defer b.mu.Unlock()
	main := b.entries[MainAgentID][:0]
	for i := len(newestFirst) - 1; i >= 0; i-- {
		entry := &conversationv1.HistoryEntry{}
		if err := proto.Unmarshal(newestFirst[i], entry); err != nil {
			panic(sprintf("fakeshim: profile resume_history[%d] does not decode: %v", i, err))
		}
		at := &conversationv1.HistoryEntryAt{
			At:    &conversationv1.HistoryPointer{Value: sprintf("resume-%d", i)},
			Entry: entry,
			Place: &conversationv1.HistoryEntryAt_RecordedPlace{RecordedPlace: &conversationv1.ConversationPlace{
				AtMs: int64(len(newestFirst) - i),
			}},
		}
		if i < len(turns) && turns[i] != "" {
			at.Turn = &conversationv1.TurnId{Value: turns[i]}
		}
		main = append(main, at)
	}
	b.entries[MainAgentID] = main
}

// record files one published frame and answers it with the pointer every
// stream delivers it at: its own when the command stated one, else the next
// minted on its agent's book. A retired frame removes its entry, which no
// page serves again.
func (b *book) record(f agentFrame) agentFrame {
	b.mu.Lock()
	defer b.mu.Unlock()
	agent := bookAgent(f.agent)
	if f.retired != nil {
		kept := b.entries[agent][:0]
		for _, at := range b.entries[agent] {
			if at.GetAt().GetValue() != f.retired.GetAt().GetValue() {
				kept = append(kept, at)
			}
		}
		b.entries[agent] = kept
		return f
	}
	if f.pointer == "" {
		b.minted[agent]++
		f.pointer = pointerAt(agent, b.minted[agent])
	}
	b.entries[agent] = append(b.entries[agent], f.place(&conversationv1.HistoryEntryAt{
		At:    &conversationv1.HistoryPointer{Value: f.pointer},
		Entry: f.entry(),
		Turn:  f.stamp(),
	}))
	return f
}

// page answers one ReadHistory page of AGENT's book: the newest page with no
// AFTER, else the entries placed before AFTER. False is a pointer the book
// never served.
func (b *book) page(agent string, after *conversationv1.HistoryPointer, size int) (*conversationv1.HistoryPage, bool) {
	b.mu.Lock()
	defer b.mu.Unlock()
	entries := b.entries[bookAgent(agent)]
	end := len(entries)
	if after != nil {
		end = -1
		for i, at := range entries {
			if at.GetAt().GetValue() == after.GetValue() {
				end = i
			}
		}
		if end < 0 {
			return nil, false
		}
	}
	start := max(end-size, 0)
	page := &conversationv1.HistoryPage{}
	for i := end - 1; i >= start; i-- {
		page.Entries = append(page.Entries, entries[i])
	}
	if start == 0 {
		page.Boundary = &conversationv1.HistoryPage_Floor{Floor: &conversationv1.HistoryFloor{}}
	} else {
		page.Boundary = &conversationv1.HistoryPage_More{More: &conversationv1.HistoryMore{LastEntry: entries[start].GetAt()}}
	}
	return page, true
}

// since answers AGENT's entries written after the one KNOWN names, newest
// first: a known_through catch-up. A pointer the book never served answers
// nothing.
func (b *book) since(agent string, known *conversationv1.HistoryPointer) *conversationv1.HistoryPage {
	b.mu.Lock()
	defer b.mu.Unlock()
	entries := b.entries[bookAgent(agent)]
	page := &conversationv1.HistoryPage{}
	for i := len(entries) - 1; i >= 0; i-- {
		if entries[i].GetAt().GetValue() == known.GetValue() {
			return page
		}
		page.Entries = append(page.Entries, entries[i])
	}
	return &conversationv1.HistoryPage{}
}

// openingPageFor is what a WatchAgent stream opens with for REQ: nothing under
// tail_only, the catch-up under known_through, and — for a request that names
// neither, which the daemon never sends — the repaint the contract defines.
func (s *server) openingPageFor(req *shimv1.WatchAgentRequest) *conversationv1.HistoryPage {
	switch {
	case req.GetTailOnly() != nil:
		return &conversationv1.HistoryPage{}
	case req.GetKnownThrough() != nil:
		return s.book.since(req.GetTarget().GetValue(), req.GetKnownThrough())
	}
	return s.openingPage()
}

// historyPageSize is the fake store's page size.
func (s *server) historyPageSize() int {
	if s.profile.HistoryPageSize > 0 {
		return s.profile.HistoryPageSize
	}
	return DefaultHistoryPageSize
}
