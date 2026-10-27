package main

import (
	"sync"

	conversationv1 "agentrepl/proto/conversation/v1"
	shimv1 "agentrepl/proto/shim/v1"

	"google.golang.org/protobuf/proto"

	"agent-repld/integration/fakestore"
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
const DefaultHistoryPageSize = fakestore.DefaultPageSize

// book is every agent's history, OLDEST FIRST, keyed by agent id.
type book struct {
	mu      sync.Mutex
	entries map[string][]*conversationv1.HistoryEntryAt
	minted  map[string]int
	// dir is where every change of the book is kept for the fake store
	// (fakestore), and mainBook the name the main agent's book is kept, and
	// stated, under: unique per workspace, as the real book is. Empty dir keeps
	// nothing.
	dir      string
	mainBook string
	size     int
	// asks is every ask a published frame opened and no later frame closed,
	// in the order they opened: what the real shim's permission gate holds
	// pending, and so what a re-announcement states as open_asks.
	asks []*conversationv1.SessionOpenAsk
}

func newBook() *book {
	return &book{entries: map[string][]*conversationv1.HistoryEntryAt{}, minted: map[string]int{}}
}

// mainBookFor is the book the fake shim of workspace WS states and keeps its
// main agent's history under: the agent id the frames carry is the one
// MainAgentID every workspace shares, and the book is the workspace's own.
func mainBookFor(ws string) string {
	return fakestore.MainBook(ws)
}

// keepIn has every change of the book kept under DIR for the fake store, the
// main agent's book as MAINBOOK, paged at SIZE, and keeps the book as it
// stands now.
func (b *book) keepIn(dir, mainBook string, size int) {
	b.mu.Lock()
	defer b.mu.Unlock()
	b.dir, b.mainBook, b.size = dir, mainBook, size
	for agent := range b.entries {
		b.keepLocked(agent)
	}
	b.keepLocked(MainAgentID)
}

// keepLocked keeps AGENT's book for the fake store. A book that cannot be kept
// panics: a reader of the store would be served a conversation that is not
// this one, which is a broken fixture, never a test outcome.
func (b *book) keepLocked(agent string) {
	if b.dir == "" {
		return
	}
	name := agent
	if agent == MainAgentID {
		name = b.mainBook
	}
	if err := fakestore.Write(b.dir, name, b.size, b.entries[agent]); err != nil {
		panic(sprintf("fakeshim: keep book %q for the fake store: %v", name, err))
	}
}

// stated is the book the fake shim names for its workspace, empty while it
// keeps none.
func (b *book) stated() string {
	b.mu.Lock()
	defer b.mu.Unlock()
	return b.mainBook
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
	main, err := fakestore.ResumeEntries(newestFirst, turns)
	if err != nil {
		panic(sprintf("fakeshim: profile resume_history: %v", err))
	}
	b.entries[MainAgentID] = main
	b.keepLocked(MainAgentID)
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
		b.keepLocked(agent)
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
	b.keepLocked(agent)
	b.noteAskLocked(f.frame)
	return f
}

// noteAskLocked opens the ask a frame starts and closes the one any other
// result of it settles, as the real shim's gate holds an ask from its start
// until its answer.
func (b *book) noteAskLocked(frame *conversationv1.AgentFrame) {
	update := frame.GetUpdate()
	var open *conversationv1.SessionOpenAsk
	var id string
	switch {
	case update.GetPermission() != nil:
		id = "permission:" + update.GetPermission().GetId().GetValue()
		if _, start := update.GetPermission().GetResult().(*conversationv1.AgentPermission_Start); start {
			open = &conversationv1.SessionOpenAsk{Agent: frame.GetAgentId(), Ask: &conversationv1.SessionOpenAsk_Permission{
				Permission: proto.Clone(update.GetPermission()).(*conversationv1.AgentPermission)}}
		}
	case update.GetQuestion() != nil:
		id = "question:" + update.GetQuestion().GetId().GetValue()
		if _, start := update.GetQuestion().GetResult().(*conversationv1.AgentQuestion_Start); start {
			open = &conversationv1.SessionOpenAsk{Agent: frame.GetAgentId(), Ask: &conversationv1.SessionOpenAsk_Question{
				Question: proto.Clone(update.GetQuestion()).(*conversationv1.AgentQuestion)}}
		}
	default:
		return
	}
	kept := b.asks[:0]
	for _, ask := range b.asks {
		if askKey(ask) != id {
			kept = append(kept, ask)
		}
	}
	b.asks = kept
	if open != nil {
		b.asks = append(b.asks, open)
	}
}

// askKey is an open ask's identity across its two kinds.
func askKey(ask *conversationv1.SessionOpenAsk) string {
	if ask.GetPermission() != nil {
		return "permission:" + ask.GetPermission().GetId().GetValue()
	}
	return "question:" + ask.GetQuestion().GetId().GetValue()
}

// openAsks answers every ask open now, in the order they opened.
func (b *book) openAsks() []*conversationv1.SessionOpenAsk {
	b.mu.Lock()
	defer b.mu.Unlock()
	out := make([]*conversationv1.SessionOpenAsk, 0, len(b.asks))
	for _, ask := range b.asks {
		out = append(out, proto.Clone(ask).(*conversationv1.SessionOpenAsk))
	}
	return out
}

// page answers one ReadHistory page of AGENT's book: the newest page with no
// AFTER, else the entries placed before AFTER. False is a pointer the book
// never served. The fake store pages its kept copy the same way.
func (b *book) page(agent string, after *conversationv1.HistoryPointer, size int) (*conversationv1.HistoryPage, bool) {
	b.mu.Lock()
	defer b.mu.Unlock()
	return fakestore.Page(b.entries[bookAgent(agent)], after, size)
}

// pageThrough answers one ReadHistory page of AGENT's book read THROUGH an
// instant, as the store's bounded read serves a fork's inherited past.
func (b *book) pageThrough(agent string, throughMs int64, size int) *conversationv1.HistoryPage {
	b.mu.Lock()
	defer b.mu.Unlock()
	return fakestore.PageThrough(b.entries[bookAgent(agent)], throughMs, size)
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
