package chat

import (
	"strings"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
)

// A THREAD'S NAME IS ITS FIRST QUESTION'S ABBREVIATION (owner ruling,
// 2026-10-26): there is no separate title. A first question short enough to
// be its own abbreviation names the thread as it was asked; a longer one
// names it once its abbreviation is minted, and as it was asked if the
// abbreviation could not be. Two threads that start alike share a name.
//
// THE NAME IS DECIDED UNDER THE LOCK AND APPLIED OFF IT: publish notes the
// name the thread's state calls for (wantName), and one drain per thread
// (drainNames) hands Deps.Name the latest wanted name until the applied one
// matches it, so names are applied in the order they were wanted and never
// with the lock held.

// opName is the naming's log operation.
const opName = "daemon.chat.name"

// OwnName answers QUESTION as a thread's name when it is its own
// abbreviation: its words on one line.
func OwnName(question string) (string, bool) {
	if NeedsAbbreviation(question) {
		return "", false
	}
	return oneLine(question), true
}

// oneLine is a question's words joined by single spaces.
func oneLine(question string) string { return strings.Join(strings.Fields(question), " ") }

// rootName answers the name the thread's first question calls for, "" while
// it calls for none yet (no first question, or its abbreviation not yet
// minted). Called under Service.mu.
func (t *thread) rootName() string {
	for _, id := range t.order {
		if n := t.nodes[id]; parentOf(n) == "" {
			return t.nameOf(ids.TurnID(n.GetTurn().GetTurnId()), n.GetQuestion().GetText(), n.GetQuestion().Abbreviation)
		}
	}
	for _, turn := range t.inFlight() {
		if turn.Question.Parent.GetThreadStart() != nil {
			return t.nameOf(turn.ID, turn.Question.Text, nil)
		}
	}
	for _, h := range t.held {
		if h.Ask.Parent.GetThreadStart() != nil {
			name, _ := OwnName(saidText(h))
			return name
		}
	}
	return ""
}

// nameOf answers the name TURN's QUESTION calls for: its abbreviation, held
// or recorded, or the question itself when it is its own or could not be
// abbreviated; "" while its abbreviation is being minted.
func (t *thread) nameOf(turn ids.TurnID, question string, abbreviation *string) string {
	if abbreviation != nil {
		return *abbreviation
	}
	if minted, ok := t.minted[turn]; ok {
		return minted
	}
	if name, ok := OwnName(question); ok {
		return name
	}
	if t.unabbreviated[turn] {
		return oneLine(question)
	}
	return ""
}

// wantName notes the name the thread now calls for and starts its drain when
// none runs. Called under Service.mu, from publish.
func (s *Service) wantName(t *thread) {
	name := t.rootName()
	if name == "" || name == t.wantedName {
		return
	}
	t.wantedName = name
	if t.naming {
		return
	}
	t.naming = true
	s.drains.Add(1)
	go s.drainNames(t)
}

// drainNames applies the thread's wanted name until the applied one matches
// it. A failure is recorded once and ends the drain; the next wanted name
// starts another.
func (s *Service) drainNames(t *thread) {
	defer s.drains.Done()
	for {
		s.mu.Lock()
		name := t.wantedName
		if name == t.appliedName {
			t.naming = false
			s.mu.Unlock()
			return
		}
		s.mu.Unlock()
		if err := s.deps.Name(s.deps.Context, t.id, name); err != nil {
			s.deps.Log(t.id).Error(opName, "the thread could not be named after its first question", dlog.Context{
				"thread": string(t.id), "name": name, "cause": err.Error(),
			})
			s.mu.Lock()
			t.naming = false
			s.mu.Unlock()
			return
		}
		s.mu.Lock()
		t.appliedName = name
		s.mu.Unlock()
	}
}

// WaitNames waits for every name drain running to finish (tests, and a
// daemon standing down).
func (s *Service) WaitNames() { s.drains.Wait() }
