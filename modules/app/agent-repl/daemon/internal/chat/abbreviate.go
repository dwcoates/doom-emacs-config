package chat

import (
	"context"
	"fmt"
	"strings"
	"time"
	"unicode/utf8"

	"agent-repld/internal/account"
	"agent-repld/internal/dlog"
	"agent-repld/internal/headless"
	"agent-repld/internal/ids"
	"agent-repld/internal/prompts"

	zettelv1 "agentrepl/proto/zettel/v1"
)

// A QUESTION'S ABBREVIATION (owner ruling, 2026-10-26): the few words the
// sidebar's branch outline draws for a question too long to draw whole
// (zettel.v1.NodeQuestion.abbreviation). A question of at most
// AbbreviationMaxWords words and AbbreviationMaxChars characters on one line
// is its own and is never abbreviated. Every longer one is abbreviated by a
// headless model call, started as soon as this service sees the question
// (its open turn, or its node without one), so it usually rides the node's
// recording; one that arrives after is set on the node (Service.minted).

const (
	// BriefQuestionAbbreviation is the abbreviation question's brief, from
	// the prompts corpus.
	BriefQuestionAbbreviation = "chat-question-abbreviation"
	// AbbreviationSite is the vendor-guard site the call asks under.
	AbbreviationSite = "chat_abbreviation"
	// AbbreviationMaxWords and AbbreviationMaxChars bound an abbreviation,
	// and are the most a question may have to be its own.
	AbbreviationMaxWords = 6
	AbbreviationMaxChars = 40
	// AbbreviationAttempts is how many times the call is made: the first,
	// plus one retry on an invalid or failed answer.
	AbbreviationAttempts = 2
	// AbbreviationTimeout bounds one attempt: a handful of tokens from a
	// small model, as the thread title's call (workspace.NamingTimeout).
	AbbreviationTimeout = 15 * time.Second
	// AbbreviationSlots is how many abbreviation calls run at once, so a
	// thread loaded with many unabbreviated questions does not fan out one
	// model process per question.
	AbbreviationSlots = 2
	// opAbbreviate is the abbreviation's log operation.
	opAbbreviate = "daemon.chat.abbreviate"
)

// NeedsAbbreviation reports whether QUESTION is too long to be its own
// abbreviation.
func NeedsAbbreviation(question string) bool {
	q := strings.TrimSpace(question)
	return strings.ContainsAny(q, "\n\r") || len(strings.Fields(q)) > AbbreviationMaxWords ||
		utf8.RuneCountInString(strings.Join(strings.Fields(q), " ")) > AbbreviationMaxChars
}

// ValidateAbbreviation answers ANSWER as an abbreviation: trimmed of
// surrounding quotes and a trailing full stop, one line, one to
// AbbreviationMaxWords words, at most AbbreviationMaxChars characters.
func ValidateAbbreviation(answer string) (string, error) {
	return headless.Words{Noun: "an abbreviation", MinWords: 1, MaxWords: AbbreviationMaxWords, MaxChars: AbbreviationMaxChars}.Validate(answer)
}

// Abbreviator mints a question's abbreviation.
type Abbreviator interface {
	// Abbreviate answers QUESTION's abbreviation, asked in thread WS. LOG
	// takes each attempt's record; the caller records the failure.
	Abbreviate(ctx context.Context, log dlog.Logger, ws ids.WorkspaceID, question string) (string, error)
}

// HeadlessAbbreviator is the production Abbreviator: the brief, asked of
// Haiku through headless.AskValidated.
type HeadlessAbbreviator struct {
	Headless   headless.Runner
	ConfigDirs account.WorkspaceConfigDirs
	PromptsDir string
	// Load reads a brief; nil is prompts.Load.
	Load func(dir, name string) (prompts.Prompt, error)
}

// Abbreviate implements Abbreviator.
func (a HeadlessAbbreviator) Abbreviate(ctx context.Context, log dlog.Logger, ws ids.WorkspaceID, question string) (string, error) {
	load := a.Load
	if load == nil {
		load = prompts.Load
	}
	brief, err := load(a.PromptsDir, BriefQuestionAbbreviation)
	if err != nil {
		return "", fmt.Errorf("the %s brief is unreadable: %w", BriefQuestionAbbreviation, err)
	}
	prompt, err := brief.Splice(map[string]string{"question": question})
	if err != nil {
		return "", fmt.Errorf("the %s brief will not splice: %w", BriefQuestionAbbreviation, err)
	}
	configDir, err := a.ConfigDirs.ConfigDirFor(ctx, ws)
	if err != nil {
		return "", fmt.Errorf("no account root for the thread: %w", err)
	}
	value, failure := headless.AskValidated(ctx, a.Headless, headless.Ask{
		Request: headless.Request{
			Site: AbbreviationSite, Model: headless.ModelHaiku, Format: headless.FormatJSON,
			ConfigDir: configDir, Prompt: prompt, Timeout: AbbreviationTimeout,
		},
		Attempts: AbbreviationAttempts,
		Validate: ValidateAbbreviation,
	}, log, opAbbreviate)
	if failure != nil {
		return "", failure
	}
	return value, nil
}

// ---- The service's abbreviations ----

// abbreviation is one question owed its abbreviation.
type abbreviation struct {
	thread   ids.WorkspaceID
	turn     ids.TurnID
	question string
}

// owedAbbreviations answers every question of the thread that needs an
// abbreviation and has not been asked for one, marking each asked: its open
// turns' and its nodes' without one. Called under Service.mu.
func (t *thread) owedAbbreviations() []abbreviation {
	var owed []abbreviation
	owe := func(turn ids.TurnID, question string) {
		if t.abbreviating[turn] || !NeedsAbbreviation(question) {
			return
		}
		t.abbreviating[turn] = true
		owed = append(owed, abbreviation{thread: t.id, turn: turn, question: question})
	}
	for _, turn := range t.open {
		if n := t.nodeOfTurn(turn.ID); n == nil || n.GetQuestion().Abbreviation == nil {
			owe(turn.ID, turn.Question.Text)
		}
	}
	for _, id := range t.order {
		if n := t.nodes[id]; n.GetQuestion().Abbreviation == nil {
			owe(ids.TurnID(n.GetTurn().GetTurnId()), n.GetQuestion().GetText())
		}
	}
	return owed
}

// abbreviateAll runs each owed abbreviation as its own job (Deps.Go).
func (s *Service) abbreviateAll(owed []abbreviation) {
	for _, a := range owed {
		s.deps.Go(func() { s.abbreviate(s.deps.Context, a) })
	}
}

// abbreviate mints A's abbreviation and delivers it: held for the turn's
// recording while the turn is no node yet, else set on its node. A failure
// is recorded here, once, and leaves the question drawn whole until the
// next daemon asks again.
func (s *Service) abbreviate(ctx context.Context, a abbreviation) {
	s.slots <- struct{}{}
	value, err := s.deps.Abbreviator.Abbreviate(ctx, s.deps.Log(a.thread), a.thread, a.question)
	<-s.slots
	log := s.deps.Log(a.thread)
	if err != nil {
		log.Error(opAbbreviate, "the question could not be abbreviated; the outline draws it whole", dlog.Context{
			"thread": string(a.thread), "turn": string(a.turn), "question_chars": utf8.RuneCountInString(a.question), "cause": err.Error(),
		})
		return
	}
	s.mu.Lock()
	t := s.threads[a.thread]
	node := t.nodeOfTurn(a.turn)
	if node == nil {
		t.minted[a.turn] = value
		s.mu.Unlock()
		log.Debug(opAbbreviate, "the question was abbreviated before its node exists; the recording takes it", dlog.Context{
			"thread": string(a.thread), "turn": string(a.turn), "abbreviation": value,
		})
		return
	}
	s.mu.Unlock()
	if node.GetQuestion().Abbreviation != nil {
		log.Debug(opAbbreviate, "the question's node already holds an abbreviation", dlog.Context{
			"thread": string(a.thread), "turn": string(a.turn), "abbreviation": node.GetQuestion().GetAbbreviation(),
		})
		return
	}
	s.setAbbreviation(ctx, a.thread, a.turn, node.GetId(), value)
}

// takeMinted answers and forgets the abbreviation held for TURN, nil when
// none is.
func (s *Service) takeMinted(ws ids.WorkspaceID, turn ids.TurnID) *string {
	s.mu.Lock()
	defer s.mu.Unlock()
	t, ok := s.threads[ws]
	if !ok {
		return nil
	}
	value, held := t.minted[turn]
	if !held {
		return nil
	}
	delete(t.minted, turn)
	return &value
}

// holdMinted puts back an abbreviation takeMinted took for a recording that
// failed, so the turn's next recording takes it.
func (s *Service) holdMinted(ws ids.WorkspaceID, turn ids.TurnID, value *string) {
	if value == nil {
		return
	}
	s.mu.Lock()
	defer s.mu.Unlock()
	s.state(ws).minted[turn] = *value
}

// setAbbreviation sets a recorded node's abbreviation in zettel and keeps
// the node as zettel answers it.
func (s *Service) setAbbreviation(ctx context.Context, ws ids.WorkspaceID, turn ids.TurnID, node *zettelv1.NodeId, value string) {
	log := s.deps.Log(ws)
	updated, err := s.deps.Zettel.SetQuestionAbbreviation(ctx, node, value)
	if err != nil {
		log.Error(opAbbreviate, "the question's abbreviation could not be set in zettel; the outline draws it whole", dlog.Context{
			"thread": string(ws), "turn": string(turn), "node": node.GetId(), "abbreviation": value, "cause": err.Error(),
		})
		return
	}
	log.Info(opAbbreviate, "the question was abbreviated", dlog.Context{
		"thread": string(ws), "turn": string(turn), "node": node.GetId(), "abbreviation": value,
	})
	s.mu.Lock()
	defer s.mu.Unlock()
	if t, ok := s.threads[ws]; ok && t.loaded {
		t.add(updated)
		s.publish(t)
	}
}
