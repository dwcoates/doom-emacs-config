package feed

import (
	"errors"
	"fmt"
	"regexp"
	"strconv"
	"strings"
	"unicode"

	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/feedid"
	"agent-repld/internal/ids"
)

// THE QUESTION DRAFT: what a standing question batch has been given so far,
// and which question the card shows.
//
// ONE RECORD, TWO INPUTS. The webapp's clicks (agentrepl.v1.UpdateQuestionDraft)
// and the editor's typed answer (agentrepl.v1.SubmitPromptAskAnswered) both
// edit the draft held on the ask's questionState, and the open card is drawn
// from it (frontend.v1.FeedQuestionOpen). There is no submit: the edit that
// answers the batch's last question CLAIMS the batch for delivery and hands the
// answers back to its caller, under the resolver's lock, so two edits racing to
// finish the batch cannot both deliver it.

// QuestionEditKind names what one edit does.
type QuestionEditKind int

// The edits.
const (
	// EditShow shows one question (a tab click).
	EditShow QuestionEditKind = iota + 1
	// EditPick clicks one option: a single-select's answer, or a multi-select's
	// tick toggled.
	EditPick
	// EditConfirm confirms a multi-select: its ticks are its answer.
	EditConfirm
	// EditType answers the ACTIVE question with typed text (an editor send).
	// A send of ONLY option numbers (optionNumbers) picks those options
	// instead: the card numbers them from 1, in the served order.
	EditType
)

// String names the kind for the log.
func (k QuestionEditKind) String() string {
	switch k {
	case EditShow:
		return "show"
	case EditPick:
		return "pick"
	case EditConfirm:
		return "confirm"
	case EditType:
		return "type"
	}
	return fmt.Sprintf("QuestionEditKind(%d)", int(k))
}

// QuestionEdit is one edit to a standing batch's draft.
type QuestionEdit struct {
	// Kind is what the edit does.
	Kind QuestionEditKind
	// Question is the served question text the edit names, ECHOED. Unused by
	// EditType, which edits the active question.
	Question string
	// Label is the served option label EditPick clicks, echoed.
	Label string
	// Text is EditType's typed answer.
	Text string
}

// QuestionDraftOutcome is what an applied edit left to do.
type QuestionDraftOutcome struct {
	// Deliver is non-nil when the edit answered the batch's last question: the
	// answers to deliver, and the batch is now CLAIMED. The caller delivers
	// them and, when the delivery fails, releases the claim
	// (ReleaseQuestionDelivery) so the batch can be finished again.
	Deliver *conversationv1.AgentQuestionAnswers
	// Agent is who asked, set with Deliver: the delivery's addressee.
	Agent *conversationv1.AgentId
}

// ErrQuestionNotStanding is an edit to an ask that is not standing — never
// drawn, already settled, or claimed by a delivery in flight.
var ErrQuestionNotStanding = errors.New("feed: the question ask is not standing")

// UnservedQuestionValueError is an edit naming a question text or option label
// the standing batch never served, or confirming a question that is not a
// multi-select.
type UnservedQuestionValueError struct {
	// Text is the unserved value, as echoed.
	Text string
	// Why says what was wrong with it.
	Why string
}

// Error implements error.
func (e *UnservedQuestionValueError) Error() string {
	return fmt.Sprintf("feed: %s: %q", e.Why, e.Text)
}

// AskKind names which card a standing ask is.
type AskKind int

// The kinds.
const (
	// AskPermission is a consent card.
	AskPermission AskKind = iota + 1
	// AskQuestion is a question card.
	AskQuestion
)

// StandingAsk names the ask a typed answer goes to.
type StandingAsk struct {
	// Kind is which card it is.
	Kind AskKind
	// ID is the ask's identity (the conversation ask id).
	ID string
}

// emptyDrafts begins a batch's draft: one empty, unanswered entry per question.
func emptyDrafts(batch *conversationv1.AgentQuestionBatch) []*frontendv1.FeedQuestionDraft {
	drafts := make([]*frontendv1.FeedQuestionDraft, len(batch.GetQuestions()))
	for i := range drafts {
		drafts[i] = &frontendv1.FeedQuestionDraft{}
	}
	return drafts
}

// openState draws the open card's state from the draft.
func openState(served *questionState) *frontendv1.FeedQuestionOpen {
	return &frontendv1.FeedQuestionOpen{
		Active: uint32(served.active),
		Drafts: served.drafts,
	}
}

// EditQuestionDraft applies one edit to a standing batch's draft, re-publishes
// the card, and reports whether the batch is now answered and claimed.
func (r *resolver) EditQuestionDraft(ws ids.WorkspaceID, ask string, edit QuestionEdit) (QuestionDraftOutcome, error) {
	r.mu.Lock()
	defer r.mu.Unlock()
	s := r.state(ws)
	log := r.logger(ws).With(dlog.Context{"ask": ask, "edit": edit.Kind.String()})

	served, ok := s.questionAsks[ask]
	if !ok || !served.open || served.delivering {
		log.Debug("daemon.feed.question_draft_refused", "a draft edit named an ask that is not standing",
			dlog.Context{"known": ok, "open": ok && served.open, "delivering": ok && served.delivering})
		return QuestionDraftOutcome{}, ErrQuestionNotStanding
	}

	index, err := r.applyQuestionEdit(served, edit)
	if err != nil {
		log.Debug("daemon.feed.question_draft_refused", "a draft edit named a value the batch never served",
			dlog.Context{"cause": err.Error()})
		return QuestionDraftOutcome{}, err
	}

	outcome := QuestionDraftOutcome{}
	if edit.Kind != EditShow && served.drafts[index].GetAnswered() {
		if next, pending := nextUnanswered(served.drafts, index); pending {
			served.active = next
		} else {
			served.delivering = true
			outcome = QuestionDraftOutcome{Deliver: draftAnswers(served), Agent: served.agent}
		}
	}
	r.publishOpenQuestion(s, served)
	r.refreshHostAsk(s)
	_, numbered := optionNumbers(edit.Text)
	log.Info("daemon.feed.question_draft", "applied a draft edit", dlog.Context{
		"question": index, "active": served.active, "deliver": outcome.Deliver != nil,
		"by_number": edit.Kind == EditType && numbered,
	})
	return outcome, nil
}

// ReleaseQuestionDelivery returns a claimed batch to editable after its
// delivery failed: the draft stands as it was, so the next edit that leaves it
// answered delivers it again.
func (r *resolver) ReleaseQuestionDelivery(ws ids.WorkspaceID, ask string) {
	r.mu.Lock()
	defer r.mu.Unlock()
	s := r.state(ws)
	served, ok := s.questionAsks[ask]
	if !ok || !served.open {
		// THE ASK SETTLED MEANWHILE (its success or failure frame landed
		// between the claim and the failed delivery), so there is no claim left
		// to release and the settled card stands.
		r.logger(ws).Info("daemon.feed.question_draft_release", "a released delivery's ask had already settled",
			dlog.Context{"ask": ask, "known": ok})
		return
	}
	served.delivering = false
	r.refreshHostAsk(s)
	r.logger(ws).Info("daemon.feed.question_draft_release", "released a failed delivery's claim on the batch",
		dlog.Context{"ask": ask})
}

// StandingAsk answers the OLDEST ask standing on the feed a submission
// addresses — the root feed when target is nil, else the sub-feed whose head
// row target names — that a typed answer can go to. A question batch claimed
// by a delivery in flight is not standing for this purpose: its answers are
// already on their way.
func (r *resolver) StandingAsk(ws ids.WorkspaceID, target *feedid.Ref) (StandingAsk, bool) {
	r.mu.Lock()
	defer r.mu.Unlock()
	s := r.state(ws)
	addressed, ok := r.addressedFeedKey(s, target)
	if !ok {
		return StandingAsk{}, false
	}

	best, found := r.oldestStandingAsk(s, addressed)
	r.logger(ws).Debug("daemon.feed.standing_ask", "looked for a standing ask on the addressed feed",
		dlog.Context{"feed": addressed, "found": found, "ask": best.ID})
	return best, found
}

// oldestStandingAsk answers the OLDEST ask standing on the feed keyed
// addressed: an open consent card, or an open question batch that is drawn and
// not claimed by a delivery in flight. It is the one rule for which ask a
// user's answer goes to, read by the typed-answer routing (StandingAsk) and
// the host view's numbered answer (hostask.go) alike.
func (r *resolver) oldestStandingAsk(s *wsState, addressed string) (StandingAsk, bool) {
	var best StandingAsk
	var bestOpened uint64
	found := false
	consider := func(kind AskKind, id string, opened uint64) {
		if !found || opened < bestOpened {
			best, bestOpened, found = StandingAsk{Kind: kind, ID: id}, opened, true
		}
	}
	for id, p := range s.permissionRows {
		if p.card.GetOpen() != nil && r.feedKey(s.id, p.feed.feed) == addressed {
			consider(AskPermission, id, p.opened)
		}
	}
	for id, q := range s.questionAsks {
		if q.open && !q.delivering && q.row != nil && r.feedKey(s.id, q.at.feed) == addressed {
			consider(AskQuestion, id, q.opened)
		}
	}
	return best, found
}

// addressedFeedKey answers the key of the feed a submission addresses: the
// root's when target is nil, else the sub-feed whose head row is target. A
// target naming no recorded head addresses no feed asks can stand on.
func (r *resolver) addressedFeedKey(s *wsState, target *feedid.Ref) (string, bool) {
	if target == nil {
		return r.feedKey(s.id, feedid.Feed{Root: true}), true
	}
	headRow := r.deps.Encode(*target).GetValue()
	for key, head := range s.subFeeds {
		if head.row.GetValue() == headRow {
			return key, true
		}
	}
	r.logger(s.id).Debug("daemon.feed.standing_ask", "the addressed head row owns no recorded sub-feed",
		dlog.Context{"target": headRow})
	return "", false
}

// applyQuestionEdit mutates the draft for one edit and answers the index of the
// question it touched.
func (r *resolver) applyQuestionEdit(served *questionState, edit QuestionEdit) (int, error) {
	if edit.Kind == EditType {
		if numbers, ok := optionNumbers(edit.Text); ok {
			return served.active, pickByNumber(served, edit.Text, numbers)
		}
		draft := served.drafts[served.active]
		if served.batch.GetQuestions()[served.active].GetSingleSelect() != nil {
			// A typed answer to a single-select REPLACES the pick: it is the
			// answer instead of one of the options.
			draft.Chosen = nil
		}
		draft.OtherText = &frontendv1.FeedQuestionOtherText{Text: edit.Text}
		draft.Answered = true
		return served.active, nil
	}

	index, asked, err := servedQuestion(served.batch, edit.Question)
	if err != nil {
		return 0, err
	}
	draft := served.drafts[index]
	single := asked.GetSingleSelect() != nil

	switch edit.Kind {
	case EditShow:
		served.active = index
	case EditPick:
		labels := servedLabels(asked)
		if !containsLabel(labels, edit.Label) {
			return 0, &UnservedQuestionValueError{Text: edit.Label, Why: "the question never offered the option"}
		}
		if single {
			if len(draft.Chosen) == 1 && draft.Chosen[0] == edit.Label {
				// A click on the option already chosen CLEARS it.
				draft.Chosen = nil
				draft.Answered = draft.OtherText != nil
			} else {
				draft.Chosen = []string{edit.Label}
				draft.OtherText = nil
				draft.Answered = true
			}
		} else {
			draft.Chosen = toggleLabel(labels, draft.Chosen, edit.Label)
			// A TICK IS NOT AN ANSWER: a multi-select has no moment its picking
			// is finished, so it waits for its confirm.
			draft.Answered = false
		}
		served.active = index
	case EditConfirm:
		if single {
			return 0, &UnservedQuestionValueError{Text: edit.Question, Why: "only a multi-select question is confirmed"}
		}
		// A confirm ALWAYS answers: with nothing ticked or typed, the answer is
		// "none of these", delivered as an empty selection.
		draft.Answered = true
		served.active = index
	default:
		panic(fmt.Sprintf("feed: unhandled question edit kind %v", edit.Kind))
	}
	return index, nil
}

// optionNumbersPattern is a send of only option numbers: one, or several
// delimited by whitespace or a comma (with or without whitespace after it).
var optionNumbersPattern = regexp.MustCompile(`^[0-9]+(?:(?:,\s*|\s+)[0-9]+)*$`)

// optionNumbers reads TEXT, trimmed, as a send of option numbers and answers
// them in the order typed. Anything else in the send makes it typed text.
func optionNumbers(text string) ([]string, bool) {
	trimmed := strings.TrimSpace(text)
	if !optionNumbersPattern.MatchString(trimmed) {
		return nil, false
	}
	return strings.FieldsFunc(trimmed, func(r rune) bool {
		return r == ',' || unicode.IsSpace(r)
	}), true
}

// pickByNumber answers the ACTIVE question with the options NUMBERS name,
// 1-based in the served order: a single-select's one pick, or a multi-select's
// whole set of ticks. Either way the send IS the answer, so it replaces any
// typed text and answers the question. Every number is checked before the
// draft is touched, so a refused send leaves the draft as it was.
func pickByNumber(served *questionState, text string, numbers []string) error {
	asked := served.batch.GetQuestions()[served.active]
	labels := servedLabels(asked)
	picked := map[string]bool{}
	for _, number := range numbers {
		n, err := strconv.Atoi(number)
		if err != nil || n < 1 || n > len(labels) {
			return &UnservedQuestionValueError{
				Text: text,
				Why:  fmt.Sprintf("the question offers no option numbered %s of %d", number, len(labels)),
			}
		}
		picked[labels[n-1]] = true
	}
	if asked.GetSingleSelect() != nil && len(picked) > 1 {
		return &UnservedQuestionValueError{Text: text, Why: "a single-select question takes one option number"}
	}
	draft := served.drafts[served.active]
	draft.Chosen = inServedOrder(labels, picked)
	draft.OtherText = nil
	draft.Answered = true
	return nil
}

// servedQuestion finds a question of the batch by its served text.
func servedQuestion(batch *conversationv1.AgentQuestionBatch, text string) (int, *conversationv1.AgentQuestionAsked, error) {
	for i, asked := range batch.GetQuestions() {
		if asked.GetQuestion().GetText() == text {
			return i, asked, nil
		}
	}
	return 0, nil, &UnservedQuestionValueError{Text: text, Why: "the batch never served the question"}
}

// servedLabels answers a question's option labels, in the served order.
func servedLabels(asked *conversationv1.AgentQuestionAsked) []string {
	offered := asked.GetSingleSelect().GetOptions()
	if multi := asked.GetMultiSelect(); multi != nil {
		offered = multi.GetOptions()
	}
	labels := make([]string, 0, len(offered))
	for _, option := range offered {
		labels = append(labels, option.GetLabel().GetLabel())
	}
	return labels
}

// containsLabel reports whether labels holds label.
func containsLabel(labels []string, label string) bool {
	for _, l := range labels {
		if l == label {
			return true
		}
	}
	return false
}

// toggleLabel flips label in chosen and answers the result in the SERVED order,
// so the draft never depends on the order of the clicks.
func toggleLabel(served, chosen []string, label string) []string {
	ticked := map[string]bool{}
	for _, c := range chosen {
		ticked[c] = true
	}
	ticked[label] = !ticked[label]
	return inServedOrder(served, ticked)
}

// inServedOrder answers the SERVED labels that are set in held, in the served
// order: the one way a question's chosen labels are ordered, so a draft never
// depends on the order of the clicks or of the typed numbers.
func inServedOrder(served []string, held map[string]bool) []string {
	var out []string
	for _, l := range served {
		if held[l] {
			out = append(out, l)
		}
	}
	return out
}

// nextUnanswered answers the first unanswered question after from, wrapping,
// and false when every question is answered.
func nextUnanswered(drafts []*frontendv1.FeedQuestionDraft, from int) (int, bool) {
	n := len(drafts)
	for step := 1; step <= n; step++ {
		i := (from + step) % n
		if !drafts[i].GetAnswered() {
			return i, true
		}
	}
	return 0, false
}

// draftAnswers builds the delivery from a fully answered draft: one selection
// per question, naming it by its served text.
func draftAnswers(served *questionState) *conversationv1.AgentQuestionAnswers {
	answers := &conversationv1.AgentQuestionAnswers{}
	for i, asked := range served.batch.GetQuestions() {
		draft := served.drafts[i]
		var other *string
		if draft.OtherText != nil {
			text := draft.GetOtherText().GetText()
			other = &text
		}
		answers.Answers = append(answers.Answers, QuestionSelection(asked.GetQuestion().GetText(), draft.GetChosen(), other))
	}
	return answers
}

// QuestionSelection builds ONE question's answer for the producer: the question
// named by its served text, the chosen labels in order, and the typed answer
// when other is non-nil (nil is "nothing typed", never the empty string). It
// is the one builder of the shape, for the card's own draft and for a client's
// whole-batch answer alike.
func QuestionSelection(question string, chosen []string, other *string) *conversationv1.AgentQuestionSelection {
	selection := &conversationv1.AgentQuestionSelection{
		Question: &conversationv1.AgentQuestionText{Text: question},
	}
	for _, label := range chosen {
		selection.Chosen = append(selection.Chosen, &conversationv1.AgentQuestionChoice{
			Label: &conversationv1.AgentQuestionOptionLabel{Label: label},
		})
	}
	if other != nil {
		selection.FreeText = &conversationv1.AgentQuestionFreeText{Text: *other}
	}
	return selection
}

// publishOpenQuestion re-publishes a standing card from its draft, in place.
func (r *resolver) publishOpenQuestion(s *wsState, served *questionState) {
	row := &frontendv1.FeedRow{
		Id: served.row,
		Row: &frontendv1.FeedRow_Question{Question: &frontendv1.FeedQuestion{
			Questions: questionItems(served.batch),
			State:     &frontendv1.FeedQuestion_Open{Open: openState(served)},
			Clock:     runningClock(served.startedAtMs),
		}},
	}
	r.stampTurn(s, row, served.turn)
	r.upsert(s, served.at, row, true)
}
