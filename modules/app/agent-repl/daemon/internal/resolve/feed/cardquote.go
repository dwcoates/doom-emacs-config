package feed

import (
	"google.golang.org/protobuf/encoding/protojson"

	frontendv1 "agentrepl/proto/frontend/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/protoarm"
)

// A CARD ASKED ABOUT. Every card the feed draws carries a question-mark
// control (endpoint_ask_about_response.proto), so the question about it must
// be able to quote any card on any of the workspace's feeds: the root feed,
// a subagent's, a merge's, a shell's.

// CardKind is what a quoted card is, so the question's preamble can say so.
type CardKind int

const (
	// CardResponse is a landed response bubble of the agent's.
	CardResponse CardKind = iota
	// CardPrompt is a prompt bubble: a user's, or one agent's to another.
	CardPrompt
	// CardRow is every other card, quoted as the row it was drawn from.
	CardRow
)

// CardText is one card as a question quotes it.
type CardText struct {
	// Kind is what the card is.
	Kind CardKind
	// Arm is the FeedRow arm the card was drawn from (`activity`,
	// `permission`, ...), with the activity's unit for an activity row
	// (`activity.simple_tool_call`), so the preamble can name the card.
	Arm string
	// Text is the quoted card: a bubble's markdown verbatim, every other
	// card's row as multi-line protojson.
	Text string
}

// CardText answers the card ROW names on any of WS's feeds, as last
// published, quoted by THE ONE RULE: a bubble with text (bubbleMarkdown) by
// its markdown, and every other row by its own protojson, so a card kind
// added later is quotable with no rule of its own. A miss (false) is a feedid
// that does not decode, or names no row of a feed this resolver holds for WS.
func (r *resolver) CardText(ws ids.WorkspaceID, row *frontendv1.FeedId) (CardText, bool) {
	r.mu.Lock()
	defer r.mu.Unlock()
	ref, err := r.deps.Decode(row)
	if err != nil || ref.WS != ws {
		r.logger(ws).Debug("daemon.feed.card_text", "the asked row names no card of this workspace", dlog.Context{
			"row": row.GetValue(), "decode_error": errorText(err), "decoded_workspace": string(ref.WS),
		})
		return CardText{}, false
	}
	s, ok := r.workspaces[ws]
	if !ok {
		return CardText{}, false
	}
	f, ok := s.feeds[r.feedKey(ws, ref.Feed)]
	if !ok {
		return CardText{}, false
	}
	drawn, ok := f.rows[row.GetValue()]
	if !ok {
		return CardText{}, false
	}
	return cardTextOf(drawn), true
}

// cardTextOf is DRAWN quoted by the one rule (CardText).
func cardTextOf(drawn *frontendv1.FeedRow) CardText {
	arm := cardArm(drawn)
	if markdown, ok := bubbleMarkdown(drawn); ok {
		kind := CardResponse
		if drawn.GetUserPrompt() != nil || drawn.GetAgentPrompt() != nil {
			kind = CardPrompt
		}
		return CardText{Kind: kind, Arm: arm, Text: markdown}
	}
	text, err := protojson.MarshalOptions{Multiline: true}.Marshal(drawn)
	if err != nil {
		// A row the resolver itself built and published always marshals; one
		// that does not is a defect in the resolver.
		panic("feed: a published row does not marshal to JSON: " + err.Error())
	}
	return CardText{Kind: CardRow, Arm: arm, Text: string(text)}
}

// cardArm names DRAWN's FeedRow arm, with an activity's unit after a dot. A
// published row always has its arm set, and an activity its unit.
func cardArm(drawn *frontendv1.FeedRow) string {
	arm := string(protoarm.Must(drawn.ProtoReflect(), "row", "daemon.feed.card_text"))
	if activity := drawn.GetActivity(); activity != nil {
		arm += "." + string(protoarm.Must(activity.ProtoReflect(), "unit", "daemon.feed.card_text"))
	}
	return arm
}

// errorText is ERR's text, empty for none.
func errorText(err error) string {
	if err == nil {
		return ""
	}
	return err.Error()
}
