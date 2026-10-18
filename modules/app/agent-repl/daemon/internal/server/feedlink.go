package server

import (
	"context"
	"fmt"

	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/prompthandler"
	"agent-repld/internal/replyquote"
	"agent-repld/internal/workspace"
	"agent-repld/internal/wsm"
)

// askAboutUnresolvedLink submits an unresolved feed link's question to the
// workspace, quoting the bubble the link was clicked in exactly as a reply to
// a selected bubble quotes it, and WITHOUT INTERRUPTING: it is delivered as a
// deferred prompt, after whatever turn is running.
//
// THE SOURCE IS QUOTED WHETHER OR NOT IT CAN BE SELECTED: a thinking bubble
// is never selected, but a link clicked inside it still quotes its text.
//
// A SOURCE ROW THAT IS NOT A LANDED ROOT BUBBLE is asked about unquoted:
// the question names the link itself, so it still stands on its own, and the
// record says why no quote was carried.
func (s *server) askAboutUnresolvedLink(ctx context.Context, log dlog.Logger, ws ids.WorkspaceID, source *frontendv1.FeedId, link *workspace.UnresolvedLink) error {
	said := &conversationv1.UserSaid{Content: &conversationv1.UserContent{Blocks: []*conversationv1.UserContentBlock{{
		Block: &conversationv1.UserContentBlock_Text{Text: &conversationv1.TextBlock{Text: link.Question}},
	}}}}
	if quoted, ok := s.deps.Feed.BubbleText(ws, source); ok {
		said = replyquote.Quote(said, quoted.Markdown, quoted.Prompt)
	} else {
		log.Info("daemon.server.open_in_editor", "the unresolved link's source row is not a landed root bubble; its question is sent unquoted", dlog.Context{
			"href": link.Href, "source_row": source.GetValue(),
		})
	}
	key, err := linkQuestionKey()
	if err != nil {
		return fmt.Errorf("ask about unresolved link %q: %w", link.Href, err)
	}
	outcome, err := s.deps.Prompts.Submit(ctx, ws, said, key,
		conversationv1.PromptOrigin_PROMPT_ORIGIN_LINK_UNRESOLVED, nil, wsm.DeliveryDeferred, nil)
	if err != nil {
		return fmt.Errorf("ask about unresolved link %q: %w", link.Href, err)
	}
	log.Info("daemon.server.open_in_editor", "asked the workspace which file an unresolved link meant", dlog.Context{
		"href": link.Href, "turn": string(outcome.Turn),
	})
	return nil
}

// linkQuestionKey mints the idempotency key an unresolved link's question is
// submitted under: every click is its own question.
func linkQuestionKey() (string, error) {
	return prompthandler.MintKey("link-unresolved")
}
