package server

import (
	"context"
	"crypto/rand"
	"encoding/hex"
	"fmt"

	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"claude-repld/internal/dlog"
	"claude-repld/internal/ids"
	"claude-repld/internal/workspace"
	"claude-repld/internal/wsm"
)

// askAboutUnresolvedLink submits an unresolved feed link's question to the
// workspace, quoting the bubble the link was clicked in exactly as a reply to
// a selected bubble quotes it, and WITHOUT INTERRUPTING: it is delivered as a
// deferred prompt, after whatever turn is running.
//
// A SOURCE ROW THAT IS NOT A SELECTABLE ROOT BUBBLE is asked about unquoted:
// the question names the link itself, so it still stands on its own, and the
// record says why no quote was carried.
func (s *server) askAboutUnresolvedLink(ctx context.Context, log dlog.Logger, ws ids.WorkspaceID, source *frontendv1.FeedId, link *workspace.UnresolvedLink) error {
	said := &conversationv1.UserSaid{Content: &conversationv1.UserContent{Blocks: []*conversationv1.UserContentBlock{{
		Block: &conversationv1.UserContentBlock_Text{Text: &conversationv1.TextBlock{Text: link.Question}},
	}}}}
	if quoted, ok := s.deps.Feed.SelectableText(ws, source); ok {
		said = prependReferencedResponse(said, quoted)
	} else {
		log.Info("daemon.server.open_in_editor", "the unresolved link's source row is not a selectable root bubble; its question is sent unquoted", dlog.Context{
			"href": link.Href, "source_row": source.GetValue(),
		})
	}
	key, err := linkQuestionKey()
	if err != nil {
		return fmt.Errorf("ask about unresolved link %q: %w", link.Href, err)
	}
	outcome, err := s.deps.Prompts.Submit(ctx, ws, said, key,
		conversationv1.PromptOrigin_PROMPT_ORIGIN_LINK_UNRESOLVED, wsm.DeliveryDeferred, nil)
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
	var b [16]byte
	if _, err := rand.Read(b[:]); err != nil {
		return "", fmt.Errorf("mint the question's idempotency key: %w", err)
	}
	return "link-unresolved-" + hex.EncodeToString(b[:]), nil
}
