package server

import (
	"context"
	"fmt"

	conversationv1 "agentrepl/proto/conversation/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/lifecycleconfig"
)

// THE SLACK FORWARD (agentrepl.v1.SubmitPromptRequest.slack_forward): the
// emoji-ack intake hands a reacted Slack message to an open workspace on the
// same task instead of creating one. The prompt carries the daemon's
// directive to tell the thread which workspace took it, and once the prompt
// is accepted the thread is stored among the recipient's links as a
// reference. The link IS the record of the forward: nothing else reads it.

// ForwardedSlackThreadLabel is the label a forwarded thread is stored under
// among the recipient's links.
const ForwardedSlackThreadLabel = "Forwarded Slack thread"

// opSlackForward names the forward's records.
const opSlackForward = "daemon.server.slack_forward"

// withSlackForward answers SAID with the forward directive for THREAD, to
// workspace NAME, appended as its own last text block, so the sender's words
// stay as they were sent.
func withSlackForward(said *conversationv1.UserSaid, thread, name string) *conversationv1.UserSaid {
	out := &conversationv1.UserSaid{Content: &conversationv1.UserContent{}}
	if said.GetContent() != nil {
		out.Content.Blocks = append(out.Content.Blocks, said.GetContent().GetBlocks()...)
	}
	out.Content.Blocks = append(out.Content.Blocks, &conversationv1.UserContentBlock{
		Block: &conversationv1.UserContentBlock_Text{Text: &conversationv1.TextBlock{Text: lifecycleconfig.SlackForwardPrompt(thread, name)}},
	})
	return out
}

// storeForwardedThread stores THREAD among WS's links as a reference labelled
// ForwardedSlackThreadLabel. A thread WS already holds keeps its own label
// (its originating thread, say), so a forward never relabels it.
func (s *server) storeForwardedThread(ctx context.Context, log dlog.Logger, ws ids.WorkspaceID, thread string) error {
	fields := dlog.Context{"thread_url": thread}
	links, err := s.deps.DB.WorkspaceLinks(ctx, ws)
	if err != nil {
		log.Error(opSlackForward, "could not read the recipient's links to store the forwarded Slack thread", dlog.WithCause(fields, err))
		return fmt.Errorf("store the forwarded Slack thread %q: read the links: %w", thread, err)
	}
	for _, link := range links {
		if link.URL == thread {
			log.Info(opSlackForward, "the recipient already holds the forwarded Slack thread; its link keeps its label", dlog.Merge(fields, dlog.Context{"label": link.Label}))
			return nil
		}
	}
	if _, err := s.deps.Verbs.SetLink(ctx, ws, thread, ForwardedSlackThreadLabel, false, ""); err != nil {
		log.Error(opSlackForward, "could not store the forwarded Slack thread among the recipient's links", dlog.WithCause(fields, err))
		return fmt.Errorf("store the forwarded Slack thread %q: %w", thread, err)
	}
	log.Info(opSlackForward, "stored the forwarded Slack thread among the recipient's links", fields)
	return nil
}
