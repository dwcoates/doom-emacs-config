package prompthandler

import (
	"context"
	"errors"
	"fmt"
	"strings"

	conversationv1 "agentrepl/proto/conversation/v1"

	"agent-repld/internal/ids"
	"agent-repld/internal/resolve/feed"
	"agent-repld/internal/sessioncommand"
	"agent-repld/internal/wsm"
)

// ErrCompactionKey is a compaction whose idempotency key could not be minted:
// nothing was submitted. A submission's own failure is returned as it is.
var ErrCompactionKey = errors.New("prompthandler: a compaction's idempotency key could not be minted")

// CompactionText is the vendor's /compact, followed by INSTRUCTIONS when they
// name any: the one spelling of a compaction the daemon submits.
func CompactionText(instructions string) string {
	compact := sessioncommand.Literal(conversationv1.SessionCommand_SESSION_COMMAND_COMPACT)
	if instructions = strings.TrimSpace(instructions); instructions != "" {
		return compact + " " + instructions
	}
	return compact
}

// SubmitCompaction is THE ONE WAY the daemon compacts a workspace's
// conversation on its own behalf: CompactionText(INSTRUCTIONS) submitted to
// WS through PROMPTS under a key MINT makes from KEYPREFIX, as ORIGIN,
// deferred behind any turn in flight. The prompt path recognizes it as the
// context cut it is, so it draws no prompt bubble and shows as any
// compaction does. A key that cannot be minted is ErrCompactionKey; nothing
// is submitted then.
func SubmitCompaction(ctx context.Context, prompts Submitter, ws ids.WorkspaceID, instructions string, origin conversationv1.PromptOrigin, mint func(prefix string) (string, error), keyPrefix string) (Outcome, error) {
	key, err := mint(keyPrefix)
	if err != nil {
		return Outcome{}, fmt.Errorf("%w: %w", ErrCompactionKey, err)
	}
	return prompts.Submit(ctx, ws, feed.SaidText(CompactionText(instructions)), key, origin, nil, wsm.DeliveryDeferred, nil)
}
