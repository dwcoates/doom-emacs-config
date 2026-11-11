package briefprompt

import (
	"context"
	"errors"
	"os"
	"path/filepath"
	"strings"
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"

	"agent-repld/internal/feedid"
	"agent-repld/internal/ids"
	"agent-repld/internal/prompthandler"
	"agent-repld/internal/wsm"
)

type fakePrompts struct {
	err      error
	ws       ids.WorkspaceID
	text     string
	key      string
	origin   conversationv1.PromptOrigin
	delivery wsm.Delivery
}

func (p *fakePrompts) Submit(_ context.Context, ws ids.WorkspaceID, said *conversationv1.UserSaid, key string, origin conversationv1.PromptOrigin, _ *ids.WorkspaceID, delivery wsm.Delivery, _ *feedid.Ref) (prompthandler.Outcome, error) {
	if p.err != nil {
		return prompthandler.Outcome{}, p.err
	}
	p.ws, p.text, p.key, p.origin, p.delivery = ws, said.GetContent().GetBlocks()[0].GetText().GetText(), key, origin, delivery
	return prompthandler.Outcome{Turn: "turn-1"}, nil
}

func composer(briefErr, mintErr error) Composer {
	return Composer{
		Briefs: func(name string, values map[string]string) (string, error) {
			if briefErr != nil {
				return "", briefErr
			}
			return name + ":" + values["x"], nil
		},
		MintKey: func(prefix string) (string, error) {
			if mintErr != nil {
				return "", mintErr
			}
			return prefix + "-1", nil
		},
	}
}

const origin = conversationv1.PromptOrigin_PROMPT_ORIGIN_WORKSPACE_NOTICE

func TestSubmitSendsTheComposedBriefDeferred(t *testing.T) {
	// Arrange
	prompts := &fakePrompts{}

	// Act
	turn, err := composer(nil, nil).Submit(context.Background(), prompts, "ws-1", "a-brief", map[string]string{"x": "y"}, "a-notice", origin)

	// Assert
	if err != nil || turn != "turn-1" {
		t.Fatalf("Submit = %q, %v", turn, err)
	}
	if prompts.ws != "ws-1" || prompts.text != "a-brief:y" || prompts.key != "a-notice-1" || prompts.origin != origin || prompts.delivery != wsm.DeliveryDeferred {
		t.Fatalf("submitted %+v, want the brief deferred under its origin and key", prompts)
	}
}

func TestSubmitNamesTheStepThatFailed(t *testing.T) {
	cause := errors.New("cause")
	tests := []struct {
		name                string
		brief, mint, submit error
		want                Step
	}{
		{name: "the brief", brief: cause, want: StepCompose},
		{name: "the key", mint: cause, want: StepMint},
		{name: "the submission", submit: cause, want: StepSubmit},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			prompts := &fakePrompts{err: tt.submit}

			// Act
			_, err := composer(tt.brief, tt.mint).Submit(context.Background(), prompts, "ws-1", "b", nil, "k", origin)

			// Assert
			if StepOf(err) != tt.want || !errors.Is(err, cause) {
				t.Fatalf("Submit = %v at step %d, want step %d wrapping the cause", err, StepOf(err), tt.want)
			}
		})
	}
}

func TestStepOfAnErrorNoSubmissionAnsweredIsZero(t *testing.T) {
	// Act, Assert
	if StepOf(errors.New("other")) != 0 || StepOf(nil) != 0 {
		t.Fatal("StepOf answered a step for an error no submission made")
	}
}

// TestEveryNotifierSubmitsThroughTheComposer holds the notifiers to the one
// shape: each submits its brief prompts through Composer, and none calls the
// prompt handler itself, so a hand-rolled site fails here.
func TestEveryNotifierSubmitsThroughTheComposer(t *testing.T) {
	for _, file := range []string{
		"../mergenotice/mergenotice.go",
		"../workspacesubs/workspacesubs.go",
		"../workspacenotice/workspacenotice.go",
		"../prwatch/poller.go",
		"../mainsync/mainsync.go",
		"../gather/gather.go",
	} {
		t.Run(filepath.Base(file), func(t *testing.T) {
			// Arrange
			raw, err := os.ReadFile(file)
			if err != nil {
				t.Fatal(err)
			}
			source := string(raw)

			// Assert
			if !strings.Contains(source, "briefprompt.Composer{") {
				t.Errorf("%s does not submit through briefprompt.Composer", file)
			}
			if strings.Contains(source, "prompts.Submit(") || strings.Contains(source, ".MintKey(\"") {
				t.Errorf("%s composes or submits a brief prompt by hand", file)
			}
		})
	}
}
