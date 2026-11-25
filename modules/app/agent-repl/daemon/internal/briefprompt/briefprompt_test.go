package briefprompt

import (
	"context"
	"errors"
	"reflect"
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"

	"agent-repld/internal/briefprompt/briefprompttest"
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
	return composerMetered(briefErr, mintErr, &briefprompttest.Meter{})
}

// composerMetered is composer recording into METER.
func composerMetered(briefErr, mintErr error, meter Meter) Composer {
	return Composer{
		Meter: meter,
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

func TestSubmitAsSendsTheComposedBriefUnderItsDelivery(t *testing.T) {
	tests := []struct {
		name     string
		delivery wsm.Delivery
	}{
		{name: "ordinary", delivery: wsm.DeliveryOrdinary},
		{name: "deferred", delivery: wsm.DeliveryDeferred},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			prompts := &fakePrompts{}

			// Act
			turn, err := composer(nil, nil).SubmitAs(context.Background(), prompts, "ws-1", "a-brief", map[string]string{"x": "y"}, "a-notice", origin, tt.delivery)

			// Assert
			if err != nil || turn != "turn-1" || prompts.delivery != tt.delivery || prompts.text != "a-brief:y" {
				t.Fatalf("SubmitAs = %q, %v; submitted %+v", turn, err, prompts)
			}
		})
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

func TestASubmittedNoticeIsMeasuredByItsBriefAndSize(t *testing.T) {
	// Arrange
	meter := &briefprompttest.Meter{}

	// Act
	_, err := composerMetered(nil, nil, meter).Submit(context.Background(), &fakePrompts{}, "ws-1", "a-brief", map[string]string{"x": "y"}, "k", origin)

	// Assert: the brief is the notice's type, and the size is its text's.
	want := []briefprompttest.Observed{{Notice: "a-brief", Bytes: len("a-brief:y")}}
	if got := meter.ObservedNotices(); err != nil || !reflect.DeepEqual(got, want) {
		t.Fatalf("observed %+v (err %v), want %+v", got, err, want)
	}
}

func TestAFailedSubmissionIsNotMeasured(t *testing.T) {
	// Arrange
	meter := &briefprompttest.Meter{}

	// Act
	_, err := composerMetered(nil, nil, meter).Submit(context.Background(), &fakePrompts{err: errors.New("refused")}, "ws-1", "a-brief", nil, "k", origin)

	// Assert
	if err == nil || len(meter.ObservedNotices()) != 0 {
		t.Fatalf("err %v, observed %+v; want the failure and nothing measured", err, meter.ObservedNotices())
	}
}

func TestAComposerWithNoMeterFailsHard(t *testing.T) {
	// Arrange
	c := composerMetered(nil, nil, nil)
	defer func() {
		// Assert
		if recover() == nil {
			t.Fatal("a Composer with no Meter submitted; want a panic")
		}
	}()

	// Act
	_, _ = c.Submit(context.Background(), &fakePrompts{}, "ws-1", "b", nil, "k", origin)
}
