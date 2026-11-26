package prompthandler

import (
	"context"
	"errors"
	"path/filepath"
	"reflect"
	"strings"
	"testing"
	"time"

	conversationv1 "agentrepl/proto/conversation/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/prompts"
	"agent-repld/internal/wsm"
)

// pausedSince is when the harness's production checks were paused.
var pausedSince = time.Date(2026, 11, 25, 12, 0, 0, 0, time.UTC)

// asProductionMediator makes theWorkspace a production mediator whose
// repository's checks are PAUSED or running.
func asProductionMediator(h *harness, paused bool) {
	h.db.mediatorOf, h.db.mediatorRepo = theWorkspace, "/repos/one/.git"
	if paused {
		h.db.productionPause = wsm.ProductionPause{Paused: true, Since: pausedSince}
	}
}

func TestAPausedProductionMediatorsPromptOpensWithThePauseNote(t *testing.T) {
	// Arrange
	h := newHarness(t)
	asProductionMediator(h, true)

	// Act
	_, err := h.submit("what are you doing?")

	// Assert
	if err != nil {
		t.Fatalf("Submit: %v", err)
	}
	if got, want := forwardedTexts(t, h), []string{prompts.Wrap("paused since: 2026-11-25T12:00:00Z\n\n"), "what are you doing?"}; !reflect.DeepEqual(got, want) {
		t.Fatalf("forwarded blocks = %q, want %q", got, want)
	}
	if rec, ok := dlog.FindRecord(h.log.Records(), dlog.RecordQuery{Level: "info", Operation: opProductionPaused}); !ok || rec.Context["repo"] != "/repos/one/.git" {
		t.Fatalf("records = %+v, want the note at INFO naming the repository", h.log.Records())
	}
}

func TestAPromptCarriesNoPauseNoteUnlessItsMediatorsChecksArePaused(t *testing.T) {
	cases := []struct {
		name    string
		arrange func(h *harness)
		origin  conversationv1.PromptOrigin
	}{
		{name: "a mediator whose checks run", arrange: func(h *harness) { asProductionMediator(h, false) },
			origin: conversationv1.PromptOrigin_PROMPT_ORIGIN_USER_SENT},
		{name: "a workspace that is no production mediator", arrange: func(*harness) {},
			origin: conversationv1.PromptOrigin_PROMPT_ORIGIN_USER_SENT},
		{name: "a daemon-born brief to a paused mediator", arrange: func(h *harness) { asProductionMediator(h, true) },
			origin: conversationv1.PromptOrigin_PROMPT_ORIGIN_PRODUCTION_MEDIATION},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			tc.arrange(h)

			// Act
			_, err := h.h.Submit(context.Background(), theWorkspace, userSaid("hello"), "key-1", tc.origin, nil, wsm.DeliveryOrdinary, nil)

			// Assert
			if err != nil {
				t.Fatalf("Submit: %v", err)
			}
			if got := forwardedTexts(t, h); !reflect.DeepEqual(got, []string{"hello"}) {
				t.Fatalf("forwarded blocks = %q, want the words alone", got)
			}
		})
	}
}

func TestAPauseNoteThatCannotBeReadForwardsNothing(t *testing.T) {
	cause := errors.New("the state database is locked")
	cases := []struct {
		name    string
		arrange func(h *harness)
		message string
	}{
		{name: "an unreadable mediator standing", arrange: func(h *harness) { h.db.mediatorErr = cause },
			message: "could not read whether the workspace is a production mediator; the prompt was not forwarded"},
		{name: "an unreadable pause", arrange: func(h *harness) {
			asProductionMediator(h, true)
			h.db.pauseErr = cause
		}, message: "could not read whether the production mediator's checks are paused; the prompt was not forwarded"},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			tc.arrange(h)

			// Act
			_, err := h.submit("hello")

			// Assert
			if !errors.Is(err, cause) || len(h.queue.forwarded()) != 0 {
				t.Fatalf("Submit = %v, forwarded %d; want the cause and nothing forwarded", err, len(h.queue.forwarded()))
			}
			if _, ok := dlog.FindRecord(h.log.Records(), dlog.RecordQuery{Level: "error", Operation: opProductionPaused, Message: tc.message}); !ok {
				t.Fatalf("records = %+v, want %q at ERROR", h.log.Records(), tc.message)
			}
		})
	}
}

func TestTheProductionPausedBriefSplicesItsStart(t *testing.T) {
	// Arrange
	dir := filepath.Join("..", "..", "..", "prompts")

	// Act
	note, err := productionPausedNote(dir, pausedSince)

	// Assert
	if err != nil || !strings.HasPrefix(note, prompts.MetaOpen) || !strings.Contains(note, "since 2026-11-25T12:00:00Z") {
		t.Fatalf("productionPausedNote = (%q, %v), want the meta-wrapped brief naming when the pause began", note, err)
	}
}
