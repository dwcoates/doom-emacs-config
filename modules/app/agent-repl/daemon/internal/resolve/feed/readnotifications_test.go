package feed

import (
	"testing"

	"google.golang.org/protobuf/proto"

	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"
)

// readNotificationsActivity wraps one notifications-read frame as unit-1's
// activity, stamped by a producer bound by the stands-alone contract.
func readNotificationsActivity(read *conversationv1.AgentReadNotifications) *conversationv1.AgentActivity {
	return bound(&conversationv1.AgentActivity{
		ActivityId: &conversationv1.AgentActivityId{Value: "unit-1"},
		Item:       &conversationv1.AgentActivity_ReadNotifications{ReadNotifications: read},
	})
}

// readNotificationsStart is the read announcing itself at 1000.
func readNotificationsStart() *conversationv1.AgentActivity {
	return readNotificationsActivity(&conversationv1.AgentReadNotifications{
		Result: &conversationv1.AgentReadNotifications_Start{Start: &conversationv1.AgentReadNotificationsStart{
			StartedAt: &conversationv1.AgentActivityStartedAt{AtMs: 1_000},
		}},
	})
}

// readNotificationsSuccess is the read returning at 2500, restating its start.
func readNotificationsSuccess(returned *string) *conversationv1.AgentActivity {
	return readNotificationsActivity(&conversationv1.AgentReadNotifications{
		Result: &conversationv1.AgentReadNotifications_Success{Success: &conversationv1.AgentReadNotificationsSuccess{
			ReturnedText: returned,
			SettledAt: &conversationv1.AgentActivitySettledAt{
				AtMs: 2_500, StartedAt: &conversationv1.AgentActivityStartedAt{AtMs: 1_000},
			},
		}},
	})
}

func TestANotificationsReadDrawsTheOrdinaryToolCard(t *testing.T) {
	tests := []struct {
		name   string
		frames []*conversationv1.AgentActivity
		check  func(*testing.T, *frontendv1.FeedSimpleToolCall)
	}{
		{
			name:   "its start is a running card headed by the tool's name",
			frames: []*conversationv1.AgentActivity{readNotificationsStart()},
			check: func(t *testing.T, card *frontendv1.FeedSimpleToolCall) {
				if card.GetRunning() == nil || card.GetName().GetText() != "ReadNotifications" {
					t.Fatalf("card = %+v, want a running ReadNotifications card", card)
				}
			},
		},
		{
			name:   "its start draws an empty input line, because the call takes none",
			frames: []*conversationv1.AgentActivity{readNotificationsStart()},
			check: func(t *testing.T, card *frontendv1.FeedSimpleToolCall) {
				if card.GetInput().GetText() != "" {
					t.Fatalf("input = %+v, want an empty line", card.GetInput())
				}
			},
		},
		{
			name: "its success draws what the read returned as text",
			frames: []*conversationv1.AgentActivity{
				readNotificationsStart(), readNotificationsSuccess(proto.String("No queued notifications.")),
			},
			check: func(t *testing.T, card *frontendv1.FeedSimpleToolCall) {
				returned := card.GetReturned()
				if returned.GetSucceeded() == nil || returned.GetText().GetText() != "No queued notifications." {
					t.Fatalf("returned = %+v, want succeeded with the vendor's sentence", returned)
				}
			},
		},
		{
			name:   "a success that returned no text draws no output",
			frames: []*conversationv1.AgentActivity{readNotificationsSuccess(nil)},
			check: func(t *testing.T, card *frontendv1.FeedSimpleToolCall) {
				if card.GetReturned().GetNone() == nil {
					t.Fatalf("form = %T, want the none arm", card.GetReturned().GetForm())
				}
			},
		},
		{
			name:   "a replayed success alone draws the card with its runtime",
			frames: []*conversationv1.AgentActivity{readNotificationsSuccess(proto.String("No queued notifications."))},
			check: func(t *testing.T, card *frontendv1.FeedSimpleToolCall) {
				if card.GetName().GetText() != "ReadNotifications" || card.GetClock().GetStopped() == nil {
					t.Fatalf("card = %+v, want the named card with a stopped clock", card)
				}
			},
		},
		{
			name: "its failure draws the tool's account as failed",
			frames: []*conversationv1.AgentActivity{readNotificationsActivity(&conversationv1.AgentReadNotifications{
				Result: &conversationv1.AgentReadNotifications_Failure{Failure: &conversationv1.AgentReadNotificationsFailure{
					Error: &conversationv1.AgentToolFailure{
						Content: &conversationv1.ToolResultContent{Blocks: []*conversationv1.ToolResultContentBlock{
							textResultBlock("Error: the queue could not be read"),
						}},
						SettledAt: &conversationv1.AgentActivitySettledAt{
							AtMs: 2_000, StartedAt: &conversationv1.AgentActivityStartedAt{AtMs: 1_000},
						},
					},
				}},
			})},
			check: func(t *testing.T, card *frontendv1.FeedSimpleToolCall) {
				returned := card.GetReturned()
				if returned.GetFailed() == nil || returned.GetText().GetText() != "Error: the queue could not be read" {
					t.Fatalf("returned = %+v, want failed with the tool's account", returned)
				}
			},
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			h := newHarness(t)

			// Act.
			for _, frame := range tt.frames {
				h.send(frame)
			}

			// Assert.
			tt.check(t, h.card())
			if errs := h.anyErrors(); len(errs) != 0 {
				t.Fatalf("errors = %v, want none", errs)
			}
		})
	}
}

func TestANotificationsReadWithNoStateDrawsNothing(t *testing.T) {
	// Arrange.
	h := newHarness(t)

	// Act.
	h.send(readNotificationsActivity(&conversationv1.AgentReadNotifications{}))

	// Assert.
	if rows := h.rows(rootFeed()); len(rows) != 0 {
		t.Fatalf("rows = %d, want none", len(rows))
	}
}
