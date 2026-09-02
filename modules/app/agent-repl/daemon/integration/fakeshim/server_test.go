package main

import (
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"
)

// TestRememberPushedBashKeysACreatedAnnouncementsStart covers the WatchBash
// opening frame's source: a `created`-origin announcement states the shell's
// original start, which is what the stream must open with.
func TestRememberPushedBashKeysACreatedAnnouncementsStart(t *testing.T) {
	// Arrange
	srv := newServer(NewRecorder(), Profile{}, nil)
	bash := &conversationv1.AgentBash{Result: &conversationv1.AgentBash_Start{Start: &conversationv1.AgentBashStart{
		Command: &conversationv1.AgentBashCommand{Line: "sleep 5"},
	}}}

	// Act
	srv.rememberPushedBash(&conversationv1.AgentFrame{
		Result: &conversationv1.AgentFrame_DetachedWork{DetachedWork: &conversationv1.AgentDetachedWork{
			Work: &conversationv1.DetachedWorkId{Value: "work-1"},
			Origin: &conversationv1.AgentDetachedWork_Created{Created: &conversationv1.DetachedWorkCreated{
				WorkCreated: &conversationv1.DetachableWork{Work: &conversationv1.DetachableWork_Bash{Bash: bash}},
			}},
		}},
	})

	// Assert
	if got := srv.bashStart("work-1").GetStart().GetCommand().GetLine(); got != "sleep 5" {
		t.Fatalf("WatchBash would open with command %q, want the announced sleep 5", got)
	}
}

// TestRememberPushedBashAliasesADetachedUnitsStart covers the other origin: a
// `detached` announcement names only the in-turn unit, whose own start frame
// carried the command.
func TestRememberPushedBashAliasesADetachedUnitsStart(t *testing.T) {
	// Arrange
	srv := newServer(NewRecorder(), Profile{}, nil)
	srv.rememberPushedBash(&conversationv1.AgentFrame{
		Result: &conversationv1.AgentFrame_Update{Update: &conversationv1.AgentUpdate{
			Update: &conversationv1.AgentUpdate_Activity{Activity: &conversationv1.AgentActivity{
				ActivityId: &conversationv1.AgentActivityId{Value: "unit-1"},
				Item: &conversationv1.AgentActivity_Bash{Bash: &conversationv1.AgentBash{
					Result: &conversationv1.AgentBash_Start{Start: &conversationv1.AgentBashStart{
						Command: &conversationv1.AgentBashCommand{Line: "make test"},
					}},
				}},
			}},
		}},
	})

	// Act
	srv.rememberPushedBash(&conversationv1.AgentFrame{
		Result: &conversationv1.AgentFrame_DetachedWork{DetachedWork: &conversationv1.AgentDetachedWork{
			Work: &conversationv1.DetachedWorkId{Value: "work-2"},
			Origin: &conversationv1.AgentDetachedWork_Detached{Detached: &conversationv1.DetachedWorkDetached{
				DetachedFromId: &conversationv1.AgentActivityId{Value: "unit-1"},
			}},
		}},
	})

	// Assert
	if got := srv.bashStart("work-2").GetStart().GetCommand().GetLine(); got != "make test" {
		t.Fatalf("WatchBash would open with command %q, want the detached unit's make test", got)
	}
}

// TestBashStartFallsBackToABareStart covers a work handle the script never
// described: the stream still opens, because the daemon's client takes the
// first frame as the open's answer.
func TestBashStartFallsBackToABareStart(t *testing.T) {
	// Arrange
	srv := newServer(NewRecorder(), Profile{}, nil)

	// Act
	got := srv.bashStart("unknown")

	// Assert
	if got.GetStart() == nil {
		t.Fatalf("bashStart = %v, want a start arm", got)
	}
}

// openAsk is the pushed frame that opens one permission ask.
func openAsk(id, gated string) *conversationv1.AgentFrame {
	return &conversationv1.AgentFrame{
		AgentId: &conversationv1.AgentId{Value: MainAgentID},
		Result: &conversationv1.AgentFrame_Update{Update: &conversationv1.AgentUpdate{
			Update: &conversationv1.AgentUpdate_Permission{Permission: &conversationv1.AgentPermission{
				Id:        &conversationv1.AgentPermissionId{Value: id},
				GatedCall: &conversationv1.AgentActivityId{Value: gated},
				Result: &conversationv1.AgentPermission_Start{
					Start: &conversationv1.AgentPermissionStart{}},
			}},
		}},
	}
}

// TestSettlePermissionPublishesADenialsSettleFrame covers the decision the
// daemon draws the answered card from: the fake must put the ask's own settle
// frame on the agent's stream, as the real shim does.
func TestSettlePermissionPublishesADenialsSettleFrame(t *testing.T) {
	// Arrange
	srv := newServer(NewRecorder(), Profile{}, nil)
	srv.rememberPushedPermission(MainAgentID, openAsk("perm-1", "act-1"))
	_, stream := srv.agents.subscribe()

	// Act
	if !srv.settlePermission(&conversationv1.AgentPermissionDecision{
		Ask:      &conversationv1.AgentPermissionId{Value: "perm-1"},
		Decision: &conversationv1.AgentPermissionDecision_Denied{Denied: &conversationv1.AgentPermissionDeniedByUser{Message: "no"}},
	}) {
		t.Fatal("settlePermission = false, want the remembered ask settled")
	}

	// Assert
	frame := (<-stream).frame.GetUpdate().GetPermission()
	if frame.GetSuccess().GetDenied().GetUser().GetMessage() != "no" {
		t.Fatalf("settle frame = %v, want a user denial carrying the reason", frame)
	}
	if frame.GetGatedCall().GetValue() != "act-1" {
		t.Fatalf("settle frame gated_call = %v, want the ask's own", frame.GetGatedCall())
	}
}

// TestSettlePermissionRefusesAnAskItWasNeverToldAbout is the other edge: an
// unknown ask settles nothing rather than inventing a frame.
func TestSettlePermissionRefusesAnAskItWasNeverToldAbout(t *testing.T) {
	// Arrange
	srv := newServer(NewRecorder(), Profile{}, nil)

	// Act
	settled := srv.settlePermission(&conversationv1.AgentPermissionDecision{
		Ask:      &conversationv1.AgentPermissionId{Value: "perm-nope"},
		Decision: &conversationv1.AgentPermissionDecision_Denied{Denied: &conversationv1.AgentPermissionDeniedByUser{}},
	})

	// Assert
	if settled {
		t.Fatal("settlePermission = true for an ask the fake never opened, want false")
	}
}

// TestSubscribeBashReplaysAFramePushedBeforeTheStreamOpened covers the
// lost-frame race the backlog exists for: a test pushes as soon as the daemon
// has drawn the shell's head row, which is before the daemon's own WatchBash
// goroutine has subscribed.
func TestSubscribeBashReplaysAFramePushedBeforeTheStreamOpened(t *testing.T) {
	// Arrange
	srv := newServer(NewRecorder(), Profile{}, nil)
	early := &conversationv1.AgentBash{Result: &conversationv1.AgentBash_Update{
		Update: &conversationv1.AgentBashUpdate{NewOutput: "building...\n"},
	}}
	srv.publishBash("work-1", early)

	// Act
	_, _, backlog := srv.subscribeBash("work-1")

	// Assert
	if len(backlog) != 1 || backlog[0] != early {
		t.Fatalf("the backlog = %v, want the one frame pushed before the subscription", backlog)
	}
}

// TestSubscribeBashLeavesAnotherWorkHandlesFramesOutOfTheBacklog covers the
// keying: the hub fans every frame out to every stream and the stream filters
// by work handle, so the replay has to filter the same way.
func TestSubscribeBashLeavesAnotherWorkHandlesFramesOutOfTheBacklog(t *testing.T) {
	// Arrange
	srv := newServer(NewRecorder(), Profile{}, nil)
	srv.publishBash("work-other", &conversationv1.AgentBash{Result: &conversationv1.AgentBash_Update{
		Update: &conversationv1.AgentBashUpdate{NewOutput: "elsewhere\n"},
	}})

	// Act
	_, _, backlog := srv.subscribeBash("work-1")

	// Assert
	if len(backlog) != 0 {
		t.Fatalf("the backlog = %v, want nothing from another shell's stream", backlog)
	}
}

// TestPublishBashAfterASubscriptionStaysOffTheBacklog covers the
// exactly-once half: a frame delivered on the channel must not ALSO be
// replayed, or the daemon reads the same delta twice as a spool gap.
func TestPublishBashAfterASubscriptionStaysOffTheBacklog(t *testing.T) {
	// Arrange
	srv := newServer(NewRecorder(), Profile{}, nil)
	_, ch, backlog := srv.subscribeBash("work-1")
	if len(backlog) != 0 {
		t.Fatalf("the backlog at open = %v, want nothing published yet", backlog)
	}
	late := &conversationv1.AgentBash{Result: &conversationv1.AgentBash_Update{
		Update: &conversationv1.AgentBashUpdate{NewOutput: "later\n"},
	}}

	// Act
	srv.publishBash("work-1", late)

	// Assert
	got := <-ch
	if got.bash != late {
		t.Fatalf("the delivered frame = %v, want the one published after the subscription", got.bash)
	}
	if _, _, replay := srv.subscribeBash("work-1"); len(replay) != 1 {
		t.Fatalf("a LATER stream's backlog = %v, want the one frame the shell has produced", replay)
	}
}

// TestDropBashStreamsForgetsTheBacklog covers a severed family's redial: a
// fresh observer of a shell that kept running, not a replay of the frames the
// severed stream already carried.
func TestDropBashStreamsForgetsTheBacklog(t *testing.T) {
	// Arrange
	srv := newServer(NewRecorder(), Profile{}, nil)
	srv.publishBash("work-1", &conversationv1.AgentBash{Result: &conversationv1.AgentBash_Update{
		Update: &conversationv1.AgentBashUpdate{NewOutput: "before the drop\n"},
	}})

	// Act
	srv.dropBashStreams()

	// Assert
	if _, _, backlog := srv.subscribeBash("work-1"); len(backlog) != 0 {
		t.Fatalf("the backlog after a drop = %v, want nothing replayed onto a redial", backlog)
	}
}
