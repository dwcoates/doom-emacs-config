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
