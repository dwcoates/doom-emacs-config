package main

import (
	"context"
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"
	shimv1 "agentrepl/proto/shim/v1"

	"connectrpc.com/connect"
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
	early := &conversationv1.AgentBash{Result: &conversationv1.AgentBash_Tail{
		Tail: &conversationv1.AgentBashTail{Text: "building...\n"},
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
	srv.publishBash("work-other", &conversationv1.AgentBash{Result: &conversationv1.AgentBash_Tail{
		Tail: &conversationv1.AgentBashTail{Text: "elsewhere\n"},
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
	late := &conversationv1.AgentBash{Result: &conversationv1.AgentBash_Tail{
		Tail: &conversationv1.AgentBashTail{Text: "later\n"},
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
	srv.publishBash("work-1", &conversationv1.AgentBash{Result: &conversationv1.AgentBash_Tail{
		Tail: &conversationv1.AgentBashTail{Text: "before the drop\n"},
	}})

	// Act
	srv.dropBashStreams()

	// Assert
	if _, _, backlog := srv.subscribeBash("work-1"); len(backlog) != 0 {
		t.Fatalf("the backlog after a drop = %v, want nothing replayed onto a redial", backlog)
	}
}

// TestPushedFrameRendersTheAgentFrameArm covers the ordinary push: an agent
// frame becomes the agent_frame arm of the history entry WatchAgent delivers.
func TestPushedFrameRendersTheAgentFrameArm(t *testing.T) {
	// Arrange.
	pushed := agentFrame{agent: MainAgentID, frame: &conversationv1.AgentFrame{
		AgentId: &conversationv1.AgentId{Value: MainAgentID},
	}}

	// Act.
	entry := pushed.entry()

	// Assert.
	if entry.GetAgentFrame() == nil {
		t.Fatalf("entry = %T, want the agent_frame arm", entry.GetEntry())
	}
}

// TestPushedUserPromptRendersTheUserPromptArm covers the arm a prompt's own
// content blocks reach the daemon on: a pushed prompt must NOT be wrapped as
// an agent frame, or the feed never draws a prompt row for it.
func TestPushedUserPromptRendersTheUserPromptArm(t *testing.T) {
	// Arrange.
	pushed := agentFrame{agent: MainAgentID, prompt: &conversationv1.AgentPrompt{
		Id: &conversationv1.TurnId{Value: "turn-1"},
	}}

	// Act.
	entry := pushed.entry()

	// Assert.
	if entry.GetUserPrompt().GetId().GetValue() != "turn-1" {
		t.Fatalf("entry = %T, want the user_prompt arm carrying turn-1", entry.GetEntry())
	}
}

// A RUN THAT ENDED STAYS ENDED, and the real shim says so to every watch
// opened afterwards -- it answers WatchBash out of a store holding every row
// the run ever wrote. A fake that forgot the ending on a redial would report a
// finished command as one still going, and no daemon test could see the
// difference.

// endedRun publishes a run's terminal frame for a work handle.
func endedRun(srv *server, work string) {
	srv.publishBash(work, &conversationv1.AgentBash{Result: &conversationv1.AgentBash_Success{
		Success: &conversationv1.AgentBashSuccess{
			Command: &conversationv1.AgentBashCommand{Line: "make"},
		},
	}})
}

func TestAWatchOpenedAfterARedialIsStillHandedTheRunsEnding(t *testing.T) {
	// Arrange: a run that ended, and a redial that severed its stream.
	srv := newServer(NewRecorder(), Profile{}, nil)
	endedRun(srv, "work-1")
	srv.dropBashStreams()

	// Act
	_, _, backlog := srv.subscribeBash("work-1")

	// Assert
	if len(backlog) != 1 || backlog[0].GetSuccess() == nil {
		t.Fatalf("backlog = %v, want the run's remembered ending", backlog)
	}
}

func TestARunsEndingIsNotHandedTwiceToAWatchTheLogAlreadyAnswers(t *testing.T) {
	// Arrange: no redial, so the log itself still carries the terminal.
	srv := newServer(NewRecorder(), Profile{}, nil)
	endedRun(srv, "work-1")

	// Act
	_, _, backlog := srv.subscribeBash("work-1")

	// Assert
	if len(backlog) != 1 {
		t.Fatalf("backlog = %d frames, want the terminal exactly once", len(backlog))
	}
}

func TestAWatchOnARunThatIsStillGoingIsHandedNoEnding(t *testing.T) {
	// Arrange: output, and no terminal. Inventing one would settle a run that
	// has not finished.
	srv := newServer(NewRecorder(), Profile{}, nil)
	srv.publishBash("work-1", &conversationv1.AgentBash{Result: &conversationv1.AgentBash_Tail{
		Tail: &conversationv1.AgentBashTail{Text: "building...\n"},
	}})
	srv.dropBashStreams()

	// Act
	_, _, backlog := srv.subscribeBash("work-1")

	// Assert
	if len(backlog) != 0 {
		t.Fatalf("backlog = %v, want nothing for a run that has not ended", backlog)
	}
}

// TestHibernateFailureProfileRefusesEveryCall covers the profile's standing
// transport failure: it is in force from the fake's birth and does not run
// out, which is what a test scripting a drain sweep's repeated attempts needs.
func TestHibernateFailureProfileRefusesEveryCall(t *testing.T) {
	// Arrange
	srv := newServer(NewRecorder(), Profile{HibernateFailure: "transport blew up"}, nil)
	req := connect.NewRequest(&shimv1.HibernateRequest{})

	// Act
	_, first := srv.Hibernate(context.Background(), req)
	_, second := srv.Hibernate(context.Background(), req)

	// Assert
	if first == nil || second == nil {
		t.Fatalf("Hibernate errors = (%v, %v), want the profile's failure on every call", first, second)
	}
}

// TestHibernateTurnInFlightProfileAnswersTheTypedRefusal covers the other
// profile arm: the shim's own turn_in_flight refusal rather than a transport
// failure.
func TestHibernateTurnInFlightProfileAnswersTheTypedRefusal(t *testing.T) {
	// Arrange
	srv := newServer(NewRecorder(), Profile{HibernateTurnInFlight: true}, nil)

	// Act
	resp, err := srv.Hibernate(context.Background(), connect.NewRequest(&shimv1.HibernateRequest{}))

	// Assert
	if err != nil {
		t.Fatalf("Hibernate = error %v, want the typed refusal", err)
	}
	if resp.Msg.GetError().GetTurnInFlight() == nil {
		t.Fatalf("Hibernate = %v, want a turn_in_flight refusal", resp.Msg)
	}
}

// TestAScriptedHibernateAnswerWinsOverTheProfile covers the precedence: a
// scripted answer is the narrower instruction and is taken first.
func TestAScriptedHibernateAnswerWinsOverTheProfile(t *testing.T) {
	// Arrange
	srv := newServer(NewRecorder(), Profile{HibernateTurnInFlight: true}, nil)
	srv.queueAnswer(RPCHibernate, &shimv1.HibernateResponse{
		Result: &shimv1.HibernateResponse_Success{Success: &shimv1.HibernateSuccess{}},
	}, "")

	// Act
	resp, err := srv.Hibernate(context.Background(), connect.NewRequest(&shimv1.HibernateRequest{}))

	// Assert
	if err != nil || resp.Msg.GetSuccess() == nil {
		t.Fatalf("Hibernate = (%v, %v), want the scripted success", resp.Msg, err)
	}
}

// TestTheReannouncementStatesTheTurnInFlightNow covers the re-announcement's
// turn_in_flight: it is the shim's LIVE answer, as the real shim's
// reannounceStart gives it, never the value StartSession answered with.
func TestTheReannouncementStatesTheTurnInFlightNow(t *testing.T) {
	turn := &conversationv1.TurnId{Value: "turn-1"}
	tests := []struct {
		name string
		// act drives the fake after its session has started.
		act  func(srv *server)
		want string
	}{
		{
			name: "a started turn is in flight",
			act:  func(srv *server) { srv.openTurn(turn) },
			want: "turn-1",
		},
		{
			name: "the main agent's success ends it",
			act: func(srv *server) {
				srv.openTurn(turn)
				srv.settleTurn(MainAgentID, &conversationv1.AgentFrame{Result: &conversationv1.AgentFrame_Success{Success: &conversationv1.AgentSuccess{}}})
			},
			want: "",
		},
		{
			name: "the main agent's failure ends it",
			act: func(srv *server) {
				srv.openTurn(turn)
				srv.settleTurn(MainAgentID, &conversationv1.AgentFrame{Result: &conversationv1.AgentFrame_Failure{Failure: &conversationv1.AgentFailure{}}})
			},
			want: "",
		},
		{
			name: "a subagent's terminal leaves it standing",
			act: func(srv *server) {
				srv.openTurn(turn)
				srv.settleTurn("sub-1", &conversationv1.AgentFrame{Result: &conversationv1.AgentFrame_Success{Success: &conversationv1.AgentSuccess{}}})
			},
			want: "turn-1",
		},
		{
			name: "a main-agent frame that is no terminal leaves it standing",
			act: func(srv *server) {
				srv.openTurn(turn)
				srv.settleTurn(MainAgentID, &conversationv1.AgentFrame{Result: &conversationv1.AgentFrame_Update{Update: &conversationv1.AgentUpdate{}}})
			},
			want: "turn-1",
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			srv := newServer(NewRecorder(), Profile{}, nil)
			srv.noteVendorSession(&shimv1.StartSessionResponse{Result: &shimv1.StartSessionResponse_Success{Success: &shimv1.StartSessionSuccess{
				Session: &conversationv1.SessionStarted{VendorSessionId: "vendor-1"},
			}}})

			// Act
			tt.act(srv)

			// Assert
			got := srv.startedSession()
			if got.GetVendorSessionId() != "vendor-1" {
				t.Fatalf("re-announced vendor session = %q, want the started vendor-1", got.GetVendorSessionId())
			}
			if got.GetTurnInFlight().GetValue() != tt.want {
				t.Fatalf("re-announced turn_in_flight = %q, want %q", got.GetTurnInFlight().GetValue(), tt.want)
			}
		})
	}
}

// TestTheReannouncementLeavesTheAnsweredSessionUntouched covers the clone: the
// re-announcement's live turn is stated on a copy, so the SessionStarted
// StartSession answered with is never rewritten under a reader of it.
func TestTheReannouncementLeavesTheAnsweredSessionUntouched(t *testing.T) {
	// Arrange
	srv := newServer(NewRecorder(), Profile{}, nil)
	answered := &conversationv1.SessionStarted{VendorSessionId: "vendor-1"}
	srv.noteVendorSession(&shimv1.StartSessionResponse{Result: &shimv1.StartSessionResponse_Success{Success: &shimv1.StartSessionSuccess{Session: answered}}})
	srv.openTurn(&conversationv1.TurnId{Value: "turn-1"})

	// Act
	srv.startedSession()

	// Assert
	if answered.GetTurnInFlight() != nil {
		t.Fatalf("the answered SessionStarted's turn_in_flight = %v, want it untouched", answered.GetTurnInFlight())
	}
}

// TestTheReannouncementIsNothingBeforeASessionStarted covers the one state
// with nothing to re-state.
func TestTheReannouncementIsNothingBeforeASessionStarted(t *testing.T) {
	// Arrange
	srv := newServer(NewRecorder(), Profile{}, nil)
	srv.openTurn(&conversationv1.TurnId{Value: "turn-1"})

	// Act
	got := srv.startedSession()

	// Assert
	if got != nil {
		t.Fatalf("re-announcement before StartSession = %v, want none", got)
	}
}

// TestAKilledTurnIsNoLongerReannouncedInFlight covers the kill's own terminal:
// the fake ends a killed turn on the stream, so no later watch re-announces it
// as still running.
func TestAKilledTurnIsNoLongerReannouncedInFlight(t *testing.T) {
	// Arrange
	srv := newServer(NewRecorder(), Profile{}, nil)
	srv.noteVendorSession(&shimv1.StartSessionResponse{Result: &shimv1.StartSessionResponse_Success{Success: &shimv1.StartSessionSuccess{
		Session: &conversationv1.SessionStarted{VendorSessionId: "vendor-1"},
	}}})
	srv.openTurn(&conversationv1.TurnId{Value: "turn-1"})

	// Act
	if _, err := srv.KillTurn(context.Background(), connect.NewRequest(&shimv1.KillTurnRequest{})); err != nil {
		t.Fatalf("KillTurn = %v, want the kill accepted", err)
	}

	// Assert
	if got := srv.startedSession().GetTurnInFlight(); got != nil {
		t.Fatalf("re-announced turn_in_flight after the kill = %v, want none", got)
	}
}

// TestTheReannouncementStatesTheLiveWorkSetLast covers set_live_work: once a
// membership is stated, every later re-announcement carries it, as the real
// shim's reannounceStart recomputes what is live now.
func TestTheReannouncementStatesTheLiveWorkSetLast(t *testing.T) {
	// Arrange
	srv := newServer(NewRecorder(), Profile{}, nil)
	shell := &conversationv1.AgentDetachedWork{Work: &conversationv1.DetachedWorkId{Value: "w-1"}}
	srv.noteVendorSession(&shimv1.StartSessionResponse{Result: &shimv1.StartSessionResponse_Success{Success: &shimv1.StartSessionSuccess{
		Session: &conversationv1.SessionStarted{VendorSessionId: "vendor-1", LiveWork: []*conversationv1.AgentDetachedWork{shell}},
	}}})

	// Act
	srv.setLiveWork(nil)

	// Assert
	if got := srv.startedSession().GetLiveWork(); len(got) != 0 {
		t.Fatalf("re-announced live_work = %v, want the empty membership last stated", got)
	}
}

// TestTheReannouncementStatesTheAnsweredLiveWorkUntilOneIsSet covers the
// default: with no membership stated, the re-announcement carries what
// StartSession answered with.
func TestTheReannouncementStatesTheAnsweredLiveWorkUntilOneIsSet(t *testing.T) {
	// Arrange
	srv := newServer(NewRecorder(), Profile{}, nil)
	shell := &conversationv1.AgentDetachedWork{Work: &conversationv1.DetachedWorkId{Value: "w-1"}}

	// Act
	srv.noteVendorSession(&shimv1.StartSessionResponse{Result: &shimv1.StartSessionResponse_Success{Success: &shimv1.StartSessionSuccess{
		Session: &conversationv1.SessionStarted{VendorSessionId: "vendor-1", LiveWork: []*conversationv1.AgentDetachedWork{shell}},
	}}})

	// Assert
	if got := srv.startedSession().GetLiveWork(); len(got) != 1 || got[0].GetWork().GetValue() != "w-1" {
		t.Fatalf("re-announced live_work = %v, want the answered w-1", got)
	}
}

// TestASilencedBashIsSilencedForItsWorkAlone covers silence_bash's keying: one
// handle's WatchBash goes unanswered, and every other handle's is served.
func TestASilencedBashIsSilencedForItsWorkAlone(t *testing.T) {
	// Arrange
	srv := newServer(NewRecorder(), Profile{}, nil)

	// Act
	srv.silenceBash("w-1")

	// Assert
	if !srv.bashSilenced("w-1") || srv.bashSilenced("w-2") {
		t.Fatalf("silenced(w-1) = %v, silenced(w-2) = %v, want true and false", srv.bashSilenced("w-1"), srv.bashSilenced("w-2"))
	}
}

// TestASilencedReannouncementSilencesOneWatchAlone covers
// silence_reannouncement's one-shot: the next open is silent, the one after
// it re-announces again.
func TestASilencedReannouncementSilencesOneWatchAlone(t *testing.T) {
	// Arrange
	srv := newServer(NewRecorder(), Profile{}, nil)

	// Act
	srv.silenceReannouncement()
	first, second := srv.reannouncementSilenced(), srv.reannouncementSilenced()

	// Assert
	if !first || second {
		t.Fatalf("silenced = (%v, %v), want (true, false)", first, second)
	}
}
