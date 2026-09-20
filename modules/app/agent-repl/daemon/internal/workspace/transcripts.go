package workspace

import (
	"context"
	"fmt"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	shimv1 "agentrepl/proto/shim/v1"
	workspacev1 "agentrepl/proto/workspace/v1"

	"claude-repld/internal/dlog"
	"claude-repld/internal/ids"
	"claude-repld/internal/wsm"
)

// The operations this file's records carry.
const (
	opListTranscripts = "daemon.workspace.list_transcripts"
	opBindSession     = "daemon.workspace.bind_session"
)

// ListTranscripts answers every vendor conversation filed under a workspace's
// own directory, so a person can choose which one the workspace runs.
//
// THE SHIM READS; THE DAEMON ADDS ONE FACT. Where the vendor files a
// conversation and what its lines mean is the adapter's knowledge, so every
// figure is relayed from the shim's own `Transcript` unchanged — the two
// surfaces cannot drift into disagreeing about one conversation. What the shim
// cannot know is which OTHER workspace already holds a conversation, and that
// is the one thing added here.
//
// A DIRECTORY THAT WAS NEVER CREATED IS AN EMPTY LIST, not a refusal. The
// shim distinguishes "nothing has ever run here" from "the directory exists
// and holds nothing"; to a person choosing a conversation the two are the same
// answer, and ListWorkspaceTranscriptsError carries no arm for the first.
func (v *verbs) ListTranscripts(ctx context.Context, ws ids.WorkspaceID) ([]*agentreplv1.WorkspaceTranscript, error) {
	_, log, err := v.owned(ctx, "ListWorkspaceTranscripts", ws)
	if err != nil {
		return nil, err
	}
	return v.listTranscripts(ctx, log, "ListWorkspaceTranscripts", ws)
}

// listTranscripts is the listing itself, shared by the list verb and the bind
// verb's validation. THE BIND VALIDATES AGAINST A FRESH LISTING rather than
// against whatever the client last saw: a transcript that went active, or that
// another workspace took, between the listing and the choice is exactly the
// case the refusals exist for.
func (v *verbs) listTranscripts(
	ctx context.Context,
	log dlog.Logger,
	rpc string,
	ws ids.WorkspaceID,
) ([]*agentreplv1.WorkspaceTranscript, error) {
	shim, live := v.deps.Shim(ws)
	if !live {
		return nil, refuse(log, rpc, ArmNoSession,
			fmt.Sprintf("workspace %q has no live session, and the shim is what reads the transcripts", ws), false)
	}
	response, err := shim.ReadTranscripts(ctx)
	if err != nil {
		log.Error(opListTranscripts, "the shim could not be asked for its conversations", dlog.Context{"cause": err.Error()})
		return nil, fmt.Errorf("list transcripts for %q: %w", ws, err)
	}
	if failure := response.GetFailure(); failure != nil {
		if unreadable := failure.GetUnreadable(); unreadable != nil {
			return nil, refuseWith(log, rpc, ArmUnreadable,
				fmt.Sprintf("the vendor project directory could not be read: %s", unreadable.GetDetail()), false,
				map[string]any{
					"searched_path": unreadable.GetSearchedPath(),
					"detail":        unreadable.GetDetail(),
				})
		}
		if noDir := failure.GetNoProjectDir(); noDir != nil {
			// NOTHING HAS EVER RUN HERE, which is an empty list and not a
			// refusal: the contract carries no arm for it, and a person
			// choosing a conversation reads the same answer either way.
			log.Info(opListTranscripts, "the workspace's directory holds no vendor project directory at all", dlog.Context{
				"searched_path": noDir.GetSearchedPath(),
			})
			return nil, nil
		}
		// A failure whose cause oneof is unset is illegal on the wire and is
		// surfaced rather than read as an empty list.
		log.Error(opListTranscripts, "the shim refused the transcript read with no cause stated", nil)
		return nil, fmt.Errorf("list transcripts for %q: the shim stated no cause", ws)
	}

	holders, err := v.transcriptHolders(ctx, ws)
	if err != nil {
		log.Error(opListTranscripts, "could not read which workspaces hold which conversations", dlog.Context{"cause": err.Error()})
		return nil, fmt.Errorf("list transcripts for %q: %w", ws, err)
	}
	current, err := v.boundConversation(ctx, ws)
	if err != nil {
		log.Error(opListTranscripts, "could not read the workspace's own binding", dlog.Context{"cause": err.Error()})
		return nil, fmt.Errorf("list transcripts for %q: %w", ws, err)
	}

	read := response.GetSuccess().GetTranscripts()
	out := make([]*agentreplv1.WorkspaceTranscript, 0, len(read))
	for _, transcript := range read {
		out = append(out, workspaceTranscript(transcript, current, holders))
	}
	log.Debug(opListTranscripts, "listed the conversations filed under the workspace's directory", dlog.Context{
		"transcripts": len(out),
	})
	return out, nil
}

// workspaceTranscript relays one shim transcript and lays the daemon's two
// facts over it: which conversation this workspace runs, and which OTHER
// workspace holds one.
func workspaceTranscript(
	transcript *shimv1.Transcript,
	current string,
	holders map[string]*workspacev1.WorkspaceRef,
) *agentreplv1.WorkspaceTranscript {
	id := transcript.GetVendorSessionId()
	out := &agentreplv1.WorkspaceTranscript{
		VendorSessionId: id,
		// EVERY OPTIONAL FIELD IS RELAYED AS THE SHIM SET IT, presence and
		// all: an absence re-spelled as a zero is how a chooser comes to rank
		// a conversation nobody could read as the smallest one.
		LastRequestAtMs: transcript.LastRequestAtMs,
		ContextTokens:   transcript.ContextTokens,
		LastModel:       transcript.GetLastModel(),
		Opening:         transcript.Opening,
		Prompts:         transcript.GetPrompts(),
	}
	if cleared := transcript.GetCleared(); cleared != nil {
		out.Cleared = &agentreplv1.WorkspaceTranscriptCleared{AtMs: cleared.GetAtMs()}
	}
	if active := transcript.GetActive(); active != nil {
		out.Active = &agentreplv1.WorkspaceTranscriptActive{AtMs: active.GetAtMs()}
	}
	// THE DAEMON'S RECORD DECIDES WHAT THE WORKSPACE RUNS, because the record
	// is the binding: it is what survives a restart and what the next resume
	// reads. The shim's own `bound` answers for a workspace whose record names
	// no conversation yet — an adopted transcript is exactly that case — so
	// neither a freshly adopted conversation nor a recorded one goes unmarked.
	if (current != "" && id == current) || (current == "" && transcript.GetBound() != nil) {
		out.Current = &agentreplv1.WorkspaceTranscriptCurrent{}
	} else if holder, held := holders[id]; held {
		out.Held = &agentreplv1.WorkspaceTranscriptHeld{Workspace: holder}
	}
	return out
}

// transcriptHolders maps every conversation ANOTHER workspace in this daemon's
// registry is bound to onto that workspace's ref. It is the one fact the
// daemon holds that a transcript cannot state, and a bind is refused on it:
// two workspaces driving one conversation is data loss.
func (v *verbs) transcriptHolders(ctx context.Context, ws ids.WorkspaceID) (map[string]*workspacev1.WorkspaceRef, error) {
	workspaces, err := v.deps.DB.ListWorkspaces(ctx)
	if err != nil {
		return nil, fmt.Errorf("list the workspaces: %w", err)
	}
	holders := make(map[string]*workspacev1.WorkspaceRef)
	for _, record := range workspaces {
		if record.ID == ws {
			continue
		}
		session, found, err := v.deps.DB.Session(ctx, record.ID)
		if err != nil {
			return nil, fmt.Errorf("read workspace %q's session record: %w", record.ID, err)
		}
		if !found || session.VendorSessionID == "" {
			continue
		}
		holders[session.VendorSessionID] = &workspacev1.WorkspaceRef{
			Id:  string(record.ID),
			Dir: record.Dir,
		}
	}
	return holders, nil
}

// boundConversation answers the conversation the workspace's own session
// record names, empty when it names none.
func (v *verbs) boundConversation(ctx context.Context, ws ids.WorkspaceID) (string, error) {
	session, found, err := v.deps.DB.Session(ctx, ws)
	if err != nil {
		return "", fmt.Errorf("read workspace %q's session record: %w", ws, err)
	}
	if !found {
		return "", nil
	}
	return session.VendorSessionID, nil
}

// BindSession points a workspace at a DIFFERENT vendor conversation in its own
// directory.
//
// IT IS A SESSION SWAP, AND IT IS SAID SO. The current session is ended and a
// new one is started through the ORDINARY resume path, so a cold conversation
// parks at its cold gate exactly as a revival does — binding never pays for a
// cold read silently.
//
// A FAILED START LEAVES THE NEW BINDING STANDING. The record names the
// conversation the user chose and the ordinary open path is what brings it up
// next; a bind that silently reverted would leave the user looking at a
// workspace they did not ask for with nothing saying why.
func (v *verbs) BindSession(ctx context.Context, ws ids.WorkspaceID, vendorSessionID string, progress BindProgress) error {
	const rpc = "BindWorkspaceSession"
	_, log, err := v.owned(ctx, rpc, ws)
	if err != nil {
		return err
	}
	log = log.With(dlog.Context{"vendor_session_id": vendorSessionID})

	// A BIND IS NOT AN INTERRUPT. It is refused while a turn is in flight, as
	// every other session-level verb is: ending the session under a running
	// turn would discard work the user is waiting on.
	if running, live := v.deps.Freeness(ws); live && running.Turn != nil {
		return refuse(log, rpc, ArmTurnInFlight,
			"a turn is in flight; end it or interrupt it first", false)
	}

	reportBindStage(progress, BindStageReadingTranscripts)
	transcripts, err := v.listTranscripts(ctx, log, rpc, ws)
	if err != nil {
		return err
	}
	if cerr := v.validateBind(log, rpc, transcripts, vendorSessionID); cerr != nil {
		return cerr
	}

	reportBindStage(progress, BindStageStoppingSession)
	if err := v.deps.Sessions.Stop(ctx, ws, false); err != nil {
		return refuseWith(log, rpc, ArmStopFailed,
			fmt.Sprintf("the current session would not end: %s", err.Error()), false,
			map[string]any{"detail": err.Error()})
	}

	reportBindStage(progress, BindStageRecordingBinding)
	if err := v.recordBinding(ctx, log, ws, vendorSessionID); err != nil {
		return err
	}

	// THE ORDINARY RESUME PATH, and nothing else. The source classifier reads
	// the record this verb just wrote, so a cold conversation is refused with
	// its cost and answered at the cold gate — the same refusal a revival
	// meets — rather than paid for behind the user's back.
	reportBindStage(progress, BindStageStartingSession)
	if err := v.deps.Sessions.Start(ctx, ws); err != nil {
		return refuseWith(log, rpc, ArmStartFailed,
			fmt.Sprintf("the session on the bound conversation would not come up: %s", err.Error()), false,
			map[string]any{"detail": err.Error()})
	}

	log.Info(opBindSession, "bound the workspace to the chosen conversation", nil)
	v.republishRegistry(ctx, log, opBindSession)
	return nil
}

// validateBind refuses a choice the fresh listing does not support, by name.
// The order is the order a person would ask the questions in: is it there, is
// it already ours, is something writing to it, does somebody else hold it.
func (v *verbs) validateBind(
	log dlog.Logger,
	rpc string,
	transcripts []*agentreplv1.WorkspaceTranscript,
	vendorSessionID string,
) error {
	var chosen *agentreplv1.WorkspaceTranscript
	for _, transcript := range transcripts {
		if transcript.GetVendorSessionId() == vendorSessionID {
			chosen = transcript
			break
		}
	}
	if chosen == nil {
		return refuseWith(log, rpc, ArmUnknownTranscript,
			fmt.Sprintf("no transcript in the workspace's directory carries the id %q", vendorSessionID), true,
			map[string]any{"vendor_session_id": vendorSessionID})
	}
	if chosen.GetCurrent() != nil {
		return refuse(log, rpc, ArmAlreadyBound,
			"the workspace already runs that conversation; nothing was changed", false)
	}
	if active := chosen.GetActive(); active != nil {
		return refuseWith(log, rpc, ArmTranscriptActive,
			"something is writing to that transcript right now; two writers on one conversation is data loss", false,
			map[string]any{"at_ms": active.GetAtMs()})
	}
	if held := chosen.GetHeld(); held != nil {
		return refuseWith(log, rpc, ArmTranscriptHeld,
			fmt.Sprintf("workspace %q is bound to that conversation", held.GetWorkspace().GetId()), false,
			map[string]any{"workspace": held.GetWorkspace()})
	}
	return nil
}

// recordBinding writes the chosen conversation onto the workspace's session
// record, which is what makes the choice survive a restart, a hibernation and
// a daemon handover exactly as the rest of that record does.
//
// THE RECORD IS RE-READ AFTER THE STOP, never carried across it: the stop
// writes the session's own terminal, and writing back a copy taken before it
// would erase the stop's account of itself. A workspace that has never
// recorded a session gets a minted host identity, because a row with none is
// refused at the write and the binding would have nowhere to live.
func (v *verbs) recordBinding(ctx context.Context, log dlog.Logger, ws ids.WorkspaceID, vendorSessionID string) error {
	session, found, err := v.deps.DB.Session(ctx, ws)
	if err != nil {
		log.Error(opBindSession, "could not read the session record to bind onto", dlog.Context{"cause": err.Error()})
		return fmt.Errorf("bind %q: read the session record: %w", ws, err)
	}
	if !found {
		session = wsm.Session{Workspace: ws, StartedAt: v.now(), LastEngagementAt: v.now()}
	}
	if session.HostSessionID == "" {
		session.HostSessionID = wsm.NewHostSessionID()
		log.Debug(opBindSession, "minted a host session identity for a workspace that had recorded none", dlog.Context{
			"host_session_id": session.HostSessionID,
		})
	}
	session.Workspace = ws
	session.VendorSessionID = vendorSessionID
	if err := v.deps.DB.PutSession(ctx, session); err != nil {
		log.Error(opBindSession, "could not record the new binding", dlog.Context{"cause": err.Error()})
		return fmt.Errorf("bind %q: record the binding: %w", ws, err)
	}
	log.Info(opBindSession, "recorded the workspace's new conversation binding", nil)
	return nil
}

// reportBindStage relays one stage to a bind's progress reporter, if the
// caller set one. A bind with no op_id emits nothing.
func reportBindStage(progress BindProgress, stage BindStage) {
	if progress != nil {
		progress.Stage(stage)
	}
}
