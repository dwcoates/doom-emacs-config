package main

import (
	shimv1 "agentrepl/proto/shim/v1"

	"google.golang.org/protobuf/proto"
)

// The shim.v1 verb names the control protocol addresses. Workflow verbs are
// deliberately absent: they answer the typed not-implemented failure and are
// never scripted.
const (
	RPCStartSession             = "StartSession"
	RPCWatchSession             = "WatchSession"
	RPCSetSessionModel          = "SetSessionModel"
	RPCSetSessionPermissionMode = "SetSessionPermissionMode"
	RPCSetSessionEffort         = "SetSessionEffort"
	RPCHibernate                = "Hibernate"
	RPCKillSession              = "KillSession"
	RPCStartTurn                = "StartTurn"
	RPCWatchAgent               = "WatchAgent"
	RPCUpdateAgent              = "UpdateAgent"
	RPCKillTurn                 = "KillTurn"
	RPCRollBackSession          = "RollBackSession"
	RPCWatchBash                = "WatchBash"
	RPCStopBash                 = "StopBash"
	RPCDetachForeground         = "DetachForeground"
	RPCReadHistory              = "ReadHistory"
	RPCGatherTitleDigest        = "GatherTitleDigest"
	RPCReadTranscripts          = "ReadTranscripts"
)

// newResponse mints the empty response message for one verb, so `answer`
// payloads decode into the right type.
func newResponse(rpc string) proto.Message {
	switch rpc {
	case RPCStartSession:
		return &shimv1.StartSessionResponse{}
	case RPCSetSessionModel:
		return &shimv1.SetSessionModelResponse{}
	case RPCSetSessionPermissionMode:
		return &shimv1.SetSessionPermissionModeResponse{}
	case RPCSetSessionEffort:
		return &shimv1.SetSessionEffortResponse{}
	case RPCHibernate:
		return &shimv1.HibernateResponse{}
	case RPCKillSession:
		return &shimv1.KillSessionResponse{}
	case RPCRollBackSession:
		return &shimv1.RollBackSessionResponse{}
	case RPCStartTurn:
		return &shimv1.StartTurnResponse{}
	case RPCUpdateAgent:
		return &shimv1.UpdateAgentResponse{}
	case RPCKillTurn:
		return &shimv1.KillTurnResponse{}
	case RPCStopBash:
		return &shimv1.StopBashResponse{}
	case RPCDetachForeground:
		return &shimv1.DetachForegroundResponse{}
	case RPCReadHistory:
		return &shimv1.ReadHistoryResponse{}
	case RPCGatherTitleDigest:
		return &shimv1.GatherTitleDigestResponse{}
	case RPCReadTranscripts:
		return &shimv1.ReadTranscriptsResponse{}
	default:
		return nil
	}
}

// knownRPC reports whether the verb can be recorded, expected and counted.
// The three stream verbs have no scriptable answer but are still recorded.
func knownRPC(rpc string) bool {
	switch rpc {
	case RPCWatchSession, RPCWatchAgent, RPCWatchBash:
		return true
	default:
		return newResponse(rpc) != nil
	}
}

// answerable reports whether `answer` may queue a response for the verb.
func answerable(rpc string) bool { return newResponse(rpc) != nil }
