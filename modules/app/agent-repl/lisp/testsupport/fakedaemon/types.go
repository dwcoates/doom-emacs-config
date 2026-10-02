package main

import (
	v1 "agentrepl/proto/agentrepl/v1"
	"google.golang.org/protobuf/proto"
)

// unaryResponseTypes is the closed set of methods /_fake/script accepts, and
// the type each scripted body is validated against.  A method absent here is
// either a stream or not an rpc at all, and the control plane answers 400.
var unaryResponseTypes = map[string]func() proto.Message{
	"SubmitPrompt":             func() proto.Message { return &v1.SubmitPromptResponse{} },
	"RequestCommandSupport":    func() proto.Message { return &v1.RequestCommandSupportResponse{} },
	"OpenFeed":                 func() proto.Message { return &v1.OpenFeedResponse{} },
	"GetFeedPage":              func() proto.Message { return &v1.GetFeedPageResponse{} },
	"Interrupt":                func() proto.Message { return &v1.InterruptResponse{} },
	"AnswerPermission":         func() proto.Message { return &v1.AnswerPermissionResponse{} },
	"AnswerQuestion":           func() proto.Message { return &v1.AnswerQuestionResponse{} },
	"AnswerColdGate":           func() proto.Message { return &v1.AnswerColdGateResponse{} },
	"CreateWorkspace":          func() proto.Message { return &v1.CreateWorkspaceResponse{} },
	"OpenWorkspace":            func() proto.Message { return &v1.OpenWorkspaceResponse{} },
	"CloseWorkspace":           func() proto.Message { return &v1.CloseWorkspaceResponse{} },
	"KillWorkspace":            func() proto.Message { return &v1.KillWorkspaceResponse{} },
	"NukeWorkspace":            func() proto.Message { return &v1.NukeWorkspaceResponse{} },
	"ForgetWorkspace":          func() proto.Message { return &v1.ForgetWorkspaceResponse{} },
	"MergeWorkspace":           func() proto.Message { return &v1.MergeWorkspaceResponse{} },
	"RestartWorkspace":         func() proto.Message { return &v1.RestartWorkspaceResponse{} },
	"SetWorkspacePriority":     func() proto.Message { return &v1.SetWorkspacePriorityResponse{} },
	"FoldRepository":           func() proto.Message { return &v1.FoldRepositoryResponse{} },
	"CreateTask":               func() proto.Message { return &v1.CreateTaskResponse{} },
	"UpdateTask":               func() proto.Message { return &v1.UpdateTaskResponse{} },
	"AssignWorkspaceTask":      func() proto.Message { return &v1.AssignWorkspaceTaskResponse{} },
	"SetModel":                 func() proto.Message { return &v1.SetModelResponse{} },
	"SetPermissionMode":        func() proto.Message { return &v1.SetPermissionModeResponse{} },
	"SetEffort":                func() proto.Message { return &v1.SetEffortResponse{} },
	"SelectAccount":            func() proto.Message { return &v1.SelectAccountResponse{} },
	"UpdateHeldPrompt":         func() proto.Message { return &v1.UpdateHeldPromptResponse{} },
	"EditHeldPrompt":           func() proto.Message { return &v1.EditHeldPromptResponse{} },
	"FoldHeldPrompt":           func() proto.Message { return &v1.FoldHeldPromptResponse{} },
	"AnswerHeldOffer":          func() proto.Message { return &v1.AnswerHeldOfferResponse{} },
	"UpdateShutdownSchedule":   func() proto.Message { return &v1.UpdateShutdownScheduleResponse{} },
	"BindWorkspaceSession":     func() proto.Message { return &v1.BindWorkspaceSessionResponse{} },
	"ListWorkspaceTranscripts": func() proto.Message { return &v1.ListWorkspaceTranscriptsResponse{} },
	"Deploy":                   func() proto.Message { return &v1.DeployResponse{} },
	"UpdateMergeQueue":         func() proto.Message { return &v1.UpdateMergeQueueResponse{} },
	"DaemonHealth":             func() proto.Message { return &v1.DaemonHealthResponse{} },
	"SessionHealth":            func() proto.Message { return &v1.SessionHealthResponse{} },
	"ClientLog":                func() proto.Message { return &v1.ClientLogResponse{} },
	"UpdatePersistentWifiMode": func() proto.Message { return &v1.UpdatePersistentWifiModeResponse{} },
	"RegisterWorkspace":        func() proto.Message { return &v1.RegisterWorkspaceResponse{} },
	"RegisterRepository":       func() proto.Message { return &v1.RegisterRepositoryResponse{} },
	"SelectWorkspace":          func() proto.Message { return &v1.SelectWorkspaceResponse{} },
	"AdoptHostWorkspace":       func() proto.Message { return &v1.AdoptHostWorkspaceResponse{} },
	"OpenLogin":                func() proto.Message { return &v1.OpenLoginResponse{} },
	"SendLoginInput":           func() proto.Message { return &v1.SendLoginInputResponse{} },
	"CloseLogin":               func() proto.Message { return &v1.CloseLoginResponse{} },
	"OpenExternal":             func() proto.Message { return &v1.OpenExternalResponse{} },
	"OpenInEditor":             func() proto.Message { return &v1.OpenInEditorResponse{} },
	"AdoptWebWorkspace":        func() proto.Message { return &v1.AdoptWebWorkspaceResponse{} },
	"DismissNewsDigest":        func() proto.Message { return &v1.DismissNewsDigestResponse{} },
	"RefreshNewsDigest":        func() proto.Message { return &v1.RefreshNewsDigestResponse{} },
}

// streamResponseTypes is the closed set of stream names the control plane
// accepts, and the response message each push/snapshot is validated against.
var streamResponseTypes = map[string]func() proto.Message{
	streamHost:   func() proto.Message { return &v1.WatchHostWorkspaceResponse{} },
	streamDaemon: func() proto.Message { return &v1.WatchDaemonResponse{} },
	streamRoster: func() proto.Message { return &v1.WatchWorkspaceRosterResponse{} },
}
