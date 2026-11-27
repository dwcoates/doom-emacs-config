package main

import (
	"fmt"
	"strings"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	conversationv1 "agentrepl/proto/conversation/v1"
	workspacev1 "agentrepl/proto/workspace/v1"

	"agent-repld/internal/effortlevel"

	"connectrpc.com/connect"
)

// The topbar's own controls each have an agent API setter that sends the
// control's rpc: `workspace set -permission-mode`, `-effort`, `-account` and
// `-default-model` for the per-workspace cells, and `runtime persistent-wifi`
// for the machine-wide chip. Each sends what the control sends and the daemon
// validates it as the control's rpc does, so a refusal here is the control's
// refusal by the same arm.

// effortWords are the levels `-effort` names, in the selector's order.
var effortWords = []string{"low", "medium", "high", "xhigh", "max"}

// permissionModeRequest reads a -permission-mode flag into the request that
// switches workspace REF's session to that mode. The mode is sent as typed:
// the daemon validates it against the modes the picker serves and, for an
// ungated mode, the consent recorded at the workspace's creation.
func permissionModeRequest(ref *workspacev1.WorkspaceRef, mode string) (*agentreplv1.SetPermissionModeRequest, error) {
	if strings.TrimSpace(mode) == "" {
		return nil, fmt.Errorf("-permission-mode names a mode the picker serves, not a blank")
	}
	return &agentreplv1.SetPermissionModeRequest{Workspace: ref, Mode: mode}, nil
}

// effortRequest reads an -effort flag into the request that sets workspace
// REF's reasoning effort. The level is one of effortWords: the selector
// serves no other.
func effortRequest(ref *workspacev1.WorkspaceRef, word string) (*agentreplv1.SetEffortRequest, error) {
	level, err := effortlevel.Parse(word)
	if err != nil || level == conversationv1.AgentEffortLevel_AGENT_EFFORT_LEVEL_UNSPECIFIED {
		return nil, fmt.Errorf("-effort is one of %s, not %q", strings.Join(effortWords, ", "), word)
	}
	return &agentreplv1.SetEffortRequest{Workspace: ref, Effort: level}, nil
}

// accountRequest reads an -account flag into the request that makes workspace
// REF's session spend as the login root CONFIGDIR, an option the account cell
// serves.
func accountRequest(ref *workspacev1.WorkspaceRef, configDir string) (*agentreplv1.SelectAccountRequest, error) {
	if strings.TrimSpace(configDir) == "" {
		return nil, fmt.Errorf("-account names a login root from the account cell's options, not a blank")
	}
	return &agentreplv1.SelectAccountRequest{Workspace: ref, ConfigDir: configDir}, nil
}

// defaultModelRequest reads a -default-model flag into the request of the
// selector row's "Make default": the model every new session on workspace
// REF's login starts under.
func defaultModelRequest(ref *workspacev1.WorkspaceRef, model string) (*agentreplv1.SetDefaultModelRequest, error) {
	if strings.TrimSpace(model) == "" {
		return nil, fmt.Errorf("-default-model names a model from the catalog, not a blank")
	}
	return &agentreplv1.SetDefaultModelRequest{Workspace: ref, Model: &conversationv1.AgentModel{Name: model}}, nil
}

// persistentWifiRequest reads the mode `runtime persistent-wifi` names into
// the request that turns the machine's persistent wifi mode on or off. An
// agent names the mode it wants, never a toggle, so the answer is the same
// whatever the mode was.
func persistentWifiRequest(mode string) (*agentreplv1.UpdatePersistentWifiModeRequest, error) {
	req := &agentreplv1.UpdatePersistentWifiModeRequest{}
	switch mode {
	case "on":
		req.Action = &agentreplv1.UpdatePersistentWifiModeRequest_On{On: &agentreplv1.UpdatePersistentWifiModeOn{}}
	case "off":
		req.Action = &agentreplv1.UpdatePersistentWifiModeRequest_Off{Off: &agentreplv1.UpdatePersistentWifiModeOff{}}
	default:
		return nil, fmt.Errorf("persistent wifi mode is on or off, not %q", mode)
	}
	return req, nil
}

// runtimePersistentWifi turns the machine's persistent wifi mode on or off
// (agentrepl.v1.UpdatePersistentWifiMode), the topbar chip's setting. It is
// the machine's, so it names no workspace.
func runtimePersistentWifi(c *cliCall, args []string) int {
	fs, stateDir := c.flags()
	positionals, ok := c.parse(fs, args)
	if !ok {
		return exitFailure
	}
	if len(positionals) != 1 {
		return c.usageFailed("the command names the mode, on or off")
	}
	req, err := persistentWifiRequest(positionals[0])
	if err != nil {
		return c.usageFailed("%v", err)
	}
	if !c.dial(*stateDir) {
		return exitFailure
	}
	resp, err := c.client.UpdatePersistentWifiMode(c.ctx, connect.NewRequest(req))
	return answerDescribed(c, resp, err, func(msg *agentreplv1.UpdatePersistentWifiModeResponse) (string, error) {
		return describePersistentWifiRefusal(msg.GetError())
	})
}

// describePermissionModeRefusal says in one plain sentence why WS's
// permission mode was not set. Every SetPermissionModeError arm has its own
// sentence.
func describePermissionModeRefusal(ws, mode string, e *agentreplv1.SetPermissionModeError) (string, error) {
	switch r := e.GetCause().(type) {
	case *agentreplv1.SetPermissionModeError_UnknownWorkspace:
		return sayNoWorkspace(ws), nil
	case *agentreplv1.SetPermissionModeError_WorkspaceRefMismatch:
		return sayLivesElsewhere(ws, r.WorkspaceRefMismatch.GetRegistryDir(), "the request"), nil
	case *agentreplv1.SetPermissionModeError_TransferringAway:
		return sayHandedOver(ws, r.TransferringAway.GetAddress(), "set it again"), nil
	case *agentreplv1.SetPermissionModeError_NotYetAdopted:
		return sayNotYetAdopted(ws, "set it again"), nil
	case *agentreplv1.SetPermissionModeError_ModeNotServed:
		return fmt.Sprintf("the mode %q is not among the permission modes %s's picker serves", mode, ws), nil
	case *agentreplv1.SetPermissionModeError_UngatedWithoutConsent:
		return fmt.Sprintf("the mode %q is ungated, and %s's creation recorded no consent to an ungated mode", mode, ws), nil
	case *agentreplv1.SetPermissionModeError_NoSession:
		return fmt.Sprintf("%s has no session to set a permission mode on", ws), nil
	case *agentreplv1.SetPermissionModeError_VendorRefused:
		return fmt.Sprintf("the vendor refused the permission mode %q for %s (%s)", mode, ws, r.VendorRefused.GetDetail()), nil
	}
	return "", undescribed("the permission mode of "+ws, e)
}

// describeEffortRefusal says in one plain sentence why WS's effort was not
// set. Every SetEffortError arm has its own sentence.
func describeEffortRefusal(ws, effort string, e *agentreplv1.SetEffortError) (string, error) {
	switch r := e.GetCause().(type) {
	case *agentreplv1.SetEffortError_UnknownWorkspace:
		return sayNoWorkspace(ws), nil
	case *agentreplv1.SetEffortError_WorkspaceRefMismatch:
		return sayLivesElsewhere(ws, r.WorkspaceRefMismatch.GetRegistryDir(), "the request"), nil
	case *agentreplv1.SetEffortError_TransferringAway:
		return sayHandedOver(ws, r.TransferringAway.GetAddress(), "set it again"), nil
	case *agentreplv1.SetEffortError_NotYetAdopted:
		return sayNotYetAdopted(ws, "set it again"), nil
	case *agentreplv1.SetEffortError_NoSession:
		return fmt.Sprintf("%s has no session to set an effort on", ws), nil
	case *agentreplv1.SetEffortError_NotSupported:
		return fmt.Sprintf("%s's model does not accept the effort %q, or takes no effort level at all", ws, effort), nil
	case *agentreplv1.SetEffortError_VendorRefused:
		return fmt.Sprintf("the vendor refused the effort %q for %s (%s)", effort, ws, r.VendorRefused.GetDetail()), nil
	}
	return "", undescribed("the effort of "+ws, e)
}

// describeSelectAccountRefusal says in one plain sentence why WS's session
// did not switch accounts. Every SelectAccountError arm has its own sentence.
func describeSelectAccountRefusal(ws, configDir string, e *agentreplv1.SelectAccountError) (string, error) {
	switch r := e.GetCause().(type) {
	case *agentreplv1.SelectAccountError_UnknownWorkspace:
		return sayNoWorkspace(ws), nil
	case *agentreplv1.SelectAccountError_WorkspaceRefMismatch:
		return sayLivesElsewhere(ws, r.WorkspaceRefMismatch.GetRegistryDir(), "the request"), nil
	case *agentreplv1.SelectAccountError_TransferringAway:
		return sayHandedOver(ws, r.TransferringAway.GetAddress(), "set it again"), nil
	case *agentreplv1.SelectAccountError_NotYetAdopted:
		return sayNotYetAdopted(ws, "set it again"), nil
	case *agentreplv1.SelectAccountError_UnknownAccount:
		return fmt.Sprintf("the login root %q is not one the daemon knows, so %s still spends as its own account", configDir, ws), nil
	}
	return "", undescribed("the account of "+ws, e)
}

// describeDefaultModelRefusal says in one plain sentence why WS's login's
// default model was not set. Every SetDefaultModelError arm has its own
// sentence.
func describeDefaultModelRefusal(ws, model string, e *agentreplv1.SetDefaultModelError) (string, error) {
	switch r := e.GetCause().(type) {
	case *agentreplv1.SetDefaultModelError_UnknownWorkspace:
		return sayNoWorkspace(ws), nil
	case *agentreplv1.SetDefaultModelError_WorkspaceRefMismatch:
		return sayLivesElsewhere(ws, r.WorkspaceRefMismatch.GetRegistryDir(), "the request"), nil
	case *agentreplv1.SetDefaultModelError_TransferringAway:
		return sayHandedOver(ws, r.TransferringAway.GetAddress(), "set it again"), nil
	case *agentreplv1.SetDefaultModelError_NotYetAdopted:
		return sayNotYetAdopted(ws, "set it again"), nil
	case *agentreplv1.SetDefaultModelError_NoSession:
		return fmt.Sprintf("%s has no session whose login the default model could be set for", ws), nil
	case *agentreplv1.SetDefaultModelError_NotInCatalog:
		return fmt.Sprintf("the model %q is not in the catalog of %s's login, so the default is unchanged", model, ws), nil
	}
	return "", undescribed("the default model of "+ws, e)
}

// describePersistentWifiRefusal says in one plain sentence why the machine's
// persistent wifi mode was not changed. Every UpdatePersistentWifiModeError
// arm has its own sentence.
func describePersistentWifiRefusal(e *agentreplv1.UpdatePersistentWifiModeError) (string, error) {
	switch r := e.GetCause().(type) {
	case *agentreplv1.UpdatePersistentWifiModeError_PowerSettingsRefused:
		return fmt.Sprintf("changing the power settings was refused (%s); the passwordless sudo grant for the exact pmset invocations is absent or refused", r.PowerSettingsRefused.GetDetail()), nil
	case *agentreplv1.UpdatePersistentWifiModeError_ModeUnreadable:
		return fmt.Sprintf("the current persistent wifi mode could not be read (%s), so nothing was changed", r.ModeUnreadable.GetDetail()), nil
	}
	return "", undescribed("the persistent wifi mode", e)
}
