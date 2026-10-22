package workspace

import (
	"context"
	"fmt"

	"agent-repld/internal/dlog"
	"agent-repld/internal/externalbrowser"
	"agent-repld/internal/ids"
)

// SetPrincipalLink records the workspace's principal link, or clears it with
// nil (agentrepl.v1.SetWorkspacePrincipalLink), and republishes the topbar
// title that opens it. A url the external browser would refuse to open is
// refused invalid_link before anything is written, so every stored link is one
// OpenExternal can open.
func (v *verbs) SetPrincipalLink(ctx context.Context, ws ids.WorkspaceID, link *string) error {
	const rpc = "SetWorkspacePrincipalLink"
	_, log, err := v.owned(ctx, rpc, ws)
	if err != nil {
		return err
	}
	fields := dlog.Context{"cleared": link == nil}
	if link != nil {
		url := *link
		fields["url"] = url
		if err := externalbrowser.Validate(url); err != nil {
			return refuseWith(log, rpc, ArmInvalidLink,
				fmt.Sprintf("%q is not a link the external browser opens: %v", url, err), false,
				map[string]any{"url": url, "reason": err.Error()})
		}
	}
	if err := v.deps.DB.SetPrincipalLink(ctx, ws, link); err != nil {
		log.Error(opSetPrincipalLink, "could not record the principal link", dlog.WithCause(fields, err))
		return fmt.Errorf("set principal link on %q: %w", ws, err)
	}
	v.deps.Topbar.SetPrincipalLink(ws, link)
	if link == nil {
		log.Info(opSetPrincipalLink, "cleared the principal link", fields)
	} else {
		log.Info(opSetPrincipalLink, "set the principal link", fields)
	}
	return nil
}
