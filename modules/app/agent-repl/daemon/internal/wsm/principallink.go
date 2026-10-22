package wsm

import (
	"context"
	"errors"
	"fmt"

	"agent-repld/internal/dlog"
)

// principalLinkDDL is the layout-71 addition: principal_link, the url that
// best stands for the workspace's task (agentrepl.v1.SetWorkspacePrincipalLink),
// which the topbar's summary opens; NULL while the workspace has none. A
// close leaves it standing: a reopened workspace has the same task.
const principalLinkDDL = `
ALTER TABLE workspaces ADD COLUMN principal_link TEXT;
`

// SetPrincipalLink records the workspace's principal link, or clears it with
// nil. An empty link is a defect in the caller (clearing is nil, never an
// empty string) and is refused rather than stored.
func (s *store) SetPrincipalLink(ctx context.Context, id WorkspaceID, link *string) error {
	const op = "daemon.wsm.set_principal_link"
	fields := dlog.Context{"cleared": link == nil}
	var value any
	if link != nil {
		if *link == "" {
			err := fmt.Errorf("wsm: principal link of %s: %w", id, errEmptyPrincipalLink)
			s.log.Error(op, "refused an empty principal link", withError(dlog.Context{"workspace": string(id)}, err))
			return err
		}
		value = *link
		fields["url"] = *link
	}
	return s.setWorkspaceColumns(ctx, op, id, fields, workspaceColumn{name: "principal_link", value: value})
}

// errEmptyPrincipalLink is the empty-link refusal: a cleared link is NULL.
var errEmptyPrincipalLink = errors.New("a principal link is a url; clearing it is nil")
