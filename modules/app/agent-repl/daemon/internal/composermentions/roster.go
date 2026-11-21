package composermentions

import (
	frontendv1 "agentrepl/proto/frontend/v1"
	"agentrepl/protohelpers/rosterwalk"
)

// RosterNames answers the name of every workspace ROSTER, the roster's
// workspaces stream, holds open, hidden ones included, in the roster's order
// and without repeats: the workspaces
// the composer completes (lisp/workspace-completion.el), and so the names a
// response's words are matched against (Deps.WorkspaceNames). A nil roster,
// before the sidebar's first publication, holds none.
func RosterNames(roster *frontendv1.WorkspaceRoster) []string {
	var out []string
	seen := map[string]bool{}
	add := func(row *frontendv1.RosterRow) {
		if name := row.GetName().GetText(); name != "" && !seen[name] {
			seen[name] = true
			out = append(out, name)
		}
	}
	for _, open := range rosterwalk.OpenRows(roster) {
		add(open.GetRow())
	}
	for _, hidden := range rosterwalk.HiddenRows(roster) {
		add(hidden)
	}
	return out
}
