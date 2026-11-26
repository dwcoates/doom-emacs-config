package wsm

// Siblings are the open workspaces that share WS's parent: the other children
// of the workspace that owns it, in ALL's order. A workspace with no parent has
// none.
func Siblings(all []Workspace, ws Workspace) []Workspace {
	if ws.Parent == nil {
		return nil
	}
	var out []Workspace
	for _, other := range all {
		if other.ID == ws.ID || other.Closed || other.Parent == nil || *other.Parent != *ws.Parent {
			continue
		}
		out = append(out, other)
	}
	return out
}
