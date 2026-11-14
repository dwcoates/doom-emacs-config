package wsm

import (
	"reflect"
	"testing"
)

func TestSiblings(t *testing.T) {
	p, q := WorkspaceID("p"), WorkspaceID("q")
	me := Workspace{ID: "me", Parent: &p}
	tests := []struct {
		name string
		ws   Workspace
		all  []Workspace
		want []WorkspaceID
	}{
		{name: "the other open children of the same parent", ws: me,
			all:  []Workspace{me, {ID: "a", Parent: &p}, {ID: "b", Parent: &p}},
			want: []WorkspaceID{"a", "b"}},
		{name: "a child of another parent is no sibling", ws: me,
			all: []Workspace{me, {ID: "a", Parent: &q}}},
		{name: "a closed child is no sibling", ws: me,
			all: []Workspace{me, {ID: "a", Parent: &p, Closed: true}}},
		{name: "a merge workspace is no sibling", ws: me,
			all: []Workspace{me, {ID: "a", Parent: &p, MergeOf: &MergeOrigin{}}}},
		{name: "a top-level workspace has no siblings", ws: Workspace{ID: "top"},
			all: []Workspace{{ID: "x"}, {ID: "y"}}},
		{name: "a top-level workspace is no one's sibling", ws: me,
			all: []Workspace{me, {ID: "x"}}},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Act
			var got []WorkspaceID
			for _, s := range Siblings(tc.all, tc.ws) {
				got = append(got, s.ID)
			}

			// Assert
			if !reflect.DeepEqual(got, tc.want) {
				t.Errorf("Siblings = %v, want %v", got, tc.want)
			}
		})
	}
}
