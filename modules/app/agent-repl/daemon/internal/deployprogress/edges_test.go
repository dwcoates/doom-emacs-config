package deployprogress

import (
	"slices"
	"testing"
)

type recordingSink struct{ got []*Progress }

func (r *recordingSink) SetDeployProgress(p *Progress) { r.got = append(r.got, p) }

func TestEdgesTellEachDeploysEdgesOnce(t *testing.T) {
	tests := []struct {
		name   string
		phases []*Progress
		want   []string
	}{
		{name: "a deploy that finishes here", phases: []*Progress{{Phase: Building}, {Phase: Building}, {Phase: Installing}, {Phase: Updated}}, want: []string{"began", "ended finished"}},
		{name: "a deploy whose line is taken down stopped short", phases: []*Progress{{Phase: Building}, nil}, want: []string{"began", "ended stopped short"}},
		{name: "a successor finishing a deploy it took over", phases: []*Progress{{Phase: Updated}}, want: []string{"ended finished"}},
		{name: "a deploy that changed nothing", phases: []*Progress{{Phase: Building}, {Phase: Updated, Unchanged: true}}, want: []string{"began", "ended unchanged"}},
		{name: "two deploys", phases: []*Progress{{Phase: Building}, {Phase: Updated}, {Phase: Building}, {Phase: Updated}}, want: []string{"began", "ended finished", "began", "ended finished"}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			var got []string
			inner := &recordingSink{}
			names := map[Ending]string{EndedFinished: "ended finished", EndedUnchanged: "ended unchanged", EndedStoppedShort: "ended stopped short"}
			e := NewEdges(inner, func() { got = append(got, "began") }, func(ending Ending) {
				got = append(got, names[ending])
			})

			// Act
			for _, p := range tt.phases {
				e.SetDeployProgress(p)
			}

			// Assert
			if !slices.Equal(got, tt.want) || len(inner.got) != len(tt.phases) {
				t.Fatalf("edges %v (inner saw %d), want %v and every progress passed on", got, len(inner.got), tt.want)
			}
		})
	}
}

func TestNewEdgesRefusesAMissingPart(t *testing.T) {
	// Arrange
	defer func() {
		if recover() == nil {
			t.Fatal("NewEdges with no sink did not panic")
		}
	}()

	// Act
	NewEdges(nil, func() {}, func(Ending) {})
}
