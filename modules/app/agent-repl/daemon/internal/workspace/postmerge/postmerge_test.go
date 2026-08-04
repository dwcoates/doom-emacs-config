package postmerge

import (
	"errors"
	"strings"
	"testing"

	"claude-repld/internal/workspace/merge"
)

type fakePostprocessing struct {
	prompt string
	err    error
	got    string
}

func (f *fakePostprocessing) PostprocessingPrompt(path string) (string, error) {
	f.got = path
	return f.prompt, f.err
}

func TestNewRequiresEveryDependency(t *testing.T) {
	post := &fakePostprocessing{}
	for _, tc := range []struct {
		name string
		cfg  Config
		want string
	}{
		{name: "log", cfg: Config{Postprocessing: post}, want: "Logf"},
		{name: "postprocessing", cfg: Config{Logf: t.Logf}, want: "Postprocessing"},
	} {
		t.Run(tc.name, func(t *testing.T) {
			got, err := New(tc.cfg)
			if err == nil || got != nil {
				t.Fatalf("New() = (%v, %v), want a construction error", got, err)
			}
			if !strings.Contains(err.Error(), tc.want) {
				t.Fatalf("New() error = %v, want %q", err, tc.want)
			}
		})
	}
}

func TestAfterActionReadsTheChildCreationRecord(t *testing.T) {
	post := &fakePostprocessing{prompt: "publish the release notes"}
	source, err := New(Config{Logf: t.Logf, Postprocessing: post})
	if err != nil {
		t.Fatalf("New() error = %v", err)
	}

	got, err := source.AfterAction(merge.Request{
		Workspace: "/ws/child",
		Name:      "child",
		SourceDir: "/worktrees/child",
	})
	if err != nil {
		t.Fatalf("AfterAction() error = %v", err)
	}
	if got != post.prompt || post.got != "/worktrees/child" {
		t.Fatalf("AfterAction() = %q, lookup = %q", got, post.got)
	}
}

func TestAfterActionSurfacesLookupFailure(t *testing.T) {
	post := &fakePostprocessing{err: errors.New("records unreadable")}
	source, err := New(Config{Logf: t.Logf, Postprocessing: post})
	if err != nil {
		t.Fatalf("New() error = %v", err)
	}

	_, err = source.AfterAction(merge.Request{Name: "child", SourceDir: "/worktrees/child"})
	if err == nil || !strings.Contains(err.Error(), "records unreadable") {
		t.Fatalf("AfterAction() error = %v, want lookup failure", err)
	}
}
