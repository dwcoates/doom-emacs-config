package wsm

import (
	"context"
	"errors"
	"testing"
)

func TestSetPrincipalLinkRecordsReplacesAndClears(t *testing.T) {
	first, second := "https://github.com/o/r/pull/1", "https://github.com/o/r/pull/2"
	tests := []struct {
		name  string
		steps []*string
		want  *string
	}{
		{name: "a set is read back", steps: []*string{&first}, want: &first},
		{name: "a second set replaces the first", steps: []*string{&first, &second}, want: &second},
		{name: "a clear leaves none", steps: []*string{&first, nil}, want: nil},
		{name: "clearing a workspace with none is no change", steps: []*string{nil}, want: nil},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, _ := testStore(t)
			ws := testWorkspace(t, s)

			// Act
			for _, link := range tt.steps {
				if err := s.SetPrincipalLink(context.Background(), ws.ID, link); err != nil {
					t.Fatalf("SetPrincipalLink(%v): %v", link, err)
				}
			}

			// Assert
			got, err := s.Workspace(context.Background(), ws.ID)
			if err != nil {
				t.Fatalf("Workspace: %v", err)
			}
			if (got.PrincipalLink == nil) != (tt.want == nil) || (tt.want != nil && *got.PrincipalLink != *tt.want) {
				t.Fatalf("principal link = %v, want %v", deref(got.PrincipalLink), deref(tt.want))
			}
		})
	}
}

func TestThePrincipalLinkOutlivesTheWorkspacesClose(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)
	link := "https://example.com/ticket/7"
	if err := s.SetPrincipalLink(context.Background(), ws.ID, &link); err != nil {
		t.Fatalf("SetPrincipalLink: %v", err)
	}

	// Act
	err := s.MarkClosed(context.Background(), ws.ID, Closure{At: instant, By: CloseByUser})

	// Assert
	if err != nil {
		t.Fatalf("MarkClosed: %v", err)
	}
	got, err := s.Workspace(context.Background(), ws.ID)
	if err != nil || got.PrincipalLink == nil || *got.PrincipalLink != link {
		t.Fatalf("after close: %v, %v; want %q", deref(got.PrincipalLink), err, link)
	}
}

func TestSetPrincipalLinkRefusesAnEmptyLinkAndWritesNothing(t *testing.T) {
	// Arrange
	s, log := testStore(t)
	ws := testWorkspace(t, s)
	standing := "https://example.com/standing"
	if err := s.SetPrincipalLink(context.Background(), ws.ID, &standing); err != nil {
		t.Fatalf("SetPrincipalLink: %v", err)
	}
	empty := ""

	// Act
	err := s.SetPrincipalLink(context.Background(), ws.ID, &empty)

	// Assert
	if !errors.Is(err, errEmptyPrincipalLink) {
		t.Fatalf("SetPrincipalLink(\"\") = %v, want errEmptyPrincipalLink", err)
	}
	got, _ := s.Workspace(context.Background(), ws.ID)
	if got.PrincipalLink == nil || *got.PrincipalLink != standing {
		t.Errorf("principal link = %v, want the standing %q untouched", deref(got.PrincipalLink), standing)
	}
	found := false
	for _, r := range log.Records() {
		if r.Operation == "daemon.wsm.set_principal_link" && r.Level == "error" && r.Context["workspace"] == string(ws.ID) {
			found = true
		}
	}
	if !found {
		t.Errorf("no error record of daemon.wsm.set_principal_link names the workspace: %v", log.Records())
	}
}

func TestSetPrincipalLinkOfAnUnknownWorkspaceIsNotFound(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	link := "https://example.com"

	// Act
	err := s.SetPrincipalLink(context.Background(), WorkspaceID("absent"), &link)

	// Assert
	if !errors.Is(err, ErrNotFound) {
		t.Fatalf("SetPrincipalLink = %v, want ErrNotFound", err)
	}
}

// deref renders an optional link for a failure message.
func deref(link *string) string {
	if link == nil {
		return "<none>"
	}
	return *link
}
