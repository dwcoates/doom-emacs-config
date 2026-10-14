package wsm

import (
	"context"
	"errors"
	"strings"
	"testing"
	"time"

	"agent-repld/internal/semver"
)

func TestARequestKeepsItsVersionIncrementInTheQueue(t *testing.T) {
	tests := []struct {
		name    string
		version *VersionIncrement
	}{
		{name: "a minor increment", version: &VersionIncrement{Part: semver.Minor, ChangelogLine: "Merges record a version"}},
		{name: "a patch increment", version: &VersionIncrement{Part: semver.Patch, ChangelogLine: "Fix the footer"}},
		{name: "no increment", version: nil},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, _ := testStore(t)
			ws := testWorkspace(t, s)
			repo := RepoKey(t.TempDir())
			source := MergeSource{Kind: MergeSourceOwnBranch, Version: tt.version}

			// Act
			err := s.RequestMerge(context.Background(), repo, ws.ID, source, time.Unix(100, 0))
			queue, readErr := s.MergeQueue(context.Background(), repo)

			// Assert
			if err != nil || readErr != nil || len(queue) != 1 {
				t.Fatalf("RequestMerge = %v, MergeQueue = %+v, %v; want one request", err, queue, readErr)
			}
			got := queue[0].Source.Version
			if (got == nil) != (tt.version == nil) || (got != nil && *got != *tt.version) {
				t.Fatalf("Version = %+v, want %+v", got, tt.version)
			}
		})
	}
}

func TestHandOffDetachedMergeCarriesTheVersionIncrementToTheMergeWorkspace(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	requester := testWorkspace(t, s)
	repo := RepoKey(t.TempDir())
	request := NewDetachedMergeID()
	version := &VersionIncrement{Part: semver.Minor, ChangelogLine: "Merges record a version"}
	source := MergeSource{Kind: MergeSourceBranch, Branch: "feature", Detach: request, DetachName: "sample-merge", Version: version}
	if err := s.RequestMerge(context.Background(), repo, requester.ID, source, time.Unix(100, 0)); err != nil {
		t.Fatalf("RequestMerge: %v", err)
	}
	forked := registerMergeWorkspace(t, s, requester.ID, request)

	// Act
	err := s.HandOffDetachedMerge(context.Background(), repo, requester.ID, request, forked.ID, "sample-merge", "", time.Unix(200, 0))

	// Assert
	if err != nil {
		t.Fatalf("HandOffDetachedMerge: %v", err)
	}
	queue, err := s.MergeQueue(context.Background(), repo)
	if err != nil || len(queue) != 1 || queue[0].Workspace != forked.ID {
		t.Fatalf("MergeQueue = %+v, %v; want the merge workspace's entry", queue, err)
	}
	if got := queue[0].Source.Version; got == nil || *got != *version {
		t.Fatalf("Version = %+v, want %+v carried over", got, version)
	}
}

func TestAVersionIncrementThatContradictsItsSourceIsInvalid(t *testing.T) {
	tests := []struct {
		name   string
		source MergeSource
		want   string
	}{
		{name: "an undeclared part", source: MergeSource{Kind: MergeSourceOwnBranch, Version: &VersionIncrement{Part: semver.Part(9), ChangelogLine: "x"}}, want: "moves minor or patch"},
		{name: "a blank changelog line", source: MergeSource{Kind: MergeSourceOwnBranch, Version: &VersionIncrement{Part: semver.Patch}}, want: "carries a changelog line"},
		{name: "a branch merged upstream", source: MergeSource{Kind: MergeSourceMergedUpstream, Version: &VersionIncrement{Part: semver.Patch, ChangelogLine: "x"}}, want: "lands nothing"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange in the table. Act.
			err := tt.source.validate()

			// Assert
			if err == nil || !strings.Contains(err.Error(), tt.want) {
				t.Fatalf("validate = %v, want %q", err, tt.want)
			}
		})
	}
}

func TestAHalfRecordedVersionIncrementIsRefusedOnRead(t *testing.T) {
	tests := []struct {
		name   string
		update string
	}{
		{name: "a part without a line", update: `UPDATE merge_queue SET source_version_part = 1 WHERE workspace_id = ?`},
		{name: "a line without a part", update: `UPDATE merge_queue SET source_changelog_line = 'x' WHERE workspace_id = ?`},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, _ := testStore(t)
			ws := testWorkspace(t, s)
			repo := RepoKey(t.TempDir())
			if err := s.RequestMerge(context.Background(), repo, ws.ID, MergeSource{Kind: MergeSourceOwnBranch}, time.Unix(100, 0)); err != nil {
				t.Fatalf("RequestMerge: %v", err)
			}
			corrupt(t, s, tt.update, ws.ID)

			// Act
			_, err := s.MergeQueue(context.Background(), repo)

			// Assert
			var decode *DecodeError
			if !errors.As(err, &decode) || decode.Field != "version" {
				t.Fatalf("MergeQueue = %v, want a version DecodeError", err)
			}
		})
	}
}
