package gitclient

import (
	"context"
	"reflect"
	"testing"
)

func TestTagListingsAskGitForTheRightSetAndSplitItsLines(t *testing.T) {
	tests := []struct {
		name    string
		list    func(Git) ([]string, error)
		stdout  string
		subject []string
		want    []string
	}{
		{
			name:    "tags reachable from a commit",
			list:    func(g Git) ([]string, error) { return g.TagsMerged(context.Background(), "/repo", "abc123") },
			stdout:  "0.0.1\n0.1.0\nstart/x\n",
			subject: []string{"tag", "--list", "--merged", "abc123"},
			want:    []string{"0.0.1", "0.1.0", "start/x"},
		},
		{
			name:    "tags pointing at a commit",
			list:    func(g Git) ([]string, error) { return g.TagsAt(context.Background(), "/repo", "abc123") },
			stdout:  "0.2.0\n",
			subject: []string{"tag", "--list", "--points-at", "abc123"},
			want:    []string{"0.2.0"},
		},
		{
			name:    "no tags at all",
			list:    func(g Git) ([]string, error) { return g.TagsMerged(context.Background(), "/repo", "abc123") },
			stdout:  "",
			subject: []string{"tag", "--list", "--merged", "abc123"},
			want:    nil,
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			git, _ := newTestClient(t)
			fake := newFakeGit(t, ok(tt.stdout))

			// Act
			got, err := tt.list(git)

			// Assert
			if err != nil || !reflect.DeepEqual(got, tt.want) {
				t.Fatalf("list = %q, %v; want %q", got, err, tt.want)
			}
			fake.assertSubject(0, tt.subject...)
		})
	}
}

func TestCreateAnnotatedTagNeverForcesAnExistingName(t *testing.T) {
	// Arrange: without -f git refuses a name that is already a tag, which is
	// what keeps a version from being re-pointed.
	git, _ := newTestClient(t)
	fake := newFakeGit(t, ok(""))

	// Act
	err := git.CreateAnnotatedTag(context.Background(), "/repo", "0.2.0", "abc123", "Merges record a version")

	// Assert
	if err != nil {
		t.Fatalf("CreateAnnotatedTag: %v", err)
	}
	fake.assertSubject(0, "tag", "--annotate", "--message", "Merges record a version", "--end-of-options", "0.2.0", "abc123")
}

func TestCreateAnnotatedTagSurfacesAndLogsGitsRefusal(t *testing.T) {
	// Arrange
	git, surfaces := newTestClient(t)
	newFakeGit(t, fails(128, "fatal: tag '0.2.0' already exists\n"))

	// Act
	err := git.CreateAnnotatedTag(context.Background(), "/repo", "0.2.0", "abc123", "x")

	// Assert
	if err == nil {
		t.Fatal("CreateAnnotatedTag = nil, want git's refusal")
	}
	record, logged := recordFor(surfaces.records(), "error", "daemon.gitclient.create_annotated_tag")
	if !logged || record.Context["exit_code"] == nil {
		t.Fatalf("records = %+v, want an ERROR create_annotated_tag record with the exit code", surfaces.records())
	}
}

func TestTagsMergedSurfacesAndLogsGitsFailure(t *testing.T) {
	// Arrange
	git, surfaces := newTestClient(t)
	newFakeGit(t, fails(129, "error: malformed object name\n"))

	// Act
	got, err := git.TagsMerged(context.Background(), "/repo", "nope")

	// Assert
	if err == nil || got != nil {
		t.Fatalf("TagsMerged = %q, %v; want git's failure and no tags", got, err)
	}
	if _, logged := recordFor(surfaces.records(), "error", "daemon.gitclient.tags_merged"); !logged {
		t.Fatalf("records = %+v, want an ERROR tags_merged record", surfaces.records())
	}
}
