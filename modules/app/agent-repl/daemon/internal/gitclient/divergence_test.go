package gitclient

import (
	"context"
	"errors"
	"strings"
	"testing"
)

func TestUpstreamAsksForEachRefForTheBranch(t *testing.T) {
	// Arrange.
	git, _ := newTestClient(t)
	fake := newFakeGit(t, ok("refs/heads/master\x00origin\x00origin/master\n"))

	// Act.
	if _, _, err := git.Upstream(context.Background(), "/repo", "master"); err != nil {
		t.Fatalf("Upstream: %v", err)
	}

	// Assert.
	fake.assertSubject(0, "for-each-ref", "--format="+upstreamFormat, "refs/heads/master")
}

func TestUpstreamAnswers(t *testing.T) {
	tests := []struct {
		name      string
		stdout    string
		want      Upstream
		wantFound bool
	}{
		{
			name:      "a tracked branch answers its remote and ref",
			stdout:    "refs/heads/master\x00origin\x00origin/master\n",
			want:      Upstream{Remote: "origin", Ref: "origin/master"},
			wantFound: true,
		},
		{
			name:      "a branch that tracks nothing is an ordinary answer",
			stdout:    "refs/heads/master\x00\x00\n",
			wantFound: false,
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			git, surfaces := newTestClient(t)
			newFakeGit(t, ok(tc.stdout))

			// Act.
			got, found, err := git.Upstream(context.Background(), "/repo", "master")

			// Assert.
			if err != nil || found != tc.wantFound || got != tc.want {
				t.Fatalf("Upstream = %+v, %v, %v; want %+v, %v, nil", got, found, err, tc.want, tc.wantFound)
			}
			if _, logged := recordFor(surfaces.records(), "error", "daemon.gitclient.upstream"); logged {
				t.Fatal("an ordinary answer was recorded at ERROR")
			}
		})
	}
}

func TestUpstreamRefusals(t *testing.T) {
	tests := []struct {
		name      string
		stdout    string
		wantCause string
	}{
		{name: "a missing branch", stdout: "", wantCause: "does not exist"},
		{name: "a half-configured upstream", stdout: "refs/heads/master\x00origin\x00\n", wantCause: "half-configured"},
		{name: "an answer naming another ref", stdout: "refs/heads/main\x00\x00\n", wantCause: "want one line naming the branch"},
		{name: "an answer with two lines", stdout: "refs/heads/master\x00\x00\nrefs/heads/master\x00\x00\n", wantCause: "want one line naming the branch"},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			git, surfaces := newTestClient(t)
			newFakeGit(t, ok(tc.stdout))

			// Act.
			got, found, err := git.Upstream(context.Background(), "/repo", "master")

			// Assert.
			if err == nil || !strings.Contains(err.Error(), tc.wantCause) || found || got != (Upstream{}) {
				t.Fatalf("Upstream = %+v, %v, %v; want a refusal containing %q", got, found, err, tc.wantCause)
			}
			record, logged := recordFor(surfaces.records(), "error", "daemon.gitclient.upstream")
			if !logged || record.Context["branch"] != "master" || record.Context["dir"] != "/repo" {
				t.Fatalf("refusal record = %+v (logged %v), want an ERROR naming the branch and dir", record, logged)
			}
		})
	}
}

func TestUpstreamCarriesGitsOwnFailure(t *testing.T) {
	// Arrange.
	git, _ := newTestClient(t)
	newFakeGit(t, fails(128, "fatal: not a git repository\n"))

	// Act.
	_, _, err := git.Upstream(context.Background(), "/repo", "master")

	// Assert.
	var failure *Error
	if !errors.As(err, &failure) || failure.ExitCode != 128 {
		t.Fatalf("Upstream error = %v (%T), want git's exit-128 failure", err, err)
	}
}

func TestAheadBehindAsksForALeftRightCount(t *testing.T) {
	// Arrange.
	git, _ := newTestClient(t)
	fake := newFakeGit(t, ok("2\t5\n"))

	// Act.
	ahead, behind, err := git.AheadBehind(context.Background(), "/repo", "master", "origin/master")

	// Assert.
	if err != nil || ahead != 2 || behind != 5 {
		t.Fatalf("AheadBehind = %d, %d, %v; want 2, 5, nil", ahead, behind, err)
	}
	fake.assertSubject(0, "rev-list", "--left-right", "--count", "--end-of-options", "master...origin/master")
}

func TestAheadBehindRefusesAnUnreadableCount(t *testing.T) {
	// Arrange.
	git, surfaces := newTestClient(t)
	newFakeGit(t, ok("two\t5\n"))

	// Act.
	ahead, behind, err := git.AheadBehind(context.Background(), "/repo", "master", "origin/master")

	// Assert.
	if err == nil || ahead != 0 || behind != 0 {
		t.Fatalf("AheadBehind = %d, %d, %v; want a refusal and no counts", ahead, behind, err)
	}
	record, logged := recordFor(surfaces.records(), "error", "daemon.gitclient.ahead_behind")
	if !logged || record.Context["stdout"] != "two\t5" || record.Context["upstream"] != "origin/master" {
		t.Fatalf("refusal record = %+v (logged %v), want an ERROR carrying git's answer", record, logged)
	}
}

func TestAheadBehindCarriesGitsOwnFailure(t *testing.T) {
	// Arrange: an upstream ref that does not resolve.
	git, _ := newTestClient(t)
	newFakeGit(t, fails(128, "fatal: ambiguous argument\n"))

	// Act.
	_, _, err := git.AheadBehind(context.Background(), "/repo", "master", "origin/master")

	// Assert.
	var failure *Error
	if !errors.As(err, &failure) || failure.ExitCode != 128 {
		t.Fatalf("AheadBehind error = %v (%T), want git's exit-128 failure", err, err)
	}
}

func TestParseLeftRightCount(t *testing.T) {
	tests := []struct {
		name       string
		in         string
		wantAhead  int
		wantBehind int
		wantErr    bool
	}{
		{name: "two counts", in: "0\t12", wantAhead: 0, wantBehind: 12},
		{name: "no tab", in: "3 4", wantErr: true},
		{name: "a negative count", in: "-1\t4", wantErr: true},
		{name: "a non-number right", in: "3\tx", wantErr: true},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Act.
			ahead, behind, err := parseLeftRightCount(tc.in)

			// Assert.
			if (err != nil) != tc.wantErr || ahead != tc.wantAhead || behind != tc.wantBehind {
				t.Fatalf("parseLeftRightCount(%q) = %d, %d, %v", tc.in, ahead, behind, err)
			}
		})
	}
}
