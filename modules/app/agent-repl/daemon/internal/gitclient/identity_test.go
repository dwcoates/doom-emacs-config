package gitclient

import (
	"context"
	"errors"
	"reflect"
	"strings"
	"testing"
)

// identityLine is one CommitIdentities output line.
func identityLine(sha, subject, an, ae, cn, ce string) string {
	return strings.Join([]string{sha, subject, an, ae, cn, ce}, fieldSep)
}

func TestIdentityStringIsGitsForm(t *testing.T) {
	// Arrange, Act.
	got := Identity{Name: "Pat Example", Email: "pat@example.com"}.String()

	// Assert.
	if want := "Pat Example <pat@example.com>"; got != want {
		t.Fatalf("String() = %q, want %q", got, want)
	}
}

func TestGlobalIdentityReadsBothGlobalKeys(t *testing.T) {
	// Arrange.
	git, _ := newTestClient(t)
	fake := newFakeGit(t,
		ok("Pat Example\n", "config", "--global", "user.name"),
		ok("pat@example.com\n", "config", "--global", "user.email"))

	// Act.
	got, err := git.GlobalIdentity(context.Background(), "/repo")

	// Assert.
	if err != nil || got != (Identity{Name: "Pat Example", Email: "pat@example.com"}) {
		t.Fatalf("GlobalIdentity = %+v, %v; want Pat Example <pat@example.com>", got, err)
	}
	fake.assertSubject(0, "config", "--global", "user.name")
	fake.assertSubject(1, "config", "--global", "user.email")
}

func TestGlobalIdentityAnswersAnUnsetKeyAsEmpty(t *testing.T) {
	// Arrange.
	git, surfaces := newTestClient(t)
	newFakeGit(t,
		fails(1, "", "config", "--global", "user.name"),
		ok("pat@example.com\n", "config", "--global", "user.email"))

	// Act.
	got, err := git.GlobalIdentity(context.Background(), "/repo")

	// Assert.
	if err != nil || got != (Identity{Email: "pat@example.com"}) {
		t.Fatalf("GlobalIdentity = %+v, %v; want an empty name and no error", got, err)
	}
	if record, logged := recordFor(surfaces.records(), "error", "daemon.gitclient.global_identity"); logged {
		t.Fatalf("an unset key is an ordinary answer, but it was logged: %+v", record)
	}
}

func TestGlobalIdentityFailsLoudlyOnGitsOwnFailure(t *testing.T) {
	// Arrange.
	git, surfaces := newTestClient(t)
	newFakeGit(t, fails(3, "fatal: bad config line 1\n", "config", "--global", "user.name"))

	// Act.
	got, err := git.GlobalIdentity(context.Background(), "/repo")

	// Assert.
	var failure *Error
	if !errors.As(err, &failure) || failure.ExitCode != 3 || got != (Identity{}) {
		t.Fatalf("GlobalIdentity = %+v, %v; want git's own failure", got, err)
	}
	record, logged := recordFor(surfaces.records(), "error", "daemon.gitclient.global_identity")
	if !logged || record.Context["exit_code"] != 3 || record.Context["dir"] != "/repo" {
		t.Fatalf("failure record = %+v (logged %v), want an ERROR carrying the exit code and dir", record, logged)
	}
}

func TestCommitIdentitiesWalksTheRangeOldestFirstWithMerges(t *testing.T) {
	// Arrange.
	git, _ := newTestClient(t)
	fake := newFakeGit(t, ok("", "rev-list"))

	// Act.
	if _, err := git.CommitIdentities(context.Background(), "/repo", "base", "tip"); err != nil {
		t.Fatalf("CommitIdentities: %v", err)
	}

	// Assert.
	fake.assertSubject(0, "rev-list", "--reverse", "--format="+identityFormat, "--no-commit-header", "--end-of-options", "base..tip")
}

func TestCommitIdentitiesAnswersEveryCommitsAuthorAndCommitter(t *testing.T) {
	// Arrange.
	git, _ := newTestClient(t)
	newFakeGit(t, ok(identityLine("aaa", "one", "Pat", "pat@example.com", "Pat", "pat@example.com")+"\n"+
		identityLine("bbb", "two", "Test", "test@example.com", "Pat", "pat@example.com")+"\n", "rev-list"))

	// Act.
	got, err := git.CommitIdentities(context.Background(), "/repo", "base", "tip")

	// Assert.
	pat, test := Identity{"Pat", "pat@example.com"}, Identity{"Test", "test@example.com"}
	want := []CommitIdentity{
		{SHA: "aaa", Subject: "one", Author: pat, Committer: pat},
		{SHA: "bbb", Subject: "two", Author: test, Committer: pat},
	}
	if err != nil || !reflect.DeepEqual(got, want) {
		t.Fatalf("CommitIdentities = %+v, %v; want %+v", got, err, want)
	}
}

func TestCommitIdentitiesRefusesALineMissingAField(t *testing.T) {
	// Arrange.
	git, surfaces := newTestClient(t)
	newFakeGit(t, ok(strings.Join([]string{"aaa", "one", "Pat"}, fieldSep)+"\n", "rev-list"))

	// Act.
	got, err := git.CommitIdentities(context.Background(), "/repo", "base", "tip")

	// Assert.
	if err == nil || got != nil {
		t.Fatalf("CommitIdentities = %+v, %v; want a refusal", got, err)
	}
	record, logged := recordFor(surfaces.records(), "error", "daemon.gitclient.commit_identities")
	if !logged || record.Context["fields"] != 3 || record.Context["tip"] != "tip" {
		t.Fatalf("refusal record = %+v (logged %v), want an ERROR naming the field count and range", record, logged)
	}
}

func TestCommitIdentitiesCarriesGitsOwnFailure(t *testing.T) {
	// Arrange.
	git, _ := newTestClient(t)
	newFakeGit(t, fails(128, "fatal: bad revision\n", "rev-list"))

	// Act.
	_, err := git.CommitIdentities(context.Background(), "/repo", "base", "tip")

	// Assert.
	var failure *Error
	if !errors.As(err, &failure) || !strings.Contains(failure.Stderr, "bad revision") {
		t.Fatalf("CommitIdentities error = %v, want git's own evidence", err)
	}
}
