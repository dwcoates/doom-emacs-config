package worktreereap

import (
	"os"
	"path/filepath"
	"strings"
	"testing"
	"time"

	"claude-repld/internal/ids"
	"claude-repld/internal/wsm"
)

// hoursAgo is an instant that many hours before now.
func hoursAgo(h float64) time.Time {
	return now.Add(-time.Duration(h * float64(time.Hour)))
}

// --- activeSince ---------------------------------------------------------

func TestActiveSince(t *testing.T) {
	cases := []struct {
		name     string
		instants []time.Time
		want     time.Time
	}{
		{name: "no activity at all is idle now", want: now},
		{name: "a repository idle for a full day right now restarts at now", instants: []time.Time{hoursAgo(24), hoursAgo(30)}, want: now},
		{name: "a repository idle for just under a day keeps its run", instants: []time.Time{hoursAgo(23.9), hoursAgo(30)}, want: hoursAgo(30)},
		{name: "a full-day gap mid-history starts the run after it", instants: []time.Time{hoursAgo(1), hoursAgo(20), hoursAgo(44), hoursAgo(50)}, want: hoursAgo(20)},
		{name: "a gap of exactly one day is a full day", instants: []time.Time{hoursAgo(1), hoursAgo(25)}, want: hoursAgo(1)},
		{name: "an unbroken history runs back to its earliest instant", instants: []time.Time{hoursAgo(1), hoursAgo(12), hoursAgo(23)}, want: hoursAgo(23)},
		{name: "the order instants arrive in does not matter", instants: []time.Time{hoursAgo(23), hoursAgo(1), hoursAgo(12)}, want: hoursAgo(23)},
		{name: "an instant after now counts as now", instants: []time.Time{now.Add(time.Hour), hoursAgo(10)}, want: hoursAgo(10)},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Act.
			got := activeSince(tc.instants, now)

			// Assert.
			if !got.Equal(tc.want) {
				t.Fatalf("activeSince = %v, want %v", got, tc.want)
			}
		})
	}
}

// --- readReflog ----------------------------------------------------------

func writeFile(t *testing.T, path, body string) {
	t.Helper()
	if err := os.MkdirAll(filepath.Dir(path), 0o755); err != nil {
		t.Fatalf("mkdir: %v", err)
	}
	if err := os.WriteFile(path, []byte(body), 0o644); err != nil {
		t.Fatalf("write %s: %v", path, err)
	}
}

func TestReadReflogAnswersEveryEntrysInstant(t *testing.T) {
	// Arrange: a message may carry '>' and a line may carry no message.
	path := filepath.Join(t.TempDir(), "HEAD")
	writeFile(t, path, reflogLine(hoursAgo(5))+
		"0000 1111 A Person <a@b.c> 1700000000 -0700\tmerge feat/x: a > b\n"+
		"0000 1111 A Person <a@b.c> 1700000100 +0000\n")

	// Act.
	got, err := readReflog(path)

	// Assert.
	want := []time.Time{time.Unix(hoursAgo(5).Unix(), 0), time.Unix(1700000000, 0), time.Unix(1700000100, 0)}
	if err != nil || len(got) != len(want) {
		t.Fatalf("readReflog = (%v, %v), want %v", got, err, want)
	}
	for i := range want {
		if !got[i].Equal(want[i]) {
			t.Fatalf("entry %d = %v, want %v", i, got[i], want[i])
		}
	}
}

func TestReadReflogOfAMissingFileIsEmpty(t *testing.T) {
	// Act.
	got, err := readReflog(filepath.Join(t.TempDir(), "absent"))

	// Assert.
	if err != nil || len(got) != 0 {
		t.Fatalf("readReflog = (%v, %v), want no entries and no error", got, err)
	}
}

func TestReadReflogRefusesALineThatIsNotAnEntry(t *testing.T) {
	// Arrange.
	path := filepath.Join(t.TempDir(), "HEAD")
	writeFile(t, path, reflogLine(hoursAgo(5))+"not a reflog entry\n")

	// Act.
	_, err := readReflog(path)

	// Assert.
	if err == nil || !strings.Contains(err.Error(), "line 2") {
		t.Fatalf("readReflog = %v, want line 2 refused", err)
	}
}

func TestReadReflogRefusesAnUnreadableTime(t *testing.T) {
	// Arrange.
	path := filepath.Join(t.TempDir(), "HEAD")
	writeFile(t, path, "0000 1111 A Person <a@b.c> soon +0000\tx\n")

	// Act.
	_, err := readReflog(path)

	// Assert.
	if err == nil || !strings.Contains(err.Error(), "unreadable time") {
		t.Fatalf("readReflog = %v, want the time refused", err)
	}
}

func TestReadReflogOfADirectoryIsAnError(t *testing.T) {
	// Arrange: opening succeeds, reading does not.
	path := t.TempDir()

	// Act.
	_, err := readReflog(path)

	// Assert.
	if err == nil {
		t.Fatal("readReflog of a directory = nil error, want a read failure")
	}
}

// --- repoActivity --------------------------------------------------------

func TestRepoActivityReadsEveryRefLogAndEveryLinkedWorktreesHEADLog(t *testing.T) {
	// Arrange.
	common := t.TempDir()
	writeFile(t, filepath.Join(common, "logs", "HEAD"), reflogLine(hoursAgo(1)))
	writeFile(t, filepath.Join(common, "logs", "refs", "heads", "feat", "x"), reflogLine(hoursAgo(2)))
	writeFile(t, filepath.Join(common, "worktrees", "wt", "logs", "HEAD"), reflogLine(hoursAgo(3)))
	writeFile(t, filepath.Join(common, "worktrees", "wt", "HEAD"), "not read\n")

	// Act.
	got, err := repoActivity(common, "repo", nil)

	// Assert.
	if err != nil || len(got) != 3 {
		t.Fatalf("repoActivity = (%v, %v), want the three reflog entries", got, err)
	}
}

func TestRepoActivityCountsOnlyItsOwnRepositorysWorkspaceStamps(t *testing.T) {
	// Arrange.
	selected, merged, activity := hoursAgo(4), hoursAgo(5), hoursAgo(6)
	workspaces := []wsm.Workspace{
		{Repo: "repo", CreatedAt: hoursAgo(3), LastSelectedAt: &selected, MergedAt: &merged, LastActivityAt: &activity},
		{Repo: ids.RepoID("other"), CreatedAt: hoursAgo(1)},
	}

	// Act.
	got, err := repoActivity(t.TempDir(), "repo", workspaces)

	// Assert.
	if err != nil || len(got) != 4 {
		t.Fatalf("repoActivity = (%v, %v), want the own workspace's four stamps", got, err)
	}
	for _, at := range got {
		if at.Equal(hoursAgo(1)) {
			t.Fatalf("repoActivity = %v, want no stamp of another repository", got)
		}
	}
}

func TestRepoActivityRefusesAReftableRepository(t *testing.T) {
	// Arrange.
	common := t.TempDir()
	writeFile(t, filepath.Join(common, "reftable", "tables.list"), "")

	// Act.
	_, err := repoActivity(common, "repo", nil)

	// Assert.
	if err == nil || !strings.Contains(err.Error(), "reftable") {
		t.Fatalf("repoActivity = %v, want the reftable refused", err)
	}
}

func TestRepoActivityPassesOnAMalformedReflog(t *testing.T) {
	// Arrange.
	common := t.TempDir()
	writeFile(t, filepath.Join(common, "logs", "HEAD"), "garbage\n")

	// Act.
	_, err := repoActivity(common, "repo", nil)

	// Assert.
	if err == nil || !strings.Contains(err.Error(), "not a reflog entry") {
		t.Fatalf("repoActivity = %v, want the malformed reflog refused", err)
	}
}

func TestRepoActivityRefusesAnUnlistableWorktreesDirectory(t *testing.T) {
	// Arrange: `worktrees` is a file, so listing it fails with something
	// other than "not there".
	common := t.TempDir()
	writeFile(t, filepath.Join(common, "worktrees"), "")

	// Act.
	_, err := repoActivity(common, "repo", nil)

	// Assert.
	if err == nil || !strings.Contains(err.Error(), "listing the linked worktrees") {
		t.Fatalf("repoActivity = %v, want the listing failure", err)
	}
}

// --- bornAt and reapedRef --------------------------------------------------

func TestBornAtIsTheCommondirFilesMtime(t *testing.T) {
	// Arrange.
	admin := t.TempDir()
	path := filepath.Join(admin, "commondir")
	writeFile(t, path, "../..\n")
	born := hoursAgo(400)
	if err := os.Chtimes(path, born, born); err != nil {
		t.Fatalf("chtimes: %v", err)
	}

	// Act.
	got, err := bornAt(admin)

	// Assert.
	if err != nil || !got.Equal(born) {
		t.Fatalf("bornAt = (%v, %v), want %v", got, err, born)
	}
}

func TestBornAtWithoutACommondirFileIsAnError(t *testing.T) {
	// Act.
	_, err := bornAt(t.TempDir())

	// Assert.
	if err == nil || !strings.Contains(err.Error(), "commondir") {
		t.Fatalf("bornAt = %v, want the missing commondir refused", err)
	}
}

func TestReapedRefNamesTheWorktreeAndTheInstantInUTC(t *testing.T) {
	// Act.
	got := reapedRef("/repo/.git/worktrees/feat-x1", time.Date(2026, 10, 7, 9, 30, 15, 0, time.FixedZone("x", 3600)))

	// Assert.
	if want := "refs/agent-repl/reaped/feat-x1/20261007T083015Z"; got != want {
		t.Fatalf("reapedRef = %q, want %q", got, want)
	}
}
