package prompts

import (
	"os"
	"path/filepath"
	"strings"
	"testing"
)

// policyBrief writes one brief into dir, creating dir.
func policyBrief(t *testing.T, dir, name, content string) {
	t.Helper()
	if err := os.MkdirAll(dir, 0o755); err != nil {
		t.Fatalf("mkdir %s: %v", dir, err)
	}
	if err := os.WriteFile(filepath.Join(dir, name+Suffix), []byte(content), 0o644); err != nil {
		t.Fatalf("writing %s: %v", name, err)
	}
}

func TestPolicyDirIsBeneathTheRepositoryRoot(t *testing.T) {
	// Arrange.
	root := filepath.Join("/tmp", "a-repository")

	// Act.
	got := PolicyDir(root)

	// Assert.
	want := filepath.Join(root, ".agent-repl", "prompts")
	if got != want {
		t.Fatalf("PolicyDir = %q, want %q", got, want)
	}
}

func TestSourceForAnswersTheCorpusForTheRepositoryTheCheckoutLivesIn(t *testing.T) {
	// Arrange: the module checkout is a directory INSIDE the repository, which
	// is what a real deployment looks like.
	repo := filepath.Join("/src", "doom")
	checkout := filepath.Join(repo, "modules", "app", "agent-repl")
	corpus := filepath.Join(checkout, "prompts")

	// Act.
	got := SourceFor(repo, checkout, corpus)

	// Assert.
	if got.Kind != SourceCorpus {
		t.Fatalf("Kind = %q, want %q", got.Kind, SourceCorpus)
	}
	if got.Dir != corpus {
		t.Fatalf("Dir = %q, want the corpus %q", got.Dir, corpus)
	}
}

func TestSourceForAnswersTheRepositorysOwnDirectoryForEveryOtherRepository(t *testing.T) {
	// Arrange.
	repo := filepath.Join("/src", "some-other-project")
	checkout := filepath.Join("/src", "doom", "modules", "app", "agent-repl")

	// Act.
	got := SourceFor(repo, checkout, filepath.Join(checkout, "prompts"))

	// Assert.
	if got.Kind != SourceRepository {
		t.Fatalf("Kind = %q, want %q", got.Kind, SourceRepository)
	}
	if got.Dir != PolicyDir(repo) {
		t.Fatalf("Dir = %q, want %q", got.Dir, PolicyDir(repo))
	}
}

func TestSourceForNeverFallsBackToTheCorpusForAnotherRepository(t *testing.T) {
	// Arrange: a repository whose path merely SHARES A PREFIX with the
	// checkout's repository is a different repository, not a parent of it.
	repo := filepath.Join("/src", "doom-fork")
	checkout := filepath.Join("/src", "doom", "modules", "app", "agent-repl")

	// Act.
	got := SourceFor(repo, checkout, filepath.Join(checkout, "prompts"))

	// Assert.
	if got.Kind != SourceRepository {
		t.Fatalf("Kind = %q, want %q: a shared path prefix is not containment", got.Kind, SourceRepository)
	}
}

func TestIsModuleRepositoryAcceptsACheckoutDeployedAtTheRepositoryRoot(t *testing.T) {
	// Arrange.
	root := filepath.Join("/src", "agent-repl")

	// Act.
	got := IsModuleRepository(root, root+string(filepath.Separator))

	// Assert.
	if !got {
		t.Fatalf("IsModuleRepository(%q, %q) = false, want true", root, root)
	}
}

func TestIsModuleRepositoryRefusesAnUnresolvedRoot(t *testing.T) {
	// Arrange: an empty checkout root names no repository at all.

	// Act.
	got := IsModuleRepository("/src/doom", "")

	// Assert.
	if got {
		t.Fatalf("IsModuleRepository with no checkout root = true, want false")
	}
}

func TestOnDiskMissingNamesEveryAbsentBriefByFileName(t *testing.T) {
	// Arrange.
	dir := filepath.Join(t.TempDir(), ".agent-repl", "prompts")
	policyBrief(t, dir, "present", "<!-- used by: policy; placeholders: none -->\nbody\n")

	// Act.
	got := OnDisk{}.Missing(dir, []string{"present", "absent-one", "absent-two"})

	// Assert.
	want := []string{"absent-one.md", "absent-two.md"}
	if strings.Join(got, ",") != strings.Join(want, ",") {
		t.Fatalf("Missing = %v, want %v", got, want)
	}
}

func TestOnDiskMissingAnswersNothingWhenEveryBriefIsPresent(t *testing.T) {
	// Arrange.
	dir := filepath.Join(t.TempDir(), ".agent-repl", "prompts")
	policyBrief(t, dir, "one", "<!-- used by: policy; placeholders: none -->\nfirst\n")
	policyBrief(t, dir, "two", "<!-- used by: policy; placeholders: none -->\nsecond\n")

	// Act.
	got := OnDisk{}.Missing(dir, []string{"one", "two"})

	// Assert.
	if len(got) != 0 {
		t.Fatalf("Missing = %v, want nothing missing", got)
	}
}

func TestOnDiskMissingCountsADirectoryNamedLikeABriefAsAbsent(t *testing.T) {
	// Arrange: only a regular file is a brief.
	dir := filepath.Join(t.TempDir(), ".agent-repl", "prompts")
	if err := os.MkdirAll(filepath.Join(dir, "merge-before"+Suffix), 0o755); err != nil {
		t.Fatalf("mkdir: %v", err)
	}

	// Act.
	got := OnDisk{}.Missing(dir, []string{PolicyMergeBefore})

	// Assert.
	if len(got) != 1 || got[0] != PolicyMergeBefore+Suffix {
		t.Fatalf("Missing = %v, want the directory counted as an absent brief", got)
	}
}

func TestOnDiskTextAnswersTheBriefBody(t *testing.T) {
	// Arrange.
	dir := filepath.Join(t.TempDir(), ".agent-repl", "prompts")
	policyBrief(t, dir, PolicyMergeAfter, "<!-- used by: merge; placeholders: none -->\nrun the checks\n")

	// Act.
	got, err := OnDisk{}.Text(dir, PolicyMergeAfter)

	// Assert.
	if err != nil {
		t.Fatalf("Text: %v", err)
	}
	if got != "run the checks" {
		t.Fatalf("Text = %q, want %q", got, "run the checks")
	}
}

func TestOnDiskTextRefusesABriefThatDeclaresPlaceholders(t *testing.T) {
	// Arrange: a policy brief is submitted verbatim, so a hole in it has
	// nothing to fill it from.
	dir := filepath.Join(t.TempDir(), ".agent-repl", "prompts")
	policyBrief(t, dir, PolicyMergeBefore, "<!-- used by: merge; placeholders: {{who}} -->\nask {{who}}\n")

	// Act.
	_, err := OnDisk{}.Text(dir, PolicyMergeBefore)

	// Assert.
	if err == nil || !strings.Contains(err.Error(), "verbatim") {
		t.Fatalf("Text error = %v, want a refusal naming the declared placeholders", err)
	}
}

func TestOnDiskTextSurfacesAnAbsentBrief(t *testing.T) {
	// Arrange.
	dir := filepath.Join(t.TempDir(), ".agent-repl", "prompts")
	if err := os.MkdirAll(dir, 0o755); err != nil {
		t.Fatalf("mkdir: %v", err)
	}

	// Act.
	_, err := OnDisk{}.Text(dir, PolicyMergeBefore)

	// Assert.
	if err == nil {
		t.Fatalf("Text of an absent brief = nil error, want the absence surfaced")
	}
}
