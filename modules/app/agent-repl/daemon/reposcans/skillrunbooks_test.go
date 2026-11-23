package reposcans

import (
	"os"
	"path/filepath"
	"regexp"
	"sort"
	"strings"
	"testing"
)

// runbookCeilingBytes is the size a runbook may not pass: 1.5k tokens at about
// four bytes a token, 1.5 times the one-thousand-token target the runbooks are
// cut to (the largest was 3,752 bytes when the skill was split, 2026-11-23).
const runbookCeilingBytes = 6000

var (
	// routerEntry matches a router table row's runbook name: the last
	// backticked cell of a `| when | `name` |` row.
	routerEntry = regexp.MustCompile("(?m)^\\|.*\\| `([a-z0-9-]+)` \\|$")
	// runbookMention matches a runbook naming another one by file.
	runbookMention = regexp.MustCompile("`([a-z0-9-]+)\\.md`")
)

// skillRunbookFindings returns every way the skill directory's router and
// runbooks disagree: a runbook over the ceiling, a runbook the router never
// names, a router row naming no runbook, and a runbook naming another that
// does not exist.
func skillRunbookFindings(t *testing.T, skillDir string) []string {
	t.Helper()
	router, err := os.ReadFile(filepath.Join(skillDir, "SKILL.md"))
	if err != nil {
		t.Fatalf("read the router %s/SKILL.md: %v", skillDir, err)
	}
	entries, err := os.ReadDir(filepath.Join(skillDir, "runbooks"))
	if err != nil {
		t.Fatalf("read %s/runbooks: %v", skillDir, err)
	}
	have := map[string]bool{}
	var findings []string
	for _, e := range entries {
		name := strings.TrimSuffix(e.Name(), ".md")
		have[name] = true
		body, err := os.ReadFile(filepath.Join(skillDir, "runbooks", e.Name()))
		if err != nil {
			t.Fatalf("read runbook %s: %v", e.Name(), err)
		}
		if len(body) > runbookCeilingBytes {
			findings = append(findings, e.Name()+" is over the size ceiling")
		}
	}
	routed := map[string]bool{}
	for _, m := range routerEntry.FindAllStringSubmatch(string(router), -1) {
		routed[m[1]] = true
		if !have[m[1]] {
			findings = append(findings, "the router names "+m[1]+", which is no runbook")
		}
	}
	for name := range have {
		if !routed[name] {
			findings = append(findings, name+".md is named by no router row")
		}
	}
	for name := range have {
		body, err := os.ReadFile(filepath.Join(skillDir, "runbooks", name+".md"))
		if err != nil {
			t.Fatalf("read runbook %s.md: %v", name, err)
		}
		for _, m := range runbookMention.FindAllStringSubmatch(string(body), -1) {
			if !have[m[1]] {
				findings = append(findings, name+".md names "+m[1]+".md, which does not exist")
			}
		}
	}
	sort.Strings(findings)
	return findings
}

func writeSkill(t *testing.T, router string, runbooks map[string]string) string {
	t.Helper()
	dir := t.TempDir()
	if err := os.MkdirAll(filepath.Join(dir, "runbooks"), 0o755); err != nil {
		t.Fatalf("make runbooks dir: %v", err)
	}
	if err := os.WriteFile(filepath.Join(dir, "SKILL.md"), []byte(router), 0o644); err != nil {
		t.Fatalf("write router: %v", err)
	}
	for name, body := range runbooks {
		if err := os.WriteFile(filepath.Join(dir, "runbooks", name+".md"), []byte(body), 0o644); err != nil {
			t.Fatalf("write runbook %s: %v", name, err)
		}
	}
	return dir
}

func TestSkillRunbookFindingsOnSyntheticSkills(t *testing.T) {
	cases := []struct {
		name     string
		router   string
		runbooks map[string]string
		want     []string
	}{
		{
			name:     "a router row per runbook is clean",
			router:   "| when | read |\n|---|---|\n| doing a | `a` |\n",
			runbooks: map[string]string{"a": "# A\n"},
			want:     nil,
		},
		{
			name:     "a runbook over the ceiling is named",
			router:   "| doing a | `a` |\n",
			runbooks: map[string]string{"a": strings.Repeat("x", runbookCeilingBytes+1)},
			want:     []string{"a.md is over the size ceiling"},
		},
		{
			name:     "a runbook at the ceiling is clean",
			router:   "| doing a | `a` |\n",
			runbooks: map[string]string{"a": strings.Repeat("x", runbookCeilingBytes)},
			want:     nil,
		},
		{
			name:     "a runbook no row names is an orphan",
			router:   "| doing a | `a` |\n",
			runbooks: map[string]string{"a": "# A\n", "b": "# B\n"},
			want:     []string{"b.md is named by no router row"},
		},
		{
			name:     "a row naming no runbook is dangling",
			router:   "| doing a | `a` |\n| doing b | `b` |\n",
			runbooks: map[string]string{"a": "# A\n"},
			want:     []string{"the router names b, which is no runbook"},
		},
		{
			name:     "a runbook naming a missing runbook is dangling",
			router:   "| doing a | `a` |\n",
			runbooks: map[string]string{"a": "see `gone.md`\n"},
			want:     []string{"a.md names gone.md, which does not exist"},
		},
		{
			name:     "a runbook naming an existing runbook is clean",
			router:   "| doing a | `a` |\n| doing b | `b` |\n",
			runbooks: map[string]string{"a": "see `b.md`\n", "b": "# B\n"},
			want:     nil,
		},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			dir := writeSkill(t, tc.router, tc.runbooks)

			// Act.
			got := skillRunbookFindings(t, dir)

			// Assert.
			if strings.Join(got, "\n") != strings.Join(tc.want, "\n") {
				t.Fatalf("skillRunbookFindings = %q, want %q", got, tc.want)
			}
		})
	}
}

// TestWorkspacesSkillRunbooksStayShortAndRouted keeps the workspaces skill's
// split honest: no runbook grows past the ceiling, and the router and the
// runbooks name each other exactly. Every finding is listed, not just the first.
func TestWorkspacesSkillRunbooksStayShortAndRouted(t *testing.T) {
	// Arrange.
	dir := filepath.Join(moduleRootDir(t), "skills", "agent-repl-workspaces")

	// Act.
	findings := skillRunbookFindings(t, dir)

	// Assert.
	if len(findings) > 0 {
		t.Fatalf("the workspaces skill's runbooks disagree with its router:\n  %s", strings.Join(findings, "\n  "))
	}
}
