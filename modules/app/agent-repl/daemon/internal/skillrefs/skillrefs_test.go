package skillrefs

import "testing"

const home = "/h"

// files knows two skills: a project skill beneath home, and one outside it.
func files(name string) (string, bool) {
	switch name {
	case "ceac-resource-usage":
		return "/h/w/.claude/skills/ceac-resource-usage/SKILL.md", true
	case "gns-cowork:gns-bootstrap":
		return "/opt/plugins/gns-bootstrap/SKILL.md", true
	case "odd_name":
		return "/h/s/odd_name/SKILL.md", true
	case "spaced":
		return "/h/My Skills/spaced/SKILL.md", true
	}
	return "", false
}

const link = "[/ceac-resource-usage](~/w/.claude/skills/ceac-resource-usage/SKILL.md)"

func TestLinkify(t *testing.T) {
	tests := []struct {
		name     string
		markdown string
		arriving bool
		want     string
	}{
		{"known skill linked", "run `ceac-resource-usage` now", false, "run " + link + " now"},
		{"leading slash form linked", "run `/ceac-resource-usage` now", false, "run " + link + " now"},
		{"padded span linked", "`` ceac-resource-usage ``", false, link},
		{"unknown name stays code", "run `compact` now", false, "run `compact` now"},
		{"name inside a longer span stays code", "`ceac-resource-usage --help`", false, "`ceac-resource-usage --help`"},
		{"plain name stays plain", "run ceac-resource-usage now", false, "run ceac-resource-usage now"},
		{"fenced block left alone", "```\n`ceac-resource-usage`\n```\n", false, "```\n`ceac-resource-usage`\n```\n"},
		{"span inside a link left alone", "[`ceac-resource-usage`](https://x)", false, "[`ceac-resource-usage`](https://x)"},
		{"file outside home kept absolute", "`gns-cowork:gns-bootstrap`", false,
			"[/gns-cowork:gns-bootstrap](/opt/plugins/gns-bootstrap/SKILL.md)"},
		{"markup characters in the name escaped", "`odd_name`", false, `[/odd\_name](~/s/odd_name/SKILL.md)`},
		{"destination with a space bracketed", "`spaced`", false, "[/spaced](<~/My Skills/spaced/SKILL.md>)"},
		{"arriving span at the end left alone", "see `ceac-resource-usage`", true, "see `ceac-resource-usage`"},
		{"arriving span before the end linked", "see `ceac-resource-usage` and", true, "see " + link + " and"},
		{"settled span at the end linked", "see `ceac-resource-usage`", false, "see " + link},
		{"every line read", "a `ceac-resource-usage`\nb `ceac-resource-usage`", true, "a " + link + "\nb `ceac-resource-usage`"},
		{"text with no backtick untouched", "no code here", false, "no code here"},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Act
			got := Linkify(tc.markdown, files, home, tc.arriving)

			// Assert
			if got != tc.want {
				t.Fatalf("Linkify(%q) = %q, want %q", tc.markdown, got, tc.want)
			}
		})
	}
}

func TestLinkifyNeverAsksForANameNoSkillCouldHave(t *testing.T) {
	// Arrange
	var asked []string
	spy := func(name string) (string, bool) { asked = append(asked, name); return "", false }

	// Act
	Linkify("`a b` `` `x` `` `/` `ok`", spy, home, false)

	// Assert
	if len(asked) != 1 || asked[0] != "ok" {
		t.Fatalf("asked %q, want only [ok]", asked)
	}
}

func TestHref(t *testing.T) {
	tests := []struct {
		name, file, home, want string
	}{
		{"beneath home", "/h/a/SKILL.md", "/h", "~/a/SKILL.md"},
		{"outside home", "/opt/a/SKILL.md", "/h", "/opt/a/SKILL.md"},
		{"sibling sharing a prefix", "/home2/a", "/home", "/home2/a"},
		{"home itself", "/h", "/h", "/h"},
		{"no home known", "/h/a", "", "/h/a"},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			if got := Href(tc.file, tc.home); got != tc.want {
				t.Fatalf("Href(%q, %q) = %q, want %q", tc.file, tc.home, got, tc.want)
			}
		})
	}
}
