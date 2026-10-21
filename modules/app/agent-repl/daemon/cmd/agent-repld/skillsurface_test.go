package main

import (
	"os"
	"path/filepath"
	"regexp"
	"sort"
	"strings"
	"testing"
)

// THE SKILLS AND THE AGENT CLI ARE ONE SURFACE (agent-repld-api.md): every
// command a skill or a daemon brief names exists, and every agent command is
// reachable from at least one skill. A command renamed or added without its
// skill fails here rather than leaving an agent a command that is not there,
// or one no agent is told of.

// moduleDir is the agent-repl module, from this package's directory.
const moduleDir = "../../.."

// codeSpan is one markdown inline code span.
var codeSpan = regexp.MustCompile("`([^`]+)`")

// fencedBlock is one fenced markdown code block, whose own backticks are no
// inline span's delimiters. Each of its lines is read as a span of its own.
var fencedBlock = regexp.MustCompile("(?ms)^[ \t]*```[^\n]*\n(.*?)^[ \t]*```[ \t]*$")

// commandMention is a noun and its verb at the start of a code span, or after
// `agent-repld `.
var commandMention = regexp.MustCompile(`(?:^|agent-repld )(workspace|merge-queue|editor|runtime) ([a-z][a-z-]*)`)

// mentionedCommands answers the "noun verb" commands TEXT's code spans and
// fenced code lines name.
func mentionedCommands(text string) []string {
	var out []string
	for _, block := range fencedBlock.FindAllStringSubmatch(text, -1) {
		for _, line := range strings.Split(block[1], "\n") {
			for _, m := range commandMention.FindAllStringSubmatch(strings.TrimSpace(line), -1) {
				out = append(out, m[1]+" "+m[2])
			}
		}
	}
	text = fencedBlock.ReplaceAllString(text, "")
	for _, span := range codeSpan.FindAllStringSubmatch(text, -1) {
		for _, m := range commandMention.FindAllStringSubmatch(span[1], -1) {
			out = append(out, m[1]+" "+m[2])
		}
	}
	return out
}

// readSkills answers each skill's SKILL.md, by skill name.
func readSkills(t *testing.T) map[string]string {
	t.Helper()
	paths, err := filepath.Glob(filepath.Join(moduleDir, "skills", "*", "SKILL.md"))
	if err != nil || len(paths) == 0 {
		t.Fatalf("glob the module's skills: %v (%d found)", err, len(paths))
	}
	skills := map[string]string{}
	for _, path := range paths {
		raw, err := os.ReadFile(path)
		if err != nil {
			t.Fatalf("read %s: %v", path, err)
		}
		skills[filepath.Base(filepath.Dir(path))] = string(raw)
	}
	return skills
}

func TestMentionedCommandsReadsCodeSpans(t *testing.T) {
	cases := []struct {
		name string
		text string
		want []string
	}{
		{name: "a command at a span's start", text: "run `workspace set here -one-shot` now", want: []string{"workspace set"}},
		{name: "a command after agent-repld", text: "`agent-repld merge-queue status`", want: []string{"merge-queue status"}},
		{name: "a noun alone names no command", text: "`agent-repld workspace` prints the usage", want: nil},
		{name: "a noun inside prose is no command", text: "the workspace merge lands it", want: nil},
		{name: "a noun mid-span is no command", text: "`-topology child -owner workspace x`", want: nil},
		{name: "a fenced block's backticks pair no span", text: "```yaml\nkey: x\n```\nthen `workspace kill WS`", want: []string{"workspace kill"}},
		{name: "a command on a fenced line", text: "```sh\nagent-repld runtime health\n```", want: []string{"runtime health"}},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Act
			got := mentionedCommands(tc.text)

			// Assert
			if strings.Join(got, ",") != strings.Join(tc.want, ",") {
				t.Fatalf("mentionedCommands(%q) = %q, want %q", tc.text, got, tc.want)
			}
		})
	}
}

func TestEveryCommandASkillNamesExists(t *testing.T) {
	for name, text := range readSkills(t) {
		for _, command := range mentionedCommands(text) {
			noun, verb, _ := strings.Cut(command, " ")
			if _, ok := cliNouns[noun][verb]; !ok {
				t.Errorf("skill %s names `agent-repld %s`, which is no command", name, command)
			}
		}
	}
}

func TestEveryCommandADaemonBriefNamesExists(t *testing.T) {
	paths, err := filepath.Glob(filepath.Join(moduleDir, "prompts", "*.md"))
	if err != nil || len(paths) == 0 {
		t.Fatalf("glob the daemon's briefs: %v (%d found)", err, len(paths))
	}
	for _, path := range paths {
		raw, err := os.ReadFile(path)
		if err != nil {
			t.Fatalf("read %s: %v", path, err)
		}
		for _, command := range mentionedCommands(string(raw)) {
			noun, verb, _ := strings.Cut(command, " ")
			if _, ok := cliNouns[noun][verb]; !ok {
				t.Errorf("brief %s names `agent-repld %s`, which is no command", filepath.Base(path), command)
			}
		}
	}
}

func TestEveryAgentCommandIsReachableFromASkill(t *testing.T) {
	// Arrange
	named := map[string]bool{}
	for _, text := range readSkills(t) {
		for _, command := range mentionedCommands(text) {
			named[command] = true
		}
	}

	// Act
	var unreached []string
	for noun, commands := range cliNouns {
		for verb := range commands {
			if !named[noun+" "+verb] {
				unreached = append(unreached, noun+" "+verb)
			}
		}
	}

	// Assert
	sort.Strings(unreached)
	if len(unreached) > 0 {
		t.Fatalf("no skill names these agent commands: %v", unreached)
	}
}

func TestEverySkillCallingTheAgentCLICarriesTheFrontDoor(t *testing.T) {
	door, err := filepath.EvalSymlinks(filepath.Join(moduleDir, "bin", "agent-repld"))
	if err != nil {
		t.Fatalf("resolve bin/agent-repld: %v", err)
	}
	for name, text := range readSkills(t) {
		if !strings.Contains(text, "~/.claude/skills/"+name+"/agent-repld") {
			continue
		}
		got, err := filepath.EvalSymlinks(filepath.Join(moduleDir, "skills", name, "agent-repld"))
		if err != nil || got != door {
			t.Errorf("skill %s calls its agent-repld but skills/%s/agent-repld resolves to %q (%v), want %s", name, name, got, err, door)
		}
	}
}
