package harness

import (
	"bufio"
	"encoding/json"
	"os"
	"path/filepath"
	"slices"
	"strconv"
	"strings"
	"testing"

	"agent-repld/internal/shellword"
)

// Recorder is a fake executable that appends its argv to a record file, so a
// test can assert what the daemon invoked without watching processes.
type Recorder struct {
	// Path is the executable the daemon is pointed at.
	Path string
	// Record is the file each invocation appends one JSON line to.
	Record string
	// Control is a file the fake reads its exit status from; absent means 0.
	Control string

	t testing.TB
}

// Invocation is one recorded run of a fake executable.
type Invocation struct {
	Argv []string          `json:"argv"`
	Cwd  string            `json:"cwd"`
	Env  map[string]string `json:"env"`
}

// Invocations reads every recorded run, oldest first. A fake that was never
// invoked reads as no invocations, never as an error.
func (r *Recorder) Invocations() []Invocation {
	r.t.Helper()
	f, err := os.Open(r.Record)
	if os.IsNotExist(err) {
		return nil
	}
	if err != nil {
		r.t.Fatalf("harness: read %s: %v", r.Record, err)
	}
	defer f.Close()

	var out []Invocation
	scanner := bufio.NewScanner(f)
	scanner.Buffer(make([]byte, 0, 64*1024), 4*1024*1024)
	for scanner.Scan() {
		line := strings.TrimSpace(scanner.Text())
		if line == "" {
			continue
		}
		var inv Invocation
		if err := json.Unmarshal([]byte(line), &inv); err != nil {
			r.t.Fatalf("harness: %s holds a malformed record %q: %v", r.Record, line, err)
		}
		out = append(out, inv)
	}
	if err := scanner.Err(); err != nil {
		r.t.Fatalf("harness: scan %s: %v", r.Record, err)
	}
	return out
}

// SetExitCode makes every later invocation exit with the status, so a test can
// script a passing or failing run.
func (r *Recorder) SetExitCode(code int) {
	r.t.Helper()
	if err := os.WriteFile(r.Control, []byte(strconv.Itoa(code)+"\n"), 0o644); err != nil {
		r.t.Fatalf("harness: write %s: %v", r.Control, err)
	}
}

// SetExitCodeFor makes every later invocation whose FIRST argument is verb
// exit with the status, ahead of SetExitCode's: a test scripts the one verb
// that must fail (launchctl's `kickstart`) while a read-only one the daemon
// also runs (`print`, at every boot's service check) still succeeds.
func (r *Recorder) SetExitCodeFor(verb string, code int) {
	r.t.Helper()
	if verb == "" || strings.ContainsAny(verb, "/ ") {
		r.t.Fatalf("harness: SetExitCodeFor needs a plain verb, got %q", verb)
	}
	if err := os.WriteFile(r.Control+".verb."+verb, []byte(strconv.Itoa(code)+"\n"), 0o644); err != nil {
		r.t.Fatalf("harness: write %s: %v", r.Control+".verb."+verb, err)
	}
}

// InvocationsExcept reads every recorded run whose first argument is none of
// verbs, oldest first: what a test means by "the daemon changed nothing" is
// the runs left once the read-only verbs are set aside.
func (r *Recorder) InvocationsExcept(verbs ...string) []Invocation {
	r.t.Helper()
	var out []Invocation
	for _, inv := range r.Invocations() {
		if len(inv.Argv) > 0 && slices.Contains(verbs, inv.Argv[0]) {
			continue
		}
		out = append(out, inv)
	}
	return out
}

// SetStdout makes every later invocation print the text before exiting, so a
// test can script test-suite output the merge tab paints.
func (r *Recorder) SetStdout(text string) {
	r.t.Helper()
	if err := os.WriteFile(r.Control+".stdout", []byte(text), 0o644); err != nil {
		r.t.Fatalf("harness: write %s: %v", r.Control+".stdout", err)
	}
}

// recorderScript is the shell body every fake executable shares: append the
// invocation as one JSON line, print the scripted stdout, exit with the
// scripted status.
//
// IT SPAWNS NOTHING ON ITS COMMON PATH. Every fake host tool -- launchctl,
// the notifier, pmset, the chime -- is this script, and the daemon suite runs
// it ~16 times per test. Escaping each argument through `$(printf | sed)`
// and reading the cwd through `$(pwd)` cost a subshell and a sed per
// argument: ~8 processes for one `launchctl print`, the largest single share
// of the ~60,000 processes the daemon suite spawned. Bash's own substitution
// escapes in place, $PWD is the cwd, and `read` takes a scripted status; only
// a scripted stdout still runs a cat.
const recorderScript = `#!/bin/bash
record="$AGENT_REPL_FAKE_RECORD"
control="$AGENT_REPL_FAKE_CONTROL"
argv=""
for a in "$@"; do
  esc=${a//\\/\\\\}
  esc=${esc//\"/\\\"}
  if [ -z "$argv" ]; then argv="\"$esc\""; else argv="$argv,\"$esc\""; fi
done
printf '{"argv":[%s],"cwd":"%s","env":{"AGENT_REPL_OWNED":"%s"}}\n' "$argv" "$PWD" "$AGENT_REPL_OWNED" >> "$record"
if [ -f "$control.stdout" ]; then cat "$control.stdout"; fi
if [ -n "$1" ] && [ -f "$control.verb.$1" ]; then read -r code < "$control.verb.$1"; exit "$code"; fi
if [ -f "$control" ]; then read -r code < "$control"; exit "$code"; fi
exit 0
`

// NewRecorderExecutable writes a fake executable that records its argv. `name`
// names the script and its record and control files inside `dir`.
func NewRecorderExecutable(t testing.TB, dir, name string) *Recorder {
	t.Helper()
	if err := os.MkdirAll(dir, 0o755); err != nil {
		t.Fatalf("harness: mkdir %s: %v", dir, err)
	}
	r := &Recorder{
		Path:    filepath.Join(dir, name),
		Record:  filepath.Join(dir, name+".record"),
		Control: filepath.Join(dir, name+".control"),
		t:       t,
	}
	script := "#!/bin/bash\n" +
		"AGENT_REPL_FAKE_RECORD=" + shellword.Quote(r.Record) + "\n" +
		"AGENT_REPL_FAKE_CONTROL=" + shellword.Quote(r.Control) + "\n" +
		"export AGENT_REPL_FAKE_RECORD AGENT_REPL_FAKE_CONTROL\n" +
		strings.TrimPrefix(recorderScript, "#!/bin/bash\n")
	writeExecutable(t, r.Path, script)
	return r
}

// NewTestAllScript installs a fake bin/test-all.sh inside a repository, the
// executable the merge orchestrator's tests tab runs.
func NewTestAllScript(t testing.TB, repoDir string) *Recorder {
	t.Helper()
	return NewRecorderExecutable(t, filepath.Join(repoDir, "bin"), "test-all.sh")
}

// TestAllScriptEnv is the daemon environment entry that points the merge
// orchestrator's tests tab at the test-all script at PATH (a NewTestAllScript
// recorder's Path, or any stand-in). It is the one spelling of the variable,
// so no integration test can point the daemon at its script under a name the
// daemon does not read.
func TestAllScriptEnv(path string) string {
	return "AGENT_REPL_TEST_ALL_SCRIPT=" + path
}

// FakeClaudeLoginMarker is printed by the fake `claude` binary the login pty
// runs, so a test can recognize the terminal it attached to.
const FakeClaudeLoginMarker = "FAKE-CLAUDE-LOGIN-READY"

// FakeClaudeMintedName is the slug the fake vendor answers a naming call
// with. It is what an integration create names its workspace, so a test may
// assert on the branch a nameless create produced.
const FakeClaudeMintedName = "fake-minted-name"

// FakeClaudeMintedTitle is the title the fake vendor answers a chat thread's
// title call with (the chat-title-from-question brief, which it recognizes by
// its first line): what a created thread is renamed to.
const FakeClaudeMintedTitle = "Fake Minted Title"

// FakeClaudeMintedAbbreviation is the abbreviation the fake vendor answers a
// chat question's abbreviation call with (the chat-question-abbreviation
// brief, which it recognizes by its first line).
const FakeClaudeMintedAbbreviation = "Fake minted abbreviation"

// fakeClaudeScript is the vendor stand-in for BOTH call shapes the daemon
// has. A headless run (`-p`) drains the question off stdin and answers as the
// requested output format: a naming call gets the JSON envelope with a legal
// slug of at most four words in `result`. Anything else is the login pty: it prints the
// marker and echoes every keystroke back, so WatchLoginTerminal has scrollback
// to replay and input to echo.
const fakeClaudeScript = `#!/bin/sh
for arg in "$@"; do
  if [ "$arg" = "-p" ]; then
    question=$(cat)
    answer="` + FakeClaudeMintedName + `"
    case "$question" in
      *"Title the chat thread"*) answer="` + FakeClaudeMintedTitle + `" ;;
      *"Abbreviate the chat question"*) answer="` + FakeClaudeMintedAbbreviation + `" ;;
    esac
    for fmt in "$@"; do
      if [ "$fmt" = "json" ]; then
        printf '{"type":"result","subtype":"success","is_error":false,"result":"%s"}' "$answer"
        exit 0
      fi
    done
    printf 'ROUTE_HOLD'
    exit 0
  fi
done
printf '%s\n' "` + FakeClaudeLoginMarker + `"
while IFS= read -r line; do
  printf 'echo:%s\n' "$line"
done
`

// NewFakeClaude writes a fake `claude` onto a directory the daemon's PATH
// carries, so OpenLogin's pty runs it instead of the real CLI.
func NewFakeClaude(t testing.TB, dir string) string {
	t.Helper()
	if err := os.MkdirAll(dir, 0o755); err != nil {
		t.Fatalf("harness: mkdir %s: %v", dir, err)
	}
	path := filepath.Join(dir, "claude")
	writeExecutable(t, path, fakeClaudeScript)
	return path
}

// NewFakeBrowser writes the external browser launcher OpenExternal invokes.
func NewFakeBrowser(t testing.TB, dir string) *Recorder {
	t.Helper()
	return NewRecorderExecutable(t, dir, "browser")
}

// FakeNotifierVersion is what the fake banner program prints for every run:
// the version answer macOS's banner program must give at boot (terminal-notifier
// 3 or later). A Linux banner reads it as no click, a dismissal.
const FakeNotifierVersion = "terminal-notifier 3.1.0.\n"

// NotifierVersionQuery is the argument the daemon asks the macOS banner
// program its version with, at boot: a run that is not a banner.
const NotifierVersionQuery = "-version"

// NewFakeNotifier writes the desktop banner program the daemon posts through
// (AGENT_REPL_NOTIFIER_CMD). It records the platform argv and prints
// FakeNotifierVersion, which the macOS boot check accepts and every
// read-back platform reads as a dismissal; SetStdout scripts a click.
func NewFakeNotifier(t testing.TB, dir string) *Recorder {
	t.Helper()
	r := NewRecorderExecutable(t, dir, "notifier")
	r.SetStdout(FakeNotifierVersion)
	return r
}

// NewFakeChime writes the banner sound player the daemon rings through
// (AGENT_REPL_CHIME_CMD). It records the platform argv and plays nothing, so
// no test ever sounds on the host.
func NewFakeChime(t testing.TB, dir string) *Recorder {
	t.Helper()
	return NewRecorderExecutable(t, dir, "chime")
}

// FakeDeployBuildRefusal is what the fake deploy builder prints before it
// fails: a harness NEVER builds, so every deploy it drives is a build failure
// that deploys nothing.
const FakeDeployBuildRefusal = "fake deploy build: a harness never builds"

// NewFakeLaunchctl writes the launchctl a service restart would drive
// (AGENT_REPL_LAUNCHCTL), so nothing the daemon runs can reach the live
// launchd.
func NewFakeLaunchctl(t testing.TB, dir string) *Recorder {
	t.Helper()
	return NewRecorderExecutable(t, dir, "launchctl")
}

// NewFakeWebappDist writes a minimal webapp dist: an index.html entry point
// and one hashed asset.
func NewFakeWebappDist(t testing.TB, dir string) string {
	t.Helper()
	assets := filepath.Join(dir, "assets")
	if err := os.MkdirAll(assets, 0o755); err != nil {
		t.Fatalf("harness: mkdir %s: %v", assets, err)
	}
	// The entry names a hashed bundle exactly as Vite's does, because the
	// deploy reads the served webapp's build off it (FakeWebappEntry).
	writeFile(t, filepath.Join(dir, "index.html"), "<!doctype html><html><title>agent-repl</title><script src=\"/assets/index-"+FakeWebappEntry+".js\"></script></html>")
	writeFile(t, filepath.Join(assets, "index-"+FakeWebappEntry+".js"), "// fake webapp bundle\n")
	writeFile(t, filepath.Join(assets, "app.js"), "// fake webapp asset\n")
	return dir
}

// NewConfigRoot writes an account root: a CLAUDE_CONFIG_DIR whose .claude.json
// names the logged-in account. An empty email leaves the file out, which is
// how a logged-out root is drawn.
func NewConfigRoot(t testing.TB, dir, email string) string {
	t.Helper()
	if err := os.MkdirAll(dir, 0o755); err != nil {
		t.Fatalf("harness: mkdir %s: %v", dir, err)
	}
	if email == "" {
		return dir
	}
	body, err := json.Marshal(map[string]any{
		"oauthAccount": map[string]any{"emailAddress": email},
	})
	if err != nil {
		t.Fatalf("harness: encode .claude.json: %v", err)
	}
	writeFile(t, filepath.Join(dir, ".claude.json"), string(body))
	return dir
}

// CopyPrompts copies the shipped prompts/ directory into the test's own tree,
// so a test can edit a brief or delete one without touching the repository.
func CopyPrompts(t testing.TB, dst string) string {
	t.Helper()
	src := filepath.Join(RepoRoot(t), "prompts")
	entries, err := os.ReadDir(src)
	if err != nil {
		t.Fatalf("harness: read %s: %v", src, err)
	}
	if err := os.MkdirAll(dst, 0o755); err != nil {
		t.Fatalf("harness: mkdir %s: %v", dst, err)
	}
	for _, e := range entries {
		if e.IsDir() {
			continue
		}
		body, err := os.ReadFile(filepath.Join(src, e.Name()))
		if err != nil {
			t.Fatalf("harness: read %s: %v", e.Name(), err)
		}
		writeFile(t, filepath.Join(dst, e.Name()), string(body))
	}
	return dst
}

func writeFile(t testing.TB, path, content string) {
	t.Helper()
	if err := os.MkdirAll(filepath.Dir(path), 0o755); err != nil {
		t.Fatalf("harness: mkdir %s: %v", filepath.Dir(path), err)
	}
	if err := os.WriteFile(path, []byte(content), 0o644); err != nil {
		t.Fatalf("harness: write %s: %v", path, err)
	}
}

// writeExecutable writes an executable file at PATH, its directory first.
func writeExecutable(t testing.TB, path, content string) {
	t.Helper()
	writeFile(t, path, content)
	if err := os.Chmod(path, 0o755); err != nil {
		t.Fatalf("harness: chmod %s: %v", path, err)
	}
}

// FakeHostNetwork is the Wi-Fi network the fake host tools report joined.
const FakeHostNetwork = "Harness Net"

// FakeHotspot is the hotspot the daemon is told to join
// (AGENT_REPL_PERSISTENT_WIFI_HOTSPOT); joining it changes nothing the fakes
// report.
const FakeHotspot = "Harness Hotspot"

// fakeHostTools are the persistent-wifi host tools, by name. They are
// STATEFUL where the daemon reads back what it wrote: `pmset -a disablesleep`
// writes the value `pmset -g` reports, through the fake `sudo`, which runs
// the command it is handed. No test ever touches the real machine's power or
// network settings.
var fakeHostTools = map[string]string{
	"sudo": `#!/bin/sh
[ "$1" = "-n" ] && shift
exec "$@"
`,
	"pmset": `#!/bin/sh
state="$(dirname "$0")/pmset.state"
case "$1" in
  -g) printf 'System-wide power settings:\n SleepDisabled\t\t%s\nCurrently in use:\n standby 1\n' "$(cat "$state" 2>/dev/null || echo 0)" ;;
  -a) [ "$2" = disablesleep ] && printf '%s\n' "$3" > "$state" ;;
esac
exit 0
`,
	"networksetup": `#!/bin/sh
[ "$1" = -listallhardwareports ] && printf 'Hardware Port: Wi-Fi\nDevice: en9\n'
exit 0
`,
	"ipconfig": `#!/bin/sh
printf '<dictionary> {\n  InterfaceType : WiFi\n  LinkStatusActive : TRUE\n  SSID : ` + FakeHostNetwork + `\n}\n'
`,
	"wifi-util":      "#!/bin/sh\nexit 0\n",
	"mac-brightness": "#!/bin/sh\nexit 0\n",
}

// NewFakeHostTools writes the persistent-wifi host tools into dir
// (AGENT_REPL_PERSISTENT_WIFI_TOOLS_DIR) and answers it. The machine they
// describe starts joined to FakeHostNetwork with the mode off.
func NewFakeHostTools(t testing.TB, dir string) string {
	t.Helper()
	if err := os.MkdirAll(dir, 0o755); err != nil {
		t.Fatalf("harness: mkdir %s: %v", dir, err)
	}
	for name, script := range fakeHostTools {
		writeExecutable(t, filepath.Join(dir, name), script)
	}
	return dir
}
