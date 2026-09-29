package harness

import (
	"bufio"
	"encoding/json"
	"os"
	"path/filepath"
	"strconv"
	"strings"
	"testing"
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

	t *testing.T
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
const recorderScript = `#!/bin/sh
record="$AGENT_REPL_FAKE_RECORD"
control="$AGENT_REPL_FAKE_CONTROL"
argv=""
for a in "$@"; do
  esc=$(printf '%s' "$a" | sed 's/\\/\\\\/g; s/"/\\"/g')
  if [ -z "$argv" ]; then argv="\"$esc\""; else argv="$argv,\"$esc\""; fi
done
cwd=$(pwd)
printf '{"argv":[%s],"cwd":"%s","env":{"AGENT_REPL_OWNED":"%s"}}\n' "$argv" "$cwd" "$AGENT_REPL_OWNED" >> "$record"
if [ -f "$control.stdout" ]; then cat "$control.stdout"; fi
if [ -f "$control" ]; then exit "$(cat "$control")"; fi
exit 0
`

// NewRecorderExecutable writes a fake executable that records its argv. `name`
// names the script and its record and control files inside `dir`.
func NewRecorderExecutable(t *testing.T, dir, name string) *Recorder {
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
	script := "#!/bin/sh\n" +
		"AGENT_REPL_FAKE_RECORD=" + shellQuote(r.Record) + "\n" +
		"AGENT_REPL_FAKE_CONTROL=" + shellQuote(r.Control) + "\n" +
		"export AGENT_REPL_FAKE_RECORD AGENT_REPL_FAKE_CONTROL\n" +
		strings.TrimPrefix(recorderScript, "#!/bin/sh\n")
	if err := os.WriteFile(r.Path, []byte(script), 0o755); err != nil {
		t.Fatalf("harness: write %s: %v", r.Path, err)
	}
	return r
}

// NewTestAllScript installs a fake bin/test-all.sh inside a repository, the
// executable the merge orchestrator's tests tab runs.
func NewTestAllScript(t *testing.T, repoDir string) *Recorder {
	t.Helper()
	return NewRecorderExecutable(t, filepath.Join(repoDir, "bin"), "test-all.sh")
}

// FakeClaudeLoginMarker is printed by the fake `claude` binary the login pty
// runs, so a test can recognize the terminal it attached to.
const FakeClaudeLoginMarker = "FAKE-CLAUDE-LOGIN-READY"

// FakeClaudeMintedName is the slug the fake vendor answers a naming call
// with. It is what an integration create names its workspace, so a test may
// assert on the branch a nameless create produced.
const FakeClaudeMintedName = "fake-minted-name"

// fakeClaudeScript is the vendor stand-in for BOTH call shapes the daemon
// has. A headless run (`-p`) drains the question off stdin and answers as the
// requested output format: a naming call gets the JSON envelope with a legal
// three-word slug in `result`. Anything else is the login pty: it prints the
// marker and echoes every keystroke back, so WatchLoginTerminal has scrollback
// to replay and input to echo.
const fakeClaudeScript = `#!/bin/sh
for arg in "$@"; do
  if [ "$arg" = "-p" ]; then
    cat > /dev/null
    for fmt in "$@"; do
      if [ "$fmt" = "json" ]; then
        printf '{"type":"result","subtype":"success","is_error":false,"result":"` + FakeClaudeMintedName + `"}'
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
func NewFakeClaude(t *testing.T, dir string) string {
	t.Helper()
	if err := os.MkdirAll(dir, 0o755); err != nil {
		t.Fatalf("harness: mkdir %s: %v", dir, err)
	}
	path := filepath.Join(dir, "claude")
	if err := os.WriteFile(path, []byte(fakeClaudeScript), 0o755); err != nil {
		t.Fatalf("harness: write %s: %v", path, err)
	}
	return path
}

// NewFakeBrowser writes the external browser launcher OpenExternal invokes.
func NewFakeBrowser(t *testing.T, dir string) *Recorder {
	t.Helper()
	return NewRecorderExecutable(t, dir, "browser")
}

// NewFakeNotifier writes the desktop banner program the daemon posts through
// (AGENT_REPL_NOTIFIER_CMD). It records the platform argv and prints nothing,
// which every platform reads as a dismissal; SetStdout scripts a click.
func NewFakeNotifier(t *testing.T, dir string) *Recorder {
	t.Helper()
	return NewRecorderExecutable(t, dir, "notifier")
}

// FakeDeployBuildRefusal is what the fake deploy builder prints before it
// fails: a harness NEVER builds, so every deploy it drives is a build failure
// that deploys nothing.
const FakeDeployBuildRefusal = "fake deploy build: a harness never builds"

// NewFakeLaunchctl writes the launchctl a service restart would drive
// (AGENT_REPL_LAUNCHCTL), so nothing the daemon runs can reach the live
// launchd.
func NewFakeLaunchctl(t *testing.T, dir string) *Recorder {
	t.Helper()
	return NewRecorderExecutable(t, dir, "launchctl")
}

// NewFakeWebappDist writes a minimal webapp dist: an index.html entry point
// and one hashed asset.
func NewFakeWebappDist(t *testing.T, dir string) string {
	t.Helper()
	assets := filepath.Join(dir, "assets")
	if err := os.MkdirAll(assets, 0o755); err != nil {
		t.Fatalf("harness: mkdir %s: %v", assets, err)
	}
	// The entry names a hashed bundle exactly as Vite's does, because the
	// deploy reads the served webapp's build off it (FakeWebappEntry).
	writeFile(t, filepath.Join(dir, "index.html"), "<!doctype html><title>agent-repl</title><script src=\"/assets/index-"+FakeWebappEntry+".js\"></script>")
	writeFile(t, filepath.Join(assets, "index-"+FakeWebappEntry+".js"), "// fake webapp bundle\n")
	writeFile(t, filepath.Join(assets, "app.js"), "// fake webapp asset\n")
	return dir
}

// NewConfigRoot writes an account root: a CLAUDE_CONFIG_DIR whose .claude.json
// names the logged-in account. An empty email leaves the file out, which is
// how a logged-out root is drawn.
func NewConfigRoot(t *testing.T, dir, email string) string {
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
func CopyPrompts(t *testing.T, dst string) string {
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

func writeFile(t *testing.T, path, content string) {
	t.Helper()
	if err := os.MkdirAll(filepath.Dir(path), 0o755); err != nil {
		t.Fatalf("harness: mkdir %s: %v", filepath.Dir(path), err)
	}
	if err := os.WriteFile(path, []byte(content), 0o644); err != nil {
		t.Fatalf("harness: write %s: %v", path, err)
	}
}

func shellQuote(s string) string { return "'" + strings.ReplaceAll(s, "'", `'\''`) + "'" }
