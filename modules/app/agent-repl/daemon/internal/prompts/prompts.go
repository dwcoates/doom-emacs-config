// Package prompts loads the AUTOMATIC prompt texts the daemon sends to
// agents — the briefs no human types, composed by the daemon itself.
//
// The texts live as plain files under modules/app/agent-repl/prompts/ so a
// user can customize what the system says without editing Go source and
// rebuilding. That is the entire point of the package, and it drives every
// design decision below:
//
//   - The file is read AT USE TIME, never cached across uses. An edit takes
//     effect on the very next prompt the daemon composes, with no rebuild and
//     no daemon restart.
//   - A missing, unreadable, or empty file is a LOUD error returned to the
//     caller, which fails the operation that wanted the prompt. There is no
//     baked-in fallback copy: silently sending a different brief than the one
//     on disk is exactly the failure this package exists to prevent.
//   - A placeholder left unsubstituted is a LOUD error naming both what the
//     file still wants and what the call site supplied, because a customized
//     file with a typo'd placeholder would otherwise ship "{{taget_dir}}" to
//     an agent as if it were prose.
package prompts

import (
	"fmt"
	"os"
	"path/filepath"
	"regexp"
	"runtime"
	"sort"
	"strings"
)

// RelDir is the prompts directory's path inside an agent-repl checkout.
const RelDir = "modules/app/agent-repl/prompts"

// DirEnv names the environment variable that overrides the resolved prompts
// directory. It exists for two callers that genuinely have no checkout to walk
// up to: tests, and a daemon binary run from somewhere other than a deployed
// checkout. It is also the escape hatch for a user who wants their prompt
// customizations to live outside the repository.
const DirEnv = "AGENT_REPL_PROMPTS_DIR"

// markerRelPath identifies a directory as an agent-repl checkout. It is the
// same marker internal/reload uses (the deploy script IS the checkout), spelled
// out here rather than imported because internal/reload depends on
// internal/workspace/merge, which is one of this package's own callers.
const markerRelPath = "modules/app/agent-repl/bin/deploy-all.sh"

// placeholderRe matches one {{name}} substitution point. The name charset is
// deliberately narrow so that ordinary prose containing braces (a code sample,
// a shell expansion) is not mistaken for a placeholder.
var placeholderRe = regexp.MustCompile(`\{\{([a-z0-9_]+)\}\}`)

// headerRe matches the leading `<!-- used by: ...; placeholders: ... -->` line
// every prompt file carries. The header documents the file for whoever edits
// it and is stripped before the text reaches an agent.
var headerRe = regexp.MustCompile(`(?s)\A<!--.*?-->\n`)

// Dir resolves the prompts directory.
//
// Resolution order, and why:
//
//  1. DirEnv, when set. An explicit answer always wins over an inferred one.
//  2. The checkout containing this daemon's own executable. A deployed binary
//     lives at <checkout>/modules/app/agent-repl/daemon/bin/claude-repld, so
//     walking up from the binary to the directory holding the deploy script IS
//     the checkout whose prompts this daemon should be sending.
//
// Failing to resolve is an error, never a guess: a guessed directory would
// either 404 on every prompt or, worse, read some unrelated checkout's
// customizations.
func Dir() (string, error) {
	if dir := strings.TrimSpace(os.Getenv(DirEnv)); dir != "" {
		return dir, nil
	}
	exe, err := os.Executable()
	if err != nil {
		return "", fmt.Errorf("prompts: locate own executable to resolve the prompts directory: %w", err)
	}
	resolved, err := filepath.EvalSymlinks(exe)
	if err != nil {
		return "", fmt.Errorf("prompts: canonicalize executable %s: %w", exe, err)
	}
	root, ok := checkoutContaining(filepath.Dir(resolved))
	if !ok {
		return "", fmt.Errorf("prompts: %s is not inside an agent-repl checkout (no %s above it), so the prompts directory cannot be resolved; set %s to name it explicitly", resolved, markerRelPath, DirEnv)
	}
	return filepath.Join(root, filepath.FromSlash(RelDir)), nil
}

// SourceDir resolves the prompts directory of the checkout containing THIS
// package's own source file.
//
// It exists for TESTS, and only for tests. A `go test` binary lives in the
// build cache with no checkout above it, so Dir's executable walk-up cannot
// answer for one — but the source tree is right there during a test run, and
// runtime.Caller names it. Production code must never call this: a deployed
// daemon's source path is a build-time artifact that may not exist on the
// machine at all, which is exactly why Dir walks from the executable instead.
//
// Tests use it as `t.Setenv(prompts.DirEnv, mustSourceDir(t))`, so the very
// same DirEnv override a user would set is what the tests exercise.
func SourceDir() (string, error) {
	_, file, _, ok := runtime.Caller(0)
	if !ok {
		return "", fmt.Errorf("prompts: runtime.Caller could not name this package's source file")
	}
	root, found := checkoutContaining(filepath.Dir(file))
	if !found {
		return "", fmt.Errorf("prompts: package source %s is not inside an agent-repl checkout (no %s above it)", file, markerRelPath)
	}
	return filepath.Join(root, filepath.FromSlash(RelDir)), nil
}

// checkoutContaining walks up from dir looking for the deploy script.
func checkoutContaining(dir string) (string, bool) {
	for {
		if info, err := os.Stat(filepath.Join(dir, filepath.FromSlash(markerRelPath))); err == nil && !info.IsDir() {
			return dir, true
		}
		parent := filepath.Dir(dir)
		if parent == dir {
			return "", false
		}
		dir = parent
	}
}

// Render reads the named prompt file and substitutes vars into it.
//
// name is the file's basename inside the prompts directory, e.g.
// "merge-conflict-resolve.md". vars maps placeholder name (without braces) to
// the text it stands for.
//
// Every failure mode is an error the caller must handle. None of them degrade
// into a partially-substituted or default prompt.
func Render(name string, vars map[string]string) (string, error) {
	dir, err := Dir()
	if err != nil {
		return "", err
	}
	path := filepath.Join(dir, name)
	raw, err := os.ReadFile(path)
	if err != nil {
		return "", fmt.Errorf("prompts: read prompt %s: %w", path, err)
	}
	text, err := renderText(path, string(raw), vars)
	if err != nil {
		return "", err
	}
	return text, nil
}

// renderText is Render's pure half: header stripping, substitution, and the
// two loud consistency checks. It is separate so the checks are testable
// without a filesystem, and it takes the source path only to name it in errors.
func renderText(path, raw string, vars map[string]string) (string, error) {
	body := headerRe.ReplaceAllString(raw, "")
	// A prompt file ends with a newline because every text editor puts one
	// there; the prompts themselves do not. Exactly one trailing newline is the
	// line terminator and is dropped. A file wanting a trailing newline in the
	// prompt carries two.
	body = strings.TrimSuffix(body, "\n")
	if strings.TrimSpace(body) == "" {
		return "", fmt.Errorf("prompts: %s is empty once its header is stripped; a blank prompt would send an empty turn", path)
	}
	// Both checks scan the TEMPLATE, before any substitution. Scanning the
	// RESULT would be wrong: a queued user prompt or a failing suite's output
	// can itself contain something shaped like "{{foo}}", and diagnosing that
	// as an unsubstituted placeholder would fail a perfectly good prompt over
	// the user's own text.
	wanted := make(map[string]bool)
	for _, match := range placeholderRe.FindAllStringSubmatch(body, -1) {
		wanted[match[1]] = true
	}
	var unknown []string
	for key := range wanted {
		if _, ok := vars[key]; !ok {
			unknown = append(unknown, "{{"+key+"}}")
		}
	}
	if len(unknown) > 0 {
		sort.Strings(unknown)
		return "", fmt.Errorf("prompts: %s contains placeholder(s) %s this call site cannot substitute; it supplies %s — fix the placeholder spelling in the file", path, strings.Join(unknown, ", "), supplied(vars))
	}
	var missing []string
	for key := range vars {
		if !wanted[key] {
			missing = append(missing, "{{"+key+"}}")
		}
	}
	if len(missing) > 0 {
		sort.Strings(missing)
		return "", fmt.Errorf("prompts: %s uses none of the placeholder(s) %s, so the value(s) behind them would be silently dropped from the prompt; this call site supplies %s", path, strings.Join(missing, ", "), supplied(vars))
	}
	body = placeholderRe.ReplaceAllStringFunc(body, func(match string) string {
		return vars[placeholderRe.FindStringSubmatch(match)[1]]
	})
	return body, nil
}

// supplied renders the call site's placeholder names for an error message.
func supplied(vars map[string]string) string {
	if len(vars) == 0 {
		return "no placeholders"
	}
	names := make([]string, 0, len(vars))
	for key := range vars {
		names = append(names, "{{"+key+"}}")
	}
	sort.Strings(names)
	return strings.Join(names, ", ")
}
