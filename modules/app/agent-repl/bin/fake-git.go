// fakegit is the `git` the bin/ harnesses put on PATH. TEST SUPPORT ONLY.
//
// It is bin/fake-git.sh's model, compiled: bin/fake-git.sh builds this
// program (keyed by its source's checksum) and execs it, and the heavy
// harnesses install the binary as `git` directly. The model, its on-disk
// layout under <top>/.fakegit, its outputs and its exit statuses are the
// script's own; see bin/fake-git.sh's header for what is modelled.
//
// WHY COMPILED. Every call of the script was bash plus find, xargs, shasum,
// awk and sort: ~25ms and six processes a call. The build-frontend and
// readiness harnesses make ~1,900 calls a run, and under a full run's load
// those spawns, not their work, set the harnesses' wall: 219s and 167s for
// 48 and 30 CPU-seconds. One process doing the same work in memory is a
// couple of milliseconds a call.
//
// NO TEST RUNS REAL GIT (owner rule). Anything not modelled exits 2 naming
// the call, so a new git call in a script under test fails its harness loudly
// instead of passing on a guess.
package main

import (
	"bufio"
	"bytes"
	"crypto/sha1"
	"encoding/hex"
	"errors"
	"fmt"
	"io"
	"io/fs"
	"os"
	"os/exec"
	"path"
	"path/filepath"
	"sort"
	"strconv"
	"strings"
	"time"
)

// exitError is a die: the message for stderr and the status to exit with.
type exitError struct {
	code int
	msg  string
}

func (e *exitError) Error() string { return e.msg }

func die(code int, format string, args ...any) error {
	return &exitError{code: code, msg: fmt.Sprintf(format, args...)}
}

func main() {
	err := run(os.Args[1:], os.Stdout)
	var ee *exitError
	switch {
	case err == nil:
		os.Exit(0)
	case errors.As(err, &ee):
		if ee.msg != "" {
			fmt.Fprintf(os.Stderr, "fake git: %s\n", ee.msg)
		}
		os.Exit(ee.code)
	default:
		fmt.Fprintf(os.Stderr, "fake git: %v\n", err)
		os.Exit(128)
	}
}

// repo is one invocation's view of the fake repository.
type repo struct {
	cwd, top, g, main, prefix string
	linked                    bool
	specs                     []string
	ignores                   []string
	headSHA, headTree         string
}

func run(argv []string, out io.Writer) error {
	if log := os.Getenv("FAKE_GIT_LOG"); log != "" {
		f, err := os.OpenFile(log, os.O_APPEND|os.O_CREATE|os.O_WRONLY, 0o644)
		if err != nil {
			return err
		}
		_, werr := fmt.Fprintf(f, "git %s\n", strings.Join(argv, " "))
		cerr := f.Close()
		if werr != nil {
			return werr
		}
		if cerr != nil {
			return cerr
		}
	}
	cwd, err := logicalWD()
	if err != nil {
		return err
	}
	args := argv
	for len(args) > 0 {
		switch args[0] {
		case "-C":
			if len(args) < 2 {
				return die(128, "cannot change to ''")
			}
			dir := args[1]
			if strings.HasPrefix(dir, "/") {
				if !isDir(dir) {
					return die(128, "cannot change to '%s'", dir)
				}
				cwd = strings.TrimSuffix(dir, "/")
				if cwd == "" {
					cwd = "/"
				}
			} else {
				next := filepath.Clean(filepath.Join(cwd, dir))
				if !isDir(next) {
					return die(128, "cannot change to '%s'", dir)
				}
				cwd = next
			}
			args = args[2:]
		case "-c":
			args = args[min(2, len(args)):]
		default:
			goto parsed
		}
	}
parsed:
	if len(args) == 0 {
		return die(2, "no subcommand")
	}
	cmd, rest := args[0], args[1:]

	if cmd == "init" {
		for _, d := range []string{"commits", "objects"} {
			if err := os.MkdirAll(filepath.Join(cwd, ".fakegit", d), 0o755); err != nil {
				return err
			}
		}
		if err := os.WriteFile(filepath.Join(cwd, ".fakegit", "branch"), []byte("master\n"), 0o644); err != nil {
			return err
		}
		return os.WriteFile(filepath.Join(cwd, ".fakegit", "index"), nil, 0o644)
	}

	r := &repo{cwd: cwd}
	top, ok := findTop(cwd)
	if !ok {
		return die(128, "not a git repository (or any of the parent directories)")
	}
	r.top, r.g, r.main = top, top+"/.fakegit", top
	if info, err := os.Stat(r.g); err == nil && info.Mode().IsRegular() {
		line, _ := firstLineRaw(r.g)
		tag, mainDir := splitFirstWord(line)
		if tag != "main" || !isDir(mainDir+"/.fakegit") {
			return die(128, "a broken linked worktree at %s", top)
		}
		r.main, r.g, r.linked = mainDir, mainDir+"/.fakegit", true
	}
	r.prefix = strings.TrimPrefix(strings.TrimPrefix(cwd, top), "/")
	ignores, err := readLines(filepath.Join(top, ".gitignore"))
	if err != nil {
		return err
	}
	for _, l := range ignores {
		if l != "" {
			r.ignores = append(r.ignores, l)
		}
	}
	r.loadHead()

	opts, specArgs, dashdash := splitDashDash(rest)
	if err := r.setSpecs(specArgs, dashdash); err != nil {
		return err
	}
	joined := cmd
	if len(rest) > 0 {
		joined += " " + strings.Join(rest, " ")
	}
	argsJoined := strings.Join(rest, " ")

	if r.linked {
		switch {
		case joined == "rev-parse --git-dir":
			fmt.Fprintf(out, "%s/worktrees/%s\n", r.g, path.Base(r.top))
			return nil
		case joined == "rev-parse --git-common-dir", joined == "rev-parse --path-format=absolute --git-common-dir":
			fmt.Fprintf(out, "%s\n", r.g)
			return nil
		case joined == "rev-parse --show-toplevel", joined == "rev-parse --git-path hooks",
			joined == "worktree list --porcelain", strings.HasPrefix(joined, "worktree add "):
		default:
			return die(2, "unmodelled invocation inside a linked worktree: %s %s", cmd, argsJoined)
		}
	}

	switch {
	case joined == "rev-parse HEAD":
		if r.headSHA == "" {
			return die(128, "ambiguous argument 'HEAD': unknown revision")
		}
		fmt.Fprintf(out, "%s\n", r.headSHA)
	case joined == "rev-parse --show-toplevel":
		fmt.Fprintf(out, "%s\n", r.top)
	case joined == "rev-parse --is-inside-work-tree":
		fmt.Fprintf(out, "true\n")
	case joined == "rev-parse --git-dir", joined == "rev-parse --git-common-dir",
		joined == "rev-parse --path-format=absolute --git-common-dir":
		fmt.Fprintf(out, "%s\n", r.g)
	case joined == "rev-parse --abbrev-ref HEAD":
		branch, err := firstLineRaw(r.g + "/branch")
		if err != nil {
			return err
		}
		if branch == "" {
			return die(2, "no branch is recorded in %s/branch", r.g)
		}
		fmt.Fprintf(out, "%s\n", branch)
	case strings.HasPrefix(joined, "tag --points-at "):
		if len(opts) != 2 {
			return die(2, "unmodelled tag form: %s", argsJoined)
		}
		return r.pointsAt(out, opts[1])
	case strings.HasPrefix(joined, "rev-parse --verify --quiet "):
		if len(opts) != 3 {
			return die(2, "unmodelled rev-parse form: %s", argsJoined)
		}
		sha, ok := r.resolve(opts[2])
		if !ok {
			return die(1, "")
		}
		fmt.Fprintf(out, "%s\n", sha)
	case strings.HasPrefix(joined, "push --atomic "):
		if len(opts) < 3 {
			return die(2, "unmodelled push form: %s", argsJoined)
		}
		return r.push(opts[1], opts[2:])
	case strings.HasPrefix(joined, "ls-remote "):
		if len(opts) != 1 {
			return die(2, "unmodelled ls-remote form: %s", argsJoined)
		}
		return r.lsRemote(out, opts[0])
	case strings.HasPrefix(joined, "tag "), strings.HasPrefix(joined, "update-ref "):
		return r.refCommand(out, cmd, opts, argsJoined)
	case strings.HasPrefix(joined, "config --type=bool --get "):
		if len(opts) != 3 {
			return die(2, "unmodelled config form: %s", argsJoined)
		}
		key := opts[2]
		value, found, err := r.configGet(key)
		if err != nil {
			return err
		}
		if !found {
			return die(1, "")
		}
		switch strings.ToLower(value) {
		case "true", "yes", "on", "1":
			fmt.Fprintf(out, "true\n")
		case "false", "no", "off", "0", "":
			fmt.Fprintf(out, "false\n")
		default:
			return die(128, "bad boolean config value '%s' for '%s'", value, key)
		}
	case strings.HasPrefix(joined, "config "):
		if len(opts) != 2 || strings.HasPrefix(opts[0], "-") {
			return die(2, "unmodelled config form: %s", argsJoined)
		}
		return appendFile(r.g+"/config", fmt.Sprintf("%s=%s\n", opts[0], opts[1]))
	case strings.HasPrefix(joined, "diff --cached --name-only"):
		if strings.Join(opts, " ") != "--cached --name-only" {
			return die(2, "unmodelled diff form: %s", argsJoined)
		}
		changed, err := changedPaths(r.headTree, r.g+"/index")
		if err != nil {
			return err
		}
		for _, p := range changed {
			if r.pathMatches(p) {
				fmt.Fprintf(out, "%s\n", p)
			}
		}
	case joined == "worktree list --porcelain":
		phys, err := physical(r.main)
		if err != nil {
			return err
		}
		fmt.Fprintf(out, "worktree %s\n\n", phys)
		wts, err := readLines(r.g + "/worktrees")
		if err != nil {
			return err
		}
		for _, wt := range wts {
			fmt.Fprintf(out, "worktree %s\n\n", wt)
		}
	case joined == "rev-parse --git-path hooks":
		hooks, err := r.hooksPath()
		if err != nil {
			return err
		}
		fmt.Fprintf(out, "%s\n", hooks)
	case strings.HasPrefix(joined, "worktree add "):
		return r.worktreeAdd(opts, argsJoined)
	case joined == "add -A":
		r.specs = nil
		return r.stage()
	case strings.HasPrefix(joined, "add "):
		// The script hands EVERY argument to set_specs here, `--` included.
		if err := r.setSpecs(rest, true); err != nil {
			return err
		}
		return r.stage()
	case strings.HasPrefix(joined, "commit "):
		msg := ""
		for i, o := range opts {
			if (o == "-qm" || o == "-m") && i+1 < len(opts) {
				msg = opts[i+1]
			}
		}
		if msg == "" {
			return die(2, "unmodelled commit form: %s", argsJoined)
		}
		return r.commitIndex(msg)
	case joined == "revert --no-edit HEAD":
		return r.revert()
	case strings.HasPrefix(joined, "status --porcelain"):
		if strings.Join(opts, " ") != "--porcelain" {
			return die(2, "unmodelled status form: %s", argsJoined)
		}
		return r.status(out)
	case strings.HasPrefix(joined, "ls-files -s"):
		if strings.Join(opts, " ") != "-s" {
			return die(2, "unmodelled ls-files form: %s", argsJoined)
		}
		index, err := readEntries(r.g + "/index")
		if err != nil {
			return err
		}
		for _, e := range index {
			if r.pathMatches(e.path) {
				fmt.Fprintf(out, "100644 %s 0\t%s\n", e.blob, e.path)
			}
		}
	case strings.HasPrefix(joined, "log -1 --format="):
		if len(opts) != 2 {
			return die(2, "unmodelled log form: %s", argsJoined)
		}
		format := strings.TrimPrefix(opts[1], "--format=")
		for sha := r.headSHA; sha != ""; {
			touches, err := r.commitTouches(sha)
			if err != nil {
				return err
			}
			if len(r.specs) == 0 || touches {
				line, err := r.formatCommit(format, sha)
				if err != nil {
					return err
				}
				fmt.Fprintf(out, "%s\n", line)
				break
			}
			sha, err = parentOf(r.g, sha)
			if err != nil {
				return err
			}
		}
	case strings.HasPrefix(joined, "show -s --format=%ct "):
		if len(opts) != 3 {
			return die(2, "unmodelled show form: %s", argsJoined)
		}
		sha, ok := r.resolve(opts[2])
		if !ok {
			return die(128, "bad object %s", opts[2])
		}
		ct, _ := firstLineRaw(r.g + "/commits/" + sha + "/ct")
		fmt.Fprintf(out, "%s\n", ct)
	case strings.HasPrefix(joined, "rev-list --count "):
		if len(opts) != 2 {
			return die(2, "unmodelled rev-list form: %s", argsJoined)
		}
		fromRev, toRev := opts[1], opts[1]
		if i := strings.Index(opts[1], ".."); i >= 0 {
			fromRev, toRev = opts[1][:i], opts[1][i+2:]
		}
		from, ok := r.resolve(fromRev)
		if !ok {
			return die(128, "bad revision '%s'", fromRev)
		}
		sha, ok := r.resolve(toRev)
		if !ok {
			return die(128, "bad revision '%s'", toRev)
		}
		count := 0
		for sha != "" && sha != from {
			touches, err := r.commitTouches(sha)
			if err != nil {
				return err
			}
			if len(r.specs) == 0 || touches {
				count++
			}
			if sha, err = parentOf(r.g, sha); err != nil {
				return err
			}
		}
		fmt.Fprintf(out, "%d\n", count)
	default:
		return die(2, "unmodelled invocation: %s %s", cmd, argsJoined)
	}
	return nil
}

// ---- named refs and ref transactions -----------------------------------------
//
// Named refs (tags, and whatever update-ref names) are a flat store, one
// "<ref> <sha>" line each in .fakegit/refs. The checked-out branch's tip is
// HEAD (.fakegit/HEAD), named by .fakegit/branch; it is not in the store.
// Every change to the store is one ref transaction, run the way git runs one:
// the reference-transaction hook's `prepared` phase, where a nonzero exit
// aborts it (and the hook then hears `aborted`), then the write, then
// `committed`. A commit does not run the hook.

const zeroOID = "0000000000000000000000000000000000000000"

// refCommand is tag NAME REV, tag -d NAME, update-ref REF REV and
// update-ref -d REF.
func (r *repo) refCommand(out io.Writer, cmd string, opts []string, argsJoined string) error {
	refs, err := r.readRefs()
	if err != nil {
		return err
	}
	if len(opts) != 2 {
		return die(2, "unmodelled %s form: %s", cmd, argsJoined)
	}
	if opts[0] == "-d" {
		ref := opts[1]
		if cmd == "tag" {
			ref = "refs/tags/" + opts[1]
		}
		old, ok := refs[ref]
		if !ok {
			if cmd == "tag" {
				return die(1, "tag '%s' not found", opts[1])
			}
			return die(2, "unmodelled: deleting the missing ref %s", ref)
		}
		if err := r.transact(ref, old, zeroOID); err != nil {
			return err
		}
		if cmd == "tag" {
			fmt.Fprintf(out, "Deleted tag '%s' (was %s)\n", opts[1], old[:7])
		}
		return nil
	}
	ref, rev := opts[0], opts[1]
	if strings.HasPrefix(ref, "-") {
		return die(2, "unmodelled %s form: %s", cmd, argsJoined)
	}
	if cmd == "tag" {
		ref = "refs/tags/" + opts[0]
		if _, exists := refs[ref]; exists {
			return die(128, "tag '%s' already exists", opts[0])
		}
	}
	sha, ok := r.resolve(rev)
	if !ok {
		return die(128, "'%s' is not a valid revision", rev)
	}
	old, ok := refs[ref]
	if !ok {
		old = zeroOID
	}
	return r.transact(ref, old, sha)
}

func (r *repo) readRefs() (map[string]string, error) {
	return readStore(r.g + "/refs")
}

// readStore reads a flat ref store, one "<ref> <sha>" line each.
func readStore(file string) (map[string]string, error) {
	lines, err := readLines(file)
	if err != nil {
		return nil, err
	}
	refs := map[string]string{}
	for _, l := range lines {
		if ref, sha, ok := strings.Cut(l, " "); ok {
			refs[ref] = sha
		}
	}
	return refs, nil
}

// writeStore writes REFS as a flat ref store, sorted by ref.
func writeStore(file string, refs map[string]string) error {
	var b strings.Builder
	for _, name := range sortedKeys(refs) {
		fmt.Fprintf(&b, "%s %s\n", name, refs[name])
	}
	return os.WriteFile(file, []byte(b.String()), 0o644)
}

func sortedKeys(m map[string]string) []string {
	keys := make([]string, 0, len(m))
	for k := range m {
		keys = append(keys, k)
	}
	sort.Strings(keys)
	return keys
}

// transact moves REF from OLD to NEW (zeroOID deletes it) as one transaction.
func (r *repo) transact(ref, old, new string) error {
	tx := fmt.Sprintf("%s %s %s\n", old, new, ref)
	code, err := r.runRefHook("prepared", tx)
	if err != nil {
		return err
	}
	if code != 0 {
		if _, err := r.runRefHook("aborted", tx); err != nil {
			return err
		}
		return die(1, "ref updates aborted by hook")
	}
	refs, err := r.readRefs()
	if err != nil {
		return err
	}
	if new == zeroOID {
		delete(refs, ref)
	} else {
		refs[ref] = new
	}
	if err := writeStore(r.g+"/refs", refs); err != nil {
		return err
	}
	_, err = r.runRefHook("committed", tx)
	return err
}

// runRefHook runs the reference-transaction hook in PHASE with TX on stdin.
func (r *repo) runRefHook(phase, tx string) (int, error) {
	return r.runHook("reference-transaction", tx, phase)
}

// runHook runs the hook NAME, when one is installed, with ARGS and STDIN, from
// the worktree's top, and answers its exit status. Like git, it sends the
// hook's stdout to stderr.
func (r *repo) runHook(name, stdin string, args ...string) (int, error) {
	hooks, err := r.hooksPath()
	if err != nil {
		return 0, err
	}
	if !filepath.IsAbs(hooks) {
		hooks = filepath.Join(r.top, hooks)
	}
	hook := filepath.Join(hooks, name)
	info, err := os.Stat(hook)
	if errors.Is(err, fs.ErrNotExist) || (err == nil && info.Mode().Perm()&0o111 == 0) {
		return 0, nil
	}
	if err != nil {
		return 0, err
	}
	c := exec.Command(hook, args...)
	c.Dir, c.Stdin, c.Stdout, c.Stderr = r.top, strings.NewReader(stdin), os.Stderr, os.Stderr
	if err := c.Run(); err != nil {
		var exit *exec.ExitError
		if errors.As(err, &exit) {
			return exit.ExitCode(), nil
		}
		return 0, err
	}
	return 0, nil
}

// pointsAt is tag --points-at REV: every tag naming REV's commit, sorted.
func (r *repo) pointsAt(out io.Writer, rev string) error {
	sha, ok := r.resolve(rev)
	if !ok {
		return die(129, "malformed object name %s", rev)
	}
	refs, err := r.readRefs()
	if err != nil {
		return err
	}
	var names []string
	for ref, target := range refs {
		if name, ok := strings.CutPrefix(ref, "refs/tags/"); ok && target == sha {
			names = append(names, name)
		}
	}
	sort.Strings(names)
	for _, name := range names {
		fmt.Fprintf(out, "%s\n", name)
	}
	return nil
}

// ---- remotes ------------------------------------------------------------------
//
// A remote is a flat ref store, one "<ref> <sha>" line each in
// .fakegit/remotes/<name>, holding what pushes to it have sent. Only an atomic
// push is modelled: the pre-push hook hears every ref the push changes, as git
// sends them, and a nonzero exit leaves the remote untouched.

// push is push --atomic REMOTE SRC:DST..., each SRC a ref or commit.
func (r *repo) push(remote string, specs []string) error {
	store := r.g + "/remotes/" + remote
	have, err := readStore(store)
	if err != nil {
		return err
	}
	var tx strings.Builder
	updates := map[string]string{}
	for _, spec := range specs {
		src, dst, ok := strings.Cut(spec, ":")
		if !ok || src == "" || dst == "" {
			return die(2, "unmodelled push refspec: %s", spec)
		}
		sha, ok := r.resolve(src)
		if !ok {
			return die(1, "src refspec %s does not match any", src)
		}
		old, ok := have[dst]
		if !ok {
			old = zeroOID
		}
		if old == sha {
			continue
		}
		fmt.Fprintf(&tx, "%s %s %s %s\n", src, sha, dst, old)
		updates[dst] = sha
	}
	if len(updates) == 0 {
		return nil
	}
	code, err := r.runHook("pre-push", tx.String(), remote, remote)
	if err != nil {
		return err
	}
	if code != 0 {
		return die(1, "failed to push some refs to '%s'", remote)
	}
	for ref, sha := range updates {
		have[ref] = sha
	}
	if err := os.MkdirAll(r.g+"/remotes", 0o755); err != nil {
		return err
	}
	return writeStore(store, have)
}

// lsRemote is ls-remote REMOTE: "<sha>\t<ref>" for every ref the remote holds.
func (r *repo) lsRemote(out io.Writer, remote string) error {
	have, err := readStore(r.g + "/remotes/" + remote)
	if err != nil {
		return err
	}
	for _, ref := range sortedKeys(have) {
		fmt.Fprintf(out, "%s\t%s\n", have[ref], ref)
	}
	return nil
}

// hooksPath is core.hooksPath as configured, else <repository>/hooks.
func (r *repo) hooksPath() (string, error) {
	hooks, found, err := r.configGet("core.hooksPath")
	if err != nil {
		return "", err
	}
	if found {
		return hooks, nil
	}
	return r.g + "/hooks", nil
}

// ---- repository discovery and state ------------------------------------------

// logicalWD is bash's $PWD: the inherited PWD when it names this directory,
// else the physical one.
func logicalWD() (string, error) {
	physicalWD, err := os.Getwd()
	if err != nil {
		return "", err
	}
	if pwd := os.Getenv("PWD"); pwd != "" && filepath.IsAbs(pwd) {
		a, errA := os.Stat(pwd)
		b, errB := os.Stat(physicalWD)
		if errA == nil && errB == nil && os.SameFile(a, b) {
			return pwd, nil
		}
	}
	return physicalWD, nil
}

func findTop(cwd string) (string, bool) {
	ceiling := os.Getenv("GIT_CEILING_DIRECTORIES")
	dir := cwd
	for {
		if _, err := os.Stat(dir + "/.fakegit"); err == nil {
			return dir, true
		}
		if ceiling != "" && dir == ceiling {
			return "", false
		}
		if dir == "/" {
			return "", false
		}
		if i := strings.LastIndex(dir, "/"); i > 0 {
			dir = dir[:i]
		} else {
			dir = "/"
		}
	}
}

func (r *repo) loadHead() {
	r.headSHA, r.headTree = "", os.DevNull
	if _, err := os.Stat(r.g + "/HEAD"); err == nil {
		r.headSHA, _ = firstLineRaw(r.g + "/HEAD")
		r.headTree = r.g + "/commits/" + r.headSHA + "/tree"
	}
}

func (r *repo) configGet(key string) (string, bool, error) {
	lines, err := readLines(r.g + "/config")
	if err != nil {
		return "", false, err
	}
	value, found := "", false
	for _, line := range lines {
		k, v, _ := strings.Cut(line, "=")
		if k == key {
			value, found = v, true
		}
	}
	return value, found, nil
}

func (r *repo) resolve(rev string) (string, bool) {
	if rev == "HEAD" {
		return r.headSHA, r.headSHA != ""
	}
	if isDir(r.g + "/commits/" + rev) {
		return rev, true
	}
	if branch, err := firstLineRaw(r.g + "/branch"); err == nil && branch != "" && rev == "refs/heads/"+branch {
		return r.headSHA, r.headSHA != ""
	}
	if refs, err := r.readRefs(); err == nil {
		if sha, ok := refs[rev]; ok {
			return sha, true
		}
	}
	return "", false
}

func parentOf(g, sha string) (string, error) {
	p, err := firstLineRaw(g + "/commits/" + sha + "/parent")
	if errors.Is(err, fs.ErrNotExist) {
		return "", nil
	}
	return p, err
}

func (r *repo) formatCommit(format, sha string) (string, error) {
	out := strings.ReplaceAll(format, "%H", sha)
	if strings.Contains(out, "%ct") {
		ct, err := firstLineRaw(r.g + "/commits/" + sha + "/ct")
		if err != nil {
			return "", err
		}
		out = strings.ReplaceAll(out, "%ct", ct)
	}
	return out, nil
}

func (r *repo) commitTouches(sha string) (bool, error) {
	changed, err := readLines(r.g + "/commits/" + sha + "/changed")
	if err != nil {
		return false, err
	}
	for _, p := range changed {
		if r.pathMatches(p) {
			return true, nil
		}
	}
	return false, nil
}

// ---- pathspecs ------------------------------------------------------------------

// splitDashDash answers the arguments before `--`, those after it, and
// whether there was one.
func splitDashDash(args []string) (opts, specs []string, dashdash bool) {
	for i, a := range args {
		if a == "--" {
			return args[:i], args[i+1:], true
		}
	}
	return args, nil, false
}

// setSpecs normalizes a pathspec to top-relative form, as set_specs does.
func (r *repo) setSpecs(args []string, _ bool) error {
	r.specs = nil
	for _, s := range args {
		neg := ""
		if strings.HasPrefix(s, ":(exclude)") {
			neg, s = ":(exclude)", strings.TrimPrefix(s, ":(exclude)")
		}
		switch {
		case s == r.top:
			s = "."
		case strings.HasPrefix(s, r.top+"/"):
			s = strings.TrimPrefix(s, r.top+"/")
		case strings.HasPrefix(s, "/"):
			return die(128, "'%s' is outside repository at '%s'", s, r.top)
		case r.prefix != "":
			s = r.prefix + "/" + s
		}
		s = strings.TrimSuffix(s, "/")
		r.specs = append(r.specs, neg+s)
	}
	return nil
}

func under(p, s string) bool { return p == s || strings.HasPrefix(p, s+"/") }

func (r *repo) pathMatches(p string) bool {
	if len(r.specs) == 0 {
		return true
	}
	included, anyInclude := false, false
	for _, s := range r.specs {
		if strings.HasPrefix(s, ":(exclude)") {
			continue
		}
		anyInclude = true
		if s == "." || under(p, s) {
			included = true
		}
	}
	if !anyInclude {
		included = true
	}
	if !included {
		return false
	}
	for _, s := range r.specs {
		if ex, ok := strings.CutPrefix(s, ":(exclude)"); ok && under(p, ex) {
			return false
		}
	}
	return true
}

// ---- snapshots ------------------------------------------------------------------

// entry is one "<blob>\t<path>" line of a snapshot, the index or a tree.
type entry struct{ blob, path string }

func readEntries(file string) ([]entry, error) {
	lines, err := readLines(file)
	if err != nil {
		return nil, err
	}
	out := make([]entry, 0, len(lines))
	for _, l := range lines {
		blob, p, _ := strings.Cut(l, "\t")
		out = append(out, entry{blob, p})
	}
	return out, nil
}

func sortEntries(es []entry) {
	sort.SliceStable(es, func(i, j int) bool {
		if es[i].path != es[j].path {
			return es[i].path < es[j].path
		}
		return es[i].blob < es[j].blob
	})
}

// snapshot is every file of the working tree not ignored, with its blob,
// sorted by path: the script's worktree_snapshot.
func (r *repo) snapshot() ([]entry, error) {
	prunePaths := map[string]bool{".fakegit": true}
	wts, err := readLines(r.g + "/worktrees")
	if err != nil {
		return nil, err
	}
	if len(wts) > 0 {
		topPhysical, err := physical(r.top)
		if err != nil {
			return nil, err
		}
		for _, wt := range wts {
			if rel, ok := strings.CutPrefix(wt, topPhysical+"/"); ok {
				prunePaths[rel] = true
			}
		}
	}
	// The walk is relative to the top, as the script's `cd "$TOP"; find .` is:
	// a top spelled with a doubled slash (mktemp under a TMPDIR ending in "/")
	// names the same directory, and every path below it is the same.
	root := filepath.Clean(r.top)
	var out []entry
	err = filepath.WalkDir(root, func(full string, d fs.DirEntry, err error) error {
		if err != nil {
			return err
		}
		if full == root {
			return nil
		}
		rel, err := filepath.Rel(root, full)
		if err != nil {
			return err
		}
		if prunePaths[rel] || r.ignored(d) {
			if d.IsDir() {
				return filepath.SkipDir
			}
			return nil
		}
		if !d.Type().IsRegular() {
			return nil
		}
		blob, err := hashFile(full)
		if err != nil {
			return err
		}
		out = append(out, entry{blob, rel})
		return nil
	})
	if err != nil {
		return nil, err
	}
	sortEntries(out)
	return out, nil
}

// ignored applies the .gitignore rules to one walked entry: `name/` prunes a
// directory of that name at any depth, any other pattern prunes an entry
// whose name matches it (find -name).
func (r *repo) ignored(d fs.DirEntry) bool {
	for _, pat := range r.ignores {
		if dirPat, ok := strings.CutSuffix(pat, "/"); ok {
			if d.IsDir() {
				if m, _ := path.Match(dirPat, d.Name()); m {
					return true
				}
			}
			continue
		}
		if m, _ := path.Match(pat, d.Name()); m {
			return true
		}
	}
	return false
}

func hashFile(p string) (string, error) {
	f, err := os.Open(p)
	if err != nil {
		return "", err
	}
	defer f.Close()
	h := sha1.New()
	if _, err := io.Copy(h, f); err != nil {
		return "", err
	}
	return hex.EncodeToString(h.Sum(nil)), nil
}

func (r *repo) filter(es []entry) []entry {
	var out []entry
	for _, e := range es {
		if r.pathMatches(e.path) {
			out = append(out, e)
		}
	}
	return out
}

// changedPaths answers the paths whose entry differs between two snapshot
// files, sorted (the script's changed_paths).
func changedPaths(a, b string) ([]string, error) {
	ea, err := readEntries(a)
	if err != nil {
		return nil, err
	}
	eb, err := readEntries(b)
	if err != nil {
		return nil, err
	}
	blob := map[string]string{}
	for _, e := range ea {
		blob[e.path] = e.blob
	}
	var out []string
	for _, e := range eb {
		if old, ok := blob[e.path]; !ok || old != e.blob {
			out = append(out, e.path)
		}
		delete(blob, e.path)
	}
	for p := range blob {
		out = append(out, p)
	}
	sort.Strings(out)
	return out, nil
}

func writeEntries(file string, es []entry) error {
	var b strings.Builder
	for _, e := range es {
		fmt.Fprintf(&b, "%s\t%s\n", e.blob, e.path)
	}
	return os.WriteFile(file, []byte(b.String()), 0o644)
}

// stage records the working tree's entries for the specs in the index, and
// their content in the object store.
func (r *repo) stage() error {
	index, err := readEntries(r.g + "/index")
	if err != nil {
		return err
	}
	var next []entry
	for _, e := range index {
		if !r.pathMatches(e.path) {
			next = append(next, e)
		}
	}
	snap, err := r.snapshot()
	if err != nil {
		return err
	}
	for _, e := range r.filter(snap) {
		obj := r.g + "/objects/" + e.blob
		if _, err := os.Stat(obj); errors.Is(err, fs.ErrNotExist) {
			if err := copyFile(filepath.Join(r.top, e.path), obj); err != nil {
				return err
			}
		} else if err != nil {
			return err
		}
		next = append(next, e)
	}
	sortEntries(next)
	return writeEntries(r.g+"/index", next)
}

// commitIndex snapshots the index as a new commit on HEAD.
func (r *repo) commitIndex(msg string) error {
	changed, err := changedPaths(r.headTree, r.g+"/index")
	if err != nil {
		return err
	}
	if len(changed) == 0 {
		return die(1, "nothing to commit, working tree clean")
	}
	ct := strconv.FormatInt(time.Now().Unix(), 10)
	if d, ok := strings.CutPrefix(os.Getenv("GIT_COMMITTER_DATE"), "@"); ok {
		ct, _, _ = strings.Cut(d, " ")
	}
	index, err := os.ReadFile(r.g + "/index")
	if err != nil {
		return err
	}
	h := sha1.New()
	fmt.Fprintf(h, "%s\n%s\n%s\n", r.headSHA, ct, msg)
	h.Write(index)
	sha := hex.EncodeToString(h.Sum(nil))
	dir := r.g + "/commits/" + sha
	if err := os.MkdirAll(dir, 0o755); err != nil {
		return err
	}
	files := map[string][]byte{
		"tree":    index,
		"changed": []byte(strings.Join(changed, "\n") + "\n"),
		"parent":  []byte(r.headSHA + "\n"),
		"ct":      []byte(ct + "\n"),
		"msg":     []byte(msg + "\n"),
	}
	for name, body := range files {
		if err := os.WriteFile(dir+"/"+name, body, 0o644); err != nil {
			return err
		}
	}
	if err := os.WriteFile(r.g+"/HEAD", []byte(sha+"\n"), 0o644); err != nil {
		return err
	}
	r.loadHead()
	return nil
}

func (r *repo) revert() error {
	h := r.headSHA
	if h == "" {
		return die(128, "no HEAD to revert")
	}
	p, err := parentOf(r.g, h)
	if err != nil {
		return err
	}
	if p == "" {
		return die(2, "reverting a root commit is not modelled")
	}
	changed, err := readLines(r.g + "/commits/" + h + "/changed")
	if err != nil {
		return err
	}
	parentTree, err := readEntries(r.g + "/commits/" + p + "/tree")
	if err != nil {
		return err
	}
	for _, path := range changed {
		blob := ""
		for _, e := range parentTree {
			if e.path == path {
				blob = e.blob
			}
		}
		target := filepath.Join(r.top, path)
		if blob != "" {
			if err := os.MkdirAll(filepath.Dir(target), 0o755); err != nil {
				return err
			}
			if err := copyFile(r.g+"/objects/"+blob, target); err != nil {
				return err
			}
		} else if err := os.Remove(target); err != nil && !errors.Is(err, fs.ErrNotExist) {
			return err
		}
	}
	if err := r.setSpecs(changed, true); err != nil {
		return err
	}
	if err := r.stage(); err != nil {
		return err
	}
	msg, err := os.ReadFile(r.g + "/commits/" + h + "/msg")
	if err != nil {
		return err
	}
	return r.commitIndex("Revert \"" + strings.TrimRight(string(msg), "\n") + "\"")
}

func (r *repo) status(out io.Writer) error {
	snap, err := r.snapshot()
	if err != nil {
		return err
	}
	index, err := readEntries(r.g + "/index")
	if err != nil {
		return err
	}
	head, err := readEntries(r.headTree)
	if err != nil {
		return err
	}
	w, i, h := map[string]string{}, map[string]string{}, map[string]string{}
	seen := map[string]bool{}
	for _, e := range r.filter(snap) {
		w[e.path], seen[e.path] = e.blob, true
	}
	for _, e := range r.filter(index) {
		i[e.path], seen[e.path] = e.blob, true
	}
	for _, e := range r.filter(head) {
		h[e.path], seen[e.path] = e.blob, true
	}
	var lines []string
	for p := range seen {
		if w[p] != i[p] || i[p] != h[p] {
			lines = append(lines, " M "+p)
		}
	}
	sort.Strings(lines)
	for _, l := range lines {
		fmt.Fprintf(out, "%s\n", l)
	}
	return nil
}

func (r *repo) worktreeAdd(opts []string, argsJoined string) error {
	wt, branch := "", ""
	for i := 1; i < len(opts); i++ {
		switch o := opts[i]; {
		case o == "--quiet" || o == "-q":
		case o == "-b":
			i++
			if i < len(opts) {
				branch = opts[i]
			}
		case o == "--detach":
			branch = "(detached)"
		case strings.HasPrefix(o, "-"):
			return die(2, "unmodelled worktree add form: %s", argsJoined)
		default:
			if wt != "" {
				return die(2, "unmodelled worktree add form: %s", argsJoined)
			}
			wt = o
		}
	}
	if wt == "" || branch == "" {
		return die(2, "unmodelled worktree add form: %s", argsJoined)
	}
	if !strings.HasPrefix(wt, "/") {
		wt = r.cwd + "/" + wt
	}
	if _, err := os.Lstat(wt); err == nil {
		return die(128, "'%s' already exists", wt)
	}
	if r.headSHA == "" {
		return die(128, "invalid reference: HEAD")
	}
	if err := os.MkdirAll(wt, 0o755); err != nil {
		return err
	}
	tree, err := readEntries(r.headTree)
	if err != nil {
		return err
	}
	for _, e := range tree {
		target := wt + "/" + e.path
		if err := os.MkdirAll(filepath.Dir(target), 0o755); err != nil {
			return err
		}
		if err := copyFile(r.g+"/objects/"+e.blob, target); err != nil {
			return err
		}
	}
	if err := os.WriteFile(wt+"/.fakegit", []byte("main "+r.main+"\n"), 0o644); err != nil {
		return err
	}
	phys, err := physical(wt)
	if err != nil {
		return err
	}
	return appendFile(r.g+"/worktrees", phys+"\n")
}

// ---- file helpers ------------------------------------------------------------

// readLines answers a file's newline-terminated lines, as a bash `while read`
// loop sees them: an unterminated last line is not one. A missing file has
// no lines.
func readLines(file string) ([]string, error) {
	data, err := os.ReadFile(file)
	if errors.Is(err, fs.ErrNotExist) {
		return nil, nil
	}
	if err != nil {
		return nil, err
	}
	if i := bytes.LastIndexByte(data, '\n'); i >= 0 {
		data = data[:i+1]
	} else {
		return nil, nil
	}
	var out []string
	sc := bufio.NewScanner(bytes.NewReader(data))
	sc.Buffer(make([]byte, 0, 64*1024), 64*1024*1024)
	for sc.Scan() {
		out = append(out, sc.Text())
	}
	return out, sc.Err()
}

// firstLineRaw is a file's first line, terminated or not (`read -r || true`).
func firstLineRaw(file string) (string, error) {
	data, err := os.ReadFile(file)
	if err != nil {
		return "", err
	}
	line, _, _ := strings.Cut(string(data), "\n")
	return line, nil
}

// splitFirstWord splits `read -r tag rest` style: the first word, then the
// rest of the line with its leading blanks removed.
func splitFirstWord(line string) (string, string) {
	line = strings.TrimLeft(line, " \t")
	word, rest, _ := strings.Cut(line, " ")
	if i := strings.IndexByte(word, '\t'); i >= 0 {
		word, rest = word[:i], word[i+1:]+" "+rest
	}
	return word, strings.TrimRight(strings.TrimLeft(rest, " \t"), " \t")
}

func isDir(p string) bool {
	info, err := os.Stat(p)
	return err == nil && info.IsDir()
}

func physical(p string) (string, error) {
	abs, err := filepath.Abs(p)
	if err != nil {
		return "", err
	}
	return filepath.EvalSymlinks(abs)
}

func appendFile(file, text string) error {
	f, err := os.OpenFile(file, os.O_APPEND|os.O_CREATE|os.O_WRONLY, 0o644)
	if err != nil {
		return err
	}
	if _, err := f.WriteString(text); err != nil {
		f.Close()
		return err
	}
	return f.Close()
}

func copyFile(src, dst string) error {
	data, err := os.ReadFile(src)
	if err != nil {
		return err
	}
	mode := fs.FileMode(0o644)
	if info, err := os.Stat(src); err == nil {
		mode = info.Mode().Perm()
	}
	return os.WriteFile(dst, data, mode)
}
