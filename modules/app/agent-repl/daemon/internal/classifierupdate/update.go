// Package classifierupdate rewrites the routing classifier's brief on the
// user's say-so: UpdateClassifierPrompt, the held card's "update classifier"
// control.
//
// The user states a change in their own words; a headless model rewrites
// `prompts/queue-routing-classifier.md` to carry it; the daemon checks the
// rewrite is still a usable brief, writes it, and COMMITS THAT ONE FILE. The
// classifier reads its brief at use time, so the next classification already
// follows the rewrite.
//
// ONLY THAT CHANGE, ONLY THAT FILE, made structural rather than hoped for:
//
//   - a brief that already carries uncommitted edits is refused before the
//     model runs, so the commit can only ever hold the rewrite;
//   - the commit is gitclient.CommitPath, which commits the brief's path
//     alone through `--only`, so nothing else staged in the checkout rides
//     along;
//   - the model never touches the filesystem: it answers text, and the daemon
//     writes exactly one file;
//   - updates run one at a time (a TryLock), so two rewrites are never made
//     from the same starting brief;
//   - a commit git refuses puts the file back the way it was.
package classifierupdate

import (
	"context"
	"errors"
	"fmt"
	"os"
	"strings"
	"sync"
	"time"

	"claude-repld/internal/atomicfile"
	"claude-repld/internal/classifier"
	"claude-repld/internal/dlog"
	"claude-repld/internal/headless"
	"claude-repld/internal/prompts"
)

// The rewrite call.
const (
	// BriefRewrite is the prompts-directory brief the rewrite is composed
	// from.
	BriefRewrite = "queue-routing-classifier-update"
	// Site is the vendor-guard site the rewrite asks under.
	Site = "classifier_update"
	// Model is the model that rewrites: Sonnet, the daemon's model for prose
	// a person reads closely, and a classifier brief is exactly that.
	Model = headless.ModelSonnet
	// RunTimeout bounds one rewrite: Sonnet reading and rewriting a brief of a
	// few kilobytes.
	RunTimeout = 3 * time.Minute
	// CommitSubject is the subject line of every rewrite's commit.
	CommitSubject = "chore(agent-repl/prompts): update the queue routing classifier prompt"
)

// The slot markers. The classifier brief's `{{name}}` placeholders are shown
// to the model as `⟦name⟧`, because a `{{...}}` inside a spliced value is
// exactly what prompts.Splice refuses, and the model's answer is turned back
// before it is checked. The rewrite brief spells the same two characters.
const (
	slotOpen  = "⟦"
	slotClose = "⟧"
)

// op is the operation every record this package writes is filed under.
const op = "daemon.classifierupdate.update"

// The refusal arms, spelled exactly as UpdateClassifierPromptError names
// them, so the server maps a refusal onto its arm by name.
const (
	ArmInProgress           = "in_progress"
	ArmUncommittedChanges   = "uncommitted_changes"
	ArmRewriteFailed        = "rewrite_failed"
	ArmRewriteRejected      = "rewrite_rejected"
	ArmUnchanged            = "unchanged"
	ArmChangedDuringRewrite = "changed_during_rewrite"
	ArmCommitFailed         = "commit_failed"
)

// Refusal is an update the contract answers with a typed arm. Every refusal
// leaves the brief on disk exactly as it was.
type Refusal struct {
	// Arm is the UpdateClassifierPromptError arm.
	Arm string
	// Reason is the refusal in the daemon's words.
	Reason string
	// Fields are the arm message's own field values, keyed by proto field
	// name.
	Fields map[string]any
}

func (r *Refusal) Error() string { return r.Arm + ": " + r.Reason }

// AsRefusal reads a typed refusal out of err.
func AsRefusal(err error) (*Refusal, bool) {
	var refusal *Refusal
	if errors.As(err, &refusal) {
		return refusal, true
	}
	return nil, false
}

// Example is the held prompt the change was asked from.
type Example struct {
	// Text is what the user said, the text blocks joined as typed.
	Text string
	// Route is the verdict the classifier gave it.
	Route classifier.Route
}

// Request is one update.
type Request struct {
	// Instruction is the change, in the user's words. Never blank: the server
	// refuses a blank one before it gets here.
	Instruction string
	// Example is the held prompt the change was asked from.
	Example Example
}

// Result is a committed update.
type Result struct {
	// Commit is the commit's full sha.
	Commit string
	// Path is the brief's absolute path.
	Path string
}

// Git is the slice of gitclient.Git an update uses.
type Git interface {
	// PathClean reports whether one path carries no uncommitted change.
	PathClean(ctx context.Context, dir, path string) (bool, error)
	// CommitPath commits one path alone and answers the commit's sha.
	CommitPath(ctx context.Context, dir, path, message string) (string, error)
}

// Files reads and writes the brief. It is a seam so a write failure is
// testable.
type Files interface {
	// Read answers the file's whole content.
	Read(path string) ([]byte, error)
	// Write replaces the file's whole content.
	Write(path string, content []byte) error
}

// Updater runs updates, one at a time.
type Updater struct {
	headless   headless.Runner
	git        Git
	files      Files
	promptsDir string
	log        dlog.Logger
	// running is held for an update's whole length. TryLock, never Lock: a
	// second update while one runs is answered in_progress at once.
	running sync.Mutex
}

// New builds an Updater over the brief in promptsDir. Every dependency is
// required.
func New(runner headless.Runner, git Git, files Files, promptsDir string, log dlog.Logger) (*Updater, error) {
	switch {
	case runner == nil:
		return nil, errors.New("classifierupdate: no headless runner")
	case git == nil:
		return nil, errors.New("classifierupdate: no git")
	case files == nil:
		return nil, errors.New("classifierupdate: no files")
	case promptsDir == "":
		return nil, errors.New("classifierupdate: no prompts directory")
	case log == nil:
		return nil, errors.New("classifierupdate: no logger")
	}
	return &Updater{headless: runner, git: git, files: files, promptsDir: promptsDir, log: log}, nil
}

// Update rewrites the routing brief to carry req and commits it. A *Refusal
// is the contract's answer and is recorded here, once; any other error is a
// failure outside the contract, recorded by the rpc that surfaces it.
func (u *Updater) Update(ctx context.Context, req Request) (Result, error) {
	path := prompts.Path(u.promptsDir, classifier.BriefRouting)
	log := u.log.With(dlog.Context{"path": path, "route": req.Example.Route.String()})
	if !u.running.TryLock() {
		return Result{}, refuse(log, dlog.Logger.Info, ArmInProgress, "another classifier update is already running", nil)
	}
	defer u.running.Unlock()

	clean, err := u.git.PathClean(ctx, u.promptsDir, path)
	if err != nil {
		return Result{}, fmt.Errorf("classifierupdate: probe %s for uncommitted changes: %w", path, err)
	}
	if !clean {
		return Result{}, refuse(log, dlog.Logger.Info, ArmUncommittedChanges,
			fmt.Sprintf("%s carries uncommitted changes; commit or revert them first", path),
			map[string]any{"path": path})
	}
	original, err := u.files.Read(path)
	if err != nil {
		return Result{}, fmt.Errorf("classifierupdate: read %s: %w", path, err)
	}
	header, body, err := splitBrief(path, string(original))
	if err != nil {
		return Result{}, err
	}

	question, err := u.compose(body, req)
	if err != nil {
		return Result{}, err
	}
	resp, err := u.headless.Run(ctx, headless.Request{
		Site:    Site,
		Model:   Model,
		Format:  headless.FormatText,
		Prompt:  question,
		Timeout: RunTimeout,
	})
	if err != nil {
		return Result{}, refuse(log, dlog.Logger.Error, ArmRewriteFailed,
			fmt.Sprintf("the rewrite did not answer (%s): %v", headless.CauseOf(err), err), nil)
	}
	rewritten, reason := rebuild(header, resp.Text)
	if reason != "" {
		return Result{}, refuse(log, dlog.Logger.Error, ArmRewriteRejected, reason, nil)
	}
	if rewritten == string(original) {
		return Result{}, refuse(log, dlog.Logger.Info, ArmUnchanged, "the rewrite left the brief unchanged", nil)
	}

	current, err := u.files.Read(path)
	if err != nil {
		return Result{}, fmt.Errorf("classifierupdate: re-read %s before writing: %w", path, err)
	}
	if string(current) != string(original) {
		return Result{}, refuse(log, dlog.Logger.Info, ArmChangedDuringRewrite,
			fmt.Sprintf("%s changed on disk while the rewrite ran", path), nil)
	}
	if err := u.files.Write(path, []byte(rewritten)); err != nil {
		return Result{}, fmt.Errorf("classifierupdate: write %s: %w", path, err)
	}
	sha, err := u.git.CommitPath(ctx, u.promptsDir, path, commitMessage(req.Instruction))
	if err != nil {
		if restoreErr := u.files.Write(path, original); restoreErr != nil {
			return Result{}, fmt.Errorf("classifierupdate: the commit of %s failed (%v) and putting the file back failed too: %w",
				path, err, restoreErr)
		}
		return Result{}, refuse(log, dlog.Logger.Error, ArmCommitFailed,
			fmt.Sprintf("git refused the commit, and %s was put back: %v", path, err), nil)
	}
	log.Info(op, "rewrote and committed the routing classifier's brief", dlog.Context{
		"commit":      sha,
		"instruction": req.Instruction,
	})
	return Result{Commit: sha, Path: path}, nil
}

// compose splices the rewrite brief.
func (u *Updater) compose(body string, req Request) (string, error) {
	if strings.Contains(body, slotOpen) || strings.Contains(body, slotClose) {
		return "", fmt.Errorf("classifierupdate: the routing brief carries %s or %s, which the rewrite reserves for its slots",
			slotOpen, slotClose)
	}
	brief, err := prompts.Load(u.promptsDir, BriefRewrite)
	if err != nil {
		return "", fmt.Errorf("classifierupdate: read the %s brief: %w", BriefRewrite, err)
	}
	question, err := brief.Splice(map[string]string{
		"current_prompt": toSlots(body),
		"instruction":    toSlots(req.Instruction),
		"example_text":   toSlots(req.Example.Text),
		"example_route":  routeWords(req.Example.Route),
	})
	if err != nil {
		return "", fmt.Errorf("classifierupdate: splice the %s brief: %w", BriefRewrite, err)
	}
	return question, nil
}

// splitBrief cuts a brief's content into its header line and the rest. The
// header is never shown to the model and is put back verbatim, so the
// declared placeholders can only be the ones the brief already declares.
func splitBrief(path, content string) (header, body string, err error) {
	if _, err := prompts.Parse(classifier.BriefRouting, path, content); err != nil {
		return "", "", fmt.Errorf("classifierupdate: the routing brief does not parse: %w", err)
	}
	header, body, _ = strings.Cut(content, "\n")
	return header, strings.TrimSuffix(body, "\n"), nil
}

// rebuild turns the model's answer back into a whole brief under HEADER, or
// answers why it is not a usable one.
func rebuild(header, answer string) (string, string) {
	text := strings.TrimSpace(answer)
	switch {
	case text == "":
		return "", "the rewrite answered nothing"
	case strings.HasPrefix(text, "```"):
		return "", "the rewrite answered inside a code fence rather than with the brief itself"
	}
	// A slot the model misspelled comes back as a malformed or undeclared
	// placeholder, which Parse refuses.
	content := header + "\n" + fromSlots(text) + "\n"
	if _, err := prompts.Parse(classifier.BriefRouting, "the rewrite", content); err != nil {
		return "", err.Error()
	}
	return content, ""
}

// toSlots shows `{{name}}` as `⟦name⟧`.
func toSlots(text string) string {
	return strings.NewReplacer("{{", slotOpen, "}}", slotClose).Replace(text)
}

// fromSlots turns `⟦name⟧` back into `{{name}}`.
func fromSlots(text string) string {
	return strings.NewReplacer(slotOpen, "{{", slotClose, "}}").Replace(text)
}

// routeWords says a verdict the way the rewrite brief shows it.
func routeWords(route classifier.Route) string {
	switch route {
	case classifier.RouteInterrupt:
		return "interrupt the running turn"
	case classifier.RouteAfterToolCall:
		return "join the running turn after its current tool call"
	case classifier.RouteQueue:
		return "wait for the running turn to end"
	default:
		panic(fmt.Sprintf("classifierupdate: route %d is not one of the three", int(route)))
	}
}

// commitMessage is the rewrite's commit message: the fixed subject, and the
// user's own words as the body so the history says why.
func commitMessage(instruction string) string {
	return CommitSubject + "\n\nRequested change: " + strings.TrimSpace(instruction) + "\n"
}

// refuse records one refusal at LEVEL and answers it.
func refuse(log dlog.Logger, level func(dlog.Logger, string, string, dlog.Context), arm, reason string, fields map[string]any) *Refusal {
	level(log, op, "refused a classifier update", dlog.Context{"arm": arm, "reason": reason})
	return &Refusal{Arm: arm, Reason: reason, Fields: fields}
}

// OnDisk is the production Files: a read, and a write that replaces the file
// in one rename so a reader never sees half a brief.
type OnDisk struct{}

// Read answers the file's whole content.
func (OnDisk) Read(path string) ([]byte, error) {
	return os.ReadFile(path)
}

// Write replaces path's content atomically, keeping its permissions.
func (OnDisk) Write(path string, content []byte) error {
	info, err := os.Stat(path)
	if err != nil {
		return err
	}
	return atomicfile.Replace(path, content, atomicfile.Options{Mode: info.Mode().Perm()})
}
