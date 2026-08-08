package sessioncontroller

import (
	"bytes"
	"context"
	"fmt"
	"os"
	"os/exec"
	"strings"
	"time"

	frontendv1 "agentrepl/proto/agentshim/frontend/v1"

	"claude-repld/internal/prompts"
	"claude-repld/internal/vendorguard"
)

// ClassifierPromptFile is the prompt file the classification brief is read
// from.
const ClassifierPromptFile = "queue-routing-classifier.md"

// unknownRunningTurn stands in for a running turn whose text the daemon never
// saw. It is a statement of FACT about the daemon's own knowledge rather than
// instruction prose, so it stays in code while the brief around it is editable.
const unknownRunningTurn = "(unknown — the running turn began before this daemon was watching)"

// The answer contract. The classifier must reply with EXACTLY ONE of these
// static tokens and nothing else that matters. They are deliberately ugly and
// unlikely to occur in prose, so the extraction below can be an ANCHORED exact
// match rather than a fuzzy read of the model's intent.
const (
	tokenJump = "<<QUEUE-JUMP>>"
	tokenHold = "<<QUEUE-HOLD>>"
)

const (
	// classifyTimeout bounds one classification. Overrunning it is a failure,
	// not a verdict: a queued prompt whose classification never returns is
	// surfaced as ERROR and delivered by the ordinary turn-end drain.
	classifyTimeout = 20 * time.Second
	// defaultClassifierModel is the cheap model a classification pins.
	defaultClassifierModel = "haiku"
	// classifierStderrCap bounds the child's captured stderr in a failure
	// message.
	classifierStderrCap = 4096
)

// CLIClassifier is the real classifier: a headless one-shot `claude -p` run
// pinned to a cheap model.
//
// It is constructed in main and injected into the Manager as a Classifier, so
// the queue logic itself never knows a subprocess exists and every queue test
// runs against a deterministic fake.
type CLIClassifier struct {
	// Model is the model to pin; empty pins defaultClassifierModel.
	Model string
	// Logf is the daemon logger. It is required so classification outcomes and
	// errors enter the daemon's canonical log.
	Logf func(string, ...any)
	// run executes the classification and returns stdout/stderr. Injected only
	// by tests, so the parsing and the token contract are testable without
	// spawning anything.
	run func(ctx context.Context, model, configDir, prompt string) (stdout, stderr []byte, err error)
}

var _ Classifier = (*CLIClassifier)(nil)

// NewCLIClassifier builds the subprocess-backed classifier.
func NewCLIClassifier(model string, logf func(string, ...any)) *CLIClassifier {
	if model == "" {
		model = defaultClassifierModel
	}
	if logf == nil {
		panic("session-controller: CLIClassifier Logf is required")
	}
	return &CLIClassifier{Model: model, Logf: logf, run: spawnClassifier}
}

// Classify implements Classifier.
//
// There is no fail-safe verdict here on purpose. If the run fails, times out,
// or answers something that is not exactly one token, this returns an ERROR —
// it never picks HOLD "to be safe". A silent default would be indistinguishable
// from a real HOLD verdict in the UI, and the whole point of the ERROR state is
// that the user can see nothing actually decided their prompt.
func (c *CLIClassifier) Classify(ctx context.Context, req ClassifyRequest) (ClassifyResult, error) {
	if c == nil || c.Logf == nil {
		panic("session-controller: CLIClassifier Logf is required")
	}
	runCtx, cancel := context.WithTimeout(ctx, classifyTimeout)
	defer cancel()

	model := c.Model
	if model == "" {
		model = defaultClassifierModel
	}
	prompt, err := ClassifierPrompt(req)
	if err != nil {
		return ClassifyResult{}, fmt.Errorf("classifier prompt unavailable: %w", err)
	}
	stdout, stderr, err := c.run(runCtx, model, req.ConfigDir, prompt)
	if err != nil {
		return ClassifyResult{}, fmt.Errorf("classifier run failed: %w (stderr: %s)", err, clampBytes(stderr, classifierStderrCap))
	}
	cls, err := ExtractVerdict(string(stdout))
	if err != nil {
		return ClassifyResult{}, err
	}
	c.logf("session-controller: classifier model=%s config_dir=%s verdict=%s", model, req.ConfigDir, cls)
	return ClassifyResult{Classification: cls, Rationale: rationaleFor(cls)}, nil
}

// logf logs through Logf. A direct struct literal is invalid without the
// required logger dependency, so reject it loudly instead of losing evidence.
func (c *CLIClassifier) logf(format string, args ...any) {
	if c.Logf == nil {
		panic("session-controller: CLIClassifier Logf is required")
	}
	c.Logf(format, args...)
}

// rationaleFor states, in the frontend's words, what a verdict means. The
// classifier is asked for a token and NOTHING else, so there is no model prose
// to quote — a free-text reason would be a second thing to get wrong, and the
// token already carries the entire decision.
func rationaleFor(cls frontendv1.QueueClassification) string {
	switch cls {
	case frontendv1.QueueClassification_QUEUE_CLASSIFICATION_INTERJECT:
		return "bears on the turn already running"
	case frontendv1.QueueClassification_QUEUE_CLASSIFICATION_HOLD:
		return "independent of the turn already running"
	default:
		return ""
	}
}

// ExtractVerdict reads the verdict off a classifier's raw stdout.
//
// The match is LINE-ANCHORED and EXACT: a token counts only when it is the
// ENTIRE content of some line, ignoring that line's surrounding whitespace.
// The classifier is asked for one token and nothing else, so a token embedded
// in prose ("I would answer <<QUEUE-JUMP>> here, because…") is the model
// explaining itself rather than answering, and is deliberately NOT a verdict —
// a substring match would read a hedged narration as a decision.
//
// Exactly one token must be present:
//
//   - Neither token: the model did not answer the question it was asked. That
//     is a failure, not a HOLD. Prose that merely mentions a token is this
//     case, not a verdict.
//   - BOTH tokens: the model contradicted itself, or is quoting the prompt back.
//     Believing either one would be picking a verdict at random.
//
// Both cases return an error, which the queue surfaces as the ERROR
// classification. Nothing here guesses.
func ExtractVerdict(out string) (frontendv1.QueueClassification, error) {
	var hasJump, hasHold bool
	for _, line := range strings.Split(out, "\n") {
		switch strings.TrimSpace(line) {
		case tokenJump:
			hasJump = true
		case tokenHold:
			hasHold = true
		}
	}
	switch {
	case hasJump && hasHold:
		return frontendv1.QueueClassification_QUEUE_CLASSIFICATION_ERROR,
			fmt.Errorf("classifier answered with BOTH tokens, so neither can be believed: %q", clampString(out, 400))
	case hasJump:
		return frontendv1.QueueClassification_QUEUE_CLASSIFICATION_INTERJECT, nil
	case hasHold:
		return frontendv1.QueueClassification_QUEUE_CLASSIFICATION_HOLD, nil
	default:
		return frontendv1.QueueClassification_QUEUE_CLASSIFICATION_ERROR,
			fmt.Errorf("classifier answered with neither %s nor %s: %q", tokenJump, tokenHold, clampString(out, 400))
	}
}

// ClassifierPrompt builds the classification brief.
//
// Three things it does deliberately:
//
//   - The two prompts are fenced and declared DATA. A queued prompt is
//     arbitrary user text heading for a model, so it is exactly the place an
//     instruction-shaped string would otherwise be obeyed.
//   - It forbids tool use and any investigation. This is a routing decision on
//     the text as given, and a classifier that starts reading files is both
//     slow and able to affect the workspace it is supposed to be judging.
//   - It asks for ONE token and nothing else. The extraction is an exact match
//     on a static string, so there is no format to parse, no JSON to be
//     malformed, and no prose to interpret.
// It reads prompts/ClassifierPromptFile at every use, so an edit takes effect
// on the next classification. An error means no brief could be composed, and
// Classify surfaces it as a classification FAILURE rather than a verdict — a
// routing decision made without the brief that defines the two answers would
// be a coin flip dressed as a judgment.
func ClassifierPrompt(req ClassifyRequest) (string, error) {
	running := req.RunningPrompt
	if strings.TrimSpace(running) == "" {
		running = unknownRunningTurn
	}
	// The two answer tokens are SUBSTITUTED rather than written into the file.
	// They are wire contract, not prose: the answer is extracted by exact match
	// on these strings, so a customized file that reworded them would leave
	// every classification unparseable. Passing them in keeps the brief
	// editable while the contract stays owned by the code that parses it.
	return prompts.Render(ClassifierPromptFile, map[string]string{
		"token_jump":   tokenJump,
		"token_hold":   tokenHold,
		"running_turn": running,
		"new_message":  req.QueuedPrompt,
	})
}

// classifierArgv is the headless one-shot invocation. The prompt rides on
// stdin, not argv, so a long queued prompt cannot overflow the argument list.
func classifierArgv(model string) []string {
	return []string{"claude", "-p", "--model", model}
}

// classifierEnv is the child's environment: the daemon's own, with
// CLAUDE_CONFIG_DIR overridden to the session's account root so the
// classification runs under the same account as the session it is about.
//
// An EMPTY configDir returns nil, inheriting the daemon's environment
// unchanged — setting it to "" would name a config root literally called "".
func classifierEnv(configDir string) []string {
	if configDir == "" {
		return nil
	}
	base := os.Environ()
	env := make([]string, 0, len(base)+1)
	for _, kv := range base {
		if !strings.HasPrefix(kv, "CLAUDE_CONFIG_DIR=") {
			env = append(env, kv)
		}
	}
	return append(env, "CLAUDE_CONFIG_DIR="+configDir)
}

// spawnClassifier runs the real CLI.
//
// cwd is a neutral temp dir so the headless run's own session hooks cannot
// resolve to a registered workspace and feed the daemon events about a session
// that is really just a classification.
func spawnClassifier(ctx context.Context, model, configDir, prompt string) (stdout, stderr []byte, err error) {
	if err := vendorguard.Check("sessioncontroller.spawnClassifier"); err != nil {
		return nil, nil, err
	}
	argv := classifierArgv(model)
	cmd := exec.CommandContext(ctx, argv[0], argv[1:]...)
	cmd.Dir = os.TempDir()
	cmd.Stdin = strings.NewReader(prompt)
	cmd.Env = classifierEnv(configDir)
	var out, errBuf bytes.Buffer
	cmd.Stdout = &out
	cmd.Stderr = &errBuf
	if runErr := cmd.Run(); runErr != nil {
		return nil, errBuf.Bytes(), fmt.Errorf("claude classifier run: %w", runErr)
	}
	return out.Bytes(), errBuf.Bytes(), nil
}

func clampString(s string, n int) string {
	if len(s) <= n {
		return s
	}
	return s[:n] + "…"
}

func clampBytes(b []byte, n int) string { return clampString(string(b), n) }
