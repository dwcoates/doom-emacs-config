package workspace

import (
	"context"
	"fmt"
	"strconv"
	"strings"
	"time"

	"claude-repld/internal/dlog"
	"claude-repld/internal/headless"
)

// EVERY DYNAMICALLY CREATED WORKSPACE IS NAMED BY THE MODEL (owner ruling,
// 2026-09-12). A create that supplies no name gets one from a headless Haiku
// call the daemon makes inside Create, before the id is used for anything,
// before git is touched. There is no word-truncation fallback: the old `Slug`
// is deleted, and a naming call that cannot answer REFUSES the create.
const (
	// BriefWorkspaceName is the naming question's brief, read from the prompts
	// directory AT USE TIME so an edit takes effect without a daemon bounce.
	BriefWorkspaceName = "workspace-name-from-prompt"
	// NamingSite is the vendor-guard site name the naming call asks under,
	// beside "classifier" and "login".
	NamingSite = "workspace_naming"
	// NamingAttempts is how many times the call is made before it refuses:
	// the first, plus EXACTLY ONE retry on an invalid or failed answer (owner
	// ruling). One retry covers the common failure — the model wrapped the
	// name in a sentence — without doubling the worst-case wait more than
	// once.
	NamingAttempts = 2
	// NamingTimeout bounds ONE attempt. It is a handful of tokens from a small
	// model; a call that has not answered in this long has failed. The bound
	// is a constant until a measurement of the real call justifies a flag.
	NamingTimeout = 15 * time.Second
	// NamingCauseInvalidAnswer is the naming refusal's cause when the model
	// answered but no answer obeyed the naming rule. Every other cause is one
	// of headless's own.
	NamingCauseInvalidAnswer = "invalid_answer"
	// CollisionSuffixLimit bounds the disambiguating -2/-3/... walk. A
	// repository with this many workspaces of one name is a fault to surface,
	// not a loop to keep spinning.
	CollisionSuffixLimit = 100
)

// opNaming is the operation the naming call's records carry.
const opNaming = "daemon.workspace.naming"

// namingFailure is a naming call that could not answer, carrying exactly what
// the refusal arm spells.
type namingFailure struct {
	// Cause is one of headless's Cause* tokens or NamingCauseInvalidAnswer.
	Cause string
	// Detail is the failure's own account, for the sentence and the log.
	Detail string
	// Attempts is how many calls were made.
	Attempts uint32
	// Answer is the LAST answer the model gave, empty when it never answered.
	Answer string
}

func (f *namingFailure) Error() string {
	return fmt.Sprintf("the workspace naming call failed after %d attempts (%s): %s", f.Attempts, f.Cause, f.Detail)
}

// mintName asks the model for the workspace's name.
//
// The prompt it names from is the RAW commission the user typed — never a
// decorated or directive-appended form, which is composed later, at submission,
// and would name the workspace after the system's own words.
//
// CONVERSATION is the summary of the conversation a FORK continues (owner
// ruling, 2026-09-27), empty for every create that starts fresh. It rides the
// SAME call, brief and validation as the prompt: a fork is not named by a
// separate mechanism, only with more to go on, and a fork whose prompt is
// blank is named from the conversation alone.
//
// The answer is VALIDATED, never repaired. An invalid or failed answer is
// tried exactly once more; a second failure is a namingFailure the caller
// turns into the create's refusal.
func (v *verbs) mintName(ctx context.Context, log dlog.Logger, repoDir, prompt, conversation string) (string, error) {
	if v.deps.Headless == nil {
		return "", &namingFailure{
			Cause:    headless.CauseNoBinary,
			Detail:   "this daemon was built with no headless vendor runner",
			Attempts: 0,
		}
	}
	brief, err := v.load(v.deps.PromptsDir, BriefWorkspaceName)
	if err != nil {
		// A MISSING BRIEF IS ITS OWN ARM. It is a deployment fault, not a
		// model failure, and the client is owed the brief's name.
		return "", refuseWith(log, "CreateWorkspace", ArmBriefMissing,
			fmt.Sprintf("the %s brief could not be read: %v", BriefWorkspaceName, err), false,
			map[string]any{"name": BriefWorkspaceName})
	}

	configDir := ""
	if v.deps.Accounts != nil {
		configDir = v.deps.Accounts.ConfigDirFor(repoDir)
	}

	var last namingFailure
	correction := ""
	for attempt := 1; attempt <= NamingAttempts; attempt++ {
		question, err := v.splice(brief, map[string]string{
			"prompt": prompt, "conversation": conversation, "correction": correction,
		})
		if err != nil {
			return "", fmt.Errorf("splice the %s brief: %w", BriefWorkspaceName, err)
		}
		log.Info(opNaming, "issued the workspace naming call", dlog.Context{
			"model": headless.ModelHaiku, "repo_dir": repoDir, "config_dir": configDir, "attempt": attempt,
			"prompt_blank": strings.TrimSpace(prompt) == "", "conversation_chars": len(conversation),
		})
		log.Debug(opNaming, "the naming prompt", dlog.Context{"prompt": question, "attempt": attempt})

		resp, err := v.deps.Headless.Run(ctx, headless.Request{
			Site:      NamingSite,
			Model:     headless.ModelHaiku,
			Format:    headless.FormatJSON,
			ConfigDir: configDir,
			Prompt:    question,
			Timeout:   NamingTimeout,
		})
		if err != nil {
			last = namingFailure{Cause: headless.CauseOf(err), Detail: err.Error(), Attempts: uint32(attempt)}
			log.Debug(opNaming, "the workspace naming call did not answer", dlog.Context{
				"cause": last.Cause, "detail": last.Detail, "attempt": attempt,
			})
			// A GUARD REFUSAL IS NOT RETRIED. It is a standing fact about this
			// process, so a second call would be refused identically and would
			// only cost the user another wait.
			if last.Cause == headless.CauseGuardRefused {
				break
			}
			continue
		}

		answer := strings.TrimSpace(resp.Text)
		if err := ValidateSlug(answer); err != nil {
			last = namingFailure{
				Cause: NamingCauseInvalidAnswer, Detail: err.Error(),
				Attempts: uint32(attempt), Answer: answer,
			}
			log.Debug(opNaming, "the naming answer did not validate", dlog.Context{
				"answer": answer, "reason": err.Error(), "attempt": attempt,
			})
			correction = namingCorrection(answer, err)
			continue
		}

		log.Info(opNaming, "the workspace naming call answered", dlog.Context{
			"model": headless.ModelHaiku, "duration_ms": resp.Duration.Milliseconds(),
			"name": answer, "attempt": attempt,
		})
		return answer, nil
	}

	log.Error(opNaming, "the workspace naming call failed", dlog.Context{
		"model": headless.ModelHaiku, "cause": last.Cause,
		"attempts": last.Attempts, "answer": last.Answer,
	})
	failure := last
	return "", &failure
}

// namingCorrection is the retry's correction: the rejected answer quoted
// back, why it was rejected, the word count it had, and the word limit, both
// read off ValidateSlug's own rule (SlugWordCount, SlugWordLimit) so the
// sentence can never state a limit the validator does not enforce.
// MEASURED 2026-09-28: a four-word answer ("agent-repl-input-shorter") drew a
// correction that never said how many words were allowed, and the retry
// failed the same way.
func namingCorrection(answer string, reason error) string {
	return fmt.Sprintf(
		"Your previous answer was %q, which is not acceptable: %s. It has %d words; "+
			"the name must be AT MOST %d words. Answer with the bare name alone.",
		answer, reason.Error(), SlugWordCount(answer), SlugWordLimit)
}

// freeName disambiguates a MINTED name against what already exists: an
// existing branch, or a registered workspace of that name, takes the next
// "-2", "-3", … suffix until one is free (owner ruling, 2026-09-12).
//
// The old system appended a random three-letter suffix to EVERY name so this
// could not arise; the roster reads these names, so the suffix is now paid
// only by the names that actually collide.
func (v *verbs) freeName(ctx context.Context, log dlog.Logger, repoDir, branch string) (string, error) {
	taken, err := v.takenNames(ctx)
	if err != nil {
		return "", err
	}
	for suffix := 1; suffix <= CollisionSuffixLimit; suffix++ {
		candidate := branch
		if suffix > 1 {
			candidate = branch + "-" + strconv.Itoa(suffix)
		}
		if taken[candidate] {
			continue
		}
		// A BRANCH IS A FACT ABOUT THE REPOSITORY, not about the registry: a
		// stale branch left by a nuked workspace collides just as hard.
		exists, err := v.deps.Git.BranchExists(ctx, repoDir, candidate)
		if err != nil {
			return "", fmt.Errorf("probe the branch %q: %w", candidate, err)
		}
		if exists {
			continue
		}
		if suffix > 1 {
			log.Info(opNaming, "the minted workspace name collided and was disambiguated", dlog.Context{
				"minted": branch, "chosen": candidate, "suffix": suffix,
			})
		}
		return candidate, nil
	}
	return "", fmt.Errorf("no free workspace name after %d attempts at %q", CollisionSuffixLimit, branch)
}

// takenNames answers the workspace names the registry already holds.
func (v *verbs) takenNames(ctx context.Context) (map[string]bool, error) {
	records, err := v.deps.DB.ListWorkspaces(ctx)
	if err != nil {
		return nil, fmt.Errorf("list the registered workspaces: %w", err)
	}
	taken := make(map[string]bool, len(records))
	for _, record := range records {
		taken[record.Name] = true
	}
	return taken, nil
}
