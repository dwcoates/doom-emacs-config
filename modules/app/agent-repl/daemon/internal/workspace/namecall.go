package workspace

import (
	"context"
	"fmt"
	"strconv"
	"strings"
	"time"

	"agent-repld/internal/dlog"
	"agent-repld/internal/headless"
)

// EVERY DYNAMICALLY CREATED WORKSPACE IS NAMED BY THE MODEL (owner ruling,
// 2026-09-12). A create that supplies no name gets one from a headless Haiku
// call the daemon makes inside Create, before the id is used for anything,
// before git is touched. There is no word-truncation fallback: the old `Slug`
// is deleted. A rejected answer is handed back to the model to SHORTEN, and
// once that retry is spent the last well-shaped answer is taken even over the
// limits (owner ruling, 2026-10-14): a slightly long name the model chose
// beats a refused create. Only a call that never produced a well-shaped
// answer REFUSES the create.
const (
	// BriefWorkspaceName is the naming question's brief, read from the prompts
	// directory AT USE TIME so an edit takes effect without a daemon bounce.
	BriefWorkspaceName = "workspace-name-from-prompt"
	// BriefWorkspaceNameShorten is the retry's brief: the rejected answer
	// handed back with what is wrong with it, to be rewritten. It is NOT the
	// naming question asked again.
	BriefWorkspaceNameShorten = "workspace-name-shorten"
	// NamingSite is the vendor-guard site name the naming call asks under,
	// beside "classifier" and "login".
	NamingSite = "workspace_naming"
	// NamingAttempts is how many times the call is made: the first, plus
	// EXACTLY ONE retry on an invalid or failed answer (owner ruling).
	NamingAttempts = 2
	// NamingTimeout bounds ONE attempt. It is a handful of tokens from a small
	// model; a call that has not answered in this long has failed. The bound
	// is a constant until a measurement of the real call justifies a flag.
	NamingTimeout = 15 * time.Second
	// NamingCauseInvalidAnswer is the naming refusal's cause when the model
	// answered but no answer had the slug's shape. Every other cause is one
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

// rejectedName is an answer the naming rule refused, which the retry hands
// back to the model to rewrite.
type rejectedName struct {
	answer string
	reason error
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
// The answer is VALIDATED, never repaired. A rejected answer is handed back
// to the model to shorten (BriefWorkspaceNameShorten); a call that did not
// answer at all is asked the naming question again. Once both attempts are
// spent, the LAST answer that had the slug's shape is taken even over the
// limits; only when no attempt produced one is it a namingFailure the caller
// turns into the create's refusal.
func (v *verbs) mintName(ctx context.Context, log dlog.Logger, repoDir, prompt, conversation string) (string, error) {
	if v.deps.Headless == nil {
		return "", &namingFailure{
			Cause:    headless.CauseNoBinary,
			Detail:   "this daemon was built with no headless vendor runner",
			Attempts: 0,
		}
	}

	configDir := ""
	if v.deps.Accounts != nil {
		configDir = v.deps.Accounts.ConfigDirFor(repoDir)
	}

	var last namingFailure
	var rejected *rejectedName
	// settle is the last answer of the slug's shape that broke only the
	// limits: what the call takes once its retry is spent.
	settle := ""
	for attempt := 1; attempt <= NamingAttempts; attempt++ {
		question, err := v.namingQuestion(log, prompt, conversation, rejected)
		if err != nil {
			return "", err
		}
		log.Info(opNaming, "issued the workspace naming call", dlog.Context{
			"model": headless.ModelHaiku, "repo_dir": repoDir, "config_dir": configDir, "attempt": attempt,
			"prompt_blank": strings.TrimSpace(prompt) == "", "conversation_chars": len(conversation),
			"shortening": rejected != nil,
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
			log.Info(opNaming, "the workspace naming call did not answer", dlog.Context{
				"cause": last.Cause, "detail": last.Detail, "attempt": attempt,
			})
			// A GUARD REFUSAL IS NOT RETRIED. It is a standing fact about this
			// process, so a second call would be refused identically and would
			// only cost the user another wait.
			if last.Cause == headless.CauseGuardRefused {
				break
			}
			// A call that never answered leaves nothing to shorten, so the
			// retry asks the naming question again. An EARLIER rejected
			// answer still stands as what the retry shortens.
			continue
		}

		answer := strings.TrimSpace(resp.Text)
		if err := ValidateSlug(answer); err != nil {
			last = namingFailure{
				Cause: NamingCauseInvalidAnswer, Detail: err.Error(),
				Attempts: uint32(attempt), Answer: answer,
			}
			if ValidateSlugShape(answer) == nil {
				settle = answer
			}
			log.Info(opNaming, "the naming answer did not validate", dlog.Context{
				"answer": answer, "reason": err.Error(), "attempt": attempt, "well_shaped": settle == answer,
			})
			rejected = &rejectedName{answer: answer, reason: err}
			continue
		}

		log.Info(opNaming, "the workspace naming call answered", dlog.Context{
			"model": headless.ModelHaiku, "duration_ms": resp.Duration.Milliseconds(),
			"name": answer, "attempt": attempt,
		})
		return answer, nil
	}

	if settle != "" {
		log.Info(opNaming, "took the last well-shaped naming answer over the naming limits", dlog.Context{
			"model": headless.ModelHaiku, "name": settle, "words": SlugWordCount(settle),
			"chars": len(settle), "attempts": last.Attempts,
		})
		return settle, nil
	}

	log.Error(opNaming, "the workspace naming call failed", dlog.Context{
		"model": headless.ModelHaiku, "cause": last.Cause,
		"attempts": last.Attempts, "answer": last.Answer,
	})
	failure := last
	return "", &failure
}

// namingQuestion composes one attempt's question: the naming brief spliced
// with the work when nothing has been rejected yet, and the shorten brief
// spliced with the rejected answer once something has.
//
// A MISSING BRIEF IS ITS OWN ARM. It is a deployment fault, not a model
// failure, and the client is owed the brief's name.
func (v *verbs) namingQuestion(log dlog.Logger, prompt, conversation string, rejected *rejectedName) (string, error) {
	name := BriefWorkspaceName
	values := map[string]string{"prompt": prompt, "conversation": conversation}
	if rejected != nil {
		name = BriefWorkspaceNameShorten
		values = map[string]string{
			"answer":  rejected.answer,
			"problem": rejected.reason.Error(),
			"limit":   strconv.Itoa(SlugWordLimit),
		}
	}
	brief, err := v.load(v.deps.PromptsDir, name)
	if err != nil {
		return "", refuseWith(log, "CreateWorkspace", ArmBriefMissing,
			fmt.Sprintf("the %s brief could not be read: %v", name, err), false,
			map[string]any{"name": name})
	}
	question, err := v.splice(brief, values)
	if err != nil {
		return "", fmt.Errorf("splice the %s brief: %w", name, err)
	}
	return question, nil
}

// freeName disambiguates a MINTED name against what already exists: an
// existing branch, a registered workspace of that name, a standing async
// merge's reservation or another allocation's claim takes the next "-2", "-3",
// … suffix until one is free (owner ruling, 2026-09-12). The chosen name is
// CLAIMED until the returned release is called (mergenames.go).
//
// The old system appended a random three-letter suffix to EVERY name so this
// could not arise; the roster reads these names, so the suffix is now paid
// only by the names that actually collide.
func (v *verbs) freeName(ctx context.Context, log dlog.Logger, repoDir, branch string) (string, func(), error) {
	v.names.mu.Lock()
	defer v.names.mu.Unlock()
	taken, err := v.takenLocked(ctx, "")
	if err != nil {
		return "", nil, err
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
			return "", nil, fmt.Errorf("probe the branch %q: %w", candidate, err)
		}
		if exists {
			continue
		}
		if suffix > 1 {
			log.Info(opNaming, "the minted workspace name collided and was disambiguated", dlog.Context{
				"minted": branch, "chosen": candidate, "suffix": suffix,
			})
		}
		return candidate, v.claimLocked(candidate), nil
	}
	return "", nil, fmt.Errorf("no free workspace name after %d attempts at %q", CollisionSuffixLimit, branch)
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
