package workspace

import (
	"context"
	"errors"
	"fmt"
	"strings"
	"unicode/utf8"

	"agent-repld/internal/dlog"
	"agent-repld/internal/headless"
	"agent-repld/internal/holdfold"
	"agent-repld/internal/ids"
	"agent-repld/internal/wsm"
)

// A CHAT THREAD'S TITLE (owner ruling, 2026-10-25): a two-to-five-word Title
// Case summary of its first question ("Fusion in the Sun"), never a slug. A
// thread is created at once under ThreadTitlePlaceholder, so the frame moves
// onto it before any model is asked anything, and is renamed when its title
// arrives (titleThread). A daemon that stopped before a title arrived resumes
// it at boot (ResumeThreadTitles): a thread whose name is the placeholder is
// a thread still owed its title.

const (
	// ThreadTitlePlaceholder is a thread's name while its title is generated.
	ThreadTitlePlaceholder = "Loading…"
	// UntitledThread is a thread's name when no title could be generated.
	UntitledThread = "Untitled chat"
	// BriefThreadTitle is the title question's brief, from the prompts corpus.
	BriefThreadTitle = "chat-title-from-question"
	// ThreadTitleMinWords and ThreadTitleMaxWords bound a title's words.
	ThreadTitleMinWords = 2
	ThreadTitleMaxWords = 5
	// ThreadTitleMaxChars bounds a title's length.
	ThreadTitleMaxChars = 48
	// opThreadTitle is the titling's log operation.
	opThreadTitle = "daemon.workspace.thread_title"
)

// ValidateThreadTitle answers ANSWER as a title: trimmed of surrounding
// quotes and a trailing full stop, two to five words, at most
// ThreadTitleMaxChars, one line.
func ValidateThreadTitle(answer string) (string, error) {
	title := strings.TrimSpace(answer)
	title = strings.Trim(title, "\"'`“”‘’")
	title = strings.TrimSuffix(strings.TrimSpace(title), ".")
	if strings.ContainsAny(title, "\n\r") {
		return "", errors.New("a title is one line")
	}
	words := strings.Fields(title)
	switch {
	case len(words) < ThreadTitleMinWords:
		return "", fmt.Errorf("a title is at least %d words, and %q is %d", ThreadTitleMinWords, title, len(words))
	case len(words) > ThreadTitleMaxWords:
		return "", fmt.Errorf("a title is at most %d words, and %q is %d", ThreadTitleMaxWords, title, len(words))
	}
	title = strings.Join(words, " ")
	if n := utf8.RuneCountInString(title); n > ThreadTitleMaxChars {
		return "", fmt.Errorf("a title is at most %d characters, and %q is %d", ThreadTitleMaxChars, title, n)
	}
	return title, nil
}

// mintThreadTitle asks the model for QUESTION's title, retrying an answer
// that is not one, up to NamingAttempts calls. A failure is answered as a
// namingFailure, recorded by the caller.
func (v *verbs) mintThreadTitle(ctx context.Context, log dlog.Logger, dir, question string) (string, error) {
	if v.deps.Headless == nil {
		return "", &namingFailure{Cause: headless.CauseNoBinary, Detail: "this daemon was built with no headless vendor runner"}
	}
	brief, err := v.load(v.deps.PromptsDir, BriefThreadTitle)
	if err != nil {
		return "", &namingFailure{Cause: ArmBriefMissing, Detail: fmt.Sprintf("the %s brief could not be read: %v", BriefThreadTitle, err)}
	}
	prompt, err := v.splice(brief, map[string]string{"question": question})
	if err != nil {
		return "", fmt.Errorf("splice the %s brief: %w", BriefThreadTitle, err)
	}
	configDir := ""
	if v.deps.Accounts != nil {
		configDir = v.deps.Accounts.ConfigDirFor(dir)
	}
	title, failure := headless.AskValidated(ctx, v.deps.Headless, headless.Ask{
		Request: headless.Request{
			Site: NamingSite, Model: headless.ModelHaiku, Format: headless.FormatJSON,
			ConfigDir: configDir, Prompt: prompt, Timeout: NamingTimeout,
		},
		Attempts: NamingAttempts,
		Validate: ValidateThreadTitle,
	}, log, opThreadTitle)
	if failure != nil {
		return "", &namingFailure{Cause: failure.Cause, Detail: failure.Detail, Attempts: failure.Attempts, Answer: failure.Answer}
	}
	return title, nil
}

// freeThreadNameLocked answers BASE, or BASE with the first " (2)", " (3)",
// … suffix no other workspace holds as its name or branch. Called under the
// naming lock, with SELF the thread being named (its own name is free to it).
func (v *verbs) freeThreadNameLocked(ctx context.Context, base string, self ids.WorkspaceID) (string, error) {
	held, err := v.holdersLocked(ctx, "")
	if err != nil {
		return "", err
	}
	name := base
	for n := 2; ; n++ {
		holder, taken := held[name]
		if !taken || (self != "" && holder.Workspace == self) {
			return name, nil
		}
		name = fmt.Sprintf("%s (%d)", base, n)
	}
}

// titleThread names RECORD after QUESTION once the model answers, or
// UntitledThread when it cannot (recorded at ERROR), and republishes every
// surface that draws the name. Run on its own goroutine (startTitling).
func (v *verbs) titleThread(ctx context.Context, log dlog.Logger, record wsm.Workspace, question string) {
	title, err := v.mintThreadTitle(ctx, log, record.Dir, question)
	if err != nil {
		log.Error(opThreadTitle, "the thread could not be titled; it is named untitled", dlog.Context{"cause": err.Error()})
		title = UntitledThread
	}
	v.names.mu.Lock()
	name, err := v.freeThreadNameLocked(ctx, title, record.ID)
	if err == nil {
		err = v.deps.DB.SetName(ctx, record.ID, name)
	}
	v.names.mu.Unlock()
	if err != nil {
		log.Error(opThreadTitle, "could not record the thread's title; it keeps its placeholder until the next boot resumes it", dlog.Context{"title": title, "cause": err.Error()})
		return
	}
	log.Info(opThreadTitle, "titled the thread", dlog.Context{"name": name})
	v.publishRename(ctx, log, record.ID, name)
}

// startTitling runs titleThread on its own goroutine, detached from CTX's
// cancellation: a title is owed whether or not the call that created the
// thread is still waiting.
func (v *verbs) startTitling(ctx context.Context, log dlog.Logger, record wsm.Workspace, question string) {
	detached := context.WithoutCancel(ctx)
	v.threadTitles.Add(1)
	go func() {
		defer v.threadTitles.Done()
		v.titleThread(detached, log, record, question)
	}()
}

// ResumeThreadTitles titles every thread a daemon stopped before titling:
// each whose name is still the placeholder, from its first question. Run at
// boot; the titles themselves arrive on their own goroutines.
func (v *verbs) ResumeThreadTitles(ctx context.Context) error {
	global := v.deps.Log.Global()
	records, err := v.deps.DB.ListWorkspaces(ctx)
	if err != nil {
		global.Error(opThreadTitle, "could not list the workspaces to resume the threads' titles", dlog.Context{"cause": err.Error()})
		return fmt.Errorf("resume thread titles: %w", err)
	}
	for _, record := range records {
		if !record.IsThread() || !strings.HasPrefix(record.Name, ThreadTitlePlaceholder) {
			continue
		}
		log := global.With(dlog.Context{"workspace": string(record.ID)})
		question, found, err := v.deps.DB.FirstThreadQuestion(ctx, record.ID)
		if err != nil {
			log.Error(opThreadTitle, "could not read the thread's first question to resume its title", dlog.Context{"cause": err.Error()})
			return fmt.Errorf("resume thread title %s: %w", record.ID, err)
		}
		if !found {
			held, err := v.deps.DB.HeldPrompts(ctx, record.ID)
			if err != nil {
				log.Error(opThreadTitle, "could not read the thread's held questions to resume its title", dlog.Context{"cause": err.Error()})
				return fmt.Errorf("resume thread title %s: %w", record.ID, err)
			}
			if len(held) > 0 {
				question, found = holdfold.SaidText(held[0].Said), true
			}
		}
		if !found {
			log.Info(opThreadTitle, "the thread holds no question to title it by; it is named untitled", nil)
		}
		log.Info(opThreadTitle, "resuming the title of a thread a stopped daemon left untitled", dlog.Context{"found_question": found})
		v.startTitling(ctx, log, record, question)
	}
	return nil
}
