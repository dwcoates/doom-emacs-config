// resumevanished.go answers the ONE question the resume-viability gate never
// asked: is this missing transcript worth trying again?
//
// THE DEFECT. A record naming a vendor conversation whose `.jsonl` has been
// deleted made every bring-up fail at the gate — correctly, because starting a
// blank conversation in its place would destroy the user's history. But the
// refusal carried no disposition, so the ensure/reattach machinery treated it
// as an ordinary bring-up failure and re-ran the identical doomed spawn on
// every open, every sweep, and every reattach: seven failures in four minutes,
// a startup sweep stalled behind them, and a log full of one fact.
//
// THE TWO SHAPES, told apart HERE and nowhere else:
//
//  1. THE WORKSPACE STILL HAS A CONVERSATION, under a different uuid. The
//     record's pointer is stale, not the history. Resuming the NEWEST sibling
//     transcript keeps the continuity commitment the pointer stood for far
//     better than wedging does, so it is CORRECTED with a loud log naming both
//     uuids.
//  2. THE WORKSPACE HAS NO TRANSCRIPT AT ALL. There is nothing to resume and
//     nothing to fall back to, and no number of retries will conjure one. This
//     is TERMINAL: it is classified with errclass.ErrResumeTargetVanished so
//     the bring-up machinery can fence the session on a single failure card
//     instead of rediscovering the same absence forever.
//
// NEITHER SHAPE WEAKENS THE REFUSAL. Shape 1 resumes a real conversation this
// workspace owns; shape 2 still refuses to start a fresh one. What changes is
// only how many times the daemon says so.
package server

import (
	"fmt"

	frontendv1 "agentrepl/proto/agentshim/frontend/v1"

	"claude-repld/internal/errclass"
	"claude-repld/internal/session"
)

// VanishedResumeTargetError is the terminal disposition of a missing resume
// target: the transcript is gone and the workspace has no sibling to fall back
// to.
//
// It WRAPS the gate's own error rather than replacing it, so every existing
// reader — errors.As for *ResumeTranscriptMissingError, the continuity
// classifier, the card renderer — keeps seeing exactly what it saw before. What
// it adds is the sentinel (terminality) and the wire detail (so a card can be
// published by a layer that has no resume vocabulary of its own).
type VanishedResumeTargetError struct {
	// Missing is the gate's verdict, unchanged.
	Missing *ResumeTranscriptMissingError
	// ProjectDir is the directory that was searched for siblings, named in the
	// card so a human can look where the daemon looked.
	ProjectDir string

	detail *frontendv1.SessionResumeFailure
}

func (e *VanishedResumeTargetError) Error() string {
	return fmt.Sprintf("%s; no other transcript exists for this workspace in %s, so nothing can be resumed and no retry can change that",
		e.Missing.Error(), e.ProjectDir)
}

func (e *VanishedResumeTargetError) Unwrap() error { return e.Missing }

// Is makes the terminal disposition matchable without matching prose.
func (e *VanishedResumeTargetError) Is(target error) bool {
	return target == errclass.ErrResumeTargetVanished
}

// SessionResumeFailureDetail carries the continuity evidence with the error, so
// the fencing layer can publish a session_resume_failed card without owning a
// classifier of its own.
func (e *VanishedResumeTargetError) SessionResumeFailureDetail() *frontendv1.SessionResumeFailure {
	return e.detail
}

// newVanishedResumeTargetError classifies a missing target as terminal, with
// the automatic-restore attempt shape every respawn-path continuity failure
// carries.
func newVanishedResumeTargetError(missing *ResumeTranscriptMissingError, projectDir string) *VanishedResumeTargetError {
	detail := sessionResumeFailureDetail(sessionResumeCommandContext{
		claudeSessionID:   missing.ResumeID,
		cwd:               missing.CWD,
		configDir:         missing.ConfigDir,
		resolvedConfigDir: missing.ResolvedConfigDir,
		automaticRestore:  true,
	})
	detail.Cause = &frontendv1.SessionResumeFailure_TranscriptUnavailable{
		TranscriptUnavailable: &frontendv1.SessionResumeFailureTranscriptUnavailable{
			SearchedPaths: append([]string(nil), missing.SearchedPaths...),
		},
	}
	return &VanishedResumeTargetError{Missing: missing, ProjectDir: projectDir, detail: detail}
}

// newestSiblingTranscript is the fallback candidate for a vanished target: the
// most recently modified transcript this workspace has under the record's OWN
// config root, other than the target itself.
//
// The record's own root and nothing else. A transcript under a DIFFERENT
// account root is not resumable by the CLI this session launches, and adopting
// one would resume a conversation the user never chose under an account they
// never chose (session.Discover makes the same distinction, and reports rather
// than adopts for the same reason).
//
// A read error is SURFACED, never read as "no sibling": treating an unreadable
// project dir as an empty one would fence a workspace whose history is intact.
func newestSiblingTranscript(opts CreateOpts) (session.Transcript, bool, error) {
	root := session.ClaudeConfigDir(opts.ConfigDir)
	newest, found, err := session.NewestTranscript(root, opts.CWD)
	if err != nil {
		return session.Transcript{}, false, fmt.Errorf("server: searching %s for a transcript to resume in place of vanished target %s: %w",
			session.ProjectDir(root, opts.CWD), opts.Resume, err)
	}
	if !found || newest.SessionID == opts.Resume {
		return session.Transcript{}, false, nil
	}
	return newest, true, nil
}
