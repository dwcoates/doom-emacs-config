package server

import (
	"errors"

	frontendv1 "agentrepl/proto/agentshim/frontend/v1"

	"claude-repld/internal/registry"
	"claude-repld/internal/session"

	"google.golang.org/protobuf/proto"
)

// sessionResumeCommandError carries the continuity evidence from the owner
// that rejected a command. It preserves the original error for every caller
// that needs errors.As while giving errclass the exact wire detail.
type sessionResumeCommandError struct {
	err    error
	detail *frontendv1.SessionResumeFailure
}

func (e *sessionResumeCommandError) Error() string { return e.err.Error() }

func (e *sessionResumeCommandError) Unwrap() error { return e.err }

func (e *sessionResumeCommandError) SessionResumeFailureDetail() *frontendv1.SessionResumeFailure {
	return e.detail
}

// queryTerminationFailureDetailer exposes typed lifecycle evidence retained by
// a driveability gate that observed the resumed SDK query terminate.
type queryTerminationFailureDetailer interface {
	QueryTerminationFailureDetail() *frontendv1.QueryTerminationFailure
}

// sessionResumeCommandContext names the durable exact-resume operation that
// owns a command failure.
type sessionResumeCommandContext struct {
	agentReplSessionID string
	claudeSessionID    string
	cwd                string
	configDir          string
	resolvedConfigDir  string
	automaticRestore   bool
}

// resumeEstablishmentAuthority is the sole classifier for a command that
// establishes an exact Claude resume. Its constructors require the operation's
// authoritative identity before an error can be classified, so Open,
// OpenDriveable, and create cannot independently assemble wire evidence.
type resumeEstablishmentAuthority struct {
	context sessionResumeCommandContext
}

func automaticResumeEstablishment(rec registry.Record) resumeEstablishmentAuthority {
	return resumeEstablishmentAuthority{context: sessionResumeCommandContext{
		agentReplSessionID: rec.SessionID,
		claudeSessionID:    rec.ClaudeSessionID,
		cwd:                rec.CWD,
		configDir:          rec.ConfigDir,
		resolvedConfigDir:  session.ClaudeConfigDir(rec.ConfigDir),
		automaticRestore:   true,
	}}
}

// restartResumeEstablishment classifies a HARD RESTART's failure.
//
// A restart re-establishes the SAME conversation under a fresh process, so a
// transcript that has gone missing underneath it is a continuity failure of
// exactly the kind the create and open paths already classify — it was simply
// the one establishment path that never reached the classifier, and so reached
// the user as `internal.unclassified` carrying raw prose.
//
// The agent-repl session id is not available at this boundary: the restart
// command is keyed by WORKSPACE, and the handler holds no registry. It is left
// blank rather than guessed, and the identity that matters for this failure —
// the conversation, its cwd, and the searched config root — is filled in by
// classify from the ResumeTranscriptMissingError itself.
func restartResumeEstablishment() resumeEstablishmentAuthority {
	return resumeEstablishmentAuthority{context: sessionResumeCommandContext{automaticRestore: true}}
}

func createResumeEstablishment(opts CreateOpts, agentReplSessionID string) resumeEstablishmentAuthority {
	return resumeEstablishmentAuthority{context: sessionResumeCommandContext{
		agentReplSessionID: agentReplSessionID,
		claudeSessionID:    opts.Resume,
		cwd:                opts.CWD,
		configDir:          opts.ConfigDir,
		resolvedConfigDir:  session.ClaudeConfigDir(opts.ConfigDir),
	}}
}

// classify preserves an exact-resume failure's original chain and attaches the
// operation's complete wire evidence. Fresh creates and malformed create
// requests retain their established classifiers because neither attempted
// continuity. Reclassification is idempotent.
func (a resumeEstablishmentAuthority) classify(err error) error {
	if err == nil {
		return nil
	}
	var classified *sessionResumeCommandError
	if errors.As(err, &classified) {
		return err
	}
	var detailed interface {
		SessionResumeFailureDetail() *frontendv1.SessionResumeFailure
	}
	if errors.As(err, &detailed) && detailed.SessionResumeFailureDetail() != nil {
		return newSessionResumeCommandError(err, proto.Clone(detailed.SessionResumeFailureDetail()).(*frontendv1.SessionResumeFailure))
	}
	var missing *ResumeTranscriptMissingError
	if errors.As(err, &missing) {
		a.context.claudeSessionID = missing.ResumeID
		a.context.cwd = missing.CWD
		a.context.configDir = missing.ConfigDir
		a.context.resolvedConfigDir = missing.ResolvedConfigDir
		detail := sessionResumeFailureDetail(a.context)
		detail.Cause = &frontendv1.SessionResumeFailure_TranscriptUnavailable{
			TranscriptUnavailable: &frontendv1.SessionResumeFailureTranscriptUnavailable{
				SearchedPaths: append([]string(nil), missing.SearchedPaths...),
			},
		}
		return newSessionResumeCommandError(err, detail)
	}
	if a.context.claudeSessionID == "" {
		return err
	}
	var invalid *InvalidCreateError
	if errors.As(err, &invalid) {
		return err
	}
	var termination queryTerminationFailureDetailer
	if errors.As(err, &termination) && termination.QueryTerminationFailureDetail() != nil {
		detail := sessionResumeFailureDetail(a.context)
		detail.Cause = &frontendv1.SessionResumeFailure_QueryTermination{
			QueryTermination: proto.Clone(termination.QueryTerminationFailureDetail()).(*frontendv1.QueryTerminationFailure),
		}
		return newSessionResumeCommandError(err, detail)
	}
	detail := sessionResumeFailureDetail(a.context)
	detail.Cause = &frontendv1.SessionResumeFailure_BringUpFailure{
		BringUpFailure: &frontendv1.SessionResumeFailureBringUpFailure{Cause: err.Error()},
	}
	return newSessionResumeCommandError(err, detail)
}

func sessionResumeFailureDetail(context sessionResumeCommandContext) *frontendv1.SessionResumeFailure {
	// The agent-repl session id is deliberately absent: a rendering frontend
	// has no session vocabulary to read it with. What the card shows is the
	// VENDOR conversation and where it was looked for, all of which is content
	// and all of which is carried.
	detail := &frontendv1.SessionResumeFailure{
		ClaudeSessionId:   context.claudeSessionID,
		Cwd:               context.cwd,
		ConfigDir:         context.configDir,
		ResolvedConfigDir: context.resolvedConfigDir,
	}
	if context.automaticRestore {
		detail.Attempt = &frontendv1.SessionResumeFailure_AutomaticRestore{
			AutomaticRestore: &frontendv1.SessionResumeFailureAutomaticRestore{},
		}
	} else {
		detail.Attempt = &frontendv1.SessionResumeFailure_Create{
			Create: &frontendv1.SessionResumeFailureCreate{},
		}
	}
	return detail
}

func newSessionResumeCommandError(err error, detail *frontendv1.SessionResumeFailure) error {
	return &sessionResumeCommandError{err: err, detail: detail}
}
