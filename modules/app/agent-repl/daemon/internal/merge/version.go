package merge

import (
	"context"
	"fmt"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/semver"
	"agent-repld/internal/wsm"

	frontendv1 "agentrepl/proto/frontend/v1"
)

// This file is a deploy's VERSION INCREMENT
// (agentrepl.v1.MergeWorkspaceVersionIncrement): the check that a request
// carries one exactly where it must, and the annotated version tag the
// landing creates (docs/protobuf-design/merge-version-increment.md).
//
// THE VERSION IS COMPUTED WHEN THE MERGE LANDS, from the highest version tag
// reachable from the tip it lands on, so merges reordered in the queue still
// produce consecutive versions. The tag is created in the committing step
// itself, right after the target moves: a restart that finds the target on the
// recorded merge commit re-enters that step's landed path (resumeCommit),
// which creates the tag if it is still missing. A landed merge therefore never
// stays without the tag it asked for.

// The version refusals' arms (agentrepl.v1.FinishWorkspaceError).
const (
	// ArmVersionIncrementRequired is an agent's deploy onto a versioned
	// repository's main branch that carries no increment.
	ArmVersionIncrementRequired = "version_increment_required"
	// ArmVersionIncrementUnexpected is an increment on a deploy that may not
	// carry one. Its `reason` field names why.
	ArmVersionIncrementUnexpected = "version_increment_unexpected"
)

// The reasons an increment is unexpected, spelled as the
// agentrepl.v1.MergeWorkspaceVersionIncrementUnexpected.reason arms.
const (
	reasonUnversionedRepository = NestedArm("unversioned_repository")
	reasonNotDefaultBranch      = NestedArm("not_default_branch")
)

// NestedArm is a refusal field naming an arm of a oneof INSIDE the error arm
// (the unexpected increment's `reason`). The rpc layer sets that arm by name.
type NestedArm string

// requestDeploy reads where a request's deploy lands and its repository's
// deploy config, refusing a repository that states no usable deploy. Nothing
// is recorded before it answers.
func (o *orchestrator) requestDeploy(ctx context.Context, requester wsm.Workspace, req Request) (deployTarget, error) {
	target, err := o.requestTarget(ctx, requester, req)
	if err != nil {
		return deployTarget{}, err
	}
	return o.deployTargetFor(ctx, requester.ID, target)
}

// checkVersion refuses a request whose increment does not match where it
// lands (merge_version_increment.proto): none on an agent's deploy onto a
// versioned repository's main branch, or one on any deploy that is not that.
// The user's own ask may land on main without one.
func checkVersion(ws ids.WorkspaceID, req Request, d deployTarget) error {
	version := req.Source.Version
	if version == nil && req.By == RequestedByUser {
		return nil
	}
	versioned := d.localMerge().GetVersionTag() != nil
	switch {
	case version == nil && versioned && d.toMain:
		return refuse(ArmVersionIncrementRequired, ws, "a deploy onto the main branch of a versioned repository names how it moves the version (minor or patch) and its changelog line")
	case version != nil && !versioned:
		return unexpectedVersion(ws, reasonUnversionedRepository, "the repository's deploy config %s versions nothing", d.repoRoot)
	case version != nil && !d.toMain:
		return unexpectedVersion(ws, reasonNotDefaultBranch, "the deploy lands in the parent workspace's checkout, not on the repository's main branch")
	}
	return nil
}

// requestTarget answers where a request's merge will land, by the rule the
// run resolves its subject by (resolveSubject): a workspace's own branch and
// an async request (which lands by its requester's geometry) in the target
// recorded at creation (the main worktree for the main checkout, which has no
// creation), another workspace's in that workspace's, and a named branch run
// in place in the repository's main worktree.
//
// An async request lands where detachedLanding says.
func (o *orchestrator) requestTarget(ctx context.Context, requester wsm.Workspace, req Request) (string, error) {
	switch {
	case req.Source.Kind == wsm.MergeSourceBranch && req.Detached == nil:
		return o.deps.Git.MainWorktree(ctx, requester.Dir)
	case req.Detached != nil:
		target, _, err := o.detachedLanding(ctx, requester, wsm.MergeSource{DetachMaker: req.Detached.Maker, DetachTargetMaker: req.Detached.IntoMaker})
		return target, err
	case req.Source.Kind == wsm.MergeSourceWorkspace:
		job, err := o.layoutFor(ctx, req.Source.Workspace)
		if err != nil {
			return "", err
		}
		return job.Layout.TargetDir, nil
	}
	job, err := o.layoutFor(ctx, requester.ID)
	if err != nil {
		return "", err
	}
	return job.Layout.TargetDir, nil
}

// unexpectedVersion is the unexpected-increment refusal with its reason arm.
func unexpectedVersion(ws ids.WorkspaceID, reason NestedArm, format string, args ...any) *RefusalError {
	refusal := refuse(ArmVersionIncrementUnexpected, ws, format, args...)
	refusal.Fields = map[string]any{"reason": reason}
	return refusal
}

// landedVersion is the version a landing tagged and the part it moved.
type landedVersion struct {
	tag  string
	part semver.Part
}

// tagVersion creates the landing's version tag on commit, the merge commit
// the target now carries, counting from the highest version reachable from
// base, the tip it landed on. It answers nil when the merge carries no
// increment.
//
// A FAULT HERE NEVER UN-LANDS THE MERGE: the target has already moved. A tag
// that cannot be made is recorded at ERROR with what the step knew, and the
// landing is drawn without a version.
//
// THE DAEMON'S EXIT IS NO FAULT, and it is the one error answered: the step
// must stop BEFORE its landing is recorded, so the next boot resumes it at
// the committing step, finds the target on the merge commit, and tags it then.
func (r *run) tagVersion(ctx context.Context, commit, base string) (*landedVersion, error) {
	const op = "daemon.merge.version"
	increment := r.source.Version
	if increment == nil {
		return nil, nil
	}
	target := r.subject.targetDir
	fields := dlog.Context{"workspace": string(r.ws), "target": target, "commit": commit, "base": base, "part": increment.Part.String()}
	log := r.o.log(ctx, r.ws)
	fault := func(message string, err error) (*landedVersion, error) {
		if err != nil && r.stopping(err) {
			log.Info(op, "the daemon is exiting before the landed merge was tagged; the next boot tags it", withField(fields, "error", err.Error()))
			return nil, err
		}
		if err != nil {
			fields["error"] = err.Error()
		}
		log.Error(op, message, fields)
		return nil, nil
	}
	// THE STEP FINISHED BEFORE A RESTART when the merge commit already carries
	// a version tag: only the queue tags a merge commit.
	at, err := r.git.TagsAt(ctx, target, commit)
	if err != nil {
		return fault("could not read the tags on the landed merge commit; it is drawn without a version", err)
	}
	if v, ok := semver.Highest(at); ok {
		log.Info(op, "the landed merge commit already carries its version tag", withField(fields, "version", v.String()))
		return &landedVersion{tag: v.String(), part: increment.Part}, nil
	}
	reachable, err := r.git.TagsMerged(ctx, target, base)
	if err != nil {
		return fault("could not read the version tags reachable from the tip the merge landed on; it is drawn without a version", err)
	}
	previous, ok := semver.Highest(reachable)
	if !ok {
		return fault("no version tag is reachable from the tip the merge landed on, so there is no version to increment; tag master by hand", nil)
	}
	next := previous.Next(increment.Part)
	fields["previous"], fields["version"] = previous.String(), next.String()
	if err := r.git.CreateAnnotatedTag(ctx, target, next.String(), commit, increment.ChangelogLine); err != nil {
		return fault("could not create the landed merge's version tag; it is drawn without a version", err)
	}
	log.Info(op, "tagged the landed merge with its version", fields)
	return &landedVersion{tag: next.String(), part: increment.Part}, nil
}

// headVersion is the bubble head's version element for a landed version.
func headVersion(v *landedVersion) *frontendv1.FeedMergeVersion {
	if v == nil {
		return nil
	}
	parsed, ok := semver.Parse(v.tag)
	if !ok {
		panic(fmt.Sprintf("merge: the landed version %q is not a version tag", v.tag))
	}
	leading, incremented, trailing := parsed.Split(v.part)
	out := &frontendv1.FeedMergeVersion{
		Leading:     &frontendv1.FeedMergeVersionLeading{Text: leading},
		Incremented: &frontendv1.FeedMergeVersionIncremented{Text: incremented},
	}
	if trailing != "" {
		out.Trailing = &frontendv1.FeedMergeVersionTrailing{Text: trailing}
	}
	return out
}

// versionDoc and landedVersion carry a landed version through the record.
type versionDoc struct {
	Tag  string `json:"tag"`
	Part string `json:"part"`
}

func toVersionDoc(v *landedVersion) *versionDoc {
	if v == nil {
		return nil
	}
	return &versionDoc{Tag: v.tag, Part: v.part.String()}
}

func (d *versionDoc) landed() (*landedVersion, error) {
	if d == nil {
		return nil, nil
	}
	part, err := semver.ParsePart(d.Part)
	if err != nil {
		return nil, err
	}
	if _, ok := semver.Parse(d.Tag); !ok {
		return nil, fmt.Errorf("merge: the recorded landed version %q is not a version tag", d.Tag)
	}
	return &landedVersion{tag: d.Tag, part: part}, nil
}
