package commandfile

import (
	"context"
	"errors"
	"fmt"
	"strings"

	"claude-repld/internal/dirpath"
	"claude-repld/internal/ids"
	"claude-repld/internal/workspace"
	"claude-repld/internal/wsm"
)

// createSpec maps a create entry onto the CreateSpec the CreateWorkspace rpc
// would fill for the same request, so a file's create and a client's create
// are one create.
//
// THE REPOSITORY COMES FIRST. The skill's `git_root` defaults to the source
// workspace's own path, which for any workspace but the main checkout is a
// worktree and not the repository; the rpc is handed a repository, so the
// worktree is resolved to its repository here. A directory that is neither is
// passed on as written, and the creation verb refuses it as
// unknown_repository with its own record.
func (i *ingress) createSpec(ctx context.Context, entry Entry) (workspace.CreateSpec, error) {
	spec := workspace.CreateSpec{
		RepoDir:       entry.GitRoot,
		InitialPrompt: entry.Prompt,
		Name:          entry.Name,
		BaseRef:       entry.base(),
		OneShot:       entry.OneShot,
		Model:         strings.TrimSpace(entry.Model),
	}
	if priority, ok := priorities[entry.Priority]; ok {
		spec.Priority = &priority
	}
	if entry.BeforeWSMerge != "" {
		spec.MergeActions.Before = []string{entry.BeforeWSMerge}
	}
	if entry.PostprocessingPrompt != "" {
		spec.MergeActions.After = []string{entry.PostprocessingPrompt}
	}
	if entry.SourceWS == nil && entry.ForkFrom == "" {
		// Nothing below needs the registry: an entry that names neither
		// resolves exactly as it always did.
		return spec, nil
	}
	if i.deps.DB == nil {
		return workspace.CreateSpec{}, fmt.Errorf("this create names a source or fork workspace and the ingress has no state client to resolve it with")
	}
	repo, found, err := i.repositoryOf(ctx, entry.GitRoot)
	if err != nil {
		return workspace.CreateSpec{}, err
	}
	if !found {
		// The verb refuses the unregistered repository; no parent is
		// resolvable in a repository nothing registered.
		return spec, nil
	}
	spec.RepoDir = repo.Dir

	parent, err := i.sourceParent(ctx, entry.SourceWS, repo)
	if err != nil {
		return workspace.CreateSpec{}, err
	}
	if entry.ForkFrom != "" {
		fork, err := i.workspaceNamed(ctx, entry.ForkFrom, repo)
		if err != nil {
			return workspace.CreateSpec{}, err
		}
		// A FORK IS ALWAYS FROM THE PARENT (CreateWorkspaceParent.fork), so a
		// source naming a different workspace asks for two parents.
		if parent != nil && *parent != fork {
			return workspace.CreateSpec{}, fmt.Errorf("fork_from %q is workspace %q, but source_ws names workspace %q; a fork is always from the create's parent", entry.ForkFrom, fork, *parent)
		}
		spec.ForkFrom = &fork
		parent = &fork
	}
	spec.Parent = parent
	return spec, nil
}

// repositoryOf answers the registered repository dir names: the one whose main
// checkout it is, else the one holding the registered workspace whose worktree
// it is. found is false when it is neither.
func (i *ingress) repositoryOf(ctx context.Context, dir string) (wsm.Repository, bool, error) {
	canonical, err := dirpath.Canonical(dir)
	if err != nil {
		return wsm.Repository{}, false, fmt.Errorf("git_root: %w", err)
	}
	repositories, err := i.deps.DB.ListRepositories(ctx)
	if err != nil {
		return wsm.Repository{}, false, fmt.Errorf("read the repository registry: %w", err)
	}
	for _, repo := range repositories {
		if repo.Dir == canonical {
			return repo, true, nil
		}
	}
	ws, err := i.deps.DB.WorkspaceByDir(ctx, dir)
	if errors.Is(err, wsm.ErrNotFound) {
		return wsm.Repository{}, false, nil
	}
	if err != nil {
		return wsm.Repository{}, false, fmt.Errorf("git_root: look up the workspace at %q: %w", dir, err)
	}
	for _, repo := range repositories {
		if repo.ID == ws.Repo {
			return repo, true, nil
		}
	}
	return wsm.Repository{}, false, fmt.Errorf("git_root: workspace %q at %q names repository %q, which the registry does not hold", ws.ID, dir, ws.Repo)
}

// sourceParent answers the parent a create's `source_ws` names: none for no
// source or for the repository's own main checkout, else the registered
// workspace at that path, which must belong to the create's repository.
func (i *ingress) sourceParent(ctx context.Context, source *SourceWorkspace, repo wsm.Repository) (*ids.WorkspaceID, error) {
	if source == nil {
		return nil, nil
	}
	canonical, err := dirpath.Canonical(source.Path)
	if err != nil {
		return nil, fmt.Errorf("source_ws.path: %w", err)
	}
	if canonical == repo.Dir {
		return nil, nil
	}
	ws, err := i.deps.DB.WorkspaceByDir(ctx, source.Path)
	if err != nil {
		return nil, fmt.Errorf("source_ws.path %q is neither repository %q's main checkout nor a registered workspace: %w", source.Path, repo.Dir, err)
	}
	if ws.Repo != repo.ID {
		return nil, fmt.Errorf("source_ws.path %q is workspace %q of repository %q, not of the create's repository %q", source.Path, ws.ID, ws.Repo, repo.ID)
	}
	return &ws.ID, nil
}

// workspaceNamed answers the ONE open workspace of repo named name. None, or
// more than one, is refused: a fork of a guessed conversation is not a fork
// anybody asked for.
func (i *ingress) workspaceNamed(ctx context.Context, name string, repo wsm.Repository) (ids.WorkspaceID, error) {
	all, err := i.deps.DB.ListWorkspaces(ctx)
	if err != nil {
		return "", fmt.Errorf("read the workspace registry: %w", err)
	}
	var matches []ids.WorkspaceID
	for _, ws := range all {
		if ws.Repo == repo.ID && ws.Name == name && !ws.Closed {
			matches = append(matches, ws.ID)
		}
	}
	switch len(matches) {
	case 1:
		return matches[0], nil
	case 0:
		return "", fmt.Errorf("fork_from %q names no open workspace of repository %q", name, repo.ID)
	default:
		return "", fmt.Errorf("fork_from %q names %d open workspaces of repository %q: %v", name, len(matches), repo.ID, matches)
	}
}
