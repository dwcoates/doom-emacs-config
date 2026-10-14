package commandfile

import (
	"context"
	"errors"
	"fmt"
	"strings"

	"agent-repld/internal/dirpath"
	"agent-repld/internal/ids"
	"agent-repld/internal/workspace"
	"agent-repld/internal/wsm"
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
	spec.MergeActions = entry.MergeActions()
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
	if repo, found := wsm.RepositoryAt(repositories, canonical); found {
		return repo, true, nil
	}
	ws, err := i.deps.DB.WorkspaceByDir(ctx, dir)
	if errors.Is(err, wsm.ErrNotFound) {
		return wsm.Repository{}, false, nil
	}
	if err != nil {
		return wsm.Repository{}, false, fmt.Errorf("git_root: look up the workspace at %q: %w", dir, err)
	}
	repo, err := workspaceRepository(repositories, ws, dir)
	if err != nil {
		return wsm.Repository{}, false, fmt.Errorf("git_root: %w", err)
	}
	return repo, true, nil
}

// workspaceRepository answers the registered repository a workspace, reached
// at dir, names. The workspace row's repository is a foreign key, so a
// registry that does not hold it is refused, never defaulted.
func workspaceRepository(repositories []wsm.Repository, ws wsm.Workspace, dir string) (wsm.Repository, error) {
	if repo, found := wsm.RepositoryWithID(repositories, ws.Repo); found {
		return repo, nil
	}
	return wsm.Repository{}, fmt.Errorf("workspace %q at %q names repository %q, which the registry does not hold", ws.ID, dir, ws.Repo)
}

// sourceParent answers the parent a create's `source_ws` names: none for no
// source or for the repository's own main checkout, else the registered
// workspace at that path, which must belong to the create's repository.
func (i *ingress) sourceParent(ctx context.Context, source *SourceWorkspace, repo wsm.Repository) (*ids.WorkspaceID, error) {
	if source == nil {
		return nil, nil
	}
	ws, main, err := i.parentAt(ctx, source.Path, repo)
	var unregistered *unregisteredParentError
	if errors.As(err, &unregistered) {
		return nil, fmt.Errorf("source_ws.path %w", err)
	}
	if err != nil {
		return nil, fmt.Errorf("source_ws.path: %w", err)
	}
	if main {
		return nil, nil
	}
	if ws.Repo != repo.ID {
		return nil, fmt.Errorf("source_ws.path %q is workspace %q of repository %q, not of the create's repository %q", source.Path, ws.ID, ws.Repo, repo.ID)
	}
	return &ws.ID, nil
}

// parentAt answers what a parent directory names within repo: its main
// checkout (main is true, and no workspace), or the registered workspace whose
// worktree it is. A directory that is neither is an unregisteredParentError.
// The create's source_ws and the retarget's parent are both spelled this way,
// and both read the main checkout as "no parent".
func (i *ingress) parentAt(ctx context.Context, dir string, repo wsm.Repository) (ws wsm.Workspace, main bool, err error) {
	canonical, err := dirpath.Canonical(dir)
	if err != nil {
		return wsm.Workspace{}, false, err
	}
	if canonical == repo.Dir {
		return wsm.Workspace{}, true, nil
	}
	ws, err = i.deps.DB.WorkspaceByDir(ctx, dir)
	if err != nil {
		return wsm.Workspace{}, false, &unregisteredParentError{dir: dir, repoDir: repo.Dir, err: err}
	}
	return ws, false, nil
}

// unregisteredParentError is a parent directory that is neither the
// repository's main checkout nor a registered workspace.
type unregisteredParentError struct {
	dir, repoDir string
	err          error
}

func (e *unregisteredParentError) Error() string {
	return fmt.Sprintf("%q is neither repository %q's main checkout nor a registered workspace: %v", e.dir, e.repoDir, e.err)
}

func (e *unregisteredParentError) Unwrap() error { return e.err }

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
