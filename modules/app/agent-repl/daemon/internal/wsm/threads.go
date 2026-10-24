package wsm

import (
	"context"
	"database/sql"
	"errors"
	"fmt"
	"path/filepath"
	"strings"
	"time"

	zettelv1 "agentrepl/proto/zettel/v1"
	"google.golang.org/protobuf/proto"

	"agent-repld/internal/dlog"
)

// A CHAT THREAD IS A KIND OF WORKSPACE (zettel, docs/knowledge-graph/PLAN.md).
// It reuses the whole session lifecycle a workspace has -- a shim, its turns,
// interrupt, rollback, hibernation, its name and summary -- and has nothing a
// repository gives a workspace: no worktree, no branch, no merge, no deploy.
//
// THE WORKSPACES TABLE REQUIRES A REPOSITORY for every row (repo_id NOT NULL,
// owner ruling 2026-09-13), and eighteen tables reference it, so a thread is
// not modeled by rebuilding that table: threads belong to ONE repository row
// of kind `zettel`, the zettel home (~/zettel/threads), which is a container of
// threads and never a git repository. Two triggers make the pairing
// structural: a thread's repository is the zettel home, and nothing else lives
// there. Every repository feature skips the zettel home by its kind, and every
// workspace feature that needs a branch skips a thread by its kind.

// WorkspaceKind is what a workspace is.
type WorkspaceKind string

const (
	// WorkspaceKindRepository is an agent workspace: a git worktree of a
	// registered repository.
	WorkspaceKindRepository WorkspaceKind = "repository"
	// WorkspaceKindThread is a zettel chat thread, living in the zettel home.
	WorkspaceKindThread WorkspaceKind = "thread"
)

// RepositoryKind is what a repository row is.
type RepositoryKind string

const (
	// RepositoryKindGit is a git repository registered by its main worktree.
	RepositoryKindGit RepositoryKind = "git"
	// RepositoryKindZettel is the zettel home: the one container every thread
	// belongs to. It is not a git repository.
	RepositoryKindZettel RepositoryKind = "zettel"
)

// IsThread reports whether the workspace is a zettel chat thread.
func (w Workspace) IsThread() bool { return w.Kind == WorkspaceKindThread }

// IsZettelHome reports whether the repository is the zettel home.
func (r Repository) IsZettelHome() bool { return r.Kind == RepositoryKindZettel }

// threadsDDL is the layout-78 addition: the kinds, and the two triggers that
// keep threads and the zettel home paired.
//
// IT IS ADDITIVE: the previous build names no `kind` column, so every row it
// writes takes the defaults (a git repository, a repository workspace), which
// is exactly what that build ever writes.
//
// It also adds where a thread prompt's question was asked from: the held
// prompt carries it (thread_parent) from the moment it is sent, so every copy
// the queue makes of the hold carries it too, and the turn row it is delivered
// as carries the question whole (thread_question, thread_parent) until the
// answer is known and the exchange is recorded in zettel. The question's TEXT
// is taken from the prompt's words when the turn row is written, so an edit of
// the held prompt can never leave a stale copy of it.
const threadsDDL = `
ALTER TABLE repositories ADD COLUMN kind TEXT NOT NULL DEFAULT 'git' CHECK (kind IN ('git', 'zettel'));
ALTER TABLE workspaces ADD COLUMN kind TEXT NOT NULL DEFAULT 'repository' CHECK (kind IN ('repository', 'thread'));
ALTER TABLE held_prompts ADD COLUMN thread_parent BLOB;
ALTER TABLE turns ADD COLUMN thread_question TEXT;
ALTER TABLE turns ADD COLUMN thread_parent BLOB;

CREATE TRIGGER workspaces_thread_lives_in_the_zettel_home_insert
BEFORE INSERT ON workspaces
WHEN (NEW.kind = 'thread') != (COALESCE((SELECT kind FROM repositories WHERE id = NEW.repo_id), 'git') = 'zettel')
BEGIN
  SELECT RAISE(ABORT, 'wsm: a thread lives in the zettel home, and nothing else does');
END;

CREATE TRIGGER workspaces_thread_lives_in_the_zettel_home_update
BEFORE UPDATE OF kind, repo_id ON workspaces
WHEN (NEW.kind = 'thread') != (COALESCE((SELECT kind FROM repositories WHERE id = NEW.repo_id), 'git') = 'zettel')
BEGIN
  SELECT RAISE(ABORT, 'wsm: a thread lives in the zettel home, and nothing else does');
END;
`

// ErrThreadNameTaken refuses a thread whose directory another workspace
// already holds.
var ErrThreadNameTaken = errors.New("wsm: a workspace already holds this thread's directory")

// RegisterThread records a new chat thread whose directory is dir, inside the
// zettel home (dir's parent), minting the zettel home's repository row on first
// sight. Unlike RegisterWorkspace it is NOT idempotent: a thread is created
// once, by CreateThread, and a directory already registered is refused with
// ErrThreadNameTaken.
func (s *store) RegisterThread(ctx context.Context, dir, name string) (Workspace, error) {
	const op = "daemon.wsm.register_thread"
	normalized, err := normalizeDir(dir)
	if err != nil {
		s.log.Error(op, "refused a thread dir that cannot be normalized", withError(dlog.Context{"dir": dir}, err))
		return Workspace{}, err
	}
	home := filepath.Dir(normalized)
	fields := dlog.Context{"dir": normalized, "zettel_home": home, "name": name}
	var out Workspace
	err = s.write(ctx, op, fields, func(ctx context.Context, tx *sql.Tx) error {
		var existing string
		switch err := tx.QueryRowContext(ctx, `SELECT id FROM workspaces WHERE dir = ?`, normalized).Scan(&existing); {
		case err == nil:
			return ErrThreadNameTaken
		case !errors.Is(err, sql.ErrNoRows):
			return err
		}
		repo, err := ensureZettelHome(ctx, tx, home)
		if err != nil {
			return err
		}
		out = Workspace{ID: NewWorkspaceID(), Kind: WorkspaceKindThread, Repo: repo, Dir: normalized, Name: name, CreatedAt: time.Now().UTC()}
		out.Place, err = landingPlace(ctx, tx, nil, out.ID)
		if err != nil {
			return err
		}
		_, err = tx.ExecContext(ctx,
			`INSERT INTO workspaces (id, kind, repo_id, dir, name, branch, parent_branch, parent_id, closed, attention, priority, task_id, is_current, last_selected_at, merged_at, serving_instance, created_at, place)
			 VALUES (?, 'thread', ?, ?, ?, '', '', NULL, 0, 0, NULL, NULL, 0, NULL, NULL, NULL, ?, ?)`,
			out.ID, out.Repo, out.Dir, out.Name, nanos(out.CreatedAt), out.Place)
		return err
	})
	if errors.Is(err, ErrThreadNameTaken) {
		s.log.Info(op, "refused a thread whose directory is already registered", fields)
		return Workspace{}, err
	}
	if err != nil {
		return Workspace{}, err
	}
	s.log.Info(op, "registered a thread", dlog.Context{"workspace": string(out.ID), "dir": out.Dir, "name": out.Name})
	return out, nil
}

// ensureZettelHome answers the zettel home's repository row, minting it on
// first sight. A git repository already registered at the same directory is
// an invariant violation: the zettel home is never a repository.
func ensureZettelHome(ctx context.Context, tx *sql.Tx, home string) (RepoID, error) {
	var id RepoID
	var kind string
	switch err := tx.QueryRowContext(ctx, `SELECT id, kind FROM repositories WHERE dir = ?`, home).Scan(&id, &kind); {
	case err == nil:
		if RepositoryKind(kind) != RepositoryKindZettel {
			return "", fmt.Errorf("wsm: the zettel home %q is registered as a %s repository", home, kind)
		}
		return id, nil
	case !errors.Is(err, sql.ErrNoRows):
		return "", err
	}
	id = NewRepoID()
	if _, err := tx.ExecContext(ctx, `INSERT INTO repositories (id, dir, name, default_branch, kind) VALUES (?, ?, 'zettel', '', 'zettel')`, id, home); err != nil {
		return "", err
	}
	return id, nil
}

// ThreadAsk is where a thread prompt's question was asked from, decided when it
// is sent. It rides the submission and the held prompt (threadsDDL), so it
// survives every hold, re-hold and restart before delivery.
type ThreadAsk struct {
	// Parent is where the question came from.
	Parent *zettelv1.NodeParent
}

// GetParentNodeID answers the node the question was asked of, "" for a thread
// start or a nil ask.
func (a *ThreadAsk) GetParentNodeID() string {
	if a == nil {
		return ""
	}
	return a.Parent.GetNode().GetNode().GetId()
}

// encodeThreadAsk renders an ask as its column, NULL for none.
func encodeThreadAsk(a *ThreadAsk) (any, error) {
	if a == nil {
		return nil, nil
	}
	if a.Parent.GetOrigin() == nil {
		return nil, errors.New("wsm: a thread ask carries where it was asked from")
	}
	blob, err := proto.Marshal(a.Parent)
	if err != nil {
		return nil, fmt.Errorf("wsm: encode a thread ask's parent: %w", err)
	}
	return blob, nil
}

// decodeThreadAsk reads the column back.
func decodeThreadAsk(table, row string, parent []byte) (*ThreadAsk, error) {
	if parent == nil {
		return nil, nil
	}
	var decoded zettelv1.NodeParent
	if err := proto.Unmarshal(parent, &decoded); err != nil {
		return nil, &DecodeError{Table: table, Row: row, Field: "thread_parent", Err: err}
	}
	if decoded.GetOrigin() == nil {
		return nil, &DecodeError{Table: table, Row: row, Field: "thread_parent", Err: errors.New("a thread ask's parent names an origin")}
	}
	return &ThreadAsk{Parent: &decoded}, nil
}

// ThreadQuestion is a thread turn's question as zettel will record it: the
// words the user typed, and where they asked them from. It rides the turn row
// (threadsDDL) from delivery until the answer is known.
type ThreadQuestion struct {
	// Text is the question as the user typed it, without the quoted material
	// the daemon adds for the model.
	Text string
	// Parent is where the question came from.
	Parent *zettelv1.NodeParent
}

// encodeThreadQuestion renders a question as its two columns, both NULL for
// none. A question is stored whole or not at all.
func encodeThreadQuestion(q *ThreadQuestion) (text, parent any, err error) {
	if q == nil {
		return nil, nil, nil
	}
	if strings.TrimSpace(q.Text) == "" {
		return nil, nil, errors.New("wsm: a thread question carries the words the user typed")
	}
	if q.Parent.GetOrigin() == nil {
		return nil, nil, errors.New("wsm: a thread question carries where it was asked from")
	}
	blob, err := proto.Marshal(q.Parent)
	if err != nil {
		return nil, nil, fmt.Errorf("wsm: encode a thread question's parent: %w", err)
	}
	return q.Text, blob, nil
}

// decodeThreadQuestion reads the two columns back; one set without the other,
// or a parent that does not decode, is a corrupt row.
func decodeThreadQuestion(table, row string, text sql.NullString, parent []byte) (*ThreadQuestion, error) {
	switch {
	case !text.Valid && parent == nil:
		return nil, nil
	case !text.Valid || parent == nil:
		return nil, &DecodeError{Table: table, Row: row, Field: "thread_question", Err: errors.New("a thread question is stored whole or not at all")}
	}
	var decoded zettelv1.NodeParent
	if err := proto.Unmarshal(parent, &decoded); err != nil {
		return nil, &DecodeError{Table: table, Row: row, Field: "thread_parent", Err: err}
	}
	if decoded.GetOrigin() == nil {
		return nil, &DecodeError{Table: table, Row: row, Field: "thread_parent", Err: errors.New("a thread question's parent names an origin")}
	}
	return &ThreadQuestion{Text: text.String, Parent: &decoded}, nil
}
