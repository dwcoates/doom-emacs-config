package daemonrules

import (
	"go/ast"
	"go/parser"
	"go/token"
	"io/fs"
	"os"
	"path/filepath"
	"regexp"
	"slices"
	"strconv"
	"strings"
	"testing"

	"agent-repld/reposcans/internal/sourcescan"
)

// internalWsmDir is the package these rules hold, under the daemon module's root.
const internalWsmDir = "internal/wsm"

// TestTheWorkspaceInstantWritersShareWriteAdvancedInstant pins that the status
// change and interaction instants are written through the one helper, so a
// hand-rolled copy of its statement or its existence check fails here.
func TestTheWorkspaceInstantWritersShareWriteAdvancedInstant(t *testing.T) {
	sourcescan.InPackage(t, internalWsmDir)
	for _, file := range []string{"statuschanged.go", "interaction.go"} {
		t.Run(file, func(t *testing.T) {
			// Arrange
			raw, err := os.ReadFile(file)
			if err != nil {
				t.Fatalf("read %s: %v", file, err)
			}
			src := string(raw)

			// Act / Assert
			if !strings.Contains(src, "s.writeAdvancedInstant(") || strings.Contains(src, "advanceInstant(ctx, tx,") {
				t.Fatalf("%s writes an instant without writeAdvancedInstant", file)
			}
		})
	}
}

// TestOnlyTheConnectionsOwnersReachThePool pins q as the one way a statement
// finds its connection. An operation holds the pool's only connection, so a
// statement inside it that went to the pool (s.db()) would wait forever for
// the connection its own operation holds. The pool is reached only where no
// operation can be running: the open, the migration, the close, and q itself.
func TestOnlyTheConnectionsOwnersReachThePool(t *testing.T) {
	sourcescan.InPackage(t, internalWsmDir)
	// Arrange
	owners := map[string]bool{"conn.go": true, "open.go": true, "memory.go": true, "migrate.go": true}
	pool := regexp.MustCompile(`s\.db\(\)`)

	// Act
	var offenders []string
	for _, file := range sourcescan.Production(t, internalWsmDir) {
		if !owners[file.Name] && pool.Match(file.Source) {
			offenders = append(offenders, file.Name)
		}
	}

	// Assert
	if len(offenders) != 0 {
		t.Fatalf("%v reach the pool with s.db(); run statements on s.q(ctx)", offenders)
	}
}

func TestEveryFamilyListScansThroughScanFamilyRef(t *testing.T) {
	sourcescan.InPackage(t, internalWsmDir)
	// Act
	handRolled := sourcescan.Count(t, internalWsmDir, "row.Scan(&ref.ID, &ref.Dir, &ref.Name)")

	// Assert: the helper's own scan is the only one.
	if handRolled != 1 {
		t.Fatalf("family member scans = %d, want only scanFamilyRef's", handRolled)
	}
}

// TestTheKeepaliveRunHasOneWrite pins countKeepalive as the run's one write:
// a hand-rolled count elsewhere would drift from its turn-clock rule.
func TestTheKeepaliveRunHasOneWrite(t *testing.T) {
	sourcescan.InPackage(t, internalWsmDir)
	// Act
	writers := sourcescan.Sites(t, internalWsmDir, "keepalive_run_beats =")

	// Assert
	if !slices.Equal(writers, []string{"keepaliverun.go"}) {
		t.Fatalf("the run is written in %v, want keepaliverun.go alone", writers)
	}
}

// TestLastWorkAtHasOneWriter pins the structure invariant A rests on: the
// only statement in this package that writes last_work_at is RecordWork's
// (through advanceInstant), beside the layout's own DDL. A second writer is
// a path that is not work reaching the idle clock.
func TestLastWorkAtHasOneWriter(t *testing.T) {
	sourcescan.InPackage(t, internalWsmDir)
	// Act
	got := sourcescan.SitesMatching(t, internalWsmDir, regexp.MustCompile(`last_work_at\s*=|"last_work_at"`))

	// Assert
	if want := []string{"lastwork.go", "lastwork.go"}; !slices.Equal(got, want) {
		t.Fatalf("last_work_at is written in %v, want only lastwork.go's DDL backfill and RecordWork", got)
	}
}

// TestRowDecodersListThroughListRows keeps every many-row read that decodes
// with a scan helper on the one loop: a hand-rolled `for rows.Next()` that
// calls a scanX(rows) decoder is a second copy of listRows to drift.
func TestRowDecodersListThroughListRows(t *testing.T) {
	sourcescan.InPackage(t, internalWsmDir)
	// Arrange
	handRolled := regexp.MustCompile(`for rows\.Next\(\) \{\s*\w+, err := scan[A-Z]\w*\(rows\)`)

	// Act
	var offenders []string
	for _, file := range sourcescan.Production(t, internalWsmDir) {
		if handRolled.Match(file.Source) {
			offenders = append(offenders, file.Name)
		}
	}

	// Assert
	if len(offenders) != 0 {
		t.Fatalf("%v decode rows in a hand-rolled loop; use listRows", offenders)
	}
}

// TestNoSiteHandRollsTheMainWorktreeRule pins that the registry and the
// workspace verbs ask IsMainWorktree (or isMainWorkspace, its in-transaction
// reading) rather than comparing a directory with a repository directory
// themselves, in Go or in SQL.
func TestNoSiteHandRollsTheMainWorktreeRule(t *testing.T) {
	sourcescan.InPackage(t, internalWsmDir)
	// Arrange
	handRolled := regexp.MustCompile(`(==|!=)\s*repoDir\b|\brepoDir\s*(==|!=)|\bdir\s*=\s*repositories\.dir\b`)
	var offenders []string

	// Act
	for _, dir := range []string{".", filepath.Join("..", "workspace")} {
		files, err := filepath.Glob(filepath.Join(dir, "*.go"))
		if err != nil {
			t.Fatalf("glob %s: %v", dir, err)
		}
		for _, file := range files {
			if strings.HasSuffix(file, "_test.go") || filepath.Base(file) == "mainworktree.go" {
				continue
			}
			src, err := os.ReadFile(file)
			if err != nil {
				t.Fatalf("read %s: %v", file, err)
			}
			if handRolled.Match(src) {
				offenders = append(offenders, file)
			}
		}
	}

	// Assert
	if len(offenders) != 0 {
		t.Fatalf("these files compare a directory with repoDir by hand; use IsMainWorktree: %v", offenders)
	}
}

func TestOnlyDropMergeLeaseDeletesAMergeLease(t *testing.T) {
	sourcescan.InPackage(t, internalWsmDir)
	// Act
	sites := sourcescan.Sites(t, internalWsmDir, "DELETE FROM leases WHERE id = ? AND workspace_id = ? AND holder = ?")

	// Assert
	if !slices.Equal(sites, []string{"mergequeue.go"}) {
		t.Fatalf("a merge lease is deleted at %v, want only dropMergeLease in mergequeue.go", sites)
	}
}

// TestEveryWriteThatSettlesARoundGoesThroughSettleLiveRounds pins that the
// package's one UPDATE writing a round's end is settleLiveRounds', beside the
// layout-155 step, which shipped before rounds had holds: a hand-rolled
// settling UPDATE could leave a round settled and held.
func TestEveryWriteThatSettlesARoundGoesThroughSettleLiveRounds(t *testing.T) {
	sourcescan.InPackage(t, internalWsmDir)
	// Arrange
	update := regexp.MustCompile(`(?s)UPDATE merge_tab_intervals(?: AS \w+)? SET (.*?)WHERE`)

	// Act
	var settling []string
	for _, f := range sourcescan.Production(t, internalWsmDir) {
		for _, m := range update.FindAllSubmatch(f.Source, -1) {
			if strings.Contains(string(m[1]), "ended_at") {
				settling = append(settling, f.Name+": "+strings.Join(strings.Fields(string(m[1])), " "))
			}
		}
	}

	// Assert
	slices.Sort(settling)
	want := []string{
		"mergeledger.go: ended_at = `+ended.text+`, outcome = ?, held_since = NULL",
		"mergeledger.go: outcome = 'restarted', ended_at = COALESCE((SELECT MIN(later.started_at) FROM merge_tab_intervals AS later",
	}
	if !slices.Equal(settling, want) {
		t.Fatalf("the UPDATEs that settle a round are\n%s\nwant only settleLiveRounds' and the layout-155 step's:\n%s", strings.Join(settling, "\n"), strings.Join(want, "\n"))
	}
}

// TestNoQueueReadSpellsOutItsColumns holds every merge_queue read to
// mergeQueueColumns: a read that lists its columns by hand can drift from the
// scan's order.
func TestNoQueueReadSpellsOutItsColumns(t *testing.T) {
	sourcescan.InPackage(t, internalWsmDir)
	// Arrange
	src, err := os.ReadFile("mergequeue.go")
	if err != nil {
		t.Fatalf("read mergequeue.go: %v", err)
	}

	// Act
	spelled := strings.Count(string(src), "pick_started_at, pick_turn, slot, slot_since")

	// Assert
	if spelled != 1 {
		t.Fatalf("the queue's column list is spelled %d times in mergequeue.go, want once (mergeQueueColumns)", spelled)
	}
}

func TestEveryReorderingReadsItsPlaceThroughEntryPlace(t *testing.T) {
	sourcescan.InPackage(t, internalWsmDir)
	// Arrange
	code, err := os.ReadFile("mergequeue.go")
	if err != nil {
		t.Fatalf("read mergequeue.go: %v", err)
	}

	// Act
	reads := strings.Count(string(code), "SELECT state, seq FROM merge_queue WHERE repo_key = ? AND workspace_id = ?")
	calls := strings.Count(string(code), "entryPlace(ctx, tx, key, id)")

	// Assert: the helper's own query, and one call per reordering (move,
	// move to a place, move to a named place (PlaceMerge), prioritize, a
	// merge stepping aside, a remediated
	// merge returning, the place a returned merge answers (returnPlaceOf),
	// and an admitted merge moving ahead of the merges it passed over) and
	// per state write guarded on its prior state (setMergeQueueState).
	if reads != 1 || calls != 9 {
		t.Fatalf("state/seq reads = %d, entryPlace calls = %d; want 1 and 9", reads, calls)
	}
}

// THE QUEUE REORDERS THROUGH ONE HELPER: a hand-rolled seq swap elsewhere
// would skip the negative parking the unique (repo_key, seq) index needs.
func TestEveryQueueReorderGoesThroughAssignQueuePlaces(t *testing.T) {
	sourcescan.InPackage(t, internalWsmDir)
	src, err := os.ReadFile("mergequeue.go")
	if err != nil {
		t.Fatal(err)
	}
	if n := strings.Count(string(src), "SET seq = -seq"); n != 1 {
		t.Fatalf("mergequeue.go parks seqs in %d places, want only assignQueuePlaces", n)
	}
}

// TestRepoKeyRefusalIsRecordedOnlyByTheHelper fails on a hand-rolled site: the
// refusal message exists once, inside normalizedRepoKey.
func TestRepoKeyRefusalIsRecordedOnlyByTheHelper(t *testing.T) {
	sourcescan.InPackage(t, internalWsmDir)
	// Arrange
	const message = "refused a merge queue key that cannot be normalized"

	// Act
	sites := sourcescan.Sites(t, internalWsmDir, message)

	// Assert
	if want := []string{"mergequeue.go"}; !slices.Equal(sites, want) {
		t.Errorf("files recording %q = %v, want %v (only normalizedRepoKey records it)", message, sites, want)
	}
}

// queueRowWrite matches a statement that adds or deletes a merge queue row,
// and queueUpdate one that updates rows, its SET clause captured.
var (
	queueRowWrite    = regexp.MustCompile(`(?is)^\s*(INSERT(\s+OR\s+\w+)?\s+INTO\s+merge_queue\b|DELETE\s+FROM\s+merge_queue\b)`)
	queueUpdate      = regexp.MustCompile(`(?is)^\s*UPDATE\s+merge_queue\s+SET\b(.*?)(\bWHERE\b|$)`)
	queueShapeColumn = regexp.MustCompile(`(?i)\b(state|seq|aside)\s*=`)
)

// queueShapeWrite reports whether TEXT changes the shape of a merge queue: a
// row added or deleted, or a row's state, place or repair moved. Only an
// update's SET clause is read: a WHERE clause that names `state` (a write
// guarded on it) moves nothing.
func queueShapeWrite(text string) bool {
	if queueRowWrite.MatchString(text) {
		return true
	}
	m := queueUpdate.FindStringSubmatch(text)
	return m != nil && queueShapeColumn.MatchString(m[1])
}

// unstampedQueueWrites answers each function of FILES that writes a merge
// queue's shape (queueShapeWrite) and reaches no slot stamping: it neither
// runs through writeQueue nor restamps by hand (restampMergeSlots). A helper
// handed the transaction (a *sql.Tx parameter) is stamped by its callers, so
// each of its callers is held to the rule in its place.
func unstampedQueueWrites(t *testing.T, files []sourcescan.File) []string {
	t.Helper()
	type fn struct {
		writes, stamps, takesTx bool
		calls                   map[string]bool
	}
	funcs := map[string]*fn{}
	fset := token.NewFileSet()
	for _, file := range files {
		parsed, err := parser.ParseFile(fset, file.Name, file.Source, 0)
		if err != nil {
			t.Fatalf("parsing %s: %v", file.Name, err)
		}
		for _, decl := range parsed.Decls {
			d, ok := decl.(*ast.FuncDecl)
			if !ok || d.Body == nil {
				continue
			}
			f := &fn{calls: map[string]bool{}}
			for _, field := range d.Type.Params.List {
				if star, ok := field.Type.(*ast.StarExpr); ok {
					if sel, ok := star.X.(*ast.SelectorExpr); ok && sel.Sel.Name == "Tx" {
						f.takesTx = true
					}
				}
			}
			ast.Inspect(d.Body, func(n ast.Node) bool {
				switch n := n.(type) {
				case *ast.BasicLit:
					if n.Kind == token.STRING {
						if text, err := strconv.Unquote(n.Value); err == nil && queueShapeWrite(text) {
							f.writes = true
						}
					}
				case *ast.CallExpr:
					name := ""
					switch fun := n.Fun.(type) {
					case *ast.Ident:
						name = fun.Name
					case *ast.SelectorExpr:
						name = fun.Sel.Name
					}
					if name == "writeQueue" || name == "restampMergeSlots" {
						f.stamps = true
					}
					f.calls[name] = true
				}
				return true
			})
			funcs[d.Name.Name] = f
		}
	}
	// THE RULE IS CARRIED UP THE CALLS until a function stamps or holds no
	// transaction of its caller's.
	writers := map[string]bool{}
	for name, f := range funcs {
		if f.writes {
			writers[name] = true
		}
	}
	var unstamped []string
	checked := map[string]bool{}
	for len(writers) > 0 {
		next := map[string]bool{}
		for name := range writers {
			if checked[name] {
				continue
			}
			checked[name] = true
			f := funcs[name]
			switch {
			case f.stamps, name == "restampMergeSlots":
			case f.takesTx:
				for caller, c := range funcs {
					if c.calls[name] {
						next[caller] = true
					}
				}
			default:
				unstamped = append(unstamped, name)
			}
		}
		writers = next
	}
	slices.Sort(unstamped)
	return unstamped
}

// TestEveryQueueShapeWriteStampsItsSlots pins writeQueue as the one path a
// write that moves, adds or deletes a merge_queue row takes (or a hand
// restamp, as Forget's cascade does): a write that bypassed both would leave
// its slot change with no moment until the next queue write.
func TestEveryQueueShapeWriteStampsItsSlots(t *testing.T) {
	sourcescan.InPackage(t, internalWsmDir)
	// Act
	got := unstampedQueueWrites(t, sourcescan.Production(t, internalWsmDir))

	// Assert
	if len(got) != 0 {
		t.Fatalf("functions writing a merge queue's shape outside writeQueue: %v", got)
	}
}

func TestUnstampedQueueWritesFindsABypass(t *testing.T) {
	sourcescan.InPackage(t, internalWsmDir)
	tests := []struct {
		name   string
		source string
		want   []string
	}{
		{name: "a delete outside writeQueue", want: []string{"drop"}, source: `package wsm
func (s *store) drop(ctx context.Context) error {
	return s.write(ctx, "op", nil, func(ctx context.Context, tx *sql.Tx) error {
		_, err := tx.ExecContext(ctx, "DELETE FROM merge_queue WHERE workspace_id = ?", 1)
		return err
	})
}`},
		{name: "a tx helper whose caller does not stamp", want: []string{"move"}, source: `package wsm
func moveInTx(ctx context.Context, tx *sql.Tx) error {
	_, err := tx.ExecContext(ctx, "UPDATE merge_queue SET seq = 2")
	return err
}
func (s *store) move(ctx context.Context) error {
	return s.write(ctx, "op", nil, func(ctx context.Context, tx *sql.Tx) error { return moveInTx(ctx, tx) })
}`},
		{name: "a write through writeQueue", source: `package wsm
func (s *store) admit(ctx context.Context) error {
	return s.writeQueue(ctx, "op", nil, "k", func(ctx context.Context, tx *sql.Tx) error {
		_, err := tx.ExecContext(ctx, "UPDATE merge_queue SET state = 1")
		return err
	})
}`},
		{name: "a write guarded on its state that moves no shape column", source: `package wsm
func (s *store) clear(ctx context.Context) error {
	return s.write(ctx, "op", nil, func(ctx context.Context, tx *sql.Tx) error {
		_, err := tx.ExecContext(ctx, "UPDATE merge_queue SET source_pick = NULL WHERE workspace_id = ? AND state = ?", 1, 2)
		return err
	})
}`},
		{name: "a write of no shape column", source: `package wsm
func (s *store) hold(ctx context.Context) error {
	return s.write(ctx, "op", nil, func(ctx context.Context, tx *sql.Tx) error {
		_, err := tx.ExecContext(ctx, "UPDATE merge_queue SET source_keep_open = 1")
		return err
	})
}`},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			got := unstampedQueueWrites(t, []sourcescan.File{{Name: "x.go", Source: []byte(tt.source)}})

			// Assert
			if !slices.Equal(got, tt.want) {
				t.Fatalf("unstamped = %v, want %v", got, tt.want)
			}
		})
	}
}

// TestEverySingleSubscriptionEndGoesThroughEndSubscription pins that the one
// statement deleting a single subscription is endSubscription's: an end that
// hand-rolls it would skip the scope's normalization or its refusal record.
func TestEverySingleSubscriptionEndGoesThroughEndSubscription(t *testing.T) {
	sourcescan.InPackage(t, internalWsmDir)
	// Arrange
	statement := "DELETE FROM merge_queue_subscriptions WHERE subscriber = ? AND repo_key = ?"
	found := 0

	for _, file := range sourcescan.Production(t, internalWsmDir) {
		// Act
		found += strings.Count(string(file.Source), statement)
	}

	// Assert
	if found != 1 {
		t.Fatalf("%q appears %d times in production source, want once (endSubscription)", statement, found)
	}
}

// TestNoQueueRowIsDeletedOutsideTheOneTransition holds every exit to
// leaveQueueInTx: a statement deleting a queue row anywhere else could delete
// a request it did not land. The one other delete is the layout-128 move of
// async requests into their own table, which deletes only what it copied.
func TestNoQueueRowIsDeletedOutsideTheOneTransition(t *testing.T) {
	sourcescan.InPackage(t, internalWsmDir)
	// Act
	sites := sourcescan.Sites(t, internalWsmDir, "DELETE FROM merge_queue ")

	// Assert
	if want := []string{"asyncrequests.go", "offqueue.go"}; !slices.Equal(sites, want) {
		t.Fatalf("merge_queue rows are deleted in %v, want only the layout-128 move and the one transition (%v)", sites, want)
	}
}

// Every refusal of a workspace already on its queue is refuseQueued's, so a
// request and a requeue answer the same place in the same words.
func TestOneReadRefusesAWorkspaceAlreadyOnItsQueue(t *testing.T) {
	sourcescan.InPackage(t, internalWsmDir)
	// Act
	sites := sourcescan.Sites(t, internalWsmDir, "SELECT seq, state FROM merge_queue WHERE repo_key = ? AND workspace_id = ?")
	callers := sourcescan.Sites(t, internalWsmDir, "refuseQueued(ctx, tx, key, id)")

	// Assert
	if want := []string{"offqueue.go"}; !slices.Equal(sites, want) {
		t.Fatalf("a workspace's queue row is read for a refusal in %v, want only refuseQueued's %v", sites, want)
	}
	if want := []string{"mergequeue.go", "offqueue.go"}; !slices.Equal(callers, want) {
		t.Fatalf("refuseQueued is called in %v, want the request and the requeue (%v)", callers, want)
	}
}

func TestEveryOlderLayoutFileIsBuiltForwardByFixtureAt(t *testing.T) {
	sourcescan.InPackage(t, internalWsmDir)
	// Arrange: every test file of this package but this one, which names the
	// pattern it refuses.
	names, err := filepath.Glob("*_test.go")
	if err != nil {
		t.Fatalf("glob: %v", err)
	}
	undo := regexp.MustCompile(`DROP COLUMN`)

	// Act / Assert: no test takes a migrated file back by undoing a layout's
	// columns. A file at an older layout is built forward with fixtureAt,
	// which replays the migrations themselves, so a new layout never has to
	// be restated as an undo for an older-layout test to stay true.
	for _, name := range names {
		if name == "olderlayouts_test.go" {
			continue
		}
		source, err := os.ReadFile(name)
		if err != nil {
			t.Fatalf("read %s: %v", name, err)
		}
		if undo.Match(source) {
			t.Errorf("%s drops a column to take a file back to an older layout, want fixtureAt", name)
		}
	}
}

// Every rollback in this package goes through endTx, so no site can quietly
// drop a failed one again.
func TestEveryRollbackGoesThroughEndTx(t *testing.T) {
	sourcescan.InPackage(t, internalWsmDir)
	// Act
	rollbacks := sourcescan.Count(t, internalWsmDir, ".Rollback()")

	// Assert
	if rollbacks != 1 {
		t.Fatalf("production source calls Rollback %d times; only endTx may", rollbacks)
	}
}

// Every commit in this package goes through committed, so no site reads a
// cancelled caller's commit as a broken store again.
func TestEveryCommitGoesThroughCommitted(t *testing.T) {
	sourcescan.InPackage(t, internalWsmDir)
	// Act
	commits := sourcescan.Count(t, internalWsmDir, ".Commit()")
	wrapped := sourcescan.Count(t, internalWsmDir, "committed(ctx, tx.Commit())")

	// Assert
	if commits != wrapped {
		t.Fatalf("production source calls Commit %d times, %d of them through committed", commits, wrapped)
	}
}

// TestPullRequestSubscriptionsAreInsertedIfAbsentThroughOneHelper pins that a
// subscription taken without replacing a held one is written through
// insertPullRequestSubscriptionIfAbsentInTx, so a hand-rolled copy of its
// statement fails here.
func TestPullRequestSubscriptionsAreInsertedIfAbsentThroughOneHelper(t *testing.T) {
	sourcescan.InPackage(t, internalWsmDir)
	for _, file := range []string{"workspacelinks.go", "followedpullrequests.go"} {
		t.Run(file, func(t *testing.T) {
			// Arrange
			raw, err := os.ReadFile(file)
			if err != nil {
				t.Fatalf("read %s: %v", file, err)
			}
			src := string(raw)

			// Act / Assert
			if !strings.Contains(src, "insertPullRequestSubscriptionIfAbsentInTx(") || strings.Contains(src, "INSERT INTO pull_request_subscriptions") {
				t.Fatalf("%s inserts a pull-request subscription without insertPullRequestSubscriptionIfAbsentInTx", file)
			}
		})
	}
}

// handRolledRef is a workspace.v1.WorkspaceRef composed from a record's id and
// dir by hand, which Ref replaces.
var handRolledRef = regexp.MustCompile(`WorkspaceRef\{Id: string\([\w.*()]+\.ID\), Dir: [\w.*()]+\.Dir\}`)

func TestNoDaemonSourceComposesAWorkspaceRefByHand(t *testing.T) {
	sourcescan.InPackage(t, internalWsmDir)
	// Arrange: the daemon module's root, two levels above this package.
	root, err := filepath.Abs(filepath.Join("..", ".."))
	if err != nil {
		t.Fatalf("Abs: %v", err)
	}
	self, err := filepath.Abs("ref.go")
	if err != nil {
		t.Fatalf("Abs: %v", err)
	}

	// Act.
	var found []string
	err = filepath.WalkDir(root, func(path string, d fs.DirEntry, err error) error {
		if err != nil {
			return err
		}
		if d.IsDir() || !strings.HasSuffix(path, ".go") || strings.HasSuffix(path, "_test.go") || path == self {
			return nil
		}
		src, err := os.ReadFile(path)
		if err != nil {
			return err
		}
		for _, hit := range handRolledRef.FindAll(src, -1) {
			found = append(found, path+": "+string(hit))
		}
		return nil
	})
	if err != nil {
		t.Fatalf("walk %s: %v", root, err)
	}

	// Assert.
	if len(found) > 0 {
		t.Fatalf("hand-composed workspace refs, use Workspace.Ref or FamilyRef.Ref:\n%s", strings.Join(found, "\n"))
	}
}

// handRolledLookup matches a loop over a repository list (named `repos` or
// `repositories`, as every site names ListRepositories' answer) whose body
// opens by comparing each repository's ID or Dir, for a match or (behind an
// optional "is this criterion set" guard) a skip: the shape RepositoryWithID,
// RepositoryAt and RepositoryNamed replaced at every site.
var handRolledLookup = regexp.MustCompile(`for _, (\w+) := range (?:repos|repositories) \{\s*if (?:[\w.]+ != "" && )?(?:string\()?(\w+)\.(?:ID|Dir)\)? [!=]= `)

// lookupHome is the one file allowed the loop: the lookups themselves.
const lookupHome = "repositoryfind.go"

// TestNoProductionCodeHandRollsARepositoryLookup pins that every site that
// finds a repository in a listed registry goes through RepositoryWithID or
// RepositoryAt, so a new site that loops by hand fails here instead of
// drifting from the shared lookup.
func TestNoProductionCodeHandRollsARepositoryLookup(t *testing.T) {
	sourcescan.InPackage(t, internalWsmDir)
	// Arrange: the daemon's module root, two directories up.
	root, err := filepath.Abs(filepath.Join("..", ".."))
	if err != nil {
		t.Fatalf("resolve the module root: %v", err)
	}

	// Act.
	var offenders []string
	err = filepath.WalkDir(root, func(path string, d fs.DirEntry, err error) error {
		if err != nil {
			return err
		}
		if d.IsDir() || !strings.HasSuffix(path, ".go") || strings.HasSuffix(path, "_test.go") {
			return nil
		}
		if path == filepath.Join(root, "internal", "wsm", lookupHome) {
			return nil
		}
		data, err := os.ReadFile(path)
		if err != nil {
			return err
		}
		for _, match := range handRolledLookup.FindAllStringSubmatch(string(data), -1) {
			if match[1] == match[2] {
				offenders = append(offenders, path)
			}
		}
		return nil
	})

	// Assert.
	if err != nil {
		t.Fatalf("walk %s: %v", root, err)
	}
	if len(offenders) > 0 {
		t.Fatalf("hand-rolled repository lookups in %v; use wsm.RepositoryWithID or wsm.RepositoryAt", offenders)
	}
}

// TestStoredRowsAreDecodedInOnePlace pins DecodeDurableFeedRow as the one
// decode of a stored row outside this package: a reader that carries the
// retired label by hand would drift from it.
func TestStoredRowsAreDecodedInOnePlace(t *testing.T) {
	sourcescan.InPackage(t, internalWsmDir)
	// Arrange
	var stray []string

	// Act
	sourcescan.EachDaemonSource(t, sourcescan.DaemonRoot(t), []string{"internal/wsm"}, func(fset *token.FileSet, file *ast.File) {
		ast.Inspect(file, func(n ast.Node) bool {
			if sourcescan.IsSelector(n, "wsm", "CarryRetiredMergeHeadLabel") {
				stray = append(stray, fset.Position(n.Pos()).String())
			}
			return true
		})
	})

	// Assert
	if len(stray) > 0 {
		t.Fatalf("stored rows are decoded by hand at %v; call wsm.DecodeDurableFeedRow", stray)
	}
}

// TestRetiredFieldsAreCarriedThroughCarryUnknown keeps every step's carry of
// a retired field on the one helper, which removes the field only after its
// successor is written: a hand-rolled peek-then-take is a second copy to
// drift, and the one this replaced stripped the field from a row it then
// refused.
func TestRetiredFieldsAreCarriedThroughCarryUnknown(t *testing.T) {
	sourcescan.InPackage(t, internalWsmDir)
	// Arrange
	handRolled := regexp.MustCompile(`peekUnknown\(`)

	// Act
	var offenders []string
	for _, file := range sourcescan.Production(t, internalWsmDir) {
		if file.Name != "rowrewrite.go" && handRolled.Match(file.Source) {
			offenders = append(offenders, file.Name)
		}
	}

	// Assert
	if len(offenders) != 0 {
		t.Fatalf("%v peek a retired field themselves; carry it through carryUnknown", offenders)
	}
}

// TestOnlyTheSharedLoopRewritesStoredRows keeps every layout step's row
// rewrite on rewriteDurableRows: a step re-encoding rows its own way is a
// second copy of the loop to drift.
func TestOnlyTheSharedLoopRewritesStoredRows(t *testing.T) {
	sourcescan.InPackage(t, internalWsmDir)
	// Arrange
	writes := regexp.MustCompile(`UPDATE durable_feed_rows SET row =`)

	// Act
	var offenders []string
	for _, file := range sourcescan.Production(t, internalWsmDir) {
		if file.Name != "rowrewrite.go" && writes.Match(file.Source) {
			offenders = append(offenders, file.Name)
		}
	}

	// Assert
	if len(offenders) != 0 {
		t.Fatalf("%v rewrite stored rows by hand; use rewriteDurableRows", offenders)
	}
}

// TestEveryRowRewriteReEncodesThroughEditFeedRow keeps each layout step's row
// rewrite on editFeedRow: a step decoding and re-encoding a row its own way is
// a second copy of the skeleton to drift, such as one that re-encodes a row it
// never changed.
func TestEveryRowRewriteReEncodesThroughEditFeedRow(t *testing.T) {
	sourcescan.InPackage(t, internalWsmDir)
	// Arrange
	reEncodes := regexp.MustCompile(`proto\.Marshal\(row\)`)

	// Act
	var offenders []string
	for _, file := range sourcescan.Production(t, internalWsmDir) {
		if file.Name != "rowrewrite.go" && reEncodes.Match(file.Source) {
			offenders = append(offenders, file.Name)
		}
	}

	// Assert
	if len(offenders) != 0 {
		t.Fatalf("%v re-encode stored rows by hand; edit them through editFeedRow", offenders)
	}
}

// TestServingWritesRefuseAnEmptyInstanceThroughRequireInstance keeps every
// serving write's empty-instance refusal on the one helper: an inline check is
// a second spelling of the refusal to drift.
func TestServingWritesRefuseAnEmptyInstanceThroughRequireInstance(t *testing.T) {
	sourcescan.InPackage(t, internalWsmDir)
	// Arrange
	inline := regexp.MustCompile(`== ""\s*\{\s*err := errors\.New\("wsm: empty daemon instance id"\)`)

	// Act
	var offenders []string
	for _, file := range sourcescan.Production(t, internalWsmDir) {
		if inline.Match(file.Source) {
			offenders = append(offenders, file.Name)
		}
	}

	// Assert
	if len(offenders) != 0 {
		t.Fatalf("%v refuse an empty instance inline; use requireInstance", offenders)
	}
}

// TestOwedWorkIsSpelledOnlyByOwedField fails on a hand-rolled spelling of
// owed start work in a record field anywhere in the daemon: every package
// names it through OwedField.
func TestOwedWorkIsSpelledOnlyByOwedField(t *testing.T) {
	sourcescan.InPackage(t, internalWsmDir)
	// Arrange
	const spelling = `"summary_of:" +`
	var offenders []string

	// Act
	err := filepath.WalkDir("..", func(path string, d fs.DirEntry, err error) error {
		if err != nil || d.IsDir() || !strings.HasSuffix(path, ".go") || strings.HasSuffix(path, "_test.go") || path == filepath.Join("..", "wsm", "sessions.go") {
			return err
		}
		body, err := os.ReadFile(path)
		if err != nil {
			return err
		}
		if strings.Contains(string(body), spelling) {
			offenders = append(offenders, path)
		}
		return nil
	})

	// Assert
	if err != nil {
		t.Fatalf("walk the daemon's packages: %v", err)
	}
	if len(offenders) != 0 {
		t.Fatalf("files spelling owed work by hand = %v, want every one through wsm.OwedField", offenders)
	}
}

// TestPlainWorkspaceUpdatesGoThroughSetWorkspaceColumns pins the shared shape:
// workspaces.go spells its plain update-by-id once, in setWorkspaceColumns.
func TestPlainWorkspaceUpdatesGoThroughSetWorkspaceColumns(t *testing.T) {
	sourcescan.InPackage(t, internalWsmDir)
	// Arrange
	source, err := os.ReadFile("workspaces.go")
	if err != nil {
		t.Fatalf("read workspaces.go: %v", err)
	}

	// Act
	spelled := strings.Count(string(source), "UPDATE workspaces SET ")

	// Assert: setWorkspaceColumns, MarkClosed's, SetCurrent's two (the clear
	// and the set), SetPriority's, and RetireSpawnedShim's compare-and-clear,
	// which share a transaction with another statement (the priority's
	// re-landing of the workspace's place) or assign NULLs and literals.
	if spelled != 6 {
		t.Fatalf("workspaces.go spells a workspace update %d times, want 6 (setWorkspaceColumns, MarkClosed, SetCurrent twice, SetPriority, RetireSpawnedShim)", spelled)
	}
}

// TestNoStoreWriteExpandsASubsection pins that the daemon never expands a
// subsection on its own: a placement and a move leave every fold as it is,
// so only SetSubsectionFolded writes a subsection's fold.
func TestNoStoreWriteExpandsASubsection(t *testing.T) {
	sourcescan.InPackage(t, internalWsmDir)
	// Arrange
	const expansion = "UPDATE subsections SET folded = 0"

	for _, file := range sourcescan.Production(t, internalWsmDir) {
		// Act
		n := strings.Count(string(file.Source), expansion)

		// Assert
		if n != 0 {
			t.Errorf("%s states the subsection expansion %d times, want 0", file.Name, n)
		}
	}
}

// TestNoCallSiteSpellsOutListedOpen holds every reader of whether a row is
// open to ListedOpen: a site that combines the three by hand would drift
// from it.
func TestNoCallSiteSpellsOutListedOpen(t *testing.T) {
	sourcescan.InPackage(t, internalWsmDir)
	// Arrange
	root := filepath.Join("..", "..")
	var offenders []string

	// Act
	err := filepath.WalkDir(root, func(path string, d fs.DirEntry, err error) error {
		if err != nil {
			return err
		}
		if d.IsDir() || !strings.HasSuffix(path, ".go") || strings.HasSuffix(path, "_test.go") {
			return nil
		}
		src, err := os.ReadFile(path)
		if err != nil {
			return err
		}
		if strings.Contains(string(src), "!ws.Closed && !ws.Hidden && !ws.MergeHidden()") && !strings.HasSuffix(path, filepath.Join("wsm", "syncasyncmerges.go")) {
			offenders = append(offenders, path)
		}
		return nil
	})

	// Assert
	if err != nil {
		t.Fatalf("walk: %v", err)
	}
	if len(offenders) != 0 {
		t.Fatalf("these files spell out an open row by hand instead of ListedOpen: %v", offenders)
	}
}

// TestNoCallSiteSpellsOutMergeHidden holds every reader of a merge workspace's
// visibility to MergeHidden: a site that tests MergeRevealed by hand would drift
// from it (the landed case is the one a hand-rolled test forgets).
func TestNoCallSiteSpellsOutMergeHidden(t *testing.T) {
	sourcescan.InPackage(t, internalWsmDir)
	// Arrange
	root := filepath.Join("..", "..")
	var offenders []string

	// Act
	err := filepath.WalkDir(root, func(path string, d fs.DirEntry, err error) error {
		if err != nil {
			return err
		}
		if d.IsDir() || !strings.HasSuffix(path, ".go") || strings.HasSuffix(path, "_test.go") {
			return nil
		}
		if strings.HasSuffix(path, filepath.Join("wsm", "syncasyncmerges.go")) || strings.HasSuffix(path, filepath.Join("wsm", "workspaces.go")) {
			return nil
		}
		src, err := os.ReadFile(path)
		if err != nil {
			return err
		}
		if strings.Contains(string(src), ".MergeRevealed") {
			offenders = append(offenders, path)
		}
		return nil
	})

	// Assert
	if err != nil {
		t.Fatalf("walk: %v", err)
	}
	if len(offenders) != 0 {
		t.Fatalf("these files read MergeRevealed by hand instead of MergeHidden: %v", offenders)
	}
}

// TestTurnCloseIsNamedOnlyByItsString fails a production switch outside this
// package that names turn closes itself: every close is named by
// TurnClose.String, so a second table cannot drift from the first.
func TestTurnCloseIsNamedOnlyByItsString(t *testing.T) {
	sourcescan.InPackage(t, internalWsmDir)
	// Act
	offenders := handRolledNames(t, "CloseAgentDied")

	// Assert
	if len(offenders) > 0 {
		t.Fatalf("turn closes named by a hand-rolled switch instead of TurnClose.String in %v", offenders)
	}
}

// TestClassificationArmIsNamedOnlyByItsString is the same guard for the
// verdict arms and ClassificationArm.String.
func TestClassificationArmIsNamedOnlyByItsString(t *testing.T) {
	sourcescan.InPackage(t, internalWsmDir)
	// Act
	offenders := handRolledNames(t, "ArmUninterruptibleTurn")

	// Assert
	if len(offenders) > 0 {
		t.Fatalf("classification arms named by a hand-rolled switch instead of ClassificationArm.String in %v", offenders)
	}
}

// handRolledNames scans every production source of the daemon outside this
// package for a switch case on the constant SELECTOR whose body returns a
// string literal: a second naming table for this package's type.
func handRolledNames(t *testing.T, selector string) []string {
	t.Helper()
	var offenders []string
	err := filepath.WalkDir(filepath.Join("..", ".."), func(path string, d fs.DirEntry, err error) error {
		if err != nil {
			return err
		}
		if d.IsDir() {
			if d.Name() == "testdata" || d.Name() == "node_modules" {
				return filepath.SkipDir
			}
			return nil
		}
		if !strings.HasSuffix(path, ".go") || strings.HasSuffix(path, "_test.go") {
			return nil
		}
		if strings.HasSuffix(filepath.ToSlash(filepath.Dir(path)), "internal/wsm") {
			return nil
		}
		file, err := parser.ParseFile(token.NewFileSet(), path, nil, 0)
		if err != nil {
			return err
		}
		ast.Inspect(file, func(n ast.Node) bool {
			clause, ok := n.(*ast.CaseClause)
			if !ok {
				return true
			}
			for _, expr := range clause.List {
				sel, ok := expr.(*ast.SelectorExpr)
				if !ok || sel.Sel.Name != selector {
					continue
				}
				for _, stmt := range clause.Body {
					if ret, ok := stmt.(*ast.ReturnStmt); ok && len(ret.Results) == 1 {
						if lit, ok := ret.Results[0].(*ast.BasicLit); ok && lit.Kind == token.STRING {
							offenders = append(offenders, path)
						}
					}
				}
			}
			return true
		})
		return nil
	})
	if err != nil {
		t.Fatalf("scan the daemon's sources: %v", err)
	}
	return offenders
}

// THE PER-TURN REPLAY READS SHARE ONE BODY: each is a turnFacts call and
// scans no turn of its own, so a hand-rolled read that drifts from it fails
// here.
func TestThePerTurnReadsAreTurnFactsCalls(t *testing.T) {
	sourcescan.InPackage(t, internalWsmDir)
	// Arrange
	fset := token.NewFileSet()
	pkgs, err := parser.ParseDir(fset, ".", func(fi fs.FileInfo) bool { return !strings.HasSuffix(fi.Name(), "_test.go") }, 0)
	if err != nil {
		t.Fatalf("parse the package: %v", err)
	}
	want := map[string]bool{"TurnAddresses": false, "TurnMirrors": false, "TurnSenders": false}

	// Act
	for _, pkg := range pkgs {
		for _, file := range pkg.Files {
			for _, decl := range file.Decls {
				fn, ok := decl.(*ast.FuncDecl)
				if !ok || fn.Recv == nil {
					continue
				}
				if _, named := want[fn.Name.Name]; !named {
					continue
				}
				calls := map[string]bool{}
				ast.Inspect(fn.Body, func(n ast.Node) bool {
					if call, ok := n.(*ast.CallExpr); ok {
						if id, ok := call.Fun.(*ast.Ident); ok {
							calls[id.Name] = true
						}
					}
					return true
				})
				if !calls["turnFacts"] || calls["scanTurn"] {
					t.Errorf("%s does not read through turnFacts alone (calls %v)", fn.Name.Name, calls)
				}
				want[fn.Name.Name] = true
			}
		}
	}

	// Assert
	for name, found := range want {
		if !found {
			t.Errorf("%s was not found in the package", name)
		}
	}
}

func TestNoSourceSpellsASettledMergeByHand(t *testing.T) {
	sourcescan.InPackage(t, internalWsmDir)
	// Arrange
	settled := regexp.MustCompile(`\.Closed\s*&&\s*\w+\.MergedAt\s*!=\s*nil`)
	var offenders []string

	// Act
	err := filepath.WalkDir("..", func(path string, d fs.DirEntry, err error) error {
		if err != nil {
			return err
		}
		if d.IsDir() || !strings.HasSuffix(path, ".go") || strings.HasSuffix(path, "_test.go") || path == filepath.Join("..", "wsm", "visibility.go") {
			return nil
		}
		src, err := os.ReadFile(path)
		if err != nil {
			return err
		}
		if settled.Match(src) {
			offenders = append(offenders, path)
		}
		return nil
	})

	// Assert
	if err != nil {
		t.Fatalf("walking internal/: %v", err)
	}
	if len(offenders) > 0 {
		t.Fatalf("these sources spell a settled merge by hand instead of through Workspace.MergeSettled: %v", offenders)
	}
}

// TestEveryByIdColumnReadGoesThroughScanWorkspaceRow pins that no store source
// hand-rolls a by-id workspace read, whose not-found mapping would drift.
func TestEveryByIdColumnReadGoesThroughScanWorkspaceRow(t *testing.T) {
	sourcescan.InPackage(t, internalWsmDir)
	// Arrange.
	handRolled := regexp.MustCompile("FROM workspaces WHERE id = \\?`, \\w+\\)\\.Scan\\(")
	var offenders []string

	// Act.
	for _, file := range sourcescan.Production(t, internalWsmDir) {
		// workspacerow.go IS the helper. faults.go's existence check names the
		// FAULT's workspace in its refusal, a different answer from a
		// workspace read's.
		if file.Name == "workspacerow.go" || file.Name == "faults.go" {
			continue
		}
		if handRolled.Match(file.Source) {
			offenders = append(offenders, file.Name)
		}
	}

	// Assert.
	if len(offenders) > 0 {
		t.Fatalf("these files read workspace columns by id by hand; use scanWorkspaceRow: %v", offenders)
	}
}

// TestEveryTouchedQuestionAsksTouchedRow pins that no store write hand-rolls
// "did it touch a row" off RowsAffected: a count compared with zero is
// touchedRow's one question.
func TestEveryTouchedQuestionAsksTouchedRow(t *testing.T) {
	sourcescan.InPackage(t, internalWsmDir)
	// Arrange
	handRolled := regexp.MustCompile(`n, err :?= res\.RowsAffected\(\)(?s:.{0,200}?)\bn (>|==|!=) 0\b`)

	for _, file := range sourcescan.Production(t, internalWsmDir) {
		// Act
		src := strings.Replace(string(file.Source), "func touchedRow(res sql.Result) (bool, error) {\n\tn, err := res.RowsAffected()\n\treturn n > 0, err", "", 1)

		// Assert
		if loc := handRolled.FindStringIndex(src); loc != nil {
			t.Errorf("%s hand-rolls touchedRow: %q", file.Name, src[loc[0]:loc[1]])
		}
	}
}

func TestHideColumnsAreTheOneHideShape(t *testing.T) {
	sourcescan.InPackage(t, internalWsmDir)
	// Act
	sites := sourcescan.Sites(t, internalWsmDir, `name: "backlogged_at"`)

	// Assert
	if len(sites) != 1 || sites[0] != "workspaces.go" {
		t.Fatalf("the backlogged instant is written at %v, want only hideColumns in workspaces.go", sites)
	}
}

func TestOptionalWorkspaceIDIsTheOneOptionalIDRead(t *testing.T) {
	sourcescan.InPackage(t, internalWsmDir)
	// Act
	sites := sourcescan.SitesMatching(t, internalWsmDir, regexp.MustCompile(`var id WorkspaceID\s+err :=[^\n]*QueryRowContext`))

	// Assert
	if len(sites) != 1 || sites[0] != "workspaces.go" {
		t.Fatalf("a lone workspace id that may be absent is read at %v, want only optionalWorkspaceID in workspaces.go", sites)
	}
}

// TestTheLookupShapeIsDetected keeps the scan below honest: it must match the
// very shape it forbids, or it passes vacuously.
func TestTheLookupShapeIsDetected(t *testing.T) {
	sourcescan.InPackage(t, internalWsmDir)
	// Arrange.
	for _, sample := range []string{
		"for _, repository := range repositories {\n\t\tif repository.ID == repo {",
		"for _, repo := range repos {\n\t\tif scope.ID != \"\" && repo.ID != scope.ID {",
	} {
		// Act.
		match := handRolledLookup.FindStringSubmatch(sample)

		// Assert.
		if match == nil || match[1] != match[2] {
			t.Fatalf("the scan does not recognize a hand-rolled repository lookup: %q", sample)
		}
	}
}

func TestTheHandRolledRefPatternMatchesTheShapeItForbids(t *testing.T) {
	sourcescan.InPackage(t, internalWsmDir)
	tests := []struct {
		name string
		src  string
		want bool
	}{
		{name: "a record's fields", src: `&workspacev1.WorkspaceRef{Id: string(record.ID), Dir: record.Dir}`, want: true},
		{name: "a nested field", src: `&workspacev1.WorkspaceRef{Id: string(p.Target.ID), Dir: p.Target.Dir}`, want: true},
		{name: "an id alone", src: `&workspacev1.WorkspaceRef{Id: string(*ws.Parent)}`, want: false},
		{name: "the helper", src: `record.Ref()`, want: false},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange: the fixture is the case.

			// Act.
			got := handRolledRef.MatchString(tc.src)

			// Assert.
			if got != tc.want {
				t.Fatalf("match(%q) = %v, want %v", tc.src, got, tc.want)
			}
		})
	}
}
