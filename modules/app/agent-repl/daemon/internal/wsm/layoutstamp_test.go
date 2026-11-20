package wsm

import (
	"context"
	"database/sql"
	"errors"
	"regexp"
	"slices"
	"strings"
	"testing"
)

// rawStamp reads the file's layout stamp without opening it through a store.
func rawStamp(t *testing.T, path string) layoutStamp {
	t.Helper()
	var st layoutStamp
	withRawDB(t, path, func(db *sql.DB) {
		var err error
		if st, _, err = readLayoutStamp(context.Background(), db, path); err != nil {
			t.Fatalf("read the layout stamp of %q: %v", path, err)
		}
	})
	return st
}

// withSteps has this build's migration list be steps, for a test of the
// stamp's own rules.
func withSteps(t *testing.T, steps ...migration) {
	t.Helper()
	realSteps, realLayout := migrations, LayoutVersion
	migrations, LayoutVersion = steps, steps[len(steps)-1].To
	t.Cleanup(func() { migrations, LayoutVersion = realSteps, realLayout })
}

// mixedSteps is a list whose contract steps stand among its additive ones.
func mixedSteps() []migration {
	return []migration{
		{To: 1, Name: "a1", Kind: MigrationAdditive},
		{To: 2, Name: "c2", Kind: MigrationContract},
		{To: 3, Name: "a3", Kind: MigrationAdditive},
		{To: 4, Name: "c4", Kind: MigrationContract},
		{To: 5, Name: "a5", Kind: MigrationAdditive},
	}
}

func TestLayoutStampAfter(t *testing.T) {
	withSteps(t, mixedSteps()...)
	tests := []struct {
		name    string
		from    layoutStamp
		step    int
		want    layoutStamp
		wantErr string
	}{
		{name: "an additive step in order moves both numbers", from: layoutStamp{0, 0}, step: 1, want: layoutStamp{1, 1}},
		{name: "a contract step in order moves both numbers", from: layoutStamp{1, 1}, step: 2, want: layoutStamp{2, 2}},
		{name: "an additive step past an owed contract moves additive_through alone", from: layoutStamp{1, 1}, step: 3, want: layoutStamp{1, 3}},
		{name: "a second additive step past two owed contracts", from: layoutStamp{1, 3}, step: 5, want: layoutStamp{1, 5}},
		{name: "an owed contract catches version up to the next owed one", from: layoutStamp{1, 5}, step: 2, want: layoutStamp{3, 5}},
		{name: "the last owed contract leaves nothing owed", from: layoutStamp{3, 5}, step: 4, want: layoutStamp{5, 5}},
		{name: "a step that already ran is refused", from: layoutStamp{1, 3}, step: 3, wantErr: "has already run"},
		{name: "a contract that is not the file's next step is refused", from: layoutStamp{1, 5}, step: 4, wantErr: "is not the next step"},
		{name: "a contract before the additive step ahead of it is refused", from: layoutStamp{0, 0}, step: 2, wantErr: "is not the next step"},
		{name: "an additive step that skips an additive one is refused", from: layoutStamp{1, 1}, step: 5, wantErr: "would run before step 3"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			got, err := tt.from.after(migrations[tt.step-1])

			// Assert
			if tt.wantErr != "" {
				if err == nil || !strings.Contains(err.Error(), tt.wantErr) || got != tt.from {
					t.Fatalf("after(step %d) = (%+v, %v), want the stamp unchanged and an error saying %q", tt.step, got, err, tt.wantErr)
				}
				return
			}
			if err != nil || got != tt.want {
				t.Fatalf("after(step %d) = (%+v, %v), want %+v", tt.step, got, err, tt.want)
			}
		})
	}
}

func TestLayoutStampPending(t *testing.T) {
	withSteps(t, mixedSteps()...)
	tests := []struct {
		name string
		from layoutStamp
		want []int
	}{
		{name: "a file at no step owes every step", from: layoutStamp{0, 0}, want: []int{1, 2, 3, 4, 5}},
		{name: "a joined file owes its contracts alone", from: layoutStamp{1, 5}, want: []int{2, 4}},
		{name: "a file joined part way owes the contracts and the later steps", from: layoutStamp{1, 3}, want: []int{2, 4, 5}},
		{name: "a file at this build's layout owes nothing", from: layoutStamp{5, 5}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			var got []int
			for _, m := range tt.from.pending() {
				got = append(got, m.To)
			}

			// Assert
			if !slices.Equal(got, tt.want) {
				t.Fatalf("pending = %v, want %v", got, tt.want)
			}
		})
	}
}

func TestLayoutStampString(t *testing.T) {
	tests := []struct {
		name string
		st   layoutStamp
		want string
	}{
		{name: "a file that owes nothing is its version", st: layoutStamp{7, 7}, want: "layout 7"},
		{name: "a file that owes a contract names both numbers", st: layoutStamp{7, 9}, want: "layout 7 with every step that is no contract run through 9"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			if got := tt.st.String(); got != tt.want {
				t.Fatalf("String() = %q, want %q", got, tt.want)
			}
		})
	}
}

func TestReadLayoutStamp(t *testing.T) {
	tests := []struct {
		name string
		// arrange answers the file to read.
		arrange    func(t *testing.T) string
		want       layoutStamp
		wantColumn bool
		wantErr    string
	}{
		{name: "a file from before the second number reads as its version", arrange: func(t *testing.T) string { return fixtureAt(t, 7) }, want: layoutStamp{7, 7}},
		{name: "a fresh file's NULL reads as its version", arrange: writableStore, want: layoutStamp{LayoutVersion, LayoutVersion}, wantColumn: true},
		{name: "a file that owes a contract reads both numbers", arrange: func(t *testing.T) string {
			path := writableStore(t)
			execRaw(t, path, `UPDATE layout SET version = 7, additive_through = 9`)
			return path
		}, want: layoutStamp{7, 9}, wantColumn: true},
		{name: "a second number below the version is undecodable", arrange: func(t *testing.T) string {
			path := writableStore(t)
			execRaw(t, path, `UPDATE layout SET version = 9, additive_through = 7`)
			return path
		}, wantErr: "additive_through 7 is below version 9"},
		{name: "a file with no layout row is undecodable", arrange: func(t *testing.T) string {
			path := writableStore(t)
			execRaw(t, path, `DELETE FROM layout`)
			return path
		}, wantErr: "no layout row"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			path := tt.arrange(t)

			// Act
			var got layoutStamp
			var column bool
			var err error
			withRawDB(t, path, func(db *sql.DB) {
				got, column, err = readLayoutStamp(context.Background(), db, path)
			})

			// Assert
			if tt.wantErr != "" {
				var undecodable *DecodeError
				if !errors.As(err, &undecodable) || !strings.Contains(err.Error(), tt.wantErr) {
					t.Fatalf("readLayoutStamp = %v, want a *DecodeError saying %q", err, tt.wantErr)
				}
				return
			}
			if err != nil || got != tt.want || column != tt.wantColumn {
				t.Fatalf("readLayoutStamp = (%+v, column %v, %v), want (%+v, column %v)", got, column, err, tt.want, tt.wantColumn)
			}
		})
	}
}

func TestAnUndecodableLayoutStampIsRecorded(t *testing.T) {
	// Arrange
	s, log := fileStore(t)
	corrupt(t, s, `UPDATE layout SET version = 9, additive_through = 7`)

	// Act
	_, err := s.layoutStamp(context.Background())

	// Assert
	var undecodable *DecodeError
	if !errors.As(err, &undecodable) {
		t.Fatalf("layoutStamp = %v, want a *DecodeError", err)
	}
	record, ok := recordFor(log, "daemon.wsm.open", "error", "state database carries no layout stamp this build can decode")
	if !ok || record.Context["path"] != s.path || !strings.Contains(record.Context["error"].(string), "additive_through 7 is below version 9") {
		t.Fatalf("record = %+v (found %v), want the undecodable stamp recorded with its path and cause", record, ok)
	}
}

func TestApplyMigrationRefusesAStepThatAlreadyRan(t *testing.T) {
	// Arrange: a fresh file, at this build's layout.
	s, _ := fileStore(t)
	before := rawStamp(t, s.path)

	// Act
	err := s.applyMigration(context.Background(), migration{To: LayoutVersion, Name: "again", Kind: MigrationAdditive,
		DDL: `CREATE TABLE ran_twice (id INTEGER)`})

	// Assert
	if err == nil || !strings.Contains(err.Error(), "has already run") {
		t.Fatalf("applyMigration = %v, want the step refused as already run", err)
	}
	if after := rawStamp(t, s.path); after != before {
		t.Fatalf("the stamp moved from %+v to %+v, want it unchanged", before, after)
	}
	if got := scalar[int](t, s, `SELECT count(*) FROM sqlite_master WHERE name = 'ran_twice'`); got != 0 {
		t.Fatalf("the refused step's table exists, want its statements never run")
	}
}

func TestStampStepRefusesAFileThatNoLongerStandsWhereItWasRead(t *testing.T) {
	// Arrange: the stamp another applier is taken to have moved.
	s, _ := fileStore(t)
	stood := rawStamp(t, s.path)
	read := layoutStamp{Version: stood.Version - 1, AdditiveThrough: stood.Version - 1}
	tx, err := s.db().BeginTx(context.Background(), nil)
	if err != nil {
		t.Fatalf("begin: %v", err)
	}

	// Act
	err = stampStep(context.Background(), tx, s.path, true, read, stood, migration{To: stood.Version, Name: "raced"})
	if rollback := tx.Rollback(); rollback != nil {
		t.Fatalf("rollback: %v", rollback)
	}

	// Assert
	if err == nil || !strings.Contains(err.Error(), "touched 0 rows") || !strings.Contains(err.Error(), "no longer stands at") {
		t.Fatalf("stampStep = %v, want the compare-and-set to refuse a file that moved", err)
	}
}

func TestOpenGivesEveryFileTheStampsSecondNumber(t *testing.T) {
	tests := []struct {
		name    string
		arrange func(t *testing.T) string
	}{
		{name: "a fresh file is created with it", arrange: func(t *testing.T) string {
			s, _ := fileStore(t)
			return s.path
		}},
		{name: "a file from before it, at this build's layout, is given it", arrange: func(t *testing.T) string {
			path := fixtureAt(t, LayoutVersion)
			handle, err := Open(context.Background(), path, WithUnsyncedWrites())
			if err != nil {
				t.Fatalf("Open: %v", err)
			}
			t.Cleanup(func() { handle.Close() })
			return path
		}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			path := tt.arrange(t)

			// Assert
			if got := rawScalar[int](t, path, `SELECT count(*) FROM pragma_table_info('layout') WHERE name = 'additive_through'`); got != 1 {
				t.Fatalf("the layout table has %d additive_through column(s), want 1", got)
			}
			if want := (layoutStamp{LayoutVersion, LayoutVersion}); rawStamp(t, path) != want {
				t.Fatalf("the file stands at %+v, want %+v", rawStamp(t, path), want)
			}
		})
	}
}

func TestAFileWhoseAdditiveStepsPassThisBuildIsADowngrade(t *testing.T) {
	opens := []struct {
		name string
		open func(path string) (DB, error)
	}{
		{name: "a writing open", open: func(path string) (DB, error) {
			return Open(context.Background(), path, WithUnsyncedWrites())
		}},
		{name: "a joining open", open: func(path string) (DB, error) {
			return OpenJoining(context.Background(), path, WithUnsyncedWrites())
		}},
		{name: "a read-only open", open: func(path string) (DB, error) {
			return OpenReadOnly(context.Background(), path, WithUnsyncedWrites())
		}},
	}
	for _, tt := range opens {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange: a newer build joined, and its contracts are owed.
			path := writableStore(t)
			execRaw(t, path, `UPDATE layout SET version = ?, additive_through = ?`, LayoutVersion-1, LayoutVersion+1)
			stood := rawStamp(t, path)

			// Act
			handle, err := tt.open(path)
			if err == nil {
				handle.Close()
			}

			// Assert
			var refusal *LayoutError
			if !errors.As(err, &refusal) || !strings.Contains(refusal.Reason, "a downgrade is not a migration") {
				t.Fatalf("open = %v, want a *LayoutError refusing the downgrade", err)
			}
			if after := rawStamp(t, path); after != stood {
				t.Fatalf("the refused file moved from %+v to %+v, want it left as found", stood, after)
			}
		})
	}
}

func TestApplyContractsRefusesAFileThatOwesMoreThanContracts(t *testing.T) {
	// Arrange: the build's last step is additive, and has not run.
	s, log := fileStore(t)
	markLastStepAdditive(t)
	corrupt(t, s, `UPDATE layout SET version = ?, additive_through = ?`, LayoutVersion-1, LayoutVersion-1)

	// Act
	err := s.ApplyContracts(context.Background())

	// Assert
	var refusal *LayoutError
	if !errors.As(err, &refusal) || !strings.Contains(refusal.Reason, "not contract steps alone") {
		t.Fatalf("ApplyContracts = %v, want a *LayoutError saying the owed steps are not contracts alone", err)
	}
	if got := rawStamp(t, s.path); got != (layoutStamp{LayoutVersion - 1, LayoutVersion - 1}) {
		t.Fatalf("the refused file stands at %+v, want it left as found", got)
	}
	if _, ok := recordFor(log, "daemon.wsm.open", "error", "refused a state database with a foreign layout version"); !ok {
		t.Fatalf("no record of the refusal was written")
	}
}

// markLastStepAdditive has this build's last step be an additive one.
func markLastStepAdditive(t *testing.T) {
	t.Helper()
	last := len(migrations) - 1
	withStepKinds(t, func(i int, m migration) MigrationKind {
		if i == last {
			return MigrationAdditive
		}
		return m.Kind
	})
}

// contractRemoval matches the only statements a contract step may hold.
var contractRemoval = regexp.MustCompile(`(?is)^(DROP\s+(TABLE|INDEX|TRIGGER|VIEW)\s|DELETE\s+FROM\s|ALTER\s+TABLE\s+\S+\s+DROP\s+COLUMN\s)`)

// A CONTRACT STEP ONLY REMOVES, which is what lets a join apply every other
// step past the contracts it owes: a step that adds a shape the build reads,
// or rewrites rows, is additive (2026-11-19: deploy_ignored_records, a new
// table, was marked a contract and was missing when the build first read it).
func TestAContractStepOnlyRemoves(t *testing.T) {
	for _, m := range migrations {
		if m.Kind != MigrationContract {
			continue
		}
		if m.Rewrite != nil {
			t.Errorf("contract step %d (%s) carries a Rewrite; a contract step only removes", m.To, m.Name)
		}
		for _, statement := range strings.Split(m.DDL, ";") {
			statement = strings.TrimSpace(statement)
			if statement != "" && !contractRemoval.MatchString(statement) {
				t.Errorf("contract step %d (%s) holds a statement that is no removal: %q", m.To, m.Name, statement)
			}
		}
	}
}

func TestTheContractRemovalGuardRecognizesWhatIsNoRemoval(t *testing.T) {
	tests := []struct {
		statement string
		want      bool
	}{
		{"DROP TABLE deploy_problems", true},
		{"drop index some_index", true},
		{"ALTER TABLE merge_queue_repos DROP COLUMN mediation_intake", true},
		{"DELETE FROM deploy_remediation_finishes", true},
		{"CREATE TABLE deploy_ignored_records (operation TEXT)", false},
		{"ALTER TABLE merge_queue_repos ADD COLUMN told INTEGER", false},
		{"ALTER TABLE merge_queue_repos RENAME COLUMN a TO b", false},
		{"UPDATE merge_tab_intervals SET held_since = NULL", false},
		{"INSERT INTO deploy_ignored_records VALUES ('a')", false},
	}
	for _, tt := range tests {
		t.Run(tt.statement, func(t *testing.T) {
			if got := contractRemoval.MatchString(tt.statement); got != tt.want {
				t.Fatalf("contractRemoval matches %q = %v, want %v", tt.statement, got, tt.want)
			}
		})
	}
}

// schemaOf answers every table of the file at path with its columns.
func schemaOf(t *testing.T, path string) map[string][]string {
	t.Helper()
	out := map[string][]string{}
	withRawDB(t, path, func(db *sql.DB) {
		rows, err := db.Query(`SELECT m.name, p.name FROM sqlite_master AS m JOIN pragma_table_info(m.name) AS p
			WHERE m.type = 'table' ORDER BY m.name, p.name`)
		if err != nil {
			t.Fatalf("read the schema of %q: %v", path, err)
		}
		defer rows.Close()
		for rows.Next() {
			var table, column string
			if err := rows.Scan(&table, &column); err != nil {
				t.Fatalf("scan the schema of %q: %v", path, err)
			}
			out[table] = append(out[table], column)
		}
		if err := rows.Err(); err != nil {
			t.Fatalf("read the schema of %q: %v", path, err)
		}
	})
	return out
}

// contractSteps answers this build's contract steps past the last breaking
// one: the steps a handover can meet owed.
func contractSteps() []migration {
	var out []migration
	for _, m := range migrations {
		if m.Kind == MigrationContract && m.To > lastBreakingLayout {
			out = append(out, m)
		}
	}
	return out
}

// THE INVARIANT A HANDOVER RESTS ON: a joined file holds every table and
// column of a fresh one, whatever contracts it still owes, so nothing the
// build reads is missing while its incumbent serves.
func TestAJoinedFileHoldsEveryShapeOfAFreshOne(t *testing.T) {
	fresh := schemaOf(t, writableStore(t))
	for _, c := range contractSteps() {
		t.Run("joined from just before "+c.Name, func(t *testing.T) {
			// Arrange
			path := fixtureAt(t, c.To-1)

			// Act
			joined, err := OpenJoining(context.Background(), path, WithUnsyncedWrites())
			if err != nil {
				t.Fatalf("OpenJoining: %v", err)
			}
			t.Cleanup(func() { joined.Close() })

			// Assert
			got := schemaOf(t, path)
			for table, columns := range fresh {
				for _, column := range columns {
					// THE STAMP'S OWN COLUMN is the one shape the build
					// reads through a probe (readLayoutStamp), so a file a
					// join had no step to run on may still lack it.
					if table == "layout" && column == "additive_through" {
						continue
					}
					if !slices.Contains(got[table], column) {
						t.Errorf("the joined file lacks %s.%s, which a fresh file holds", table, column)
					}
				}
			}
		})
	}
}

// A JOIN AND A TAKEOVER END WHERE A COLD OPEN DOES: the steps a join runs
// past its owed contracts, and the contracts the takeover then runs, leave
// the schema the list's own order leaves.
func TestAJoinThenATakeoverLeavesTheSchemaOfAColdOpen(t *testing.T) {
	for _, c := range contractSteps() {
		t.Run("from just before "+c.Name, func(t *testing.T) {
			// Arrange
			cold, joinedPath := fixtureAt(t, c.To-1), fixtureAt(t, c.To-1)

			// Act
			handle, err := Open(context.Background(), cold, WithUnsyncedWrites())
			if err != nil {
				t.Fatalf("Open: %v", err)
			}
			t.Cleanup(func() { handle.Close() })
			joined, err := OpenJoining(context.Background(), joinedPath, WithUnsyncedWrites())
			if err != nil {
				t.Fatalf("OpenJoining: %v", err)
			}
			t.Cleanup(func() { joined.Close() })
			if err := joined.Promote(context.Background()); err != nil {
				t.Fatalf("Promote: %v", err)
			}
			if err := joined.ApplyContracts(context.Background()); err != nil {
				t.Fatalf("ApplyContracts: %v", err)
			}

			// Assert
			want, got := schemaOf(t, cold), schemaOf(t, joinedPath)
			if len(want) != len(got) {
				t.Fatalf("the joined file has %d tables, the cold one %d", len(got), len(want))
			}
			for table, columns := range want {
				if !slices.Equal(got[table], columns) {
					t.Errorf("table %s: joined %v, cold %v", table, got[table], columns)
				}
			}
			if coldStamp, joinedStamp := rawStamp(t, cold), rawStamp(t, joinedPath); coldStamp != joinedStamp || coldStamp.Version != LayoutVersion {
				t.Fatalf("stamps: cold %+v, joined %+v; want both at layout %d with nothing owed", coldStamp, joinedStamp, LayoutVersion)
			}
		})
	}
}

func TestOpenMigratesNothingOnAFileWhoseEveryStepRan(t *testing.T) {
	// Arrange: every step ran, and an older build left version behind them.
	markLastStepAdditive(t)
	path := writableStore(t)
	execRaw(t, path, `UPDATE layout SET version = ?, additive_through = ?`, LayoutVersion-1, LayoutVersion)

	// Act
	handle, err := Open(context.Background(), path, WithUnsyncedWrites())
	if err == nil {
		handle.Close()
	}

	// Assert
	if err != nil {
		t.Fatalf("Open: %v", err)
	}
	if copies := backupsBeside(t, path); len(copies) != 0 {
		t.Fatalf("copies = %v, want nothing copied aside for a file with no step to run", copies)
	}
}
