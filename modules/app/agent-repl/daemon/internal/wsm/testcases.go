package wsm

import (
	"context"
	"database/sql"
	"errors"
	"fmt"

	"google.golang.org/protobuf/proto"

	testrunv1 "agentrepl/proto/testrun/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/testcases"
)

// A WORKSPACE'S RECORDED TEST CASES (`agent-repld test run -rerun`): every
// case any test run of the workspace named, and every failing case a merge
// queue gate read off its branch, each once. The record is the branch's
// regression set: a case that passes stays in it, and a case leaves it only
// when a rerun finds the tree no longer holds it. It lives as long as its
// workspace does (the foreign key), across restarts and handovers.

// testCasesDDL is the table, as both a fresh file and the layout-190 step
// create it. case_key is the case's identity (testcases.Key), so a case
// recorded twice is one row; selection is the case as a
// testrun.v1.TestSelection, which a rerun sends as it was recorded.
const testCasesDDL = `
CREATE TABLE workspace_test_cases (
  workspace_id TEXT NOT NULL REFERENCES workspaces(id) ON DELETE CASCADE,
  case_key     TEXT NOT NULL,
  selection    BLOB NOT NULL,
  recorded_at  INTEGER NOT NULL,
  PRIMARY KEY (workspace_id, case_key)
);
`

// errNoCases refuses a record or a forgetting of no case: the caller always
// has one, so none is its defect.
var errNoCases = errors.New("a workspace's test cases are recorded or forgotten some at a time, never none")

// RecordTestCases adds CASES (each naming one case, testcases.Of) to the
// workspace's record, at AT (unix milliseconds); a case already recorded
// keeps its row. An unknown workspace is ErrNotFound.
func (s *store) RecordTestCases(ctx context.Context, id WorkspaceID, cases []*testrunv1.TestSelection, at int64) error {
	const op = "daemon.wsm.record_test_cases"
	fields := dlog.Context{"workspace": string(id), "cases": len(cases)}
	if len(cases) == 0 {
		s.log.Error(op, "refused to record no test case", withError(fields, errNoCases))
		return errNoCases
	}
	rows := make([][]byte, len(cases))
	for i, c := range cases {
		b, err := proto.MarshalOptions{Deterministic: true}.Marshal(c)
		if err != nil {
			err = fmt.Errorf("encode test case %s: %w", testcases.Key(c), err)
			s.log.Error(op, "a test case could not be encoded", withError(fields, err))
			return err
		}
		rows[i] = b
	}
	return s.write(ctx, op, fields, func(ctx context.Context, tx *sql.Tx) error {
		var existing string
		if err := scanWorkspaceRow(ctx, tx, id, "id", &existing); err != nil {
			return err
		}
		for i, c := range cases {
			if _, err := tx.ExecContext(ctx,
				`INSERT INTO workspace_test_cases (workspace_id, case_key, selection, recorded_at) VALUES (?, ?, ?, ?)
				 ON CONFLICT(workspace_id, case_key) DO NOTHING`,
				id, testcases.Key(c), rows[i], at); err != nil {
				return err
			}
		}
		return nil
	})
}

// TestCasesOf loads the workspace's recorded cases in the order they were
// first recorded, all-or-nothing: a row that does not decode fails the whole
// read as a DecodeError naming its case.
func (s *store) TestCasesOf(ctx context.Context, id WorkspaceID) ([]*testrunv1.TestSelection, error) {
	var out []*testrunv1.TestSelection
	err := s.read(ctx, "daemon.wsm.test_cases_of", dlog.Context{"workspace": string(id)}, func(ctx context.Context) error {
		var err error
		out, err = listRows(ctx, s.q(ctx),
			`SELECT case_key, selection FROM workspace_test_cases WHERE workspace_id = ? ORDER BY recorded_at, rowid`,
			func(row interface{ Scan(...any) error }) (*testrunv1.TestSelection, error) {
				var key string
				var blob []byte
				if err := row.Scan(&key, &blob); err != nil {
					return nil, err
				}
				sel := &testrunv1.TestSelection{}
				if err := proto.Unmarshal(blob, sel); err != nil || sel.GetTarget() == nil {
					if err == nil {
						err = errors.New("the stored case names no target")
					}
					return nil, &DecodeError{Table: "workspace_test_cases", Row: string(id) + "/" + key, Err: err}
				}
				return sel, nil
			}, id)
		return err
	})
	if err != nil {
		return nil, err
	}
	return out, nil
}

// ForgetTestCases removes CASES from the workspace's record, answering how
// many stood: a case that was not recorded is no error.
func (s *store) ForgetTestCases(ctx context.Context, id WorkspaceID, cases []*testrunv1.TestSelection) (int, error) {
	const op = "daemon.wsm.forget_test_cases"
	fields := dlog.Context{"workspace": string(id), "cases": len(cases)}
	if len(cases) == 0 {
		s.log.Error(op, "refused to forget no test case", withError(fields, errNoCases))
		return 0, errNoCases
	}
	forgot := 0
	err := s.write(ctx, op, fields, func(ctx context.Context, tx *sql.Tx) error {
		for _, c := range cases {
			res, err := tx.ExecContext(ctx, `DELETE FROM workspace_test_cases WHERE workspace_id = ? AND case_key = ?`, id, testcases.Key(c))
			if err != nil {
				return err
			}
			n, err := res.RowsAffected()
			if err != nil {
				return err
			}
			forgot += int(n)
		}
		return nil
	})
	return forgot, err
}
