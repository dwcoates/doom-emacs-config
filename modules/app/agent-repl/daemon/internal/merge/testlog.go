package merge

import (
	"context"
	"errors"
	"fmt"
	"os"
	"path/filepath"
	"regexp"
	"strconv"
	"strings"

	"agentrepl/logging"
	frontendv1 "agentrepl/proto/frontend/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
)

// This file is a merge's TEST LOG as a link: the token the tests tab serves,
// the label it draws, and the resolution OpenInEditor makes of a token handed
// back.
//
// THE TOKEN IS DERIVED, NOT REMEMBERED. It spells the merge's lease and the
// tests round, which is exactly what names the log file under the state root,
// so a daemon that restarted -- or a successor that took over -- resolves a
// token the bubble served before it, for as long as the log is there. A token
// is resolved FOR ONE WORKSPACE: its lease must be one of that workspace's own
// merges, read off the workspace's durable merge ledger, so a token cannot open
// another workspace's log.

// mergeLogsDir is where every gate run's output is written, under the state
// root.
const mergeLogsDir = "merge-logs"

// testLog is one tests round's log: its token and its path.
type testLog struct {
	token string
	path  string
}

// testLog names one tests round's log.
func (o *orchestrator) testLog(lease ids.LeaseID, round int) testLog {
	return testLog{
		token: fmt.Sprintf("%s/%d", lease, round),
		path:  filepath.Join(TestLogsDir(o.deps.StateDir), fmt.Sprintf("%s-tests-%d.log", lease, round)),
	}
}

// TestLogsDir is the directory every merge's test logs are written to, under
// the state root stateDir. The daemon's log retention pass prunes it
// (dlog.Surfaces.RetainDirectory) by IsTestLog.
func TestLogsDir(stateDir string) string { return filepath.Join(stateDir, mergeLogsDir) }

// testLogName matches the basename testLog gives a round's log.
var testLogName = regexp.MustCompile(`^[0-9a-f]+-tests-[0-9]+\.log$`)

// IsTestLog answers whether base names a round's test log.
func IsTestLog(base string) bool { return testLogName.MatchString(base) }

// createTestLog creates (or truncates) one round's log, its directory first,
// under the shared lock the log retention pass honors, so a log is never
// pruned while it is being written.
func createTestLog(log testLog) (*os.File, error) {
	if err := os.MkdirAll(filepath.Dir(log.path), 0o755); err != nil {
		return nil, fmt.Errorf("merge: creating the test log directory %s: %w", filepath.Dir(log.path), err)
	}
	file, err := logging.CreateLocked(log.path)
	if err != nil {
		return nil, fmt.Errorf("merge: creating the test log %s: %w", log.path, err)
	}
	return file, nil
}

// writeTestLog writes one round's whole log at once.
func writeTestLog(log testLog, body string) error {
	file, err := createTestLog(log)
	if err != nil {
		return err
	}
	if _, err := file.WriteString(body); err != nil {
		return errors.Join(fmt.Errorf("merge: writing the test log %s: %w", log.path, err), file.Close())
	}
	if err := file.Close(); err != nil {
		return fmt.Errorf("merge: writing the test log %s: %w", log.path, err)
	}
	return nil
}

// testLogLink is the tests tab's link to one log: the token, and the path with
// the home directory shortened to ~.
func (o *orchestrator) testLogLink(log testLog) *frontendv1.FeedMergeTestLog {
	return &frontendv1.FeedMergeTestLog{
		Token: &frontendv1.FeedMergeTestLogToken{Value: log.token},
		Label: &frontendv1.FeedMergeTestLogLabel{Text: tildePath(log.path, o.deps.Home)},
	}
}

// tildePath shortens a path inside home to start with ~.
func tildePath(path, home string) string {
	if home == "" {
		return path
	}
	if path == home {
		return "~"
	}
	if rest, inside := strings.CutPrefix(path, home+string(filepath.Separator)); inside {
		return filepath.Join("~", rest)
	}
	return path
}

// TestLogPath resolves a token the tests tab served to the log's path, for
// one workspace. A token that is malformed, names a merge that is not the
// workspace's own, or names a log that is no longer there is REFUSED
// (unknown_merge_test_log). The first two are recorded at WARN. A log of the
// workspace's own merge that is gone is recorded at INFO: the log retention
// pass removes every test log three days after its last write, so a link the
// feed drew before then outliving its file is the ordinary course.
func (o *orchestrator) TestLogPath(ctx context.Context, ws ids.WorkspaceID, token string) (string, error) {
	const op = "daemon.merge.test_log"
	log := o.log(ctx, ws)
	refusal := func(why string) error {
		log.Warn(op, "refused a test log token", dlog.Context{"workspace": string(ws), "token": token, "why": why})
		return &RefusalError{Arm: ArmUnknownMergeTestLog, Workspace: ws, Reason: why}
	}
	lease, n, found := strings.Cut(token, "/")
	round, err := strconv.Atoi(n)
	if !found || lease == "" || err != nil || round < 1 {
		return "", refusal("the token is not one this daemon mints")
	}
	entries, err := o.deps.DB.MergeLedger(ctx, ws)
	if err != nil {
		log.Error(op, "could not read the workspace's merge ledger to resolve a test log token",
			dlog.Context{"workspace": string(ws), "token": token, "error": err.Error()})
		return "", err
	}
	owned := false
	for _, entry := range entries {
		if string(entry.Lease) == lease {
			owned = true
			break
		}
	}
	if !owned {
		return "", refusal("the token names no merge of this workspace")
	}
	path := o.testLog(ids.LeaseID(lease), round).path
	if _, err := os.Stat(path); err != nil {
		if errors.Is(err, os.ErrNotExist) {
			why := fmt.Sprintf("the log %s is no longer there", path)
			log.Info(op, "refused a test log token whose log aged out of the log retention", dlog.Context{
				"workspace": string(ws), "token": token, "why": why, "retention": logging.DefaultRetention.String()})
			return "", &RefusalError{Arm: ArmUnknownMergeTestLog, Workspace: ws, Reason: why}
		}
		log.Error(op, "could not read a test log's file", dlog.Context{"workspace": string(ws), "path": path, "error": err.Error()})
		return "", err
	}
	log.Debug(op, "resolved a test log token", dlog.Context{"workspace": string(ws), "token": token, "path": path})
	return path, nil
}
