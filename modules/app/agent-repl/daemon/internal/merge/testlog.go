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
	"agent-repld/internal/refusalarm"
	"agent-repld/internal/wsm"
)

// This file is a merge's TEST LOG as a link: the token the tests tab serves,
// the label it draws, and the resolution OpenInEditor makes of a token handed
// back.
//
// THE TOKEN IS DERIVED, NOT REMEMBERED. It spells the merge's lease and the
// tests round, which is exactly what names the log file under the state root,
// so a daemon that restarted -- or a successor that took over -- resolves a
// token the bubble served before it, for as long as the log is there. A token
// is resolved FOR ONE WORKSPACE: the FEED THAT DRAWS ITS MERGE'S BUBBLE. The
// lease's durable ledger names the workspace that ran the merge, and bubbleOf
// names the feed its bubble is drawn in -- the workspace itself, or for an
// async merge the workspace that requested it -- so every link the daemon drew
// opens from where it was drawn, and a token cannot open a log whose bubble the
// asking feed does not hold.

// mergeLogsDir is where every log the queue writes lives, under the state
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
		path:  filepath.Join(LogsDir(o.deps.StateDir), fmt.Sprintf("%s-tests-%d.log", lease, round)),
	}
}

// LogsDir is the directory every log the queue writes lives in, under the
// state root stateDir: each gate run's output, each script hook round's, and
// each released deploy hook's. The daemon's log retention pass prunes it
// (dlog.Surfaces.RetainDirectory) by IsLog.
func LogsDir(stateDir string) string { return filepath.Join(stateDir, mergeLogsDir) }

// logName matches the basename of every log the queue writes: a tests round's
// (testLog), a script hook round's (hookLogName) and a released deploy hook's
// (releaseLogName).
var logName = regexp.MustCompile(`^(?:[0-9a-f]+-(?:tests|pre_deploy|deploy|post_deploy)-[0-9]+|release-[0-9]+)\.log$`)

// IsLog answers whether base names a log the queue writes.
func IsLog(base string) bool { return logName.MatchString(base) }

// createLog creates (or truncates) one of the queue's logs, its directory
// first, under the shared lock the log retention pass honors, so a log is
// never pruned while it is being written.
func createLog(path string) (*os.File, error) {
	if err := os.MkdirAll(filepath.Dir(path), 0o755); err != nil {
		return nil, fmt.Errorf("merge: creating the log directory %s: %w", filepath.Dir(path), err)
	}
	file, err := logging.CreateLocked(path)
	if err != nil {
		return nil, fmt.Errorf("merge: creating the log %s: %w", path, err)
	}
	return file, nil
}

// writeLog writes one of the queue's logs whole, at once.
func writeLog(path, body string) error {
	file, err := createLog(path)
	if err != nil {
		return err
	}
	if _, err := file.WriteString(body); err != nil {
		return errors.Join(fmt.Errorf("merge: writing the log %s: %w", path, err), file.Close())
	}
	if err := file.Close(); err != nil {
		return fmt.Errorf("merge: writing the log %s: %w", path, err)
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

// The reasons a test log token is refused, spelled as the
// agentrepl.v1.OpenInEditorUnknownMergeTestLog.reason arms.
const (
	reasonTestLogMalformed    = refusalarm.Nested("malformed")
	reasonTestLogNotDrawnHere = refusalarm.Nested("not_drawn_here")
	reasonTestLogAgedOut      = refusalarm.Nested("aged_out")
)

// TestLogPath resolves a token the tests tab served to the log's path, for
// the workspace whose feed the link was clicked in. A token that is malformed,
// names a merge whose bubble that feed does not draw, or names a log that is no
// longer there is REFUSED (unknown_merge_test_log), its `reason` naming which.
// The first two are recorded at WARN. A log of a merge the feed draws that is
// gone is recorded at INFO: the log retention pass removes every test log
// three days after its last write, so a link the feed drew before then
// outliving its file is the ordinary course.
func (o *orchestrator) TestLogPath(ctx context.Context, ws ids.WorkspaceID, token string) (string, error) {
	const op = "daemon.merge.test_log"
	log := o.log(ctx, ws)
	refusal := func(why string, reason refusalarm.Nested, fields dlog.Context) error {
		record := dlog.Context{"workspace": string(ws), "token": token, "why": why}
		for name, value := range fields {
			record[name] = value
		}
		log.Warn(op, "refused a test log token", record)
		return &RefusalError{Arm: ArmUnknownMergeTestLog, Workspace: ws, Reason: why, Fields: map[string]any{"reason": reason}}
	}
	lease, n, found := strings.Cut(token, "/")
	round, err := strconv.Atoi(n)
	if !found || lease == "" || err != nil || round < 1 {
		return "", refusal("the token is not one this daemon mints", reasonTestLogMalformed, nil)
	}
	ran, err := o.deps.DB.MergeLedgerWorkspace(ctx, ids.LeaseID(lease))
	if errors.Is(err, wsm.ErrNotFound) {
		return "", refusal("the token names no merge this daemon's ledger holds", reasonTestLogNotDrawnHere, nil)
	}
	if err != nil {
		log.Error(op, "could not read the merge ledger to resolve a test log token",
			dlog.Context{"workspace": string(ws), "token": token, "error": err.Error()})
		return "", err
	}
	if drawnIn := o.bubbleOf(ran).ws; drawnIn != ws {
		return "", refusal("the token names a merge whose bubble another workspace's feed draws", reasonTestLogNotDrawnHere,
			dlog.Context{"merge_workspace": string(ran), "drawn_in": string(drawnIn)})
	}
	path := o.testLog(ids.LeaseID(lease), round).path
	if _, err := os.Stat(path); err != nil {
		if errors.Is(err, os.ErrNotExist) {
			why := fmt.Sprintf("the log %s is no longer there", path)
			log.Info(op, "refused a test log token whose log aged out of the log retention", dlog.Context{
				"workspace": string(ws), "token": token, "why": why, "retention": logging.DefaultRetention.String()})
			return "", &RefusalError{Arm: ArmUnknownMergeTestLog, Workspace: ws, Reason: why,
				Fields: map[string]any{"reason": reasonTestLogAgedOut}}
		}
		log.Error(op, "could not read a test log's file", dlog.Context{"workspace": string(ws), "path": path, "error": err.Error()})
		return "", err
	}
	log.Debug(op, "resolved a test log token", dlog.Context{"workspace": string(ws), "token": token, "path": path})
	return path, nil
}
