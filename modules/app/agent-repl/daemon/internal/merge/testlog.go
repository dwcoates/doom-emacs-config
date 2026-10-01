package merge

import (
	"context"
	"errors"
	"fmt"
	"os"
	"path/filepath"
	"strconv"
	"strings"

	frontendv1 "agentrepl/proto/frontend/v1"

	"claude-repld/internal/dlog"
	"claude-repld/internal/ids"
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
		path:  filepath.Join(o.deps.StateDir, mergeLogsDir, fmt.Sprintf("%s-tests-%d.log", lease, round)),
	}
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
// (unknown_merge_test_log) and recorded at WARN.
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
			return "", refusal(fmt.Sprintf("the log %s is no longer there", path))
		}
		log.Error(op, "could not read a test log's file", dlog.Context{"workspace": string(ws), "path": path, "error": err.Error()})
		return "", err
	}
	log.Debug(op, "resolved a test log token", dlog.Context{"workspace": string(ws), "token": token, "path": path})
	return path, nil
}
