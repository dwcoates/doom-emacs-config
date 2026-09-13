package convert

// foreignspawn.go — A RUN THIS STREAM DID NOT LAUNCH, BUT MAY STILL STOP.
//
// The vendor's background agents OUTLIVE THE TRANSCRIPT THAT LAUNCHED THEM. A
// `/clear` (and a resume, and a fork) rotates the session id and starts a NEW
// transcript file while the harness process keeps every running background
// agent — so the very next thing that file may say about such a run is the
// TaskStop that settles it, with the launch sitting in the PREVIOUS file.
//
// THE VENDOR STATES WHERE THE LAUNCH LIVES, and it states it in the one place
// this converter can reach: the task-notification records it writes about a
// background agent name the run's SPOOL, and the spool sits under the session
// directory of the session that launched it —
// `<tmp>/<project>/<launching session uuid>/tasks/<task id>.output`. When that
// session is not this file's own, the launch was written to another stream and
// no amount of reading THIS file could have produced it.
//
// Observed on the owner's machine 2026-09-13: five TaskStop results at offset
// ~50 MB of `6a1b0e3a-….jsonl` — a transcript read FROM BYTE 0, so the
// join-offset carve-out did not apply — each naming a task whose spool the
// vendor had placed under `37dc1374-…/tasks/`. Five warnings for the ordinary
// shape of background agents surviving a `/clear`.

import (
	"path"
	"strings"
)

// taskNotificationTag brackets the fields a task-notification carries.
const (
	taskIDOpen      = "<task-id>"
	taskIDClose     = "</task-id>"
	outputFileOpen  = "<output-file>"
	outputFileClose = "</output-file>"
)

// spoolDirName is the directory the vendor puts a session's task spools in. It
// is what makes the segment above it the LAUNCHING SESSION rather than some
// other path component that happens to sit two levels up.
const spoolDirName = "tasks"

// noteForeignSpawn records a task whose spool the vendor placed under a session
// OTHER than this file's, so a later TaskStop for it can say the launch was
// never this reader's to see.
//
// It reads only records that carry a task-notification, and it concludes
// nothing when either session id is unknown: an unknown owner is not a foreign
// one.
func (c *Converter) noteForeignSpawn(record map[string]any, at Attribution) {
	if at.VendorSessionID == "" {
		return
	}
	text := taskNotificationText(record)
	if text == "" {
		return
	}
	taskID := betweenTags(text, taskIDOpen, taskIDClose)
	spool := betweenTags(text, outputFileOpen, outputFileClose)
	if taskID == "" || spool == "" {
		return
	}
	owner := spoolOwningSession(spool)
	if owner == "" || owner == at.VendorSessionID {
		return
	}
	if _, known := c.foreignSpawns[taskID]; known {
		return
	}
	c.foreignSpawns[taskID] = owner
	c.log.With(at.ctxFor("foreign-spawn")).
		LogVerbose("task %s was launched by session %s, whose transcript is a different file; a stop for it here has no launch to join", taskID, owner)
}

// taskNotificationText returns the task-notification body a record carries, or
// "" for a record that carries none.
//
// TWO CARRIERS, ONE BODY. The harness writes the same notification twice: once
// as a `queue-operation` naming what it queued, and once as the `attachment`
// that delivers it. Reading both is what makes the fact available whichever of
// the two this reader happens to have in its window.
func taskNotificationText(record map[string]any) string {
	var text string
	switch str(record["type"]) {
	case "queue-operation":
		text = str(record["content"])
	case "attachment":
		text = str(obj(record["attachment"])["prompt"])
	default:
		return ""
	}
	if !strings.Contains(text, taskIDOpen) {
		return ""
	}
	return text
}

// spoolOwningSession names the session whose directory a task spool lives in,
// or "" for a path that is not shaped like one.
func spoolOwningSession(spool string) string {
	dir := path.Dir(spool)
	if path.Base(dir) != spoolDirName {
		return ""
	}
	owner := path.Base(path.Dir(dir))
	if owner == "." || owner == "/" {
		return ""
	}
	return owner
}

// betweenTags returns the text between the first open tag and the close tag
// that follows it, or "" when the element is not there.
func betweenTags(s, open, closing string) string {
	i := strings.Index(s, open)
	if i < 0 {
		return ""
	}
	rest := s[i+len(open):]
	j := strings.Index(rest, closing)
	if j < 0 {
		return ""
	}
	return strings.TrimSpace(rest[:j])
}
