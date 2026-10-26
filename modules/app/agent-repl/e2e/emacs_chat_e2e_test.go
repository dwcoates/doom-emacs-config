// emacs_chat_e2e_test.go — zettel chat threads from the real Emacs: the
// bindings, the new-thread prompt, the thread's workspace off the tab bar,
// the tab bar kept while a thread is on screen, the roster's thread rows
// decoded by the strict codec, and an archived thread leaving Emacs.
//
// ONE SCENARIO, MANY FACTS (owner ruling, 2026-10-24: chat e2e tests are few
// and packed), asserted in the order a user meets them.
package e2e

import (
	"context"
	"encoding/json"
	"strings"
	"testing"

	"connectrpc.com/connect"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	workspacev1 "agentrepl/proto/workspace/v1"

	"agent-repld/integration/grantedclock"
	"agent-repld/integration/harness"
)

// emacsThreadPrefix is the prefix of a thread's Emacs workspace name, which
// is its id, never its title (roster.el's `agent-repl-roster-thread-workspace-name').
const emacsThreadPrefix = "chat/"

// truthy reports whether an evaluated form answered non-nil.
func truthy(raw json.RawMessage) bool { return !isJSONNull(raw) && string(raw) != "false" }

// awaitAnswered waits for THREAD's chat view to show an answer saying WORDS,
// read from the daemon at ADDR.
func awaitAnswered(t *testing.T, addr string, thread *workspacev1.WorkspaceRef, words string) {
	t.Helper()
	ctx, cancel := grantedclock.WithBound(context.Background(), emacsVerbBound)
	defer cancel()
	stream, err := harness.DialAt(t, addr).WatchChat(ctx, connect.NewRequest(&agentreplv1.WatchChatRequest{Thread: thread}))
	if err != nil {
		t.Fatalf("WatchChat: %v", err)
	}
	defer stream.Close()
	for stream.Receive() {
		for _, ex := range stream.Msg().GetView().GetConversation().GetExchanges() {
			if strings.Contains(ex.GetAnswer().GetMarkdown().GetText(), words) {
				return
			}
		}
	}
	t.Fatalf("the chat view never showed an answer saying %q: %v", words, stream.Err())
}

func TestEmacsReachesAChatThreadOffTheTabBar(t *testing.T) {
	t.Parallel()
	box := requireSandbox(t)
	w := NewEmacsWorld(t, box, WithZettel())
	e := w.Emacs
	e.EnsureDaemon()

	// ---- The three thread keys are bound under SPC TAB ----
	for key, want := range map[string]string{
		"TAB t": "agent-repl-thread-new", "TAB T": "agent-repl-thread-recent", "TAB .": "agent-repl-thread-pick",
	} {
		if got := e.LeaderBinding(key); got != want {
			t.Fatalf("SPC %s resolves to %q, want %q", key, got, want)
		}
	}

	// ---- SPC TAB t asks the question, and the new thread opens ----
	e.Eval(`(cl-letf (((symbol-function 'agent-repl-verbs--read-required-prompt)
                        (lambda (&rest _) "Why is the sky blue?")))
               (call-interactively #'agent-repl-thread-new)
               t)`)
	// The question is its own abbreviation, so it names the thread at once;
	// the row carries it, and its workspace (named by its id) is open and
	// marked a thread.
	var threadID string
	e.AwaitEvalFor(emacsVerbBound, "the named thread's workspace, marked a thread",
		`(let ((row (car (agent-repl-roster-threads))))
           (and row (equal (plist-get row :name) `+elispString("Why is the sky blue?")+`)
                (agent-repl-roster-thread-p (agent-repl-roster-thread-workspace-name row))
                (plist-get (plist-get row :thread) :id)))`,
		func(raw json.RawMessage) bool {
			return json.Unmarshal(raw, &threadID) == nil && threadID != ""
		})
	emacsThreadName := emacsThreadPrefix + threadID
	e.AwaitEvalFor(emacsTabOpenBound, "the frame moved onto the new thread",
		`(agent-repl--ws-current-name)`,
		func(raw json.RawMessage) bool {
			var name string
			return json.Unmarshal(raw, &name) == nil && name == emacsThreadName
		})
	if order := decodeStrings(e.Eval(emacsTabOrderForm)); containsString(order, emacsThreadName) {
		t.Fatalf("tab order = %v, want no tab for the thread", order)
	}
	threadDir := decodeStrings(e.Eval(`(list (plist-get (plist-get (car (agent-repl-roster-threads)) :thread) :dir))`))[0]
	thread := &workspacev1.WorkspaceRef{Id: threadID, Dir: threadDir}
	awaitAnswered(t, e.DaemonAddr(), thread, "echo: Why is the sky blue?")

	// ---- The tab bar stays while the thread is on screen ----
	// The wait reads each input of the rule (status.el,
	// agent-repl--tabbar-target-rows), so a miss names the one that is wrong.
	e.AwaitEvalFor(emacsVerbBound, "the tab bar kept over the thread's webview",
		`(vector (and (agent-repl--tabbar-webview-shown-p (selected-frame)) t)
                 (and (agent-repl-roster-thread-p (agent-repl--ws-frame-name (selected-frame))) t)
                 (or (frame-parameter nil 'tab-bar-lines) 0))`,
		func(raw json.RawMessage) bool {
			// [webview shown, frame's workspace a thread, tab-bar rows]
			var got []any
			return json.Unmarshal(raw, &got) == nil && len(got) == 3 &&
				got[0] == true && got[1] == true && got[2] != float64(0)
		})

	// ---- SPC TAB . offers it by name ----
	e.Eval(`(cl-letf (((symbol-function 'completing-read)
                        (lambda (_prompt collection &rest _)
                          (car (all-completions "" collection)))))
               (call-interactively #'agent-repl-thread-pick)
               t)`)
	if got := decodeStrings(e.Eval(`(list (agent-repl--ws-current-name))`)); got[0] != emacsThreadName {
		t.Fatalf("after SPC TAB . the frame shows %v, want the thread", got)
	}

	// ---- Archived, the thread leaves Emacs and no command offers it ----
	if resp, err := harness.DialAt(t, e.DaemonAddr()).CloseWorkspace(context.Background(), connect.NewRequest(&agentreplv1.CloseWorkspaceRequest{
		Workspace: thread, Requester: harness.User(),
	})); err != nil || resp.Msg.GetSuccess() == nil {
		t.Fatalf("CloseWorkspace = %v, %v", resp, err)
	}
	e.AwaitEvalFor(emacsVerbBound, "the archived thread offered to no command",
		`(null (agent-repl-roster-threads))`, truthy)
	e.AwaitEvalFor(emacsVerbBound, "the archived thread's workspace torn down",
		`(null (agent-repl--ws-by-ref-id `+elispString(threadID)+`))`, truthy)
}
