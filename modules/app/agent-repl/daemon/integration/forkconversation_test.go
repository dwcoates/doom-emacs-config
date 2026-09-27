//go:build integration

package integration

import (
	"os"
	"path/filepath"
	"strings"
	"testing"

	"connectrpc.com/connect"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"
	workspacev1 "agentrepl/proto/workspace/v1"

	"claude-repld/integration/harness"
)

// A FORK PORTS THE WHOLE CONVERSATION. The child's feed is the parent's
// conversation up to the fork point followed by the child's own turns, in one
// order — the parent's QUESTION included, which used to reach the child
// nowhere at all: the ported artifact was the vendor transcript, whose
// assistant lines reach a feed through the store, while the prompt bubbles
// come from the daemon's own per-workspace conversation rows.

// TestForkedWorkspaceFeedCarriesTheParentsQuestionAndAnswerBeforeItsOwn is the
// defect (a headless run's section 2, A.6) as an assertion.
func TestForkedWorkspaceFeedCarriesTheParentsQuestionAndAnswerBeforeItsOwn(t *testing.T) {
	t.Parallel()
	// Arrange: a parent that was asked something and answered it, with the
	// vendor transcript on disk that a real session would have left behind.
	f := newOpened(t, harness.Opts{})
	if req := f.shim.ExpectStartSession(); req.GetFresh() == nil {
		t.Fatalf("the parent's StartSession = %v, want fresh", req)
	}
	vendorID := f.shim.Info().VendorSessionID
	if vendorID == "" {
		t.Fatal("the fake shim reports no vendor session id after StartSession(fresh)")
	}
	f.submit(forkParentQuestion, "fork-parent-turn", conversationv1.PromptOrigin_PROMPT_ORIGIN_WEBAPP_USER_SENT)

	parentProject := createProjectDir(f.d.DefaultConfigDir, f.ws.GetDir())
	if err := os.MkdirAll(parentProject, 0o755); err != nil {
		t.Fatalf("mkdir the parent's project dir: %v", err)
	}
	if err := os.WriteFile(filepath.Join(parentProject, vendorID+".jsonl"),
		[]byte(`{"type":"summary"}`+"\n"), 0o644); err != nil {
		t.Fatalf("seed the parent's transcript: %v", err)
	}

	// Act: fork it, then let the ported conversation's ANSWER arrive on the
	// child's own watch — the route the ported transcript's assistant lines
	// take — and only then ask the fork its own first question.
	child := forkOf(t, f)
	childShim := f.d.Shim(child)
	if resume := childShim.ExpectStartSession().GetResume(); resume == nil {
		t.Fatal("the forked child's StartSession carries no resume")
	}
	kid := &fixture{d: f.d, repo: f.repo, ws: child, shim: childShim, t: t}
	childShim.PushAgentFrame(mainAgent, feedResponseFrames("fork-parent-answer", forkParentAnswer)[0])
	childShim.PushAgentFrame(mainAgent, feedResponseFrames("fork-parent-answer", forkParentAnswer)[1])
	kid.submit(forkOwnQuestion, "fork-child-turn", conversationv1.PromptOrigin_PROMPT_ORIGIN_WEBAPP_USER_SENT)

	// Assert: all three, in one order, with the parent's turn whole and above
	// the fork's own.
	page, _ := kid.openFeedOnceCarrying("the parent's turn and the fork's own",
		func(p *frontendv1.FeedPage) bool {
			texts := forkFeedTexts(p)
			return forkIndexOf(texts, forkParentQuestion) >= 0 &&
				forkIndexOf(texts, forkParentAnswer) >= 0 &&
				forkIndexOf(texts, forkOwnQuestion) >= 0
		})
	texts := forkFeedTexts(page)
	question, answer, own := forkIndexOf(texts, forkParentQuestion), forkIndexOf(texts, forkParentAnswer), forkIndexOf(texts, forkOwnQuestion)
	if !(question < answer && answer < own) {
		t.Fatalf("the fork's feed reads %v: the parent's question is at %d, its answer at %d and the fork's own question at %d, "+
			"want the parent's whole turn above the fork's own", texts, question, answer, own)
	}
}

// TestForkedWorkspaceFeedDrawsTheParentsQuestionUnderItsOwnTurnId covers the
// identity half: the ported row carries the CHILD's turn, never the parent's,
// because the same mapping that re-minted the transcript re-minted it.
func TestForkedWorkspaceFeedDrawsTheParentsQuestionUnderItsOwnTurnId(t *testing.T) {
	t.Parallel()
	// Arrange
	f := newOpened(t, harness.Opts{})
	if req := f.shim.ExpectStartSession(); req.GetFresh() == nil {
		t.Fatalf("the parent's StartSession = %v, want fresh", req)
	}
	vendorID := f.shim.Info().VendorSessionID
	f.submit(forkParentQuestion, "fork-id-turn", conversationv1.PromptOrigin_PROMPT_ORIGIN_WEBAPP_USER_SENT)
	parentTurn := forkTurnOf(t, f, forkParentQuestion)

	parentProject := createProjectDir(f.d.DefaultConfigDir, f.ws.GetDir())
	if err := os.MkdirAll(parentProject, 0o755); err != nil {
		t.Fatalf("mkdir the parent's project dir: %v", err)
	}
	if err := os.WriteFile(filepath.Join(parentProject, vendorID+".jsonl"),
		[]byte(`{"type":"summary"}`+"\n"), 0o644); err != nil {
		t.Fatalf("seed the parent's transcript: %v", err)
	}

	// Act
	child := forkOf(t, f)
	childShim := f.d.Shim(child)
	if resume := childShim.ExpectStartSession().GetResume(); resume == nil {
		t.Fatal("the forked child's StartSession carries no resume")
	}
	kid := &fixture{d: f.d, repo: f.repo, ws: child, shim: childShim, t: t}

	// Assert
	page, _ := kid.openFeedOnceCarrying("the parent's ported question", func(p *frontendv1.FeedPage) bool {
		return forkIndexOf(forkFeedTexts(p), forkParentQuestion) >= 0
	})
	ported := forkTurnIDOf(page, forkParentQuestion)
	if ported == "" || ported == parentTurn {
		t.Fatalf("the ported question's turn = %q, want the child's own re-minted id and never the parent's %q", ported, parentTurn)
	}
}

// A FORK'S INHERITED PAST ARRIVES LATE, AND IT NEVER BOUNDS THE FORK'S OWN TURN.
// The fork resumes a copy of its parent's transcript, and the file plane
// ingests that copy into the fork's book while the fork's first turn runs: the
// parent's long conversation — sixteen compactions in it — reaches the fork's
// watch as live entries, interleaved with the fork's own answers (ship-gns,
// 2026-09-27). The fork's feed must read: the newest inherited divider, what
// followed it, then the fork's own question and its answers, contiguous; and a
// reader following the feed must be pushed only the fork's own rows, so
// nothing on its screen is ever truncated by an inherited divider.
func TestAForksLongInheritedHistoryArrivingLiveNeverDisplacesItsOwnTurn(t *testing.T) {
	t.Parallel()
	// Arrange: a parent with a recorded conversation and a transcript on disk,
	// forked; a reader following the child's feed from its first moment.
	f := newOpened(t, harness.Opts{})
	if req := f.shim.ExpectStartSession(); req.GetFresh() == nil {
		t.Fatalf("the parent's StartSession = %v, want fresh", req)
	}
	vendorID := f.shim.Info().VendorSessionID
	f.submit(forkParentQuestion, "fork-long-parent-turn", conversationv1.PromptOrigin_PROMPT_ORIGIN_WEBAPP_USER_SENT)
	parentProject := createProjectDir(f.d.DefaultConfigDir, f.ws.GetDir())
	if err := os.MkdirAll(parentProject, 0o755); err != nil {
		t.Fatalf("mkdir the parent's project dir: %v", err)
	}
	if err := os.WriteFile(filepath.Join(parentProject, vendorID+".jsonl"),
		[]byte(`{"type":"summary"}`+"\n"), 0o644); err != nil {
		t.Fatalf("seed the parent's transcript: %v", err)
	}
	child := forkOf(t, f)
	childShim := f.d.Shim(child)
	if resume := childShim.ExpectStartSession().GetResume(); resume == nil {
		t.Fatal("the forked child's StartSession carries no resume")
	}
	kid := &fixture{d: f.d, repo: f.repo, ws: child, shim: childShim, t: t}
	tail := kid.watchRootFeed()
	own := kid.submit(forkOwnQuestion, "fork-long-child-turn", conversationv1.PromptOrigin_PROMPT_ORIGIN_WEBAPP_USER_SENT).
		GetSuccess().GetTurn().GetTurn().GetValue()
	if own == "" {
		t.Fatal("the fork's own SubmitPrompt minted no turn")
	}

	// Act: the copied conversation streams in — sixteen compactions, each with
	// the parent's work around it — and the fork answers in the middle of it.
	const cuts = 16
	for i := 0; i < cuts; i++ {
		childShim.PushAgentFrameIn(mainAgent, "parent-turn-"+itoa(i), feedResponseFrames("inherited-"+itoa(i), inheritedText(i))[1])
		childShim.PushAgentFrameAt(mainAgent, "inherited-cut-"+itoa(i), updateFrame(mainAgent, &conversationv1.AgentUpdate{
			Update: &conversationv1.AgentUpdate_ContextCut{ContextCut: &conversationv1.ContextCut{
				Cut: &conversationv1.ContextCut_Compacted{Compacted: &conversationv1.ContextCompacted{
					Summary: &conversationv1.AgentResponseProse{Markdown: "inherited summary " + itoa(i)},
				}},
			}},
		}))
		if i == cuts/2 {
			childShim.PushAgentFrameIn(mainAgent, own, feedResponseFrames("own-first", forkOwnFirstAnswer)[1])
		}
	}
	childShim.PushAgentFrameIn(mainAgent, "parent-turn-last", feedResponseFrames("inherited-last", inheritedText(cuts))[1])
	childShim.PushAgentFrameIn(mainAgent, own, feedResponseFrames("own-last", forkOwnLastAnswer)[1])

	// Assert (the push): everything the reader was pushed up to the fork's
	// last answer is the fork's own.
	var pushed []*frontendv1.FeedRow
	awaitRow(t, kid, tail, "the fork's last answer on the push", func(r *frontendv1.FeedRow) bool {
		pushed = append(pushed, r)
		return r.GetActivity().GetResponse().GetSuccess().GetProse().GetMarkdown() == forkOwnLastAnswer
	})
	for _, r := range pushed {
		prose := r.GetActivity().GetResponse().GetSuccess().GetProse().GetMarkdown()
		if r.GetSeparation() != nil || strings.HasPrefix(prose, inheritedPrefix) {
			t.Fatalf("the reader was pushed an inherited row %q: the inherited past is served by pages only", r.GetId().GetValue())
		}
	}

	// Assert (the page): the newest inherited divider, the inherited turn
	// after it, then the fork's question and both its answers, contiguous.
	page, _ := kid.openFeedOnceCarrying("the inherited turn after the newest cut", func(p *frontendv1.FeedPage) bool {
		return forkIndexOf(forkFeedTexts(p), inheritedText(cuts)) >= 0
	})
	var got []string
	for _, r := range page.GetSuccess().GetRows() {
		switch {
		case r.GetSeparation() != nil:
			got = append(got, "divider: "+r.GetSeparation().GetCompacted().GetSummary().GetMarkdown())
		case r.GetUserPrompt() != nil && promptText(r) == forkParentQuestion:
			// The ported question stands above the store's copy by the ported
			// plane's own rule; it is not what this case is about.
		case r.GetUserPrompt() != nil:
			got = append(got, promptText(r))
		case r.GetActivity().GetResponse() != nil:
			got = append(got, r.GetActivity().GetResponse().GetSuccess().GetProse().GetMarkdown())
		}
	}
	want := []string{
		"divider: inherited summary " + itoa(cuts-1), inheritedText(cuts),
		forkOwnQuestion, forkOwnFirstAnswer, forkOwnLastAnswer,
	}
	if strings.Join(got, " | ") != strings.Join(want, " | ") {
		t.Fatalf("the fork's feed reads %v, want %v", got, want)
	}
}

// inheritedPrefix marks every sentence of the fork's inherited past.
const inheritedPrefix = "inherited work "

// inheritedText is the Ith inherited response's sentence.
func inheritedText(i int) string { return inheritedPrefix + itoa(i) }

// The fork's own two answers in the long-history case.
const (
	forkOwnFirstAnswer = "the fork's first answer"
	forkOwnLastAnswer  = "the fork's last answer"
)

// The three sentences the fork tests read back off the feed. They are
// distinctive so a substring match cannot confuse one for another.
const (
	forkParentQuestion = "what did the parent ask"
	forkParentAnswer   = "what the parent was told"
	forkOwnQuestion    = "what the fork asks for itself"
)

// forkOf forks the fixture's workspace and answers the child.
func forkOf(t *testing.T, f *fixture) *workspacev1.WorkspaceRef {
	t.Helper()
	resp, err := f.d.Client().CreateWorkspace(f.d.Ctx(), connect.NewRequest(&agentreplv1.CreateWorkspaceRequest{
		Repository: createRepositoryRef(t, f.d, f.repo),
		Form:       &agentreplv1.CreateWorkspaceRequest_Standard{Standard: &agentreplv1.CreateWorkspaceStandard{}},
		Parent: &agentreplv1.CreateWorkspaceParent{
			Workspace: f.ws,
			Fork:      &agentreplv1.CreateWorkspaceFork{},
		},
	}))
	if err != nil || resp.Msg.GetSuccess() == nil {
		t.Fatalf("CreateWorkspace(fork) = (%v, %v), want a success", resp, err)
	}
	return resp.Msg.GetSuccess().GetWorkspace()
}

// forkFeedTexts renders a page's rows as the sentences a reader would see, in
// the page's own order.
func forkFeedTexts(page *frontendv1.FeedPage) []string {
	var out []string
	for _, row := range page.GetSuccess().GetRows() {
		switch {
		case row.GetUserPrompt() != nil:
			for _, block := range row.GetUserPrompt().GetSuccess().GetBody().GetBlocks() {
				out = append(out, block.GetText().GetText())
			}
		case row.GetActivity().GetResponse() != nil:
			out = append(out, row.GetActivity().GetResponse().GetSuccess().GetProse().GetMarkdown())
		}
	}
	return out
}

// forkIndexOf answers where a sentence sits among the drawn texts, -1 when it
// is on none of them.
func forkIndexOf(texts []string, want string) int {
	for i, text := range texts {
		if strings.Contains(text, want) {
			return i
		}
	}
	return -1
}

// forkTurnIDOf answers the turn a page's row carrying want is stamped with.
func forkTurnIDOf(page *frontendv1.FeedPage, want string) string {
	for _, row := range page.GetSuccess().GetRows() {
		for _, block := range row.GetUserPrompt().GetSuccess().GetBody().GetBlocks() {
			if strings.Contains(block.GetText().GetText(), want) {
				return row.GetTurn().GetValue()
			}
		}
	}
	return ""
}

// forkTurnOf answers the turn the fixture's own feed stamped a prompt with.
func forkTurnOf(t *testing.T, f *fixture, want string) string {
	t.Helper()
	page, _ := f.openFeedOnceCarrying("the workspace's own prompt row", func(p *frontendv1.FeedPage) bool {
		return forkTurnIDOf(p, want) != ""
	})
	return forkTurnIDOf(page, want)
}
