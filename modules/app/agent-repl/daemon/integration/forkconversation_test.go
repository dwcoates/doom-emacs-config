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
// defect (playtest section 2, A.6) as an assertion.
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
