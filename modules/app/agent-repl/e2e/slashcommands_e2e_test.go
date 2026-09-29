// slashcommands_e2e_test.go — SPEC.md §C "Slash commands", tests #60-63.
//
// Contract read: SPEC.md §B ("Waits", the driveScenarioToCompletion/
// driveDocumentedPrompt contract), §C "Slash commands" (#60-63), §D row 65
// (`vendor-answered-slash-commands` -> test #60, no new scenario needed) and
// §F item 5 (real git RULED, THEN REVERSED BY THE USER: this suite mocks
// every external dependency, git included) is not relevant to this file
// either way — it drives no git fact; workspaces here just need SOME
// registered directory, provided by harness.NewRepo's scripted fake git.
// docs/overhaul/daemon.md "Failure classification — where each failure
// lives": "Entry-less residue (machinery/shim/internal/client-local) is
// frontend.v1 failure.proto's vocabulary" and frontend/v1/failure.proto's own
// header ("What HAPPENS ON ITS OWN... is named here" / the FailureKind oneof)
// together establish that a VENDOR-SPECIFIC residue record (no entry, no
// classified failure) has no feed-row arm of its own — so a slash command the
// vendor answers itself surfaces on the wire only as an ORDINARY concluded
// turn, never as a distinct unit. conversation/v1/slash_command.proto's
// SessionCommand is a SEPARATE, unrelated vocabulary: the daemon's own
// closed set of `/xxx` commands it recognizes and never forwards, which is
// not what these scenarios exercise (`!name` here is the fake-SDK's own
// scenario selector, driving what the mocked VENDOR does with a submitted
// prompt).
//
// Scenario goldens driven (agent-shim/claude/shim/src/fake/scenarios/
// session.ts): SLASH_LOCAL (name "slash"), SLASH_SHAPE_A (name
// "slash-shape-a"), SLASH_SHAPE_A_UNNAMED (name "slash-shape-a-unnamed").
//
// UNGROUNDED/INVENTED FLAG (agent-shim/claude/shim/testdata/captures/
// MANIFEST.md, "e2ecleanup/fakesdk-ext additions"): `!slash-shape-a` and
// `!slash-shape-a-unnamed` are marked UNGROUNDED, INVENTED — no capture in
// the manifest exercises the CLI's own raw `user`-typed slash-command
// bookkeeping record; the nearest real artifact
// (`vendor-answered-slash-commands`) is the VENDOR answering the command
// itself, a different record, already covered by `!slash`/SLASH_LOCAL. The
// shape was instead built from the deleted daemon/e2e suite's
// `machinery_e2e_test.go` constants. TestSlashShapeANamed and
// TestSlashShapeAUnnamed below drive this INVENTED shape and their
// assertions are scoped accordingly: they prove the fake SDK's own
// documented file-plane write landed exactly as `session.ts` specifies, not
// that any real vendor binary produces this record.
//
// `!slash`/SLASH_LOCAL "emits NOTHING on the stream" is FALSE for that one —
// SLASH_LOCAL runs a real turn (a system message, a transcript append and a
// closing assistant response) but SLASH_SHAPE_A / SLASH_SHAPE_A_UNNAMED are
// genuinely FILE-PLANE-ONLY for their own bookkeeping write: the raw
// transcript record they append produces no SessionUpdate of its own. The
// only way this suite can observe that write crossing a real wire (never
// hand-authored — see the grep gate in main_test.go) is to read back the
// vendor transcript file at the path the store's own GetSidecarCursors
// verb — a read, not a write, verb — names as durably advanced for this
// turn's project directory. That is what scTranscriptRecordsUnderProject does
// below; the file is one the REAL shim (running --fake) actually wrote via
// its own TranscriptWriter.append, never a test-authored fixture.
package e2e

import (
	"bytes"
	"context"
	"encoding/json"
	"os"
	"strings"
	"testing"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"
	workspacev1 "agentrepl/proto/workspace/v1"

	"connectrpc.com/connect"

	"claude-repld/integration/harness"
)

// ---------------------------------------------------------------------------
// Shared helpers, local to this file — no other area file touches it.
// ---------------------------------------------------------------------------

// scOpenFeedRows fetches the workspace's root feed page in full. A read verb
// (OpenFeed), never a write — the grep gate's own discipline.
func scOpenFeedRows(t *testing.T, w *World, ws *workspacev1.WorkspaceRef) []*frontendv1.FeedRow {
	t.Helper()
	resp, err := w.Client().OpenFeed(w.Ctx(), connect.NewRequest(&agentreplv1.OpenFeedRequest{Workspace: ws}))
	if err != nil {
		t.Fatalf("OpenFeed: %v", err)
	}
	success := resp.Msg.GetSuccess()
	if success == nil {
		t.Fatalf("OpenFeed = %v, want success", resp.Msg)
	}
	return success.GetPage().GetSuccess().GetRows()
}

// scFindRow answers the first row satisfying pred, failing the test loudly
// (naming every row's kind) when none does — so a mismatch names what WAS
// there instead of just "not found".
func scFindRow(t *testing.T, rows []*frontendv1.FeedRow, what string, pred func(*frontendv1.FeedRow) bool) *frontendv1.FeedRow {
	t.Helper()
	for _, row := range rows {
		if pred(row) {
			return row
		}
	}
	t.Fatalf("e2e: no feed row found for %s among %d rows", what, len(rows))
	return nil
}

// scAnsweringResponseProse walks a concluded turn's own FeedTurnEnded row to
// the answering response row (FeedTurnEndedConcluded.Answer) and answers its
// settled markdown.
func scAnsweringResponseProse(t *testing.T, w *World, ws *workspacev1.WorkspaceRef, turn *conversationv1.TurnId) string {
	t.Helper()
	rows := scOpenFeedRows(t, w, ws)
	ended := scFindRow(t, rows, "turn "+turn.GetValue()+"'s FeedTurnEnded", endsTurn(turn))
	concluded := ended.GetTurnEnded().GetConcluded()
	if concluded == nil {
		t.Fatalf("e2e: turn %s did not end Concluded: %v", turn.GetValue(), ended.GetTurnEnded())
	}
	answerID := concluded.GetAnswer()
	if answerID.GetValue() == "" {
		t.Fatalf("e2e: turn %s concluded with no answering response row", turn.GetValue())
	}
	answer := scFindRow(t, rows, "the answering response row "+answerID.GetValue(), func(r *frontendv1.FeedRow) bool {
		return r.GetId().GetValue() == answerID.GetValue()
	})
	success := answer.GetActivity().GetResponse().GetSuccess()
	if success == nil {
		t.Fatalf("e2e: answering row %s is not a settled FeedResponseSuccess: %v", answerID.GetValue(), answer)
	}
	return success.GetProse().GetMarkdown()
}

// scTranscriptRecordsUnderProject reads back every non-blank line of every
// vendor transcript file the store's own GetSidecarCursors verb names as
// living under this workspace's project directory, and answers them as raw
// JSON bytes, one slice element per line. This is a READ of a file the real
// shim (--fake) wrote itself; nothing here authors or mutates it (the grep
// gate's forbidden shapes are os.MkdirAll/os.WriteFile/os.Create, none used).
func scTranscriptRecordsUnderProject(t *testing.T, w *World, projectDir string) [][]byte {
	t.Helper()
	ctx, cancel := context.WithTimeout(w.Ctx(), DefaultTimeout)
	defer cancel()
	var lines [][]byte
	for _, c := range w.Store.Cursors(t, ctx) {
		if !strings.HasPrefix(c.GetPath(), projectDir) {
			continue
		}
		body, err := os.ReadFile(c.GetPath())
		if err != nil {
			t.Fatalf("e2e: read vendor transcript %s: %v", c.GetPath(), err)
		}
		for _, line := range bytes.Split(body, []byte("\n")) {
			if len(bytes.TrimSpace(line)) == 0 {
				continue
			}
			lines = append(lines, line)
		}
	}
	if len(lines) == 0 {
		t.Fatalf("e2e: no vendor transcript file found under project dir %s (store cursors: %v)", projectDir, w.Store.Cursors(t, ctx))
	}
	return lines
}

// scLocalCommandSystemRecord is the Shape B raw transcript shape SLASH_LOCAL
// appends (session.ts: `type: "system", subtype: "local_command"`).
type scLocalCommandSystemRecord struct {
	Type    string `json:"type"`
	Subtype string `json:"subtype"`
	Content string `json:"content"`
	IsMeta  bool   `json:"isMeta"`
}

// scShapeAUserRecord is the Shape A / Shape A unnamed raw transcript shape
// (session.ts: `type: "user", message: {role: "user", content: ...}`).
type scShapeAUserRecord struct {
	Type    string `json:"type"`
	Message struct {
		Role    string `json:"role"`
		Content string `json:"content"`
	} `json:"message"`
}

// ---------------------------------------------------------------------------
// #60 — VendorAnsweredSlashCommand: the vendor answers a slash command
// itself. Golden: vendor-answered-slash-commands (SPEC.md §D row 65).
// Scenario driven: SLASH_LOCAL ("!slash").
// ---------------------------------------------------------------------------

// TestVendorAnsweredSlashCommand drives "!slash" and asserts the turn draws
// as an ORDINARY concluded turn whose answering response is exactly
// SLASH_LOCAL's own closing text — never a distinct unit for the vendor's
// `local_command_output` system message or the `system`/`local_command`
// transcript record it also writes (per daemon.md's "Failure classification"
// / frontend/v1/failure.proto: entry-less vendor-specific residue is never a
// feed row). SLASH_LOCAL is deliberately built with NO reasoning prelude
// (unlike every other turn — the vendor answered this one itself), but
// thinking has no feed-row arm of its own regardless of scenario
// (frontend/v1/feed.proto's FeedTurnActivity oneof carries no thinking arm),
// so that distinction is not independently observable at this wire and this
// test does not attempt to assert it.
func TestVendorAnsweredSlashCommand(t *testing.T) {
	t.Parallel()
	// Arrange.
	w := NewWorld(t, WorldOpts{})
	repo := harness.NewRepo(t)
	ws := harness.Register(t, w.Daemon, repo.Dir)

	// Act.
	turn := driveScenarioToCompletion(t, w, ws, w.DefaultConfigDir, "slash")

	// Assert.
	const wantConclusion = "Answered the slash command locally."
	if got := scAnsweringResponseProse(t, w, ws, turn); got != wantConclusion {
		t.Errorf("answering response prose = %q, want %q", got, wantConclusion)
	}
}

// ---------------------------------------------------------------------------
// #63 — SlashShapeBViaSlash: the SAME "!slash" turn, asserting the OTHER
// half of what SLASH_LOCAL writes — the durable `system`/`local_command`
// transcript record itself (remediation item 2: "no new scenario code
// needed", retiring a hand-fabricated fixture of exactly this shape).
// Distinct from #60, which asserts the DRAWN answer; this test asserts the
// STORED raw record — daemon-side bookkeeping is #61/#62's, not this one's.
// ---------------------------------------------------------------------------

// TestSlashShapeBViaSlash drives "!slash" and asserts the vendor transcript
// durably carries the `system`/`local_command` record SLASH_LOCAL appends,
// wrapping the vendor's own output in `<local-command-stdout>...
// </local-command-stdout>` with `isMeta: false` — read back from the real
// file the real shim wrote, never asserted by inspecting store internals or
// hand-authoring a fixture.
func TestSlashShapeBViaSlash(t *testing.T) {
	t.Parallel()
	// Arrange.
	w := NewWorld(t, WorldOpts{})
	repo := harness.NewRepo(t)
	ws := harness.Register(t, w.Daemon, repo.Dir)
	projectDir := harness.ProjectDir(w.DefaultConfigDir, ws.GetDir())

	// Act.
	driveScenarioToCompletion(t, w, ws, w.DefaultConfigDir, "slash")

	// Assert.
	var found *scLocalCommandSystemRecord
	for _, line := range scTranscriptRecordsUnderProject(t, w, projectDir) {
		var rec scLocalCommandSystemRecord
		if err := json.Unmarshal(line, &rec); err != nil {
			continue // a differently-shaped line (prompt, turn record, ...); not this record
		}
		if rec.Type == "system" && rec.Subtype == "local_command" {
			found = &rec
			break
		}
	}
	if found == nil {
		t.Fatalf("e2e: no system/local_command transcript record found under %s", projectDir)
	}
	if found.IsMeta {
		t.Errorf("system/local_command record IsMeta = true, want false")
	}
	if !strings.HasPrefix(found.Content, "<local-command-stdout>") || !strings.HasSuffix(found.Content, "</local-command-stdout>") {
		t.Errorf("system/local_command record Content = %q, want it wrapped in <local-command-stdout>...</local-command-stdout>", found.Content)
	}
}

// ---------------------------------------------------------------------------
// #61 — SlashShapeANamed: the CLI's own slash-command bookkeeping, a raw
// `user`-typed transcript record naming a command. UNGROUNDED/INVENTED (see
// file header). Scenario driven: SLASH_SHAPE_A ("!slash-shape-a [command]").
// ---------------------------------------------------------------------------

// TestSlashShapeANamed drives "!slash-shape-a merge" (a command name chosen
// to prove the name is genuinely a parameter, not the scenario's own
// "compact" default) and asserts BOTH halves SPEC.md's #61 entry names: the
// durable file-plane `user`-typed bookkeeping record (never signaled on the
// stream — this is the whole reason it needs a durability wait rather than a
// stream assertion) and the ordinary conclusion text the turn still ends
// with.
func TestSlashShapeANamed(t *testing.T) {
	t.Parallel()
	// Arrange.
	w := NewWorld(t, WorldOpts{})
	repo := harness.NewRepo(t)
	ws := harness.Register(t, w.Daemon, repo.Dir)
	projectDir := harness.ProjectDir(w.DefaultConfigDir, ws.GetDir())

	// Act.
	turn := driveDocumentedPrompt(t, w, ws, w.DefaultConfigDir, "!slash-shape-a merge")

	// Assert: the raw Shape-A bookkeeping record landed with the given name.
	const wantContent = "<command-message>merge</command-message>\n<command-name>/merge</command-name>\n<command-args></command-args>"
	var found *scShapeAUserRecord
	for _, line := range scTranscriptRecordsUnderProject(t, w, projectDir) {
		var rec scShapeAUserRecord
		if err := json.Unmarshal(line, &rec); err != nil {
			continue
		}
		if rec.Type == "user" && rec.Message.Content == wantContent {
			found = &rec
			break
		}
	}
	if found == nil {
		t.Fatalf("e2e: no user-typed transcript record with Shape-A content %q found under %s", wantContent, projectDir)
	}

	// Assert: the turn still concludes ordinarily.
	const wantConclusion = "Recorded the Shape-A bookkeeping for /merge."
	if got := scAnsweringResponseProse(t, w, ws, turn); got != wantConclusion {
		t.Errorf("answering response prose = %q, want %q", got, wantConclusion)
	}
}

// ---------------------------------------------------------------------------
// #62 — SlashShapeAUnnamed: the negative — a record naming NO command.
// UNGROUNDED/INVENTED (see file header). Scenario driven:
// SLASH_SHAPE_A_UNNAMED ("!slash-shape-a-unnamed").
// ---------------------------------------------------------------------------

// TestSlashShapeAUnnamed drives "!slash-shape-a-unnamed" and asserts the
// withheld-unnamed record: `<local-command-stdout>...</local-command-stdout>`
// only, with NO `<command-name>` element anywhere in it — the negative
// #61 exists to prove alongside.
func TestSlashShapeAUnnamed(t *testing.T) {
	t.Parallel()
	// Arrange.
	w := NewWorld(t, WorldOpts{})
	repo := harness.NewRepo(t)
	ws := harness.Register(t, w.Daemon, repo.Dir)
	projectDir := harness.ProjectDir(w.DefaultConfigDir, ws.GetDir())

	// Act.
	turn := driveScenarioToCompletion(t, w, ws, w.DefaultConfigDir, "slash-shape-a-unnamed")

	// Assert: the withheld-unnamed record landed, naming no command.
	const wantContent = "<local-command-stdout>total 4\ndrwxr-xr-x</local-command-stdout>"
	var found *scShapeAUserRecord
	for _, line := range scTranscriptRecordsUnderProject(t, w, projectDir) {
		var rec scShapeAUserRecord
		if err := json.Unmarshal(line, &rec); err != nil {
			continue
		}
		if rec.Type == "user" && rec.Message.Content == wantContent {
			found = &rec
			break
		}
	}
	if found == nil {
		t.Fatalf("e2e: no user-typed transcript record with the withheld-unnamed content %q found under %s", wantContent, projectDir)
	}
	if strings.Contains(found.Message.Content, "<command-name>") {
		t.Errorf("withheld-unnamed record Content = %q, want no <command-name> element", found.Message.Content)
	}

	// Assert: the turn still concludes ordinarily.
	const wantConclusion = "Recorded the withheld-unnamed bookkeeping."
	if got := scAnsweringResponseProse(t, w, ws, turn); got != wantConclusion {
		t.Errorf("answering response prose = %q, want %q", got, wantConclusion)
	}
}
