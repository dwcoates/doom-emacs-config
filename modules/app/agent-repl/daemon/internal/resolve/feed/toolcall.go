package feed

import (
	"encoding/base64"
	"errors"
	"fmt"
	"strings"

	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"claude-repld/internal/dlog"
	"claude-repld/internal/feedid"
)

// ② THE GREY TOOL-CALL BUBBLE. ONE shared shell — head, input line, output by
// DRAWN FORM — and the daemon composes every per-tool specific, so the client
// holds NO per-tool knowledge. A tool-specific affordance would be a new arm,
// which is the posture on purpose.

// toolRow finishes a tool card: the shared shell around whatever outcome the
// family resolved, with the unit's carried facts folded back in.
//
// THE INPUT LINE'S TEXT IS BARE (ruled 2026-08-29): a command line carries no
// "$ " prefix, a path no verb, a query no "grep: " label. The daemon states the
// FORM and the client draws that form's chrome — a decorated text would be the
// daemon drawing, which is the client's job, and would double the chrome
// wherever the client drew its own.
func (r *resolver) toolRow(s *wsState, at placement, unitID, name string, outcome toolOutcome) *frontendv1.FeedRow {
	u := s.unit(unitID)
	input := &frontendv1.FeedToolCallInput{Text: u.input}
	applyInputForm(input, u.inputForm)

	card := &frontendv1.FeedSimpleToolCall{
		Name:  &frontendv1.FeedToolCallName{Text: name},
		Input: input,
	}
	outcome(card)
	row := &frontendv1.FeedRow{
		Id: r.rowID(s.id, at.feed, feedid.RowKey{Kind: feedid.KindActivity, ID: unitID}),
		Row: &frontendv1.FeedRow_Activity{Activity: &frontendv1.FeedTurnActivity{
			Unit: &frontendv1.FeedTurnActivity_SimpleToolCall{SimpleToolCall: card},
		}},
	}
	u.row = row
	u.feedKey = r.feedKey(s.id, at.feed)
	u.name = name
	return row
}

// toolOutcome sets a card's outcome arm. A setter rather than the generated
// oneof interface, whose method is unexported and unimplementable from here.
type toolOutcome func(*frontendv1.FeedSimpleToolCall)

// runningOutcome is the shared running arm: no output yet, and the last sign
// of life the daemon observed.
func runningOutcome(u *unitState) toolOutcome {
	running := &frontendv1.FeedToolCallRunning{}
	if u.lastProgressMs > 0 {
		running.LastProgress = &frontendv1.FeedToolCallLastProgress{AtMs: u.lastProgressMs}
	}
	return func(card *frontendv1.FeedSimpleToolCall) {
		card.Outcome = &frontendv1.FeedSimpleToolCall_Running{Running: running}
	}
}

// returnedOutcome builds the returned arm: the verdict badge, the output's
// drawn form, the settled clock and any diagnostics raised against it.
func returnedOutcome(u *unitState, ok bool, form returnedForm, settledAtMs int64) toolOutcome {
	// A DENIED CALL'S TERMINAL IS NOT A FAILURE. The gate refuses, the vendor
	// still settles the tool unit -- with a `failure` whose content is unset,
	// because nothing ran -- and drawing that as a generic failure would tell
	// the reader the tool tried and broke. The unit is joined to its
	// permission by id, and the card keeps saying denied.
	if u.denied && !ok {
		return deniedOutcome()
	}
	returned := &frontendv1.FeedToolCallReturned{}
	if ok {
		returned.Verdict = &frontendv1.FeedToolCallReturned_Succeeded{Succeeded: &frontendv1.FeedToolCallSucceeded{}}
	} else {
		returned.Verdict = &frontendv1.FeedToolCallReturned_Failed{Failed: &frontendv1.FeedToolCallFailed{}}
	}
	applyReturnedForm(returned, form)
	if text, ok := formatRuntime(u.startedAtMs, settledAtMs); ok {
		returned.Runtime = &frontendv1.FeedToolCallRuntime{Text: text}
	}
	if len(u.diagnostics) > 0 {
		returned.Diagnostics = &frontendv1.FeedToolCallDiagnostics{Lines: u.diagnostics}
	}
	return func(card *frontendv1.FeedSimpleToolCall) {
		card.Outcome = &frontendv1.FeedSimpleToolCall_Returned{Returned: returned}
	}
}

// textForm is the plain-text output form.
func textForm(text string) returnedForm {
	if text == "" {
		// Nothing to draw is the `none` arm, never an empty text output.
		return nil
	}
	return func(returned *frontendv1.FeedToolCallReturned) {
		returned.Form = &frontendv1.FeedToolCallReturned_Text{Text: &frontendv1.FeedToolCallTextOutput{Text: text}}
	}
}

// imageForm is the IMAGE output form — the feed's shared FeedImageBlock, the
// same block a prompt body draws an image with. Reused rather than restated so
// a tool's image and a prompt's image reach the webview by one code path, and
// so the reference is resolved on the one end that can resolve it.
func imageForm(block *frontendv1.FeedImageBlock) returnedForm {
	return func(returned *frontendv1.FeedToolCallReturned) {
		returned.Form = &frontendv1.FeedToolCallReturned_Image{Image: block}
	}
}

// withExit folds a shell's exit chip onto a returned card. It is applied AFTER
// the outcome so a card that did not reach the returned arm — a denied one,
// say — silently carries no chip rather than growing a field on an arm that
// has none. A nil exit changes nothing: absence draws no chip, NEVER a zero.
func withExit(outcome toolOutcome, exit *frontendv1.FeedShellExit) toolOutcome {
	if exit == nil {
		return outcome
	}
	return func(card *frontendv1.FeedSimpleToolCall) {
		outcome(card)
		if returned := card.GetReturned(); returned != nil {
			returned.Exit = exit
		}
	}
}

// failureText is the account a failed call gives, drawn in the same forms a
// successful one is.
func failureText(failure *conversationv1.AgentToolFailure) string {
	var parts []string
	for _, block := range failure.GetContent().GetBlocks() {
		if text, ok := block.GetBlock().(*conversationv1.ToolResultContentBlock_Text); ok {
			parts = append(parts, text.Text.GetText())
		}
	}
	return strings.Join(parts, "\n")
}

// failureForm is the form a failed call's account is DRAWN in.
//
// THE TEXT IS THE ACCOUNT whenever there is one: AgentToolFailure's own doc
// says a consumer shows the text, and the sentence that explains the failure
// is what the reader came for. But a tool may answer a failure with an IMAGE
// and nothing else — ToolResultContentBlock carries an image arm and the shim
// populates it — and `form` is a oneof, so the picture is drawn only when no
// text block carries a word. Dropping it silently, which is what a text-only
// read did, left the card with an empty body and no sign anything was lost.
func (r *resolver) failureForm(s *wsState, failure *conversationv1.AgentToolFailure) returnedForm {
	if text := failureText(failure); text != "" {
		r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row condition", dlog.Context{"function": "feed", "condition": "text := failureText(failure); text != \"\""})
		return textForm(text)
	}
	return r.failureImageForm(s, failure)
}

// failureImageForm draws the FIRST image a wordless failure returned, through
// the same resolver and the same shared block a prompt's image is drawn with.
// An image the daemon cannot resolve into a src is recorded LOUDLY and draws
// no body, rather than carrying a src that renders broken on every client.
func (r *resolver) failureImageForm(s *wsState, failure *conversationv1.AgentToolFailure) returnedForm {
	for _, block := range failure.GetContent().GetBlocks() {
		image, ok := block.GetBlock().(*conversationv1.ToolResultContentBlock_Image)
		if !ok {
			r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row condition", dlog.Context{"function": "feed", "condition": "!ok"})
			continue
		}
		src, alt, err := r.resolveImage(image.Image)
		if err != nil {
			r.logger(s.id).Warn("daemon.feed.tool_failure_image_unresolved",
				"a failed tool answered with an image that resolves to no drawable src; the card draws no output body",
				dlog.Context{"media_type": image.Image.GetMediaType(), "cause": err.Error()})
			return nil
		}
		return imageForm(&frontendv1.FeedImageBlock{Src: src, Alt: alt})
	}
	return nil
}

// errSettleNotRestated is a settled frame that restated nothing of its call,
// arriving with no start held to draw it from. It draws NO ROW: a card with an
// empty input line (or a send with an empty body) is the defect the settle's
// restatement exists to prevent. The sink records it once, at ERROR.
var errSettleNotRestated = errors.New("feed: a settled frame restated nothing of its call, and no start was held to draw it from")

// restatedOrHeld answers what a SETTLED frame draws its input from.
//
// THE SETTLE STANDS ALONE (conversation/v1/agent_activity.proto): a call's
// start and its settle upsert ONE unit, and the store keeps one row per unit,
// so a replay (a workspace open, a transcript select) serves the settle with no
// start beside it. Every settled arm therefore restates what its start carried,
// and that restatement is what is drawn.
//
// A SETTLE THAT RESTATES NOTHING IS AN INVARIANT VIOLATION by its producer. It
// is recorded at ERROR either way: when this process held the start it is drawn
// from what the start said, and when it did not the frame draws no row at all,
// answered as errSettleNotRestated.
func (r *resolver) restatedOrHeld(s *wsState, u *unitState, unitID, kind, restated, held string) (string, error) {
	if restated != "" {
		return restated, nil
	}
	if !u.startHeld {
		return "", fmt.Errorf("%w (unit %s, kind %s)", errSettleNotRestated, unitID, kind)
	}
	r.logger(s.id).Error("daemon.feed.settle_not_restated",
		"a settled frame restated nothing of its call; it is drawn from the start this process held",
		dlog.Context{"unit": unitID, "kind": kind})
	return held, nil
}

// failureSettledMs is the instant a failed call settled, zero when none was
// observed.
func failureSettledMs(failure *conversationv1.AgentToolFailure) int64 {
	return failure.GetSettledAt().GetAtMs()
}

// ---- READ ----

// drawRead draws a file read: the path as the input line, the file as
// syntax-highlighted spans, and the head cut stated as the omitted line.
func (r *resolver) drawRead(s *wsState, at placement, act *conversationv1.AgentActivity, read *conversationv1.AgentRead) (*frontendv1.FeedRow, error) {
	unitID := act.GetActivityId().GetValue()
	u := s.unit(unitID)

	switch state := read.GetResult().(type) {
	case *conversationv1.AgentRead_Start:
		r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row decision", dlog.Context{"function": "drawRead", "branch": "case *conversationv1.AgentRead_Start"})
		u.startedAtMs = state.Start.GetStartedAt().GetAtMs()
		u.input = state.Start.GetPath().GetPath()
		u.inputForm = inputFormPath
		if u.denied {
			r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row condition", dlog.Context{"function": "feed", "condition": "u.denied"})
			return r.toolRow(s, at, unitID, "Read", deniedOutcome()), nil
		}
		return r.toolRow(s, at, unitID, "Read", runningOutcome(u)), nil
	case *conversationv1.AgentRead_Progress:
		r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row decision", dlog.Context{"function": "drawRead", "branch": "case *conversationv1.AgentRead_Progress"})
		u.lastProgressMs = state.Progress.GetLastProgressAtMs()
		return r.toolRow(s, at, unitID, "Read", runningOutcome(u)), nil
	case *conversationv1.AgentRead_Success:
		r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row decision", dlog.Context{"function": "drawRead", "branch": "case *conversationv1.AgentRead_Success"})
		path := state.Success.GetPath().GetPath()
		u.input = path
		u.inputForm = inputFormPath
		form, err := r.readForm(s, path, state.Success)
		if err != nil {
			r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row condition", dlog.Context{"function": "feed", "condition": "err != nil"})
			return nil, err
		}
		return r.toolRow(s, at, unitID, "Read",
			returnedOutcome(u, true, form, state.Success.GetSettledAt().GetAtMs())), nil
	case *conversationv1.AgentRead_Failure:
		r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row decision", dlog.Context{"function": "drawRead", "branch": "case *conversationv1.AgentRead_Failure"})
		return r.toolRow(s, at, unitID, "Read",
			returnedOutcome(u, false, r.failureForm(s, state.Failure.GetError()),
				failureSettledMs(state.Failure.GetError()))), nil
	}
	return nil, errNotARow
}

// readForm highlights what came back and states the cut.
func (r *resolver) readForm(s *wsState, path string, success *conversationv1.AgentReadSuccess) (returnedForm, error) {
	var (
		contents string
		omitted  *frontendv1.FeedToolCallOmitted
	)
	switch extent := success.GetExtent().(type) {
	case *conversationv1.AgentReadSuccess_Whole:
		r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row decision", dlog.Context{"function": "readForm", "branch": "case *conversationv1.AgentReadSuccess_Whole"})
		contents = extent.Whole.GetContents()
	case *conversationv1.AgentReadSuccess_Head:
		r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row decision", dlog.Context{"function": "readForm", "branch": "case *conversationv1.AgentReadSuccess_Head"})
		contents = extent.Head.GetContents()
		omitted = &frontendv1.FeedToolCallOmitted{
			Text: formatShowingOf(countLines(contents), uint64(extent.Head.GetTotalLines())),
		}
	case *conversationv1.AgentReadSuccess_Range:
		r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row decision", dlog.Context{"function": "readForm", "branch": "case *conversationv1.AgentReadSuccess_Range"})
		contents = extent.Range.GetContents()
		omitted = &frontendv1.FeedToolCallOmitted{
			Text: formatLineRange(uint64(extent.Range.GetFirstLine()),
				uint64(extent.Range.GetLineCount()), uint64(extent.Range.GetTotalLines())),
		}
	default:
		r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row decision", dlog.Context{"function": "readForm", "branch": "default"})
		// A read that came back with no extent has nothing to draw; the card
		// still says it returned.
		return nil, nil
	}

	spans, err := r.highlight(s, path, contents)
	if err != nil {
		r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row condition", dlog.Context{"function": "feed", "condition": "err != nil"})
		return nil, err
	}
	return func(returned *frontendv1.FeedToolCallReturned) {
		returned.Form = &frontendv1.FeedToolCallReturned_Code{Code: &frontendv1.FeedToolCallCodeOutput{
			Spans: spans, Omitted: omitted,
		}}
	}, nil
}

// highlight paints a code block. A painter that refuses is a WARN and a plain
// span, never a lost card: the file the agent read is worth more than its
// coloring.
func (r *resolver) highlight(s *wsState, path, code string) ([]*frontendv1.FeedCodeSpan, error) {
	painter := r.painter()
	if painter == nil {
		r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row condition", dlog.Context{"function": "feed", "condition": "painter == nil"})
		return []*frontendv1.FeedCodeSpan{{Text: code}}, nil
	}
	lang := langFromPath(path)
	spans, err := painter.Highlight(lang, code)
	if err != nil {
		r.logger(s.id).Warn("daemon.feed.highlight_failed",
			"a read's code could not be highlighted; it is drawn unpainted",
			dlog.Context{"path": path, "language": lang, "cause": err.Error()})
		return []*frontendv1.FeedCodeSpan{{Text: code}}, nil
	}
	out := make([]*frontendv1.FeedCodeSpan, 0, len(spans))
	for _, span := range spans {
		out = append(out, &frontendv1.FeedCodeSpan{Text: span.Text, PaintClass: span.Class})
	}
	return out, nil
}

// deniedOutcome is the gate's refusal: the call never ran, and the consent
// story is the permission card's.
func deniedOutcome() toolOutcome {
	return func(card *frontendv1.FeedSimpleToolCall) {
		card.Outcome = &frontendv1.FeedSimpleToolCall_Denied{Denied: &frontendv1.FeedToolCallDenied{}}
	}
}

// ---- WRITE and EDIT ----

// drawWrite draws a whole-file write: the change as diff lines, and the IDE's
// findings when the post-terminal frame brings them.
func (r *resolver) drawWrite(s *wsState, at placement, act *conversationv1.AgentActivity, write *conversationv1.AgentWrite) (*frontendv1.FeedRow, error) {
	unitID := act.GetActivityId().GetValue()
	u := s.unit(unitID)

	switch state := write.GetResult().(type) {
	case *conversationv1.AgentWrite_Start:
		r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row decision", dlog.Context{"function": "drawWrite", "branch": "case *conversationv1.AgentWrite_Start"})
		u.startedAtMs = state.Start.GetStartedAt().GetAtMs()
		u.input = state.Start.GetPath().GetPath()
		u.inputForm = inputFormPath
		if u.denied {
			r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row condition", dlog.Context{"function": "feed", "condition": "u.denied"})
			return r.toolRow(s, at, unitID, "Write", deniedOutcome()), nil
		}
		return r.toolRow(s, at, unitID, "Write", runningOutcome(u)), nil
	case *conversationv1.AgentWrite_Progress:
		r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row decision", dlog.Context{"function": "drawWrite", "branch": "case *conversationv1.AgentWrite_Progress"})
		u.lastProgressMs = state.Progress.GetLastProgressAtMs()
		return r.toolRow(s, at, unitID, "Write", runningOutcome(u)), nil
	case *conversationv1.AgentWrite_Success:
		r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row decision", dlog.Context{"function": "drawWrite", "branch": "case *conversationv1.AgentWrite_Success"})
		// The path form's text is the PATH, bare. Whether the write created or
		// replaced the file is the diff's story (a creation's every hunk line
		// is an addition), not the input line's.
		u.input = state.Success.GetPath().GetPath()
		u.inputForm = inputFormPath
		return r.toolRow(s, at, unitID, "Write",
			returnedOutcome(u, true, diffForm(state.Success.GetPatch()),
				state.Success.GetSettledAt().GetAtMs())), nil
	case *conversationv1.AgentWrite_Failure:
		r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row decision", dlog.Context{"function": "drawWrite", "branch": "case *conversationv1.AgentWrite_Failure"})
		return r.toolRow(s, at, unitID, "Write",
			returnedOutcome(u, false, r.failureForm(s, state.Failure.GetError()),
				failureSettledMs(state.Failure.GetError()))), nil
	case *conversationv1.AgentWrite_Diagnostics:
		r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row decision", dlog.Context{"function": "drawWrite", "branch": "case *conversationv1.AgentWrite_Diagnostics"})
		return r.applyDiagnostics(s, unitID, state.Diagnostics)
	}
	return nil, errNotARow
}

// drawEdit draws a matched-string replacement, on the same terms as a write.
func (r *resolver) drawEdit(s *wsState, at placement, act *conversationv1.AgentActivity, edit *conversationv1.AgentEdit) (*frontendv1.FeedRow, error) {
	unitID := act.GetActivityId().GetValue()
	u := s.unit(unitID)

	switch state := edit.GetResult().(type) {
	case *conversationv1.AgentEdit_Start:
		r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row decision", dlog.Context{"function": "drawEdit", "branch": "case *conversationv1.AgentEdit_Start"})
		u.startedAtMs = state.Start.GetStartedAt().GetAtMs()
		u.input = state.Start.GetPath().GetPath()
		u.inputForm = inputFormPath
		if u.denied {
			r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row condition", dlog.Context{"function": "feed", "condition": "u.denied"})
			return r.toolRow(s, at, unitID, "Edit", deniedOutcome()), nil
		}
		return r.toolRow(s, at, unitID, "Edit", runningOutcome(u)), nil
	case *conversationv1.AgentEdit_Progress:
		r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row decision", dlog.Context{"function": "drawEdit", "branch": "case *conversationv1.AgentEdit_Progress"})
		u.lastProgressMs = state.Progress.GetLastProgressAtMs()
		return r.toolRow(s, at, unitID, "Edit", runningOutcome(u)), nil
	case *conversationv1.AgentEdit_Success:
		r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row decision", dlog.Context{"function": "drawEdit", "branch": "case *conversationv1.AgentEdit_Success"})
		u.input = state.Success.GetPath().GetPath()
		u.inputForm = inputFormPath
		return r.toolRow(s, at, unitID, "Edit",
			returnedOutcome(u, true, diffForm(state.Success.GetPatch()),
				state.Success.GetSettledAt().GetAtMs())), nil
	case *conversationv1.AgentEdit_Failure:
		r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row decision", dlog.Context{"function": "drawEdit", "branch": "case *conversationv1.AgentEdit_Failure"})
		return r.toolRow(s, at, unitID, "Edit",
			returnedOutcome(u, false, r.failureForm(s, state.Failure.GetError()),
				failureSettledMs(state.Failure.GetError()))), nil
	case *conversationv1.AgentEdit_Diagnostics:
		r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row decision", dlog.Context{"function": "drawEdit", "branch": "case *conversationv1.AgentEdit_Diagnostics"})
		return r.applyDiagnostics(s, unitID, state.Diagnostics)
	}
	return nil, errNotARow
}

// diffForm turns the producer's structured hunks into the diff output form.
// The producer diffs at the moment of the change, so the lines describe the
// file as it actually was and no consumer re-reads anything.
func diffForm(hunks []*conversationv1.FilePatchHunk) returnedForm {
	var lines []*frontendv1.FeedDiffLine
	for _, hunk := range hunks {
		lines = append(lines, &frontendv1.FeedDiffLine{
			Kind: &frontendv1.FeedDiffLine_Header{Header: &frontendv1.FeedDiffLineHeader{}},
			Text: hunkHeader(hunk),
		})
		for _, line := range hunk.GetLines() {
			lines = append(lines, diffLine(line))
		}
	}
	if len(lines) == 0 {
		return nil
	}
	return func(returned *frontendv1.FeedToolCallReturned) {
		returned.Form = &frontendv1.FeedToolCallReturned_Diff{Diff: &frontendv1.FeedToolCallDiffOutput{Lines: lines}}
	}
}

// hunkHeader composes a hunk's "@@ -3,7 +3,9 @@" line from the stated ranges.
func hunkHeader(hunk *conversationv1.FilePatchHunk) string {
	return fmt.Sprintf("@@ -%d,%d +%d,%d @@",
		hunk.GetOldRange().GetStart(), hunk.GetOldRange().GetLines(),
		hunk.GetNewRange().GetStart(), hunk.GetNewRange().GetLines())
}

// diffLine reads a unified-diff line's leading marker and strips it: the ARM
// carries that fact, so the text must not carry it twice.
func diffLine(line string) *frontendv1.FeedDiffLine {
	if line == "" {
		return &frontendv1.FeedDiffLine{
			Kind: &frontendv1.FeedDiffLine_Context{Context: &frontendv1.FeedDiffLineContext{}},
		}
	}
	switch line[0] {
	case '+':
		return &frontendv1.FeedDiffLine{
			Kind: &frontendv1.FeedDiffLine_Added{Added: &frontendv1.FeedDiffLineAdded{}},
			Text: line[1:],
		}
	case '-':
		return &frontendv1.FeedDiffLine{
			Kind: &frontendv1.FeedDiffLine_Removed{Removed: &frontendv1.FeedDiffLineRemoved{}},
			Text: line[1:],
		}
	case ' ':
		return &frontendv1.FeedDiffLine{
			Kind: &frontendv1.FeedDiffLine_Context{Context: &frontendv1.FeedDiffLineContext{}},
			Text: line[1:],
		}
	case '@':
		return &frontendv1.FeedDiffLine{
			Kind: &frontendv1.FeedDiffLine_Header{Header: &frontendv1.FeedDiffLineHeader{}},
			Text: line,
		}
	}
	return &frontendv1.FeedDiffLine{
		Kind: &frontendv1.FeedDiffLine_Context{Context: &frontendv1.FeedDiffLineContext{}},
		Text: line,
	}
}

// applyDiagnostics folds an injected diagnostics report onto its change card.
// It is a consequence, not a state: when it arrives after the terminal it
// amends the card the consumer already has; when it arrives first it is retained
// until the stream plane draws the card.
func (r *resolver) applyDiagnostics(s *wsState, unitID string, report *conversationv1.AgentDiagnosticsReport) (*frontendv1.FeedRow, error) {
	u := s.unit(unitID)
	u.diagnostics = composeDiagnostics(report)
	if u.row == nil {
		// Transcript-plane diagnostics and stream-plane Edit/Write cards are
		// independently delivered for the same unit, so either can arrive first.
		// Nothing is lost: the unit retains these lines and the later card applies
		// them. The daemon cannot distinguish this ordinary cross-plane ordering
		// from a delayed card, so warning here can only ever be a false alarm.
		r.logger(s.id).Debug("daemon.feed.diagnostics_without_card",
			"a diagnostics attachment arrived before its change card and was retained for that card",
			dlog.Context{"unit": unitID})
		return nil, errNotARow
	}
	returned := u.row.GetActivity().GetSimpleToolCall().GetReturned()
	if returned == nil {
		r.logger(s.id).Warn("daemon.feed.diagnostics_before_terminal",
			"a diagnostics report arrived before its change settled",
			dlog.Context{"unit": unitID})
		return nil, errNotARow
	}
	returned.Diagnostics = &frontendv1.FeedToolCallDiagnostics{Lines: u.diagnostics}
	return u.row, nil
}

// composeDiagnostics renders each finding as one drawn line.
func composeDiagnostics(report *conversationv1.AgentDiagnosticsReport) []string {
	var lines []string
	for _, file := range report.GetFiles() {
		for _, d := range file.GetDiagnostics() {
			// The vendor's lines are zero-based; a reader counts from one.
			lines = append(lines, fmt.Sprintf("%s:%d · %s · %s",
				file.GetPath(), d.GetStartLine()+1, severityWord(d.GetSeverity()), d.GetMessage()))
		}
	}
	return lines
}

// severityWord names an LSP severity for a drawn line.
func severityWord(severity conversationv1.AgentDiagnosticSeverity) string {
	switch severity {
	case conversationv1.AgentDiagnosticSeverity_AGENT_DIAGNOSTIC_SEVERITY_ERROR:
		return "error"
	case conversationv1.AgentDiagnosticSeverity_AGENT_DIAGNOSTIC_SEVERITY_WARNING:
		return "warning"
	case conversationv1.AgentDiagnosticSeverity_AGENT_DIAGNOSTIC_SEVERITY_INFORMATION:
		return "info"
	case conversationv1.AgentDiagnosticSeverity_AGENT_DIAGNOSTIC_SEVERITY_HINT:
		return "hint"
	}
	return "unspecified"
}

// ---- GREP and GLOB ----

// drawGrep draws a content search. Matching nothing is a SUCCESS with an empty
// answer: the caller asked a question and got one.
func (r *resolver) drawGrep(s *wsState, at placement, act *conversationv1.AgentActivity, grep *conversationv1.AgentGrep) (*frontendv1.FeedRow, error) {
	unitID := act.GetActivityId().GetValue()
	u := s.unit(unitID)

	switch state := grep.GetResult().(type) {
	case *conversationv1.AgentGrep_Start:
		r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row decision", dlog.Context{"function": "drawGrep", "branch": "case *conversationv1.AgentGrep_Start"})
		u.startedAtMs = state.Start.GetStartedAt().GetAtMs()
		u.input = state.Start.GetQuery().GetPattern()
		u.inputForm = inputFormQuery
		if u.denied {
			r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row condition", dlog.Context{"function": "feed", "condition": "u.denied"})
			return r.toolRow(s, at, unitID, "Grep", deniedOutcome()), nil
		}
		return r.toolRow(s, at, unitID, "Grep", runningOutcome(u)), nil
	case *conversationv1.AgentGrep_Progress:
		r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row decision", dlog.Context{"function": "drawGrep", "branch": "case *conversationv1.AgentGrep_Progress"})
		u.lastProgressMs = state.Progress.GetLastProgressAtMs()
		return r.toolRow(s, at, unitID, "Grep", runningOutcome(u)), nil
	case *conversationv1.AgentGrep_Success:
		r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row decision", dlog.Context{"function": "drawGrep", "branch": "case *conversationv1.AgentGrep_Success"})
		u.input = state.Success.GetQuery().GetPattern()
		u.inputForm = inputFormQuery
		return r.toolRow(s, at, unitID, "Grep",
			returnedOutcome(u, true, grepForm(state.Success), state.Success.GetSettledAt().GetAtMs())), nil
	case *conversationv1.AgentGrep_Failure:
		r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row decision", dlog.Context{"function": "drawGrep", "branch": "case *conversationv1.AgentGrep_Failure"})
		return r.toolRow(s, at, unitID, "Grep",
			returnedOutcome(u, false, r.failureForm(s, state.Failure.GetError()),
				failureSettledMs(state.Failure.GetError()))), nil
	}
	return nil, errNotARow
}

// grepForm picks the output form the search's answer shape calls for.
func grepForm(success *conversationv1.AgentGrepSuccess) returnedForm {
	switch matches := success.GetMatches().(type) {
	case *conversationv1.AgentGrepSuccess_Content:
		content := matches.Content
		out := &frontendv1.FeedToolCallLinesOutput{Lines: splitLines(content.GetContent())}
		if partial, ok := content.GetExtent().(*conversationv1.AgentGrepContent_Partial); ok {
			out.Omitted = &frontendv1.FeedToolCallOmitted{
				Text: formatOmittedExact(uint64(partial.Partial.GetLinesOmitted()), "lines"),
			}
		}
		if len(out.Lines) == 0 && out.Omitted == nil {
			return nil
		}
		return linesForm(out)
	case *conversationv1.AgentGrepSuccess_Files:
		files := matches.Files
		out := &frontendv1.FeedToolCallLinesOutput{Lines: files.GetPaths()}
		if partial, ok := files.GetExtent().(*conversationv1.AgentGrepFiles_Partial); ok {
			out.Omitted = &frontendv1.FeedToolCallOmitted{
				Text: formatOmittedExact(uint64(partial.Partial.GetFilesOmitted()), "files"),
			}
		}
		if len(out.Lines) == 0 && out.Omitted == nil {
			return nil
		}
		return linesForm(out)
	case *conversationv1.AgentGrepSuccess_Count:
		return textForm(fmt.Sprintf("%s matches", formatCount(uint64(matches.Count.GetMatches()))))
	}
	return nil
}

// drawGlob draws a path match.
func (r *resolver) drawGlob(s *wsState, at placement, act *conversationv1.AgentActivity, glob *conversationv1.AgentGlob) (*frontendv1.FeedRow, error) {
	unitID := act.GetActivityId().GetValue()
	u := s.unit(unitID)

	switch state := glob.GetResult().(type) {
	case *conversationv1.AgentGlob_Start:
		r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row decision", dlog.Context{"function": "drawGlob", "branch": "case *conversationv1.AgentGlob_Start"})
		u.startedAtMs = state.Start.GetStartedAt().GetAtMs()
		u.input = state.Start.GetQuery().GetPattern()
		u.inputForm = inputFormQuery
		if u.denied {
			r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row condition", dlog.Context{"function": "feed", "condition": "u.denied"})
			return r.toolRow(s, at, unitID, "Glob", deniedOutcome()), nil
		}
		return r.toolRow(s, at, unitID, "Glob", runningOutcome(u)), nil
	case *conversationv1.AgentGlob_Progress:
		r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row decision", dlog.Context{"function": "drawGlob", "branch": "case *conversationv1.AgentGlob_Progress"})
		u.lastProgressMs = state.Progress.GetLastProgressAtMs()
		return r.toolRow(s, at, unitID, "Glob", runningOutcome(u)), nil
	case *conversationv1.AgentGlob_Success:
		r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row decision", dlog.Context{"function": "drawGlob", "branch": "case *conversationv1.AgentGlob_Success"})
		u.input = state.Success.GetQuery().GetPattern()
		u.inputForm = inputFormQuery
		return r.toolRow(s, at, unitID, "Glob",
			returnedOutcome(u, true, globForm(state.Success), state.Success.GetSettledAt().GetAtMs())), nil
	case *conversationv1.AgentGlob_Failure:
		r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row decision", dlog.Context{"function": "drawGlob", "branch": "case *conversationv1.AgentGlob_Failure"})
		return r.toolRow(s, at, unitID, "Glob",
			returnedOutcome(u, false, r.failureForm(s, state.Failure.GetError()),
				failureSettledMs(state.Failure.GetError()))), nil
	}
	return nil, errNotARow
}

// globForm renders the matched paths and the omitted line. A FLOOR and an
// exact remainder are different claims, and only the arm says which.
func globForm(success *conversationv1.AgentGlobSuccess) returnedForm {
	out := &frontendv1.FeedToolCallLinesOutput{Lines: success.GetPaths()}
	if partial, ok := success.GetExtent().(*conversationv1.AgentGlobSuccess_Partial); ok {
		switch omitted := partial.Partial.GetOmitted().(type) {
		case *conversationv1.AgentGlobPartial_Exact:
			out.Omitted = &frontendv1.FeedToolCallOmitted{
				Text: formatOmittedExact(uint64(omitted.Exact.GetFilesOmitted()), "paths"),
			}
		case *conversationv1.AgentGlobPartial_AtLeast:
			out.Omitted = &frontendv1.FeedToolCallOmitted{
				Text: formatOmittedAtLeast(uint64(omitted.AtLeast.GetFilesOmittedAtLeast()), "paths"),
			}
		}
	}
	if len(out.Lines) == 0 && out.Omitted == nil {
		return nil
	}
	return linesForm(out)
}

// linesForm is the line-list output form.
func linesForm(out *frontendv1.FeedToolCallLinesOutput) returnedForm {
	return func(returned *frontendv1.FeedToolCallReturned) {
		returned.Form = &frontendv1.FeedToolCallReturned_Lines{Lines: out}
	}
}

// splitLines splits rendered search output into drawn lines without inventing
// a trailing empty one.
func splitLines(text string) []string {
	if text == "" {
		return nil
	}
	return strings.Split(strings.TrimSuffix(text, "\n"), "\n")
}

// ---- BASH, in the FOREGROUND ----

// drawBash draws a foreground shell call. A foreground command's output is
// observable NOWHERE while it runs — it arrives once, whole, at the return —
// so the card goes straight from running to returned.
func (r *resolver) drawBash(s *wsState, at placement, act *conversationv1.AgentActivity, bash *conversationv1.AgentBash) (*frontendv1.FeedRow, error) {
	unitID := act.GetActivityId().GetValue()
	u := s.unit(unitID)

	// THE WORK LEFT FOR THE BACKGROUND. Once this call detached, its head is the
	// shell bubble (KindShellHead) that detachForegroundShell drew in place of
	// the retired running card. Every later frame of this unit — the vendor's
	// launch receipt, the other plane's replay, the next turn's live-work
	// reconciliation — restates the CALL and never the move, so drawing one here
	// would put a second, stale card beside the bubble.
	//
	// A TERMINAL IS THE ONE FRAME THAT STILL COUNTS. A call whose work really
	// moved returns the backgrounding receipt, which settles nothing; a
	// terminal on this unit is therefore its work's own ending, and it settles
	// the head the card became. Dropping it left the head running forever when
	// no other source ended the run -- the store holding no rows for it.
	if u.moved {
		r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row condition", dlog.Context{"function": "feed", "condition": "u.moved"})
		if bash.GetSuccess() != nil || bash.GetFailure() != nil {
			r.drawDetachedShell(s, &conversationv1.DetachedWorkId{Value: u.movedTo}, bash)
			r.logger(s.id).Debug("daemon.feed.detached_shell_settled_by_call",
				"a moved call's own terminal settled the detached shell head it became",
				dlog.Context{"unit": unitID, "work": u.movedTo, "failed": bash.GetFailure() != nil})
		}
		return nil, errNotARow
	}

	switch state := bash.GetResult().(type) {
	case *conversationv1.AgentBash_Start:
		r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row decision", dlog.Context{"function": "drawBash", "branch": "case *conversationv1.AgentBash_Start"})
		u.startedAtMs = state.Start.GetStartedAt().GetAtMs()
		u.input = state.Start.GetCommand().GetLine()
		u.inputForm = inputFormCommand
		if u.denied {
			r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row condition", dlog.Context{"function": "feed", "condition": "u.denied"})
			return r.toolRow(s, at, unitID, "Bash", deniedOutcome()), nil
		}
		return r.toolRow(s, at, unitID, "Bash", runningOutcome(u)), nil
	case *conversationv1.AgentBash_Progress:
		r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row decision", dlog.Context{"function": "drawBash", "branch": "case *conversationv1.AgentBash_Progress"})
		u.lastProgressMs = state.Progress.GetLastProgressAtMs()
		return r.toolRow(s, at, unitID, "Bash", runningOutcome(u)), nil
	case *conversationv1.AgentBash_Update:
		r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row decision", dlog.Context{"function": "drawBash", "branch": "case *conversationv1.AgentBash_Update"})
		// A foreground call reports no growth; an update here belongs to the
		// work's own detached stream and is drawn there.
		return nil, errNotARow
	case *conversationv1.AgentBash_Success:
		r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row decision", dlog.Context{"function": "drawBash", "branch": "case *conversationv1.AgentBash_Success"})
		u.input = state.Success.GetCommand().GetLine()
		u.inputForm = inputFormCommand
		u.ending = bash
		ok, form := r.bashOutcomeForm(s, u.input, state.Success)
		// THE EXIT CODE IS THE COMMAND'S OWN VERDICT ON ITSELF, and the
		// foreground card states it exactly as the detached shell's settled
		// shape does. Without it a shell that failed drew `failed` and no
		// number, so the reader was told the command went wrong and never told
		// how. The two sites must stay parallel; see FeedToolCallReturned.exit.
		return r.toolRow(s, at, unitID, "Bash",
			withExit(returnedOutcome(u, ok, form, state.Success.GetSettledAt().GetAtMs()),
				bashExit(state.Success))), nil
	case *conversationv1.AgentBash_Failure:
		r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row decision", dlog.Context{"function": "drawBash", "branch": "case *conversationv1.AgentBash_Failure"})
		u.ending = bash
		return r.toolRow(s, at, unitID, "Bash",
			returnedOutcome(u, false, r.failureForm(s, state.Failure.GetError()),
				failureSettledMs(state.Failure.GetError()))), nil
	}
	return nil, errNotARow
}

// bashExit is the exit status a settled foreground shell REPORTED, when it
// reported one, drawn through the SAME element the detached shell's settled
// shape carries (FeedShellSettled.exit).
//
// UNSET when the producer stated no termination — a foreground call ordinarily
// states one only when the command exited non-zero — and unset for a KILLED
// command too: an exit code and a kill are different endings, and only one of
// them has a number. Absence draws no chip, never a zero.
func bashExit(success *conversationv1.AgentBashSuccess) *frontendv1.FeedShellExit {
	completed, ok := success.GetOutcome().(*conversationv1.AgentBashSuccess_Completed)
	if !ok {
		return nil
	}
	exited, ok := completed.Completed.GetTermination().GetHow().(*conversationv1.AgentBashTermination_Exited)
	if !ok {
		return nil
	}
	return &frontendv1.FeedShellExit{Code: exited.Exited.GetCode()}
}

// bashImageForm draws a command whose output was IMAGE DATA rather than
// characters — a screenshot, a rendered chart — through the feed's shared
// image block.
//
// THE DAEMON RESOLVES THE REFERENCE, as FeedImageBlock says it must: the
// record carries the bytes and their media type, and the src a browser can
// load is composed from the two here rather than at either end that cannot.
// A record that states one without the other resolves to NOTHING drawable, so
// the gap is recorded loudly and the card falls back to the `none` arm rather
// than carrying a src that renders as a broken image on every client.
func (r *resolver) bashImageForm(s *wsState, command string, image *conversationv1.AgentBashOutputImage) returnedForm {
	mediaType := image.GetMediaType()
	data := image.GetData()
	if mediaType == "" || len(data) == 0 {
		r.logger(s.id).Warn("daemon.feed.bash_image_unresolved",
			"a command's image output states no source the webview can load; the card draws no output body",
			dlog.Context{
				"media_type": mediaType,
				"bytes":      len(data),
				"command":    command,
			})
		return nil
	}
	return imageForm(&frontendv1.FeedImageBlock{
		Src: "data:" + mediaType + ";base64," + base64.StdEncoding.EncodeToString(data),
		// The command line is the caption a reader has: nothing else in the
		// record names what the picture is of.
		Alt: command,
	})
}

// bashOutcomeForm renders a settled shell's output as a drawn form and says
// whether the badge reads ok. A COMPLETED command succeeded as a CALL whatever
// its exit code — the code is the command's verdict on itself. So did an
// INTERRUPTED one: THE PROTO ALWAYS WINS, and conversation.v1 nests
// AgentBashInterrupted inside AgentBashSuccess, so an interrupt is a SUCCESS
// arm carrying the interrupted marker. The badge reads succeeded and the body
// says how it was cut; only AgentBash_Failure — the call itself breaking —
// draws `failed`.
//
// AN INTERRUPTED COMMAND IS DRAWN AS TEXT EVEN WHEN ITS OUTPUT IS IMAGE DATA.
// The form is a oneof, the lead line ("timed out after 2 m") is the fact the
// reader came for, and no producer emits an image on that arm; carrying the
// picture there would drop the sentence that explains it.
func (r *resolver) bashOutcomeForm(s *wsState, command string, success *conversationv1.AgentBashSuccess) (bool, returnedForm) {
	switch outcome := success.GetOutcome().(type) {
	case *conversationv1.AgentBashSuccess_Completed:
		r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row decision", dlog.Context{"function": "bashOutcomeForm", "branch": "case *conversationv1.AgentBashSuccess_Completed"})
		output := outcome.Completed.GetOutput()
		if image, ok := output.GetForm().(*conversationv1.AgentBashOutput_Image); ok {
			r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row condition", dlog.Context{"function": "feed", "condition": "image, ok := output.GetForm().(*conversationv1.AgentBashOutput_Image); ok"})
			return true, r.bashImageForm(s, command, image.Image)
		}
		return true, textForm(bashOutputText(output))
	case *conversationv1.AgentBashSuccess_Interrupted:
		r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row decision", dlog.Context{"function": "bashOutcomeForm", "branch": "case *conversationv1.AgentBashSuccess_Interrupted"})
		lead := "interrupted"
		switch cause := outcome.Interrupted.GetCause().(type) {
		case *conversationv1.AgentBashInterrupted_ByUser:
			r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row decision", dlog.Context{"function": "bashOutcomeForm", "branch": "case *conversationv1.AgentBashInterrupted_ByUser"})
			lead = "interrupted by the user"
		case *conversationv1.AgentBashInterrupted_TimedOut:
			r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row decision", dlog.Context{"function": "bashOutcomeForm", "branch": "case *conversationv1.AgentBashInterrupted_TimedOut"})
			lead = "timed out after " + formatDuration(int64(cause.TimedOut.GetTimeoutMs()))
		}
		body := bashOutputText(outcome.Interrupted.GetOutput())
		if body == "" {
			r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row condition", dlog.Context{"function": "feed", "condition": "body == \"\""})
			return true, textForm(lead)
		}
		return true, textForm(lead + "\n" + body)
	}
	return true, nil
}

// bashOutputText renders a command's output. The two streams are carried apart
// and joined here, in that order, because nothing states an interleaving.
func bashOutputText(output *conversationv1.AgentBashOutput) string {
	// The IMAGE form never reaches here on the completed arm — bashOutcomeForm
	// routes it to the image block — and the `not_observed` form has no text
	// by construction: the producer states that it does not know.
	text, ok := output.GetForm().(*conversationv1.AgentBashOutput_Text)
	if !ok {
		return ""
	}
	parts := make([]string, 0, 3)
	if text.Text.GetStdout() != "" {
		parts = append(parts, text.Text.GetStdout())
	}
	if text.Text.GetStderr() != "" {
		parts = append(parts, text.Text.GetStderr())
	}
	joined := strings.Join(parts, "\n")
	if partial, ok := text.Text.GetExtent().(*conversationv1.AgentBashOutputText_Partial); ok {
		joined = joined + "\n" + fmt.Sprintf("%s bytes more not shown",
			formatCount(partial.Partial.GetBytesOmitted()))
	}
	return joined
}

// ---- WEB FETCH and WEB SEARCH ----

// drawWebFetch draws one URL's fetch: the input line LINKS to the page, and an
// HTTP error page is a served answer rather than a tool failure.
func (r *resolver) drawWebFetch(s *wsState, at placement, act *conversationv1.AgentActivity, fetch *conversationv1.AgentWebFetch) (*frontendv1.FeedRow, error) {
	unitID := act.GetActivityId().GetValue()
	u := s.unit(unitID)

	var url string
	switch state := fetch.GetResult().(type) {
	case *conversationv1.AgentWebFetch_Start:
		r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row decision", dlog.Context{"function": "drawWebFetch", "branch": "case *conversationv1.AgentWebFetch_Start"})
		u.startedAtMs = state.Start.GetStartedAtMs()
		url = state.Start.GetTarget().GetUrl()
		u.input = url
		u.inputForm = inputFormPath
		row := r.toolRow(s, at, unitID, "WebFetch", runningOutcome(u))
		linkInput(row, url)
		return row, nil
	case *conversationv1.AgentWebFetch_Progress:
		r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row decision", dlog.Context{"function": "drawWebFetch", "branch": "case *conversationv1.AgentWebFetch_Progress"})
		u.lastProgressMs = state.Progress.GetLastProgressAtMs()
		row := r.toolRow(s, at, unitID, "WebFetch", runningOutcome(u))
		linkInput(row, u.input)
		return row, nil
	case *conversationv1.AgentWebFetch_Success:
		r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row decision", dlog.Context{"function": "drawWebFetch", "branch": "case *conversationv1.AgentWebFetch_Success"})
		url = state.Success.GetTarget().GetUrl()
		u.input = url
		u.inputForm = inputFormPath
		code := state.Success.GetStatus().GetCode()
		body := fmt.Sprintf("%d %s\n\n%s", code, state.Success.GetStatus().GetText(), state.Success.GetResult())
		row := r.toolRow(s, at, unitID, "WebFetch",
			returnedOutcome(u, code < 400, textForm(body), 0))
		linkInput(row, url)
		return row, nil
	case *conversationv1.AgentWebFetch_Failure:
		r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row decision", dlog.Context{"function": "drawWebFetch", "branch": "case *conversationv1.AgentWebFetch_Failure"})
		url = state.Failure.GetTarget().GetUrl()
		u.input = url
		u.inputForm = inputFormPath
		row := r.toolRow(s, at, unitID, "WebFetch",
			returnedOutcome(u, false, r.failureForm(s, state.Failure.GetFailure()),
				failureSettledMs(state.Failure.GetFailure())))
		linkInput(row, url)
		return row, nil
	}
	return nil, errNotARow
}

// linkInput makes a card's input line a hyperlink.
func linkInput(row *frontendv1.FeedRow, url string) {
	if url == "" {
		return
	}
	card := row.GetActivity().GetSimpleToolCall()
	if card == nil {
		return
	}
	card.Input.Link = &frontendv1.FeedToolCallInputLink{Url: url}
}

// drawWebSearch draws a web search: the engine's answer as a link list, with
// its narration lines kept in order and not clickable.
func (r *resolver) drawWebSearch(s *wsState, at placement, act *conversationv1.AgentActivity, search *conversationv1.AgentWebSearch) (*frontendv1.FeedRow, error) {
	unitID := act.GetActivityId().GetValue()
	u := s.unit(unitID)

	switch state := search.GetResult().(type) {
	case *conversationv1.AgentWebSearch_Start:
		r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row decision", dlog.Context{"function": "drawWebSearch", "branch": "case *conversationv1.AgentWebSearch_Start"})
		u.startedAtMs = state.Start.GetStartedAtMs()
		u.input = state.Start.GetQuery().GetTerms()
		u.inputForm = inputFormQuery
		return r.toolRow(s, at, unitID, "WebSearch", runningOutcome(u)), nil
	case *conversationv1.AgentWebSearch_Progress:
		r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row decision", dlog.Context{"function": "drawWebSearch", "branch": "case *conversationv1.AgentWebSearch_Progress"})
		u.lastProgressMs = state.Progress.GetLastProgressAtMs()
		return r.toolRow(s, at, unitID, "WebSearch", runningOutcome(u)), nil
	case *conversationv1.AgentWebSearch_Success:
		r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row decision", dlog.Context{"function": "drawWebSearch", "branch": "case *conversationv1.AgentWebSearch_Success"})
		u.input = state.Success.GetQuery().GetTerms()
		u.inputForm = inputFormQuery
		return r.toolRow(s, at, unitID, "WebSearch",
			returnedOutcome(u, true, linksForm(state.Success.GetResults()), 0)), nil
	case *conversationv1.AgentWebSearch_Failure:
		r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row decision", dlog.Context{"function": "drawWebSearch", "branch": "case *conversationv1.AgentWebSearch_Failure"})
		return r.toolRow(s, at, unitID, "WebSearch",
			returnedOutcome(u, false, r.failureForm(s, state.Failure.GetFailure()),
				failureSettledMs(state.Failure.GetFailure()))), nil
	}
	return nil, errNotARow
}

// linksForm renders the engine's heterogeneous answer: links and narration
// lines in the served order.
func linksForm(results []*conversationv1.AgentWebSearchResult) returnedForm {
	links := make([]*frontendv1.FeedToolCallLink, 0, len(results))
	for _, result := range results {
		switch entry := result.GetEntry().(type) {
		case *conversationv1.AgentWebSearchResult_Link:
			links = append(links, &frontendv1.FeedToolCallLink{
				Text: entry.Link.GetTitle(),
				Url:  &frontendv1.FeedToolCallLinkUrl{Url: entry.Link.GetUrl()},
			})
		case *conversationv1.AgentWebSearchResult_Note:
			links = append(links, &frontendv1.FeedToolCallLink{Text: entry.Note.GetText()})
		}
	}
	if len(links) == 0 {
		return nil
	}
	return func(returned *frontendv1.FeedToolCallReturned) {
		returned.Form = &frontendv1.FeedToolCallReturned_Links{
			Links: &frontendv1.FeedToolCallLinksOutput{Links: links},
		}
	}
}
