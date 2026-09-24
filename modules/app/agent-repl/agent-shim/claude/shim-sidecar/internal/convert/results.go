package convert

// results.go — reading the vendor's `toolUseResult` object into the settled arms.
//
// THE FIDELITY PRINCIPLE GOVERNS THESE: a vendor field lands even when no UI
// draws it. What is deliberately absent is anything the vendor did not state —
// an absent figure stays UNSET rather than becoming a zero a consumer would draw
// as a fact.

import (
	"encoding/base64"
	"strings"

	conversationv1 "agentrepl/proto/conversation/v1"
	"agentrepl/shim-claude-sidecar/internal/logging"
)

// readSuccess states HOW MUCH of the file came back. The set arm IS whether more
// exists, so a whole read carries no truncation vocabulary at all.
//
// THE EXTENT IS A FACT ABOUT BOTH SIDES: the vendor states what came back
// (`content`, `startLine`, `numLines`, `totalLines`, `truncatedByTokenCap`) and
// the CALLER states what was asked for (`offset`, `limit`). A caller that asked
// for a MIDDLE SLICE got a range, never a head — the omitted vocabulary of a
// head is always a claim about the file's TAIL, and drawing "lines 2-3 of 4" as
// an omission would tell a reader lines were cut that were never asked for.
func readSuccess(call openCall, result map[string]any, ts int64) *conversationv1.AgentReadSuccess {
	file := obj(result["file"])
	path := firstNonEmpty(str(file["filePath"]), str(pick(call.input, "file_path", "path")))
	success := &conversationv1.AgentReadSuccess{
		Path:      &conversationv1.ReadPath{Path: path},
		SettledAt: settledAt(ts, call.startedAt),
	}
	// A NON-TEXT READ HAS NO EXTENT ARM THIS WAVE. AgentReadSuccess retired the
	// image, pdf, notebook, parts and file_unchanged tags, so no arm can say how
	// much came back: the extent stays UNSET and the read still settles. Falling
	// through to `whole` would invent a fact — an empty whole file — and the
	// daemon would draw an empty code block instead of feed.proto's `none`
	// output arm. The shim's own converter states the same
	// (convert/tools/read.ts readSuccess).
	if kind := str(result["type"]); kind != "" && kind != "text" {
		return success
	}
	contents := str(file["content"])
	totalLines := uint32(number(file["totalLines"]))
	numLines := uint32(number(file["numLines"]))
	startLine := uint32(number(file["startLine"]))
	// A vendor that stated no startLine began at line 1: absence is not "past
	// the top of the file".
	fromFirstLine := !has(file, "startLine") || startLine <= 1
	stoppedShort := has(file, "totalLines") && has(file, "numLines") && numLines < totalLines

	switch {
	case boolean(pick(file, "truncatedByTokenCap", "truncatedByTokens")) && fromFirstLine:
		// The vendor auto-paginated a whole-file read: a head cut at a token
		// budget, whose last line may be incomplete.
		success.Extent = readHead(contents, totalLines, true)
	case has(call.input, "offset"):
		// The caller asked for a MIDDLE SLICE and that slice came back.
		success.Extent = readRange(contents, startLine, numLines, totalLines)
	case has(call.input, "limit") && fromFirstLine && stoppedShort:
		// The read began at line 1 and stopped at a line budget.
		success.Extent = readHead(contents, totalLines, false)
	case stoppedShort && !fromFirstLine:
		// Fewer lines than the file holds, beginning past line 1, with no
		// offset asked: a range is the only arm that can state where it began.
		success.Extent = readRange(contents, startLine, numLines, totalLines)
	case stoppedShort:
		// NOT A WHOLE FILE whatever the caller asked: calling it whole would
		// claim there is nothing more to fetch.
		success.Extent = readHead(contents, totalLines, false)
	default:
		success.Extent = &conversationv1.AgentReadSuccess_Whole{Whole: &conversationv1.AgentReadWhole{Contents: contents}}
	}
	return success
}

// readHead states the leading portion of a file, with WHAT CUT IT.
func readHead(contents string, totalLines uint32, tokenCap bool) *conversationv1.AgentReadSuccess_Head {
	head := &conversationv1.AgentReadHead{Contents: contents, TotalLines: totalLines}
	if tokenCap {
		head.Cut = &conversationv1.AgentReadHead_TokenCap{TokenCap: &conversationv1.AgentReadCutAtTokenCap{}}
	} else {
		head.Cut = &conversationv1.AgentReadHead_LineCap{LineCap: &conversationv1.AgentReadCutAtLineCap{}}
	}
	return &conversationv1.AgentReadSuccess_Head{Head: head}
}

// readRange states a middle slice, which CARRIES NO OMISSION: what lies outside
// it was never asked for.
func readRange(contents string, firstLine, lineCount, totalLines uint32) *conversationv1.AgentReadSuccess_Range {
	return &conversationv1.AgentReadSuccess_Range{Range: &conversationv1.AgentReadRange{
		Contents:   contents,
		FirstLine:  firstLine,
		LineCount:  lineCount,
		TotalLines: totalLines,
	}}
}

// writeSuccess states whether the file EXISTED beforehand, so a creation is
// never drawn as a rewrite of nothing.
//
// THE PATCH IS THE CHANGE, NEVER THE WHOLE FILE. The vendor leaves
// `structuredPatch` EMPTY for a creation, so the producer diffs `originalFile`
// against `content` itself — an absent original diffs against the empty file,
// which makes every line an addition, exactly as the proto describes.
func writeSuccess(call openCall, result map[string]any, ts int64) *conversationv1.AgentWriteSuccess {
	path := firstNonEmpty(str(result["filePath"]), str(pick(call.input, "file_path", "path")))
	success := &conversationv1.AgentWriteSuccess{
		Path:         &conversationv1.ReadPath{Path: path},
		Patch:        writePatch(result),
		UserModified: boolean(result["userModified"]),
		SettledAt:    settledAt(ts, call.startedAt),
	}
	switch {
	case str(result["type"]) == "create":
		success.Outcome = &conversationv1.AgentWriteSuccess_Created{Created: &conversationv1.AgentWriteCreated{}}
	case str(result["type"]) == "update":
		success.Outcome = &conversationv1.AgentWriteSuccess_Updated{Updated: &conversationv1.AgentWriteUpdated{}}
	case str(result["originalFile"]) == "":
		// The vendor stated neither word. Nothing existed to rewrite, which is
		// the only reading of an absent original.
		success.Outcome = &conversationv1.AgentWriteSuccess_Created{Created: &conversationv1.AgentWriteCreated{}}
	default:
		success.Outcome = &conversationv1.AgentWriteSuccess_Updated{Updated: &conversationv1.AgentWriteUpdated{}}
	}
	return success
}

// writePatch DIFFS THE TWO VERSIONS, and does not prefer the patch the vendor
// happened to state.
//
// agent_activity.proto is explicit on AgentWriteSuccess.patch: "The producer
// diffs after the fact precisely so a card can show the CHANGE rather than the
// whole file it was handed" — unconditionally, unlike AgentEditSuccess, whose
// patch the vendor does state. Preferring `structuredPatch` here made this
// plane disagree with the stream plane, which always diffs: the same write
// drew "@@ -4,1 +4,2 @@" through one plane and "@@ -2,3 +2,4 @@" through the
// other, and WHICH ONE a card showed was a race between the two producers
// settling the unit. The file doc of diff.go states the guarantee this
// restores — both planes mint the identical patch for one write.
//
// A result that states no content cannot be diffed at all; the vendor's own
// stated patch is then the only account of the change there is, so it is kept
// rather than dropped.
func writePatch(result map[string]any) []*conversationv1.FilePatchHunk {
	if !has(result, "content") {
		return patchHunks(result["structuredPatch"])
	}
	return diffHunks(str(result["originalFile"]), str(result["content"]))
}

func editSuccess(call openCall, result map[string]any, ts int64) *conversationv1.AgentEditSuccess {
	path := firstNonEmpty(str(result["filePath"]), str(pick(call.input, "file_path", "path")))
	return &conversationv1.AgentEditSuccess{
		Path:         &conversationv1.ReadPath{Path: path},
		Patch:        patchHunks(result["structuredPatch"]),
		UserModified: boolean(result["userModified"]),
		SettledAt:    settledAt(ts, call.startedAt),
	}
}

// patchHunks reads the producer's structured patch. STATED RATHER THAN COMPUTED:
// the producer diffed at the moment of the change, so the line numbers describe
// the file as it actually was.
func patchHunks(raw any) []*conversationv1.FilePatchHunk {
	var hunks []*conversationv1.FilePatchHunk
	for _, el := range list(raw) {
		h := obj(el)
		if h == nil {
			continue
		}
		hunk := &conversationv1.FilePatchHunk{
			OldRange: &conversationv1.FilePatchHunkRange{
				Start: uint32(number(h["oldStart"])),
				Lines: uint32(number(h["oldLines"])),
			},
			NewRange: &conversationv1.FilePatchHunkRange{
				Start: uint32(number(h["newStart"])),
				Lines: uint32(number(h["newLines"])),
			},
		}
		for _, line := range list(h["lines"]) {
			hunk.Lines = append(hunk.Lines, str(line))
		}
		hunks = append(hunks, hunk)
	}
	return hunks
}

// grepSuccess answers in the SHAPE the call asked for. The three modes are
// genuinely different answers rather than views of one, so each carries only
// what applies to it. Matching nothing is a success with an empty answer.
//
// THE VENDOR'S TYPED OUTPUT IS THE ANSWER, the rendered text only its fallback:
// the typed output states the TOTALS, and the omitted figure is subtracted from
// them here, once, so no consumer re-derives it. A search read from text alone
// knows no total, so it can only claim what it holds.
func grepSuccess(call openCall, result, block map[string]any, ts int64) *conversationv1.AgentGrepSuccess {
	success := &conversationv1.AgentGrepSuccess{
		Query:     grepQuery(call.input),
		SettledAt: settledAt(ts, call.startedAt),
	}
	// The vendor's own default output mode applies when neither the result nor
	// the call named one, rather than a guess of our own.
	mode := firstNonEmpty(str(result["mode"]), str(pick(call.input, "output_mode", "outputMode")), "files_with_matches")
	switch {
	case mode == "count" && has(result, "numMatches"):
		success.Matches = &conversationv1.AgentGrepSuccess_Count{Count: &conversationv1.AgentGrepCount{
			Matches: uint32(number(result["numMatches"])),
		}}
	case mode == "content" && has(result, "numLines"):
		numLines := uint32(number(result["numLines"]))
		content := &conversationv1.AgentGrepContent{Content: str(result["content"])}
		if omitted := omittedBeyond(result, "totalLines", numLines); omitted != nil {
			content.Extent = &conversationv1.AgentGrepContent_Partial{Partial: &conversationv1.AgentGrepContentPartial{
				LinesReturned: numLines,
				LinesOmitted:  *omitted,
			}}
		} else {
			content.Extent = &conversationv1.AgentGrepContent_All{All: &conversationv1.AgentGrepContentAll{
				LinesReturned: numLines,
			}}
		}
		success.Matches = &conversationv1.AgentGrepSuccess_Content{Content: content}
	case mode == "files_with_matches" && has(result, "numFiles"):
		numFiles := uint32(number(result["numFiles"]))
		files := &conversationv1.AgentGrepFiles{Paths: stringList(result["filenames"])}
		if omitted := omittedBeyond(result, "totalFiles", numFiles); omitted != nil {
			files.Extent = &conversationv1.AgentGrepFiles_Partial{Partial: &conversationv1.AgentGrepFilesPartial{
				FilesReturned: numFiles,
				FilesOmitted:  *omitted,
			}}
		} else {
			files.Extent = &conversationv1.AgentGrepFiles_All{All: &conversationv1.AgentGrepFilesAll{
				FilesReturned: numFiles,
			}}
		}
		success.Matches = &conversationv1.AgentGrepSuccess_Files{Files: files}
	default:
		setGrepFromText(success, mode, flattenResultText(block["content"]))
	}
	return success
}

// setGrepFromText reads a search the vendor typed nothing for. It states only
// what the rendered answer HOLDS: with no total stated, no omission is claimed.
func setGrepFromText(success *conversationv1.AgentGrepSuccess, mode, text string) {
	switch mode {
	case "count":
		success.Matches = &conversationv1.AgentGrepSuccess_Count{Count: &conversationv1.AgentGrepCount{
			Matches: uint32(countMatches(text)),
		}}
	case "content":
		lines := nonEmptyLines(text)
		success.Matches = &conversationv1.AgentGrepSuccess_Content{Content: &conversationv1.AgentGrepContent{
			Content: text,
			Extent: &conversationv1.AgentGrepContent_All{All: &conversationv1.AgentGrepContentAll{
				LinesReturned: uint32(len(lines)),
			}},
		}}
	default:
		paths := nonEmptyLines(text)
		success.Matches = &conversationv1.AgentGrepSuccess_Files{Files: &conversationv1.AgentGrepFiles{
			Paths: paths,
			Extent: &conversationv1.AgentGrepFiles_All{All: &conversationv1.AgentGrepFilesAll{
				FilesReturned: uint32(len(paths)),
			}},
		}}
	}
}

// globSuccess states the paths a pattern matched and WHETHER THE LIST STOPPED
// SHORT. The two omitted arms are kept apart because they are different claims:
// `totalMatches` is exact, but `countIsComplete == false` means the search
// capped its own counting, so the figure is a FLOOR — "42 more" and "at least
// 42 more" must never be drawn as the same thing.
func globSuccess(call openCall, result, block map[string]any, ts int64) *conversationv1.AgentGlobSuccess {
	success := &conversationv1.AgentGlobSuccess{
		Query:     globQuery(call.input),
		SettledAt: settledAt(ts, call.startedAt),
	}
	if !has(result, "numFiles") && !has(result, "filenames") {
		// The vendor typed nothing: the rendered list is all there is, and with
		// no total stated no omission can be claimed.
		paths := nonEmptyLines(flattenResultText(block["content"]))
		success.Paths = paths
		success.Extent = &conversationv1.AgentGlobSuccess_All{All: &conversationv1.AgentGlobAll{FilesReturned: uint32(len(paths))}}
		return success
	}
	paths := stringList(result["filenames"])
	numFiles := uint32(len(paths))
	if has(result, "numFiles") {
		numFiles = uint32(number(result["numFiles"]))
	}
	success.Paths = paths
	if !boolean(result["truncated"]) {
		success.Extent = &conversationv1.AgentGlobSuccess_All{All: &conversationv1.AgentGlobAll{FilesReturned: numFiles}}
		return success
	}
	partial := &conversationv1.AgentGlobPartial{FilesReturned: numFiles}
	switch {
	case !has(result, "totalMatches"):
		// A truncated match that stated no total leaves only the honest floor:
		// trivially true, and never overstating what was left out.
		partial.Omitted = &conversationv1.AgentGlobPartial_AtLeast{AtLeast: &conversationv1.AgentGlobOmittedAtLeast{}}
	case boolean(pick(result, "countIsComplete")) || !has(result, "countIsComplete"):
		partial.Omitted = &conversationv1.AgentGlobPartial_Exact{Exact: &conversationv1.AgentGlobOmittedExact{
			FilesOmitted: globOmitted(result, numFiles),
		}}
	default:
		partial.Omitted = &conversationv1.AgentGlobPartial_AtLeast{AtLeast: &conversationv1.AgentGlobOmittedAtLeast{
			FilesOmittedAtLeast: globOmitted(result, numFiles),
		}}
	}
	success.Extent = &conversationv1.AgentGlobSuccess_Partial{Partial: partial}
	return success
}

// globOmitted subtracts what came back from the stated total, clamped at zero so
// a total trailing the returned count never becomes a negative figure.
func globOmitted(result map[string]any, returned uint32) uint32 {
	total := uint32(number(result["totalMatches"]))
	if total <= returned {
		return 0
	}
	return total - returned
}

// omittedBeyond states how many a stated total leaves out, or nil when the
// vendor stated no total — an unstated total is not "none omitted".
func omittedBeyond(result map[string]any, totalKey string, returned uint32) *uint32 {
	if !has(result, totalKey) {
		return nil
	}
	total := uint32(number(pick(result, totalKey)))
	if total <= returned {
		return nil
	}
	left := total - returned
	return &left
}

// stringList reads a vendor array of paths, dropping anything that is not one.
func stringList(raw any) []string {
	var out []string
	for _, el := range list(raw) {
		if s, ok := el.(string); ok {
			out = append(out, s)
		}
	}
	return out
}

// bashMovedToBackground reports whether a shell receipt says the command LEFT
// rather than ended.
//
// PRESENCE OF THE TASK ID IS THE WHOLE FACT: the vendor returns the same shape
// for a command that finished and for one it launched into the background, and
// only the id distinguishes them. Both of the vendor's spellings are read
// because the disk carries both.
func bashMovedToBackground(result map[string]any) bool {
	return str(pick(result, "backgroundTaskId", "background_task_id")) != ""
}

// bashSuccess: a NONZERO EXIT IS STILL THIS ARM. The failure arm is for a call
// that could not be performed; a command that ran and failed ran, and what it
// printed is the answer the caller wanted.
func bashSuccess(call openCall, result map[string]any, block map[string]any, exit *int32, ts int64) *conversationv1.AgentBashSuccess {
	success := &conversationv1.AgentBashSuccess{
		Command:   bashCommand(call.input),
		SettledAt: settledAt(ts, call.startedAt),
	}
	output := bashOutput(result, block)
	if boolean(result["interrupted"]) {
		interrupted := &conversationv1.AgentBashInterrupted{Output: output}
		if timeout := optionalInt64(result, "timedOutAfterMs"); timeout != nil {
			interrupted.Cause = &conversationv1.AgentBashInterrupted_TimedOut{TimedOut: &conversationv1.AgentBashInterruptedByTimeout{
				TimeoutMs: uint64(*timeout),
			}}
		} else {
			interrupted.Cause = &conversationv1.AgentBashInterrupted_ByUser{ByUser: &conversationv1.AgentBashInterruptedByUser{}}
		}
		success.Outcome = &conversationv1.AgentBashSuccess_Interrupted{Interrupted: interrupted}
		return success
	}
	completed := &conversationv1.AgentBashCompleted{Output: output}
	// TERMINATION IS UNSET FOR A FOREGROUND COMMAND: no producer states one, and
	// claiming an exit the vendor did not report would be a fabrication. It is
	// set for a detached shell, whose spool the shell itself terminates.
	if exit != nil {
		completed.Termination = &conversationv1.AgentBashTermination{
			How: &conversationv1.AgentBashTermination_Exited{Exited: &conversationv1.AgentBashExited{Code: *exit}},
		}
	}
	success.Outcome = &conversationv1.AgentBashSuccess_Completed{Completed: completed}
	return success
}

// statedExitCode reads the exit status a result STATED, if it stated one.
// `exitCode` is the PRECISE datum when the vendor gives one; a result without
// it may still carry `returnCodeInterpretation` ("exited with code 3"), prose
// ABOUT the same status, read here only as the fallback — camelCase before the
// vendor's snake_case spelling, `return_code_interpretation`, since the disk
// carries both. A result that states neither leaves termination UNSET rather
// than synthesizing a zero.
func statedExitCode(result map[string]any) *int32 {
	if code := optionalInt64(result, "exitCode"); code != nil {
		v := int32(*code)
		return &v
	}
	return firstSignedInt(str(pick(result, "returnCodeInterpretation", "return_code_interpretation")))
}

// bashExitCode reads the exit status a Bash result stated ANYWHERE it states
// one. THE EXIT CODE IS THE COMMAND'S OWN VERDICT ON ITSELF, and its presence —
// not the vendor's `is_error` flag — is what says the command ran. A nonzero
// exit arrives as an is_error result whose `toolUseResult` is a bare string
// ("Error: Exit code 7"), so the structured fields carry nothing and the stated
// ending survives only in the text returned to the model. That text is read
// ONLY for a result the vendor marked an error: a command that succeeded states
// its ending in the structured fields, and mining its OUTPUT for a status would
// let a passing command that merely printed the words "exit code 3" be drawn as
// having exited 3.
func bashExitCode(result map[string]any, block map[string]any, failed bool) *int32 {
	if code := statedExitCode(result); code != nil {
		return code
	}
	if !failed {
		return nil
	}
	return statedExitFromText(resultText(block["content"]))
}

// statedExitFromText reads the vendor's prose spelling of a shell status
// ("Exit code 7", "exited with code 3"). The statement is a LINE OF ITS OWN in
// the returned text, so a mention of those words mid-line is output, not a
// verdict, and text that names no exit yields nil rather than the first number
// it happens to contain.
func statedExitFromText(text string) *int32 {
	for _, line := range strings.Split(text, "\n") {
		trimmed := strings.ToLower(strings.TrimSpace(line))
		for _, marker := range []string{"error: exit code", "exited with code", "exit code"} {
			if strings.HasPrefix(trimmed, marker) {
				return firstSignedInt(trimmed[len(marker):])
			}
		}
	}
	return nil
}

// resultText flattens a result block's content down to the text the tool
// returned, in the two shapes the vendor uses for it.
func resultText(raw any) string {
	switch value := raw.(type) {
	case string:
		return value
	case []any:
		var parts []string
		for _, el := range value {
			if block := obj(el); block != nil && str(block["type"]) == "text" {
				parts = append(parts, str(block["text"]))
			}
		}
		return strings.Join(parts, "\n")
	default:
		return ""
	}
}

// firstSignedInt reads the first signed decimal in a sentence, or nil when it
// holds none.
func firstSignedInt(text string) *int32 {
	for i := 0; i < len(text); i++ {
		if text[i] < '0' || text[i] > '9' {
			continue
		}
		end := i
		for end < len(text) && text[end] >= '0' && text[end] <= '9' {
			end++
		}
		v := int32(atoiSafe(text[i:end]))
		if i > 0 && text[i-1] == '-' {
			v = -v
		}
		return &v
	}
	return nil
}

// bashOutput keeps stdout and stderr APART rather than interleaving them: a
// consumer that wants them woven can concatenate, while one handed a single blob
// can never pull them apart again.
func bashOutput(result map[string]any, block map[string]any) *conversationv1.AgentBashOutput {
	if boolean(result["isImage"]) {
		// THE PICTURE IS IN THE RESULT BLOCK, NOT IN `toolUseResult`. The
		// vendor's Output object states only that the output WAS an image
		// (`isImage`); the bytes and their media type arrive as the answering
		// `tool_result`'s own image content block, which is the only place
		// either is stated. Reading `mediaType` off the Output object alone
		// produced an image arm carrying neither, which no consumer can draw.
		data, mediaType := bashResultImage(block)
		return &conversationv1.AgentBashOutput{
			Form: &conversationv1.AgentBashOutput_Image{Image: &conversationv1.AgentBashOutputImage{
				Data: data,
				// The Output object's own spelling is preferred where it
				// exists; the content block is what actually carries one.
				MediaType: firstNonEmpty(str(result["mediaType"]), mediaType),
			}},
		}
	}
	text := &conversationv1.AgentBashOutputText{
		Stdout: str(result["stdout"]),
		Stderr: str(result["stderr"]),
	}
	applyBashTextExtent(text, result)
	return &conversationv1.AgentBashOutput{Form: &conversationv1.AgentBashOutput_Text{Text: text}}
}

// applyBashTextExtent states whether everything the command printed is carried
// inline.
//
// THIS PLANE MUST STATE THE TRUNCATION TOO, and that is why this exists. The
// stream plane and this one convert the SAME tool result into the SAME unit
// under one upsert key, so whichever arrives last is what the feed draws — and
// a file-plane row claiming `whole` ERASED the truncation summary the stream
// plane had already drawn. `!bash-spill` then reached the card as three lines
// of `y` with nothing saying the other 200 kB ever existed
// (e2e/detachedbash_e2e_test.go's TestBashPartialOutputWithSpill, red whenever
// the sidecar's row landed second).
//
// The subtraction is the shim converter's, restated (convert/tools/bash.ts
// `textExtent`): the vendor declares a TOTAL and the proto carries the OMITTED
// figure, so it is derived once and clamped, so a total that trails the inline
// bytes can never become a negative "fewer bytes not shown".
func applyBashTextExtent(text *conversationv1.AgentBashOutputText, result map[string]any) {
	if !has(result, "persistedOutputSize") {
		text.Extent = &conversationv1.AgentBashOutputText_Whole{Whole: &conversationv1.AgentBashOutputWhole{}}
		return
	}
	total := uint64(number(pick(result, "persistedOutputSize", "persisted_output_size")))
	inline := uint64(len(text.GetStdout()) + len(text.GetStderr()))
	var omitted uint64
	if total > inline {
		omitted = total - inline
	}
	text.Extent = &conversationv1.AgentBashOutputText_Partial{Partial: &conversationv1.AgentBashOutputPartial{
		BytesOmitted: omitted,
		Spilled:      bashSpilledOutput(result, total),
	}}
}

// bashSpilledOutput names the file the WHOLE output was kept in, or nil when the
// producer kept none.
//
// WITHOUT A PATH A TRUNCATION IS A DEAD END, and that is a real state the proto
// spells as an unset field: the omitted bytes are simply gone, and inventing a
// path would offer a reader a file that is not there.
func bashSpilledOutput(result map[string]any, total uint64) *conversationv1.AgentBashSpilledOutput {
	path := str(pick(result, "persistedOutputPath", "persisted_output_path"))
	if path == "" {
		return nil
	}
	return &conversationv1.AgentBashSpilledOutput{Path: path, SizeBytes: total}
}

// sendMessageSuccess states HOW the message got there: one costs nothing beyond
// the message, the other RESTARTED A DORMANT AGENT, which begins consuming
// tokens again.
func sendMessageSuccess(call openCall, result map[string]any, ts int64) *conversationv1.AgentSendMessageSuccess {
	recipient := firstNonEmpty(str(result["resumedAgentId"]), sendAddressedTo(call.input))
	success := &conversationv1.AgentSendMessageSuccess{
		RecipientAgentId: agentID(recipient),
		SettledAt:        settledAt(ts, call.startedAt),
		// RESTATED so the settled frame stands alone: the start it upserts
		// over is gone once it lands, and a replay draws the send from this.
		AddressedTo: sendAddressedTo(call.input),
		Summary:     sendMessageSummary(call.input),
	}
	if str(result["resumedAgentId"]) != "" {
		success.Delivery = &conversationv1.AgentSendMessageSuccess_ResumedRecipient{ResumedRecipient: &conversationv1.AgentSendMessageResumedRecipient{}}
	} else {
		success.Delivery = &conversationv1.AgentSendMessageSuccess_QueuedToLive{QueuedToLive: &conversationv1.AgentSendMessageQueuedToLive{}}
	}
	return success
}

// webFetchSuccess: an HTTP error page is still a SERVED ANSWER, so the status
// rides beside the content rather than routing to the failure arm.
func webFetchSuccess(call openCall, result map[string]any) *conversationv1.AgentWebFetchSuccess {
	url := firstNonEmpty(str(result["url"]), str(call.input["url"]))
	return &conversationv1.AgentWebFetchSuccess{
		Target: &conversationv1.AgentWebFetchTarget{Url: url},
		Status: &conversationv1.AgentWebFetchHttpStatus{
			Code: uint32(number(result["code"])),
			Text: str(result["codeText"]),
		},
		Result:     str(result["result"]),
		Bytes:      uint64(number(result["bytes"])),
		DurationMs: uint64(number(result["durationMs"])),
		// PRESENCE IS THE FACT: the vendor states the artifact route by
		// supplying the descriptor, never by a boolean.
		ArtifactRead: obj(result["artifactRead"]) != nil,
	}
}

// webSearchSuccess preserves the engine's ORDER across both entry kinds: the
// producer's array is heterogeneous, HIT GROUPS mixed with bare narration
// strings.
//
// A GROUP IS NOT A LINK. `WebSearchOutput.results` holds
// `{ tool_use_id, content: {title,url}[] } | string` — the object is the
// vendor's batching of ONE server-side call, and the pages are inside its
// `content` array. Reading `title`/`url` off the GROUP finds neither, which
// minted one link with an empty title and an empty url and dropped every page
// the search actually found: the card drew an invisible dead row where two
// clickable results belonged. So a group is FLATTENED, one link per hit in
// order, exactly as the stream plane's convert/tools/web-search.ts does — the
// two planes mint the identical answer for one search.
func (c *Converter) webSearchSuccess(call openCall, result map[string]any, at Attribution) *conversationv1.AgentWebSearchSuccess {
	success := &conversationv1.AgentWebSearchSuccess{
		Query: &conversationv1.AgentWebSearchQuery{
			Terms: firstNonEmpty(str(result["query"]), str(pick(call.input, "query", "terms"))),
		},
		SearchCount:     uint32(number(result["searchCount"])),
		DurationSeconds: number(result["durationSeconds"]),
	}
	for _, raw := range list(result["results"]) {
		if text, ok := raw.(string); ok {
			success.Results = append(success.Results, &conversationv1.AgentWebSearchResult{
				Entry: &conversationv1.AgentWebSearchResult_Note{Note: &conversationv1.AgentWebSearchNote{Text: text}},
			})
			continue
		}
		group := obj(raw)
		if group == nil {
			c.log.With(at.ctxWarn("web-search")).With(logging.Context{ActivityID: call.activityID}).
				Log("a web search result entry was neither a narration line nor a hit group; it is dropped")
			continue
		}
		hits, ok := group["content"]
		if !ok {
			c.log.With(at.ctxWarn("web-search")).With(logging.Context{ActivityID: call.activityID}).
				Log("a web search hit group carried no content array; it is dropped")
			continue
		}
		for _, hit := range list(hits) {
			page := obj(hit)
			url := str(page["url"])
			if url == "" {
				// A hit with no url is not a page anyone can open, and a link
				// row built around an empty href is a dead row drawn as a
				// live one.
				c.log.With(at.ctxWarn("web-search")).With(logging.Context{ActivityID: call.activityID}).
					Log("a web search hit named no url; it is dropped rather than drawn as a dead link")
				continue
			}
			success.Results = append(success.Results, &conversationv1.AgentWebSearchResult{
				Entry: &conversationv1.AgentWebSearchResult_Link{Link: &conversationv1.AgentWebSearchLink{
					Title: str(page["title"]),
					Url:   url,
				}},
			})
		}
	}
	return success
}

// wakeupSuccess reads the RECEIPT rather than the request: `stopped` in the
// output is the stop's own receipt, and the vendor's exclusivity rule (a stop
// makes every other input field ignored) is what makes the arms exclusive.
func wakeupSuccess(call openCall, result map[string]any) *conversationv1.AgentScheduleWakeupSuccess {
	if boolean(result["stopped"]) || boolean(call.input["stop"]) {
		return &conversationv1.AgentScheduleWakeupSuccess{
			Outcome: &conversationv1.AgentScheduleWakeupSuccess_Stopped{Stopped: &conversationv1.AgentScheduleWakeupStopped{
				CancelledWakeups: uint32(number(result["cancelledWakeups"])),
			}},
		}
	}
	return &conversationv1.AgentScheduleWakeupSuccess{
		Outcome: &conversationv1.AgentScheduleWakeupSuccess_Scheduled{Scheduled: &conversationv1.AgentScheduleWakeupScheduled{
			WakeAtMs:            wakeAtMs(result),
			ClampedDelaySeconds: uint32(number(result["clampedDelaySeconds"])),
			WasClamped:          boolean(result["wasClamped"]),
		}},
	}
}

// wakeAtMs reads the instant the wakeup fires — THE fact the footer's countdown
// ticks from. The vendor states `scheduledFor` as epoch millis; an older
// spelling of it as an RFC3339 string is still read rather than dropped.
func wakeAtMs(result map[string]any) int64 {
	if ms := optionalInt64(result, "scheduledFor"); ms != nil {
		return *ms
	}
	return parseInstant(str(result["scheduledFor"]))
}

func artifactSuccess(call openCall, result map[string]any) *conversationv1.AgentArtifactSuccess {
	if str(call.input["action"]) == "list" {
		return &conversationv1.AgentArtifactSuccess{
			Outcome: &conversationv1.AgentArtifactSuccess_Listed{Listed: &conversationv1.AgentArtifactListed{}},
		}
	}
	return &conversationv1.AgentArtifactSuccess{
		Outcome: &conversationv1.AgentArtifactSuccess_Published{Published: &conversationv1.AgentArtifactPublished{
			Url:   str(result["url"]),
			Title: optionalString(result["title"]),
		}},
	}
}

func planModeSuccess(call openCall, result map[string]any, ts int64) *conversationv1.AgentPlanModeSuccess {
	success := &conversationv1.AgentPlanModeSuccess{SettledAt: settledAt(ts, call.startedAt)}
	if call.name == "ExitPlanMode" {
		exited := &conversationv1.AgentPlanModeExited{
			PlanWasEdited:          boolean(result["planWasEdited"]),
			FilePath:               optionalString(pick(result, "planFilePath", "filePath")),
			IsAgent:                boolean(result["isAgent"]),
			HasTaskTool:            boolean(result["hasTaskTool"]),
			AwaitingLeaderApproval: boolean(result["awaitingLeaderApproval"]),
		}
		if plan := str(pick(call.input, "plan", "content")); plan != "" {
			exited.Plan = &conversationv1.AgentResponseProse{Markdown: plan}
		}
		success.Act = &conversationv1.AgentPlanModeSuccess_Exited{Exited: exited}
		return success
	}
	success.Act = &conversationv1.AgentPlanModeSuccess_Entered{Entered: &conversationv1.AgentPlanModeEntered{
		Message: str(result["message"]),
	}}
	return success
}

// findingsSuccess reads the review's typed report in the TOOL'S OWN ORDER —
// most-severe first by its contract; a consumer never re-sorts. Empty is a real
// report: a review that found nothing.
//
// THE TOOL'S OWN OUTPUT IS PREFERRED over the call's input: what the review
// actually reported is what the tool answered with, and the verify pass's
// VERDICT and a re-report's OUTCOME exist only there. The input is the fallback
// for a vendor that echoed nothing, since the report is otherwise lost.
func findingsSuccess(call openCall, result map[string]any, ts int64) *conversationv1.AgentReportFindingsSuccess {
	success := &conversationv1.AgentReportFindingsSuccess{SettledAt: settledAt(ts, call.startedAt)}
	if level := readEffort(pick(call.input, "effort", "level")); level != nil {
		success.Level = *level
	}
	reported := pick(result, "findings")
	if !has(result, "findings") {
		reported = pick(call.input, "findings")
	}
	for _, raw := range list(reported) {
		f := obj(raw)
		if f == nil {
			continue
		}
		finding := &conversationv1.AgentFinding{
			File:            str(f["file"]),
			Line:            optionalUint32(f, "line"),
			Summary:         str(f["summary"]),
			ShortSummary:    optionalString(pick(f, "short_summary", "shortSummary")),
			FailureScenario: str(pick(f, "failure_scenario", "failureScenario")),
			Category:        optionalString(f["category"]),
		}
		setFindingVerdict(finding, str(f["verdict"]))
		setFindingOutcome(finding, str(pick(f, "outcome")))
		success.Findings = append(success.Findings, finding)
	}
	return success
}

// setFindingVerdict states what the verify pass concluded. UNSET when no verify
// pass ran, and UNSET for a word this contract has no arm for — a verdict
// nobody stated is not a verdict of "plausible".
func setFindingVerdict(finding *conversationv1.AgentFinding, verdict string) {
	switch verdict {
	case "CONFIRMED":
		finding.Verdict = &conversationv1.AgentFinding_Confirmed{Confirmed: &conversationv1.AgentFindingConfirmed{}}
	case "PLAUSIBLE":
		finding.Verdict = &conversationv1.AgentFinding_Plausible{Plausible: &conversationv1.AgentFindingPlausible{}}
	}
}

// setFindingOutcome states what became of a finding, which only a RE-REPORT
// after fixes were applied carries.
func setFindingOutcome(finding *conversationv1.AgentFinding, outcome string) {
	switch outcome {
	case "fixed":
		finding.Outcome = &conversationv1.AgentFinding_Fixed{Fixed: &conversationv1.AgentFindingFixed{}}
	case "skipped":
		finding.Outcome = &conversationv1.AgentFinding_Skipped{Skipped: &conversationv1.AgentFindingSkipped{}}
	case "no_change_needed", "noChangeNeeded":
		finding.Outcome = &conversationv1.AgentFinding_NoChangeNeeded{NoChangeNeeded: &conversationv1.AgentFindingNoChangeNeeded{}}
	}
}

// worktreeSuccess states WHERE THE SESSION MOVED. The tree's path is the whole
// subject of both arms — the divider names it — and the vendor spells it
// `worktreePath`, never `path`, so reading the wrong key leaves the divider
// blank. What became of the tree comes from the vendor's own `action`, not from
// a boolean it does not emit.
func worktreeSuccess(call openCall, result map[string]any, ts int64) *conversationv1.AgentWorktreeSuccess {
	success := &conversationv1.AgentWorktreeSuccess{SettledAt: settledAt(ts, call.startedAt)}
	path := str(pick(result, "worktreePath", "worktree_path", "path"))
	branch := optionalString(pick(result, "worktreeBranch", "worktree_branch", "branch"))
	if call.name == "ExitWorktree" {
		exited := &conversationv1.AgentWorktreeExited{
			OriginalCwd:     str(pick(result, "originalCwd", "original_cwd")),
			Path:            path,
			Branch:          branch,
			TmuxSessionName: optionalString(pick(result, "tmuxSessionName", "tmux_session_name")),
			Message:         str(result["message"]),
		}
		setWorktreeExitOutcome(exited, result)
		success.Act = &conversationv1.AgentWorktreeSuccess_Exited{Exited: exited}
		return success
	}
	success.Act = &conversationv1.AgentWorktreeSuccess_Entered{Entered: &conversationv1.AgentWorktreeEntered{
		Path:    path,
		Branch:  branch,
		Message: str(result["message"]),
	}}
	return success
}

// setWorktreeExitOutcome reads what became of the tree from the vendor's action
// word. The discarded figures stay UNSET when it stated none — "no figure" is
// not "none were discarded" — and the outcome itself stays UNSET for an action
// this contract has no arm for rather than claiming the tree was kept.
func setWorktreeExitOutcome(exited *conversationv1.AgentWorktreeExited, result map[string]any) {
	switch {
	case str(result["action"]) == "remove" || boolean(pick(result, "removed", "wasRemoved")):
		exited.Outcome = &conversationv1.AgentWorktreeExited_Removed{Removed: &conversationv1.AgentWorktreeRemoved{
			DiscardedFiles:   optionalUint32(result, "discardedFiles"),
			DiscardedCommits: optionalUint32(result, "discardedCommits"),
		}}
	case str(result["action"]) == "keep":
		exited.Outcome = &conversationv1.AgentWorktreeExited_Kept{Kept: &conversationv1.AgentWorktreeKept{}}
	}
}

func cronSuccess(call openCall, result map[string]any, ts int64) *conversationv1.AgentCronSuccess {
	success := &conversationv1.AgentCronSuccess{SettledAt: settledAt(ts, call.startedAt)}
	switch call.name {
	case "CronDelete":
		success.Act = &conversationv1.AgentCronSuccess_Deleted{Deleted: &conversationv1.AgentCronDeleted{
			JobId: firstNonEmpty(str(pick(result, "job_id", "jobId")), str(pick(call.input, "job_id", "jobId", "id"))),
		}}
	case "CronList":
		listed := &conversationv1.AgentCronListed{}
		for _, raw := range list(pick(result, "jobs")) {
			job := obj(raw)
			if job == nil {
				continue
			}
			listed.Jobs = append(listed.Jobs, &conversationv1.AgentCronJob{
				JobId:         str(pick(job, "job_id", "jobId", "id")),
				Cron:          str(job["cron"]),
				HumanSchedule: str(pick(job, "human_schedule", "humanSchedule")),
				Prompt:        str(job["prompt"]),
				Recurring:     boolean(job["recurring"]),
				Durable:       boolean(job["durable"]),
			})
		}
		success.Act = &conversationv1.AgentCronSuccess_Listed{Listed: listed}
	default:
		success.Act = &conversationv1.AgentCronSuccess_Created{Created: &conversationv1.AgentCronCreated{
			JobId:         str(pick(result, "job_id", "jobId", "id")),
			HumanSchedule: str(pick(result, "human_schedule", "humanSchedule")),
			Recurring:     boolean(result["recurring"]),
			Durable:       boolean(result["durable"]),
		}}
	}
	return success
}

// pushSuccess states WHETHER THE VENDOR DELIVERED, which matters: an agent that
// believes it notified an absent user, and did not, left them waiting on nothing.
func pushSuccess(call openCall, result map[string]any, ts int64) *conversationv1.AgentPushNotificationSuccess {
	success := &conversationv1.AgentPushNotificationSuccess{SettledAt: settledAt(ts, call.startedAt)}
	if boolean(pick(result, "pushSent", "push_sent")) || boolean(pick(result, "localSent", "local_sent")) {
		success.Outcome = &conversationv1.AgentPushNotificationSuccess_Sent{Sent: &conversationv1.AgentPushNotificationSent{
			PushSent:  boolean(pick(result, "pushSent", "push_sent")),
			LocalSent: boolean(pick(result, "localSent", "local_sent")),
			SentAtMs:  sentAtMs(result),
		}}
		return success
	}
	notSent := &conversationv1.AgentPushNotificationNotSent{}
	// The vendor's own closed set, under its own key. A word outside it leaves
	// the DELIVERY fact stated — it was not sent — and only the reason unstated;
	// defaulting to config_off would blame a setting nothing named.
	switch str(pick(result, "disabledReason", "disabled_reason", "reason", "notSentReason")) {
	case "user_present":
		notSent.Reason = &conversationv1.AgentPushNotificationNotSent_UserPresent{UserPresent: &conversationv1.AgentPushNotificationUserPresent{}}
	case "no_transport":
		notSent.Reason = &conversationv1.AgentPushNotificationNotSent_NoTransport{NoTransport: &conversationv1.AgentPushNotificationNoTransport{}}
	case "config_off":
		notSent.Reason = &conversationv1.AgentPushNotificationNotSent_ConfigOff{ConfigOff: &conversationv1.AgentPushNotificationConfigOff{}}
	}
	success.Outcome = &conversationv1.AgentPushNotificationSuccess_NotSent{NotSent: notSent}
	return success
}

// sentAtMs reads the vendor's send instant. It is an ISO string on the wire,
// and genuinely absent sometimes — resumed sessions replay pre-`sentAt` outputs
// verbatim — so the field stays UNSET rather than falling back to the settle
// instant, which is a different clock reading a different moment.
func sentAtMs(result map[string]any) *int64 {
	if ms := optionalInt64(result, "sentAtMs"); ms != nil {
		return ms
	}
	iso := parseInstant(str(pick(result, "sentAt", "sent_at")))
	if iso == 0 {
		return nil
	}
	return &iso
}

// bashResultImage reads the image an answering `tool_result` carried: the bytes
// as the vendor base64'd them, and their media type.
//
// A BLOCK WHOSE PAYLOAD DOES NOT DECODE YIELDS NOTHING RATHER THAN HALF AN
// IMAGE. The media type is still returned when it was stated, so a consumer's
// own refusal can name what it was handed; the daemon draws nothing for an
// image missing either half, which is the honest end of a payload we cannot
// reconstruct.
func bashResultImage(block map[string]any) ([]byte, string) {
	blocks, ok := block["content"].([]any)
	if !ok {
		return nil, ""
	}
	for _, el := range blocks {
		content := obj(el)
		if content == nil || str(content["type"]) != "image" {
			continue
		}
		source := obj(content["source"])
		if source == nil {
			continue
		}
		mediaType := str(source["media_type"])
		if str(source["type"]) != "base64" {
			return nil, mediaType
		}
		data, err := base64.StdEncoding.DecodeString(str(source["data"]))
		if err != nil {
			return nil, mediaType
		}
		return data, mediaType
	}
	return nil, ""
}
