package convert

// results.go — reading the vendor's `toolUseResult` object into the settled arms.
//
// THE FIDELITY PRINCIPLE GOVERNS THESE: a vendor field lands even when no UI
// draws it. What is deliberately absent is anything the vendor did not state —
// an absent figure stays UNSET rather than becoming a zero a consumer would draw
// as a fact.

import (
	conversationv1 "agentrepl/proto/conversation/v1"
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
		SettledAt: settledAt(ts),
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
		SettledAt:    settledAt(ts),
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

// writePatch prefers the patch the vendor STATED and falls back to diffing the
// two versions it handed over — which is the whole story for a creation, whose
// structured patch the vendor leaves empty.
func writePatch(result map[string]any) []*conversationv1.FilePatchHunk {
	if stated := patchHunks(result["structuredPatch"]); len(stated) > 0 {
		return stated
	}
	if !has(result, "content") {
		return nil
	}
	return diffHunks(str(result["originalFile"]), str(result["content"]))
}

func editSuccess(call openCall, result map[string]any, ts int64) *conversationv1.AgentEditSuccess {
	path := firstNonEmpty(str(result["filePath"]), str(pick(call.input, "file_path", "path")))
	return &conversationv1.AgentEditSuccess{
		Path:         &conversationv1.ReadPath{Path: path},
		Patch:        patchHunks(result["structuredPatch"]),
		UserModified: boolean(result["userModified"]),
		SettledAt:    settledAt(ts),
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
func grepSuccess(call openCall, block map[string]any, ts int64) *conversationv1.AgentGrepSuccess {
	success := &conversationv1.AgentGrepSuccess{
		Query:     grepQuery(call.input),
		SettledAt: settledAt(ts),
	}
	text := flattenResultText(block["content"])
	switch str(pick(call.input, "output_mode", "outputMode")) {
	case "files_with_matches":
		paths := nonEmptyLines(text)
		files := &conversationv1.AgentGrepFiles{Paths: paths}
		files.Extent = &conversationv1.AgentGrepFiles_All{All: &conversationv1.AgentGrepFilesAll{
			FilesReturned: uint32(len(paths)),
		}}
		success.Matches = &conversationv1.AgentGrepSuccess_Files{Files: files}
	case "count":
		success.Matches = &conversationv1.AgentGrepSuccess_Count{Count: &conversationv1.AgentGrepCount{
			Matches: uint32(countMatches(text)),
		}}
	default:
		content := &conversationv1.AgentGrepContent{Content: text}
		content.Extent = &conversationv1.AgentGrepContent_All{All: &conversationv1.AgentGrepContentAll{
			LinesReturned: uint32(len(nonEmptyLines(text))),
		}}
		success.Matches = &conversationv1.AgentGrepSuccess_Content{Content: content}
	}
	return success
}

func globSuccess(call openCall, block map[string]any, ts int64) *conversationv1.AgentGlobSuccess {
	paths := nonEmptyLines(flattenResultText(block["content"]))
	return &conversationv1.AgentGlobSuccess{
		Query:     globQuery(call.input),
		Paths:     paths,
		Extent:    &conversationv1.AgentGlobSuccess_All{All: &conversationv1.AgentGlobAll{FilesReturned: uint32(len(paths))}},
		SettledAt: settledAt(ts),
	}
}

// bashSuccess: a NONZERO EXIT IS STILL THIS ARM. The failure arm is for a call
// that could not be performed; a command that ran and failed ran, and what it
// printed is the answer the caller wanted.
func bashSuccess(call openCall, result map[string]any, ts int64) *conversationv1.AgentBashSuccess {
	success := &conversationv1.AgentBashSuccess{
		Command:   bashCommand(call.input),
		SettledAt: settledAt(ts),
	}
	output := bashOutput(result)
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
	if code := optionalInt64(result, "exitCode"); code != nil {
		completed.Termination = &conversationv1.AgentBashTermination{
			How: &conversationv1.AgentBashTermination_Exited{Exited: &conversationv1.AgentBashExited{Code: int32(*code)}},
		}
	}
	success.Outcome = &conversationv1.AgentBashSuccess_Completed{Completed: completed}
	return success
}

// bashOutput keeps stdout and stderr APART rather than interleaving them: a
// consumer that wants them woven can concatenate, while one handed a single blob
// can never pull them apart again.
func bashOutput(result map[string]any) *conversationv1.AgentBashOutput {
	if boolean(result["isImage"]) {
		return &conversationv1.AgentBashOutput{
			Form: &conversationv1.AgentBashOutput_Image{Image: &conversationv1.AgentBashOutputImage{
				MediaType: str(result["mediaType"]),
			}},
		}
	}
	text := &conversationv1.AgentBashOutputText{
		Stdout: str(result["stdout"]),
		Stderr: str(result["stderr"]),
	}
	text.Extent = &conversationv1.AgentBashOutputText_Whole{Whole: &conversationv1.AgentBashOutputWhole{}}
	return &conversationv1.AgentBashOutput{Form: &conversationv1.AgentBashOutput_Text{Text: text}}
}

// sendMessageSuccess states HOW the message got there: one costs nothing beyond
// the message, the other RESTARTED A DORMANT AGENT, which begins consuming
// tokens again.
func sendMessageSuccess(call openCall, result map[string]any, ts int64) *conversationv1.AgentSendMessageSuccess {
	recipient := firstNonEmpty(str(result["resumedAgentId"]), str(pick(call.input, "to", "agent", "recipient")))
	success := &conversationv1.AgentSendMessageSuccess{
		RecipientAgentId: agentID(recipient),
		SettledAt:        settledAt(ts),
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
	}
}

// webSearchSuccess preserves the engine's ORDER across both entry kinds: the
// producer's array is heterogeneous, links mixed with bare narration strings.
func webSearchSuccess(call openCall, result map[string]any) *conversationv1.AgentWebSearchSuccess {
	success := &conversationv1.AgentWebSearchSuccess{
		Query: &conversationv1.AgentWebSearchQuery{
			Terms: firstNonEmpty(str(result["query"]), str(pick(call.input, "query", "terms"))),
		},
		SearchCount:     uint32(number(result["searchCount"])),
		DurationSeconds: number(result["durationSeconds"]),
	}
	for _, raw := range list(result["results"]) {
		switch value := raw.(type) {
		case string:
			success.Results = append(success.Results, &conversationv1.AgentWebSearchResult{
				Entry: &conversationv1.AgentWebSearchResult_Note{Note: &conversationv1.AgentWebSearchNote{Text: value}},
			})
		case map[string]any:
			success.Results = append(success.Results, &conversationv1.AgentWebSearchResult{
				Entry: &conversationv1.AgentWebSearchResult_Link{Link: &conversationv1.AgentWebSearchLink{
					Title: str(value["title"]),
					Url:   str(value["url"]),
				}},
			})
		}
	}
	return success
}

func wakeupSuccess(call openCall, result map[string]any) *conversationv1.AgentScheduleWakeupSuccess {
	if boolean(call.input["stop"]) {
		return &conversationv1.AgentScheduleWakeupSuccess{
			Outcome: &conversationv1.AgentScheduleWakeupSuccess_Stopped{Stopped: &conversationv1.AgentScheduleWakeupStopped{
				CancelledWakeups: uint32(number(result["cancelledWakeups"])),
			}},
		}
	}
	return &conversationv1.AgentScheduleWakeupSuccess{
		Outcome: &conversationv1.AgentScheduleWakeupSuccess_Scheduled{Scheduled: &conversationv1.AgentScheduleWakeupScheduled{
			WakeAtMs:            parseInstant(str(result["scheduledFor"])),
			ClampedDelaySeconds: uint32(number(result["clampedDelaySeconds"])),
			WasClamped:          boolean(result["wasClamped"]),
		}},
	}
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
	success := &conversationv1.AgentPlanModeSuccess{SettledAt: settledAt(ts)}
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
func findingsSuccess(call openCall, ts int64) *conversationv1.AgentReportFindingsSuccess {
	success := &conversationv1.AgentReportFindingsSuccess{SettledAt: settledAt(ts)}
	if level := readEffort(pick(call.input, "effort", "level")); level != nil {
		success.Level = *level
	}
	for _, raw := range list(pick(call.input, "findings")) {
		f := obj(raw)
		if f == nil {
			continue
		}
		finding := &conversationv1.AgentFinding{
			File:            str(f["file"]),
			Line:            optionalUint32(f, "line"),
			Summary:         str(f["summary"]),
			ShortSummary:    optionalString(f["short_summary"]),
			FailureScenario: str(pick(f, "failure_scenario", "failureScenario")),
			Category:        optionalString(f["category"]),
		}
		success.Findings = append(success.Findings, finding)
	}
	return success
}

func worktreeSuccess(call openCall, result map[string]any, ts int64) *conversationv1.AgentWorktreeSuccess {
	success := &conversationv1.AgentWorktreeSuccess{SettledAt: settledAt(ts)}
	if call.name == "ExitWorktree" {
		exited := &conversationv1.AgentWorktreeExited{
			OriginalCwd:     str(pick(result, "originalCwd", "original_cwd")),
			Path:            str(result["path"]),
			Branch:          optionalString(result["branch"]),
			TmuxSessionName: optionalString(pick(result, "tmuxSessionName", "tmux_session_name")),
			Message:         str(result["message"]),
		}
		if boolean(pick(result, "removed", "wasRemoved")) {
			exited.Outcome = &conversationv1.AgentWorktreeExited_Removed{Removed: &conversationv1.AgentWorktreeRemoved{
				DiscardedFiles:   optionalUint32(result, "discardedFiles"),
				DiscardedCommits: optionalUint32(result, "discardedCommits"),
			}}
		} else {
			exited.Outcome = &conversationv1.AgentWorktreeExited_Kept{Kept: &conversationv1.AgentWorktreeKept{}}
		}
		success.Act = &conversationv1.AgentWorktreeSuccess_Exited{Exited: exited}
		return success
	}
	success.Act = &conversationv1.AgentWorktreeSuccess_Entered{Entered: &conversationv1.AgentWorktreeEntered{
		Path:    str(result["path"]),
		Branch:  optionalString(result["branch"]),
		Message: str(result["message"]),
	}}
	return success
}

func cronSuccess(call openCall, result map[string]any, ts int64) *conversationv1.AgentCronSuccess {
	success := &conversationv1.AgentCronSuccess{SettledAt: settledAt(ts)}
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
func pushSuccess(result map[string]any, ts int64) *conversationv1.AgentPushNotificationSuccess {
	success := &conversationv1.AgentPushNotificationSuccess{SettledAt: settledAt(ts)}
	if boolean(pick(result, "pushSent", "push_sent")) || boolean(pick(result, "localSent", "local_sent")) {
		success.Outcome = &conversationv1.AgentPushNotificationSuccess_Sent{Sent: &conversationv1.AgentPushNotificationSent{
			PushSent:  boolean(pick(result, "pushSent", "push_sent")),
			LocalSent: boolean(pick(result, "localSent", "local_sent")),
			SentAtMs:  optionalInt64(result, "sentAtMs"),
		}}
		return success
	}
	notSent := &conversationv1.AgentPushNotificationNotSent{}
	switch str(pick(result, "reason", "notSentReason")) {
	case "user_present":
		notSent.Reason = &conversationv1.AgentPushNotificationNotSent_UserPresent{UserPresent: &conversationv1.AgentPushNotificationUserPresent{}}
	case "no_transport":
		notSent.Reason = &conversationv1.AgentPushNotificationNotSent_NoTransport{NoTransport: &conversationv1.AgentPushNotificationNoTransport{}}
	default:
		notSent.Reason = &conversationv1.AgentPushNotificationNotSent_ConfigOff{ConfigOff: &conversationv1.AgentPushNotificationConfigOff{}}
	}
	success.Outcome = &conversationv1.AgentPushNotificationSuccess_NotSent{NotSent: notSent}
	return success
}
