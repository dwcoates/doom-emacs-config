package convert

// streamowned.go — UNITS THE STREAM PLANE OWNS WHOLE.
//
// A DROP HERE IS NEITHER THE EXEMPT SET NOR RESIDUE. The unit IS carried on the
// feed — by the SHIM, the only plane that can state it truthfully — so it is
// not "a tool the feed does not show" (the exempt set) and not "a record we
// could not convert" (residue). It is one unit with ONE author.
//
// AskUserQuestion is the whole set, and the reason is the answer's SHAPE. The
// shim gates the ask through `canUseTool` and receives the user's answer as
// `AgentQuestionAnswers` — a REPEATED `chosen` per question — then joins the
// picked labels into the vendor's one-string-per-question form exactly once, at
// the SDK boundary. The transcript records only that joined string, and
// question.proto's retired tag 4 states it is structurally indistinguishable
// from a selection carrying typed text: a file-plane reader cannot split it
// back, so two picks come back as ONE label and a label containing a comma
// cannot be told from two labels. Re-authored under `question:<tool_use_id>` —
// the key BOTH planes once minted — that lossy frame SUPERSEDED the shim's, and
// a file-plane start frame arriving after the shim's settle re-opened an
// answered ask. Both are the same defect: a second authority on one question.
var streamOwnedTools = map[string]bool{
	"AskUserQuestion": true,
}

// IsStreamOwned reports whether the stream plane authors this tool's unit whole,
// so the file plane converts neither its call nor its result.
func IsStreamOwned(tool string) bool { return streamOwnedTools[tool] }
