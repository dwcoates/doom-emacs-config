package main

import (
	"crypto/md5"
	"encoding/hex"
	"encoding/json"
	"fmt"
	"os"
	"path/filepath"
)

// EnvProfileDir names a directory of startup profiles the harness writes
// BEFORE the daemon spawns the fake, for the behaviors that must already be
// in force by the time the first request arrives (a bring-up death, a delayed
// readiness push, a cold resume). Everything else is scripted live over the
// control socket.
const EnvProfileDir = "FAKESHIM_PROFILE_DIR"

// EnvBuildSHA overrides the runtime shim_build_sha the fake reports.
const EnvBuildSHA = "FAKESHIM_BUILD_SHA"

// Profile is one workspace's startup script. The harness names the file
// <md5hex(clean abs workspace dir)>.json inside EnvProfileDir, falling back to
// default.json.
type Profile struct {
	// BuildSHA overrides the reported runtime shim_build_sha.
	BuildSHA string `json:"build_sha,omitempty"`
	// DelayDiagnostics withholds the automatic healthy diagnostics push on the
	// FIRST session stream, so readiness only arrives when the test pushes it.
	// Later streams open normally: the flag exists to gate the daemon's
	// bring-up, and withholding on every stream would hang the session watcher
	// the daemon opens once bring-up has already succeeded.
	DelayDiagnostics bool `json:"delay_diagnostics,omitempty"`
	// OpeningFault makes the opening diagnostics push of EVERY session stream
	// UNHEALTHY, carrying one store_unreachable fault with this detail. It
	// models the shim that answers at once and stands on a fault it never
	// clears — the realtest-1 survivor — so the daemon's bring-up is offered
	// an ANSWER rather than silence. It is a PROFILE rather than a scripted
	// answer because the opening frame of the daemon's very first stream is
	// what it governs.
	OpeningFault string `json:"opening_fault,omitempty"`
	// OpeningNetworkUnreachable makes the opening diagnostics push of EVERY
	// session stream UNHEALTHY with one network_unreachable fault carrying
	// this detail: a shim that saw this machine offline.
	OpeningNetworkUnreachable string `json:"opening_network_unreachable,omitempty"`
	// ExitOn kills the process at a named moment: "startup", "start_session"
	// or "watch_session".
	ExitOn string `json:"exit_on,omitempty"`
	// ExitCode is the status ExitOn dies with.
	ExitCode int `json:"exit_code,omitempty"`
	// Stderr is written to stderr before an ExitOn death, as failure evidence.
	Stderr string `json:"stderr,omitempty"`
	// ColdOnResume answers StartSession(resume) with the cold failure carrying
	// these facts.
	ColdOnResume *ColdFacts `json:"cold_on_resume,omitempty"`
	// VendorStartFailed answers EVERY StartSession with the
	// `vendor_start_failed` refusal carrying this detail: the shim process is
	// up and serving and only the vendor failed to start inside it. It is a
	// PROFILE rather than a scripted answer because StartSession is the
	// daemon's very first request, which a control-socket script would race.
	VendorStartFailed string `json:"vendor_start_failed,omitempty"`
	// VendorStartRetryable labels the VendorStartFailed refusal RETRYABLE
	// (shim.v1 StartSessionVendorStartFailed.retry); unset labels it REJECTED.
	VendorStartRetryable bool `json:"vendor_start_retryable,omitempty"`
	// VendorStartFailTimes answers the FIRST n StartSessions with a RETRYABLE
	// `vendor_start_failed` refusal carrying VendorStartFailDetail, and every
	// one after them normally, as a vendor that was slow to start does. It
	// drives the daemon's vendor-start retry run end to end.
	VendorStartFailTimes  int    `json:"vendor_start_fail_times,omitempty"`
	VendorStartFailDetail string `json:"vendor_start_fail_detail,omitempty"`
	// VendorStartRejectTimes answers the FIRST n StartSessions with a REJECTED
	// (non-retryable) `vendor_start_failed` refusal carrying
	// VendorStartFailDetail, and every one after them normally: a start that
	// fails outright and a later start on the same shim that succeeds.
	VendorStartRejectTimes int `json:"vendor_start_reject_times,omitempty"`
	// NoTranscriptUntilTurn withholds the conversation's transcript until its
	// FIRST TURN, which is what the real vendor does: StartSession assigns the
	// vendor session id, and the file only appears once there is something to
	// write into it. A session bounced before its first turn therefore names a
	// conversation with no transcript at all.
	NoTranscriptUntilTurn bool `json:"no_transcript_until_turn,omitempty"`
	// VendorSessionID pins the id a fresh StartSession mints, so a test can
	// predict the session lock's path.
	VendorSessionID string `json:"vendor_session_id,omitempty"`
	// LiveWork is what the opening states is ALREADY RUNNING: each element is
	// one binary-encoded conversation.v1 AgentDetachedWork, answered verbatim
	// as SessionStarted.live_work.
	//
	// It is a PROFILE rather than a scripted answer because the opening is the
	// daemon's very first request: a test that queued the answer over the
	// control socket would be racing the spawn it is scripting.
	LiveWork [][]byte `json:"live_work,omitempty"`
	// HibernateFailure makes EVERY Hibernate answer a transport-level error
	// carrying this detail, and HibernateTurnInFlight makes every one answer
	// the typed turn_in_flight refusal.
	//
	// THEY ARE PROFILE FIELDS RATHER THAN SCRIPTED ANSWERS BECAUSE THE DRAIN
	// SWEEP DOES NOT WAIT FOR THE TEST. A test whose subject is a hibernate
	// the shim keeps refusing runs the daemon at a 50ms idle cutoff and a
	// 50ms sweep, so the first sweep can land before the test has scripted
	// anything: the fake hibernates for real, the daemon stands the session
	// down, the fake exits, and the test either waits out its deadline for a
	// refusal that can no longer happen or fails writing to a control socket
	// whose process is gone. A profile is in force from the fake's BIRTH, so
	// there is no such window — and no guessed number of queued answers to
	// run out of either.
	HibernateFailure      string `json:"hibernate_failure,omitempty"`
	HibernateTurnInFlight bool   `json:"hibernate_turn_in_flight,omitempty"`
	// HangStartSession makes StartSession never answer, waiting out the
	// caller's context instead. It is a PROFILE rather than a scripted answer
	// because the daemon's boot bring-up sends StartSession as its very first
	// request, which a control-socket script would race.
	HangStartSession bool `json:"hang_start_session,omitempty"`
	// ResumeHistory is the conversation a RESUMED session already holds: each
	// element is one binary-encoded conversation.v1 HistoryEntry, stated
	// NEWEST FIRST as a producer serves a page. They seed the main agent's
	// book (book.go), which ReadHistory pages; a watch that asks for a repaint
	// (which the daemon never does) still opens with them.
	//
	// It models what the real shim does off the store — a resume serves the
	// agent's whole book, a fresh start serves an empty floor — and it is a
	// PROFILE rather than a scripted answer because the page is the opening
	// frame of a stream the daemon opens during bring-up, which a control
	// socket script would race.
	ResumeHistory [][]byte `json:"resume_history,omitempty"`
	// HistoryPageSize is the fake store's page size, DefaultHistoryPageSize
	// when unset: what ReadHistory pages each agent's book in (book.go).
	HistoryPageSize int `json:"history_page_size,omitempty"`
}

// ColdFacts are the shim's stated facts on a cold refusal.
type ColdFacts struct {
	ContextTokens   uint64 `json:"context_tokens"`
	LastRequestAtMS int64  `json:"last_request_at_ms"`
	RequestedModel  string `json:"requested_model"`
	CacheTTLMS      int64  `json:"cache_ttl_ms"`
}

// ProfileFileName is the per-workspace profile file name for a directory.
func ProfileFileName(absDir string) string {
	sum := md5.Sum([]byte(filepath.Clean(absDir)))
	return hex.EncodeToString(sum[:]) + ".json"
}

// LoadProfile reads the profile for a workspace directory. A missing profile
// is the zero profile; a malformed one is an error and never a default.
func LoadProfile(profileDir, absDir string) (Profile, error) {
	if profileDir == "" {
		return Profile{}, nil
	}
	for _, name := range []string{ProfileFileName(absDir), "default.json"} {
		raw, err := os.ReadFile(filepath.Join(profileDir, name))
		if os.IsNotExist(err) {
			continue
		}
		if err != nil {
			return Profile{}, fmt.Errorf("fakeshim: read profile %s: %w", name, err)
		}
		var p Profile
		if err := json.Unmarshal(raw, &p); err != nil {
			return Profile{}, fmt.Errorf("fakeshim: parse profile %s: %w", name, err)
		}
		return p, nil
	}
	return Profile{}, nil
}
