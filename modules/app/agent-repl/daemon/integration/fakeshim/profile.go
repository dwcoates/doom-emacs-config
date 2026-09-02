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
