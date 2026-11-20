package testenv

import (
	"fmt"
	"os"
	"path/filepath"
	"regexp"
	"testing"
)

// ProfileDir names the directory a profiling run (testrun.v1.TestRunRequest
// .profile) gives each unit for its profiles: the unit's own CPU profile lands
// in it, and every harness points the production processes it starts (the
// daemon, shims, the store, the sidecar) at a directory under it, named by the
// test that started them (ProfileDirFor). Unset, the run is not profiling.
const ProfileDir = "AGENT_REPL_TEST_PROFILE_DIR"

// ExitProfileEnv is the variable a production process (the daemon, a shim,
// the store, the sidecar) reads at its start to profile its whole life into a
// directory, writing the profile when it stops: proto/vocab/profile-session.json
// `exit_profile_env`, which this package's test asserts it against.
const ExitProfileEnv = "AGENT_REPL_EXIT_PROFILE_DIR"

// unsafeName is every character a profile's file or directory name never
// carries: a unit id's ':' and '/', a test path's '/', and anything a shell
// or a profile viewer would read as something else.
var unsafeName = regexp.MustCompile(`[^A-Za-z0-9._-]+`)

// ProfileName is NAME (a unit id, a test path) as one path component of a
// profiles directory: every run of characters outside [A-Za-z0-9._-] becomes
// one '_'. testrun names its unit directories by it and the harnesses their
// test directories, so a reader finds a unit's or a test's profiles by its
// name.
func ProfileName(name string) string {
	if name == "" {
		panic("testenv: a profile is named by a unit or a test, never by nothing")
	}
	return unsafeName.ReplaceAllString(name, "_")
}

// ProfileDirFor answers the directory the processes a test named TEST starts
// write their profiles into, created: ProfileDir's subdirectory named by
// ProfileName(TEST). It answers "" when the process does not run under a
// profiling run, and an error when the directory cannot be made, which the
// harness fails its test with rather than profiling into nowhere.
func ProfileDirFor(getenv func(string) string, test string) (string, error) {
	root := getenv(ProfileDir)
	if root == "" {
		return "", nil
	}
	if !filepath.IsAbs(root) {
		return "", fmt.Errorf("testenv: %s names %q, which is not absolute", ProfileDir, root)
	}
	dir := filepath.Join(root, ProfileName(test))
	if err := os.MkdirAll(dir, 0o755); err != nil {
		return "", fmt.Errorf("testenv: create the profiles directory of %s: %w", test, err)
	}
	return dir, nil
}

// ExitProfileEnvFor answers the environment a harness adds to the production
// processes the test named TEST starts, so each profiles its whole life into
// the test's profiles directory (ProfileDirFor): one ExitProfileEnv binding
// under a profiling run, none outside one. The directory is answered beside
// it, "" when there is none.
func ExitProfileEnvFor(getenv func(string) string, test string) ([]string, string, error) {
	dir, err := ProfileDirFor(getenv, test)
	if err != nil || dir == "" {
		return nil, "", err
	}
	return []string{ExitProfileEnv + "=" + dir}, dir, nil
}

// ExitProfile is ExitProfileEnvFor for the test T, read from this process's
// environment, failing T when the directory cannot be made: the one call
// every harness makes before it starts a production process.
func ExitProfile(t testing.TB) (env []string, dir string) {
	t.Helper()
	env, dir, err := ExitProfileEnvFor(os.Getenv, t.Name())
	if err != nil {
		t.Fatalf("%v", err)
	}
	return env, dir
}
