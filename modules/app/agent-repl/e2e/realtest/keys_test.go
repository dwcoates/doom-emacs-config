//go:build realtest

package realtest

import (
	"bufio"
	"context"
	"errors"
	"os"
	"os/exec"
	"path/filepath"
	"strconv"
	"strings"
	"sync"
	"testing"
	"time"
)

// THE KEY HELPER'S REFUSAL POLICY, TESTED WITHOUT AN EMACS AND WITHOUT A GUESS.
//
// keydriver.swift decides one thing this harness cannot afford to get wrong:
// whether a target the window server says cannot receive a key is POSTED INTO
// or REFUSED. The 2026-09-12 16:12 sweep is what happens when that decision is
// wrong in the strict direction — forty presses refused on a healthy editor —
// and a silent drop is what happens when it is wrong in the permissive one.
// The settled answer is that it depends on whether anything downstream will
// check the post: `--hold` says the caller reads Emacs's own marks back, so the
// reading is advisory; without it nobody checks, so a definite NO refuses.
//
// THE TARGET IS A REAL APPLICATION THAT CAN NEVER OWN A KEY WINDOW. A plain
// process is not addressable at all (`NSRunningApplication(processIdentifier:)`
// answers nil for one), and a real GUI application's readiness depends on
// whatever else is on the owner's desktop. An `NSApplication` that sets the
// accessory activation policy and opens no window is registered — so it is
// addressable — and owns no window for the window server to make key, so all
// three accessibility questions answer a definite NO every time. That makes the
// two branches of the policy deterministic on any machine.

// TestMain removes what the Swift builds below left on disk.
//
// It lives here because these are the only tests in the package that compile
// anything, and a binary that is built once for the whole run cannot be cleaned
// up by the one test that happened to build it first.
func TestMain(m *testing.M) {
	code := m.Run()
	for _, binary := range []string{keyDriverPath, windowlessPath} {
		if binary != "" {
			_ = os.RemoveAll(filepath.Dir(binary))
		}
	}
	os.Exit(code)
}

// keyDriverBinary compiles keydriver.swift once for the whole package.
//
// Once rather than per test because swiftc is the slowest thing here and the
// binary is the same for every one of them.
var (
	keyDriverBuild  sync.Once
	keyDriverPath   string
	keyDriverErr    error
	windowlessBuild sync.Once
	windowlessPath  string
	windowlessErr   error
)

// swiftBuild compiles one Swift source into a named binary under a directory
// that outlives a single test.
func swiftBuild(source, name, contents string) (string, error) {
	dir, err := os.MkdirTemp("", "realtest-keydriver-")
	if err != nil {
		return "", err
	}
	if contents != "" {
		source = filepath.Join(dir, name+".swift")
		if err := os.WriteFile(source, []byte(contents), 0o644); err != nil {
			return "", err
		}
	}
	absolute, err := filepath.Abs(source)
	if err != nil {
		return "", err
	}
	binary := filepath.Join(dir, name)
	ctx, cancel := context.WithTimeout(context.Background(), 2*time.Minute)
	defer cancel()
	out, err := exec.CommandContext(ctx, "swiftc", "-O", "-o", binary, absolute).CombinedOutput()
	if err != nil {
		return "", errors.New("swiftc " + absolute + ": " + err.Error() + "; it said:\n" + string(out))
	}
	return binary, nil
}

// buildKeyDriver returns the compiled helper, skipping the test when swiftc is
// not on this machine at all — which is a statement about the machine, not a
// failure of the policy under test.
func buildKeyDriver(t *testing.T) string {
	t.Helper()
	if _, err := exec.LookPath("swiftc"); err != nil {
		t.Skip("swiftc is not installed, so the key helper cannot be compiled here")
	}
	keyDriverBuild.Do(func() {
		keyDriverPath, keyDriverErr = swiftBuild(keyDriverSource, "keydriver", "")
	})
	if keyDriverErr != nil {
		t.Fatalf("compile the key helper: %v", keyDriverErr)
	}
	return keyDriverPath
}

// windowlessAppSource is an application that is registered with the window
// server and owns no window, ever.
const windowlessAppSource = `import AppKit
let app = NSApplication.shared
app.setActivationPolicy(.accessory)
print(ProcessInfo.processInfo.processIdentifier)
fflush(stdout)
app.run()
`

// startWindowlessApp starts that application and answers its pid.
//
// The pid is read off its stdout rather than taken from the child process
// handle so the test does not race the app's own registration: the line is
// printed after `setActivationPolicy`, so a pid that has arrived is a pid the
// window server already knows.
func startWindowlessApp(t *testing.T) int {
	t.Helper()
	windowlessBuild.Do(func() {
		windowlessPath, windowlessErr = swiftBuild("", "windowless", windowlessAppSource)
	})
	if windowlessErr != nil {
		t.Fatalf("compile the windowless target application: %v", windowlessErr)
	}

	command := exec.Command(windowlessPath)
	stdout, err := command.StdoutPipe()
	if err != nil {
		t.Fatalf("open the windowless application's stdout: %v", err)
	}
	if err := command.Start(); err != nil {
		t.Fatalf("start the windowless application: %v", err)
	}
	t.Cleanup(func() {
		_ = command.Process.Kill()
		_ = command.Wait()
	})

	announced := make(chan string, 1)
	go func() {
		scanner := bufio.NewScanner(stdout)
		if scanner.Scan() {
			announced <- strings.TrimSpace(scanner.Text())
			return
		}
		close(announced)
	}()

	select {
	case line, ok := <-announced:
		if !ok {
			t.Fatal("the windowless application exited without announcing its pid")
		}
		pid, err := strconv.Atoi(line)
		if err != nil {
			t.Fatalf("the windowless application announced %q, which is not a pid: %v", line, err)
		}
		return pid
	case <-time.After(30 * time.Second):
		t.Fatal("the windowless application never announced its pid")
		return 0
	}
}

// requireAccessibilityTrust skips when this process cannot ask the window
// server anything.
//
// Without trust every accessibility query answers "API disabled", which the
// helper reads as UNANSWERED — never as a NO — so neither branch of the policy
// is reachable and a run here would prove nothing either way.
func requireAccessibilityTrust(t *testing.T, helper string) {
	t.Helper()
	out, _ := exec.Command(helper, "--check").CombinedOutput()
	if strings.TrimSpace(string(out)) != "trusted" {
		t.Skip("this process does not hold accessibility trust, so the readiness questions cannot be answered here")
	}
}

// pressWindowlessTarget runs the helper against a target that can never own a
// key window and answers what it said.
func pressWindowlessTarget(t *testing.T, hold bool) (stdout, stderr string, err error) {
	t.Helper()
	helper := buildKeyDriver(t)
	requireAccessibilityTrust(t, helper)
	pid := startWindowlessApp(t)

	args := make([]string, 0, 3)
	if hold {
		args = append(args, "--hold=0.5")
	}
	// Keycode 53 is escape, which is the chord the show phase presses and the
	// most harmless key there is: it reaches nothing here in any case.
	args = append(args, strconv.Itoa(pid), "53")

	ctx, cancel := context.WithTimeout(context.Background(), 60*time.Second)
	defer cancel()
	command := exec.CommandContext(ctx, helper, args...)
	command.Stdin = strings.NewReader("release\n")
	var out, errs strings.Builder
	command.Stdout = &out
	command.Stderr = &errs
	err = command.Run()
	return out.String(), errs.String(), err
}

// A press nobody will check must not be posted into a target the window server
// says cannot receive it: that is the silent loss the whole helper exists to
// prevent, and the unheld path has no channel to the editor to catch it.
func TestKeyDriverRefusesAnUnheldPressToATargetWithNoKeyWindow(t *testing.T) {
	stdout, _, err := pressWindowlessTarget(t, false)

	if err == nil {
		t.Fatalf("the unheld press was accepted; it must be refused. The helper said: %q", stdout)
	}
	if strings.Contains(stdout, keyDeliveryPostedLine) {
		t.Errorf("the helper reported a post it refused to make: %q", stdout)
	}
}

// A refusal is evidence or it is nothing: it has to say which question answered
// no, so a finding can be checked rather than taken.
func TestKeyDriverRefusalNamesWhatAccessibilityAnswered(t *testing.T) {
	_, stderr, err := pressWindowlessTarget(t, false)

	if err == nil {
		t.Fatalf("the unheld press was accepted; it must be refused. The helper's stderr was: %q", stderr)
	}
	for _, want := range []string{"AXFrontmost is false", "frontmost=no", "focusedWindow=no", "isActive=no"} {
		if !strings.Contains(stderr, want) {
			t.Errorf("the refusal does not carry %q; it said: %q", want, stderr)
		}
	}
}

// A HELD press is confirmed against Emacs's own marks afterwards, so the
// readiness reading is advisory and must not refuse. This is the regression the
// 2026-09-12 16:12 sweep hit forty times.
func TestKeyDriverPostsAHeldPressToATargetWithNoKeyWindow(t *testing.T) {
	stdout, stderr, err := pressWindowlessTarget(t, true)

	if err != nil {
		t.Fatalf("the held press was refused: %v; the helper said %q / %q", err, stdout, stderr)
	}
	if !strings.Contains(stdout, keyDeliveryPostedLine) {
		t.Errorf("the held press never reported the post: %q", stdout)
	}
}

// The reading the post no longer refuses on still has to REACH the caller, and
// `isActive` — the answer that refused forty healthy presses — is part of it.
// Dropping it would lose the only trace of that failure mode.
func TestKeyDriverHeldReceiptCarriesTheReadinessReading(t *testing.T) {
	stdout, stderr, err := pressWindowlessTarget(t, true)

	if err != nil {
		t.Fatalf("the held press was refused: %v; the helper said %q / %q", err, stdout, stderr)
	}
	for _, want := range []string{"keydriver-receipt:", "readiness=", "ready=no", "isActive=no", "readyAfter="} {
		if !strings.Contains(stdout, want) {
			t.Errorf("the receipt does not carry %q; it said: %q", want, stdout)
		}
	}
}
