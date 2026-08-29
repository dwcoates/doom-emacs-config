//go:build integration

package integration

import (
	"net/http"
	"os"
	"path/filepath"
	"strings"
	"testing"

	"claude-repld/integration/harness"
)

func TestBootWritesAndRemovesTheAddressFile(t *testing.T) {
	// Arrange
	d := newDaemon(t, harness.Opts{})

	// Act: StartDaemon already waited for the file; assert its shape, then stop.
	raw, err := os.ReadFile(d.AddrFile())
	if err != nil {
		t.Fatalf("read daemon.addr = error %v, want the bound address", err)
	}

	// Assert
	if !strings.HasPrefix(string(raw), "127.0.0.1:") || !strings.HasSuffix(string(raw), "\n") {
		t.Fatalf("daemon.addr = %q, want \"127.0.0.1:<port>\\n\"", raw)
	}
	d.Stop()
	if _, err := os.Stat(d.AddrFile()); !os.IsNotExist(err) {
		t.Fatalf("daemon.addr after SIGTERM: stat err = %v, want it removed on an orderly exit", err)
	}
}

func TestSecondDaemonOnTheSameStateRootRefusesToBoot(t *testing.T) {
	// Arrange
	incumbent := newDaemon(t, harness.Opts{})
	before, err := os.ReadFile(incumbent.AddrFile())
	if err != nil {
		t.Fatalf("read the incumbent's daemon.addr: %v", err)
	}

	// Act
	second := harness.StartDaemon(t, harness.Opts{StateDir: incumbent.StateDir, ExpectEarlyExit: true})
	code := second.AwaitExit()

	// Assert
	if code == 0 {
		t.Fatalf("the second daemon exited 0, want a non-zero refusal\nstderr:\n%s", second.Stderr())
	}
	after, err := os.ReadFile(incumbent.AddrFile())
	if err != nil {
		t.Fatalf("read the incumbent's daemon.addr after the refusal: %v", err)
	}
	if string(after) != string(before) {
		t.Fatalf("daemon.addr = %q after a refused second daemon, want the incumbent's %q untouched", after, before)
	}
	if _, err := incumbent.Client().DaemonHealth(incumbent.Ctx(), healthRequest()); err != nil {
		t.Fatalf("the incumbent stopped serving after a refused second daemon: %v", err)
	}
}

func TestJoiningDaemonDoesNotClaimTheAddressFile(t *testing.T) {
	// Arrange
	incumbent := newDaemon(t, harness.Opts{})
	before, err := os.ReadFile(incumbent.AddrFile())
	if err != nil {
		t.Fatalf("read the incumbent's daemon.addr: %v", err)
	}

	// Act
	joining := harness.StartDaemon(t, harness.Opts{StateDir: incumbent.StateDir, Joining: incumbent.Addr})
	joining.ExpectFileUnchanged(incumbent.AddrFile(), string(before), harness.ProbeWindow)

	// Assert
	after, err := os.ReadFile(incumbent.AddrFile())
	if err != nil {
		t.Fatalf("read daemon.addr while a joining daemon owns no workspace: %v", err)
	}
	if string(after) != string(before) {
		t.Fatalf("daemon.addr = %q, want the incumbent's %q: a joining daemon writes it only once it owns every workspace", after, before)
	}
	if joining.Exited() {
		t.Fatalf("the joining daemon exited instead of binding its own port\nstderr:\n%s", joining.Stderr())
	}
}

func TestBootRefusesAnUnwritableStateRoot(t *testing.T) {
	// Arrange
	root := filepath.Join(t.TempDir(), "readonly")
	if err := os.MkdirAll(root, 0o500); err != nil {
		t.Fatalf("mkdir a read-only state root: %v", err)
	}
	t.Cleanup(func() { os.Chmod(root, 0o755) })

	// Act
	d := harness.StartDaemon(t, harness.Opts{StateDir: root, ExpectEarlyExit: true})
	code := d.AwaitExit()

	// Assert
	if code == 0 {
		t.Fatalf("boot on an unwritable state root exited 0, want a loud non-zero refusal")
	}
	if !strings.Contains(d.Stderr(), root) {
		t.Fatalf("boot stderr = %q, want it to name the misconfigured state root %q", d.Stderr(), root)
	}
}

func TestBootOpensTheWorkspaceStateFresh(t *testing.T) {
	// Arrange: a pre-existing legacy database the rebuild must abandon in place.
	root := t.TempDir()
	legacy := filepath.Join(root, "state.db")
	if err := os.WriteFile(legacy, []byte("legacy sqlite bytes"), 0o644); err != nil {
		t.Fatalf("seed state.db: %v", err)
	}

	// Act
	d := harness.StartDaemon(t, harness.Opts{StateDir: root})

	// Assert
	got, err := os.ReadFile(legacy)
	if err != nil {
		t.Fatalf("read state.db after boot: %v", err)
	}
	if string(got) != "legacy sqlite bytes" {
		t.Fatalf("state.db = %q after boot, want it left untouched", got)
	}
	if _, err := os.Stat(filepath.Join(d.StateDir, "wsm.db")); err != nil {
		t.Fatalf("stat wsm.db = %v, want the fresh database created at boot", err)
	}
}

func TestPprofServesOnAnExplicitLoopbackAddress(t *testing.T) {
	// Arrange
	d := newDaemon(t, harness.Opts{Pprof: "127.0.0.1:0"})
	record := d.AwaitRunLogOperation("daemon.pprof.enabled")
	addr, _ := record.Context["address"].(string)
	if addr == "" {
		t.Fatalf("daemon.pprof.enabled context = %v, want the resolved address", record.Context)
	}

	// Act
	resp, err := d.HTTP().Get("http://" + addr + "/debug/pprof/")

	// Assert
	if err != nil {
		t.Fatalf("GET /debug/pprof/ = error %v, want the profiling surface", err)
	}
	defer resp.Body.Close()
	if resp.StatusCode != http.StatusOK {
		t.Fatalf("GET /debug/pprof/ = %d, want 200", resp.StatusCode)
	}
	d.ExpectWarnings("daemon.pprof.enabled")
}

func TestPprofRefusesARoutableBind(t *testing.T) {
	// Arrange / Act
	d := harness.StartDaemon(t, harness.Opts{Pprof: "0.0.0.0:6060", ExpectEarlyExit: true})
	code := d.AwaitExit()

	// Assert
	if code == 0 {
		t.Fatalf("boot with -pprof 0.0.0.0:6060 exited 0, want a refusal at construction")
	}
	if !strings.Contains(d.Stderr(), "0.0.0.0") {
		t.Fatalf("boot stderr = %q, want it to name the refused wildcard bind", d.Stderr())
	}
}

func TestRunLogIsJSONLPerTheLoggingContract(t *testing.T) {
	// Arrange
	d := newDaemon(t, harness.Opts{})
	d.AwaitRunLogOperation("daemon.pprof.disabled")

	// Act
	records := d.RunLog()

	// Assert
	if len(records) == 0 {
		t.Fatal("the run log holds no records, want the boot sequence recorded")
	}
	for _, r := range records {
		if !harness.TimestampPattern.MatchString(r.Timestamp) {
			t.Fatalf("record %q timestamp = %q, want the contracted pattern", r.Operation, r.Timestamp)
		}
		if r.Runtime != "daemon" {
			t.Fatalf("record %q runtime = %q, want \"daemon\"", r.Operation, r.Runtime)
		}
		if r.PID != d.PID() {
			t.Fatalf("record %q pid = %d, want the daemon's pid %d", r.Operation, r.PID, d.PID())
		}
		if r.Operation == "" {
			t.Fatalf("record %q has no operation, want daemon.<package>.<verb>", r.Raw)
		}
		if !strings.HasPrefix(r.Operation, "daemon.") {
			t.Fatalf("record operation = %q, want the daemon.<package>.<verb> form", r.Operation)
		}
		if r.Context == nil {
			t.Fatalf("record %q has no context object, want structured context", r.Operation)
		}
	}
	d.ExpectWarnings(harness.AllowAllWarnings)
}
