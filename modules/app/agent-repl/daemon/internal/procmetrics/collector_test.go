package procmetrics

import (
	"errors"
	"strings"
	"sync"
	"testing"

	"agent-repld/internal/dlog"

	"github.com/prometheus/client_golang/prometheus"
	dto "github.com/prometheus/client_model/go"
)

// fakeReader is a scripted kernel: identities, argvs and usages, any of
// which a test can change between scrapes or make fail.
type fakeReader struct {
	mu       sync.Mutex
	ids      map[int]Identity
	argv     map[int][]string
	usage    map[int]Usage
	pidsErr  error
	usageErr map[int]error
	host     Host
	hostErr  error
}

func (f *fakeReader) PIDs() ([]int, error) {
	f.mu.Lock()
	defer f.mu.Unlock()
	if f.pidsErr != nil {
		return nil, f.pidsErr
	}
	out := make([]int, 0, len(f.ids))
	for pid := range f.ids {
		out = append(out, pid)
	}
	return out, nil
}

func (f *fakeReader) Identity(pid int) (Identity, error) {
	f.mu.Lock()
	defer f.mu.Unlock()
	id, ok := f.ids[pid]
	if !ok {
		return Identity{}, ErrGone
	}
	return id, nil
}

func (f *fakeReader) Args(pid int) ([]string, error) {
	f.mu.Lock()
	defer f.mu.Unlock()
	a, ok := f.argv[pid]
	if !ok {
		return nil, ErrGone
	}
	return a, nil
}

func (f *fakeReader) Usage(pid int) (Usage, error) {
	f.mu.Lock()
	defer f.mu.Unlock()
	if err := f.usageErr[pid]; err != nil {
		return Usage{}, err
	}
	u, ok := f.usage[pid]
	if !ok {
		return Usage{}, ErrGone
	}
	return u, nil
}

func (f *fakeReader) Host(string) (Host, error) {
	f.mu.Lock()
	defer f.mu.Unlock()
	return f.host, f.hostErr
}

func (f *fakeReader) setUsage(pid int, u Usage) {
	f.mu.Lock()
	defer f.mu.Unlock()
	f.usage[pid] = u
}

func (f *fakeReader) addProcess(id Identity, u Usage) {
	f.mu.Lock()
	defer f.mu.Unlock()
	f.ids[id.PID] = id
	f.usage[id.PID] = u
}

func (f *fakeReader) removeProcess(pid int) {
	f.mu.Lock()
	defer f.mu.Unlock()
	delete(f.ids, pid)
	delete(f.usage, pid)
}

// newWorld is a daemon with one shim (pid 30) running one tool (pid 40).
func newWorld() *fakeReader {
	return &fakeReader{
		ids: map[int]Identity{
			testEmacs: {PID: testEmacs, PPID: 1, Name: "Emacs", Responsible: testEmacs, StartUnixMicros: 1},
			testSelf:  {PID: testSelf, PPID: 1, Name: "agent-repld", Responsible: testEmacs, StartUnixMicros: 2},
			30:        {PID: 30, PPID: testSelf, Name: "node", Responsible: testEmacs, StartUnixMicros: 3},
			40:        {PID: 40, PPID: 30, Name: "claude", Responsible: testEmacs, StartUnixMicros: 4},
		},
		argv: map[int][]string{30: {"node", "main.js", "--listen", "/state/sock/ws1.sock"}},
		usage: map[int]Usage{
			testEmacs: {CPUSeconds: 1000, ResidentBytes: 500},
			testSelf:  {CPUSeconds: 10, ResidentBytes: 100, Threads: 20, OpenFDs: 30},
			30:        {CPUSeconds: 5, ResidentBytes: 50, DiskWriteBytes: 1000, EnergyJoules: 2},
			40:        {CPUSeconds: 7, ResidentBytes: 70},
		},
		usageErr: map[int]error{},
		host: Host{
			CPUSeconds:       map[string]float64{"user": 1, "idle": 9},
			MemoryBytes:      map[string]uint64{"wired": 4},
			MemoryTotalBytes: 16, MemoryPressure: 1, DiskFreeBytes: 3, DiskTotalBytes: 10,
		},
	}
}

type harness struct {
	reader  *fakeReader
	log     *dlog.TestLogger
	reg     *prometheus.Registry
	battery bool
	powErr  error
}

func newHarness(t *testing.T, reader *fakeReader) *harness {
	t.Helper()
	h := &harness{reader: reader, log: dlog.NewTestLogger(), reg: prometheus.NewRegistry()}
	c, err := New(Deps{
		Reader: reader, Self: testSelf, SockDir: testSock, DiskPath: "/state",
		OnBattery: func() (bool, error) { return h.battery, h.powErr },
		Log:       h.log,
	})
	if err != nil {
		t.Fatalf("New: %v", err)
	}
	h.reg.MustRegister(c)
	return h
}

// scrape gathers once and answers value lookups by name and label pairs.
func (h *harness) scrape(t *testing.T) func(name string, labels ...string) (float64, bool) {
	t.Helper()
	families, err := h.reg.Gather()
	if err != nil {
		t.Fatalf("Gather: %v", err)
	}
	return func(name string, labels ...string) (float64, bool) {
		for _, f := range families {
			if f.GetName() != name {
				continue
			}
			for _, m := range f.GetMetric() {
				if matches(m, labels) {
					switch {
					case m.GetCounter() != nil:
						return m.GetCounter().GetValue(), true
					default:
						return m.GetGauge().GetValue(), true
					}
				}
			}
		}
		return 0, false
	}
}

func matches(m *dto.Metric, pairs []string) bool {
	got := map[string]string{}
	for _, l := range m.GetLabel() {
		got[l.GetName()] = l.GetValue()
	}
	for i := 0; i+1 < len(pairs); i += 2 {
		if got[pairs[i]] != pairs[i+1] {
			return false
		}
	}
	return true
}

func mustValue(t *testing.T, get func(string, ...string) (float64, bool), want float64, name string, labels ...string) {
	t.Helper()
	got, ok := get(name, labels...)
	if !ok || got != want {
		t.Fatalf("%s%v = (%v, present %v), want %v", name, labels, got, ok, want)
	}
}

func shimTools() []string { return []string{"process", ProcessShimTools, "workspace", "ws1"} }
func shim() []string      { return []string{"process", ProcessShim, "workspace", "ws1"} }

func TestTheFirstScrapeTakesTheBaselineAndCountsNothing(t *testing.T) {
	// Arrange
	h := newHarness(t, newWorld())

	// Act
	get := h.scrape(t)

	// Assert
	mustValue(t, get, 0, "agentrepl_process_cpu_seconds_total", "process", ProcessEmacs)
	mustValue(t, get, 0, "agentrepl_process_cpu_seconds_total", shim()...)
	mustValue(t, get, 500, "agentrepl_process_resident_bytes", "process", ProcessEmacs)
	mustValue(t, get, 70, "agentrepl_process_resident_bytes", shimTools()...)
	mustValue(t, get, 1, "agentrepl_process_count", shimTools()...)
	mustValue(t, get, 20, "agentrepl_process_threads", "process", ProcessDaemon)
	mustValue(t, get, 30, "agentrepl_process_open_fds", "process", ProcessDaemon)
}

func TestALiveProcessAddsItsGrowth(t *testing.T) {
	// Arrange
	h := newHarness(t, newWorld())
	h.scrape(t)
	h.reader.setUsage(30, Usage{CPUSeconds: 8, DiskWriteBytes: 1500, EnergyJoules: 2.5})

	// Act
	get := h.scrape(t)

	// Assert
	mustValue(t, get, 3, "agentrepl_process_cpu_seconds_total", shim()...)
	mustValue(t, get, 500, "agentrepl_process_disk_written_bytes_total", shim()...)
	mustValue(t, get, 0.5, "agentrepl_process_energy_joules_total", shim()...)
}

func TestAProcessNewToItsGroupAddsAllItUsed(t *testing.T) {
	// Arrange
	h := newHarness(t, newWorld())
	h.scrape(t)
	h.reader.addProcess(Identity{PID: 41, PPID: 30, Name: "bash", StartUnixMicros: 9}, Usage{CPUSeconds: 4, ResidentBytes: 30})

	// Act
	get := h.scrape(t)

	// Assert
	mustValue(t, get, 4, "agentrepl_process_cpu_seconds_total", shimTools()...)
	mustValue(t, get, 100, "agentrepl_process_resident_bytes", shimTools()...)
	mustValue(t, get, 2, "agentrepl_process_count", shimTools()...)
}

func TestAReusedPidIsANewProcess(t *testing.T) {
	// Arrange
	h := newHarness(t, newWorld())
	h.scrape(t)
	h.reader.addProcess(Identity{PID: 40, PPID: 30, Name: "bash", StartUnixMicros: 99}, Usage{CPUSeconds: 2})

	// Act
	get := h.scrape(t)

	// Assert: the reused pid's 2s are all new, not 2-7 clamped to zero.
	mustValue(t, get, 2, "agentrepl_process_cpu_seconds_total", shimTools()...)
}

func TestAnExitedProcessKeepsItsGroupsCounterAndLeavesItsGauges(t *testing.T) {
	// Arrange
	h := newHarness(t, newWorld())
	h.scrape(t)
	h.reader.setUsage(40, Usage{CPUSeconds: 9, ResidentBytes: 70})
	h.scrape(t)
	h.reader.removeProcess(40)

	// Act
	get := h.scrape(t)

	// Assert
	mustValue(t, get, 2, "agentrepl_process_cpu_seconds_total", shimTools()...)
	mustValue(t, get, 0, "agentrepl_process_resident_bytes", shimTools()...)
	mustValue(t, get, 0, "agentrepl_process_count", shimTools()...)
}

func TestTheHostAndItsPowerAreServed(t *testing.T) {
	// Arrange
	h := newHarness(t, newWorld())
	h.battery = true

	// Act
	get := h.scrape(t)

	// Assert
	mustValue(t, get, 9, "agentrepl_host_cpu_seconds_total", "mode", "idle")
	mustValue(t, get, 4, "agentrepl_host_memory_bytes", "state", "wired")
	mustValue(t, get, 16, "agentrepl_host_memory_total_bytes")
	mustValue(t, get, 1, "agentrepl_host_memory_pressure")
	mustValue(t, get, 3, "agentrepl_host_disk_free_bytes")
	mustValue(t, get, 10, "agentrepl_host_disk_total_bytes")
	mustValue(t, get, 1, "agentrepl_host_on_battery")
}

func errorRecords(log *dlog.TestLogger) []dlog.Record {
	var out []dlog.Record
	for _, r := range log.Records() {
		if r.Level == "error" {
			out = append(out, r)
		}
	}
	return out
}

func TestAFailedListingOmitsTheProcessesOnceAndKeepsTheHost(t *testing.T) {
	// Arrange
	reader := newWorld()
	reader.pidsErr = errors.New("proc_listallpids: operation not permitted")
	h := newHarness(t, reader)

	// Act
	h.scrape(t)
	get := h.scrape(t)

	// Assert
	if _, ok := get("agentrepl_process_cpu_seconds_total", "process", ProcessDaemon); ok {
		t.Fatalf("a failed listing still served process series")
	}
	mustValue(t, get, 16, "agentrepl_host_memory_total_bytes")
	errs := errorRecords(h.log)
	if len(errs) != 1 || errs[0].Operation != Operation || errs[0].Context["step"] != "list" ||
		!strings.Contains(errs[0].Context["error"].(string), "operation not permitted") {
		t.Fatalf("error records = %+v, want one list failure across two scrapes", errs)
	}
}

func TestARecoveredListingIsRecorded(t *testing.T) {
	// Arrange
	reader := newWorld()
	reader.pidsErr = errors.New("proc_listallpids: operation not permitted")
	h := newHarness(t, reader)
	h.scrape(t)
	reader.mu.Lock()
	reader.pidsErr = nil
	reader.mu.Unlock()

	// Act
	get := h.scrape(t)

	// Assert
	mustValue(t, get, 1, "agentrepl_process_count", "process", ProcessDaemon)
	var recovered bool
	for _, r := range h.log.Records() {
		if r.Level == "info" && r.Context["step"] == "list" {
			recovered = true
		}
	}
	if !recovered {
		t.Fatalf("no recovery record in %+v", h.log.Records())
	}
}

func TestAnUnknownWebKitRoleIsRecordedOnceAndItsRecoveryToo(t *testing.T) {
	// Arrange
	reader := newWorld()
	reader.ids[20] = Identity{PID: 20, PPID: 1, Name: "com.apple.WebKit.Model", Responsible: testEmacs, StartUnixMicros: 5}
	reader.usage[20] = Usage{CPUSeconds: 1}
	h := newHarness(t, reader)

	// Act
	h.scrape(t)
	get := h.scrape(t)
	reader.mu.Lock()
	delete(reader.ids, 20)
	reader.mu.Unlock()
	h.scrape(t)

	// Assert
	if _, ok := get("agentrepl_process_count", "process", "webkit_model"); ok {
		t.Fatalf("an unknown WebKit role became a group")
	}
	errs := errorRecords(h.log)
	if len(errs) != 1 || errs[0].Context["step"] != "webkit_role" {
		t.Fatalf("error records = %+v, want one webkit_role record across two scrapes", errs)
	}
	var recovered bool
	for _, r := range h.log.Records() {
		if r.Level == "info" && r.Context["step"] == "webkit_role" {
			recovered = true
		}
	}
	if !recovered {
		t.Fatalf("no recovery record in %+v", h.log.Records())
	}
}

func TestAFailedUsageReadIsRecordedAndAGoneProcessIsNot(t *testing.T) {
	// Arrange
	reader := newWorld()
	reader.usageErr[40] = errors.New("proc_pid_rusage: operation not permitted")
	reader.usageErr[30] = ErrGone
	h := newHarness(t, reader)

	// Act
	get := h.scrape(t)

	// Assert
	mustValue(t, get, 1, "agentrepl_process_count", "process", ProcessDaemon)
	errs := errorRecords(h.log)
	if len(errs) != 1 || errs[0].Context["step"] != "usage" {
		t.Fatalf("error records = %+v, want only the usage failure", errs)
	}
}

func TestAFailedHostOrPowerReadOmitsOnlyItsOwnSeries(t *testing.T) {
	// Arrange
	reader := newWorld()
	reader.hostErr = errors.New("host_statistics answered 5")
	h := newHarness(t, reader)
	h.powErr = errors.New("IOPSCopyPowerSourcesInfo answered nothing")

	// Act
	get := h.scrape(t)

	// Assert
	if _, ok := get("agentrepl_host_memory_total_bytes"); ok {
		t.Fatalf("a failed host read still served host series")
	}
	if _, ok := get("agentrepl_host_on_battery"); ok {
		t.Fatalf("a failed power read still served the battery series")
	}
	mustValue(t, get, 1, "agentrepl_process_count", "process", ProcessDaemon)
	steps := map[any]bool{}
	for _, r := range errorRecords(h.log) {
		steps[r.Context["step"]] = true
	}
	if !steps["host"] || !steps["power"] || len(steps) != 2 {
		t.Fatalf("failed steps = %v, want host and power", steps)
	}
}

func TestNewRefusesMissingDeps(t *testing.T) {
	// Act
	_, err := New(Deps{Reader: newWorld(), Self: testSelf})

	// Assert
	if err == nil || !strings.Contains(err.Error(), "are required") {
		t.Fatalf("New = %v, want the missing deps refused", err)
	}
}
