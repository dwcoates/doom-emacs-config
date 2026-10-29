package procmetrics

import (
	"errors"
	"sort"
	"strings"
	"sync"

	"agent-repld/internal/causelog"
	"agent-repld/internal/dlog"

	"github.com/prometheus/client_golang/prometheus"
)

// Operation is the canonical log operation of every record this package
// writes.
const Operation = "daemon.metrics.process"

// Deps are the collector's collaborators, every one required.
type Deps struct {
	Reader Reader
	// Self is the daemon's pid.
	Self int
	// Emacs is the Emacs whose WatchDaemon stream stands, measured with the
	// WebKit processes it is responsible for.
	Emacs interface{ PID() (int, bool) }
	// SockDir is the directory the daemon's shims listen in.
	SockDir string
	// DiskPath is a path on the volume whose free space is reported (the
	// state root).
	DiskPath string
	// OnBattery answers whether the host runs on battery
	// (cpugov.Host.Power).
	OnBattery func() (bool, error)
	Log       dlog.Logger
}

// pidState is what the last scrape read for one process.
type pidState struct {
	group Group
	start int64
	usage Usage
}

// totals are a group's monotonic counters.
type totals struct {
	cpu, read, write, energy float64
}

// Collector is the process and host collector.
//
// COUNTERS STAY MONOTONIC ACROSS PROCESS CHURN. A group's CPU, disk and
// energy are not the sum of its live processes' lifetimes (a process exiting
// would make that sum fall, which a rate reads as a reset and a spike).
// Each scrape adds every live process's growth since the last scrape; a
// process new to the group adds all it has used, except at the first scrape,
// which only takes the baseline, so a daemon restart starts every counter
// from zero rather than from the lifetimes of processes that predate it. A
// reused pid is told from its predecessor by its start time.
type Collector struct {
	deps   Deps
	causes causelog.Tracker

	mu     sync.Mutex
	primed bool
	seen   map[int]pidState
	totals map[Group]*totals
	// latest is the last scrape's per-group footprints, for the rail's memory
	// popups (memorypopup), which read it between scrapes.
	latest Snapshot
	// sampled is whether any scrape has run.
	sampled bool
	// hostBase is each CPU mode's host total at the first host read, so the
	// host's CPU counts from the daemon's start like every other counter
	// here: a counter since the host's boot read as all of the host's
	// history the first time a new run's series was queried.
	hostBase map[string]float64

	cpu, read, write, energy             *prometheus.Desc
	resident, footprint, threads, fds    *prometheus.Desc
	count                                *prometheus.Desc
	hostCPU, hostMemory, hostMemoryTotal *prometheus.Desc
	hostPressure, hostDiskFree           *prometheus.Desc
	hostDiskTotal, hostOnBattery         *prometheus.Desc
}

// New builds a collector.
func New(deps Deps) (*Collector, error) {
	if deps.Reader == nil || deps.Self <= 0 || deps.Emacs == nil || deps.SockDir == "" || deps.DiskPath == "" || deps.OnBattery == nil || deps.Log == nil {
		return nil, errors.New("procmetrics: a reader, the daemon's pid, the Emacs holder, the socket directory, a disk path, a power source and a logger are required")
	}
	labels := []string{"process", "workspace"}
	desc := func(name, help string, l []string) *prometheus.Desc { return prometheus.NewDesc(name, help, l, nil) }
	return &Collector{
		deps:            deps,
		seen:            map[int]pidState{},
		totals:          map[Group]*totals{},
		hostBase:        map[string]float64{},
		cpu:             desc("agentrepl_process_cpu_seconds_total", "CPU seconds (user and system) a group of agent-repl processes used since the daemon started.", labels),
		read:            desc("agentrepl_process_disk_read_bytes_total", "Bytes a group read from disk since the daemon started.", labels),
		write:           desc("agentrepl_process_disk_written_bytes_total", "Bytes a group wrote to disk since the daemon started.", labels),
		energy:          desc("agentrepl_process_energy_joules_total", "Energy macOS attributes to a group since the daemon started.", labels),
		resident:        desc("agentrepl_process_resident_bytes", "Resident memory of a group's live processes.", labels),
		footprint:       desc("agentrepl_process_footprint_bytes", "Physical footprint (compressed and swapped included) of a group's live processes.", labels),
		threads:         desc("agentrepl_process_threads", "Threads in a group's live processes.", labels),
		fds:             desc("agentrepl_process_open_fds", "Open file descriptors in a group's live processes.", labels),
		count:           desc("agentrepl_process_count", "Live processes in a group.", labels),
		hostCPU:         desc("agentrepl_host_cpu_seconds_total", "CPU seconds every processor spent in each mode since the daemon started.", []string{"mode"}),
		hostMemory:      desc("agentrepl_host_memory_bytes", "Physical memory by state.", []string{"state"}),
		hostMemoryTotal: desc("agentrepl_host_memory_total_bytes", "All physical memory.", nil),
		hostPressure:    desc("agentrepl_host_memory_pressure", "The kernel's memory pressure level: 1 normal, 2 warning, 4 critical.", nil),
		hostDiskFree:    desc("agentrepl_host_disk_free_bytes", "Free bytes on the volume holding the state root.", nil),
		hostDiskTotal:   desc("agentrepl_host_disk_total_bytes", "Size of the volume holding the state root.", nil),
		hostOnBattery:   desc("agentrepl_host_on_battery", "1 while the host runs on battery, 0 on AC.", nil),
	}, nil
}

// Describe implements prometheus.Collector.
func (c *Collector) Describe(ch chan<- *prometheus.Desc) {
	for _, d := range []*prometheus.Desc{
		c.cpu, c.read, c.write, c.energy, c.resident, c.footprint, c.threads, c.fds, c.count,
		c.hostCPU, c.hostMemory, c.hostMemoryTotal, c.hostPressure, c.hostDiskFree, c.hostDiskTotal, c.hostOnBattery,
	} {
		ch <- d
	}
}

// Collect implements prometheus.Collector. Every read that fails for a
// reason other than a process exiting is recorded once per cause and omits
// only what it would have measured.
func (c *Collector) Collect(ch chan<- prometheus.Metric) {
	c.collectProcesses(ch)
	c.collectHost(ch)
	c.collectPower(ch)
}

// gauges are a group's point-in-time sums.
type gauges struct {
	resident, footprint uint64
	threads, fds, count int
}

// goneSince answers whether the process listed as WAS has gone since: a
// failed usage read of a process that exited meanwhile is the race of a
// process exiting, not a failure. The kernel need not answer ESRCH for it (a
// process mid-exit answered EPERM, 2026-10-22), so the process is asked for
// again, and gone, or replaced by another process under its pid, is gone.
func (c *Collector) goneSince(pid int, was Identity) bool {
	now, err := c.deps.Reader.Identity(pid)
	if errors.Is(err, ErrGone) {
		return true
	}
	return err == nil && now.StartUnixMicros != was.StartUnixMicros
}

// Snapshot is one scrape's physical footprint per group: what the rail's
// memory popups draw.
type Snapshot struct {
	// Footprints holds every group a live process was in, summed.
	Footprints map[Group]uint64
	// Failed is set when the scrape could not list the host's processes, so
	// it measured nothing; Footprints is then empty.
	Failed error
}

// Latest answers the last scrape's snapshot, false before the first scrape.
// The figures are as fresh as the metrics store's scrape cadence.
func (c *Collector) Latest() (Snapshot, bool) {
	c.mu.Lock()
	defer c.mu.Unlock()
	return c.latest, c.sampled
}

func (c *Collector) collectProcesses(ch chan<- prometheus.Metric) {
	pids, err := c.deps.Reader.PIDs()
	if !c.note("list", err) {
		c.mu.Lock()
		c.latest, c.sampled = Snapshot{Failed: err}, true
		c.mu.Unlock()
		return
	}
	ids := make(map[int]Identity, len(pids))
	var identityErr error
	for _, pid := range pids {
		id, err := c.deps.Reader.Identity(pid)
		switch {
		case err == nil:
			ids[pid] = id
		case !errors.Is(err, ErrGone):
			identityErr = err
		}
	}
	c.note("identity", identityErr)
	emacs, _ := c.deps.Emacs.PID()
	classified := classify(ids, c.deps.Self, emacs, c.deps.SockDir, c.deps.Reader.Args)
	c.note("argv", errors.Join(classified.errs...))
	c.noteUnknownWebKit(classified.unknownWebKit)

	live := map[int]pidState{}
	var usageErr error
	for pid, group := range classified.groups {
		u, err := c.deps.Reader.Usage(pid)
		switch {
		case err == nil:
			live[pid] = pidState{group: group, start: ids[pid].StartUnixMicros, usage: u}
		case !errors.Is(err, ErrGone) && !c.goneSince(pid, ids[pid]):
			usageErr = err
		}
	}
	c.note("usage", usageErr)

	c.mu.Lock()
	sums := c.account(live)
	footprints := make(map[Group]uint64, len(sums))
	for g, s := range sums {
		footprints[g] = s.footprint
	}
	c.latest, c.sampled = Snapshot{Footprints: footprints}, true
	snapshot := make(map[Group]totals, len(c.totals))
	for g, t := range c.totals {
		snapshot[g] = *t
	}
	c.mu.Unlock()

	groups := make([]Group, 0, len(snapshot))
	for g := range snapshot {
		groups = append(groups, g)
	}
	sort.Slice(groups, func(i, j int) bool {
		return groups[i].Process+"/"+groups[i].Workspace < groups[j].Process+"/"+groups[j].Workspace
	})
	for _, g := range groups {
		t, s := snapshot[g], sums[g]
		lv := []string{g.Process, g.Workspace}
		ch <- prometheus.MustNewConstMetric(c.cpu, prometheus.CounterValue, t.cpu, lv...)
		ch <- prometheus.MustNewConstMetric(c.read, prometheus.CounterValue, t.read, lv...)
		ch <- prometheus.MustNewConstMetric(c.write, prometheus.CounterValue, t.write, lv...)
		ch <- prometheus.MustNewConstMetric(c.energy, prometheus.CounterValue, t.energy, lv...)
		ch <- prometheus.MustNewConstMetric(c.resident, prometheus.GaugeValue, float64(s.resident), lv...)
		ch <- prometheus.MustNewConstMetric(c.footprint, prometheus.GaugeValue, float64(s.footprint), lv...)
		ch <- prometheus.MustNewConstMetric(c.threads, prometheus.GaugeValue, float64(s.threads), lv...)
		ch <- prometheus.MustNewConstMetric(c.fds, prometheus.GaugeValue, float64(s.fds), lv...)
		ch <- prometheus.MustNewConstMetric(c.count, prometheus.GaugeValue, float64(s.count), lv...)
	}
}

// account folds one scrape's live processes into the counters and answers
// each group's gauges. A group keeps its counters after its last process
// leaves (its gauges read zero), so a workspace's shim restarting continues
// one series. Called with c.mu held.
func (c *Collector) account(live map[int]pidState) map[Group]gauges {
	sums := map[Group]gauges{}
	for pid, now := range live {
		t := c.totals[now.group]
		if t == nil {
			t = &totals{}
			c.totals[now.group] = t
		}
		prev, known := c.seen[pid]
		switch {
		case !c.primed:
			// The baseline: nothing counted.
		case known && prev.start == now.start && prev.group == now.group:
			t.cpu += nonNegative(now.usage.CPUSeconds - prev.usage.CPUSeconds)
			t.read += nonNegative(float64(now.usage.DiskReadBytes) - float64(prev.usage.DiskReadBytes))
			t.write += nonNegative(float64(now.usage.DiskWriteBytes) - float64(prev.usage.DiskWriteBytes))
			t.energy += nonNegative(now.usage.EnergyJoules - prev.usage.EnergyJoules)
		default:
			t.cpu += now.usage.CPUSeconds
			t.read += float64(now.usage.DiskReadBytes)
			t.write += float64(now.usage.DiskWriteBytes)
			t.energy += now.usage.EnergyJoules
		}
		s := sums[now.group]
		s.resident += now.usage.ResidentBytes
		s.footprint += now.usage.FootprintBytes
		s.threads += now.usage.Threads
		s.fds += now.usage.OpenFDs
		s.count++
		sums[now.group] = s
	}
	c.seen = live
	c.primed = true
	return sums
}

func nonNegative(v float64) float64 {
	if v < 0 {
		return 0
	}
	return v
}

func (c *Collector) collectHost(ch chan<- prometheus.Metric) {
	h, err := c.deps.Reader.Host(c.deps.DiskPath)
	if !c.note("host", err) {
		return
	}
	for _, mode := range sortedKeys(h.CPUSeconds) {
		ch <- prometheus.MustNewConstMetric(c.hostCPU, prometheus.CounterValue, c.hostCPUSinceStart(mode, h.CPUSeconds[mode]), mode)
	}
	for _, state := range sortedKeys(h.MemoryBytes) {
		ch <- prometheus.MustNewConstMetric(c.hostMemory, prometheus.GaugeValue, float64(h.MemoryBytes[state]), state)
	}
	ch <- prometheus.MustNewConstMetric(c.hostMemoryTotal, prometheus.GaugeValue, float64(h.MemoryTotalBytes))
	ch <- prometheus.MustNewConstMetric(c.hostPressure, prometheus.GaugeValue, float64(h.MemoryPressure))
	ch <- prometheus.MustNewConstMetric(c.hostDiskFree, prometheus.GaugeValue, float64(h.DiskFreeBytes))
	ch <- prometheus.MustNewConstMetric(c.hostDiskTotal, prometheus.GaugeValue, float64(h.DiskTotalBytes))
}

// hostCPUSinceStart answers mode's host CPU seconds since the first host read
// that carried the mode.
func (c *Collector) hostCPUSinceStart(mode string, total float64) float64 {
	c.mu.Lock()
	defer c.mu.Unlock()
	base, ok := c.hostBase[mode]
	if !ok {
		c.hostBase[mode] = total
		return 0
	}
	return nonNegative(total - base)
}

func (c *Collector) collectPower(ch chan<- prometheus.Metric) {
	battery, err := c.deps.OnBattery()
	if !c.note("power", err) {
		return
	}
	v := 0.0
	if battery {
		v = 1
	}
	ch <- prometheus.MustNewConstMetric(c.hostOnBattery, prometheus.GaugeValue, v)
}

// note records a step's failure once per cause and its recovery once,
// answering whether the step succeeded.
func (c *Collector) note(step string, err error) bool {
	return c.causes.Note(step, err, func(cause string) {
		c.deps.Log.Error(Operation, "a process metrics read failed; what it measures is omitted until it succeeds", dlog.Context{"step": step, "error": cause})
	}, func() {
		c.deps.Log.Info(Operation, "a process metrics read succeeds again", dlog.Context{"step": step})
	})
}

// noteUnknownWebKit records, once per set of names, Emacs's WebKit processes
// whose role no WebKit group names: they are measured nowhere until the
// closed set (webkitRoles) names their role.
func (c *Collector) noteUnknownWebKit(names []string) {
	const step = "webkit_role"
	if len(names) == 0 {
		if c.causes.Succeeded(step) {
			c.deps.Log.Info(Operation, "every WebKit process of Emacs's is in a WebKit group again", dlog.Context{"step": step})
		}
		return
	}
	sorted := append([]string(nil), names...)
	sort.Strings(sorted)
	if c.causes.Failed(step, strings.Join(sorted, ",")) {
		c.deps.Log.Error(Operation, "a WebKit process of Emacs's has a role no WebKit group names; it is measured in no group", dlog.Context{
			"step": step, "names": sorted,
		})
	}
}

func sortedKeys[V any](m map[string]V) []string {
	keys := make([]string, 0, len(m))
	for k := range m {
		keys = append(keys, k)
	}
	sort.Strings(keys)
	return keys
}
