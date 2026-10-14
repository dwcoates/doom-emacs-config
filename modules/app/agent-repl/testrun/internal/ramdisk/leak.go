package ramdisk

import (
	"errors"
	"fmt"
	"os"
	"path/filepath"
	"regexp"
	"strconv"
	"strings"
	"sync"
	"time"
)

// Leak is one attached RAM disk this runner made that no live run's record
// names.
type Leak struct {
	// Device is the image's whole disk.
	Device string
	// Mounts are where its volumes are mounted, if anywhere.
	Mounts []string
	// Pid is the run that made it, from the record naming it or from its
	// artr-<pid> mount; 0 when neither says.
	Pid int
	// PidAlive is whether a process of Pid still exists (a run stuck exiting
	// in the kernel does, 2026-10-12).
	PidAlive bool
	// Record is the dead run's record that names it; empty when none does.
	Record string
}

func (l Leak) String() string {
	owner := "owning pid unknown"
	if l.Pid != 0 {
		alive := "not alive"
		if l.PidAlive {
			alive = "still alive"
		}
		owner = fmt.Sprintf("owning run pid %d, %s", l.Pid, alive)
	}
	mounts := "not mounted"
	if len(l.Mounts) > 0 {
		mounts = "mounted at " + strings.Join(l.Mounts, ", ")
	}
	record := "named by no record"
	if l.Record != "" {
		record = "named by the dead run's record " + l.Record
	}
	return fmt.Sprintf("%s (%s, %s, %s)", l.Device, owner, mounts, record)
}

// LeakError is Acquire's refusal to attach while leaked RAM disks stand.
type LeakError struct {
	Leaks []Leak
	// Info is the `hdiutil info` the leaks were read from.
	Info string
}

func (e *LeakError) Error() string {
	names := make([]string, len(e.Leaks))
	for i, l := range e.Leaks {
		names[i] = l.String()
	}
	return fmt.Sprintf("%d leaked RAM disk(s) stand, so no new one is attached: %s", len(e.Leaks), strings.Join(names, "; "))
}

// runMount is the mount point a run of a pid is given, wherever it is.
var runMount = regexp.MustCompile(`/artr-([0-9]+)$`)

// recordInfo is one record in the registry, as the leak check reads it.
type recordInfo struct {
	path   string
	pid    int
	device string
	live   bool
}

// readRecordsLocked reads every record in the registry and whether its run
// holds it. The caller holds the registry lock.
func (m Manager) readRecordsLocked() ([]recordInfo, error) {
	paths, err := filepath.Glob(filepath.Join(m.Registry, "*.lock"))
	if err != nil {
		return nil, fmt.Errorf("list the RAM disk records: %w", err)
	}
	var records []recordInfo
	for _, path := range paths {
		pid, err := strconv.Atoi(strings.TrimSuffix(filepath.Base(path), ".lock"))
		if err != nil {
			return nil, fmt.Errorf("the RAM disk record %s is not named by a pid", path)
		}
		f, dead, err := probeRecordLocked(path)
		if err != nil {
			return nil, err
		}
		if f != nil {
			if err := f.Close(); err != nil {
				return nil, fmt.Errorf("close the probed RAM disk record %s: %w", path, err)
			}
		}
		raw, err := os.ReadFile(path)
		if errors.Is(err, os.ErrNotExist) {
			continue
		}
		if err != nil {
			return nil, fmt.Errorf("read the RAM disk record %s: %w", path, err)
		}
		device := parseRecord(string(raw)).Device
		records = append(records, recordInfo{path: path, pid: pid, device: device, live: !dead})
	}
	return records, nil
}

// leaksLocked answers every RAM disk in info that this runner made and no live
// run's record names. The caller holds the registry lock, so no live run is
// between its attach and its record.
//
// A RAM DISK IS THE RUNNER'S when a record names one of its devices, or when
// one of its volumes is mounted at an artr-<pid> mount point (the volume name
// and the mount point a run gives its disk). A RAM disk neither marks is not
// the runner's to judge, and is left out.
func (m Manager) leaksLocked(info string) ([]Leak, error) {
	records, err := m.readRecordsLocked()
	if err != nil {
		return nil, err
	}
	var leaks []Leak
	for _, img := range parseImages(info) {
		if !img.isRAM() || len(img.Devices) == 0 {
			continue
		}
		var named *recordInfo
		for i := range records {
			for _, device := range img.Devices {
				if records[i].device == device {
					named = &records[i]
				}
			}
		}
		if named != nil && named.live {
			continue
		}
		leak := Leak{Device: img.Devices[0], Mounts: img.Mounts}
		switch {
		case named != nil:
			leak.Pid, leak.Record = named.pid, named.path
		default:
			for _, mount := range img.Mounts {
				if match := runMount.FindStringSubmatch(mount); match != nil {
					leak.Pid, _ = strconv.Atoi(match[1])
				}
			}
		}
		if named == nil && leak.Pid == 0 {
			continue
		}
		leak.PidAlive = m.Alive(leak.Pid)
		leaks = append(leaks, leak)
	}
	return leaks, nil
}

// Trouble is why a run must not attach a RAM disk: leaked RAM disks, a disk
// manager that did not answer, or a registry lock whose holder is stuck.
type Trouble struct {
	// Lock is the registry lock that was not taken within its bound; nil when
	// it was, or was never waited for.
	Lock *LockTimeoutError
	// Leaks are the leaked RAM disks; empty when none were found (or the disk
	// manager did not answer the listing).
	Leaks []Leak
	// Info is `hdiutil info`'s answer; InfoErr its failure or timeout.
	Info     string
	InfoErr  error
	InfoTook time.Duration
	// List is `diskutil list`'s answer; ListErr its failure or timeout.
	List     string
	ListErr  error
	ListTook time.Duration
	// At is when the probes began, on the wall clock, so the check lines up
	// against every other run's acquire and release edges.
	At time.Time
	// Host is what the host was doing when a probe did not answer
	// (hostSnapshot); empty when both answered.
	Host string

	// late are the probes that missed their bounds, still watched.
	late []*lateProbe
}

// lateProbe is one probe that missed its bound, watched to its own outcome.
// Its outcome is logged exactly once: when it arrives, or, when the run ends
// first, as still unanswered (SettleProbes).
type lateProbe struct {
	call  string
	asked time.Time
	once  sync.Once
}

// watchProbe is the probe of call within bound, asked at asked, as timeProbe
// runs it. A probe that misses its bound is recorded in t and watched to its
// own outcome, which goes to the run's log.
func (m Manager) watchProbe(t *Trouble, asked time.Time, bound time.Duration, name string, args ...string) func() (string, error) {
	return func() (string, error) {
		out, late, err := m.Watch(bound, name, args...)
		if late != nil {
			p := &lateProbe{call: name + " " + strings.Join(args, " "), asked: asked}
			t.late = append(t.late, p)
			go func() {
				outcome := <-late
				p.once.Do(func() { m.logLate(t, p, outcome) })
			}()
		}
		return out, err
	}
}

// logLate records a late probe's own outcome. An answer says the disk manager
// was slow, not wedged; a failure at CommandBound says it was wedged.
func (m Manager) logLate(t *Trouble, p *lateProbe, outcome Late) {
	if outcome.Err != nil {
		m.Log.Errorf("RAM disk check at %s: the disk manager never answered %s, which failed after %s: %v", stamp(t.At), p.call, outcome.Took, outcome.Err)
		return
	}
	m.Log.Infof("RAM disk check at %s: the disk manager answered %s late, after %s, so it was slow, not wedged", stamp(t.At), p.call, outcome.Took)
}

// SettleProbes ends the watch of t's late probes when the run ends: a probe
// whose own outcome has not arrived is recorded as still unanswered, and its
// outcome, should it arrive later, is not.
func (m Manager) SettleProbes(t *Trouble) {
	for _, p := range t.late {
		p.once.Do(func() {
			m.Log.Infof("RAM disk check at %s: the disk manager had still not answered %s when the run ended, %s after it was asked", stamp(t.At), p.call, m.Now().Sub(p.asked))
		})
	}
}

// Answered says whether the disk manager answered both probes.
func (t *Trouble) Answered() bool { return t.InfoErr == nil && t.ListErr == nil }

// String is the trouble in one line, for the run's ERROR record.
func (t *Trouble) String() string {
	var parts []string
	if t.Lock != nil {
		parts = append(parts, t.Lock.Error())
	}
	if len(t.Leaks) > 0 {
		names := make([]string, len(t.Leaks))
		for i, l := range t.Leaks {
			names[i] = l.String()
		}
		parts = append(parts, fmt.Sprintf("%d leaked RAM disk(s): %s", len(t.Leaks), strings.Join(names, "; ")))
	}
	parts = append(parts, probeLine("hdiutil info", t.InfoErr, t.InfoTook), probeLine("diskutil list", t.ListErr, t.ListTook))
	if t.Host != "" {
		parts = append(parts, "the host then: "+hostLine(t.Host))
	}
	return strings.Join(parts, "; ")
}

func probeLine(name string, err error, took time.Duration) string {
	if err != nil {
		return fmt.Sprintf("the disk manager did not answer %s after %s: %v", name, took, err)
	}
	return fmt.Sprintf("the disk manager answered %s in %s", name, took)
}

// Setup is a run's whole RAM disk start. It probes the disk manager, reclaims
// the dead runs' disks, and acquires this run's. It answers the volume, or a
// Trouble when a leak, a disk manager that does not answer or a registry lock
// whose holder is stuck forbids any attach, or the error of an acquire that failed otherwise. With either of the
// latter the run's root goes on the disk.
func (m Manager) Setup(sizeMiB int) (*Volume, *Trouble, error) {
	t := &Trouble{At: m.Now()}
	t.List, t.ListTook, t.ListErr = m.timeProbe(t.At, m.watchProbe(t, t.At, ListBound, "diskutil", "list"))
	infoAsked := m.Now()
	t.Info, t.InfoTook, t.InfoErr = m.timeProbe(infoAsked, m.watchProbe(t, infoAsked, InfoBound, "hdiutil", "info"))
	m.Log.Infof("RAM disk check at %s: %s; %s", stamp(t.At), probeLine("diskutil list", t.ListErr, t.ListTook), probeLine("hdiutil info", t.InfoErr, t.InfoTook))
	if !t.Answered() {
		// WHAT THE HOST WAS DOING IS READ AT ONCE, so the next probe that does
		// not answer says whether the host was starved (its load) or the disk
		// manager was busy with another run's disk (its processes).
		t.Host = m.hostSnapshot()
		m.Log.Infof("RAM disk check: the host when the disk manager did not answer: %s", hostLine(t.Host))
		// A DISK MANAGER THAT DOES NOT ANSWER IS NOT ASKED AGAIN: a reclaim's
		// or an acquire's every command would run out its own bound in turn.
		return nil, t, nil
	}
	reclaimed, err := m.Reclaim()
	for _, r := range reclaimed {
		m.logReclaimed(r)
	}
	// A STUCK REGISTRY LOCK IS NOT WAITED ON TWICE: the acquire would wait
	// out the same bound on the same holder.
	if errors.As(err, &t.Lock) {
		return nil, t, nil
	}
	if err != nil {
		m.Log.Errorf("reclaiming a dead run's RAM disk failed: %v", err)
	}
	v, err := m.Acquire(sizeMiB)
	var leaked *LeakError
	if errors.As(err, &leaked) {
		t.Leaks, t.Info = leaked.Leaks, leaked.Info
		return nil, t, nil
	}
	if errors.As(err, &t.Lock) {
		return nil, t, nil
	}
	return v, nil, err
}

// timeProbe runs probe and answers its answer with how long it took since
// start, by the Manager's clock.
func (m Manager) timeProbe(start time.Time, probe func() (string, error)) (string, time.Duration, error) {
	out, err := probe()
	return out, m.Now().Sub(start), err
}

// HostBound is how long each read of the host's state (`sysctl`, `ps`) may
// run when a probe did not answer: measured at the runs' own priority on the
// owner's host (2026-10-13, load average 4.5, 60 calls each) at max 2ms and
// 34ms. It is read exactly when the host is loaded, where the probes' own
// answers ran up to 6.7x their quiet max (56ms against 8.4ms), so the bound
// is just over twice 34ms scaled by that: 500ms.
const HostBound = 500 * time.Millisecond

// diskProcess matches a `ps` line of a process that serves or asks the disk
// manager: another run's hdiutil or diskutil, or the daemons behind them.
var diskProcess = regexp.MustCompile(`hdiutil|diskutil|diskimages|diskarbitration`)

// hostSnapshot is what the host was doing when a probe did not answer, one
// fact per line: its load average, and every disk process then running, as
// `sysctl` and `ps` list them within HostBound. Either read's failure is named
// in its place, never dropped.
func (m Manager) hostSnapshot() string {
	var b strings.Builder
	if load, err := m.Probe(HostBound, "sysctl", "-n", "vm.loadavg"); err != nil {
		fmt.Fprintf(&b, "load average unknown, sysctl failed: %v\n", err)
	} else {
		fmt.Fprintf(&b, "load average %s\n", strings.Trim(strings.TrimSpace(load), "{ }"))
	}
	ps, err := m.Probe(HostBound, "ps", "-axo", "pid=,stat=,etime=,args=")
	if err != nil {
		fmt.Fprintf(&b, "disk processes unknown, ps failed: %v\n", err)
		return b.String()
	}
	found := false
	for _, line := range strings.Split(ps, "\n") {
		if line = strings.TrimSpace(line); line != "" && diskProcess.MatchString(line) {
			fmt.Fprintf(&b, "disk process %s\n", strings.Join(strings.Fields(line), " "))
			found = true
		}
	}
	if !found {
		b.WriteString("no disk process running\n")
	}
	return b.String()
}

// hostLine is a hostSnapshot on one line, for a log record.
func hostLine(host string) string {
	return strings.Join(strings.Split(strings.TrimRight(host, "\n"), "\n"), ", ")
}

// stamp is a log edge's wall-clock time, to the millisecond.
func stamp(t time.Time) string { return t.Format("2006-01-02T15:04:05.000Z07:00") }
