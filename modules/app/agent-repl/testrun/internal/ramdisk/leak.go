package ramdisk

import (
	"errors"
	"fmt"
	"os"
	"path/filepath"
	"regexp"
	"strconv"
	"strings"
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
		device, _ := parseRecord(string(raw))
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
	t := &Trouble{}
	start := m.Now()
	t.List, t.ListErr = m.Probe(ListBound, "diskutil", "list")
	t.ListTook = m.Now().Sub(start)
	start = m.Now()
	t.Info, t.InfoErr = m.listImages()
	t.InfoTook = m.Now().Sub(start)
	m.Log.Infof("RAM disk check: %s; %s", probeLine("diskutil list", t.ListErr, t.ListTook), probeLine("hdiutil info", t.InfoErr, t.InfoTook))
	if !t.Answered() {
		// A DISK MANAGER THAT DOES NOT ANSWER IS NOT ASKED AGAIN: a reclaim's
		// or an acquire's every command would run out its own bound in turn.
		return nil, t, nil
	}
	reclaimed, err := m.Reclaim()
	for _, r := range reclaimed {
		m.Log.Infof("reclaimed the RAM disk %s at %s left by dead run pid %d (attached %v, killed %d leftover processes)", r.Device, r.Mount, r.Pid, r.Attached, r.Killed)
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
