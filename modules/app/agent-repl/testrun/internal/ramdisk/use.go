package ramdisk

import (
	"fmt"
	"sync"
	"syscall"
	"time"
)

// UseEvery is how often a run's RAM disk is sampled for the space in use.
const UseEvery = time.Second

// MountUsed answers the bytes in use on the filesystem mounted at mount.
func MountUsed(mount string) (uint64, error) {
	var st syscall.Statfs_t
	if err := syscall.Statfs(mount, &st); err != nil {
		return 0, fmt.Errorf("statfs %s: %w", mount, err)
	}
	return (st.Blocks - st.Bfree) * uint64(st.Bsize), nil
}

// use is the record of a RAM disk's space in use over its run: the most it
// held at once, which is what the disk must be sized to (DefaultSizeMiB).
type use struct {
	mu       sync.Mutex
	peak     uint64
	samples  int
	failures int
	firstErr error
	stop     chan struct{}
	done     chan struct{}
}

// WatchUse samples the volume's space in use every interval until Release,
// which logs the peak. A RAM disk's memory is taken as blocks are first
// written and never handed back before the detach, so the peak live use is
// the least the disk can be sized to, and the size is what each run holds.
func (v *Volume) WatchUse(every time.Duration) {
	v.use = &use{stop: make(chan struct{}), done: make(chan struct{})}
	v.sampleUse()
	go func() {
		defer close(v.use.done)
		t := time.NewTicker(every)
		defer t.Stop()
		for {
			select {
			case <-v.use.stop:
				return
			case <-t.C:
				v.sampleUse()
			}
		}
	}()
}

// sampleUse reads the volume's space in use once.
func (v *Volume) sampleUse() {
	used, err := v.m.Used(v.Mount)
	v.use.mu.Lock()
	defer v.use.mu.Unlock()
	if err != nil {
		v.use.failures++
		if v.use.firstErr == nil {
			v.use.firstErr = err
		}
		return
	}
	v.use.samples++
	v.use.peak = max(v.use.peak, used)
}

// stopUse ends the sampling and logs the peak: at INFO, and at ERROR as well
// when a sample failed, naming the first failure.
func (v *Volume) stopUse() {
	if v.use == nil {
		return
	}
	select {
	case <-v.use.stop:
		return
	default:
	}
	close(v.use.stop)
	<-v.use.done
	v.use.mu.Lock()
	defer v.use.mu.Unlock()
	v.m.Log.Infof("RAM disk use: peak %d MiB of %d MiB on %s (%d samples)", v.use.peak>>20, v.SizeMiB, v.Device, v.use.samples)
	if v.use.failures > 0 {
		v.m.Log.Errorf("RAM disk use: %d of %d samples of %s failed, so the peak may be low; the first: %v", v.use.failures, v.use.failures+v.use.samples, v.Mount, v.use.firstErr)
	}
}
