//go:build darwin

package persistentwifi

/*
#cgo LDFLAGS: -framework IOKit -framework CoreFoundation
#include <stdint.h>
#include <stdlib.h>
#include <CoreFoundation/CoreFoundation.h>
#include <IOKit/IOKitLib.h>
#include <IOKit/IOMessage.h>
#include <IOKit/pwr_mgt/IOPM.h>

extern void persistentwifiLidHeard(uintptr_t handle, int closed);

// lid_watch is one subscription: the root power domain's interest
// notification, on the run loop of the thread that opened it. It is C
// memory, because IOKit holds a pointer to it for the subscription's life.
typedef struct {
	io_service_t root;
	IONotificationPortRef port;
	io_object_t notifier;
	CFRunLoopRef loop;
	CFRunLoopSourceRef stop;
	uintptr_t handle;
} lid_watch;

// lid_interest is the root domain's general-interest callback. Only the
// clamshell message is a lid change, and its argument carries the new state:
// kClamshellStateBit set is closed.
static void lid_interest(void *refcon, io_service_t service, natural_t type, void *arg) {
	if (type != kIOPMMessageClamshellStateChange) {
		return;
	}
	lid_watch *w = refcon;
	persistentwifiLidHeard(w->handle, (((uintptr_t)arg) & kClamshellStateBit) ? 1 : 0);
}

// lid_stop_perform runs on the watch's own run loop when lid_stop signals it,
// so a stop asked for before the loop runs is still taken once it does.
static void lid_stop_perform(void *info) {
	CFRunLoopStop(CFRunLoopGetCurrent());
}

// lid_open subscribes, THEN reads the state into *state (0 open, 1 closed, 2
// no lid). Answers 0, or the failing step: -1 no root domain, -2 no
// notification port, -3 no stop source, or IOKit's own positive code.
static int lid_open(lid_watch *w, uintptr_t handle, int *state) {
	w->handle = handle;
	w->root = IOServiceGetMatchingService(kIOMainPortDefault, IOServiceMatching("IOPMrootDomain"));
	if (w->root == IO_OBJECT_NULL) {
		return -1;
	}
	w->port = IONotificationPortCreate(kIOMainPortDefault);
	if (w->port == NULL) {
		IOObjectRelease(w->root);
		return -2;
	}
	CFRunLoopSourceContext ctx = {0};
	ctx.perform = lid_stop_perform;
	w->stop = CFRunLoopSourceCreate(kCFAllocatorDefault, 0, &ctx);
	if (w->stop == NULL) {
		IONotificationPortDestroy(w->port);
		IOObjectRelease(w->root);
		return -3;
	}
	w->loop = CFRunLoopGetCurrent();
	CFRunLoopAddSource(w->loop, IONotificationPortGetRunLoopSource(w->port), kCFRunLoopDefaultMode);
	CFRunLoopAddSource(w->loop, w->stop, kCFRunLoopDefaultMode);
	kern_return_t kr = IOServiceAddInterestNotification(w->port, w->root, kIOGeneralInterest, lid_interest, w, &w->notifier);
	if (kr != KERN_SUCCESS) {
		CFRunLoopRemoveSource(w->loop, w->stop, kCFRunLoopDefaultMode);
		CFRelease(w->stop);
		IONotificationPortDestroy(w->port);
		IOObjectRelease(w->root);
		return (int)kr;
	}
	CFTypeRef v = IORegistryEntryCreateCFProperty(w->root, CFSTR(kAppleClamshellStateKey), kCFAllocatorDefault, 0);
	if (v == NULL) {
		*state = 2;
	} else {
		*state = (CFGetTypeID(v) == CFBooleanGetTypeID() && CFBooleanGetValue((CFBooleanRef)v)) ? 1 : 0;
		CFRelease(v);
	}
	return 0;
}

static void lid_run(void) { CFRunLoopRun(); }

static void lid_stop(lid_watch *w) {
	CFRunLoopSourceSignal(w->stop);
	CFRunLoopWakeUp(w->loop);
}

static void lid_close(lid_watch *w) {
	IOObjectRelease(w->notifier);
	CFRunLoopRemoveSource(w->loop, w->stop, kCFRunLoopDefaultMode);
	CFRelease(w->stop);
	IONotificationPortDestroy(w->port);
	IOObjectRelease(w->root);
}
*/
import "C"

import (
	"context"
	"errors"
	"fmt"
	"runtime"
	"runtime/cgo"
	"sync"
	"unsafe"
)

// SystemLid is the operating system's lid: IOKit's clamshell-state interest
// notification on the root power domain (kIOPMMessageClamshellStateChange),
// heard on a run loop of its own thread.
func SystemLid() LidWatcher { return systemLid{} }

type systemLid struct{}

// Watch runs the subscription on one locked thread, since a run loop belongs
// to the thread that runs it.
func (systemLid) Watch(ctx context.Context, heard func(Lid)) error {
	runtime.LockOSThread()
	defer runtime.UnlockOSThread()

	handle := cgo.NewHandle(heard)
	defer handle.Delete()
	w := (*C.lid_watch)(C.calloc(1, C.size_t(unsafe.Sizeof(C.lid_watch{}))))
	defer C.free(unsafe.Pointer(w))

	var state C.int
	if rc := C.lid_open(w, C.uintptr_t(handle), &state); rc != 0 {
		return fmt.Errorf("subscribe to the lid's IOKit notifications: %s", lidOpenFailure(int(rc)))
	}
	defer C.lid_close(w)
	switch state {
	case 0:
		heard(LidOpen)
	case 1:
		heard(LidClosed)
	default:
		heard(LidNone)
	}

	// THE STOP IS A RUN-LOOP SOURCE, so a ctx that ends before the loop runs
	// still stops it. The stopper is waited for before the subscription is
	// closed, so it never signals a source that is gone.
	finished := make(chan struct{})
	var stopper sync.WaitGroup
	stopper.Add(1)
	// inflight: joined: the function that starts it waits for it.
	go func() {
		defer stopper.Done()
		select {
		case <-ctx.Done():
			C.lid_stop(w)
		case <-finished:
		}
	}()
	C.lid_run()
	close(finished)
	stopper.Wait()
	if ctx.Err() != nil {
		return nil
	}
	return errors.New("the lid's run loop returned while the daemon still ran")
}

// lidOpenFailure names lid_open's failing step.
func lidOpenFailure(rc int) string {
	switch rc {
	case -1:
		return "IOPMrootDomain was not found"
	case -2:
		return "IONotificationPortCreate answered nothing"
	case -3:
		return "the run loop's stop source could not be made"
	}
	return fmt.Sprintf("IOServiceAddInterestNotification answered 0x%x", rc)
}
