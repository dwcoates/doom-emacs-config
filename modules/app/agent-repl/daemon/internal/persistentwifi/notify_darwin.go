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

// THE RUN LOOP every heard fact's subscription runs on (watchOnRunLoop): the
// loop of the thread that opened it, and a stop source that ends it. It is C
// memory, because the loop holds a pointer to the source for its life.
typedef struct {
	CFRunLoopRef loop;
	CFRunLoopSourceRef stop;
} rl_core;

// rl_stop_perform runs on the core's own run loop when rl_stop signals it,
// so a stop asked for before the loop runs is still taken once it does.
static void rl_stop_perform(void *info) {
	CFRunLoopStop(CFRunLoopGetCurrent());
}

// rl_open makes the stop source on the current thread's run loop. Answers 0,
// or -1 when the source could not be made.
static int rl_open(rl_core *c) {
	CFRunLoopSourceContext ctx = {0};
	ctx.perform = rl_stop_perform;
	c->stop = CFRunLoopSourceCreate(kCFAllocatorDefault, 0, &ctx);
	if (c->stop == NULL) {
		return -1;
	}
	c->loop = CFRunLoopGetCurrent();
	CFRunLoopAddSource(c->loop, c->stop, kCFRunLoopDefaultMode);
	return 0;
}

static void rl_run(void) { CFRunLoopRun(); }

static void rl_stop(rl_core *c) {
	CFRunLoopSourceSignal(c->stop);
	CFRunLoopWakeUp(c->loop);
}

static void rl_close(rl_core *c) {
	CFRunLoopRemoveSource(c->loop, c->stop, kCFRunLoopDefaultMode);
	CFRelease(c->stop);
}

// lid_watch is the lid's subscription: the root power domain's interest
// notification, on the current thread's run loop. It is C memory, because
// IOKit holds a pointer to it for the subscription's life.
typedef struct {
	io_service_t root;
	IONotificationPortRef port;
	io_object_t notifier;
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

// lid_open subscribes, THEN reads the state into *state (0 open, 1 closed, 2
// no lid). Answers 0, or the failing step: -1 no root domain, -2 no
// notification port, or IOKit's own positive code.
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
	CFRunLoopAddSource(CFRunLoopGetCurrent(), IONotificationPortGetRunLoopSource(w->port), kCFRunLoopDefaultMode);
	kern_return_t kr = IOServiceAddInterestNotification(w->port, w->root, kIOGeneralInterest, lid_interest, w, &w->notifier);
	if (kr != KERN_SUCCESS) {
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

static void lid_close(lid_watch *w) {
	IOObjectRelease(w->notifier);
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

// watchOnRunLoop runs one IOKit subscription on a run loop of its own locked
// thread, since a run loop belongs to the thread that runs it, until ctx
// ends. subscribe subscribes on the current thread's loop, THEN reads and
// hands the state read to its heard, and answers how to unsubscribe. what
// names the subscription for the error of a loop that returned on its own.
func watchOnRunLoop(ctx context.Context, what string, subscribe func() (unsubscribe func(), err error)) error {
	runtime.LockOSThread()
	defer runtime.UnlockOSThread()

	core := (*C.rl_core)(C.calloc(1, C.size_t(unsafe.Sizeof(C.rl_core{}))))
	defer C.free(unsafe.Pointer(core))
	if C.rl_open(core) != 0 {
		return fmt.Errorf("subscribe to %s: the run loop's stop source could not be made", what)
	}
	defer C.rl_close(core)
	unsubscribe, err := subscribe()
	if err != nil {
		return err
	}
	defer unsubscribe()

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
			C.rl_stop(core)
		case <-finished:
		}
	}()
	C.rl_run()
	close(finished)
	stopper.Wait()
	if ctx.Err() != nil {
		return nil
	}
	return errors.New(what + "'s run loop returned while the daemon still ran")
}

// SystemLid is the operating system's lid: IOKit's clamshell-state interest
// notification on the root power domain (kIOPMMessageClamshellStateChange),
// heard on a run loop of its own thread (watchOnRunLoop).
func SystemLid() LidWatcher { return systemLid{} }

type systemLid struct{}

func (systemLid) Watch(ctx context.Context, heard func(Lid)) error {
	handle := cgo.NewHandle(heard)
	defer handle.Delete()
	w := (*C.lid_watch)(C.calloc(1, C.size_t(unsafe.Sizeof(C.lid_watch{}))))
	defer C.free(unsafe.Pointer(w))
	return watchOnRunLoop(ctx, "the lid", func() (func(), error) {
		var state C.int
		if rc := C.lid_open(w, C.uintptr_t(handle), &state); rc != 0 {
			return nil, fmt.Errorf("subscribe to the lid's IOKit notifications: %s", lidOpenFailure(int(rc)))
		}
		switch state {
		case 0:
			heard(LidOpen)
		case 1:
			heard(LidClosed)
		default:
			heard(LidNone)
		}
		return func() { C.lid_close(w) }, nil
	})
}

// lidOpenFailure names lid_open's failing step.
func lidOpenFailure(rc int) string {
	switch rc {
	case -1:
		return "IOPMrootDomain was not found"
	case -2:
		return "IONotificationPortCreate answered nothing"
	}
	return fmt.Sprintf("IOServiceAddInterestNotification answered 0x%x", rc)
}

