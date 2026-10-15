package proc

import (
	"context"
	"fmt"
	"os"
	"os/signal"
	"syscall"
)

// Interrupted is a measurement's context, ended by SIGINT or SIGTERM with
// the signal as its cause, so the run's last record names what ended it.
func Interrupted(parent context.Context) (context.Context, func()) {
	signals := make(chan os.Signal, 1)
	signal.Notify(signals, syscall.SIGINT, syscall.SIGTERM)
	ctx, stop := interruptedBy(parent, signals)
	return ctx, func() {
		signal.Stop(signals)
		stop()
	}
}

// interruptedBy ends its context with the first signal signals delivers.
func interruptedBy(parent context.Context, signals <-chan os.Signal) (context.Context, func()) {
	ctx, cancel := context.WithCancelCause(parent)
	go func() {
		select {
		case sig := <-signals:
			cancel(fmt.Errorf("%v received (pid %d's run ended from outside)", sig, os.Getpid()))
		case <-ctx.Done():
		}
	}()
	return ctx, func() { cancel(context.Canceled) }
}
