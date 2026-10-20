package noticeloop

import (
	"context"
	"reflect"
	"sync"
	"testing"
)

// recorder captures deliveries and untold items.
type recorder struct {
	mu        sync.Mutex
	delivered []int
	untold    []int
	got       chan int
}

func (r *recorder) deliver(_ context.Context, item int) {
	r.mu.Lock()
	r.delivered = append(r.delivered, item)
	r.mu.Unlock()
	if r.got != nil {
		r.got <- item
	}
}

func TestRunDeliversInReportOrder(t *testing.T) {
	// Arrange
	r := &recorder{got: make(chan int, 3)}
	l := New(func(int) {})
	ctx, cancel := context.WithCancel(context.Background())
	done := make(chan struct{})
	go func() { l.Run(ctx, r.deliver); close(done) }()

	// Act
	for _, i := range []int{1, 2, 3} {
		l.Report(i)
	}
	for range 3 {
		<-r.got
	}
	cancel()
	<-done

	// Assert
	if !reflect.DeepEqual(r.delivered, []int{1, 2, 3}) {
		t.Errorf("delivered = %v, want [1 2 3]", r.delivered)
	}
}

func TestRunDeliversWhatWasPendingWhenItsContextEnds(t *testing.T) {
	// Arrange
	r := &recorder{}
	l := New(func(int) {})
	l.Report(7)
	l.Report(8)
	ctx, cancel := context.WithCancel(context.Background())
	cancel()

	// Act
	l.Run(ctx, r.deliver)

	// Assert
	if !reflect.DeepEqual(r.delivered, []int{7, 8}) {
		t.Errorf("delivered = %v, want [7 8]", r.delivered)
	}
}

func TestReportAfterRunReturnedGoesToUntold(t *testing.T) {
	// Arrange
	r := &recorder{}
	l := New(func(i int) { r.untold = append(r.untold, i) })
	ctx, cancel := context.WithCancel(context.Background())
	cancel()
	l.Run(ctx, r.deliver)

	// Act
	l.Report(9)

	// Assert
	if len(r.delivered) != 0 || !reflect.DeepEqual(r.untold, []int{9}) {
		t.Errorf("delivered = %v untold = %v, want nothing delivered and 9 untold", r.delivered, r.untold)
	}
}
