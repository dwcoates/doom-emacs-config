package cpugov

import (
	"testing"
)

func TestHostSamplerReadsTheHostUnlessALoadIsFixed(t *testing.T) {
	// Arrange.
	tests := []struct {
		name  string
		fixed string
		host  bool
		load  float64
	}{
		{name: "unset", fixed: "", host: true},
		{name: "idle", fixed: "0", load: 0},
		{name: "busy", fixed: "0.6", load: 0.6},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act.
			s, err := HostSampler(tt.fixed)

			// Assert.
			if err != nil {
				t.Fatalf("HostSampler(%q): %v", tt.fixed, err)
			}
			if _, isHost := s.(*Host); isHost != tt.host {
				t.Fatalf("HostSampler(%q) = %T, want the host's own ticks %v", tt.fixed, s, tt.host)
			}
			if f, ok := s.(*Fixed); ok && f.Utilization != tt.load {
				t.Fatalf("HostSampler(%q) fixed %v, want %v", tt.fixed, f.Utilization, tt.load)
			}
		})
	}
}

func TestHostSamplerRefusesAValueThatIsNoLoad(t *testing.T) {
	for _, value := range []string{"busy", "-0.1", "1.5"} {
		t.Run(value, func(t *testing.T) {
			// Act.
			s, err := HostSampler(value)

			// Assert.
			if err == nil || s != nil {
				t.Fatalf("HostSampler(%q) = %v, %v; want the refusal", value, s, err)
			}
		})
	}
}

func TestAFixedLoadIsWhatTheGovernorSmoothsTo(t *testing.T) {
	// Arrange: a governor reading a fixed 60% load.
	r := newRig(t)
	g, err := New(DefaultConfig(), &Fixed{Utilization: 0.6, CPUs: 16}, r.clock.Now, r.log)
	if err != nil {
		t.Fatalf("New: %v", err)
	}

	// Act: many intervals, each SampleEvery apart.
	for i := 0; i < 200; i++ {
		r.clock.advance(SampleEvery)
		g.observe()
	}

	// Assert.
	g.mu.Lock()
	defer g.mu.Unlock()
	if got := g.sample.Utilization; got < 0.59 || got > 0.61 {
		t.Fatalf("the smoothed load is %v, want 0.6", got)
	}
}
