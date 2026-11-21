package hotreload

import "testing"

// svcWith is a service named NAME with STEPS' states, in order.
func svcWith(name string, steps ...State) *Service {
	svc := &Service{Name: name}
	for i, st := range steps {
		svc.Steps = append(svc.Steps, Step{Name: string(rune('a' + i)), State: st})
	}
	return svc
}

func TestServiceState(t *testing.T) {
	done := func(words string, from, to int64) State {
		return State{Kind: KindDone, Words: words, StartedMS: from, EndedMS: to}
	}
	tests := []struct {
		name string
		svc  *Service
		want State
	}{
		{"every step scheduled", svcWith(ServiceStore, scheduled(), scheduled()),
			State{Kind: KindScheduled, Words: "scheduled"}},
		{"between steps reads the last finished one", svcWith(ServiceStore, done("built", 10, 20), scheduled()),
			State{Kind: KindScheduled, Words: "built"}},
		{"a running step spans from the first start", svcWith(ServiceStore, done("built", 10, 20), State{Kind: KindRunning, Words: "restarting", StartedMS: 30}),
			State{Kind: KindRunning, Words: "restarting", StartedMS: 10}},
		{"a waiting step", svcWith(ServiceStore, done("built", 10, 20), State{Kind: KindWaiting, Words: "waits", StartedMS: 30}),
			State{Kind: KindWaiting, Words: "waits", StartedMS: 10}},
		{"a failed step wins", svcWith(ServiceStore, State{Kind: KindFailed, Words: "the build failed", StartedMS: 10, EndedMS: 15}, scheduled()),
			State{Kind: KindFailed, Words: "the build failed", StartedMS: 10, EndedMS: 15}},
		{"a skipped step", svcWith(ServiceStore, State{Kind: KindSkipped, Words: "stopped"}),
			State{Kind: KindSkipped, Words: "stopped"}},
		{"every step done spans the whole work", svcWith(ServiceStore, done("built", 10, 20), done("restarted", 30, 45)),
			State{Kind: KindDone, Words: "restarted", StartedMS: 10, EndedMS: 45}},
		{"an unchanged reload is unchanged", svcWith(ServiceStore, done("built", 10, 20), State{Kind: KindUnchanged, Words: "current"}),
			State{Kind: KindUnchanged, Words: "current"}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			svc := tt.svc

			// Act
			got := ServiceState(svc)

			// Assert
			if got != tt.want {
				t.Fatalf("ServiceState = %+v, want %+v", got, tt.want)
			}
		})
	}
}

func TestReloadStateReadsTheInstancesOnceJudged(t *testing.T) {
	inst := func(kind Kind, words string, from, to int64) Instance {
		return Instance{Key: words, Name: "ws", State: State{Kind: kind, Words: words, StartedMS: from, EndedMS: to}}
	}
	tests := []struct {
		name      string
		judged    bool
		own       State
		instances []Instance
		want      State
	}{
		{"not yet judged keeps its own", false, State{Kind: KindRunning, Words: "judging", StartedMS: 5}, nil,
			State{Kind: KindRunning, Words: "judging", StartedMS: 5}},
		{"judged with none is unchanged", true, State{Kind: KindRunning, Words: "judging", StartedMS: 5}, nil,
			State{Kind: KindUnchanged, Words: "every shim runs the fresh build"}},
		{"a running instance leads", true, State{Kind: KindRunning, StartedMS: 5}, []Instance{inst(KindWaiting, "waits", 7, 0), inst(KindRunning, "replacing", 6, 0)},
			State{Kind: KindRunning, Words: "replacing", StartedMS: 5}},
		{"then a waiting one", true, State{Kind: KindRunning, StartedMS: 5}, []Instance{inst(KindDone, "replaced", 6, 9), inst(KindWaiting, "waits", 7, 0)},
			State{Kind: KindWaiting, Words: "waits", StartedMS: 5}},
		{"then a failed one, named", true, State{Kind: KindRunning, StartedMS: 5}, []Instance{inst(KindFailed, "could not", 6, 8), inst(KindDone, "replaced", 6, 9)},
			State{Kind: KindFailed, Words: "ws could not", StartedMS: 5, EndedMS: 9}},
		{"every one done", true, State{Kind: KindRunning, StartedMS: 5}, []Instance{inst(KindDone, "replaced", 6, 9), inst(KindSkipped, "closed", 0, 0)},
			State{Kind: KindDone, Words: "1 replaced", StartedMS: 5, EndedMS: 9}},
		{"a failed deploy keeps its own", true, State{Kind: KindFailed, Words: "the deploy failed"}, []Instance{inst(KindRunning, "replacing", 6, 0)},
			State{Kind: KindFailed, Words: "the deploy failed"}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			svc := &Service{Name: ServiceShim, Judged: tt.judged, Instances: tt.instances, Steps: []Step{{Name: "replace", State: tt.own}}}

			// Act
			got := ReloadState(svc)

			// Assert
			if got != tt.want {
				t.Fatalf("ReloadState = %+v, want %+v", got, tt.want)
			}
		})
	}
}

func TestSummaryCountsTheDoneInstances(t *testing.T) {
	tests := []struct {
		name string
		svc  *Service
		want string
	}{
		{"no instances", &Service{Name: ServiceShim}, ""},
		{"a service that counts none", &Service{Name: ServiceStore, Instances: []Instance{{}}}, ""},
		{"some done", &Service{Name: ServiceWebapp, Instances: []Instance{{State: State{Kind: KindDone}}, {State: State{Kind: KindRunning}}}}, "1 of 2 reloaded"},
		{"every one ended", &Service{Name: ServiceWebapp, Instances: []Instance{{State: State{Kind: KindDone}}, {State: State{Kind: KindSkipped}}}}, ""},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			svc := tt.svc

			// Act
			got := Summary(svc)

			// Assert
			if got != tt.want {
				t.Fatalf("Summary = %q, want %q", got, tt.want)
			}
		})
	}
}

func TestWireStateDrawsEveryArm(t *testing.T) {
	tests := []struct {
		kind Kind
		want string
	}{
		{KindScheduled, "scheduled:w"}, {KindRunning, "running:w"}, {KindWaiting, "waiting:w"}, {KindDone, "done:w"},
		{KindUnchanged, "unchanged:w"}, {KindFailed, "failed:w"}, {KindSkipped, "skipped:w"},
	}
	for _, tt := range tests {
		t.Run(string(tt.kind), func(t *testing.T) {
			// Arrange
			st := State{Kind: tt.kind, Words: "w", StartedMS: 1, EndedMS: 2}

			// Act
			got := wireState(st)

			// Assert
			if arm(got) != tt.want {
				t.Fatalf("wireState = %s, want %s", arm(got), tt.want)
			}
		})
	}
}

func TestWireStateCarriesTheSpanOfATimedArm(t *testing.T) {
	// Arrange
	st := State{Kind: KindDone, Words: "w", StartedMS: 1, EndedMS: 2}

	// Act
	got := wireState(st).GetDone()

	// Assert
	if got.GetStartedAtMs() != 1 || got.GetEndedAtMs() != 2 {
		t.Fatalf("done = %v, want the span 1..2", got)
	}
}

func TestWireStateRefusesAKindItDoesNotName(t *testing.T) {
	// Arrange
	defer func() {
		if recover() == nil {
			t.Fatal("an unnamed kind did not panic")
		}
	}()

	// Act
	wireState(State{Kind: "nonsense"})
}

func TestResolveNamesTheFirstFailureOnTheBadge(t *testing.T) {
	// Arrange
	c := newCard("mediator", start)
	c.Services[0].Steps[0].State = State{Kind: KindFailed, Words: "the build failed", StartedMS: 1, EndedMS: 2}
	for _, svc := range c.Services[1:] {
		for i := range svc.Steps {
			svc.Steps[i].State = State{Kind: KindSkipped, Words: "stopped"}
		}
	}
	c.Services[0].Steps[1].State = State{Kind: KindSkipped}
	c.Services[0].Steps[2].State = State{Kind: KindSkipped}

	// Act
	card := Resolve(c)

	// Assert
	if got := badge(card); got != "failed:store: the build failed" {
		t.Fatalf("badge %s, want the store's failure", got)
	}
}

func TestShortTrimsAHashToSevenCharacters(t *testing.T) {
	tests := []struct{ in, want string }{{"0123456789", "0123456"}, {"abc", "abc"}}
	for _, tt := range tests {
		t.Run(tt.in, func(t *testing.T) {
			// Act
			got := short(tt.in)

			// Assert
			if got != tt.want {
				t.Fatalf("short = %q, want %q", got, tt.want)
			}
		})
	}
}

// Every service the card lists names what reloading it means.
func TestEveryServiceSaysWhatReloadingItMeans(t *testing.T) {
	// Arrange
	c := newCard("mediator", start)

	// Act
	card := Resolve(c)

	// Assert
	for _, svc := range card.GetServices() {
		if svc.GetAbout().GetText() == "" {
			t.Fatalf("service %s has no about", svc.GetName().GetText())
		}
	}
}
