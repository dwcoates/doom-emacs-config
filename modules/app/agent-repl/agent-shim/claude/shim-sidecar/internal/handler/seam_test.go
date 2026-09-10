package handler

// seam_test.go — pins seamObserver.TaskStopped, which convert.Observer calls
// on a real TaskStop settle; the handler package's own unit tests never drove
// it directly.

import "testing"

func TestSeamObserverTaskStoppedForwardsToTheAdoptedCallback(t *testing.T) {
	// Arrange: a reader that adopted the stop callback.
	var got string
	o := &seamObserver{stopped: func(taskID string) { got = taskID }}

	// Act
	o.TaskStopped("t1")

	// Assert
	if got != "t1" {
		t.Fatalf("stopped callback saw %q, want t1", got)
	}
}

func TestSeamObserverTaskStoppedIsSilentWhenNobodyAdoptedIt(t *testing.T) {
	// Arrange: a reader that asked for spawn observations only.
	o := &seamObserver{}

	// Act, Assert: must not panic on a nil callback.
	o.TaskStopped("t1")
}
