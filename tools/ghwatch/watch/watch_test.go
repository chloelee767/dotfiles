package watch

import (
	"bytes"
	"context"
	"errors"
	"regexp"
	"testing"
	"time"
)

// scriptedSource returns one snapshot per poll, repeating the last one.
type scriptedSource struct {
	snapshots [][]Run
	errs      []error
	calls     int
}

func (s *scriptedSource) Fetch(context.Context) ([]Run, error) {
	i := min(s.calls, len(s.snapshots)-1)
	s.calls++
	if i < len(s.errs) && s.errs[i] != nil {
		return nil, s.errs[i]
	}
	return s.snapshots[i], nil
}

type fakeClock struct{ t time.Time }

func (c *fakeClock) Now() time.Time        { return c.t }
func (c *fakeClock) Sleep(d time.Duration) { c.t = c.t.Add(d) }

func runWatch(t *testing.T, src Source, filter Filter, failFast bool, timeout time.Duration) (string, string, int) {
	t.Helper()
	var out, errOut bytes.Buffer
	clock := &fakeClock{t: time.Unix(0, 0)}
	code := Watch(context.Background(), Config{
		Source:   src,
		Repo:     "o/r",
		Filter:   filter,
		Interval: 20 * time.Second,
		Timeout:  timeout,
		FailFast: failFast,
		Out:      &out,
		Err:      &errOut,
		Now:      clock.Now,
		Sleep:    clock.Sleep,
	})
	return out.String(), errOut.String(), code
}

func assertOutput(t *testing.T, got, want string) {
	t.Helper()
	if got != want {
		t.Errorf("output mismatch\n--- got ---\n%s\n--- want ---\n%s", got, want)
	}
}

func assertCode(t *testing.T, got, want int) {
	t.Helper()
	if got != want {
		t.Errorf("exit code = %d, want %d", got, want)
	}
}

func TestWatch_PrintsOnlyTransitionsThenSuccess(t *testing.T) {
	src := &scriptedSource{snapshots: [][]Run{
		{{ID: 1, Workflow: "CI", Status: "in_progress", URL: "u1", Jobs: []Job{
			{ID: 10, Name: "build", Status: "queued"},
			{ID: 11, Name: "lint", Status: "in_progress"},
		}}},
		{{ID: 1, Workflow: "CI", Status: "in_progress", URL: "u1", Jobs: []Job{
			{ID: 10, Name: "build", Status: "in_progress"},
			{ID: 11, Name: "lint", Status: "in_progress"},
		}}},
		{{ID: 1, Workflow: "CI", Status: "completed", Conclusion: "success", URL: "u1", Jobs: []Job{
			{ID: 10, Name: "build", Status: "completed", Conclusion: "success"},
			{ID: 11, Name: "lint", Status: "completed", Conclusion: "skipped"},
		}}},
	}}

	out, _, code := runWatch(t, src, Filter{}, true, time.Hour)

	assertCode(t, code, ExitSuccess)
	assertOutput(t, out, `run 1  CI  u1
  0:00  queued       build
  0:00  in_progress  lint
  0:20  in_progress  build
  0:40  success      build
  0:40  skipped      lint
---
SUCCESS -- 1 job(s) passed, 1 skipped
`)
}

// Matrix jobs appear only after an earlier job finishes. Seeing every
// existing job done must not end the watch while the run is still going.
func TestWatch_WaitsForRunCompletionNotJustVisibleJobs(t *testing.T) {
	src := &scriptedSource{snapshots: [][]Run{
		{{ID: 1, Workflow: "CI", Status: "in_progress", URL: "u1", Jobs: []Job{
			{ID: 10, Name: "plan", Status: "completed", Conclusion: "success"},
		}}},
		{{ID: 1, Workflow: "CI", Status: "completed", Conclusion: "success", URL: "u1", Jobs: []Job{
			{ID: 10, Name: "plan", Status: "completed", Conclusion: "success"},
			{ID: 11, Name: "gen-go (svc)", Status: "completed", Conclusion: "success"},
		}}},
	}}

	out, _, code := runWatch(t, src, Filter{}, true, time.Hour)

	assertCode(t, code, ExitSuccess)
	assertOutput(t, out, `run 1  CI  u1
  0:00  success      plan
  0:20  success      gen-go (svc)
---
SUCCESS -- 2 job(s) passed
`)
}

func TestWatch_FilterScopesOutputAndVerdict(t *testing.T) {
	src := &scriptedSource{snapshots: [][]Run{
		{{ID: 1, Workflow: "CI", Status: "completed", Conclusion: "failure", URL: "u1", Jobs: []Job{
			{ID: 10, Name: "gen-go (a)", Status: "completed", Conclusion: "success"},
			{ID: 11, Name: "gen-go (b)", Status: "completed", Conclusion: "success"},
			{ID: 12, Name: "gen-java (a)", Status: "completed", Conclusion: "failure"},
		}}},
	}}
	filter := Filter{
		Include: []*regexp.Regexp{regexp.MustCompile(`^gen-go`)},
		Exclude: []*regexp.Regexp{regexp.MustCompile(`\(b\)`)},
	}

	out, _, code := runWatch(t, src, filter, true, time.Hour)

	assertCode(t, code, ExitSuccess)
	assertOutput(t, out, `run 1  CI  u1
  0:00  success      gen-go (a)
---
SUCCESS -- 1 job(s) passed
`)
}

func TestWatch_FailureListsRealFailuresSeparatelyFromKnockOnCancels(t *testing.T) {
	src := &scriptedSource{snapshots: [][]Run{
		{{ID: 1, Workflow: "CI", Status: "completed", Conclusion: "failure", URL: "u1", Jobs: []Job{
			{ID: 10, Name: "test (a)", Status: "completed", Conclusion: "failure", URL: "j10"},
			{ID: 11, Name: "test (b)", Status: "completed", Conclusion: "cancelled", URL: "j11"},
			{ID: 12, Name: "test (c)", Status: "completed", Conclusion: "cancelled", URL: "j12"},
		}}},
	}}

	out, _, code := runWatch(t, src, Filter{}, true, time.Hour)

	assertCode(t, code, ExitFailed)
	assertOutput(t, out, `run 1  CI  u1
  0:00  failure      test (a)
  0:00  cancelled    test (b)
  0:00  cancelled    test (c)
---
FAILED:
  failure      test (a)  j10
(also cancelled as a knock-on: 2 job(s))
logs: gh run view 1 --repo o/r --log-failed
`)
}

func TestWatch_FailFastExitsBeforeRunCompletes(t *testing.T) {
	src := &scriptedSource{snapshots: [][]Run{
		{{ID: 1, Workflow: "CI", Status: "in_progress", URL: "u1", Jobs: []Job{
			{ID: 10, Name: "lint", Status: "completed", Conclusion: "failure", URL: "j10"},
			{ID: 11, Name: "build", Status: "in_progress"},
		}}},
	}}

	out, _, code := runWatch(t, src, Filter{}, true, time.Hour)

	assertCode(t, code, ExitFailed)
	assertOutput(t, out, `run 1  CI  u1
  0:00  failure      lint
  0:00  in_progress  build
---
FAILED:
  failure      lint  j10
logs: gh run view 1 --repo o/r --log-failed
(exited early on first failure, 1 job(s) still running -- pass --no-fail-fast to wait for all)
`)
}

func TestWatch_NoFailFastWaitsForRunCompletion(t *testing.T) {
	src := &scriptedSource{snapshots: [][]Run{
		{{ID: 1, Workflow: "CI", Status: "in_progress", URL: "u1", Jobs: []Job{
			{ID: 10, Name: "lint", Status: "completed", Conclusion: "failure", URL: "j10"},
			{ID: 11, Name: "build", Status: "in_progress"},
		}}},
		{{ID: 1, Workflow: "CI", Status: "completed", Conclusion: "failure", URL: "u1", Jobs: []Job{
			{ID: 10, Name: "lint", Status: "completed", Conclusion: "failure", URL: "j10"},
			{ID: 11, Name: "build", Status: "completed", Conclusion: "success"},
		}}},
	}}

	out, _, code := runWatch(t, src, Filter{}, false, time.Hour)

	assertCode(t, code, ExitFailed)
	assertOutput(t, out, `run 1  CI  u1
  0:00  failure      lint
  0:00  in_progress  build
  0:20  success      build
---
FAILED:
  failure      lint  j10
logs: gh run view 1 --repo o/r --log-failed
`)
}

func TestWatch_OnlyCancelledJobsIsCancelledVerdict(t *testing.T) {
	src := &scriptedSource{snapshots: [][]Run{
		{{ID: 1, Workflow: "CI", Status: "completed", Conclusion: "cancelled", URL: "u1", Jobs: []Job{
			{ID: 10, Name: "plan", Status: "completed", Conclusion: "cancelled", URL: "j10"},
			{ID: 11, Name: "build", Status: "completed", Conclusion: "skipped"},
		}}},
	}}

	out, _, code := runWatch(t, src, Filter{}, true, time.Hour)

	assertCode(t, code, ExitFailed)
	assertOutput(t, out, `run 1  CI  u1
  0:00  cancelled    plan
  0:00  skipped      build
---
CANCELLED (no job failed outright -- run may have been superseded or cancelled):
  cancelled    plan  j10
`)
}

// With a filter excluding the job that carries the cancellation, the
// matched jobs are merely skipped; the run conclusion explains why.
func TestWatch_CancelledRunWithOnlySkippedMatchedJobs(t *testing.T) {
	src := &scriptedSource{snapshots: [][]Run{
		{{ID: 1, Workflow: "CI", Status: "completed", Conclusion: "cancelled", URL: "u1", Jobs: []Job{
			{ID: 10, Name: "plan", Status: "completed", Conclusion: "cancelled"},
			{ID: 11, Name: "gen-go (a)", Status: "completed", Conclusion: "skipped"},
		}}},
	}}
	filter := Filter{Include: []*regexp.Regexp{regexp.MustCompile(`gen-go`)}}

	out, _, code := runWatch(t, src, filter, true, time.Hour)

	assertCode(t, code, ExitFailed)
	assertOutput(t, out, `run 1  CI  u1
  0:00  skipped      gen-go (a)
---
CANCELLED: run 1 (CI) was cancelled before any matched job ran  u1
`)
}

func TestWatch_AllMatchedJobsSkipped(t *testing.T) {
	src := &scriptedSource{snapshots: [][]Run{
		{{ID: 1, Workflow: "CI", Status: "completed", Conclusion: "success", URL: "u1", Jobs: []Job{
			{ID: 10, Name: "a", Status: "completed", Conclusion: "skipped"},
			{ID: 11, Name: "b", Status: "completed", Conclusion: "skipped"},
		}}},
	}}

	out, _, code := runWatch(t, src, Filter{}, true, time.Hour)

	assertCode(t, code, ExitFailed)
	assertOutput(t, out, `run 1  CI  u1
  0:00  skipped      a
  0:00  skipped      b
---
NO JOBS RAN -- all 2 matched job(s) were skipped
`)
}

func TestWatch_NoJobsMatchedFilter(t *testing.T) {
	src := &scriptedSource{snapshots: [][]Run{
		{{ID: 1, Workflow: "CI", Status: "completed", Conclusion: "success", URL: "u1", Jobs: []Job{
			{ID: 10, Name: "build", Status: "completed", Conclusion: "success"},
		}}},
	}}
	filter := Filter{Include: []*regexp.Regexp{regexp.MustCompile(`deploy`)}}

	out, _, code := runWatch(t, src, filter, true, time.Hour)

	assertCode(t, code, ExitFailed)
	assertOutput(t, out, `run 1  CI  u1
---
NO JOBS MATCHED the --job/--exclude filters
`)
}

func TestWatch_RunThatFailedBeforeCreatingJobs(t *testing.T) {
	src := &scriptedSource{snapshots: [][]Run{
		{{ID: 1, Workflow: "CI", Status: "completed", Conclusion: "startup_failure", URL: "u1"}},
	}}

	out, _, code := runWatch(t, src, Filter{}, true, time.Hour)

	assertCode(t, code, ExitFailed)
	assertOutput(t, out, `run 1  CI  u1
---
FAILED:
  startup_failure  run 1 (CI) created no jobs  u1
`)
}

func TestWatch_MultipleRunsPrefixJobsWithWorkflow(t *testing.T) {
	src := &scriptedSource{snapshots: [][]Run{
		{
			{ID: 1, Workflow: "CI", Status: "completed", Conclusion: "success", URL: "u1", Jobs: []Job{
				{ID: 10, Name: "build", Status: "completed", Conclusion: "success"},
			}},
			{ID: 2, Workflow: "Lint", Status: "completed", Conclusion: "success", URL: "u2", Jobs: []Job{
				{ID: 20, Name: "build", Status: "completed", Conclusion: "success"},
			}},
		},
	}}

	out, _, code := runWatch(t, src, Filter{}, true, time.Hour)

	assertCode(t, code, ExitSuccess)
	assertOutput(t, out, `run 1  CI  u1
  0:00  success      CI / build
run 2  Lint  u2
  0:00  success      Lint / build
---
SUCCESS -- 2 job(s) passed
`)
}

func TestWatch_TimeoutListsPendingMatchedJobs(t *testing.T) {
	src := &scriptedSource{snapshots: [][]Run{
		{{ID: 1, Workflow: "CI", Status: "in_progress", URL: "u1", Jobs: []Job{
			{ID: 10, Name: "build", Status: "completed", Conclusion: "success"},
			{ID: 11, Name: "test", Status: "in_progress"},
		}}},
	}}

	out, _, code := runWatch(t, src, Filter{}, true, 30*time.Second)

	assertCode(t, code, ExitTimeout)
	assertOutput(t, out, `run 1  CI  u1
  0:00  success      build
  0:00  in_progress  test
---
TIMED OUT after 30s -- still pending:
  in_progress  test
`)
}

func TestWatch_WaitsForRunsToAppearThenHintsAndTimesOut(t *testing.T) {
	src := &scriptedSource{snapshots: [][]Run{{}}}

	out, errOut, code := runWatch(t, src, Filter{}, true, 90*time.Second)

	assertCode(t, code, ExitTimeout)
	assertOutput(t, out, `---
TIMED OUT after 1m30s -- no runs found
`)
	assertOutput(t, errOut, "still no runs after 1:00 -- was the commit pushed, and does a workflow trigger on it?\n")
}

func TestWatch_FetchErrorIsRetried(t *testing.T) {
	src := &scriptedSource{
		errs: []error{errors.New("HTTP 502")},
		snapshots: [][]Run{
			nil,
			{{ID: 1, Workflow: "CI", Status: "completed", Conclusion: "success", URL: "u1", Jobs: []Job{
				{ID: 10, Name: "build", Status: "completed", Conclusion: "success"},
			}}},
		},
	}

	out, errOut, code := runWatch(t, src, Filter{}, true, time.Hour)

	assertCode(t, code, ExitSuccess)
	assertOutput(t, errOut, "warning: HTTP 502\n")
	assertOutput(t, out, `run 1  CI  u1
  0:20  success      build
---
SUCCESS -- 1 job(s) passed
`)
}

func TestWatch_HideSkippedOmitsSkippedLinesButKeepsThemInVerdict(t *testing.T) {
	src := &scriptedSource{snapshots: [][]Run{
		{{ID: 1, Workflow: "CI", Status: "completed", Conclusion: "success", URL: "u1", Jobs: []Job{
			{ID: 10, Name: "build", Status: "completed", Conclusion: "success"},
			{ID: 11, Name: "deploy", Status: "completed", Conclusion: "skipped"},
		}}},
	}}
	var out bytes.Buffer
	clock := &fakeClock{t: time.Unix(0, 0)}

	code := Watch(context.Background(), Config{
		Source:      src,
		Repo:        "o/r",
		Interval:    20 * time.Second,
		Timeout:     time.Hour,
		HideSkipped: true,
		Out:         &out,
		Err:         &bytes.Buffer{},
		Now:         clock.Now,
		Sleep:       clock.Sleep,
	})

	assertCode(t, code, ExitSuccess)
	assertOutput(t, out.String(), `run 1  CI  u1
  0:00  success      build
---
SUCCESS -- 1 job(s) passed, 1 skipped
`)
}
