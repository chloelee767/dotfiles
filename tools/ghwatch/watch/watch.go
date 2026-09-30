// Package watch polls GitHub Actions runs and reports job state transitions
// and a final verdict, without ever fetching logs.
package watch

import (
	"context"
	"fmt"
	"io"
	"regexp"
	"time"
)

type Job struct {
	ID         int64
	Name       string
	Status     string
	Conclusion string
	URL        string
}

// State is the conclusion once a job has one, otherwise its status
// (queued, in_progress, waiting, ...).
func (j Job) State() string {
	if j.Status == "completed" && j.Conclusion != "" {
		return j.Conclusion
	}
	return j.Status
}

func (j Job) Completed() bool { return j.Status == "completed" }

type Run struct {
	ID         int64
	Workflow   string
	Status     string
	Conclusion string
	URL        string
	Jobs       []Job
}

func (r Run) Completed() bool { return r.Status == "completed" }

// Source returns the current state of every watched run. It may return
// no runs while CI has not registered the push yet.
type Source interface {
	Fetch(ctx context.Context) ([]Run, error)
}

type Filter struct {
	Include []*regexp.Regexp
	Exclude []*regexp.Regexp
}

func (f Filter) Match(jobName string) bool {
	for _, re := range f.Exclude {
		if re.MatchString(jobName) {
			return false
		}
	}
	if len(f.Include) == 0 {
		return true
	}
	for _, re := range f.Include {
		if re.MatchString(jobName) {
			return true
		}
	}
	return false
}

func (f Filter) active() bool { return len(f.Include) > 0 || len(f.Exclude) > 0 }

const (
	ExitSuccess = 0
	ExitFailed  = 1
	ExitTimeout = 2
	ExitUsage   = 3
)

type Config struct {
	Source   Source
	Repo     string
	Filter   Filter
	Interval time.Duration
	Timeout  time.Duration
	FailFast bool
	// HideSkipped omits skipped jobs from the transition log; the verdict
	// still counts them.
	HideSkipped bool
	Out         io.Writer
	Err         io.Writer
	// Now and Sleep default to the real clock; tests replace them.
	Now   func() time.Time
	Sleep func(time.Duration)
}

// noRunsHintAfter is how long to wait for CI to register a push before
// suggesting the push may never trigger a run.
const noRunsHintAfter = time.Minute

// Watch polls until every watched run completes (or a matched job fails,
// with FailFast), printing one line per job state transition, then a verdict.
// It returns the process exit code.
func Watch(ctx context.Context, cfg Config) int {
	if cfg.Now == nil {
		cfg.Now = time.Now
	}
	if cfg.Sleep == nil {
		cfg.Sleep = time.Sleep
	}
	w := &watcher{cfg: cfg, start: cfg.Now(), seenJobs: map[int64]string{}, seenRuns: map[int64]bool{}}
	var runs []Run
	hinted := false

	for {
		pollStart := cfg.Now()
		fetched, err := cfg.Source.Fetch(ctx)
		if err != nil {
			fmt.Fprintf(cfg.Err, "warning: %v\n", err)
		} else {
			runs = fetched
			w.printTransitions(runs, pollStart)

			if cfg.FailFast && hasMatchedFailure(runs, cfg.Filter) && !allCompleted(runs) {
				fmt.Fprintln(cfg.Out, "---")
				code := verdict(cfg.Out, runs, cfg.Filter, cfg.Repo)
				fmt.Fprintf(cfg.Out, "(exited early on first failure, %d job(s) still running -- pass --no-fail-fast to wait for all)\n",
					countPending(runs, cfg.Filter))
				return code
			}
			if len(runs) > 0 && allCompleted(runs) {
				fmt.Fprintln(cfg.Out, "---")
				return verdict(cfg.Out, runs, cfg.Filter, cfg.Repo)
			}
		}

		elapsed := cfg.Now().Sub(w.start)
		if elapsed >= cfg.Timeout {
			fmt.Fprintln(cfg.Out, "---")
			timedOut(cfg.Out, runs, cfg.Filter, cfg.Timeout)
			return ExitTimeout
		}
		if len(runs) == 0 && !hinted && elapsed >= noRunsHintAfter {
			fmt.Fprintf(cfg.Err, "still no runs after %s -- was the commit pushed, and does a workflow trigger on it?\n", fmtElapsed(elapsed))
			hinted = true
		}
		cfg.Sleep(cfg.Interval)
	}
}

type watcher struct {
	cfg      Config
	start    time.Time
	seenJobs map[int64]string // job ID -> last printed state
	seenRuns map[int64]bool
}

func (w *watcher) printTransitions(runs []Run, polledAt time.Time) {
	elapsed := fmtElapsed(polledAt.Sub(w.start))
	for _, run := range runs {
		if !w.seenRuns[run.ID] {
			fmt.Fprintf(w.cfg.Out, "run %d  %s  %s\n", run.ID, run.Workflow, run.URL)
			w.seenRuns[run.ID] = true
		}
		for _, job := range run.Jobs {
			if !w.cfg.Filter.Match(job.Name) {
				continue
			}
			state := job.State()
			if w.cfg.HideSkipped && state == "skipped" {
				continue
			}
			if w.seenJobs[job.ID] == state {
				continue
			}
			w.seenJobs[job.ID] = state
			fmt.Fprintf(w.cfg.Out, "%6s  %-11s  %s\n", elapsed, state, displayName(runs, run, job))
		}
	}
}

// displayName prefixes the workflow only when several runs are watched,
// since job names alone are then ambiguous.
func displayName(runs []Run, run Run, job Job) string {
	if len(runs) > 1 {
		return run.Workflow + " / " + job.Name
	}
	return job.Name
}

func fmtElapsed(d time.Duration) string {
	secs := int(d.Round(time.Second).Seconds())
	return fmt.Sprintf("%d:%02d", secs/60, secs%60)
}

func allCompleted(runs []Run) bool {
	for _, run := range runs {
		if !run.Completed() {
			return false
		}
	}
	return true
}

func hasMatchedFailure(runs []Run, filter Filter) bool {
	for _, run := range runs {
		for _, job := range run.Jobs {
			if filter.Match(job.Name) && isFailure(job.State()) {
				return true
			}
		}
	}
	return false
}

func countPending(runs []Run, filter Filter) int {
	n := 0
	for _, run := range runs {
		for _, job := range run.Jobs {
			if filter.Match(job.Name) && !job.Completed() {
				n++
			}
		}
	}
	return n
}
