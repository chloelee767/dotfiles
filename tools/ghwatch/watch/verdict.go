package watch

import (
	"fmt"
	"io"
	"time"
)

func isFailure(state string) bool {
	switch state {
	case "failure", "timed_out", "startup_failure", "action_required":
		return true
	}
	return false
}

func isPass(state string) bool { return state == "success" || state == "neutral" }

type jobRef struct {
	run Run
	job Job
}

// verdict prints the outcome of the matched jobs and returns the exit code.
func verdict(out io.Writer, runs []Run, filter Filter, repo string) int {
	var failed, cancelled, passed []jobRef
	matched := 0
	for _, run := range runs {
		for _, job := range run.Jobs {
			if !filter.Match(job.Name) {
				continue
			}
			matched++
			ref := jobRef{run, job}
			switch state := job.State(); {
			case isFailure(state):
				failed = append(failed, ref)
			case state == "cancelled":
				cancelled = append(cancelled, ref)
			case isPass(state):
				passed = append(passed, ref)
			}
		}
	}

	// A run that fails before creating any job (eg. invalid workflow YAML)
	// only reports the failure at run level.
	var jobless []Run
	for _, run := range runs {
		if len(run.Jobs) == 0 && isFailure(run.Conclusion) {
			jobless = append(jobless, run)
		}
	}

	if len(failed) > 0 || len(jobless) > 0 {
		fmt.Fprintln(out, "FAILED:")
		for _, run := range jobless {
			fmt.Fprintf(out, "  %-11s  run %d (%s) created no jobs  %s\n", run.Conclusion, run.ID, run.Workflow, run.URL)
		}
		for _, ref := range failed {
			fmt.Fprintf(out, "  %-11s  %s  %s\n", ref.job.State(), displayName(runs, ref.run, ref.job), ref.job.URL)
		}
		// Siblings the runner cancelled because of the failure are noise:
		// they point at the wrong job.
		if len(cancelled) > 0 {
			fmt.Fprintf(out, "(also cancelled as a knock-on: %d job(s))\n", len(cancelled))
		}
		for _, id := range failedRunIDs(failed) {
			fmt.Fprintf(out, "logs: gh run view %d --repo %s --log-failed\n", id, repo)
		}
		return ExitFailed
	}

	if len(cancelled) > 0 {
		fmt.Fprintln(out, "CANCELLED (no job failed outright -- run may have been superseded or cancelled):")
		for _, ref := range cancelled {
			fmt.Fprintf(out, "  %-11s  %s  %s\n", "cancelled", displayName(runs, ref.run, ref.job), ref.job.URL)
		}
		return ExitFailed
	}

	if len(passed) == 0 {
		// A cancelled run may leave the matched jobs merely skipped, so
		// the run-level conclusion is what explains an empty result.
		for _, run := range runs {
			if run.Conclusion == "cancelled" {
				fmt.Fprintf(out, "CANCELLED: run %d (%s) was cancelled before any matched job ran  %s\n", run.ID, run.Workflow, run.URL)
				return ExitFailed
			}
		}
		switch {
		case matched == 0 && filter.active():
			fmt.Fprintln(out, "NO JOBS MATCHED the --job/--exclude filters")
		case matched == 0:
			fmt.Fprintln(out, "NO JOBS RAN -- the run(s) created no jobs")
		default:
			fmt.Fprintf(out, "NO JOBS RAN -- all %d matched job(s) were skipped\n", matched)
		}
		return ExitFailed
	}

	fmt.Fprintf(out, "SUCCESS -- %d job(s) passed", len(passed))
	if skipped := matched - len(passed); skipped > 0 {
		fmt.Fprintf(out, ", %d skipped", skipped)
	}
	fmt.Fprintln(out)
	return ExitSuccess
}

func failedRunIDs(failed []jobRef) []int64 {
	var ids []int64
	seen := map[int64]bool{}
	for _, ref := range failed {
		if !seen[ref.run.ID] {
			seen[ref.run.ID] = true
			ids = append(ids, ref.run.ID)
		}
	}
	return ids
}

func timedOut(out io.Writer, runs []Run, filter Filter, timeout time.Duration) {
	if len(runs) == 0 {
		fmt.Fprintf(out, "TIMED OUT after %s -- no runs found\n", timeout)
		return
	}
	fmt.Fprintf(out, "TIMED OUT after %s -- still pending:\n", timeout)
	for _, run := range runs {
		if run.Completed() {
			continue
		}
		if len(run.Jobs) == 0 {
			fmt.Fprintf(out, "  %-11s  run %d (%s), no jobs yet\n", run.Status, run.ID, run.Workflow)
		}
		for _, job := range run.Jobs {
			if filter.Match(job.Name) && !job.Completed() {
				fmt.Fprintf(out, "  %-11s  %s\n", job.State(), displayName(runs, run, job))
			}
		}
	}
}
