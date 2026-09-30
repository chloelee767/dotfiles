// ghwatch watches GitHub Actions runs and prints one line per job state
// transition, then a verdict. Unlike `gh run watch` it never reprints the
// job tree and never fetches logs, so a whole run costs a few dozen lines.
package main

import (
	"context"
	"errors"
	"flag"
	"fmt"
	"os"
	"os/exec"
	"regexp"
	"strings"
	"time"

	"github.com/cli/go-gh/v2/pkg/api"
	"github.com/cli/go-gh/v2/pkg/repository"

	"ghwatch/watch"
)

const usage = `Usage: ghwatch [target] [filters] [timing]

Watch GitHub Actions runs, printing one line per job state change and a final
verdict. Never fetches logs.

Target (at most one; default: the current git branch):
  --run <id|url>      a specific run (run or job URL also accepted)
  --branch <name>     runs for the branch's current tip commit on GitHub
  --sha <sha>         runs for a commit
  --pr <num|url>      runs for a PR's head commit
  --repo <owner/name> default: from the URL, or the current directory's remote
  --workflow <name>   only runs of this workflow, by name or file (eg. ci.yml); repeatable

Filters (regex on job name; scope both output and verdict):
  --job <regex>       include matching jobs; repeatable, OR'd
  --exclude <regex>   drop matching jobs; repeatable

Timing:
  --interval <dur>    poll interval (default 15s)
  --timeout <dur>     give up after this long (default 30m)
  --no-fail-fast      wait for every run to finish even after a job fails

Exit codes: 0 success, 1 failed/cancelled/no jobs ran, 2 timed out, 3 usage or lookup error.
`

type stringsFlag []string

func (s *stringsFlag) String() string     { return strings.Join(*s, ",") }
func (s *stringsFlag) Set(v string) error { *s = append(*s, v); return nil }

func main() {
	os.Exit(run())
}

func run() int {
	fs := flag.NewFlagSet("ghwatch", flag.ContinueOnError)
	fs.Usage = func() { fmt.Fprint(os.Stderr, usage) }
	var (
		runRef, branch, sha, prRef, repo string
		workflows, jobs, excludes        stringsFlag
		interval, timeout                time.Duration
		noFailFast                       bool
	)
	fs.StringVar(&runRef, "run", "", "")
	fs.StringVar(&branch, "branch", "", "")
	fs.StringVar(&sha, "sha", "", "")
	fs.StringVar(&prRef, "pr", "", "")
	fs.StringVar(&repo, "repo", "", "")
	fs.Var(&workflows, "workflow", "")
	fs.Var(&jobs, "job", "")
	fs.Var(&excludes, "exclude", "")
	fs.DurationVar(&interval, "interval", 15*time.Second, "")
	fs.DurationVar(&timeout, "timeout", 30*time.Minute, "")
	fs.BoolVar(&noFailFast, "no-fail-fast", false, "")
	if err := fs.Parse(os.Args[1:]); err != nil {
		if errors.Is(err, flag.ErrHelp) {
			return watch.ExitSuccess
		}
		return watch.ExitUsage
	}
	if fs.NArg() > 0 {
		return usageErr("unexpected argument: %s", fs.Arg(0))
	}

	targets := 0
	for _, v := range []string{runRef, branch, sha, prRef} {
		if v != "" {
			targets++
		}
	}
	if targets > 1 {
		return usageErr("pass at most one of --run, --branch, --sha, --pr")
	}

	filter, err := buildFilter(jobs, excludes)
	if err != nil {
		return usageErr("%v", err)
	}

	src := &githubSource{workflows: workflows, completedJobs: map[int64][]watch.Job{}}
	var prNumber int
	switch {
	case runRef != "":
		urlRepo, id, err := parseRunRef(runRef)
		if err != nil {
			return usageErr("%v", err)
		}
		repo = firstNonEmpty(repo, urlRepo)
		src.runIDs = []int64{id}
	case prRef != "":
		urlRepo, n, err := parsePRRef(prRef)
		if err != nil {
			return usageErr("%v", err)
		}
		repo = firstNonEmpty(repo, urlRepo)
		prNumber = n
	}

	if repo == "" {
		current, err := repository.Current()
		if err != nil {
			return usageErr("cannot infer the repo from the current directory (%v); pass --repo owner/name", err)
		}
		repo = current.Owner + "/" + current.Name
	}
	src.repo = repo

	client, err := api.DefaultRESTClient()
	if err != nil {
		return lookupErr("github client: %v", err)
	}
	src.client = client
	ctx := context.Background()

	switch {
	case len(src.runIDs) > 0:
		fmt.Printf("watching run %d in %s\n", src.runIDs[0], repo)
	case prNumber != 0:
		var pr struct {
			Head struct {
				SHA string `json:"sha"`
			} `json:"head"`
		}
		if err := client.DoWithContext(ctx, "GET", fmt.Sprintf("repos/%s/pulls/%d", repo, prNumber), nil, &pr); err != nil {
			return lookupErr("PR %d in %s: %v", prNumber, repo, err)
		}
		src.sha = pr.Head.SHA
		fmt.Printf("watching %s PR #%d at %s\n", repo, prNumber, shortSHA(src.sha))
	case sha != "":
		// The runs API only matches full SHAs, so resolve abbreviations.
		var commit struct {
			SHA string `json:"sha"`
		}
		if err := client.DoWithContext(ctx, "GET", fmt.Sprintf("repos/%s/commits/%s", repo, sha), nil, &commit); err != nil {
			return lookupErr("commit %s in %s: %v", sha, repo, err)
		}
		src.sha = commit.SHA
		fmt.Printf("watching %s commit %s\n", repo, shortSHA(src.sha))
	default:
		if branch == "" {
			out, err := exec.Command("git", "branch", "--show-current").Output()
			branch = strings.TrimSpace(string(out))
			if err != nil || branch == "" {
				return usageErr("not on a git branch; pass --run, --branch, --sha or --pr")
			}
		}
		// The branch tip on GitHub, not the newest run: right after a push
		// the newest run is still the previous commit's.
		var b struct {
			Commit struct {
				SHA string `json:"sha"`
			} `json:"commit"`
		}
		if err := client.DoWithContext(ctx, "GET", fmt.Sprintf("repos/%s/branches/%s", repo, branch), nil, &b); err != nil {
			return lookupErr("branch %q in %s: %v", branch, repo, err)
		}
		src.sha = b.Commit.SHA
		src.branch = branch
		warnIfLocalHeadDiffers(repo, src.sha)
		fmt.Printf("watching %s branch %s at %s\n", repo, branch, shortSHA(src.sha))
	}

	return watch.Watch(ctx, watch.Config{
		Source:   src,
		Repo:     repo,
		Filter:   filter,
		Interval: interval,
		Timeout:  timeout,
		FailFast: !noFailFast,
		Out:      os.Stdout,
		Err:      os.Stderr,
	})
}

// warnIfLocalHeadDiffers flags an unpushed (or not yet fetched) HEAD, which
// would otherwise look like CI silently ignoring the latest commit.
func warnIfLocalHeadDiffers(repo, remoteSHA string) {
	remote, err := exec.Command("git", "remote", "get-url", "origin").Output()
	if err != nil || !sameRepo(string(remote), repo) {
		return
	}
	head, err := exec.Command("git", "rev-parse", "HEAD").Output()
	if err != nil {
		return
	}
	if localSHA := strings.TrimSpace(string(head)); localSHA != remoteSHA {
		fmt.Fprintf(os.Stderr, "warning: local HEAD %s differs from the branch tip on GitHub %s -- watching the GitHub tip. Unpushed commits?\n",
			shortSHA(localSHA), shortSHA(remoteSHA))
	}
}

func buildFilter(jobs, excludes []string) (watch.Filter, error) {
	var f watch.Filter
	for _, j := range jobs {
		re, err := regexp.Compile(j)
		if err != nil {
			return f, fmt.Errorf("--job %q: %v", j, err)
		}
		f.Include = append(f.Include, re)
	}
	for _, e := range excludes {
		re, err := regexp.Compile(e)
		if err != nil {
			return f, fmt.Errorf("--exclude %q: %v", e, err)
		}
		f.Exclude = append(f.Exclude, re)
	}
	return f, nil
}

func firstNonEmpty(a, b string) string {
	if a != "" {
		return a
	}
	return b
}

func shortSHA(sha string) string {
	if len(sha) > 7 {
		return sha[:7]
	}
	return sha
}

func usageErr(format string, args ...any) int {
	fmt.Fprintf(os.Stderr, "error: "+format+"\n", args...)
	fmt.Fprintln(os.Stderr, "run ghwatch --help for usage")
	return watch.ExitUsage
}

func lookupErr(format string, args ...any) int {
	fmt.Fprintf(os.Stderr, "error: "+format+"\n", args...)
	return watch.ExitUsage
}
