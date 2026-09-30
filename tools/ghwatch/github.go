package main

import (
	"context"
	"fmt"
	"net/url"
	"path"
	"strings"

	"github.com/cli/go-gh/v2/pkg/api"

	"ghwatch/watch"
)

type apiRun struct {
	ID         int64  `json:"id"`
	Name       string `json:"name"`
	Path       string `json:"path"`
	Status     string `json:"status"`
	Conclusion string `json:"conclusion"`
	HTMLURL    string `json:"html_url"`
}

type apiJob struct {
	ID         int64  `json:"id"`
	Name       string `json:"name"`
	Status     string `json:"status"`
	Conclusion string `json:"conclusion"`
	HTMLURL    string `json:"html_url"`
}

// githubSource watches either fixed run IDs, or every run for a commit
// (re-listed on each poll, so workflows registering late are picked up).
type githubSource struct {
	client    *api.RESTClient
	repo      string
	runIDs    []int64
	sha       string
	branch    string
	workflows []string
	// Jobs of completed runs never change, so they are fetched only once.
	completedJobs map[int64][]watch.Job
}

func (s *githubSource) Fetch(ctx context.Context) ([]watch.Run, error) {
	apiRuns, err := s.listRuns(ctx)
	if err != nil {
		return nil, err
	}
	var runs []watch.Run
	for _, ar := range apiRuns {
		if !s.workflowMatches(ar) {
			continue
		}
		jobs, err := s.jobs(ctx, ar)
		if err != nil {
			return nil, err
		}
		runs = append(runs, watch.Run{
			ID:         ar.ID,
			Workflow:   ar.Name,
			Status:     ar.Status,
			Conclusion: ar.Conclusion,
			URL:        ar.HTMLURL,
			Jobs:       jobs,
		})
	}
	return runs, nil
}

func (s *githubSource) listRuns(ctx context.Context) ([]apiRun, error) {
	if len(s.runIDs) > 0 {
		var runs []apiRun
		for _, id := range s.runIDs {
			var r apiRun
			if err := s.get(ctx, fmt.Sprintf("repos/%s/actions/runs/%d", s.repo, id), &r); err != nil {
				return nil, err
			}
			runs = append(runs, r)
		}
		return runs, nil
	}
	q := url.Values{"head_sha": {s.sha}, "per_page": {"100"}}
	if s.branch != "" {
		q.Set("branch", s.branch)
	}
	var resp struct {
		WorkflowRuns []apiRun `json:"workflow_runs"`
	}
	if err := s.get(ctx, fmt.Sprintf("repos/%s/actions/runs?%s", s.repo, q.Encode()), &resp); err != nil {
		return nil, err
	}
	// The API lists newest first; oldest first reads naturally in the log.
	runs := resp.WorkflowRuns
	for i, j := 0, len(runs)-1; i < j; i, j = i+1, j-1 {
		runs[i], runs[j] = runs[j], runs[i]
	}
	return runs, nil
}

// workflowMatches accepts the workflow's display name or its file name
// (eg. "CI" or "ci.yml").
func (s *githubSource) workflowMatches(r apiRun) bool {
	if len(s.workflows) == 0 {
		return true
	}
	for _, w := range s.workflows {
		if strings.EqualFold(w, r.Name) || strings.EqualFold(w, path.Base(r.Path)) {
			return true
		}
	}
	return false
}

func (s *githubSource) jobs(ctx context.Context, r apiRun) ([]watch.Job, error) {
	if jobs, ok := s.completedJobs[r.ID]; ok {
		return jobs, nil
	}
	var jobs []watch.Job
	for page := 1; ; page++ {
		var resp struct {
			TotalCount int      `json:"total_count"`
			Jobs       []apiJob `json:"jobs"`
		}
		p := fmt.Sprintf("repos/%s/actions/runs/%d/jobs?filter=latest&per_page=100&page=%d", s.repo, r.ID, page)
		if err := s.get(ctx, p, &resp); err != nil {
			return nil, err
		}
		for _, j := range resp.Jobs {
			jobs = append(jobs, watch.Job{ID: j.ID, Name: j.Name, Status: j.Status, Conclusion: j.Conclusion, URL: j.HTMLURL})
		}
		if len(resp.Jobs) == 0 || len(jobs) >= resp.TotalCount {
			break
		}
	}
	if r.Status == "completed" {
		s.completedJobs[r.ID] = jobs
	}
	return jobs, nil
}

func (s *githubSource) get(ctx context.Context, p string, resp any) error {
	return s.client.DoWithContext(ctx, "GET", p, nil, resp)
}
