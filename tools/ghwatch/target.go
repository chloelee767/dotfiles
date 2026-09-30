package main

import (
	"fmt"
	"net/url"
	"strconv"
	"strings"
)

// parseRunRef accepts a run ID or a run/job URL such as
// https://github.com/o/r/actions/runs/123/job/456. repo is empty for a bare ID.
func parseRunRef(s string) (repo string, runID int64, err error) {
	if id, err := strconv.ParseInt(s, 10, 64); err == nil {
		return "", id, nil
	}
	repo, rest, err := parseGitHubURL(s)
	if err != nil {
		return "", 0, err
	}
	// rest: actions/runs/<id>[/...]
	if len(rest) < 3 || rest[0] != "actions" || rest[1] != "runs" {
		return "", 0, fmt.Errorf("not a run ID or Actions run URL: %s", s)
	}
	id, err := strconv.ParseInt(rest[2], 10, 64)
	if err != nil {
		return "", 0, fmt.Errorf("not a run ID or Actions run URL: %s", s)
	}
	return repo, id, nil
}

// parsePRRef accepts a PR number or a URL such as https://github.com/o/r/pull/12.
func parsePRRef(s string) (repo string, number int, err error) {
	if n, err := strconv.Atoi(s); err == nil {
		return "", n, nil
	}
	repo, rest, err := parseGitHubURL(s)
	if err != nil {
		return "", 0, err
	}
	if len(rest) < 2 || rest[0] != "pull" {
		return "", 0, fmt.Errorf("not a PR number or PR URL: %s", s)
	}
	n, err := strconv.Atoi(rest[1])
	if err != nil {
		return "", 0, fmt.Errorf("not a PR number or PR URL: %s", s)
	}
	return repo, n, nil
}

func parseGitHubURL(s string) (repo string, rest []string, err error) {
	u, err := url.Parse(s)
	if err != nil || u.Host == "" {
		return "", nil, fmt.Errorf("not a number or GitHub URL: %s", s)
	}
	parts := strings.Split(strings.Trim(u.Path, "/"), "/")
	if len(parts) < 2 {
		return "", nil, fmt.Errorf("URL has no owner/repo: %s", s)
	}
	return parts[0] + "/" + parts[1], parts[2:], nil
}

// sameRepo reports whether a git remote URL (https or ssh) points at
// owner/name.
func sameRepo(remoteURL, repo string) bool {
	u := strings.TrimSuffix(strings.TrimSpace(remoteURL), ".git")
	u = strings.TrimSuffix(u, "/")
	return strings.HasSuffix(strings.ToLower(u), "/"+strings.ToLower(repo)) ||
		strings.HasSuffix(strings.ToLower(u), ":"+strings.ToLower(repo))
}
