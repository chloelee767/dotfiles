package main

import (
	"bytes"
	"context"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"
	"time"

	"wtprune/scan"
)

func TestParseAge(t *testing.T) {
	for in, want := range map[string]time.Duration{
		"36h":   36 * time.Hour,
		"14d":   14 * 24 * time.Hour,
		"2w":    14 * 24 * time.Hour,
		"90m":   90 * time.Minute,
		"1h30m": 90 * time.Minute,
	} {
		got, err := parseAge(in)
		if err != nil || got != want {
			t.Errorf("parseAge(%q) = %v, %v; want %v", in, got, err, want)
		}
	}
	for _, in := range []string{"", "d", "1.5d", "-1d", "soon"} {
		if _, err := parseAge(in); err == nil {
			t.Errorf("parseAge(%q) succeeded, want an error", in)
		}
	}
}

func TestFilterByAgeKeepsRowsWithUnknownIdle(t *testing.T) {
	old := &scan.Worktree{ID: "old", LastActive: time.Now(), Idle: 30 * 24 * time.Hour}
	recent := &scan.Worktree{ID: "recent", LastActive: time.Now(), Idle: time.Hour}
	unknown := &scan.Worktree{ID: "unknown"}

	got := filterByAge([]*scan.Worktree{old, recent, unknown}, 14*24*time.Hour)
	if len(got) != 2 || got[0] != old || got[1] != unknown {
		t.Errorf("filterByAge = %v, want old and unknown", got)
	}
}

func runCLI(t *testing.T, ctx context.Context, args ...string) (code int, stdout, stderr string) {
	t.Helper()
	var out, errOut bytes.Buffer
	code = run(ctx, func() {}, args, streams{out: &out, err: &errOut})
	return code, out.String(), errOut.String()
}

func TestUsageErrorsPointAtHelp(t *testing.T) {
	for _, args := range [][]string{{"--bogus"}, {"--depth", "0"}, {"--repo", "a", "--dir", "b"}, {"extra"}, {"--older-than", "soon"}} {
		code, _, stderr := runCLI(t, context.Background(), args...)
		if code != exitUsage || !strings.Contains(stderr, "run wtprune --help") || strings.Contains(stderr, "Usage:") {
			t.Errorf("%v: exit %d, stderr %q; want 2 and a pointer to --help", args, code, stderr)
		}
	}
}

func TestHelpGoesToStdout(t *testing.T) {
	code, stdout, _ := runCLI(t, context.Background(), "--help")
	if code != exitOK || !strings.HasPrefix(stdout, "Usage: wtprune") {
		t.Errorf("exit %d, stdout %q", code, stdout)
	}
}

func gitRepoWithWorktree(t *testing.T) (root string) {
	t.Helper()
	t.Setenv("GIT_CONFIG_GLOBAL", "/dev/null")
	t.Setenv("GIT_CONFIG_NOSYSTEM", "1")
	t.Setenv("GIT_AUTHOR_NAME", "Test")
	t.Setenv("GIT_AUTHOR_EMAIL", "test@example.com")
	t.Setenv("GIT_COMMITTER_NAME", "Test")
	t.Setenv("GIT_COMMITTER_EMAIL", "test@example.com")
	root = t.TempDir()
	repo := filepath.Join(root, "repo")
	for _, args := range [][]string{
		{"init", "-q", "-b", "main", repo},
		{"-C", repo, "commit", "-q", "--allow-empty", "-m", "init"},
		{"-C", repo, "worktree", "add", "-q", "-b", "feat", filepath.Join(root, "wts", "feat")},
	} {
		if out, err := exec.Command("git", args...).CombinedOutput(); err != nil {
			t.Fatalf("git %v: %v\n%s", args, err, out)
		}
	}
	return root
}

func TestListInDirMode(t *testing.T) {
	root := gitRepoWithWorktree(t)

	code, stdout, stderr := runCLI(t, context.Background(), "--dir", filepath.Join(root, "wts"), "--list")
	if code != exitOK {
		t.Fatalf("exit %d, stderr %q", code, stderr)
	}
	for _, want := range []string{"1 worktree in 1 repo  ·", "REPO", "feat", "--      merged (in main)"} {
		if !strings.Contains(stdout, want) {
			t.Errorf("--list output lacks %q:\n%s", want, stdout)
		}
	}
}

func TestListWithNoRowsPrintsNoTable(t *testing.T) {
	root := gitRepoWithWorktree(t)

	code, stdout, _ := runCLI(t, context.Background(), "--dir", filepath.Join(root, "wts"), "--older-than", "1000w")
	if code != exitOK || strings.Contains(stdout, "WORKTREE") || !strings.Contains(stdout, "1 hidden by --older-than") {
		t.Errorf("exit %d, output:\n%s", code, stdout)
	}
}

func TestInterruptedScanExits130Quietly(t *testing.T) {
	root := gitRepoWithWorktree(t)
	ctx, cancel := context.WithCancel(context.Background())
	cancel()

	code, stdout, stderr := runCLI(t, ctx, "--repo", filepath.Join(root, "repo"))
	if code != exitInterrupted || stderr != "" || stdout != "" {
		t.Errorf("exit %d, stdout %q, stderr %q; want 130 and no output", code, stdout, stderr)
	}
}
