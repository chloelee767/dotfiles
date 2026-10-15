package tui_test

import (
	"context"
	"os"
	"os/exec"
	"path/filepath"
	"testing"

	"wtprune/scan"
	"wtprune/tui"
)

func run(t *testing.T, dir, name string, args ...string) {
	t.Helper()
	cmd := exec.Command(name, args...)
	cmd.Dir = dir
	if out, err := cmd.CombinedOutput(); err != nil {
		t.Fatalf("%s %v in %s: %v\n%s", name, args, dir, err, out)
	}
}

func write(t *testing.T, path, content string) {
	t.Helper()
	if err := os.WriteFile(path, []byte(content), 0o644); err != nil {
		t.Fatal(err)
	}
}

// TestScannedRowsToConfirmedPlan drives the model over rows from a real
// scan: a worktree on a merged branch and a dirty one with a unique commit.
func TestScannedRowsToConfirmedPlan(t *testing.T) {
	t.Setenv("GIT_CONFIG_GLOBAL", "/dev/null")
	t.Setenv("GIT_CONFIG_NOSYSTEM", "1")
	t.Setenv("GIT_AUTHOR_NAME", "Test")
	t.Setenv("GIT_AUTHOR_EMAIL", "test@example.com")
	t.Setenv("GIT_COMMITTER_NAME", "Test")
	t.Setenv("GIT_COMMITTER_EMAIL", "test@example.com")
	root := t.TempDir()
	repo := filepath.Join(root, "repo")
	run(t, root, "git", "init", "-q", "-b", "main", repo)
	write(t, filepath.Join(repo, "README"), "hello\n")
	run(t, repo, "git", "add", ".")
	run(t, repo, "git", "commit", "-qm", "init")
	run(t, repo, "git", "worktree", "add", "-q", "-b", "merged", filepath.Join(root, "merged"))
	run(t, repo, "git", "worktree", "add", "-q", "-b", "dirty", filepath.Join(root, "dirty"))
	write(t, filepath.Join(root, "dirty", "README"), "unique\n")
	run(t, filepath.Join(root, "dirty"), "git", "commit", "-qam", "unique")
	write(t, filepath.Join(root, "dirty", "README"), "edited\n")

	noProcesses := func(context.Context) ([]scan.Process, error) { return nil, nil }
	res, err := scan.Scan(context.Background(), scan.Options{Repo: repo, Cwd: "/", Processes: noProcesses})
	if err != nil {
		t.Fatal(err)
	}
	m := tui.New(tui.Config{Rows: res.Rows, DeleteBranches: true})

	send(m, key("a"), enter, key("yes"), enter)
	p := m.ConfirmedPlan()
	if p == nil {
		t.Fatal("not confirmed")
	}
	if len(p.AtRisk) != 1 || filepath.Base(p.AtRisk[0].W.Path) != "dirty" {
		t.Errorf("at risk = %+v, want the dirty worktree", p.AtRisk)
	}
	if len(p.Safe) != 1 || filepath.Base(p.Safe[0].W.Path) != "merged" {
		t.Errorf("safe = %+v, want the merged worktree", p.Safe)
	}
	if len(p.Branches) != 1 || p.Branches[0].Name != "merged" {
		t.Errorf("branches = %+v, want only merged", p.Branches)
	}
}
