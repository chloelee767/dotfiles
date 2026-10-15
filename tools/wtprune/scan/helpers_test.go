package scan_test

import (
	"context"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"

	"wtprune/scan"
)

// isolateGit keeps the user's global/system git config out of the tests.
func isolateGit(t *testing.T) {
	t.Setenv("GIT_CONFIG_GLOBAL", "/dev/null")
	t.Setenv("GIT_CONFIG_NOSYSTEM", "1")
	t.Setenv("GIT_AUTHOR_NAME", "Test")
	t.Setenv("GIT_AUTHOR_EMAIL", "test@example.com")
	t.Setenv("GIT_COMMITTER_NAME", "Test")
	t.Setenv("GIT_COMMITTER_EMAIL", "test@example.com")
}

// newRepo creates root/repo with one commit. root is a fresh t.TempDir,
// which on macOS is a symlinked (non-canonical) /var/folders path.
func newRepo(t *testing.T) (root, repo string) {
	t.Helper()
	isolateGit(t)
	root = t.TempDir()
	repo = filepath.Join(root, "repo")
	run(t, root, "git", "init", "-q", "-b", "main", repo)
	write(t, filepath.Join(repo, "README"), "hello\n")
	write(t, filepath.Join(repo, ".gitignore"), ".env\n.DS_Store\nbuild/\n")
	run(t, repo, "git", "add", ".")
	run(t, repo, "git", "commit", "-qm", "init")
	return root, repo
}

// addOrigin creates a bare root/origin.git, pushes main and sets origin/HEAD.
func addOrigin(t *testing.T, root, repo string) {
	t.Helper()
	run(t, root, "git", "init", "-q", "--bare", "-b", "main", filepath.Join(root, "origin.git"))
	run(t, repo, "git", "remote", "add", "origin", filepath.Join(root, "origin.git"))
	run(t, repo, "git", "push", "-q", "origin", "main")
	run(t, repo, "git", "remote", "set-head", "origin", "main")
}

func run(t *testing.T, dir, name string, args ...string) string {
	t.Helper()
	cmd := exec.Command(name, args...)
	cmd.Dir = dir
	out, err := cmd.CombinedOutput()
	if err != nil {
		t.Fatalf("%s %v in %s: %v\n%s", name, args, dir, err, out)
	}
	return strings.TrimSpace(string(out))
}

// tryRun runs a command expected to fail (eg. a conflicting merge).
func tryRun(t *testing.T, dir, name string, args ...string) {
	t.Helper()
	cmd := exec.Command(name, args...)
	cmd.Dir = dir
	_ = cmd.Run()
}

func write(t *testing.T, path, content string) {
	t.Helper()
	if err := os.MkdirAll(filepath.Dir(path), 0o755); err != nil {
		t.Fatal(err)
	}
	if err := os.WriteFile(path, []byte(content), 0o644); err != nil {
		t.Fatal(err)
	}
}

func canonical(t *testing.T, path string) string {
	t.Helper()
	resolved, err := filepath.EvalSymlinks(path)
	if err != nil {
		t.Fatal(err)
	}
	return resolved
}

func noProcesses(context.Context) ([]scan.Process, error) { return nil, nil }

func scanRepo(t *testing.T, repo string) *scan.Result {
	t.Helper()
	res, err := scan.Scan(context.Background(), scan.Options{Repo: repo, Cwd: "/", Processes: noProcesses})
	if err != nil {
		t.Fatal(err)
	}
	return res
}

func scanDir(t *testing.T, dir string, depth int) *scan.Result {
	t.Helper()
	res, err := scan.Scan(context.Background(), scan.Options{Dir: dir, Depth: depth, Cwd: "/", Processes: noProcesses})
	if err != nil {
		t.Fatal(err)
	}
	return res
}

func row(t *testing.T, res *scan.Result, name string) *scan.Worktree {
	t.Helper()
	for _, w := range res.Rows {
		if filepath.Base(w.Path) == name {
			return w
		}
	}
	t.Fatalf("no row named %s", name)
	return nil
}

func names(res *scan.Result) []string {
	var got []string
	for _, w := range res.Rows {
		got = append(got, filepath.Base(w.Path))
	}
	return got
}
