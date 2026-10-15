package git_test

import (
	"context"
	"errors"
	"os"
	"os/exec"
	"path/filepath"
	"reflect"
	"testing"

	"wtprune/git"
)

func TestParseWorktreeList(t *testing.T) {
	out := "worktree /r\x00HEAD aaa\x00branch refs/heads/main\x00\x00" +
		"worktree /w1\x00HEAD bbb\x00detached\x00locked my reason\x00\x00" +
		"worktree /w 2\x00HEAD ccc\x00branch refs/heads/feat\x00prunable gitdir file points to non-existent location\x00\x00"

	got := git.ParseWorktreeList(out)

	want := []git.WorktreeEntry{
		{Path: "/r", Head: "aaa", Branch: "refs/heads/main"},
		{Path: "/w1", Head: "bbb", Detached: true, Locked: true, LockReason: "my reason"},
		{Path: "/w 2", Head: "ccc", Branch: "refs/heads/feat", Prunable: true},
	}
	if !reflect.DeepEqual(got, want) {
		t.Errorf("got %+v\nwant %+v", got, want)
	}
}

func TestParseWorktreeList_Bare(t *testing.T) {
	got := git.ParseWorktreeList("worktree /r.git\x00bare\x00\x00")

	want := []git.WorktreeEntry{{Path: "/r.git", Bare: true}}
	if !reflect.DeepEqual(got, want) {
		t.Errorf("got %+v, want %+v", got, want)
	}
}

func TestParseStatus(t *testing.T) {
	out := "# branch.oid 4751bde\x00# branch.head feat\x00# branch.upstream origin/feat\x00# branch.ab +2 -1\x00" +
		"1 .M N... 100644 100644 100644 aaa aaa a file.txt\x00" +
		"2 RM N... 100644 100644 100644 bbb bbb R100 new name\x00old name\x00" +
		"u UU N... 100644 100644 100644 100644 c1 c2 c3 conflict.go\x00" +
		"? new dir/\x00" +
		"! .env\x00" +
		"! build/\x00"

	got, err := git.ParseStatus(out)
	if err != nil {
		t.Fatal(err)
	}

	want := git.Status{
		OID: "4751bde", Upstream: "origin/feat", Ahead: 2, Behind: 1, HasAB: true,
		Entries: []git.StatusEntry{
			{Kind: '1', XY: ".M", Path: "a file.txt"},
			{Kind: '2', XY: "RM", Path: "new name", OrigPath: "old name"},
			{Kind: 'u', XY: "UU", Path: "conflict.go"},
			{Kind: '?', XY: "??", Path: "new dir/"},
			{Kind: '!', XY: "!!", Path: ".env"},
			{Kind: '!', XY: "!!", Path: "build/"},
		},
	}
	if !reflect.DeepEqual(got, want) {
		t.Errorf("got %+v\nwant %+v", got, want)
	}
}

func TestParseIndex(t *testing.T) {
	out := "H 100644 aaa 0\ta.go\x00h 100644 bbb 0\thidden.go\x00S 100644 ccc 0\tsparse.go\x00" +
		"s 100644 ddd 0\tdir/with space.txt\x00H 160000 eee 0\tsub\x00"

	got := git.ParseIndex(out)

	want := []git.IndexEntry{
		{Tag: 'H', Mode: "100644", SHA: "aaa", Path: "a.go"},
		{Tag: 'h', Mode: "100644", SHA: "bbb", Path: "hidden.go"},
		{Tag: 'S', Mode: "100644", SHA: "ccc", Path: "sparse.go"},
		{Tag: 's', Mode: "100644", SHA: "ddd", Path: "dir/with space.txt"},
		{Tag: 'H', Mode: "160000", SHA: "eee", Path: "sub"},
	}
	if !reflect.DeepEqual(got, want) {
		t.Errorf("got %+v\nwant %+v", got, want)
	}
	if !got[1].AssumeUnchanged() || got[2].AssumeUnchanged() || !got[3].SkipWorktree() || !got[3].AssumeUnchanged() || !got[4].Gitlink() {
		t.Errorf("tag/mode predicates wrong for %+v", got)
	}
}

// Real git output, to catch format drift between git versions.
func TestParseStatus_RealGit(t *testing.T) {
	t.Setenv("GIT_CONFIG_GLOBAL", "/dev/null")
	t.Setenv("GIT_CONFIG_NOSYSTEM", "1")
	t.Setenv("GIT_AUTHOR_NAME", "Test")
	t.Setenv("GIT_AUTHOR_EMAIL", "test@example.com")
	t.Setenv("GIT_COMMITTER_NAME", "Test")
	t.Setenv("GIT_COMMITTER_EMAIL", "test@example.com")
	repo := t.TempDir()
	run(t, repo, "git", "init", "-q", "-b", "main")
	write(t, filepath.Join(repo, ".gitignore"), ".env\n")
	write(t, filepath.Join(repo, "old"), "x\n")
	run(t, repo, "git", "add", ".")
	run(t, repo, "git", "commit", "-qm", "init")
	run(t, repo, "git", "mv", "old", "new")
	write(t, filepath.Join(repo, "untracked"), "u\n")
	write(t, filepath.Join(repo, ".env"), "secret\n")

	out, err := git.Run(context.Background(), repo, "status", "--porcelain=v2", "--branch", "-z", "-unormal", "--ignored=matching")
	if err != nil {
		t.Fatal(err)
	}
	got, err := git.ParseStatus(out)
	if err != nil {
		t.Fatal(err)
	}

	want := []git.StatusEntry{
		{Kind: '2', XY: "R.", Path: "new", OrigPath: "old"},
		{Kind: '?', XY: "??", Path: "untracked"},
		{Kind: '!', XY: "!!", Path: ".env"},
	}
	if !reflect.DeepEqual(got.Entries, want) {
		t.Errorf("got entries %+v\nwant %+v", got.Entries, want)
	}
}

func TestRun_ErrorIncludesStderr(t *testing.T) {
	_, err := git.Run(context.Background(), t.TempDir(), "rev-parse", "--git-dir")

	var gitErr *git.Error
	if !errors.As(err, &gitErr) || gitErr.ExitCode != 128 || gitErr.Stderr == "" {
		t.Fatalf("got %#v, want a *git.Error with exit 128 and stderr", err)
	}
}

func run(t *testing.T, dir, name string, args ...string) {
	t.Helper()
	cmd := exec.Command(name, args...)
	cmd.Dir = dir
	if out, err := cmd.CombinedOutput(); err != nil {
		t.Fatalf("%s %v: %v\n%s", name, args, err, out)
	}
}

func write(t *testing.T, path, content string) {
	t.Helper()
	if err := os.WriteFile(path, []byte(content), 0o644); err != nil {
		t.Fatal(err)
	}
}

func TestRun_DisablesShowSignature(t *testing.T) {
	t.Setenv("GIT_CONFIG_GLOBAL", "/dev/null")
	t.Setenv("GIT_CONFIG_NOSYSTEM", "1")
	repo := t.TempDir()
	run(t, repo, "git", "init", "-q")
	run(t, repo, "git", "config", "log.showSignature", "true")

	out, err := git.Run(context.Background(), repo, "config", "--get", "log.showSignature")

	if err != nil || out != "false\n" {
		t.Errorf("log.showSignature = %q (%v), want false", out, err)
	}
}
