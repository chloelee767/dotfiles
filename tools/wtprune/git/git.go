// Package git runs git commands and parses the porcelain formats wtprune
// relies on. Every command runs with --no-optional-locks so scanning never
// rewrites an index (which would bump the idle signal and race other gits),
// and with log.showSignature=false so user config can't add lines to
// log/reflog output.
package git

import (
	"bytes"
	"context"
	"errors"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strconv"
	"strings"
	"sync"
	"syscall"
)

// Error is a failed git command, carrying its trimmed stderr.
type Error struct {
	Args     []string
	ExitCode int
	Stderr   string
	Err      error
}

func (e *Error) Error() string {
	msg := e.Stderr
	if msg == "" {
		msg = e.Err.Error()
	}
	return fmt.Sprintf("git %s: %s", strings.Join(e.Args, " "), msg)
}

func (e *Error) Unwrap() error { return e.Err }

// Run runs `git --no-optional-locks -C dir args...` and returns stdout.
// An empty dir runs in the current directory.
func Run(ctx context.Context, dir string, args ...string) (string, error) {
	return RunInput(ctx, dir, "", args...)
}

// RunInput is Run with stdin.
func RunInput(ctx context.Context, dir, stdin string, args ...string) (string, error) {
	full := []string{"--no-optional-locks", "-c", "log.showSignature=false"}
	if dir != "" {
		full = append(full, "-C", dir)
	}
	full = append(full, args...)
	cmd := exec.CommandContext(ctx, binary(), full...)
	// Own process group: a terminal Ctrl-C (SIGINT to the foreground group)
	// must not kill a git that is mid-action; Execute finishes the in-flight
	// actions, and cancelling ctx still kills git during a scan.
	cmd.SysProcAttr = &syscall.SysProcAttr{Setpgid: true}
	if stdin != "" {
		cmd.Stdin = strings.NewReader(stdin)
	}
	var stdout, stderr bytes.Buffer
	cmd.Stdout = &stdout
	cmd.Stderr = &stderr
	if err := cmd.Run(); err != nil {
		code := -1
		var exitErr *exec.ExitError
		if errors.As(err, &exitErr) {
			code = exitErr.ExitCode()
		}
		return stdout.String(), &Error{Args: args, ExitCode: code, Stderr: strings.TrimSpace(stderr.String()), Err: err}
	}
	return stdout.String(), nil
}

// binary is the git that PATH's git runs, resolved once: on macOS
// /usr/bin/git is an xcrun shim that costs a few ms per run, and a scan
// runs hundreds. Anything unexpected falls back to plain "git".
var binary = sync.OnceValue(func() string {
	out, err := exec.Command("git", "--exec-path").Output()
	if err != nil {
		return "git"
	}
	path := filepath.Join(strings.TrimSpace(string(out)), "git")
	if info, err := os.Stat(path); err != nil || !info.Mode().IsRegular() || info.Mode().Perm()&0o111 == 0 {
		return "git"
	}
	return path
})

// ExitCode returns the exit code of a failed git command, or -1.
func ExitCode(err error) int {
	var gitErr *Error
	if errors.As(err, &gitErr) {
		return gitErr.ExitCode
	}
	return -1
}

// IsAncestor reports whether commit a is an ancestor of (or equal to) b.
func IsAncestor(ctx context.Context, dir, a, b string) (bool, error) {
	_, err := Run(ctx, dir, "merge-base", "--is-ancestor", a, b)
	if err == nil {
		return true, nil
	}
	if ExitCode(err) == 1 {
		return false, nil
	}
	return false, err
}

// CountUnreachable counts commits reachable from tip that no branch, remote
// or tag contains, ignoring the local branches in exclude.
//
// The --exclude patterns that apply to --branches are matched without the
// refs/heads/ prefix: --exclude=refs/heads/<b> silently excludes nothing.
func CountUnreachable(ctx context.Context, dir, tip string, exclude []string) (int, error) {
	args := []string{"rev-list", "--count", tip, "--not"}
	for _, b := range exclude {
		args = append(args, "--exclude="+b)
	}
	args = append(args, "--branches", "--remotes", "--tags")
	out, err := Run(ctx, dir, args...)
	if err != nil {
		return 0, err
	}
	n, err := strconv.Atoi(strings.TrimSpace(out))
	if err != nil {
		return 0, fmt.Errorf("rev-list --count: unexpected output %q", out)
	}
	return n, nil
}
