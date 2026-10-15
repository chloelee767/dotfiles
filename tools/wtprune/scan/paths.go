package scan

import (
	"os"
	"path/filepath"
	"runtime"
	"strings"
)

// Canonical resolves symlinks (/tmp -> /private/tmp, /var -> /private/var)
// so paths from git, lsof and os.Getwd compare equal. Paths that don't exist
// are only made absolute and cleaned.
func Canonical(p string) string {
	abs, err := filepath.Abs(p)
	if err != nil {
		return filepath.Clean(p)
	}
	if resolved, err := filepath.EvalSymlinks(abs); err == nil {
		return resolved
	}
	return abs
}

// within reports whether path is dir or inside it. Both must be canonical.
// macOS volumes are case-insensitive by default, so compare case-folded
// there: a false match only ever blocks or flags a row, never unblocks one.
func within(path, dir string) bool {
	if runtime.GOOS == "darwin" {
		path, dir = strings.ToLower(path), strings.ToLower(dir)
	}
	if path == dir {
		return true
	}
	return strings.HasPrefix(path, strings.TrimSuffix(dir, string(filepath.Separator))+string(filepath.Separator))
}

// strictlyWithin reports whether path is inside dir and not dir itself.
func strictlyWithin(path, dir string) bool {
	return within(path, dir) && !within(dir, path)
}

// readGitFile returns the gitdir a `.git` file points to, resolved against
// the worktree dir (git >= 2.48 can write relative paths).
func readGitFile(worktreeDir string) (string, error) {
	data, err := os.ReadFile(filepath.Join(worktreeDir, ".git"))
	if err != nil {
		return "", err
	}
	gitdir, ok := strings.CutPrefix(strings.TrimSpace(string(data)), "gitdir: ")
	if !ok {
		return "", &os.PathError{Op: "parse", Path: filepath.Join(worktreeDir, ".git"), Err: os.ErrInvalid}
	}
	if !filepath.IsAbs(gitdir) {
		gitdir = filepath.Join(worktreeDir, gitdir)
	}
	return filepath.Clean(gitdir), nil
}

// readRelFile reads a one-line path file inside an admin dir (gitdir,
// commondir), resolving a relative path against that dir.
func readRelFile(dir, name string) (string, error) {
	data, err := os.ReadFile(filepath.Join(dir, name))
	if err != nil {
		return "", err
	}
	p := strings.TrimSpace(string(data))
	if !filepath.IsAbs(p) {
		p = filepath.Join(dir, p)
	}
	return filepath.Clean(p), nil
}

func exists(p string) bool {
	_, err := os.Lstat(p)
	return err == nil
}
