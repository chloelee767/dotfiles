package scan

import (
	"bytes"
	"context"
	"fmt"
	"os/exec"
	"strconv"
	"strings"
)

// ProcessLister returns every process with a known cwd.
type ProcessLister func(ctx context.Context) ([]Process, error)

// ListProcesses lists process cwds with `lsof -d cwd -Fpcn`. lsof exits
// non-zero when some processes are inaccessible, so partial output counts.
func ListProcesses(ctx context.Context) ([]Process, error) {
	var stdout, stderr bytes.Buffer
	cmd := exec.CommandContext(ctx, "lsof", "-d", "cwd", "-Fpcn")
	cmd.Stdout = &stdout
	cmd.Stderr = &stderr
	err := cmd.Run()
	if err != nil && stdout.Len() == 0 {
		return nil, fmt.Errorf("lsof: %v: %s", err, strings.TrimSpace(stderr.String()))
	}
	return ParseLsof(stdout.String()), nil
}

// ParseLsof parses `lsof -Fpcn` field output: p<pid>, c<command>, then
// f<fd>/n<name> pairs per open file.
func ParseLsof(out string) []Process {
	var procs []Process
	var pid int
	var command string
	for _, line := range strings.Split(out, "\n") {
		if line == "" {
			continue
		}
		value := line[1:]
		switch line[0] {
		case 'p':
			pid, _ = strconv.Atoi(value)
			command = ""
		case 'c':
			command = value
		case 'n':
			procs = append(procs, Process{PID: pid, Command: command, Cwd: value})
		}
	}
	return procs
}
