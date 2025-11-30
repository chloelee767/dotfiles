This directory contains various scripts to run commonly used `kubectl` commands.
Most of them are designed to be used interactively, but there are also a few helper scripts used by other scripts that haven't been designed with interactive use in mind (eg. `kcontainer`, `kcurrentnamespace`).

Rarely used currently, and so might not be up to date:
- `kexecall`
- `kslogs`

Development notes:
- There are several common flags used by multiple scripts, we keep these the same between scripts as much as possible, eg. `--namespace` / `-n` for namespace
- There's also some common configs between some scripts, eg. default grep filters for logs, default filters for pod names
