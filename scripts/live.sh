#!/usr/bin/env bash
# run-pty launches this wrapper instead of launching Lamdera directly.

source "$(dirname -- "${BASH_SOURCE[0]}")/persistence.sh"
cd -- "$project_dir"
lock_project
require_stopped

before=$(stat -c '%y:%s' -- "$snapshot_path" 2>/dev/null || true)
session_model=$(model_files)
status=0

# Keep Lamdera in the foreground so Ctrl+C reaches it. Bash waits for its
# finalizer before running the trap and continuing with the backup.
trap ':' INT TERM HUP
lamdera live "$@" || status=$?
trap '' INT TERM HUP

after=$(stat -c '%y:%s' -- "$snapshot_path" 2>/dev/null || true)
if [[ -s "$snapshot_path" && "$after" != "$before" ]]; then
    backup_state shutdown
else
    printf '\nNo se creó respaldo: Lamdera no escribió un snapshot nuevo al cerrar. Se conservan los anteriores.\n' >&2
fi

exit "$status"
