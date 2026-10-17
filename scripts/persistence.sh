#!/usr/bin/env bash
# Shared by live.sh and restore.sh; snapshots never belong in Git or public/.

set -euo pipefail
umask 077

project_dir=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")/.." && pwd -P)
backup_dir="$project_dir/respaldos"
snapshot_path="$project_dir/elm-stuff/lamdera/.lamdera-bem-dev"
backup_limit=5
export LC_ALL=C

fail() {
    printf 'Error: %s\n' "$*" >&2
    exit 1
}

for required_command in flock pgrep node tar sha256sum lamdera npm; do
    command -v "$required_command" >/dev/null || fail "Falta el comando requerido: $required_command."
done

lock_project() {
    mkdir -p -- "$project_dir/.local"
    exec 9>"$project_dir/.local/persistence.lock"
    flock -n 9 || fail 'Hay otro arranque o restauración en curso. Detén Lamdera primero.'
}

require_stopped() {
    local pid process_dir
    while read -r pid; do
        process_dir=$(readlink -f -- "/proc/$pid/cwd" 2>/dev/null || true)
        if [[ "$process_dir" == "$project_dir" || "$process_dir" == "$project_dir/"* ]]; then
            fail "Lamdera sigue ejecutándose en este proyecto (PID $pid). Ciérralo normalmente primero."
        fi
    done < <(pgrep -f '(^|[ /])lamdera[[:space:]]+live([[:space:]]|$)' || true)
}

compiler_version() {
    local help_text
    help_text=$(lamdera --help)
    printf '%s\n' "${help_text%%$'\n'*}"
}

model_files() {
    # These files define BackendModel and all of its project-owned field types.
    (cd -- "$project_dir" && sha256sum -- elm.json src/Types.elm src/Domain.elm)
}

valid_snapshot() {
    node - "$1" <<'JS'
const fs = require("node:fs");
try {
    const snapshot = JSON.parse(fs.readFileSync(process.argv[2], "utf8"));
    if (snapshot.t !== "p" || typeof snapshot.b !== "string" || !snapshot.b.length ||
        Buffer.from(snapshot.b, "base64").toString("base64") !== snapshot.b) {
        process.exit(1);
    }
} catch {
    process.exit(1);
}
JS
}

list_backups() {
    local directory name
    backups=()
    for directory in "$backup_dir"/lamdera-*; do
        name=${directory##*/}
        if [[ -d "$directory" && ! -L "$directory" && "$name" =~ ^lamdera-[0-9]{8}T[0-9]{6}\.[0-9]{9}Z-[[:alnum:]]{6}$ ]]; then
            backups+=("$directory")
        fi
    done
}

backup_state() (
    # Publish the complete directory before removing any older backup.
    local reason=$1 staging='' destination index
    mkdir -p -- "$backup_dir"
    chmod 700 -- "$backup_dir"
    staging=$(mktemp -d "$backup_dir/.incomplete-XXXXXX")
    trap '[[ -z "$staging" ]] || rm -rf -- "$staging"' EXIT

    cp -- "$snapshot_path" "$staging/.lamdera-bem-dev"
    if [[ "$reason" == shutdown ]]; then
        valid_snapshot "$staging/.lamdera-bem-dev" || fail 'El archivo no tiene el formato esperado de un snapshot Lamdera. Se conservan los respaldos anteriores.'
    fi

    # Include working files, not only HEAD: this project may have uncommitted changes.
    # Tailwind can still be writing its generated CSS when only Lamdera is stopped.
    tar --exclude=public/styles.css -czf "$staging/codigo.tar.gz" -C "$project_dir" \
        src scripts public elm.json package.json package-lock.json head.html \
        tailwind.config.js README.md .gitignore
    model_files >"$staging/modelo.sha256"
    compiler_version >"$staging/lamdera-version.txt"
    if [[ "$reason" == before-restore ]]; then
        # The existing file may predate this code; preserve it without certifying it.
        printf 'manual\n' >"$staging/restauracion.txt"
    elif [[ "${session_model:-}" != "$(cat -- "$staging/modelo.sha256")" ]]; then
        printf 'manual\n' >"$staging/restauracion.txt"
        printf 'Aviso: los archivos del modelo difieren entre arranque y cierre; esta copia exige recuperación manual.\n' >&2
    else
        printf 'automatic\n' >"$staging/restauracion.txt"
    fi
    {
        printf 'Fecha UTC: %s\nMotivo: %s\n' "$(date -u +%FT%TZ)" "$reason"
        printf 'Revisión Git: '
        git -C "$project_dir" rev-parse HEAD 2>/dev/null || printf 'sin commit\n'
        printf '\nEstado del código al respaldar:\n'
        git -C "$project_dir" status --short 2>/dev/null || true
    } >"$staging/revision.txt"
    (cd -- "$staging" && sha256sum -- .lamdera-bem-dev codigo.tar.gz modelo.sha256 lamdera-version.txt revision.txt restauracion.txt >SHA256SUMS)

    destination="$backup_dir/lamdera-$(date -u +%Y%m%dT%H%M%S.%NZ)-${staging##*-}"
    mv -- "$staging" "$destination"
    staging=''
    printf '\nRespaldo guardado: %s\n' "$destination"

    list_backups
    for ((index = 0; index < ${#backups[@]} - backup_limit; index++)); do
        rm -rf -- "${backups[index]}"
        printf 'Respaldo antiguo eliminado: %s\n' "${backups[index]##*/}"
    done
)
