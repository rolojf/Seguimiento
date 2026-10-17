#!/usr/bin/env bash
# Select a local snapshot, preserve the current disk state, then start normally.

source "$(dirname -- "${BASH_SOURCE[0]}")/persistence.sh"
cd -- "$project_dir"
lock_project
require_stopped
list_backups
(( ${#backups[@]} > 0 )) || fail 'No hay respaldos en respaldos/.'

printf 'Respaldos disponibles (el más reciente primero):\n'
choices=()
for ((index = ${#backups[@]} - 1; index >= 0; index--)); do
    choices+=("${backups[index]}")
    label='solo recuperación manual'
    if [[ "$(cat -- "${backups[index]}/restauracion.txt" 2>/dev/null || true)" == automatic ]]; then
        label='restaurable'
    fi
    printf '%d) %s [%s]\n' "${#choices[@]}" "${backups[index]##*/}" "$label"
done
printf '0) Cancelar\n'
read -r -p 'Número del respaldo: ' choice
[[ "$choice" == 0 ]] && exit 0
[[ "$choice" =~ ^[1-5]$ ]] && (( choice <= ${#choices[@]} )) || fail 'Selección no válida.'
selected=${choices[choice-1]}

(cd -- "$selected" && sha256sum --check --status SHA256SUMS) || fail 'El respaldo está incompleto o cambió su contenido.'
valid_snapshot "$selected/.lamdera-bem-dev" || fail 'El respaldo seleccionado no tiene el formato de snapshot esperado.'
[[ "$(cat -- "$selected/restauracion.txt")" == automatic ]] || fail 'Esta copia está marcada para recuperación manual. Revisa revision.txt y la compatibilidad del código antes de recuperar sus datos.'
cmp -s <(model_files) "$selected/modelo.sha256" || fail 'Los tipos o elm.json cambiaron. Recupera primero el código compatible de codigo.tar.gz en una copia separada; no se reemplazaron datos.'
cmp -s <(compiler_version) "$selected/lamdera-version.txt" || fail 'La versión de Lamdera difiere de la del respaldo. Usa la versión indicada en lamdera-version.txt; no se reemplazaron datos.'

printf '\nSe reemplazarán los datos locales con %s y se iniciará la aplicación.\n' "${selected##*/}"
printf 'Primero se guardará el archivo actual, si existe. También cuenta dentro de los cinco respaldos.\n'
read -r -p 'Escribe RESTAURAR para continuar: ' confirmation
[[ "$confirmation" == RESTAURAR ]] || exit 0
require_stopped

mkdir -p -- "$(dirname -- "$snapshot_path")"
candidate=$(mktemp "$(dirname -- "$snapshot_path")/.restore-XXXXXX")
trap 'rm -f -- "$candidate"' EXIT

# Stage the selection before retention can remove it while saving the old state.
cp -- "$selected/.lamdera-bem-dev" "$candidate"
if [[ -f "$snapshot_path" ]]; then
    backup_state before-restore
fi
mv -f -- "$candidate" "$snapshot_path"
trap - EXIT
printf 'Datos restaurados. Iniciando Lamdera y Tailwind…\n'

flock -u 9
exec 9>&-
exec npm start
