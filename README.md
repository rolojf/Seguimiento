# Seguimiento · Elm y Lamdera

Base para migrar el tablero de oportunidades, responsabilidades, procesos y
pendientes a Elm, con la persistencia integrada de Lamdera y estilos Tailwind.

Repositorio: <https://github.com/rolojf/Seguimiento>.

## Estado actual

Una sola pantalla con `Lamdera.frontend`, sin Elm Land ni Google. El acceso usa
dos usuarios fijos, `Rolo` y `Dot`, cada uno con su propia clave aleatoria.
El backend exige sesión para enviar datos o aceptar modificaciones.

El contador de la plantilla se sustituyó por el tablero:

- Mapa 5×5 de oportunidades y responsabilidades, filtros Ventex/Contratista,
  calificaciones, posición por arrastre o teclado, notas, fuente, validación,
  edición, descarte, sugerencias de completado y archivo de completadas.
- Procesos Nueva OTI y Entregas con sus 17 y 23 tareas, varios responsables por
  tarea, cumplimiento independiente, envío a Pendientes, cancelación reversible,
  notas y vista de completados. Evidencia, cierre
  personal e historial se conservan sin confundir cierre con tareas completadas.
- Pendientes agrupados por Rolo, Dot y Otros, con aprobación, confirmación del
  cumplimiento, orden manual y separadores en blanco o con texto.
- Importación de respaldos anteriores y descarga del formato Lamdera que conserva
  los separadores y el orden manual.

Los dos usuarios comparten el mismo tablero. Las versiones de cada registro
impiden que una edición antigua sobrescriba un cambio ajeno; las vistas conservan
los formularios abiertos cuando llegan actualizaciones. No se necesita sondeo
HTTP periódico ni una base SQL adicional.

Desde el tablero, Ctrl+A añade un pendiente, Ctrl+O una oportunidad y Ctrl+R
un proceso; cada botón subraya su letra y muestra el atajo al pasar el cursor.
Dentro de un campo o formulario se conservan los atajos habituales
del navegador, incluido Ctrl+A para seleccionar texto. Los atajos respetan los
botones deshabilitados y no abren formularios durante una confirmación o guardado.
Al añadir un pendiente, el cursor comienza en su texto: Tab pasa a Responsable,
otro Tab a Guardar y Enter lo guarda y cierra el formulario cuando Lamdera confirma.
Tipo y posición siguen disponibles en «Más opciones» después de los botones.

Los procesos se distribuyen en dos columnas en pantalla ancha y una en móvil.
La explicación de cada tarea aparece al pasar el puntero, enfocar o tocar su
nombre. La casilla inicial «Pend» crea un pendiente por cada responsable que
aún no termina. El texto de las nuevas copias incluye cliente, tipo de proceso y
actividad separados por espacios, sin diagonales; se puede editar sin perder el
vínculo: la relación usa los identificadores del proceso y del paso, no el texto.
Repetir el envío no duplica pendientes; los descartados se recuperan con la
aprobación inicial correspondiente.

«Pend» queda marcada mientras haya alguna copia vinculada no descartada, también
si ya pasó a Completados. Desmarcarla mueve las copias abiertas a Descartados y
desvincula las completadas sin cambiar su estado ni fecha de completado. El
cumplimiento y los avisos del proceso se conservan. Volver a marcar recupera las
partes que aún falten; no vuelve a copiar las ya completadas. Las descripciones
anteriores se conservan, ya que pueden haber sido editadas.

Cada tarea define `owner : List Owner` en `src/Domain.elm`, reutilizando
`Rolando | Dot | Others`; por ejemplo, `owner = [ Rolando, Dot ]`. Las plantillas
conservan sus asignaciones anteriores como listas de un elemento. Los nombres de
responsables externos, como Compras o Lidia, quedan en el detalle de la actividad.
Las columnas Rolo, Dot y Otros muestran hasta tres casillas independientes. El
avance cuenta la tarea completa solo cuando todos sus responsables terminan.

Las partes enviadas se completan o reabren desde Pendientes, respetando su
aprobación y confirmación de cumplimiento: marcar un pendiente que aún necesita
revisión no completa todavía su parte del proceso. Al confirmarse definitivamente,
el backend marca solo la casilla de ese responsable; reabrirlo la desmarca. No se
pide una segunda revisión en Procesos. Mientras el vínculo esté activo, la casilla
del proceso refleja ese estado y no se edita directamente. El pendiente vinculado
admite cambios de texto y orden, pero no de responsable ni conversión en separador.

Las partes no enviadas conservan el cambio directo por cualquiera de los dos
usuarios. Cada casilla de cumplimiento tiene su propio aviso: si `Rolo` la cambia,
`Dot` ve «!»; su primer clic reconoce sin cambiar el valor y el siguiente lo cambia
y avisa a `Rolo`. Reconocer no produce otro aviso. Los estados y revisiones se
guardan en Lamdera, también al cerrar sesión.

La cancelación está al final de la fila, junto a Notas, mediante un botón X.
Es reversible y tiene su propio aviso de revisión. Excluye la actividad del avance,
la muestra gris y tachada, descarta sus pendientes vinculados todavía abiertos y
conserva los completados. Restaurar la actividad no restaura automáticamente esos
pendientes: «Pend» permite recuperarlos. Los pendientes de una actividad cancelada
no se pueden restaurar ni reabrir hasta restaurarla. No se conservan comentarios N/A.

Solo Rolo puede mover un proceso a «Completados», mediante un botón con
confirmación y siempre que no queden pendientes vinculados activos sin finalizar.
El proceso conserva todos sus pasos y notas, visibles en modo consulta para ambos
usuarios. No se pueden reabrir sus pendientes vinculados mientras esté completado. Desde esa vista, Rolo puede borrarlo definitivamente
con una segunda confirmación. El backend exige tanto el usuario como el estado
correctos. Completar un proceso no cambia automáticamente sus casillas. Borrarlo
desvincula los pendientes conservados, sin borrarlos. Borrar un pendiente no revierte
el cumplimiento ya registrado en el proceso.

La columna Notas, junto al botón de cancelación, abre las notas del paso en un modal. El ícono distingue
entre notas vacías y notas con contenido. Ambos usuarios pueden guardar o vaciar
las notas de procesos activos; se conservan hasta 10000 caracteres por paso.

Las oportunidades nuevas muestran «!» al otro usuario. Confirmarlas las valida.
Descartar oculta el registro de la vista activa y resuelve su aviso, pero conserva
los datos. El filtro «Descartados» permite consultarlos con su fecha de descarte,
del más reciente al más antiguo.

En oportunidades, seleccionar el elemento y pulsar «Descartar» lo retira sin
otro diálogo. Desde «Descartados», «Volver a vigentes» lo restaura al mapa;
también se conserva el aviso inmediato «Deshacer».

Dot puede marcar una casilla pequeña junto a cada burbuja para sugerir su
completado. Rolo ve esa sugerencia y decide: solo él puede completar una
oportunidad, con confirmación. Sale del mapa y pasa al filtro «Completados»,
donde solo Rolo puede borrarla definitivamente, también con confirmación.
El descarte se mantiene como una acción distinta, reversible. Las sugerencias
solo existen en oportunidades activas; al restaurar un descarte comienza sin
sugerencia. El backend valida estos permisos y estados.

En pendientes, ✓ aprueba y ✕ descarta. Una tarea debe aprobarse antes de marcarla
como completada. Si Rolo la crea para sí mismo, se aprueba automáticamente.
Si uno la asigna al otro, aprueba el destinatario; si Dot la crea para Dot,
aprueba Rolo. Para «Otros» aprueba el otro usuario.

Al completar una tarea, el otro usuario ve «!» junto a su casilla: el primer
clic inicia una espera de tres segundos, manteniendo la casilla en su lugar.
Si no llega un segundo clic, confirma y pasa de «Activos» a «Completados».
Un segundo clic dentro de esos tres segundos la deja pendiente y pide aprobación a
quien la había marcado como completada. No se guarda una confirmación intermedia.
La casilla de «Completados» también permite reabrirla después.
Si quien la completó desmarca su propia tarea,
se aplica la regla inicial de aprobación. Rolo puede completar sus propias tareas
sin confirmación ajena: pasan directamente a «Completados».

«Completados» muestra lo más reciente arriba. La X junto al nombre de cada
responsable borra la tarea de más abajo, una por clic y sin confirmación.
El backend comprueba que siga siendo la última tarea de ese grupo.

«Editar» permite elegir «Pendiente» o «Completado», incluso desde Completados.
Cambiar el estado desde el editor aplica las mismas reglas de aprobación;
guardar un estado Completado que ya estaba marcado no confirma el aviso ajeno.
El formulario ofrece «Descartar» o «Restaurar» según corresponda.
Dentro del editor, «Borrar definitivamente» aparece únicamente para pendientes
descartados; el backend también exige ese estado. Para modificar un descartado
hay que restaurarlo primero. Una tarea marcada como completada debe guardarse
primero como Pendiente antes de cambiar de responsable o convertirse en separador.
Editar su texto conservando Completado no cambia su fecha de completado.
El orden se cambia arrastrando desde el asa o indicando
cuántas posiciones subir (positivo) o bajar (negativo) al editar, conservando
el responsable. Los separadores ocupan una posición y no cuentan como tareas:
tienen una X en lugar de «Editar», que los borra directamente, sin confirmación
y sin enviarlos a descartados ni completados.
Una actualización ajena del orden obliga a revisar el movimiento antes de
guardarlo, para evitar aplicar posiciones antiguas.

La tipografía principal parte de 18 px y la secundaria de 16 px con la
configuración habitual del navegador; ambas respetan su tamaño de letra y zoom.
Los tooltips conservan su letra pequeña. Oportunidades, procesos y pendientes
usan respectivamente tonos verdes, índigo y ámbar. «Apariencia» permite elegir
Sistema, Claro u Oscuro; Sistema sigue la preferencia del dispositivo. La elección
manual se conserva mientras la página permanece abierta. En modo oscuro, las
tres secciones tienen también textos con su propio matiz de color.

Acadia y SQLite no forman parte del proyecto; la aplicación JavaScript anterior
se conserva en `../Seguimiento/`.

## Arranque local

Requisitos: Linux/WSL con Bash, `flock`, `pgrep`, GNU tar/coreutils, Node.js con npm
y el compilador Lamdera. Las dependencias JavaScript
son locales y se reproducen con `package-lock.json`. Se verificó con Node 22.23.3,
npm 12.2.0 y Tailwind 3.4.19.

En una copia nueva, instalar las dependencias:

```sh
npm ci
```

Configurar las claves una sola vez:

```sh
node scripts/setup-access.mjs
```

El generador guarda las claves de ambos usuarios en `.local/access.txt`, con
permisos privados y excluido de Git. No las imprime en la consola. Escribe solo
sus hashes en `src/Env.elm`. En esta copia ya se ejecutó: abre el archivo local
para copiar cada clave a tu gestor de contraseñas.

Iniciar la aplicación:

```sh
npm start
```

`npm start` (equivalente a `npm run start`) ejecuta Lamdera y Tailwind mediante
`run-pty`. La aplicación se abre en <http://localhost:8000>. No hay generación
previa de fuentes. Lamdera se inicia a través de `scripts/live.sh`, que crea el
respaldo al terminar; ejecutar `lamdera live` directamente omite ese respaldo.

Con npm 12 se autoriza únicamente el script de instalación revisado de
`node-pty`. En esta máquina, una instalación limpia puede usar las cabeceras Node
existentes con `npm_package_config_node_gyp_nodedir=/usr npm ci`.

Para regenerar solo los estilos:

```sh
npm run build:css
```

## Respaldos automáticos del desarrollo local

`lamdera live` lee y escribe el estado local en
`elm-stuff/lamdera/.lamdera-bem-dev`, dentro de la carpeta que contiene `elm.json`.
El código de Lamdera guarda la instantánea al cerrar el servidor; copiar el
archivo mientras sigue ejecutándose puede copiar una versión anterior. Véase
el [cierre del servidor en el código de Lamdera](https://github.com/lamdera/compiler/blob/lamdera-next/extra/Lamdera/CLI/Live.hs).
En esta máquina el compilador instalado se identifica como Lamdera 1.4.0;
`lamdera --version` muestra la versión de Elm, no la de Lamdera.

Para trabajar y respaldar:

1. Ejecuta `npm start` y usa el tablero normalmente.
2. Espera a que los cambios terminen de guardarse en la aplicación.
3. En la terminal de `run-pty`, pulsa **Ctrl+Z** para ver su panel y luego
   **Ctrl+C una vez** para cerrar Lamdera y Tailwind. Si ya estás en el panel,
   basta Ctrl+C.
4. Espera el mensaje **«Respaldo guardado: …»**. El envoltorio espera a que
   Lamdera termine antes de copiar el archivo. Si cierras solo el comando de
   Lamdera, también respalda; Tailwind puede continuar hasta que lo cierres.

Los respaldos se guardan en `respaldos/`, fuera de `elm-stuff` y excluidos de Git.
Cada carpeta tiene fecha UTC y un identificador:

```text
respaldos/lamdera-AAAAMMDDTHHMMSS.nnnnnnnnnZ-XXXXXX/
  .lamdera-bem-dev      Estado completo del backend local
  codigo.tar.gz        Código actual, incluidos cambios sin commit
  modelo.sha256        Huellas de elm.json, Types.elm y Domain.elm
  lamdera-version.txt  Versión del compilador
  revision.txt         Fecha, motivo, revisión Git y estado del código
  restauracion.txt     Indica si admite restauración automática
  SHA256SUMS           Comprobación de integridad de estos archivos
```

Después de crear una copia completa se conservan **los cinco respaldos más
recientes** y se eliminan las carpetas antiguas generadas por estos scripts.
No se rota si falla la copia. Los directorios incompletos no se ofrecen para
restaurar. `codigo.tar.gz` incluye fuentes, configuración y scripts; excluye
dependencias, `.local/` y el CSS generado, que Tailwind reconstruye al arrancar.
Los respaldos tienen permisos privados porque contienen datos, sesiones y hashes.
No copies esta carpeta a `public/` ni la publiques en Git.

Si Lamdera no reescribe el snapshot al cerrar, el script avisa y conserva los
respaldos anteriores. **Un cierre forzado, SIGKILL o un corte de energía puede
impedir tanto el guardado como el respaldo.** No fuerces el cierre con varios
Ctrl+C seguidos ni cierres la terminal antes del mensaje de respaldo. Controla
el cierre mediante `run-pty`, no enviando señales directamente al PID del script
Bash. Esta protección tampoco recupera cambios que aún no llegaron al servidor
ni sustituye una copia en otro disco.

### Restaurar y arrancar

Primero detén la instancia actual normalmente y cierra sus pestañas del navegador.
Desde la carpeta del proyecto ejecuta:

```sh
npm run restore
```

También puedes invocar directamente el script Bash:

```sh
bash scripts/restore.sh
```

El script muestra los respaldos del más reciente al más antiguo. Elige el número
de uno, o `0` para cancelar. Comprueba su integridad y exige las mismas
definiciones de modelo (`Types.elm`, `Domain.elm`), `elm.json` y versión de Lamdera.
Es una comprobación conservadora: un cambio en esos archivos puede impedir la
restauración aunque fuese compatible. No modifica el código ni realiza una
migración automática. Si difieren, recupera el código de `codigo.tar.gz` en una
copia separada y usa la versión indicada antes de recuperar sus datos.

Después pide escribir **RESTAURAR**. Solo entonces guarda el archivo actual
como respaldo `before-restore`, si existe, reemplaza el snapshot con la copia
elegida e inicia `npm start`. Ese respaldo preventivo también cuenta dentro del
límite de cinco. La copia `before-restore` se reserva para recuperación manual,
pues no se puede asegurar con qué tipos se escribió el archivo actual. Conserva
incluso un archivo dañado; no se ofrece como restaurable automáticamente.
La selección se copia a un temporal antes de la rotación, por lo que puede
restaurarse aunque fuera el respaldo más antiguo. Abre de nuevo
<http://localhost:8000> después del arranque.

Los scripts comparten un bloqueo para evitar dos arranques o una restauración
simultánea, y detectan una instancia `lamdera live` ejecutada directamente en este
proyecto. Usa siempre estos comandos: una instancia manual no participa en el
bloqueo. La validación comprueba el archivo y sus huellas; no ejecuta el
decodificador Elm. Si los archivos del modelo difieren entre arranque y cierre, la
copia se conserva para recuperación manual y el script no la restaura: el
backend podría haber seguido funcionando con una compilación anterior. El archivo
de código refleja los archivos al cerrar, no necesariamente esa compilación.
Lamdera puede reiniciar un modelo que no logra decodificar, como muestra su
[código de recuperación](https://github.com/lamdera/compiler/blob/lamdera-next/extra/LocalDev/runtime-src/Lamdera/Live.elm).

`elm-stuff` y `respaldos/` no se incluyen en los commits. Limpiar `elm-stuff` o
ejecutar `lamdera reset` elimina el snapshot de trabajo, pero no estas cinco
copias. Desplegar la aplicación tampoco transfiere automáticamente los datos
locales: el entorno alojado mantiene su propia persistencia.

## Acceso

Las claves tienen 32 bytes aleatorios (256 bits), codificados en 43 caracteres
base64url. Se valida su SHA256 en `Access.elm`, importado únicamente por el
backend. Este diseño depende de usar claves generadas al azar: no sustituirlas
por contraseñas cortas o elegidas por una persona.

El formulario recibe la clave al escribirla y la envía al backend para
validarla. No contiene credenciales incorporadas; vacía la clave del modelo al
enviarla. Una configuración vacía o una clave incorrecta no permiten entrar.
No hay registro de usuarios, recuperación de contraseña ni proveedor externo.

La identidad se obtiene del `SessionId` que entrega Lamdera. Cada mensaje que
modifica datos comprueba esa sesión; los datos se envían exclusivamente a
clientes autenticados. El frontend mantiene los datos privados solo dentro del
estado `SignedIn`. Al reconectar recibe una instantánea autorizada del tablero.

La sesión se conserva hasta cerrar sesión o reemplazar la clave. Cerrar sesión
revoca el acceso de todas las pestañas que comparten esa sesión. No se añadió
vencimiento automático ni limitación de intentos; el control de acceso impide
que una conexión anónima cambie el estado de la aplicación.

Para renovar las dos claves y revocar las sesiones anteriores:

```sh
node scripts/setup-access.mjs --replace
```

La revocación se aplica cuando el backend carga los nuevos hashes. Los clientes
ya conectados dejan de recibir datos y su siguiente petición se rechaza; los
datos que ya habían recibido no se pueden recuperar del navegador.

`lamdera live` ejecuta también el backend en el navegador y dispone de herramientas
de inspección y depuración de mensajes. Estas claves son para desarrollo local,
no deben reutilizarse como claves de producción. No se ha desplegado la aplicación.
Para un despliegue, generar otras claves y configurar `userOneKeyHash` y
`userTwoKeyHash` como valores secretos de Lamdera; su compilador impide usarlos
en el frontend. Fuera de localhost el acceso requiere HTTPS.

## Datos anteriores y respaldos

Por decisión del usuario, la aplicación comienza vacía: no contiene datos personales incrustados en el
código ni publicados en `public/`. Después de entrar, «Importar respaldo» permite
seleccionar `../Seguimiento/seguimiento-data-20261014T202903613Z.json` u otro
respaldo compatible. El archivo local disponible es una instantánea del 14 de
octubre de 2026, no una lectura del sitio actual.

El backend acepta `seguimiento-d1-backup`, versión 1, y
`seguimiento-lamdera-backup`, versiones 2, 3 y 4, hasta 5 MB. Comprueba todas las tablas
y filas antes de modificar el estado.
Solo permite importar sobre un tablero vacío y sin historial previo; no reemplaza
datos existentes. Se conservan identificadores, versiones, fechas, posiciones,
filas eliminadas, evidencia, cierre personal y resumen anterior de los procesos.
Las tablas antiguas `checks` y `process_snapshots` se guardan como archivo histórico
en el backend, no se convierten en tareas activas duplicadas.

«Descargar respaldo» exporta las cinco tablas, incluido ese archivo histórico,
en el formato Lamdera versión 4. Este conserva el tipo, posición, aprobación y
confirmación de cumplimiento de cada pendiente, su vínculo con una actividad,
el cumplimiento por responsable de cada tarea, las notas y estado de cada proceso,
el estado y sugerencias de completado del mapa, y el siguiente identificador,
incluso después de borrar filas. La importación valida que los vínculos existan,
no estén duplicados y concuerden con los responsables y cumplimientos. Los formatos
anteriores se importan sin vínculos y su cumplimiento único se aplica a los
responsables de la plantilla actual. No es
un archivo para restaurar directamente en la aplicación JavaScript anterior.
No incluye claves, sesiones ni avisos de revisión de oportunidades o casillas de
procesos; esos avisos se consideran ya revisados al importar. Sí conserva el flujo
de aprobación de pendientes. Los pendientes completados de respaldos v1/v2 pasan
a «Completados» sin atribuirles un autor desconocido; los procesos antiguos se
consideran activos. Los motivos N/A antiguos se
descartan al importar y no se exportan. No se importaron los datos
del respaldo anterior; se conserva únicamente como referencia y recuperación
opcional si el usuario decide utilizarlo más adelante.

El respaldo JSON que descarga el tablero es distinto del snapshot completo del
backend local. Sirve para importar los registros compatibles mediante la interfaz;
no debe copiarse a `.lamdera-bem-dev`. Los scripts de respaldo y restauración
descritos arriba conservan el snapshot completo. Para otro entorno o ante cambios
de tipos, conserva también una exportación JSON desde «Descargar respaldo».

## Organización

- `src/Domain.elm`: datos tipados, mensajes del tablero y plantillas de tareas.
- `src/BoardStore.elm`: validación y cambios de estado con control de versiones.
- `src/Backup.elm`: importación de respaldos D1 v1 y Lamdera v2/v3/v4; exportación v4.
- `src/Opportunities.elm`, `src/Processes.elm`, `src/Pending.elm`: vistas y
  formularios de cada sección, con Tailwind.
- `src/Dashboard.elm`: reúne esas tres secciones y sus avisos de guardado.
- `src/Frontend.elm`: acceso, estado de pantalla e importación/descarga de archivos.
- `src/Types.elm`: modelos y mensajes de Lamdera.
- `src/Backend.elm`: estado persistente, conexiones y autorización de cada operación.
- `src/Access.elm`, `src/Env.elm`: validación de claves y sus hashes.
- `scripts/setup-access.mjs`: generador local de claves con `node:crypto`.
- `scripts/live.sh`: arranque local y respaldo después de cerrar Lamdera.
- `scripts/restore.sh`: selección, comprobación y restauración antes del arranque.
- `scripts/persistence.sh`: bloqueo, copia privada y retención de cinco respaldos.
- `src/styles.css`, `tailwind.config.js`, `public/styles.css`: estilos Tailwind.

No se añadieron paquetes para el tablero: `elm/file` y `elm/time` ya estaban
presentes y ahora se declaran como dependencias directas. No se añadieron servicios
externos ni un generador de páginas. `public/keyboard.js` conecta los atajos de
teclado con los botones existentes de Elm.

## Compilación y Evergreen

```sh
npm run check
```

Para este cambio se comprobó la compilación conjunta sin iniciar el servidor:

```sh
ELM_HOME="$PWD/node_modules/.cache/elm" lamdera make src/Frontend.elm src/Backend.elm --output=/dev/null
npm run build:css
```

Por indicación del usuario, no se añadieron tests ni se ejecutaron pruebas del
tablero. La revisión de reglas, autorización y vínculos fue estática; no se probaron
los flujos nuevos de la interfaz en navegador ni se modificaron datos locales.
El nuevo `TaskState` y el origen de pendientes cambian el modelo persistido; un
snapshot local anterior no constituye una migración al nuevo formato. Para datos
que deban conservarse se requiere exportar JSON con el código anterior e importarlo
sobre un tablero vacío con el nuevo. En este cambio el usuario indicó que no
necesita conservar los datos locales anteriores.

El remoto `lamdera` está asociado a `seguimiento`. La consulta pública de versión
indica `v = 0`: todavía no hay un primer despliegue. `lamdera check` compila y
regenera el snapshot `src/Evergreen/V1/`; no corresponde crear una migración V2.
Después se detiene al comprobar configuración porque falta autenticación de CLI
(`lamdera login`). La comprobación completa y el despliegue siguen pendientes.

Se usó el compilador nativo ya instalado y una caché dentro de
`node_modules/.cache/elm`, mediante `ELM_HOME`, sin instalar herramientas globales.
Las futuras migraciones `src/Evergreen/` deben revisarse y conservarse en Git.

## Auditoría de dependencias

La última auditoría, realizada al retirar Elm Land, pasó de 19 a **5 alertas
altas**, todas derivadas de `braces` 3.0.3:
[GHSA-vfj7-8cjw-p6xm](https://github.com/advisories/GHSA-vfj7-8cjw-p6xm).
Los paquetes señalados fueron `braces`, `chokidar`, `micromatch`, `fast-glob` y
`tailwindcss`. En esa revisión no había parche publicado de `braces`.

Se conservan Tailwind 3 y los overrides PostCSS 8.5.30 y
postcss-selector-parser 7.1.6. Esta modificación no cambió las dependencias npm.

## Procedencia

La base fue
[elm-land-lamdera-auth-tailwind-template](https://github.com/kraklin/elm-land-lamdera-auth-tailwind-template),
commit `a9615c37c6edd934f0632e7b66e502bd582d19cf` del 15 de noviembre de 2023.
La interfaz y el acceso se reescribieron con Lamdera directamente; Tailwind
continúa generando los estilos.

Referencias:

- [Desarrollo local de Lamdera](https://dashboard.lamdera.app/docs/building)
- [Mensajes entre frontend y backend](https://dashboard.lamdera.app/docs/wire)
- [Evergreen](https://dashboard.lamdera.app/docs/evergreen)
- [Configuración de entorno](https://dashboard.lamdera.app/docs/environment)
- [Tailwind CLI](https://v3.tailwindcss.com/docs/installation)
