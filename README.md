# Seguimiento · Elm y Lamdera

Base para migrar el tablero de oportunidades, responsabilidades, procesos y
pendientes a Elm, con la persistencia integrada de Lamdera y estilos Tailwind.

Repositorio: <https://github.com/rolojf/Seguimiento>.

## Estado actual

Una sola pantalla con `Lamdera.frontend`, sin Elm Land ni Google. El acceso usa
dos usuarios fijos, `usuario1` y `usuario2`, cada uno con su propia clave aleatoria.
El backend exige sesión para enviar datos o aceptar modificaciones.

El contador de la plantilla se sustituyó por el tablero:

- Mapa 5×5 de oportunidades y responsabilidades, filtros Ventex/Contratista,
  calificaciones, posición por arrastre o teclado, notas, fuente, validación,
  edición, borrado lógico y deshacer.
- Procesos Nueva OTI y Entregas con sus 17 y 23 tareas, responsables, avance,
  confirmación, pendiente y N/A reversible. Evidencia, cierre personal e historial
  se conservan sin confundir cierre con tareas completadas.
- Pendientes agrupados por Rolando, dot y Otros, con edición, cumplimiento, orden
  manual y separadores en blanco o con texto.
- Importación de respaldos anteriores y descarga del formato Lamdera que conserva
  los separadores y el orden manual.

Los dos usuarios comparten el mismo tablero. Las versiones de cada registro
impiden que una edición antigua sobrescriba un cambio ajeno; las vistas conservan
los formularios abiertos cuando llegan actualizaciones. No se necesita sondeo
HTTP periódico ni una base SQL adicional.

Los procesos se distribuyen en dos columnas en pantalla ancha y una en móvil.
La explicación de cada tarea aparece al pasar el puntero, enfocar o tocar su
nombre. N/A tiene su propia columna; las tareas excluidas aparecen grises y
tachadas. Se retiraron los comentarios de N/A, incluidos los anteriores.

Cada casilla de proceso conserva por separado quién debe revisar el último
cambio. Si `usuario1` la cambia, `usuario2` ve «!»: su primer clic confirma
la revisión sin cambiar el valor; el siguiente clic modifica el valor y avisa
a `usuario1`. Reconocer un cambio no produce otro aviso. Estos datos se guardan
en Lamdera, también al cerrar sesión. Las columnas dot, Rolando y Otros indican
responsables de tareas; son independientes de las dos cuentas de acceso.

Las oportunidades y los pendientes nuevos también muestran «!» al otro usuario.
Confirmar una oportunidad la valida; confirmar un pendiente mantiene su estado
de cumplimiento. Descartar oculta el registro de la vista activa y resuelve su
aviso, pero conserva los datos. El filtro «Descartados» permite consultarlos con
su fecha de descarte, del más reciente al más antiguo. Los registros anteriores
comienzan sin avisos retroactivos.

En oportunidades, seleccionar el elemento y pulsar «Descartar» lo retira sin
otro diálogo. Desde «Descartados», «Volver a vigentes» lo restaura al mapa;
también se conserva el aviso inmediato «Deshacer».

En pendientes, «Editar» abre un formulario con «Descartar» o «Restaurar» según su
estado. «Borrar definitivamente» aparece únicamente para pendientes descartados;
el backend también exige ese estado. Para modificar un descartado hay que
restaurarlo primero. El orden se cambia arrastrando desde el asa o indicando
cuántas posiciones subir (positivo) o bajar (negativo) al editar, conservando
el responsable. Los separadores ocupan una posición y no cuentan como tareas.
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

Requisitos: Node.js con npm y el compilador Lamdera. Las dependencias JavaScript
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

`npm start` ejecuta Lamdera y Tailwind. La aplicación se abre en
<http://localhost:8000>. No hay generación previa de fuentes.

Con npm 12 se autoriza únicamente el script de instalación revisado de
`node-pty`. En esta máquina, una instalación limpia puede usar las cabeceras Node
existentes con `npm_package_config_node_gyp_nodedir=/usr npm ci`.

Para regenerar solo los estilos:

```sh
npm run build:css
```

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
`seguimiento-lamdera-backup`, versión 2, hasta 5 MB. Comprueba todas las tablas
y filas antes de modificar el estado.
Solo permite importar sobre un tablero vacío y sin historial previo; no reemplaza
datos existentes. Se conservan identificadores, versiones, fechas, posiciones,
filas eliminadas, evidencia, cierre personal y resumen anterior de los procesos.
Las tablas antiguas `checks` y `process_snapshots` se guardan como archivo histórico
en el backend, no se convierten en tareas activas duplicadas.

«Descargar respaldo» exporta las cinco tablas, incluido ese archivo histórico,
en el formato Lamdera versión 2. Este conserva el tipo y posición de cada
pendiente y el siguiente identificador, incluso después de borrar filas. No es
un archivo para restaurar directamente en la aplicación JavaScript anterior.
No incluye claves, sesiones ni avisos de revisión; al importar, los registros
se consideran ya revisados. Los motivos N/A antiguos se
descartan al importar y no se exportan. No se importaron los datos
del respaldo anterior; se conserva únicamente como referencia y recuperación
opcional si el usuario decide utilizarlo más adelante.

En desarrollo, `lamdera live` simula la persistencia en el navegador; conservar
el respaldo y exportar los cambios antes de limpiar sus datos. La persistencia
alojada requiere asociar y desplegar una aplicación Lamdera.

## Organización

- `src/Domain.elm`: datos tipados, mensajes del tablero y plantillas de tareas.
- `src/BoardStore.elm`: validación y cambios de estado con control de versiones.
- `src/Backup.elm`: importación de respaldos D1 v1 y Lamdera v2; exportación v2.
- `src/Opportunities.elm`, `src/Processes.elm`, `src/Pending.elm`: vistas y
  formularios de cada sección, con Tailwind.
- `src/Dashboard.elm`: reúne esas tres secciones y sus avisos de guardado.
- `src/Frontend.elm`: acceso, estado de pantalla e importación/descarga de archivos.
- `src/Types.elm`: modelos y mensajes de Lamdera.
- `src/Backend.elm`: estado persistente, conexiones y autorización de cada operación.
- `src/Access.elm`, `src/Env.elm`: validación de claves y sus hashes.
- `scripts/setup-access.mjs`: generador local de claves con `node:crypto`.
- `src/styles.css`, `tailwind.config.js`, `public/styles.css`: estilos Tailwind.

No se añadieron paquetes para el tablero: `elm/file` y `elm/time` ya estaban
presentes y ahora se declaran como dependencias directas. No se añadieron servicios
externos, JavaScript del cliente ni un generador de páginas.

## Compilación y Evergreen

```sh
npm run check
```

Frontend y backend compilan con el tablero integrado; también se generó el CSS.
Por indicación del usuario, no se añadieron tests ni se ejecutaron pruebas del
tablero. La revisión de reglas, autorización e importación fue estática; el uso
real en el navegador y la importación de datos siguen sin comprobarse.

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
