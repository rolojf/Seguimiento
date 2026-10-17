import { createHash, randomBytes, randomUUID } from 'node:crypto';
import { chmod, link, lstat, mkdir, rename, rm, writeFile } from 'node:fs/promises';
import { join } from 'node:path';
import { fileURLToPath } from 'node:url';

const KEY_BYTES = 32;
const DIRECTORY_MODE = 0o700;
const FILE_MODE = 0o600;
const MODE = Object.freeze({ CREATE: 'create', REPLACE: 'replace' });
const PROJECT_DIR = fileURLToPath(new URL('..', import.meta.url));

function makeKey() {
  const key = randomBytes(KEY_BYTES).toString('base64url');
  const hash = createHash('sha256').update(key, 'utf8').digest('hex');

  return { key, hash };
}

function envSource(userOne, userTwo) {
  return `module Env exposing (userOneKeyHash, userTwoKeyHash)


userOneKeyHash : String
userOneKeyHash =
    "${userOne.hash}"


userTwoKeyHash : String
userTwoKeyHash =
    "${userTwo.hash}"
`;
}

async function configure(mode) {
  const localDir = join(PROJECT_DIR, '.local');
  const accessPath = join(localDir, 'access.txt');
  const envPath = join(PROJECT_DIR, 'src', 'Env.elm');
  const suffix = randomUUID();
  const accessTemp = join(localDir, `.access-${suffix}.tmp`);
  const envTemp = join(PROJECT_DIR, 'src', `.Env-${suffix}.tmp`);

  await mkdir(localDir, { recursive: true, mode: DIRECTORY_MODE });

  // Refuse a symlink so keys stay inside this project's private directory.
  if (!(await lstat(localDir)).isDirectory()) {
    throw new Error('La ruta .local debe ser un directorio propio del proyecto.');
  }

  await chmod(localDir, DIRECTORY_MODE);

  const userOne = makeKey();
  const userTwo = makeKey();

  try {
    // Prepare both files before publishing either; raw keys never enter Env.elm.
    await writeFile(accessTemp, `Rolo: ${userOne.key}\nDot: ${userTwo.key}\n`, {
      flag: 'wx',
      mode: FILE_MODE,
    });
    await writeFile(envTemp, envSource(userOne, userTwo), {
      flag: 'wx',
      mode: FILE_MODE,
    });

    if (mode === MODE.REPLACE) {
      await rename(accessTemp, accessPath);
    } else {
      // link creates the destination exclusively, including if it is a symlink.
      await link(accessTemp, accessPath);
    }

    // Keep the recoverable keys if this final step fails.
    await rename(envTemp, envPath);

    return { accessPath, envPath };
  } finally {
    await Promise.allSettled([
      rm(accessTemp, { force: true }),
      rm(envTemp, { force: true }),
    ]);
  }
}

async function main() {
  const args = process.argv.slice(2);

  if (args.length === 1 && args[0] === '--help') {
    console.log('Uso: node scripts/setup-access.mjs [--replace]\nGenera claves locales para Rolo y Dot. --replace renueva las existentes.\nGuarda las claves en .local/access.txt y sus hashes SHA256 en src/Env.elm.');
    return;
  }

  if (args.length > 1 || (args.length === 1 && args[0] !== '--replace')) {
    throw new Error('Opción desconocida. Usa --help.');
  }

  const mode = args[0] === '--replace' ? MODE.REPLACE : MODE.CREATE;
  const { accessPath, envPath } = await configure(mode);

  console.log(`Claves locales: ${accessPath}\nHashes: ${envPath}`);
}

main().catch((error) => {
  if (error.code === 'EEXIST') {
    console.error('Las claves ya existen en .local/access.txt. Usa --replace para regenerarlas.');
  } else if (error.code) {
    console.error(`No se completó la configuración (${error.code}). Revisa .local/ y src/. Si se creó .local/access.txt, conserva ese archivo y vuelve a ejecutar con --replace.`);
  } else {
    console.error(error.message);
  }

  process.exitCode = 1;
});
