const esbuild = require('esbuild');
const glob = require('glob');
const path = require('path');
const { NodeModulesPolyfillPlugin } = require('@esbuild-plugins/node-modules-polyfill');
const polyfill = require('@esbuild-plugins/node-globals-polyfill');

const production = process.argv.includes('--production');
const watch = process.argv.includes('--watch');

async function main() {
  const ctx = await esbuild.context({
    entryPoints: ['extension2.js'],
    bundle: true,
    format: 'cjs',
    minify: production,
    sourcemap: !production,
    sourcesContent: false,
    platform: 'browser',
    outfile: 'web_ide.js',
    external: ['vscode'],
    logLevel: 'silent',
    // Node.js global to browser globalThis
    define: {
      global: 'globalThis'
    },

    plugins: [
      NodeModulesPolyfillPlugin(),
      polyfill.NodeGlobalsPolyfillPlugin({
        process: true,
        buffer: true
      })
    ]
  });
  if (watch) {
    await ctx.watch();
  } else {
    await ctx.rebuild();
    await ctx.dispose();
  }
}

main().catch(e => {
  console.error(e);
  process.exit(1);
});
