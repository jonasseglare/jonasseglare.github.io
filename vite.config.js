/// <reference types="vitest" />
import { defineConfig } from 'vite';
import { visualizer } from 'rollup-plugin-visualizer';

export default defineConfig({
  test: {
    include: ['build/**/**test.mjs'],
  },
  build: {
    outDir: 'docs',
  },
  plugins: [visualizer({ open: false, filename: 'bundle-visualization.html' })],
});
