/// <reference types="vitest" />
import { defineConfig } from 'vite';
import { visualizer } from 'rollup-plugin-visualizer';

export default defineConfig({
  test: {
    include: ['docs/**/**test.mjs', 'docs/**/**test.jsx'],
  },
  plugins: [visualizer({ open: false, filename: 'bundle-visualization.html' })],
});
