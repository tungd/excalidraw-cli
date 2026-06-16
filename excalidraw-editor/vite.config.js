import { defineConfig } from "vite";
import react from "@vitejs/plugin-react";

export default defineConfig({
  plugins: [react()],
  base: "./",
  build: {
    outDir: "dist",
    assetsDir: ".",
    // Inline everything so we get a minimal set of files for crunch
    // CSS will still be extracted but we keep JS as a single bundle
  },
  server: {
    proxy: {
      "/api": "http://localhost:3001",
    },
  },
});
