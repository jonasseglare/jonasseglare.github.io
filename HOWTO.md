# HOWTO

## Lägga till bloggsidor (eller andra HTML-sidor) som behöver den kompilerade JS-koden

Vite processar `index.html` i projektroten som entry point och skriver om
script-referenser till hashade filnamn i `docs/`. För att bloggsidor ska
få samma behandling måste de läggas till som Vite entry points.

### Steg

1. Generera HTML-filerna (t.ex. från markdown) *före* Vite-bygget. Placera dem
   i projektroten eller en underkatalog, t.ex. `blog/post1.html`.

2. Inkludera samma script-tagg som i `index.html`:

   ```html
   <script type="module" src="/build/js/index.mjs"></script>
   ```

3. Lägg till filerna som inputs i `vite.config.js`:

   ```js
   import { resolve } from 'path';

   export default defineConfig({
     build: {
       outDir: 'docs',
       rollupOptions: {
         input: {
           main: resolve(__dirname, 'index.html'),
           post1: resolve(__dirname, 'blog/post1.html'),
           // lägg till fler sidor här
         },
       },
     },
   });
   ```

4. Kör `make build`. Vite skriver om script-referenserna i alla listade
   HTML-filer och lägger dem i `docs/`.

### Varför inte `public/`?

Filer i `public/` kopieras oförändrade av Vite. Eftersom JS-filnamnet
innehåller en content-hash som ändras vid varje build kan du inte hårdkoda
sökvägen i HTML-filer som ligger i `public/`. Använd entry points istället.
