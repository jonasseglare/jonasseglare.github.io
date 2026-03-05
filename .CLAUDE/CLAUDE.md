# Om det här projektet

Det här är ett frontend-projekt för en personlig webbplats, skrivet med Squint.

## Kodstruktur

### Kataloger och filer

| Namn     | Beskrivning                                         |
|----------|-----------------------------------------------------|
| `build/` | Squint matar ut byggen hit.                         |
| `src/`   | Källkod, mest Squint                                |
| `docs/`  | Bygget, som publiceras på Github pages, hamnar här. |

Observera att katalogen måste heta `docs/` för att Github ska hitta det.

### Teknikval

* Squint, en ClojureScript-dialekt
* Vite, ett system som bygger js->js och paketerar
* NPM, för beroenden på JavaScript

### Så här fungerar det

I grunden ett NPM-projekt, se `package.json`, som byggs med Vite, se `vite.config.js`. Vi använder Clojurescript-dialekten Squint, se `bb.edn` för byggkommandon för Squint och `squint.edn` för konfigurationen. Squint bygger till `build/`. När Squint har byggts paketeras det av Vite och exporteras till `docs/`

En övergripande `Makefile` är tänkt att knyta ihop allt och tillhandahålla kommandon på hög nivå.
