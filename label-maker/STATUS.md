# Status: første etape

Dato: 2026-09-20

## Tredje udbygning

- Billedbiblioteket er nu skrivebeskyttet for almindelige besøgende.
- Upload og lagring af genererede motiver kræver administratoroplåsning.
- Administratornøglen læses kun fra `LABEL_ADMIN_TOKEN` og skal være mindst
  32 bytes; den ligger ikke i `config.yml` eller i browserens HTML.
- Oplåsningen gælder kun den aktuelle Shiny-session og kan låses igen manuelt.
- Begge skrivehandlinger kontrollerer tilladelsen på serversiden.
- Et nyt `image-admin.R` håndterer sletning uden for webappen.
- Sletning kræver et fuldt billed-id og en særskilt `--yes`-bekræftelse.
- Slettede originaler og miniaturer flyttes til en papirkurv uden for det
  aktive bibliotek og kan gendannes.
- Eksisterende SQLite-biblioteker får automatisk feltet `deleted_at` ved
  næste opstart.

## Anden udbygning

- Lokale TTF-filer ligger i `fonts/` og vælges via `config.yml`.
- Titel- og bundtekstskrift kan vælges direkte og uafhængigt i appen.
- Alle familier under `fonts.families` bliver automatisk valgmuligheder.
- Skrifterne registreres ved opstart og bruges ens i preview, PNG og PDF.
- Taludtrykket nederst i det pseudovidenskabelige diagram er fjernet.
- Billedbiblioteket vises som et klikbart miniaturegalleri.
- Den medfølgende DejaVu Serif bruges som klassisk etiketskrift.
- Bundtekst og datafelter bruger den medfølgende DejaVu Sans Mono.
- Titel og bundtekst skaleres og holdes inden for rammen på lave etiketter.
- Etiketstørrelser defineres i `config.yml` og vælges i appen.
- PDF-eksporten er altid A4 og kan indeholde fra én til det maksimale
  antal etiketter, der fysisk kan være på arket.
- Fire etiketter placeres som et centreret 2 x 2-layout; øvrige antal
  får automatisk et centreret layout.

## Rettet efter test på målmaskinen

- Motivvælgeren returnerer nu motivnummeret i stedet for motivets navn.
- Motivtypen valideres uden advarsler om konvertering til `NA`.
- En tom billedsamling giver ikke længere fejl i miniaturevisningen.
- Smoke-testen kontrollerer både motivvælgerens mapping og tomme billed-id'er.

## Implementeret

- Shiny-editor med live preview.
- Standardformat 58 × 74 mm.
- Redigering af titel, undertekst, dato, batch, kode og bundtekst.
- Fire godkendte tilfældige motiver med reproducerbart seed.
- Vedvarende fælles billedbibliotek i SQLite.
- Upload af PNG, JPEG, WebP og SVG.
- Rensning, auto-orientering, rasterisering og konvertering til PNG.
- UUID-filnavne og separate miniaturebilleder.
- Søgning i biblioteket på navn og mærkater.
- Genbrug af uploadede billeder.
- Lagring og genbrug af genererede motiver.
- Sort-hvid-visning samt valg mellem tilpasning og beskæring.
- PNG-eksport i 300 eller 600 dpi.
- A4-PDF med valgbart antal etiketter i fysisk korrekte mål.
- Konfiguration af persistent datamappe med `LABEL_DATA_DIR`.
- README og automatiseret smoke-test.
- Administratorlås til alle ændringer af billedbiblioteket.
- Kommandolinjeværktøj til reversibel sletning og gendannelse.

## Kontrolleret

- Alle R-filer er kontrolleret for balancerede parenteser, klammer,
  strenge og kommentarer efter denne udbygning.
- `config.yml` er indlæst og kontrolleret med en YAML-parser.
- A4-layoutet med fire etiketter er visualiseret og kontrolleret som et
  centreret 2 x 2-layout.
- De responsive tekstområder er beregnet for både standardformatet og den
  lave profil på 74 x 45 mm.
- Den tidligere version har fået udført motivgeneratorerne og den samlede
  etiketrendering i R.

## Ikke kørt i arbejdsmiljøet

Den fulde Shiny-app og SQLite/upload-smoke-testen er ikke udført her,
fordi arbejdsmiljøet ikke har en almindelig R-installation eller de
nødvendige kompilerede R-pakker. `smoke-test.R` er med i projektet og
kan køres direkte på målmaskinen.

## Næste oplagte trin

- Kør `source("smoke-test.R")` på målmaskinen.
- Start appen og afprøv upload gennem browseren.
- Tilføj eventuelt omdøbning i billedbiblioteket.
- Afprøv administratoroplåsning, upload, sletning og gendannelse på
  målmaskinen.
- Tilføj flere grunddesigns.
