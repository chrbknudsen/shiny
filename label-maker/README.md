# Nusse Label Maker

Første etape af en Shiny-app til etiketter med:

- størrelsesprofiler fra `config.yml`, herunder standardmålet 58 × 74 mm;
- live preview;
- fire tilfældigt genererede motiver;
- vedvarende billedbibliotek;
- upload af PNG, JPEG, WebP og SVG;
- klikbart galleri med uploadede og gemte genererede billeder;
- skrivebeskyttet offentlig adgang med sessionsbaseret administratoroplåsning;
- separat, reversibel sletning fra serverens kommandolinje;
- separate fontvælgere til titel og data/bundtekst;
- PNG-eksport i 300 eller 600 dpi;
- A4-PDF med 1 til det maksimalt mulige antal etiketter i fysisk størrelse.

## Pakker

Installer de nødvendige R-pakker:

```r
install.packages(
  c(
    "shiny",
    "DBI",
    "RSQLite",
    "magick",
    "rsvg",
    "uuid",
    "yaml",
    "sysfonts",
    "showtext"
  )
)
```

`magick` og `rsvg` kan desuden kræve deres almindelige
systembiblioteker på Linux.

Skrifterne følger med projektet som TTF-filer og registreres af appen ved
opstart. Der skal derfor ikke installeres skrifter i operativsystemet.

## Størrelser og skrifter

Størrelserne og skrifterne står i `config.yml`. Der kan tilføjes så mange
størrelsesprofiler som ønsket:

```yaml
sizes:
  standard:
    label: "Standard - 58 x 74 mm"
    width_mm: 58
    height_mm: 74

  krydderiglas:
    label: "Krydderiglas - 45 x 35 mm"
    width_mm: 45
    height_mm: 35
```

Skriftfiler lægges i `fonts/`. Hver familie under `fonts.families` bliver en
valgmulighed i appens to skriftvælgere:

```yaml
fonts:
  directory: "fonts"
  default_display: "dejavu_serif"
  default_typewriter: "dejavu_mono"

  families:
    dejavu_serif:
      label: "DejaVu Serif"
      family: "label-dejavu-serif"
      regular: "DejaVuSerif.ttf"
      bold: "DejaVuSerif-Bold.ttf"

    dejavu_mono:
      label: "DejaVu Sans Mono"
      family: "label-dejavu-mono"
      regular: "DejaVuSansMono.ttf"
      bold: "DejaVuSansMono-Bold.ttf"
```

`regular` er obligatorisk. `bold`, `italic` og `bolditalic` er valgfrie;
manglende varianter falder tilbage til regular. Filnavne er relative til
`fonts.directory`, mens absolutte stier også kan bruges. TTF, TTC og OTF
understøttes. Hvis standardvalgene udelades, bruges den første familie i
kataloget.

De medfølgende DejaVu Serif, DejaVu Sans og DejaVu Sans Mono er valgt som et
transportabelt udgangspunkt. Andre licenserede TTF-filer kan kopieres til
`fonts/` og tilføjes som nye poster under `fonts.families`. De bliver synlige
i appen efter genstart.

Den første profil bliver standardvalget. Miljøvariablen
`LABEL_CONFIG_FILE` kan pege på en alternativ konfigurationsfil.

## Start appen

Fra mappen, der indeholder projektmappen:

```r
shiny::runApp("label-maker")
```

Eller fra selve projektmappen:

```r
shiny::runApp()
```

## Vedvarende billedbibliotek

Appen læser placeringen fra miljøvariablen `LABEL_DATA_DIR`.

Eksempel:

```r
Sys.setenv(
  LABEL_DATA_DIR = "/var/lib/label-maker"
)

shiny::runApp("label-maker")
```

Shiny-processen skal have læse- og skriverettigheder til mappen.

Hvis miljøvariablen ikke er sat, bruger appen:

```text
label-maker/label-data/
```

Det er praktisk til lokal afprøvning. På en server bør data ligge
uden for appmappen, så de ikke forsvinder ved en ny installation.

Mappen får denne struktur:

```text
LABEL_DATA_DIR/
├── images/
├── thumbnails/
├── trash/
│   ├── images/
│   └── thumbnails/
└── library.sqlite
```

Uploadede filer gemmes ikke under deres oprindelige filnavne.
Appen giver dem et UUID og opbevarer det viste navn og mærkater i
SQLite-databasen.

## Beskyt upload på en offentlig server

Biblioteket er skrivebeskyttet, medmindre serveren har miljøvariablen
`LABEL_ADMIN_TOKEN`. Nøglen skal være mindst 32 bytes lang; brug helst en
tilfældig nøgle på 64 hex-tegn:

```bash
openssl rand -hex 32
```

Gem nøglen som en hemmelig miljøvariabel på serveren. Den må ikke stå i
`config.yml`, kildekoden eller et Git-repository. Til lokal brug kan den stå i
en `.Renviron`-fil, som projektets `.gitignore` allerede udelukker:

```text
LABEL_ADMIN_TOKEN=indsæt-den-lange-tilfældige-nøgle-her
```

Genstart R-sessionen og appen efter ændringen. Fanen **Adgang** kan derefter låse upload og
lagring af genererede motiver op i den aktuelle Shiny-session. **Lås
skriveadgang** låser den igen. En ny browsersession begynder altid låst.

Hvis nøglen mangler eller er for kort, kan alle fortsat skabe og hente
etiketter samt bruge eksisterende biblioteksbilleder, men ingen kan føje
billeder til biblioteket. Begge skriveveje kontrolleres på serveren; de er
ikke kun skjult i brugerfladen.

Kør altid den offentlige app bag HTTPS. Et maskeret kodeordsfelt skjuler kun
tegnene på skærmen; HTTPS beskytter nøglen under transport. OWASP anbefaler
TLS på alle sider i en webapplikation:
<https://cheatsheetseries.owasp.org/cheatsheets/Transport_Layer_Security_Cheat_Sheet.html>.

Denne løsning er en enkel administratorlås til ét fælles bibliotek. Hvis der
senere skal være flere administratorer, individuelle konti eller
rettighedsstyring, bør egentlig login-infrastruktur tilføjes foran appen.

## Slet og gendan billeder uden for appen

Sletning findes kun i kommandolinjeværktøjet `image-admin.R`. Kør det på den
maskine, hvor appens `LABEL_DATA_DIR` ligger:

```bash
cd label-maker

# Vis aktive billeder og deres fulde id
Rscript image-admin.R list

# Kontrollér valget; denne kommando ændrer intet
Rscript image-admin.R delete 00000000-0000-0000-0000-000000000000

# Flyt derefter billedet til papirkurven
Rscript image-admin.R delete 00000000-0000-0000-0000-000000000000 --yes

# Vis slettede billeder
Rscript image-admin.R list --deleted

# Gendan et billede
Rscript image-admin.R restore 00000000-0000-0000-0000-000000000000
```

Brug id'et fra `list`; værktøjet accepterer ikke forkortede id'er eller navne.
Sletning er reversibel: databaseposten markeres som slettet, og original samt
miniature flyttes til `trash/`. De vises ikke i appen og ligger ikke længere i
den mappe, som appen offentliggør miniaturer fra. Værktøjet har med vilje ingen
kommando til permanent tømning af papirkurven. Åbne appsessioner opdaterer
biblioteket automatisk inden for få sekunder.

## Uploadbehandling

Tilladte filtyper:

- PNG
- JPEG
- WebP
- SVG

Grænsen er 10 MB. Billeder må højst være 12.000 pixels på en led
og 50 megapixels i alt.

Appen:

1. læser første frame;
2. anvender EXIF-orientering;
3. fjerner metadata;
4. konverterer billedet til PNG;
5. laver en separat miniature.

SVG bliver rasteriseret ved upload og serveres derfor ikke som
aktiv SVG-kode.

## Smoke-test

Kør fra projektmappen:

```r
source("smoke-test.R")
```

Testen bruger en midlertidig datamappe og:

- kontrollerer administratornøglens længde, sammenligning og afvisning;
- kontrollerer motivvælgerens værdier;
- indlæser og validerer `config.yml`;
- kontrollerer, at de konfigurerede skriftfiler findes og registreres;
- kontrollerer fontkataloget, standardvalgene og opslag fra font-id til familie;
- kontrollerer håndtering af et tomt billedbibliotek;
- kontrollerer A4-kapacitet og placering af fire etiketter;
- opretter SQLite-biblioteket;
- genererer og gemmer et motiv;
- genfinder motivet i databasen;
- sletter motivet reversibelt, kontrollerer papirkurven og gendanner det;
- kontrollerer automatisk migrering af et eksisterende bibliotek;
- eksporterer en normal og en lav PNG samt en A4-PDF med fire etiketter;
- kontrollerer, at filerne er oprettet og ikke er tomme.

## Første etapes afgrænsning

Følgende er ikke med endnu:

- omdøbning i billedbiblioteket;
- permanent tømning af papirkurven;
- flere grunddesigns;
- placering og rotation af uploadede billeder;
- individuelle brugerkonti og private biblioteker.
