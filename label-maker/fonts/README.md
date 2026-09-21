# Lokale skrifter

Appen registrerer skrifterne i denne mappe ved opstart. Valget sker i
`../config.yml`; filnavnene dér er relative til `fonts.directory`.

Der følger tre familier med:

- `DejaVuSerif*.ttf` til titel og undertekst;
- `DejaVuSans*.ttf` som neutral skrift;
- `DejaVuSansMono*.ttf` til dato, batch, kode og bundtekst.

Alle familier, der defineres under `fonts.families` i `config.yml`, bliver
valgmuligheder i appen. Andre TTF-filer kan lægges i mappen og tilføjes til
kataloget. Angiv altid `regular`. `bold`, `italic` og `bolditalic` er
valgfrie. Brug forskellige id'er og interne `family`-navne til forskellige
familier.

Eksempel:

```yaml
fonts:
  directory: "fonts"
  default_display: "min_titelskrift"
  default_typewriter: "min_maskinskrift"

  families:
    min_titelskrift:
      label: "Min titelskrift"
      family: "min-titelskrift"
      regular: "MinTitelskrift-Regular.ttf"
      bold: "MinTitelskrift-Bold.ttf"

    min_maskinskrift:
      label: "Min maskinskrift"
      family: "min-maskinskrift"
      regular: "MinMaskinskrift-Regular.ttf"
```

Sørg for, at licensen tillader, at skrifterne bruges og distribueres sammen
med appen.
