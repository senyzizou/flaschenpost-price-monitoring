# Flaschenpost Price Monitoring

Das Monitoring erfasst Preise und Sortimente für w36 Berlin-Kreuzberg. Der
R-Collector behält auch alkoholfreie Produkte. Die Hauptanalyse verwendet
ABV > 0,5 % vol, die Sensitivitätsanalyse ABV ≥ 0,5 % vol. Die bestehenden
Flags im versionierten Store behalten ihre Bedeutung. Die belegten
Korrekturen vom 25. November 2026 gelten in der neuen Analysesicht ab
dem angegebenen Stichtag für die dokumentierte Produktversion. Rohwerte
und frühere Auswertungen bleiben erhalten.

## Offline prüfen

Die Laufzeit ist R 4.6.0. Die Paketversionen stehen zentral in
`preparation/config/ubuntu_packages.csv`. Die Tests installieren keine
Pakete und verwenden keine Händler- oder UKEbox-Verbindung.

```sh
Rscript --vanilla scripts/check_release.R
Rscript --vanilla tests/test_ci_synthetic.R
Rscript --vanilla tests/test_versioned.R
Rscript --vanilla tests/test_remote_state.R
Rscript --vanilla tests/test_daily_recovery.R
Rscript --vanilla scripts/daily_flaschenpost.R --plan
```

`REPOSITORY_FILES.txt` ist die Übernahmeliste. Roharchive, alte
Projektstände und die historischen Tests mit Originaldaten liegen im
geschützten Forschungsarchiv.

## Historischen Startbestand vorbereiten

Der private Startbestand enthält die unveränderten Commits c0001 bis c0005,
s0001 und s0002, die fünf Erhebungsprotokolle und 419 Rohantworten. Er
bleibt außerhalb des Repositorys. Der Abgleich verwendet sowohl das
Dateiinventar als auch die Hashes der Originalprotokolle und Store-Manifeste.

```sh
Rscript --vanilla scripts/historical_archive.R --verify "$BUNDLE"
Rscript --vanilla scripts/historical_archive.R --mock "$BUNDLE"
```

Die archivabhängigen Tests bleiben im Originalpilot ausführbar, etwa
`Rscript --vanilla tests/test_real_store_integrity.R` aus dessen
Projektwurzel. Dort bleiben auch die ursprünglichen Auswertungen erhalten.

Für UKEbox die Elternverzeichnisse `APM/flaschenpost` bereitstellen und den
berechtigten App-Zugang für den WebDAV-Zielbereich abklären. Nach Freigabe
`FP_ARCHIVE_TRANSFER_ENABLED=true` ausschließlich für die beauftragte
Übertragung setzen. `--probe` prüft die Schreib- und Rücklesesemantik.
`--seed "$BUNDLE"` überträgt den geprüften Startbestand und verifiziert
die Wiederherstellung in einem neuen lokalen Verzeichnis. Beide Befehle
verwenden dieselben drei UKEbox-Umgebungsvariablen wie der Tageslauf.

## Tageslauf abnehmen

1. Auf GitHub zuerst den manuellen Workflow im Modus `offline` ausführen.
   Der Workflow installiert die gepinnten Paketversionen und prüft den
   eigenständigen Checkout.
2. UKEbox testen und den historischen Startbestand nach Freigabe übertragen.
3. Die GitHub Secrets `FP_UKEBOX_WEBDAV_URL`, `FP_UKEBOX_USERNAME` und
   `FP_UKEBOX_APP_PASSWORD` hinterlegen. Für das genehmigte Betriebsfenster
   die Betriebsfreigabe einmal dokumentieren und die Repository-Variablen
   `FP_LIVE_ENABLED=true` und `FP_VALID_FROM` setzen. Für eine befristete
   Freigabe zusätzlich `FP_VALID_UNTIL` als letzten freigegebenen Tag setzen.
   Datumswerte verwenden das Format YYYY-MM-DD und beziehen sich auf
   Berliner Kalendertage. Bei Widerruf `FP_LIVE_ENABLED` auf `false` setzen.
4. Auf dem Standardbranch einen manuellen Lauf im Modus `collect`
   abnehmen. Den auskommentierten Zeitplan für 10:15 Uhr Europe/Berlin
   erst nach erfolgreicher Abnahme aktivieren.

Jeder Versuch wird auf UKEbox reserviert. Bestätigte Rohantworten werden
mit Prüfsumme und Zeitpunkt im selben WebDAV-Bereich gespeichert. Aus
vollständigen Antworten kann derselbe Workflow-Stand einen unterbrochenen
Lauf offline rekonstruieren. Journale und Zwischenstände bleiben im
geschützten WebDAV-Bereich. Ein gespeicherter Zwischenstand enthält
die exakten Dateien des zugehörigen Store-Commits und sein Rohdatenmanifest.
Der Commit-Marker wird erst nach Prüfung der Rohantworten veröffentlicht.
Ein Wiederholungslauf prüft auch bei vorhandenem Commit das Archiv.
Antworten, deren Speicherung beim Abbruch noch ausstand, gelten als
unbestätigt. Ein unvollständiger Versuch ist kein Tagesergebnis.

Der Workflow und der WebDAV-Writer-Lock serialisieren die Läufe. Bei einem
verbliebenen Lock zuerst den Besitzer und dessen Laufstatus prüfen.
Bei einem Versionswechsel einen unterbrochenen Lauf ohne Zwischenstand
zuerst mit seiner ursprünglichen Workflow-Revision wiederaufnehmen.
Ein späterer Lauf rekonstruiert nur gespeicherte historische Antworten. Neue
Händlerabfragen gelten immer für den aktuellen Berliner Tag.

### Archivierten Versuch offline neu bewerten

Nach ausdrücklicher Freigabe auf `main` den manuellen Modus `reassess`
mit Berliner Datum, Versuchs-ID, ursprünglichem Commit-SHA und der
offline bestätigten SKU-Zahl auslösen. Der Workflow prüft die gespeicherte
Revisionsbindung und unveränderte Collector-Dateien. Er liest die
gespeicherten Antworten aus UKEbox und nutzt die aktuellen
Abdeckungsprüfungen. Der neue Tagesbeleg dokumentiert beide Revisionen und
den autorisierenden Workflow-Lauf. Die ursprüngliche Ablehnung und das
Rohjournal bleiben erhalten. Die Neubewertung stellt keine Anfragen an
Flaschenpost.
