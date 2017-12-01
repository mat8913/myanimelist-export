# myanimelist-export

Export anime or manga lists from MyAnimeList in XML format.  Uses the web
interface, because the malappinfo API exports in a slightly different format
which causes problems on importing.

## Executable

For convenience, an executable is provided which will export lists and save them
to local files. In order to run it, a configuration file must be created first.

On GNU/Linux: ~/.config/myanimelist-export.yaml

On Windows: %APPDATA%\myanimelist-export.yaml

```yaml
username: username
password: password
animeXmlPath: /path/to/save/anime.xml
mangaXmlPath: /path/to/save/manga.xml
```

Leave out the `animeXmlPath` or `mangaXmlPath` field if you don't wish to export
the respective list.
