```bash
$ ./bmm -h
bmm -- a terminal application for managing bonelab mods
  -h, --help      Show help
      --token     Mod.io OAuth 2 Token
  -p, --path      Bonelab mod folder path
  -u, --update    Download subscribed mods from Mod.io, and update them if they are out of date
  -s, --subscribe Subscribe to mods that are on your disk, may not work for all mods
                  Intended for subscribing to mods gotten from fusion
```

You have to kinda delete all of your mods and reinstall them with `bmm -u`.

# todo
- generate package manager db without reinstalling all mods
