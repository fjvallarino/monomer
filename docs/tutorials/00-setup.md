# Installation

## Stack

In case you dont have it installed in your system, you should visit
[Stack's](https://docs.haskellstack.org/en/stable/install_and_upgrade/) website
and follow the appropriate installation method for your OS. If you are on
Linux or macOS, you will be provided with a shell command, while if you are on
Windows a regular installer is available.

## SDL2

### macOS

```bash
brew install sdl2
```

In case you don't have Homebrew installed, visit [Homebrew](https://brew.sh)

### Linux

#### Ubuntu

```bash
sudo apt-get install -y libsdl2-dev
```

#### Arch

```bash
pacman -S sdl2
```

### Windows

**Pending, check below url**
https://www.reddit.com/r/haskell/comments/cjv2hh/help_setting_up_sdl2_on_windows/

## Clone the sample project

```bash
git clone https://github.com/fjvallarino/monomer-sample.git
```

## Build the project

```bash
stack build
```

## Development mode

Since compilation times can be annoying, I personally prefer to rely on `ghcid`
for a nicer development experience. First, you need to install it:

```bash
stack install ghcid
```

If you use Visual Studio Code, you can also use this very nice extension:

```
https://marketplace.visualstudio.com/items?itemName=ndmitchell.haskell-ghcid
```

Once installed, pressing `Ctrl-Shift-P` will allow invoking the `Start Ghcid`
command. You can also run `ghcid` on the command line directly.

With this, you will be running your application in interpreted mode (`ghcid`
under the hood uses `ghci`), allowing you to make changes and test them almost
immediately.

## Notes for macOS users

If you have a discrete GPU, you may want to copy Info.plist into the bin directory
the build command shows. It will be something like:

```bash
$HOME/.../monomer-sample/.stack-work/install/x86_64-osx/......../8.10.4/bin
```

By doing this, you will be using the integrated GPU instead of the discrete one.
This provides faster startup, better battery life and it will work just fine for
most UIs.

Further, you may want to do this for your local stack install directory, since
you'll be using ghcid and Info.plist needs to be in the directory where the
executable is located. The path is usually:

```bash
$HOME/.local/bin
```

### ghcid

The sample project includes custom .ghci and .ghcid files. The most important
change in .ghci is for Mac users, since Mac does not allow rendering to happen
outside the main thread (by default `ghci` spawns a thread of user code).
