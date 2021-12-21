# Installation

## Stack

In case you don't have Stack installed in your system, you should visit
[Stack's](https://docs.haskellstack.org/en/stable/install_and_upgrade/) website
and follow the appropriate installation method for your OS. If you are on
Linux or macOS, you will be provided with a shell command, while if you are on
Windows a regular installer is available.

## Clone the starter project

```bash
git clone https://github.com/fjvallarino/monomer-starter.git <your-app-name>
```

## Libraries: SDL2 and GLEW

### macOS

In case you don't have Homebrew installed, visit [Homebrew](https://brew.sh)

```bash
brew install sdl2
brew install glew
```

#### Notes

Currently Monomer fails to compile on M1 Macs. This issue should be fixed when
GHC 9.2 is released, since it will include a native code generator for that CPU.

Until that time, it has been [reported](https://github.com/fjvallarino/monomer/issues/1#issuecomment-914866110)
that building for x86 and running the application in the Rosetta shell works
well.

### Linux

#### Debian/Ubuntu

```bash
sudo apt-get install libsdl2-dev
sudo apt-get install libglew-dev
```

##### Ubuntu notes

It seems there are issues when installing `libsdl2-dev` on some Ubuntu versions.
These issues can usually be solved by running `sudo aptitude install libsdl2-dev`
and choosing one of the suggested actions, although doing this is a bit more
dangerous than a regular package install.

Aptitude can be installed with `sudo apt-get install aptitude`.

#### Arch

```bash
pacman -S sdl2
pacman -S glew
```

#### Fedora

```bash
sudo dnf install SDL2-devel glew-devel
```

### Windows

Inside your project's directory:

```bash
stack setup
stack exec -- pacman -S mingw-w64-x86_64-pkg-config
stack exec -- pacman -S mingw-w64-x86_64-SDL2
stack exec -- pacman -S mingw-w64-x86_64-freeglut
stack exec -- pacman -S mingw-w64-x86_64-glew
```

#### Notes

If these steps fail with a message similar to `"invalid or corrupted database"`
or `"signature from ... is unknown trust"`, you may have an old MinGW installed
by a previous version of Stack. At some point the MinGW project changed their
certificates and, even if it should be possible to upgrade them manually, it is
not an easy process (and I have not had luck with it).

Stack installs its files in two locations:

- `%STACK_ROOT%` (usually `C:\sr`, unless modified)
- `%LOCALAPPDATA%\Programs\stack`

The second location is the one that contains MinGW. Removing
`%LOCALAPPDATA%\Programs\stack` and running the above steps again is usually
enough to get a working environment.

If this does not work, removing `%STACK_ROOT%` and reinstalling Stack may be
required.

## Build the project

Inside your project's directory:

```bash
stack build
```

#### Linux notes

One case has been reported where an _"Unable to make GL context current"_ error
ocurred on application startup. This seems to be a driver issue, and it's not
something that can be fixed from the library.

As a workaround, an application configuration option called
`appRenderOnMainThread` is available. It can be added to the `config` list of
the starter application or to the corresponding section of any of the examples.

## Build the examples included with the library

In case you want to test the examples the library provides, you need to clone
the library itself with:

```bash
git clone https://github.com/fjvallarino/monomer.git
```

Then, inside the cloned directory, build the project with:

```bash
stack build
```

In case you have not followed the instructions for the starter project, you
still need to install the [dependencies](#libraries-sdl2-and-glew).

### Running the examples

Inside the cloned directory, you can run each of the examples with `stack run`:

```bash
stack run todo
stack run books
stack run ticker
stack run generative
```

## Development mode

Since compilation times can be annoying, I personally prefer to rely on `ghcid`
for a nicer development experience. First, you need to install it:

```bash
stack install ghcid
```

Then, inside your project's directory:

```bash
ghcid
```

If you use Visual Studio Code, you can also use this very nice extension:

```
https://marketplace.visualstudio.com/items?itemName=ndmitchell.haskell-ghcid
```

Once installed, pressing `Ctrl-Shift-P` will allow you to invoke the
`Start Ghcid` command. You can also run `ghcid` on the command line directly.

With this you will be running your application in interpreted mode (`ghcid`
under the hood uses `ghci`), allowing you to make changes and test them almost
immediately.

## Notes for macOS users

If you have a discrete GPU, and you'd rather have your application use the
integrated GPU, you may want to copy `Info.plist` into the bin directory the
build command shows. It will be something like (replacing 8.10.7 with the
corresponding GHC version):

```bash
$HOME/.../monomer-starter/.stack-work/install/x86_64-osx/......../8.10.7/bin
```

By doing this, macOS will default to using the integrated GPU instead of the
discrete one. This provides slightly faster startup, better battery life and it
will work fine for most UIs, although you may notice a bit of lag when resizing
windows.

Further, you may want to do this for your local stack install directory, since
you'll be using ghcid and Info.plist needs to be in the directory where the
executable is located. The path is usually:

```bash
$HOME/.local/bin
```

You will also need to add it to the directory where `ghc`/`ghci` are installed:

```bash
$HOME/.stack/programs/x86_64-osx/ghc-8.10.7/lib/ghc-8.10.7/bin/
```

Reference: http://supermegaultragroovy.com/2016/12/10/auto-graphics-switching

### ghcid

The sample project includes custom .ghci and .ghcid files. The most important
change in .ghci is for Mac users, since macOS does not allow graphics setup to
happen outside the main thread (by default `ghci` spawns a thread for user
code). If you create your custom GHCi config and run into issues, check if you
are providing the necessary flags.

<br/>

[Next: Basics](01-basics.md)
