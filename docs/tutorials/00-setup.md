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

In case you don't have Homebrew installed, visit [Homebrew](https://brew.sh).

```bash
brew install sdl2
brew install glew
```

#### Notes: pkg-config

You may need to install `pkg-config`, in case it was not previously installed:

```bash
brew install pkg-config
```

#### Notes: M1

The standard build process currently fails on M1 Macs. This issue should be
fixed when support for `GHC 9.2` is added to `stack`, since that version of GHC
includes a native code generator for M1 processors.

It has been [reported](https://github.com/fjvallarino/monomer/issues/1) that:

- Building for x86 and running the application in the Rosetta shell works well.
- Applying some workarounds, mentioned in the same issue, the build can work on
  M1.

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
sudo dnf install SDL2-devel
sudo dnf install glew-devel
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

The second location is the directory that contains MinGW. Removing
`%LOCALAPPDATA%\Programs\stack` and running the above steps again is usually
enough to get a working environment. If this does not work, removing
`%STACK_ROOT%` and reinstalling Stack may be required.

If the previous steps did not fix the issue, updating the keyring with the
following commands and building again may help:

```bash
stack exec -- pacman -S msys2-keyring
stack exec -- pacman -Syu
```

## Build the project

Inside your project's directory:

```bash
stack build
```

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

#### Notes

Monomer uses a secondary thread for rendering to be able to redraw the content
while the user resizes the window. In some configurations, mainly with NVIDIA
drivers on Linux, setting up an OpenGL context in a secondary thread fails.

If this happens, Monomer will try to fall back to rendering in the main thread
and warn about the situation with a message similar to:

```
Setup of the rendering thread failed: Unable to make GL context current
Falling back to rendering in the main thread.
```

Besides having the content stretched while resizing the window (i.e. not
dinamically resized), there are no other differences between the threaded and
non-threaded modes.

## Development mode

Since compilation times can be annoying, I personally prefer to rely on
[ghcid](https://github.com/ndmitchell/ghcid) for a nicer development experience.
First, you need to install it:

```bash
stack install ghcid
```

Then, inside your project's directory:

```bash
ghcid
```

If you use Visual Studio Code, you can also use this [very nice
extension](https://marketplace.visualstudio.com/items?itemName=ndmitchell.haskell-ghcid).

Once installed, pressing `Ctrl-Shift-P` will allow you to invoke the
`Start Ghcid` command. You can also run `ghcid` on the command line directly.

With this you will be running your application in interpreted mode (`ghcid`
under the hood uses `ghci`), allowing you to make changes and test them almost
immediately.

Note: when a file is saved, a new instance of the application will be in a new
window. The previous window needs to be closed manually.

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
