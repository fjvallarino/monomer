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

Currently, Monomer fails to compile on M1 Macs.

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

## Build the project

Inside your project's directory:

### macOS / Linux

```bash
stack build
```

### Windows

```bash
stack build --flag regex-posix:_regex-posix-clib
```

## Build the examples included with the library

In case you want to test the examples the library provides, you need to clone
the library itself with:

```bash
git clone https://github.com/fjvallarino/monomer.git
```

Then, inside the cloned directory, build the project with:

### Linux and macOS

```bash
stack build
```

### Windows

If you're using Windows, you have to do some extra steps and provide additional
flags as in the starter project:

```bash
stack setup
stack exec -- pacman -S mingw-w64-x86_64-pkg-config
stack exec -- pacman -S mingw-w64-x86_64-SDL2
stack exec -- pacman -S mingw-w64-x86_64-freeglut
stack exec -- pacman -S mingw-w64-x86_64-glew

stack build --flag regex-posix:_regex-posix-clib
```

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
build command shows. It will be something like:

```bash
$HOME/.../monomer-starter/.stack-work/install/x86_64-osx/......../8.10.4/bin
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

Reference: http://supermegaultragroovy.com/2016/12/10/auto-graphics-switching

### ghcid

The sample project includes custom .ghci and .ghcid files. The most important
change in .ghci is for Mac users, since Mac does not allow rendering to happen
outside the main thread (by default `ghci` spawns a thread for user code). If
you create your custom GHCi config and run into issues, check if you are
providing the necessary flags.

<br/>

[Next: Basics](01-basics.md)
