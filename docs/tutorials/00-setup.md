# Installation

## Stack

### Regular Stack installer - All except Apple Silicon

Visit [Stack's](https://docs.haskellstack.org/en/stable/install_and_upgrade/)
website and follow the appropriate installation method for your OS. If you are
on Linux or macOS, you will be provided with a shell command, while if you are
on Windows a regular installer is available.

#### Note: Apple Silicon

The default Stack installer does not currently provide support for Apple Silicon
(M1/M2 processors). The best option in this case is installing through GHCup.

### GHCup - All except non-WSL2 Windows

Visit [GHCup's](https://www.haskell.org/ghcup/) website and follow the
appropriate installation method for your OS. Answer **Yes** when asked: _"Do you
want to enable better integration of stack with GHCup?"_.

#### Note: Windows without WSL2

To use Windows without installing the Windows Subsystem for Linux, use the
regular Stack installer described previously.

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

If you get a `Failed to build double-conversion` error, you may need to run:

```bash
sudo dnf install gcc-c++
```

### Windows

Inside your project's directory:

```bash
stack setup
stack exec -- pacman -S msys2-keyring
stack exec -- pacman -S mingw-w64-x86_64-pkg-config
stack exec -- pacman -S mingw-w64-x86_64-SDL2
stack exec -- pacman -S mingw-w64-x86_64-freeglut
stack exec -- pacman -S mingw-w64-x86_64-glew
stack exec -- pacman -S mingw-w64-x86_64-freetype
stack exec -- pacman -Syu
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

If the previous steps did not fix the issue, maybe the manual solution
mentioned here can help: https://github.com/fjvallarino/monomer/issues/201.

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
stack build --flag monomer:examples
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
dynamically resized), there are no other differences between the threaded and
non-threaded modes.

## Development mode

Since compilation times can be annoying, I prefer to rely on
[ghcid](https://github.com/ndmitchell/ghcid) for a nicer development experience.
First, you need to install it:

```bash
stack install ghcid
```

Then, inside your project's directory:

```bash
ghcid
```

With this you will be running your application in interpreted mode (`ghcid`
under the hood uses `ghci`), allowing you to make changes and test them almost
immediately.

If the `appModelFingerprint` setting is provided, Monomer will attempt to reuse
the active model when the application is reloaded. This allows for faster
iteration since the application will return to the previous state but with all
the code and style changes that triggered the reload. Closing the window causes
the old model to be discarded, and the application will start from scratch when
reloaded.

In general, this will work fine, but in some cases modifying data types can
cause ghci to crash. Restarting ghcid/ghci will solve the issue. You can read
more details
[here](https://hackage.haskell.org/package/monomer/docs/Monomer-Main-Types.html#v:appModelFingerprint).

### VS Code

If you use Visual Studio Code, you can also use this [very nice
extension](https://marketplace.visualstudio.com/items?itemName=ndmitchell.haskell-ghcid).

Once installed, pressing `Ctrl-Shift-P` will allow you to invoke the
`Start Ghcid` command. You can also run `ghcid` on the command line directly.

## Notes for Intel Mac users

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
change in .ghci is for Mac users since macOS does not allow graphics setup to
happen outside the main thread (by default `ghci` spawns a thread for user
code). If you create your custom GHCi config and run into issues, check if you
are providing the necessary flags.

<br/>

[Next: Basics](01-basics.md)
