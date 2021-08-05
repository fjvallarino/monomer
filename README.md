# Monomer

An easy to use, cross platform, GUI library for writing Haskell applications.

It provides a framework similar to the Elm Architecture, allowing the creation
of GUIs using an extensible set of widgets with pure Haskell.

## Objectives

- Be easy to learn and use.
- Be extensible with custom widgets.
- Run on Windows, Linux and macOS.
- Have good documentation.
- Have good examples.

### These are not objectives for this project

- Have a native look and feel.

## Why would you want to use this library

- You want to write your application in Haskell.
- You want to write a native, not web based, application.

## Usage

### Setup

Monomer requires GHC and SDL2 to be installed.

You can read how to setup the library [here](docs/tutorials/00-setup.md).

### Tutorials

Introductory tutorials are available:

- [01 - Basics](docs/tutorials/01-basics.md)
- [02 - Styling](docs/tutorials/02-styling.md)
- [03 - Life cycle](docs/tutorials/03-life-cycle.md)
- [04 - Tasks](docs/tutorials/04-tasks.md)
- [05 - Producers](docs/tutorials/05-producers.md)
- [06 - Composite](docs/tutorials/06-composite.md)
- [07 - Custom widget](docs/tutorials/07-custom-widget.md)
- [08 - Themes](docs/tutorials/08-themes.md)

### Examples

Beyond the tutorials, a few _real world like_ examples are available:

- [Todo](docs/examples/01-todo.md)
- [Books](docs/examples/02-books.md)
- [Ticker](docs/examples/03-ticker.md)
- [Generative](docs/examples/04-generative.md)

## Roadmap

There is not a planned timeline for the tasks in the roadmap.

- Stability and performance.
- Mobile support.
- Vulkan/Metal support.

## License

The library is licensed under the [BSD-3 license](LICENSE).

Fonts used in examples:

- [Roboto] (https://fonts.google.com/specimen/Roboto), licensed under [Apache license](http://www.apache.org/licenses/LICENSE-2.0)
- [Remix Icon](https://remixicon.com), licensed under [Apache license](http://www.apache.org/licenses/LICENSE-2.0)

## Acknowledgments

Thanks to memononen for the amazing [nanovg](https://github.com/memononen/nanovg) library.
