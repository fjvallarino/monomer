# Monomer

An easy to use, cross platform, GUI library for writing Haskell applications.

It provides a framework similar to the Elm Architecture, allowing to create GUIs
using an extensible set of widgets with pure Haskell.

## Objectives

- It should be easy to use.
- It should be extensible with custom widgets.
- It should run on Windows, Linux and macOS.
- It should have good documentation.
- It should have good examples.

### These are not objectives for this project

- Have a native look and feel.

## Why would you want to use this library

- You want to write your application in Haskell.
- You want to write a native, not web based, application.

## Current limitations

- Multi-platform features depend (mostly) on what SDL already provides.
  - Copy/paste is only supported for text, not images.
- Only supports left to right text editing at the moment.

## Roadmap

There is not a planned timeline for the tasks in the roadmap, nor a guarantee
that they will be implemented.

- Add mobile support.
- Add support for Vulkan/Metal.

## License

The library is licensed under [BSD-3 license](LICENSE).

Fonts used in examples:

- [Roboto] (https://fonts.google.com/specimen/Roboto), licensed under [Apache license](http://www.apache.org/licenses/LICENSE-2.0)
- [Remix Icon](https://remixicon.com), licensed under [Apache license](http://www.apache.org/licenses/LICENSE-2.0)

## Acknowledgments

Thanks to memononen for the amazing [nanovg](https://github.com/memononen/nanovg) library.
