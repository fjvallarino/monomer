# Monomer

An easy to use, cross platform, GUI library for writing Haskell applications.

It provides a framework similar to the Elm Architecture, allowing to create GUIs
functionally with an extensible set of widgets.

## Objectives

- It should be easy to use.
- It should be possible to extend it with custom widgets.
- It should work on Windows, Linux and macOS.
- It should have good documentation.
- It should have good examples.

### Ideally, in the future, this project will

- Be able to run in mobile.
- Support Vulkan/Metal.

### Why would you want to use this library

- You want to write your application in Haskell.
- You want to write a native, not web based, application.

### These are not objectives for this project

- Have a native look and feel.

### Current limitations

- Multi-platform features depend on what SDL already provides.
  - Copy/paste is only supported for text, not images.
- Text input capabilities are basic, supporting only left to right editing at the moment.
