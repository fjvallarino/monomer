- Done
  - Check events only for interested widgets (use focus for keyboard and region for mouse)
  - Add handling of disabled widget nodes
  - Add handling of custom external actions for widgets
  - Do something with return of custom handlers (the exact same thing we do with event handlers)
  - Add scroll support
  - Add hstack/vstack containers
  - Improve input (keyboard) handling
  - Implement copy/paste
  - Add HiDPI handling
  - Handle window resize
  - Improve handling of Color (turn to 0.0 to 1.0 instead of 0 to 255? only do it for alpha?)
  - Add handling of non visible widget nodes
  - Handle widget fixed size
  - Add good looking caret to textField
  - Add support for onFocus/onBlur
  - Maybe we don't need this and we can get by with position in render function?
    - Add support for onEnter/onLeave (keep track of previous active widgets)
    - We probably need it for drag&drop
  - How is the user going to provide state updates?
    - We already provide this with the State monad and corresponding lenses
  - Improve mouse support/current state of mouse and keyboard for event handling
  - Make handleEvent run inside MonadState (required to update user provided lenses) **CANCELLED**
  - Add logic that allows widgets to update user state
  - Does it make sense to avoid lenses internally, given that we already include the dependency?
  - How will long running (i.e., not immediate) user tasks will be handled?
  - Using local coordinates for widgets **CANCELLED**
    - How do we adjust current displacement?
  - Track drawing operations made by a Widget
    - Reorganize drawing operations
- Pending
  - Stop, think and design
    - How should all of this be organized?
    - How should modules be layed out?
    - What are good interfaces for the different parts of the system?
    - Does it make sense that handleEvent is the only pure function in a widget?
    - Based on the previous design, refactor modules
  - Current massive refactor
    - Replace Tree with Containers' Tree
    - Fix issue with event handling (click makes everything disappear)
    - Fix focus situation (remove _focusRing and replace with single focus, then use _widgetNextFocusable)
    - Provide focus to render (needed by textField)
    - Check if resize children still makes sense (maybe the widget itself can resize on the corresponding event?)
    - Check if WidgetState is really needed
    - Can we generalize _widgetFind?
    - Rethink Tree.Path import
    - Clean up Seq imports
    - Where can we use Seq? Does it make sense to use it everywhere? What about Traversable?
    - Reorganize Common Types. What do other projects do? They should be simple to import and use
  - Replace Default instances for Monoid, if possible
  - Improve merge process. Implement Global keys
  - Improve hstack/vstack
    - If available space is greater than requested, do not apply resizing logic
  - Does a styling engine make sense or doing something similar to Flutter is simpler?
    - Could container handle padding and centering?
    - Implement styling engine. Think why Maybe Double instead of Maybe Dimension (to handle pixels, percent, etc)
  - Improve ergonomics
    - Check if advanced type level features would improve the design
    - Check what syntax extensions can be abused to make life easier
    - Look for ways that allow both lenses and user events to be used in the same widget
    - Related to previous, look for ways to simplify widget setup. Default instance with common values?
    - Find way of providing instance config (style, visibility, etc) before providing children (some sort of flip operator)
  - Keep sending mouse move event if mouse is away but button is still pressed
  - Create layer widget to handle overlays/dialog boxes/tooltips (takes care of overlays)
  - Add text selection/editing to textField
  - Create Checkbox
  - Create Radio
  - Create Dropdown
  - Create Color Selector
  - Create Dialog
  - Create File Selector
  - Drag & drop for user (add attribute indicating if component supports being source/target)
    - Add new request types (drag started, drag stopped, drag cancelled)
    - Add new events (drag hover)
  - Implement SDL_Surface + Cairo backend
    - Can we cache some drawing operations?
  - Check if using [lifted-async](https://github.com/maoe/lifted-async) is worth it






What we have
============

  Main
    Collects System Events
    Propagates System Events to Widgets
    Updates Widget hierarchy
    Collects App Events
    Calls App Events Handler
    Runs Widget Tasks
    Runs User Tasks
  Widget
    Consumes System Events
    Updates Internal State
    Creates new copy on handleEvent
    Generates App Events
    Generates Widget Tasks
  App
    Consumes App Events
    Generates User Tasks
    Updates Model

What we need/want
=================

Create Data records for different types of Widget:
  StatelessWidget
  StatefulWidget
  StatelessContainer
  StatefulContainer

  Composite

  Main
    Collects System Events
    Propagates System Events to Widgets
    Updates Widget hierarchy
    Collects App Events
    Calls App Events Handler
    Runs Widget Tasks
    Runs User Tasks
  Widget
    Consumes System Events
    Updates Internal State
    Creates new copy on handleEvent
    Generates App Events
    Generates Widget Tasks
  App
    Consumes App Events
    Generates User Tasks
    Updates Model

Widget
  render
    draws itself
    calls children to draw themselves
    returns ()
  handleSystemEvent
    handles own events
    decides if event is handled by its children
    returns
      maybe widget tree (if changes where made)
      app events
      system requests
      async tasks
  handleTaskResult
    returns
      maybe widget tree (if changes where made)
      app events
      system requests
      async tasks
  mergeWith (receives current state)

  preferredSize
  resize