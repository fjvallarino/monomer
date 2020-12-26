# Core
  Some StyleUtil functions

# Event
  Nothing to test

# Graphics
  Nothing to test

# Main
  Refactor Core's mainLoop so it takes a list of SystemEvents
  Handlers
  Some sort of _real_ application, with nested Composites

# Widget Utils
- Focus functions
  - [x] Next Target
  - [x] Focus Candidate
- Style functions
  - [x] activeStyle
  - [x] handleStyleChange
- Text functions
  - [x] Ellipsis
  - [x] Multiline

# Widgets
- Alert
  - [x] Click Close
  - [x] Click Outside
- Box
  - [x] Click
  - [x] Expand (click)
  - [x] Align (click)
  - Button
  - Currently covered
  - [x] Enter/space
- Checkbox
  - [x] Click
  - [x] Enter/space
  - [x] Handle value version
- Composite
  - Maybe tested as part of _real_ test application
  - [x] Event handling
  - [x] Message handling
  - [x] Nested composites
- Confirm
  - [x] Click Accept
  - [x] Click Cancel
  - [x] Click Outside
- Container
  - Nothing for the time being; tested through other components
- Dropdown
  - [x] Open/close/select with mouse
  - [x] Open/close/select with keyboard
  - [ ] Select on blur
  - [ ] Implement focus/blur
- Floating field
  - [x] Input valid number
  - [x] Input two dots
  - [x] Event with letter
  - [x] Remove character
- Grid
  - Currently covered
- Image
  - [x] Check load task request is created
  - [x] Check merging with new path creates new load task
- InputField
  - Tested through TextField and numeric related versions
- Integral field
  - [x] Input valid number
  - [x] Input one dot
  - [x] Event with letter
  - [x] Remove character
- Label
  - [x] Size
  - [x] Size multiline
- ListView
  - [x] Navigate/select with keyboard
  - [x] Navigate/scroll/select with mouse
- Radio
  - [x] Click (more than one instance)
  - [x] Enter/space
  - [x] Handle value version
- Scroll
  - Tested through ListView
  - [ ] Nest a box component to check viewport assignment
- Single
  - Nothing for the time being; tested through other components
- Spacer
  - [x] Tested in stack
- Stack
  - [x] Currently covered
  - [x] Add RangedSize tests?
  - [x] Ignore empty click (mention it's tested in zstack)
- Text dropdown
  - Already covered in dropdown
- Textfield
  - [x] Select on focus (delete too)
  - [x] Add/remove characters
  - [x] Copy/Paste
- ZStack
  - [x] Assign size
  - [x] Handle click passthrough/focus
  - [x] stack's ignore empty click
