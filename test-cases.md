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
- Base functions
- Focus functions
- Style functions
- Text functions
  [ ] Ellipsis
  [ ] Multiline

# Widgets
- Alert
  - [x] Click Close
  - [x] Click Outside
- Box
  - [ ] Click
  - [ ] Expand (click)
  - [ ] Align (click)
- Button
  - [ ] Currently covered
  - [ ] Enter/space
- Checkbox
  - [ ] Click
  - [ ] Enter/space
  - [ ] Click on empty spaces
- Composite
  - [ ] Event handling
  - [ ] Message handling
  - [ ] Nested composites
- Confirm
  - [x] Click Accept
  - [x] Click Cancel
  - [x] Click Outside
- Container
  - Nothing for the time being; tested through other components
- Dropdown
  - Manual overlay handling (not going) through Core's Handlers
  - [ ] Open/close/select with mouse
  - [ ] Open/close/select with keyboard
  - [ ] Select on blur
- Floating field
  - [ ] Input valid number
  - [ ] Input two dots
  - [ ] Event with letter
  - [ ] Remove character
- Grid
  - Currently covered
- Image
  - [ ] Check load task request is created
  - [ ] Check merging with new path creates new load task
  - [ ] Check image is registered in renderer
- InputField
  - Tested through TextField and numeric related versions
- Integral field
  - [ ] Input valid number
  - [ ] Input one dot
  - [ ] Event with letter
  - [ ] Remove character
- Label
  - [ ] Size
  - [ ] Size multiline
- ListView
  - [ ] Navigate/select with keyboard
  - [ ] Navigate/scroll/select with mouse
- Radio
  - [ ] Click (more than one instance)
  - [ ] Enter/space
  - [ ] Click on empty spaces
- Scroll
  - Tested through ListView
  - [ ] Nest a box component to check viewport assignment
- Single
  - Nothing for the time being; tested through other components
- Spacer
  - [ ] Size
- Stack
  - Currently covered
  - [ ] Maybe add RangedSize tests?
  - [ ] Ignore empty click (mention it's tested in zstack)
- Text dropdown
  - Already covered in dropdown
- Textfield
  - [ ] Select on focus (delete too)
  - [ ] Add/remove characters
  - [ ] Copy/Paste
- ZStack
  - [ ] Assign size
  - [ ] Handle click passthrough/focus
  - [ ] stack's ignore empty click
