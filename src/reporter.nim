import types/position
import std/terminal

proc getTerminalWidth(): int =
  ## Get terminal width or default to 80 if not available
  try:
    result = terminalWidth()
  except:
    result = 80

proc extractLine(
    source: string, position: int
): tuple[line: string, columnOffset: int] =
  ## Extract the line containing the error and calculate the adjusted column offset
  var
    lastNewlinePos = -1
    nextNewlinePos = source.len

  # Find the last newline before position
  for i in countdown(position - 1, 0):
    if i < source.len and (source[i] == '\n' or source[i] == '\r'):
      lastNewlinePos = i
      break

  # Find the next newline after position
  for i in (position) ..< source.len:
    if source[i] == '\n' or source[i] == '\r':
      nextNewlinePos = i
      break

  # Extract the line and calculate column offset
  let
    startPos = lastNewlinePos + 1
    problemLine = source[startPos ..< nextNewlinePos]
    columnOffset = if lastNewlinePos == -1: 0 else: startPos

  return (problemLine, columnOffset)

proc logError*(stage: string, error: string, position: Position, hint: string = "") =
  ## Log an error with source code context
  let
    termWidth = getTerminalWidth()
    extracted = extractLine(position.file.content, position.offset)
    problemLine = extracted.line
    columnInLine = position.offset - extracted.columnOffset

  # Print file and position information
  stdout.styledWriteLine(
    fgCyan, position.file.path & ":" & $position.line & ":" & $position.column
  )

  # Print stage and error message
  stdout.styledWriteLine(fgRed, "[" & stage & " Error] " & error)

  # Print the problematic line if it fits in the terminal
  if problemLine.len <= termWidth:
    echo problemLine

    # Print caret pointing to the error position
    var caretLine = ""
    for i in 0 ..< columnInLine:
      caretLine.add(' ')
    caretLine.add('^')

    stdout.styledWriteLine(fgRed, caretLine)

    # Print hint if provided
    if hint.len > 0:
      var hintLine = ""
      for i in 0 ..< columnInLine:
        hintLine.add(' ')
      hintLine.add("└ " & hint)
      stdout.styledWriteLine(fgYellow, hintLine)
  else:
    # For long lines, print a message and the position
    echo "Line too long to display (exceeds terminal width)"
