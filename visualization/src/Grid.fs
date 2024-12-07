module Grid
type Block =
    | Floor
    | Wall
    | Player of (int * int)
    | Trail
    | Peek
    | PeekHighlight
