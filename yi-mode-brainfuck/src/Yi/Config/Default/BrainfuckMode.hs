module Yi.Config.Default.BrainfuckMode (configureBrainfuckMode) where

import Yi.Mode.Brainfuck (brainfuckMode)
import Yi.Config.Simple (ConfigM, addMode)

configureBrainfuckMode :: ConfigM ()
configureBrainfuckMode = addMode brainfuckMode