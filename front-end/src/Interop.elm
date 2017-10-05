module Interop where

import Signal exposing (..)

interopMailbox : Mailbox String
interopMailbox = mailbox "NOOP"