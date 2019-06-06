-- |
-- Module: Errors
-- Description: Errors Helpers
module Extensions.Errors where

-- | Throws an error with \"This scenario can't happen.\" message.
errCant :: b
errCant = error "This scenario can't happen."