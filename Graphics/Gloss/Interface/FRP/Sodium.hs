{-# LANGUAGE UnicodeSyntax, Rank2Types #-}

module Graphics.Gloss.Interface.FRP.Sodium (playSodium, InputEvent) where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game (playIO)
import qualified Graphics.Gloss.Interface.IO.Game as G
import FRP.Sodium
import Control.Monad (void)

-- | A useful type synonym for Gloss event values, to avoid confusion between
--   Gloss and Sodium.
type InputEvent = G.Event

-- | Play the game in a window, updating when the value of the provided
--   Behavior Picture changes.
playSodium ∷ Display -- ^ The display method
           → Color   -- ^ The background colour
           → Int     -- ^ The refresh rate, in Hertz
           → (  Event Float
              → Event InputEvent
              → Reactive (Behavior Picture))
           -- ^ A Reactive action to generate the Picture Behavior, taking
           --   the refresh and input Events with respect to which to build it.
           --   The refresh event generates a Float indicating the time delta
           --   since the last refresh.
           → IO ()
playSodium display colour frequency mPicture = do
  (unreg, bPicture, pushTick, pushInput) ← sync $ do
    (eTick,  pushTick ) ← newEvent
    (eInput, pushInput) ← newEvent
    bPicture ← mPicture eTick eInput

    -- To avoid https://github.com/kentuckyfriedtakahe/sodium/issues/14
    unreg ← listen (value bPicture) . const $ return ()
    
    return (unreg, bPicture, pushTick, pushInput)

  playIO display colour frequency ()
    (\      _ →        sync $ sample bPicture)
    (\ ev   _ → void . sync $ pushInput ev   )
    (\ time _ → void . sync $ pushTick time  )

  unreg
