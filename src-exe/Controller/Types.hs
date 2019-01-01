{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- |
-- Copyright: © 2018 Herbert Valerio Riedel
-- SPDX-License-Identifier: GPL-3.0-or-later
--
module Controller.Types where

import           Prelude.Local

import           Data.Pool
import qualified Database.PostgreSQL.Simple as PGS

import           PkgId
import           PkgIdxTsSet                (PkgIdxTsSet)

-- | See 'fetchPkgLstCache'
data PkgLstCache = PkgLstCache !PkgIdxTs !(Vector PkgN)

data App = App
  { appDbPool        :: Pool PGS.Connection
  , appPkgLstCache   :: MVar PkgLstCache   -- ^ see 'fetchPkgLstCache'
  , appPkgIdxTsCache :: MVar PkgIdxTsSet -- ^ see 'fetchPkgIdxTsCache'
  }

newtype ETag = ETag ByteString
             deriving (Eq,NFData)
