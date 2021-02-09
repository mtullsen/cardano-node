{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -freduction-depth=0 #-}

module NewApiStuff
  ( -- * Initialization / Accumulation
    initialLedgerState
  , applyBlock

    -- * Types
  , LedgerState
      ( ..
      , LedgerStateByron
      , LedgerStateShelley
      , LedgerStateAllegra
      , LedgerStateMary
      )
  , Env(..)
  , envSecurityParam
  )
  where

import           Control.Exception
import           Control.Monad.Except
import           Control.Monad.Trans.Except.Extra
import           Data.Aeson as Aeson
import qualified Data.Aeson.Types as Data.Aeson.Types.Internal
import           Data.ByteArray (ByteArrayAccess)
import qualified Data.ByteArray
import           Data.ByteString as BS
import qualified Data.ByteString.Base16 as Base16
import           Data.ByteString.Short as BSS
import           Data.Foldable
import           Data.Functor.Identity (Identity (..))
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe (fromMaybe)
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import           Data.Word
import qualified Data.Yaml as Yaml
import           GHC.Natural
import           System.FilePath

import qualified Cardano.Api.Block
import qualified Cardano.Api.Eras
import qualified Cardano.Chain.Genesis
import qualified Cardano.Chain.UTxO
import qualified Cardano.Chain.Update
import qualified Cardano.Crypto
import qualified Cardano.Crypto.Hash.Blake2b
import qualified Cardano.Crypto.Hash.Class
import qualified Cardano.Crypto.Hashing
import qualified Cardano.Crypto.ProtocolMagic
import qualified Cardano.Slotting.EpochInfo.API
import qualified Cardano.Slotting.Slot
import qualified Ouroboros.Consensus.Block.Abstract
import qualified Ouroboros.Consensus.BlockchainTime.WallClock.Types
import qualified Ouroboros.Consensus.Byron.Ledger.Block
import qualified Ouroboros.Consensus.Cardano
import qualified Ouroboros.Consensus.Cardano as C
import qualified Ouroboros.Consensus.Cardano.Block
import qualified Ouroboros.Consensus.Cardano.Block as C
import qualified Ouroboros.Consensus.Cardano.CanHardFork
import qualified Ouroboros.Consensus.Cardano.Node
import qualified Ouroboros.Consensus.Config
import qualified Ouroboros.Consensus.Config as C
import qualified Ouroboros.Consensus.HardFork.Combinator.AcrossEras
import qualified Ouroboros.Consensus.HardFork.Combinator.Basics
import qualified Ouroboros.Consensus.HardFork.Combinator.State
import qualified Ouroboros.Consensus.HeaderValidation
import qualified Ouroboros.Consensus.Ledger.Abstract
import qualified Ouroboros.Consensus.Ledger.Basics
import qualified Ouroboros.Consensus.Ledger.Extended
import qualified Ouroboros.Consensus.Ledger.Extended as C
import qualified Ouroboros.Consensus.Node.ProtocolInfo
import qualified Ouroboros.Consensus.Shelley.Eras
import qualified Ouroboros.Consensus.Shelley.Ledger.Block
import qualified Ouroboros.Consensus.Shelley.Ledger.Ledger
import qualified Ouroboros.Consensus.Shelley.Protocol
import qualified Ouroboros.Network.Block
import qualified Ouroboros.Network.Magic
import qualified Shelley.Spec.Ledger.API.Protocol
import qualified Shelley.Spec.Ledger.Address
import qualified Shelley.Spec.Ledger.BaseTypes
import qualified Shelley.Spec.Ledger.Coin
import qualified Shelley.Spec.Ledger.Credential
import qualified Shelley.Spec.Ledger.EpochBoundary
import qualified Shelley.Spec.Ledger.Genesis
import qualified Shelley.Spec.Ledger.Keys
import qualified Shelley.Spec.Ledger.LedgerState
import qualified Shelley.Spec.Ledger.PParams
import qualified Shelley.Spec.Ledger.STS.Tickn


-- | Get the initial ledger state (and corresponding environment).
initialLedgerState
  :: FilePath
  -- ^ Path to the cardano-node config file (e.g. <path to cardano-node project>/configuration/cardano/mainnet-config.json)
  -> IO (Env, LedgerState)
  -- ^ ( The environment
  --   , The initial ledger state
  --   )
initialLedgerState dbSyncConfFilePath = do
  dbSyncConf <- readDbSyncNodeConfig (NodeConfigFile dbSyncConfFilePath)
  genConf <- fmap (either (error . Text.unpack . renderDbSyncNodeError) id) $ runExceptT (readCardanoGenesisConfig dbSyncConf)
  env <- either (error . Text.unpack . renderDbSyncNodeError) return (genesisConfigToEnv genConf)
  let ledgerState = initLedgerStateVar genConf
  return (env, ledgerState)

-- | Apply a single block to the current ledger state.
-- TODO to what extent if any does this validate the block?
applyBlock
  :: Env
  -- ^ The environment returned by @initialLedgerState@
  -> LedgerState
  -- ^ The current ledger state
  -> Cardano.Api.Block.Block era
  -- ^ Some block to apply
  -> LedgerState
  -- ^ The new ledger state.
applyBlock env oldState block = let
  cardanoBlock :: Ouroboros.Consensus.Cardano.Block.CardanoBlock Ouroboros.Consensus.Shelley.Eras.StandardCrypto
  cardanoBlock = case block of
    Cardano.Api.Block.ByronBlock byronBlock -> Ouroboros.Consensus.Cardano.Block.BlockByron byronBlock
    Cardano.Api.Block.ShelleyBlock blockEra shelleyBlock -> case blockEra of
      Cardano.Api.Eras.ShelleyBasedEraShelley -> Ouroboros.Consensus.Cardano.Block.BlockShelley shelleyBlock
      Cardano.Api.Eras.ShelleyBasedEraAllegra -> Ouroboros.Consensus.Cardano.Block.BlockAllegra shelleyBlock
      Cardano.Api.Eras.ShelleyBasedEraMary    -> Ouroboros.Consensus.Cardano.Block.BlockMary shelleyBlock
  LedgerStateSnapshot newState _ = applyBlock' env oldState cardanoBlock
  in newState

pattern LedgerStateByron
  :: Ouroboros.Consensus.Ledger.Basics.LedgerState Ouroboros.Consensus.Byron.Ledger.Block.ByronBlock
  -> LedgerState
pattern LedgerStateByron st <- LedgerState
  (Ouroboros.Consensus.Ledger.Extended.ExtLedgerState
    (Ouroboros.Consensus.Cardano.Block.LedgerStateByron st)
    _headSt
  )

pattern LedgerStateShelley
  :: Ouroboros.Consensus.Ledger.Basics.LedgerState (Ouroboros.Consensus.Shelley.Ledger.Block.ShelleyBlock (Ouroboros.Consensus.Shelley.Eras.ShelleyEra Ouroboros.Consensus.Shelley.Eras.StandardCrypto))
  -> LedgerState
pattern LedgerStateShelley st <- LedgerState
  (Ouroboros.Consensus.Ledger.Extended.ExtLedgerState
    (Ouroboros.Consensus.Cardano.Block.LedgerStateShelley st)
    _headSt
  )

pattern LedgerStateAllegra
  :: Ouroboros.Consensus.Ledger.Basics.LedgerState (Ouroboros.Consensus.Shelley.Ledger.Block.ShelleyBlock (Ouroboros.Consensus.Shelley.Eras.AllegraEra Ouroboros.Consensus.Shelley.Eras.StandardCrypto))
  -> LedgerState
pattern LedgerStateAllegra st <- LedgerState
  (Ouroboros.Consensus.Ledger.Extended.ExtLedgerState
    (Ouroboros.Consensus.Cardano.Block.LedgerStateAllegra st)
    _headSt
  )

pattern LedgerStateMary
  :: Ouroboros.Consensus.Ledger.Basics.LedgerState (Ouroboros.Consensus.Shelley.Ledger.Block.ShelleyBlock (Ouroboros.Consensus.Shelley.Eras.MaryEra Ouroboros.Consensus.Shelley.Eras.StandardCrypto))
  -> LedgerState
pattern LedgerStateMary st <- LedgerState
  (Ouroboros.Consensus.Ledger.Extended.ExtLedgerState
    (Ouroboros.Consensus.Cardano.Block.LedgerStateMary st)
    _headSt
  )

{-# COMPLETE LedgerStateByron
           , LedgerStateShelley
           , LedgerStateAllegra
           , LedgerStateMary #-}

--------------------------------------------------------------------------------
-- Everything below this is just coppied from db-sync                         --
--------------------------------------------------------------------------------

genesisConfigToEnv ::
  -- DbSyncNodeParams ->
  GenesisConfig ->
  Either DbSyncNodeError Env
genesisConfigToEnv
  -- enp
  genCfg =
    case genCfg of
      GenesisCardano _ bCfg sCfg
        | Cardano.Crypto.ProtocolMagic.unProtocolMagicId (Cardano.Chain.Genesis.configProtocolMagicId bCfg) /= Shelley.Spec.Ledger.Genesis.sgNetworkMagic (scConfig sCfg) ->
            Left . NECardanoConfig $
              mconcat
                [ "ProtocolMagicId ", textShow (Cardano.Crypto.ProtocolMagic.unProtocolMagicId $ Cardano.Chain.Genesis.configProtocolMagicId bCfg)
                , " /= ", textShow (Shelley.Spec.Ledger.Genesis.sgNetworkMagic $ scConfig sCfg)
                ]
        | Cardano.Chain.Genesis.gdStartTime (Cardano.Chain.Genesis.configGenesisData bCfg) /= Shelley.Spec.Ledger.Genesis.sgSystemStart (scConfig sCfg) ->
            Left . NECardanoConfig $
              mconcat
                [ "SystemStart ", textShow (Cardano.Chain.Genesis.gdStartTime $ Cardano.Chain.Genesis.configGenesisData bCfg)
                , " /= ", textShow (Shelley.Spec.Ledger.Genesis.sgSystemStart $ scConfig sCfg)
                ]
        | otherwise ->
            Right $ Env
                  { envNetwork = Shelley.Spec.Ledger.Genesis.sgNetworkId (scConfig sCfg)
                  , envConfig = Ouroboros.Consensus.Node.ProtocolInfo.pInfoConfig (mkProtocolInfoCardano genCfg)
                  }

readDbSyncNodeConfig :: NodeConfigFile -> IO DbSyncNodeConfig
readDbSyncNodeConfig (NodeConfigFile ncf) = do
    ncfg <- parseNodeConfig <$> readByteString ncf "node"
    coalesceConfig ncfg (mkAdjustPath ncf)

data NodeConfig = NodeConfig
  { ncPBftSignatureThreshold :: !(Maybe Double)
  , ncByronGenesisFile :: !GenesisFile
  , ncByronGenesisHash :: !GenesisHashByron
  , ncShelleyGenesisFile :: !GenesisFile
  , ncShelleyGenesisHash :: !GenesisHashShelley
  , ncRequiresNetworkMagic :: !Cardano.Crypto.RequiresNetworkMagic
  , ncByronSotfwareVersion :: !Cardano.Chain.Update.SoftwareVersion
  , ncByronProtocolVersion :: !Cardano.Chain.Update.ProtocolVersion

  -- Shelley hardfok parameters
  , ncByronToShelley :: !ByronToShelley

  -- Allegra hardfok parameters
  , ncShelleyToAllegra :: !ShelleyToAllegra

  -- Mary hardfok parameters
  , ncAllegraToMary :: !AllegraToMary
  }


instance FromJSON NodeConfig where
  parseJSON v =
      Aeson.withObject "NodeConfig" parse v
    where
      parse :: Object -> Data.Aeson.Types.Internal.Parser NodeConfig
      parse o =
        NodeConfig
          <$> o .:? "PBftSignatureThreshold"
          <*> fmap GenesisFile (o .: "ByronGenesisFile")
          <*> fmap GenesisHashByron (o .: "ByronGenesisHash")
          <*> fmap GenesisFile (o .: "ShelleyGenesisFile")
          <*> fmap GenesisHashShelley (o .: "ShelleyGenesisHash")
          <*> o .: "RequiresNetworkMagic"
          <*> parseByronSoftwareVersion o
          <*> parseByronProtocolVersion o
          <*> (Ouroboros.Consensus.Cardano.Node.ProtocolParamsTransition <$> parseShelleyHardForkEpoch o)
          <*> (Ouroboros.Consensus.Cardano.Node.ProtocolParamsTransition <$> parseAllegraHardForkEpoch o)
          <*> (Ouroboros.Consensus.Cardano.Node.ProtocolParamsTransition <$> parseMaryHardForkEpoch o)

      parseByronProtocolVersion :: Object -> Data.Aeson.Types.Internal.Parser Cardano.Chain.Update.ProtocolVersion
      parseByronProtocolVersion o =
        Cardano.Chain.Update.ProtocolVersion
          <$> o .: "LastKnownBlockVersion-Major"
          <*> o .: "LastKnownBlockVersion-Minor"
          <*> o .: "LastKnownBlockVersion-Alt"

      parseByronSoftwareVersion :: Object -> Data.Aeson.Types.Internal.Parser Cardano.Chain.Update.SoftwareVersion
      parseByronSoftwareVersion o =
        Cardano.Chain.Update.SoftwareVersion
          <$> fmap Cardano.Chain.Update.ApplicationName (o .: "ApplicationName")
          <*> o .: "ApplicationVersion"

      parseShelleyHardForkEpoch :: Object -> Data.Aeson.Types.Internal.Parser Ouroboros.Consensus.Cardano.CanHardFork.TriggerHardFork
      parseShelleyHardForkEpoch o =
        asum
          [ Ouroboros.Consensus.Cardano.CanHardFork.TriggerHardForkAtEpoch <$> o .: "TestShelleyHardForkAtEpoch"
          , pure $ Ouroboros.Consensus.Cardano.CanHardFork.TriggerHardForkAtVersion 2 -- Mainnet default
          ]

      parseAllegraHardForkEpoch :: Object -> Data.Aeson.Types.Internal.Parser Ouroboros.Consensus.Cardano.CanHardFork.TriggerHardFork
      parseAllegraHardForkEpoch o =
        asum
          [ Ouroboros.Consensus.Cardano.CanHardFork.TriggerHardForkAtEpoch <$> o .: "TestAllegraHardForkAtEpoch"
          , pure $ Ouroboros.Consensus.Cardano.CanHardFork.TriggerHardForkAtVersion 3 -- Mainnet default
          ]

      parseMaryHardForkEpoch :: Object -> Data.Aeson.Types.Internal.Parser Ouroboros.Consensus.Cardano.CanHardFork.TriggerHardFork
      parseMaryHardForkEpoch o =
        asum
          [ Ouroboros.Consensus.Cardano.CanHardFork.TriggerHardForkAtEpoch <$> o .: "TestMaryHardForkAtEpoch"
          , pure $ Ouroboros.Consensus.Cardano.CanHardFork.TriggerHardForkAtVersion 4 -- Mainnet default
          ]

parseNodeConfig :: ByteString -> NodeConfig
parseNodeConfig bs =
  case Yaml.decodeEither' bs of
    Left err -> error . Text.unpack $ "Error parsing node config: " <> textShow err
    Right nc -> nc

coalesceConfig
    :: NodeConfig -> (FilePath -> FilePath)
    -> IO DbSyncNodeConfig
coalesceConfig ncfg adjustGenesisPath = do
  pure $ DbSyncNodeConfig
          {
          --   dncNetworkName = pcNetworkName pcfg
          -- , dncLoggingConfig = lc
          -- , dncNodeConfigFile = pcNodeConfigFile pcfg
          dncRequiresNetworkMagic = ncRequiresNetworkMagic ncfg
          -- , dncEnableLogging = pcEnableLogging pcfg
          -- , dncEnableMetrics = pcEnableMetrics pcfg
          , dncPBftSignatureThreshold = ncPBftSignatureThreshold ncfg
          , dncByronGenesisFile = adjustGenesisFilePath adjustGenesisPath (ncByronGenesisFile ncfg)
          , dncByronGenesisHash = ncByronGenesisHash ncfg
          , dncShelleyGenesisFile = adjustGenesisFilePath adjustGenesisPath (ncShelleyGenesisFile ncfg)
          , dncShelleyGenesisHash = ncShelleyGenesisHash ncfg
          , dncByronSoftwareVersion = ncByronSotfwareVersion ncfg
          , dncByronProtocolVersion = ncByronProtocolVersion ncfg

          -- , dncShelleyHardFork = ncShelleyHardFork ncfg
          -- , dncAllegraHardFork = ncAllegraHardFork ncfg
          -- , dncMaryHardFork = ncMaryHardFork ncfg

          , dncByronToShelley = ncByronToShelley ncfg
          , dncShelleyToAllegra = ncShelleyToAllegra ncfg
          , dncAllegraToMary = ncAllegraToMary ncfg
          }

adjustGenesisFilePath :: (FilePath -> FilePath) -> GenesisFile -> GenesisFile
adjustGenesisFilePath f (GenesisFile p) = GenesisFile (f p)

mkAdjustPath :: FilePath -> (FilePath -> FilePath)
mkAdjustPath nodeConfigFilePath fp = takeDirectory nodeConfigFilePath </> fp

readByteString :: FilePath -> Text -> IO ByteString
readByteString fp cfgType =
  catch (BS.readFile fp) $ \(_ :: IOException) ->
    error . Text.unpack $ mconcat [ "Cannot find the ", cfgType, " configuration file at : ", Text.pack fp ]


initLedgerStateVar :: GenesisConfig -> LedgerState
initLedgerStateVar genesisConfig = LedgerState
  { clsState = Ouroboros.Consensus.Node.ProtocolInfo.pInfoInitLedger protocolInfo
  }
  where
    protocolInfo = mkProtocolInfoCardano genesisConfig

newtype LedgerState = LedgerState
  { clsState :: (C.ExtLedgerState (C.CardanoBlock C.StandardCrypto))
  }

-- Usually only one constructor, but may have two when we are preparing for a HFC event.
data GenesisConfig
  = GenesisCardano !DbSyncNodeConfig !Cardano.Chain.Genesis.Config !ShelleyConfig

data ShelleyConfig = ShelleyConfig
  { scConfig :: !(Shelley.Spec.Ledger.Genesis.ShelleyGenesis Ouroboros.Consensus.Shelley.Eras.StandardShelley)
  , scGenesisHash :: !GenesisHashShelley
  }


data DbSyncNodeConfig = DbSyncNodeConfig
  { --   dncNetworkName :: !NetworkName
  -- , dncLoggingConfig :: !BM.Configuration
  -- , dncNodeConfigFile :: !NodeConfigFile
  dncRequiresNetworkMagic :: !Cardano.Crypto.RequiresNetworkMagic
  -- , dncEnableLogging :: !Bool
  -- , dncEnableMetrics :: !Bool
  , dncPBftSignatureThreshold :: !(Maybe Double)
  , dncByronGenesisFile :: !GenesisFile
  , dncByronGenesisHash :: !GenesisHashByron
  , dncShelleyGenesisFile :: !GenesisFile
  , dncShelleyGenesisHash :: !GenesisHashShelley
  , dncByronSoftwareVersion :: !Cardano.Chain.Update.SoftwareVersion
  , dncByronProtocolVersion :: !Cardano.Chain.Update.ProtocolVersion

  -- , dncShelleyHardFork :: !Ouroboros.Consensus.Cardano.CanHardFork.TriggerHardFork
  -- , dncAllegraHardFork :: !Ouroboros.Consensus.Cardano.CanHardFork.TriggerHardFork
  -- , dncMaryHardFork :: !Ouroboros.Consensus.Cardano.CanHardFork.TriggerHardFork

  , dncByronToShelley :: !ByronToShelley
  , dncShelleyToAllegra :: !ShelleyToAllegra
  , dncAllegraToMary :: !AllegraToMary
  }

type ByronToShelley =
  C.ProtocolParamsTransition Ouroboros.Consensus.Byron.Ledger.Block.ByronBlock
    (Ouroboros.Consensus.Shelley.Ledger.Block.ShelleyBlock Ouroboros.Consensus.Shelley.Eras.StandardShelley)

type ShelleyToAllegra =
  C.ProtocolParamsTransition
    (Ouroboros.Consensus.Shelley.Ledger.Block.ShelleyBlock Ouroboros.Consensus.Shelley.Eras.StandardShelley)
    (Ouroboros.Consensus.Shelley.Ledger.Block.ShelleyBlock Ouroboros.Consensus.Shelley.Eras.StandardAllegra)

type AllegraToMary =
  C.ProtocolParamsTransition
    (Ouroboros.Consensus.Shelley.Ledger.Block.ShelleyBlock Ouroboros.Consensus.Shelley.Eras.StandardAllegra)
    (Ouroboros.Consensus.Shelley.Ledger.Block.ShelleyBlock Ouroboros.Consensus.Shelley.Eras.StandardMary)

newtype GenesisFile = GenesisFile
  { unGenesisFile :: FilePath
  } deriving Show

newtype GenesisHashByron = GenesisHashByron
  { unGenesisHashByron :: Text
  } deriving newtype (Eq, Show)

newtype GenesisHashShelley = GenesisHashShelley
  { unGenesisHashShelley :: Cardano.Crypto.Hash.Class.Hash Cardano.Crypto.Hash.Blake2b.Blake2b_256 ByteString
  } deriving newtype (Eq, Show)

newtype LedgerStateDir = LedgerStateDir
  {  unLedgerStateDir :: FilePath
  } deriving Show

newtype NetworkName = NetworkName
  { unNetworkName :: Text
  } deriving Show

newtype NodeConfigFile = NodeConfigFile
  { unNodeConfigFile :: FilePath
  } deriving Show

newtype SocketPath = SocketPath
  { unSocketPath :: FilePath
  } deriving Show

mkProtocolInfoCardano :: GenesisConfig -> Ouroboros.Consensus.Node.ProtocolInfo.ProtocolInfo IO CardanoBlock
mkProtocolInfoCardano = Ouroboros.Consensus.Cardano.protocolInfo . mkProtocolCardano

type CardanoBlock =
        Ouroboros.Consensus.HardFork.Combinator.Basics.HardForkBlock
            (Ouroboros.Consensus.Cardano.Block.CardanoEras C.StandardCrypto)

mkProtocolCardano :: GenesisConfig -> C.Protocol m CardanoBlock CardanoProtocol
mkProtocolCardano ge =
  case ge of
    GenesisCardano dnc byronGenesis shelleyGenesis ->
        C.ProtocolCardano
          C.ProtocolParamsByron
            { C.byronGenesis = byronGenesis
            , C.byronPbftSignatureThreshold = C.PBftSignatureThreshold <$> dncPBftSignatureThreshold dnc
            , C.byronProtocolVersion = dncByronProtocolVersion dnc
            , C.byronSoftwareVersion = dncByronSoftwareVersion dnc
            , C.byronLeaderCredentials = Nothing
            }
          C.ProtocolParamsShelleyBased
            { C.shelleyBasedGenesis = scConfig shelleyGenesis
            , C.shelleyBasedInitialNonce = shelleyPraosNonce shelleyGenesis
            , C.shelleyBasedLeaderCredentials = []
            }
          C.ProtocolParamsShelley
            { C.shelleyProtVer = shelleyProtVer dnc
            }
          C.ProtocolParamsAllegra
            { C.allegraProtVer = shelleyProtVer dnc
            }
          C.ProtocolParamsMary
            { C.maryProtVer = shelleyProtVer dnc
            }
          (dncByronToShelley dnc)
          (dncShelleyToAllegra dnc)
          (dncAllegraToMary dnc)

shelleyPraosNonce :: ShelleyConfig -> Shelley.Spec.Ledger.BaseTypes.Nonce
shelleyPraosNonce sCfg = Shelley.Spec.Ledger.BaseTypes.Nonce (Cardano.Crypto.Hash.Class.castHash . unGenesisHashShelley $ scGenesisHash sCfg)

shelleyProtVer :: DbSyncNodeConfig -> Shelley.Spec.Ledger.PParams.ProtVer
shelleyProtVer dnc =
  let bver = dncByronProtocolVersion dnc in
  Shelley.Spec.Ledger.PParams.ProtVer
    (fromIntegral $ Cardano.Chain.Update.pvMajor bver)
    (fromIntegral $ Cardano.Chain.Update.pvMinor bver)

type CardanoProtocol =
        Ouroboros.Consensus.HardFork.Combinator.Basics.HardForkProtocol
            '[ Ouroboros.Consensus.Byron.Ledger.Block.ByronBlock
            , Ouroboros.Consensus.Shelley.Ledger.Block.ShelleyBlock Ouroboros.Consensus.Shelley.Eras.StandardShelley
            , Ouroboros.Consensus.Shelley.Ledger.Block.ShelleyBlock Ouroboros.Consensus.Shelley.Eras.StandardAllegra
            , Ouroboros.Consensus.Shelley.Ledger.Block.ShelleyBlock Ouroboros.Consensus.Shelley.Eras.StandardMary
            ]

readCardanoGenesisConfig
        :: DbSyncNodeConfig
        -> ExceptT DbSyncNodeError IO GenesisConfig
readCardanoGenesisConfig enc =
  GenesisCardano enc <$> readByronGenesisConfig enc <*> readShelleyGenesisConfig enc

data DbSyncNodeError
  = NELookup !Text !LookupFail
  | NEError !Text
  | NEInvariant !Text !DbSyncInvariant
  | NEBlockMismatch !Word64 !ByteString !ByteString
  | NEByronConfig !FilePath !Cardano.Chain.Genesis.ConfigurationError
  | NEShelleyConfig !FilePath !Text
  | NECardanoConfig !Text

renderDbSyncNodeError :: DbSyncNodeError -> Text
renderDbSyncNodeError ne =
  case ne of
    NELookup loc lf -> mconcat [ "DB lookup fail in ", loc, ": ", renderLookupFail lf ]
    NEError t -> "Error: " <> t
    NEInvariant loc i -> mconcat [ loc, ": " <> renderDbSyncInvariant i ]
    NEBlockMismatch blkNo hashDb hashBlk ->
      mconcat
        [ "Block mismatch for block number ", textShow blkNo, ", db has "
        , bsBase16Encode hashDb, " but chain provided ", bsBase16Encode hashBlk
        ]
    NEByronConfig fp ce ->
      mconcat
        [ "Failed reading Byron genesis file ", textShow fp, ": ", textShow ce
        ]
    NEShelleyConfig fp txt ->
      mconcat
        [ "Failed reading Shelley genesis file ", textShow fp, ": ", txt
        ]
    NECardanoConfig err ->
      mconcat
        [ "With Cardano protocol, Byron/Shelley config mismatch:\n"
        , "   ", err
        ]

unTxHash :: Cardano.Crypto.Hashing.Hash Cardano.Chain.UTxO.Tx -> ByteString
unTxHash =  Cardano.Crypto.Hashing.abstractHashToBytes

renderDbSyncInvariant :: DbSyncInvariant -> Text
renderDbSyncInvariant ei =
  case ei of
    EInvInOut inval outval ->
      mconcat [ "input value ", textShow inval, " < output value ", textShow outval ]
    EInvTxInOut tx inval outval ->
      mconcat
        [ "tx ", bsBase16Encode (unTxHash $ Cardano.Crypto.Hashing.serializeCborHash tx)
        , " : input value ", textShow inval, " < output value ", textShow outval
        , "\n", textShow tx
        ]

bsBase16Encode :: ByteString -> Text
bsBase16Encode bs =
  case Text.decodeUtf8' (Base16.encode bs) of
    Left _ -> Text.pack $ "UTF-8 decode failed for " ++ show bs
    Right txt -> txt

renderLookupFail :: LookupFail -> Text
renderLookupFail lf =
  case lf of
    DbLookupBlockHash h -> "block hash " <> base16encode h
    DbLookupBlockId blkid -> "block id " <> textShow blkid
    DbLookupMessage txt -> txt
    DbLookupTxHash h -> "tx hash " <> base16encode h
    DbLookupTxOutPair h i ->
        Text.concat [ "tx out pair (", base16encode h, ", ", textShow i, ")" ]
    DbLookupEpochNo e ->
        Text.concat [ "epoch number ", textShow e ]
    DbLookupSlotNo s ->
        Text.concat [ "slot number ", textShow s ]
    DbMetaEmpty -> "Meta table is empty"
    DbMetaMultipleRows -> "Multiple rows in Meta table which should only contain one"

base16encode :: ByteString -> Text
base16encode = Text.decodeUtf8 . Base16.encode

data LookupFail
  = DbLookupBlockHash !ByteString
  | DbLookupBlockId !Word64
  | DbLookupMessage !Text
  | DbLookupTxHash !ByteString
  | DbLookupTxOutPair !ByteString !Word16
  | DbLookupEpochNo !Word64
  | DbLookupSlotNo !Word64
  | DbMetaEmpty
  | DbMetaMultipleRows
  deriving (Eq, Show)

data DbSyncInvariant
  = EInvInOut !Word64 !Word64
  | EInvTxInOut !Cardano.Chain.UTxO.Tx !Word64 !Word64

readByronGenesisConfig
        :: DbSyncNodeConfig
        -> ExceptT DbSyncNodeError IO Cardano.Chain.Genesis.Config
readByronGenesisConfig enc = do
  let file = unGenesisFile $ dncByronGenesisFile enc
  genHash <- firstExceptT NEError
                . hoistEither
                $ Cardano.Crypto.Hashing.decodeAbstractHash (unGenesisHashByron $ dncByronGenesisHash enc)
  firstExceptT (NEByronConfig file)
                $ Cardano.Chain.Genesis.mkConfigFromFile (dncRequiresNetworkMagic enc) file genHash


readShelleyGenesisConfig
    :: DbSyncNodeConfig
    -> ExceptT DbSyncNodeError IO ShelleyConfig
readShelleyGenesisConfig enc = do
  let file = unGenesisFile $ dncShelleyGenesisFile enc
  firstExceptT (NEShelleyConfig file . renderShelleyGenesisError)
    $ readGenesis (GenesisFile file) Nothing

textShow :: Show a => a -> Text
textShow = Text.pack . show

readGenesis
    :: GenesisFile -> Maybe GenesisHashShelley
    -> ExceptT ShelleyGenesisError IO ShelleyConfig
readGenesis (GenesisFile file) mbExpectedGenesisHash = do
    content <- handleIOExceptT (GenesisReadError file . textShow) $ BS.readFile file
    let genesisHash = GenesisHashShelley (Cardano.Crypto.Hash.Class.hashWith id content)
    checkExpectedGenesisHash genesisHash
    genesis <- firstExceptT (GenesisDecodeError file . Text.pack)
                  . hoistEither
                  $ Aeson.eitherDecodeStrict' content
    pure $ ShelleyConfig genesis genesisHash
  where
    checkExpectedGenesisHash :: GenesisHashShelley -> ExceptT ShelleyGenesisError IO ()
    checkExpectedGenesisHash actual =
      case mbExpectedGenesisHash of
        Just expected | actual /= expected
          -> left (GenesisHashMismatch actual expected)
        _ -> pure ()

data ShelleyGenesisError
     = GenesisReadError !FilePath !Text
     | GenesisHashMismatch !GenesisHashShelley !GenesisHashShelley -- actual, expected
     | GenesisDecodeError !FilePath !Text
     deriving Show

renderShelleyGenesisError :: ShelleyGenesisError -> Text
renderShelleyGenesisError sge =
    case sge of
      GenesisReadError fp err ->
        mconcat
          [ "There was an error reading the genesis file: ", Text.pack fp
          , " Error: ", err
          ]

      GenesisHashMismatch actual expected ->
        mconcat
          [ "Wrong Shelley genesis file: the actual hash is ", renderHash actual
          , ", but the expected Shelley genesis hash given in the node "
          , "configuration file is ", renderHash expected, "."
          ]

      GenesisDecodeError fp err ->
        mconcat
          [ "There was an error parsing the genesis file: ", Text.pack fp
          , " Error: ", err
          ]
  where
    renderHash :: GenesisHashShelley -> Text
    renderHash (GenesisHashShelley h) = Text.decodeUtf8 $ Base16.encode (Cardano.Crypto.Hash.Class.hashToBytes h)



data ProtoParams = ProtoParams
  { ppMinfeeA :: !Natural
  , ppMinfeeB :: !Natural
  , ppMaxBBSize :: !Natural
  , ppMaxTxSize :: !Natural
  , ppMaxBHSize :: !Natural
  , ppKeyDeposit :: !Shelley.Spec.Ledger.Coin.Coin
  , ppPoolDeposit :: !Shelley.Spec.Ledger.Coin.Coin
  , ppMaxEpoch :: !Cardano.Slotting.Slot.EpochNo
  , ppOptialPoolCount :: !Natural
  , ppInfluence :: !Rational
  , ppMonetaryExpandRate :: !Shelley.Spec.Ledger.BaseTypes.UnitInterval
  , ppTreasuryGrowthRate :: !Shelley.Spec.Ledger.BaseTypes.UnitInterval
  , ppDecentralisation :: !Shelley.Spec.Ledger.BaseTypes.UnitInterval
  , ppExtraEntropy :: !Shelley.Spec.Ledger.BaseTypes.Nonce
  , ppProtocolVersion :: !Shelley.Spec.Ledger.PParams.ProtVer
  , ppMinUTxOValue :: !Shelley.Spec.Ledger.Coin.Coin
  , ppMinPoolCost :: !Shelley.Spec.Ledger.Coin.Coin
  }

-- The `ledger-specs` code defines a `RewardUpdate` type that is parameterised over
-- Shelley/Allegra/Mary. This is a huge pain in the neck for `db-sync` so we define a
-- generic one instead.
newtype Rewards
  = Rewards { unRewards :: Map StakeCred Shelley.Spec.Ledger.Coin.Coin }

newtype StakeCred
  = StakeCred { unStakeCred :: ByteString }
  deriving (Eq, Ord)

newtype StakeDist
  = StakeDist { unStakeDist :: Map StakeCred Shelley.Spec.Ledger.Coin.Coin }

data EpochUpdate = EpochUpdate
  { euProtoParams :: !ProtoParams
  , euRewards :: !(Maybe Rewards)
  , euStakeDistribution :: !StakeDist
  , euNonce :: !Shelley.Spec.Ledger.BaseTypes.Nonce
  }


allegraEpochUpdate
  :: Env
  -> Ouroboros.Consensus.Ledger.Basics.LedgerState (Ouroboros.Consensus.Shelley.Ledger.Block.ShelleyBlock Ouroboros.Consensus.Shelley.Eras.StandardAllegra)
  -> Maybe Rewards
  -> Maybe Shelley.Spec.Ledger.BaseTypes.Nonce
  -> EpochUpdate
allegraEpochUpdate env sls mRewards mNonce =
  EpochUpdate
    { euProtoParams = allegraProtoParams sls
    , euRewards = mRewards
    , euStakeDistribution = allegraStakeDist env sls
    , euNonce = fromMaybe Shelley.Spec.Ledger.BaseTypes.NeutralNonce mNonce
    }

allegraStakeDist :: Env -> Ouroboros.Consensus.Ledger.Basics.LedgerState (Ouroboros.Consensus.Shelley.Ledger.Block.ShelleyBlock Ouroboros.Consensus.Shelley.Eras.StandardAllegra) -> StakeDist
allegraStakeDist env
  = StakeDist
  . Map.mapKeys (toStakeCred env)
  . Shelley.Spec.Ledger.EpochBoundary.unStake
  . Shelley.Spec.Ledger.EpochBoundary._stake
  . Shelley.Spec.Ledger.EpochBoundary._pstakeSet
  . Shelley.Spec.Ledger.LedgerState.esSnapshots
  . Shelley.Spec.Ledger.LedgerState.nesEs
  . Ouroboros.Consensus.Shelley.Ledger.Ledger.shelleyLedgerState

maryStakeDist :: Env -> Ouroboros.Consensus.Ledger.Basics.LedgerState (Ouroboros.Consensus.Shelley.Ledger.Block.ShelleyBlock Ouroboros.Consensus.Shelley.Eras.StandardMary) -> StakeDist
maryStakeDist env
  = StakeDist
  . Map.mapKeys (toStakeCred env)
  . Shelley.Spec.Ledger.EpochBoundary.unStake
  . Shelley.Spec.Ledger.EpochBoundary._stake
  . Shelley.Spec.Ledger.EpochBoundary._pstakeSet
  . Shelley.Spec.Ledger.LedgerState.esSnapshots
  . Shelley.Spec.Ledger.LedgerState.nesEs
  . Ouroboros.Consensus.Shelley.Ledger.Ledger.shelleyLedgerState

shelleyStakeDist :: Env -> Ouroboros.Consensus.Ledger.Basics.LedgerState (Ouroboros.Consensus.Shelley.Ledger.Block.ShelleyBlock Ouroboros.Consensus.Shelley.Eras.StandardShelley) -> StakeDist
shelleyStakeDist env
  = StakeDist
  . Map.mapKeys (toStakeCred env)
  . Shelley.Spec.Ledger.EpochBoundary.unStake
  . Shelley.Spec.Ledger.EpochBoundary._stake
  . Shelley.Spec.Ledger.EpochBoundary._pstakeSet
  . Shelley.Spec.Ledger.LedgerState.esSnapshots
  . Shelley.Spec.Ledger.LedgerState.nesEs
  . Ouroboros.Consensus.Shelley.Ledger.Ledger.shelleyLedgerState

allegraRewards
  :: Env
  -> Ouroboros.Consensus.Ledger.Basics.LedgerState (Ouroboros.Consensus.Shelley.Ledger.Block.ShelleyBlock Ouroboros.Consensus.Shelley.Eras.StandardAllegra)
  -> Maybe Rewards
allegraRewards env
  = fmap (Rewards . Map.mapKeys (toStakeCred env) . Shelley.Spec.Ledger.LedgerState.rs)
  . Shelley.Spec.Ledger.BaseTypes.strictMaybeToMaybe
  . Shelley.Spec.Ledger.LedgerState.nesRu
  . Ouroboros.Consensus.Shelley.Ledger.Ledger.shelleyLedgerState

maryRewards
  :: Env
  -> Ouroboros.Consensus.Ledger.Basics.LedgerState (Ouroboros.Consensus.Shelley.Ledger.Block.ShelleyBlock Ouroboros.Consensus.Shelley.Eras.StandardMary)
  -> Maybe Rewards
maryRewards env
  = fmap (Rewards . Map.mapKeys (toStakeCred env) . Shelley.Spec.Ledger.LedgerState.rs)
  . Shelley.Spec.Ledger.BaseTypes.strictMaybeToMaybe
  . Shelley.Spec.Ledger.LedgerState.nesRu
  . Ouroboros.Consensus.Shelley.Ledger.Ledger.shelleyLedgerState

shelleyRewards
  :: Env
  -> Ouroboros.Consensus.Ledger.Basics.LedgerState (Ouroboros.Consensus.Shelley.Ledger.Block.ShelleyBlock Ouroboros.Consensus.Shelley.Eras.StandardShelley)
  -> Maybe Rewards
shelleyRewards env
  = fmap (Rewards . Map.mapKeys (toStakeCred env) . Shelley.Spec.Ledger.LedgerState.rs)
  . Shelley.Spec.Ledger.BaseTypes.strictMaybeToMaybe
  . Shelley.Spec.Ledger.LedgerState.nesRu
  . Ouroboros.Consensus.Shelley.Ledger.Ledger.shelleyLedgerState

toStakeCred :: Env -> Shelley.Spec.Ledger.Credential.Credential 'Shelley.Spec.Ledger.Keys.Staking era -> StakeCred
toStakeCred env cred
  = StakeCred
  $ Shelley.Spec.Ledger.Address.serialiseRewardAcnt
  $ Shelley.Spec.Ledger.Address.RewardAcnt (envNetwork env) cred

maryEpochUpdate
  :: Env
  -> Ouroboros.Consensus.Ledger.Basics.LedgerState (Ouroboros.Consensus.Shelley.Ledger.Block.ShelleyBlock Ouroboros.Consensus.Shelley.Eras.StandardMary)
  -> Maybe Rewards
  -> Maybe Shelley.Spec.Ledger.BaseTypes.Nonce
  -> EpochUpdate
maryEpochUpdate env sls mRewards mNonce =
  EpochUpdate
    { euProtoParams = maryProtoParams sls
    , euRewards = mRewards
    , euStakeDistribution = maryStakeDist env sls
    , euNonce = fromMaybe Shelley.Spec.Ledger.BaseTypes.NeutralNonce mNonce
    }

shelleyEpochUpdate
  :: Env
  -> Ouroboros.Consensus.Ledger.Basics.LedgerState (Ouroboros.Consensus.Shelley.Ledger.Block.ShelleyBlock Ouroboros.Consensus.Shelley.Eras.StandardShelley)
  -> Maybe Rewards
  -> Maybe Shelley.Spec.Ledger.BaseTypes.Nonce -> EpochUpdate
shelleyEpochUpdate env sls mRewards mNonce =
  EpochUpdate
    { euProtoParams = shelleyProtoParams sls
    , euRewards = mRewards
    , euStakeDistribution = shelleyStakeDist env sls
    , euNonce = fromMaybe Shelley.Spec.Ledger.BaseTypes.NeutralNonce mNonce
    }

allegraProtoParams :: Ouroboros.Consensus.Ledger.Basics.LedgerState (Ouroboros.Consensus.Shelley.Ledger.Block.ShelleyBlock Ouroboros.Consensus.Shelley.Eras.StandardAllegra) -> ProtoParams
allegraProtoParams =
  toProtoParams . Shelley.Spec.Ledger.LedgerState.esPp . Shelley.Spec.Ledger.LedgerState.nesEs . Ouroboros.Consensus.Shelley.Ledger.Ledger.shelleyLedgerState

maryProtoParams :: Ouroboros.Consensus.Ledger.Basics.LedgerState (Ouroboros.Consensus.Shelley.Ledger.Block.ShelleyBlock Ouroboros.Consensus.Shelley.Eras.StandardMary) -> ProtoParams
maryProtoParams =
  toProtoParams . Shelley.Spec.Ledger.LedgerState.esPp . Shelley.Spec.Ledger.LedgerState.nesEs . Ouroboros.Consensus.Shelley.Ledger.Ledger.shelleyLedgerState

shelleyProtoParams :: Ouroboros.Consensus.Ledger.Basics.LedgerState (Ouroboros.Consensus.Shelley.Ledger.Block.ShelleyBlock Ouroboros.Consensus.Shelley.Eras.StandardShelley) -> ProtoParams
shelleyProtoParams =
  toProtoParams . Shelley.Spec.Ledger.LedgerState.esPp . Shelley.Spec.Ledger.LedgerState.nesEs . Ouroboros.Consensus.Shelley.Ledger.Ledger.shelleyLedgerState


toProtoParams :: Shelley.Spec.Ledger.PParams.PParams' Identity era -> ProtoParams
toProtoParams params =
  ProtoParams
    { ppMinfeeA = Shelley.Spec.Ledger.PParams._minfeeA params
    , ppMinfeeB = Shelley.Spec.Ledger.PParams._minfeeB params
    , ppMaxBBSize = Shelley.Spec.Ledger.PParams._maxBBSize params
    , ppMaxTxSize = Shelley.Spec.Ledger.PParams._maxTxSize params
    , ppMaxBHSize = Shelley.Spec.Ledger.PParams._maxBHSize params
    , ppKeyDeposit = Shelley.Spec.Ledger.PParams._keyDeposit params
    , ppPoolDeposit = Shelley.Spec.Ledger.PParams._poolDeposit params
    , ppMaxEpoch = Shelley.Spec.Ledger.PParams._eMax params
    , ppOptialPoolCount = Shelley.Spec.Ledger.PParams._nOpt params
    , ppInfluence = Shelley.Spec.Ledger.PParams._a0 params
    , ppMonetaryExpandRate = Shelley.Spec.Ledger.PParams._rho params
    , ppTreasuryGrowthRate = Shelley.Spec.Ledger.PParams._tau params
    , ppDecentralisation  = Shelley.Spec.Ledger.PParams._d params
    , ppExtraEntropy = Shelley.Spec.Ledger.PParams._extraEntropy params
    , ppProtocolVersion = Shelley.Spec.Ledger.PParams._protocolVersion params
    , ppMinUTxOValue = Shelley.Spec.Ledger.PParams._minUTxOValue params
    , ppMinPoolCost = Shelley.Spec.Ledger.PParams._minPoolCost params
    }

data LedgerStateSnapshot = LedgerStateSnapshot
  { lssState :: !LedgerState
  , lssEpochUpdate :: !(Maybe EpochUpdate) -- Only Just for a single block at the epoch boundary
  }

data Env = Env
  { envNetwork :: !Shelley.Spec.Ledger.BaseTypes.Network
  , envConfig :: !(C.TopLevelConfig (C.CardanoBlock C.StandardCrypto))
  }

envSecurityParam :: Env -> Word64
envSecurityParam env = k
  where
    C.SecurityParam k
      = Ouroboros.Consensus.HardFork.Combinator.Basics.hardForkConsensusConfigK
      $ C.topLevelConfigProtocol
      $ envConfig env


-- The function 'tickThenReapply' does zero validation, so add minimal validation ('blockPrevHash'
-- matches the tip hash of the 'LedgerState'). This was originally for debugging but the check is
-- cheap enough to keep.
applyBlock'
  :: Env
  -> LedgerState
  -> Ouroboros.Consensus.Cardano.Block.CardanoBlock Ouroboros.Consensus.Shelley.Eras.StandardCrypto
  -> LedgerStateSnapshot
applyBlock' env oldState blk =
  let !newState = oldState { clsState = applyBlk (C.ExtLedgerCfg (envConfig env)) blk (clsState oldState) }
  in LedgerStateSnapshot
            { lssState = newState
            , lssEpochUpdate =
                if ledgerEpochNo env newState == ledgerEpochNo env oldState + 1
                  then ledgerEpochUpdate env (clsState newState)
                          (ledgerRewardUpdate env (Ouroboros.Consensus.Ledger.Extended.ledgerState $ clsState oldState))
                  else Nothing
            }
  where
    applyBlk
        :: C.ExtLedgerCfg (C.CardanoBlock C.StandardCrypto) -> C.CardanoBlock C.StandardCrypto
        -> C.ExtLedgerState (C.CardanoBlock C.StandardCrypto)
        -> C.ExtLedgerState (C.CardanoBlock C.StandardCrypto)
    applyBlk cfg block lsb =
      case tickThenReapplyCheckHash cfg block lsb of
        Left err -> error $ Text.unpack err
        Right result -> result

-- This will return a 'Just' from the time the rewards are updated until the end of the
-- epoch. It is 'Nothing' for the first block of a new epoch (which is slightly inconvenient).
ledgerRewardUpdate :: Env -> Ouroboros.Consensus.Ledger.Basics.LedgerState (C.CardanoBlock C.StandardCrypto) -> Maybe Rewards
ledgerRewardUpdate env lsc =
    case lsc of
      Ouroboros.Consensus.Cardano.Block.LedgerStateByron _ -> Nothing -- This actually happens during the Byron era.
      Ouroboros.Consensus.Cardano.Block.LedgerStateShelley sls -> shelleyRewards env sls
      Ouroboros.Consensus.Cardano.Block.LedgerStateAllegra als -> allegraRewards env als
      Ouroboros.Consensus.Cardano.Block.LedgerStateMary mls -> maryRewards env mls

-- Create an EpochUpdate from the current epoch state and the rewards from the last epoch.
ledgerEpochUpdate :: Env -> C.ExtLedgerState (C.CardanoBlock C.StandardCrypto) -> Maybe Rewards -> Maybe EpochUpdate
ledgerEpochUpdate env els mRewards =
  case Ouroboros.Consensus.Ledger.Extended.ledgerState els of
    Ouroboros.Consensus.Cardano.Block.LedgerStateByron _ -> Nothing
    Ouroboros.Consensus.Cardano.Block.LedgerStateShelley sls -> Just $ shelleyEpochUpdate env sls mRewards mNonce
    Ouroboros.Consensus.Cardano.Block.LedgerStateAllegra als -> Just $ allegraEpochUpdate env als mRewards mNonce
    Ouroboros.Consensus.Cardano.Block.LedgerStateMary mls -> Just $ maryEpochUpdate env mls mRewards mNonce
  where
    mNonce :: Maybe Shelley.Spec.Ledger.BaseTypes.Nonce
    mNonce = extractEpochNonce els

extractEpochNonce :: Ouroboros.Consensus.Ledger.Extended.ExtLedgerState (C.CardanoBlock era) -> Maybe Shelley.Spec.Ledger.BaseTypes.Nonce
extractEpochNonce extLedgerState =
    case Ouroboros.Consensus.HeaderValidation.headerStateChainDep (Ouroboros.Consensus.Ledger.Extended.headerState extLedgerState) of
      Ouroboros.Consensus.Cardano.Block.ChainDepStateByron _ -> Nothing
      Ouroboros.Consensus.Cardano.Block.ChainDepStateShelley st -> Just $ extractNonce st
      Ouroboros.Consensus.Cardano.Block.ChainDepStateAllegra st -> Just $ extractNonce st
      Ouroboros.Consensus.Cardano.Block.ChainDepStateMary st -> Just $ extractNonce st
  where
    extractNonce :: Ouroboros.Consensus.Shelley.Protocol.TPraosState crypto -> Shelley.Spec.Ledger.BaseTypes.Nonce
    extractNonce
      = Shelley.Spec.Ledger.STS.Tickn.ticknStateEpochNonce
      . Shelley.Spec.Ledger.API.Protocol.csTickn
      . Ouroboros.Consensus.Shelley.Protocol.tpraosStateChainDepState


ledgerEpochNo :: Env -> LedgerState -> Cardano.Slotting.Slot.EpochNo
ledgerEpochNo env cls =
    case Ouroboros.Consensus.Ledger.Abstract.ledgerTipSlot (Ouroboros.Consensus.Ledger.Extended.ledgerState (clsState cls)) of
      Cardano.Slotting.Slot.Origin -> 0 -- An empty chain is in epoch 0
      Ouroboros.Consensus.Block.Abstract.NotOrigin slot -> runIdentity $ Cardano.Slotting.EpochInfo.API.epochInfoEpoch epochInfo slot
  where
    epochInfo :: Cardano.Slotting.EpochInfo.API.EpochInfo Identity
    epochInfo = Ouroboros.Consensus.HardFork.Combinator.State.epochInfoLedger
      (Ouroboros.Consensus.Config.configLedger $ envConfig env)
      (Ouroboros.Consensus.HardFork.Combinator.Basics.hardForkLedgerStatePerEra . Ouroboros.Consensus.Ledger.Extended.ledgerState $ clsState cls)

-- Like 'Consensus.tickThenReapply' but also checks that the previous hash from the block matches
-- the head hash of the ledger state.
tickThenReapplyCheckHash
    :: C.ExtLedgerCfg (C.CardanoBlock C.StandardCrypto) -> C.CardanoBlock C.StandardCrypto
    -> C.ExtLedgerState (C.CardanoBlock C.StandardCrypto)
    -> Either Text (C.ExtLedgerState (C.CardanoBlock C.StandardCrypto))
tickThenReapplyCheckHash cfg block lsb =
  if Ouroboros.Consensus.Block.Abstract.blockPrevHash block == Ouroboros.Consensus.Ledger.Abstract.ledgerTipHash (Ouroboros.Consensus.Ledger.Extended.ledgerState lsb)
    then Right $ Ouroboros.Consensus.Ledger.Abstract.tickThenReapply cfg block lsb
    else Left $ mconcat
                  [ "Ledger state hash mismatch. Ledger head is slot "
                  , textShow
                      $ Cardano.Slotting.Slot.unSlotNo
                      $ Cardano.Slotting.Slot.fromWithOrigin
                          (Cardano.Slotting.Slot.SlotNo 0)
                          (Ouroboros.Consensus.Ledger.Abstract.ledgerTipSlot $ Ouroboros.Consensus.Ledger.Extended.ledgerState lsb)
                  , " hash "
                  , renderByteArray
                      $ unChainHash
                      $ Ouroboros.Consensus.Ledger.Abstract.ledgerTipHash
                      $ Ouroboros.Consensus.Ledger.Extended.ledgerState lsb
                  , " but block previous hash is "
                  , renderByteArray (unChainHash $ Ouroboros.Consensus.Block.Abstract.blockPrevHash block)
                  , " and block current hash is "
                  , renderByteArray
                      $ BSS.fromShort
                      $ Ouroboros.Consensus.HardFork.Combinator.AcrossEras.getOneEraHash
                      $ Ouroboros.Network.Block.blockHash block
                  , "."
                  ]

renderByteArray :: ByteArrayAccess bin => bin -> Text
renderByteArray =
  Text.decodeUtf8 . Base16.encode . Data.ByteArray.convert

unChainHash :: Ouroboros.Network.Block.ChainHash (C.CardanoBlock era) -> ByteString
unChainHash ch =
  case ch of
    Ouroboros.Network.Block.GenesisHash -> "genesis"
    Ouroboros.Network.Block.BlockHash bh -> BSS.fromShort (Ouroboros.Consensus.HardFork.Combinator.AcrossEras.getOneEraHash bh)


