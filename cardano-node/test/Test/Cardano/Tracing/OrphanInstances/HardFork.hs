module Test.Cardano.Tracing.OrphanInstances.HardFork (tests) where

import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import           Data.TreeDiff.Class (ediff)
import           Data.TreeDiff.Pretty (prettyEditExpr)
import           Text.PrettyPrint (render)
import           Data.SOP.Strict (NP ((:*), Nil))

import Test.Tasty (defaultMain, TestTree, testGroup)
import Test.Tasty.Golden.Advanced (goldenTest)

import           Cardano.Ledger.Crypto (StandardCrypto)

import           Ouroboros.Consensus.Byron.Ledger.NetworkProtocolVersion as Consensus.Cardano
import qualified Ouroboros.Consensus.Cardano.Block as Consensus.Cardano
import qualified Ouroboros.Consensus.HardFork.Combinator.Serialisation.Common as Consensus
import           Ouroboros.Consensus.Shelley.Ledger.NetworkProtocolVersion as Consensus.Cardano

import Cardano.Tracing.OrphanInstances.Byron ()
import Cardano.Tracing.OrphanInstances.HardFork ()
import Cardano.Tracing.OrphanInstances.Shelley ()

tests :: IO Bool
tests = goldenTests >>= defaultMain >> pure True

goldenTests :: IO TestTree
goldenTests = pure $ testGroup "HardForkNodeToClientVersion JSON instances"
                   $ fmap (uncurry goldenTestJSON)
                   $ [ (ntcByronOnly,               "ntcByronOnly.json")
                     , (ntc_HFV1_allDisabled,       "ntc_HFV1_allDisabled.json")
                     , (ntc_HFV1_ByronV1,           "ntc_HFV1_ByronV1.json")
                     , (ntc_HFV1_ByronV1_ShelleyV1, "ntc_HFV1_ByronV1_ShelleyV1.json")
                     ]

--------------------------------------------------------------------------------
-- Examples
--------------------------------------------------------------------------------

ntcByronOnly ::
  Consensus.HardForkNodeToClientVersion
    (Consensus.Cardano.CardanoEras StandardCrypto)
ntcByronOnly =
    Consensus.HardForkNodeToClientDisabled
      (Consensus.Cardano.ByronNodeToClientVersion1)

ntc_HFV1_allDisabled ::
  Consensus.HardForkNodeToClientVersion
    (Consensus.Cardano.CardanoEras StandardCrypto)
ntc_HFV1_allDisabled =
  Consensus.HardForkNodeToClientEnabled
    Consensus.HardForkSpecificNodeToClientVersion1
    (    Consensus.EraNodeToClientDisabled -- Byron
      :* Consensus.EraNodeToClientDisabled -- Shelley
      :* Consensus.EraNodeToClientDisabled -- Allegra
      :* Consensus.EraNodeToClientDisabled -- Mary
      :* Consensus.EraNodeToClientDisabled -- Alonzo
      :* Consensus.EraNodeToClientDisabled -- Babbage
      :* Consensus.EraNodeToClientDisabled -- Conway
      :* Nil
    )

ntc_HFV1_ByronV1 ::
  Consensus.HardForkNodeToClientVersion
    (Consensus.Cardano.CardanoEras StandardCrypto)
ntc_HFV1_ByronV1 =
  Consensus.HardForkNodeToClientEnabled
    Consensus.HardForkSpecificNodeToClientVersion1
    (    Consensus.EraNodeToClientEnabled (Consensus.Cardano.ByronNodeToClientVersion1) -- Byron
      :* Consensus.EraNodeToClientDisabled -- Shelley
      :* Consensus.EraNodeToClientDisabled -- Allegra
      :* Consensus.EraNodeToClientDisabled -- Mary
      :* Consensus.EraNodeToClientDisabled -- Alonzo
      :* Consensus.EraNodeToClientDisabled -- Babbage
      :* Consensus.EraNodeToClientDisabled -- Conway
      :* Nil
    )

ntc_HFV1_ByronV1_ShelleyV1 ::
  Consensus.HardForkNodeToClientVersion
    (Consensus.Cardano.CardanoEras StandardCrypto)
ntc_HFV1_ByronV1_ShelleyV1 =
  Consensus.HardForkNodeToClientEnabled
    (Consensus.HardForkSpecificNodeToClientVersion1 )
    (    Consensus.EraNodeToClientEnabled (Consensus.Cardano.ByronNodeToClientVersion1)   -- Byron
      :* Consensus.EraNodeToClientEnabled (Consensus.Cardano.ShelleyNodeToClientVersion1) -- Shelley
      :* Consensus.EraNodeToClientDisabled -- Allegra
      :* Consensus.EraNodeToClientDisabled -- Mary
      :* Consensus.EraNodeToClientDisabled -- Alonzo
      :* Consensus.EraNodeToClientDisabled -- Babbage
      :* Consensus.EraNodeToClientDisabled -- Conway
      :* Nil
    )

--------------------------------------------------------------------------------
-- Helper functions
--------------------------------------------------------------------------------

goldenTestJSON :: Aeson.ToJSON a => a -> FilePath -> TestTree
goldenTestJSON valueToEncode goldenFileBaseName =
    goldenTest
      goldenFileBaseName
      -- We expect the contents to the golde file to be small, hence we read them all in one go.
      (BS.readFile $ addPrefix goldenFileBaseName)
      (pure $ BL.toStrict $ Aeson.encode valueToEncode)
      (\golden result -> pure $
         if golden == result
            then Nothing
            else Just $ render $ prettyEditExpr $ ediff golden result
      )
      (BS.writeFile $ addPrefix goldenFileBaseName)
  where
    addPrefix fname = "test/Test/Cardano/Tracing/OrphanInstances/data/" <> fname
