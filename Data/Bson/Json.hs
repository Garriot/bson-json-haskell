-- | ToJSON and FromJSON instances for BSON Documents and Values

{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, OverloadedStrings #-}

module Data.Bson.Json where

import qualified Data.Bson as B
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as AT
import qualified Data.Text.Encoding as E
import qualified Data.Text as T
import qualified Data.HashMap.Lazy as HM
import qualified Data.Vector as V
import qualified Data.Scientific as S
import           Control.Monad


instance A.ToJSON B.Document where
    toJSON fields = A.object $ map toJSONPair fields
        where
            toJSONPair :: B.Field -> AT.Pair
            toJSONPair (label B.:= value) = (label, A.toJSON value)

instance A.ToJSON B.Value where
    toJSON val = case val of
        B.Float double -> A.toJSON double
        B.String string -> A.toJSON string
        B.Doc doc -> A.toJSON doc
        B.Array array -> A.toJSON array
        B.Bin binary -> A.toJSON binary
        B.Fun function -> A.toJSON function
        B.Uuid uuid -> A.toJSON uuid
        B.Md5 md5 -> A.toJSON md5
        B.UserDef userDef -> A.toJSON userDef
        B.ObjId objId -> A.toJSON objId
        B.Bool bool -> A.toJSON bool
        B.UTC utc -> A.toJSON utc
        B.Null -> A.toJSON A.Null
        B.RegEx regex -> A.toJSON regex
        B.JavaScr js -> A.toJSON js
        B.Sym sym -> A.toJSON sym
        B.Int32 int32 -> A.toJSON int32
        B.Int64 int64 -> A.toJSON int64
        B.Stamp stamp -> A.toJSON stamp
        B.MinMax minMaxKey -> A.toJSON minMaxKey

instance A.ToJSON B.Binary where
    toJSON (B.Binary bs) = A.object [("#_BSON_Binary" , A.toJSON $ E.decodeUtf8 bs)]

instance A.ToJSON B.Function where
    toJSON (B.Function bs) = A.object [("#_BSON_Function" , A.toJSON $ E.decodeUtf8 bs)]

instance A.ToJSON B.UUID where
    toJSON (B.UUID bs) = A.object [("#_BSON_UUID" , A.toJSON $ E.decodeUtf8 bs)]

instance A.ToJSON B.MD5 where
    toJSON (B.MD5 bs) = A.object [("#_BSON_MD5" , A.toJSON $ E.decodeUtf8 bs)]

instance A.ToJSON B.UserDefined where
    toJSON (B.UserDefined bs) = A.object [("#_BSON_UserDefined" , A.toJSON $ E.decodeUtf8 bs)]

instance A.ToJSON B.ObjectId where
    toJSON oid = A.object [("#_BSON_ObjectId" , A.String $! T.pack $ show oid)]

instance A.ToJSON B.Regex where
    toJSON (B.Regex regex options) = A.object [("#_BSON_Regex" , A.object [
              ("pattern", A.toJSON regex)
            , ("options", A.toJSON options)
        ])]

instance A.ToJSON B.Javascript where
    toJSON (B.Javascript env code) = A.object [("#_BSON_Javascript" , A.object [
              ("enviroment", A.toJSON env)
            , ("code", A.toJSON code)
        ])]

instance A.ToJSON B.Symbol where
    toJSON (B.Symbol sym) = A.object [("#_BSON_Symbol" , A.toJSON sym)]

instance A.ToJSON B.MongoStamp where
    toJSON (B.MongoStamp stamp) = A.object [ ("#_BSON_MongoStamp", A.toJSON stamp)] 

instance A.ToJSON B.MinMaxKey where
    toJSON x = A.object [("#_BSON_MinMaxKey", A.String $ case x of B.MinKey -> "MinKey"; B.MaxKey -> "MaxKey")]

instance A.FromJSON B.Document where
    parseJSON (A.Object dict) = mapM parseJSONPair $ HM.toList dict
        where
            parseJSONPair (label, value)= fmap (label B.:=) $ A.parseJSON value
    parseJSON x = fail $ "BSON document expects to be deriving from a JSON Object, not " ++ show x

instance A.FromJSON B.Value where
    parseJSON v = case v of
        AT.Object _ -> msum [
            B.Bin <$> A.parseJSON v,
            B.Fun <$> A.parseJSON v,
            B.Uuid <$> A.parseJSON v,
            B.Md5 <$> A.parseJSON v,
            B.UserDef <$> A.parseJSON v,
            B.ObjId <$> A.parseJSON v,
            B.RegEx <$> A.parseJSON v,
            B.JavaScr <$> A.parseJSON v,
            B.Sym <$> A.parseJSON v,
            B.Stamp <$> A.parseJSON v,
            B.MinMax <$> A.parseJSON v,
            B.Doc <$> A.parseJSON v ]
        A.Array vec -> B.Array <$> mapM A.parseJSON (V.toList vec)
        A.String text -> pure $ case A.fromJSON v of
            A.Success utcTime -> B.UTC utcTime
            A.Error _ -> B.String text
        A.Number num -> pure $ case S.floatingOrInteger num of
            Left float -> B.Float float
            Right int -> B.val (int::Int)
        A.Bool bool -> pure $ B.Bool bool
        A.Null -> pure B.Null


instance A.FromJSON B.Binary where
    parseJSON v = go `mplus` fail' where
        go = do
            A.Object dict <- return v
            [("#_BSON_Binary", A.String text)] <- return $ HM.toList dict
            return $ B.Binary $ E.encodeUtf8 text
        fail' = fail $ "BSON Binary expects object with special field #_BSON_Binary, not " ++ show v

instance A.FromJSON B.Function where
    parseJSON v = go `mplus` fail' where
        go = do
            A.Object dict <- return v
            [("#_BSON_Function", A.String text)] <- return $ HM.toList dict
            return $ B.Function $ E.encodeUtf8 text
        fail' = fail $ "BSON Function expects object with special field #_BSON_Function, not " ++ show v

instance A.FromJSON B.UUID where
    parseJSON v = go `mplus` fail' where
        go = do
            A.Object dict <- return v
            [("#_BSON_UUID", A.String text)] <- return $ HM.toList dict
            return $ B.UUID $ E.encodeUtf8 text
        fail' = fail $ "BSON UUID expects object with special field #_BSON_UUID, not " ++ show v

instance A.FromJSON B.MD5 where
    parseJSON v = go `mplus` fail' where
        go = do
            A.Object dict <- return v
            [("#_BSON_MD5", A.String text)] <- return $ HM.toList dict
            return $ B.MD5 $ E.encodeUtf8 text
        fail' = fail $ "BSON MD5 expects object with special field #_BSON_MD5, not " ++ show v

instance A.FromJSON B.UserDefined where
    parseJSON v = go `mplus` fail' where
        go = do
            A.Object dict <- return v
            [("#_BSON_UserDefined", A.String text)] <- return $ HM.toList dict
            return $ B.UserDefined $ E.encodeUtf8 text
        fail' = fail $ "BSON UserDefined expects object with special field #_BSON_UserDefined, not " ++ show v

instance A.FromJSON B.ObjectId where
    parseJSON v = go `mplus` fail' where
        go = do
            A.Object dict <- return v
            [("#_BSON_ObjectId", A.String text)] <- return $ HM.toList dict
            return $ read $ T.unpack text
        fail' = fail $ "BSON ObjectId expects object with special field #_BSON_ObjectId, not " ++ show v

instance A.FromJSON B.Regex where
    parseJSON v = go `mplus` fail' where
        go = do
            A.Object dict <- return v
            [("#_BSON_Regex", A.Object dict2)] <- return $ HM.toList dict
            [("pattern", A.String pattern), ("options", A.String options)] <- return $ HM.toList dict2
            return $ B.Regex pattern options
        fail' = fail $ "BSON Regex expects object with special field #_BSON_Regex, not " ++ show v

instance A.FromJSON B.Javascript where
    parseJSON v = go `mplus` fail' where
        go = do
            A.Object dict <- return v
            [("#_BSON_Javascript", A.Object dict2)] <- return $ HM.toList dict
            [("environment", A.Object env), ("code", A.String code)] <- return $ HM.toList dict2
            flip B.Javascript code <$> A.parseJSON (A.Object env)
        fail' = fail $ "BSON Javascript expects object with special field #_BSON_Javascript, not " ++ show v

instance A.FromJSON B.Symbol where
    parseJSON v = go `mplus` fail' where
        go = do
            A.Object dict <- return v
            [("#_BSON_MD5", A.String text)] <- return $ HM.toList dict
            return $ B.Symbol text
        fail' = fail $ "BSON Symbol expects object with special field #_BSON_Symbol, not " ++ show v

instance A.FromJSON B.MongoStamp where
    parseJSON v = go `mplus` fail' where
        go = do
            A.Object dict <- return v
            [("#_BSON_MongoStamp", A.Number int)] <- return $ HM.toList dict
            mongoStamp int
        fail' = fail $ "BSON MongoStamp expects object with special field #_BSON_MongoStamp, not " ++ show v
        mongoStamp int = case S.toBoundedInteger int of 
            Just x -> return $ B.MongoStamp x
            Nothing -> fail'

instance A.FromJSON B.MinMaxKey where
    parseJSON v = go `mplus` fail' where
        go = do
            A.Object dict <- return v
            [("#_BSON_MinMaxKey", A.String text)] <- return $ HM.toList dict
            case text of "MinKey" -> return B.MinKey; "MaxKey" -> return B.MaxKey; _ -> mzero
        fail' = fail $ "BSON MinMaxKey expects object with special field #_BSON_MinMaxKey, not " ++ show v